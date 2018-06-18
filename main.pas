unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeEngine, Series, StdCtrls, ExtCtrls, TeeProcs, Chart;

type
  TForm1 = class(TForm)
    AtomGroup: TRadioGroup;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox2: TGroupBox;
    cbWafefunction: TCheckBox;
    cbProbability: TCheckBox;
    ButtonVisualize: TButton;
    Chart1: TChart;
    MemoDebug: TMemo;
    LabelDebug: TLabel;
    Series1: TLineSeries;
    Series2: TLineSeries;
    procedure ButtonVisualizeClick(Sender: TObject);
  private
    { Private declarations }
    function GetZetta(): double;
    function CekSign(A: double; B: double): double;
    procedure DoMesh();
    procedure InitPot();
    procedure SolveSheq();
    procedure PlotWaveFunction();
    procedure PlotProbability();
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mesh, n, l: integer;
  zeta, zmesh, rmax, xmin, dx, e: double;
  r, akar, r2, y, vpot: array of double;

implementation

uses Math;

{$R *.dfm}

function TForm1.CekSign(A: double; B: double): double;
begin
  if B > 0 then
    CekSign := abs(A)
  else
    CekSign := -abs(A);
end;

function TForm1.GetZetta(): double;
var number: double;
begin
  number := 0;
  case AtomGroup.ItemIndex of
    0: number := 1.000000;
    1: number := 2.000000;
    2: number := 1.259286;
    3: number := 1.655890;
    4: number := 1.562241;
    5: number := 1.819850;
    6: number := 2.067546;
    7: number := 2.001328;
  end;

  GetZetta := number;
end;

procedure TForm1.ButtonVisualizeClick(Sender: TObject);
begin
  // init
  zeta  := GetZetta();
  zmesh := zeta;
  rmax  := 100.0;
  xmin  := -8.0;
  dx    := 0.01;
  mesh  := trunc((ln(zmesh * rmax)-xmin) / dx) + 1;
  SetLength(r, mesh);
  SetLength(akar, mesh);
  SetLength(r2, mesh);
  SetLength(y, mesh);
  SetLength(vpot, mesh);

  // debug init
  MemoDebug.Lines.Clear;
  MemoDebug.Lines.Add('---------------------');
  MemoDebug.Lines.Add(' Initialization   ');
  MemoDebug.Lines.Add('---------------------');
  MemoDebug.Lines.Add('zmesh = ' + FloatToStr(zmesh));
  MemoDebug.Lines.Add('rmax = ' + FloatToStr(rmax));
  MemoDebug.Lines.Add('xmin = ' + FloatToStr(xmin));
  MemoDebug.Lines.Add('dx = ' + FloatToStr(dx));
  MemoDebug.Lines.Add('mesh = ' + IntToStr(mesh));
  MemoDebug.Lines.Add('');

  // calc
  DoMesh;
  InitPot;
  SolveSheq;

  // visualize
  if cbWavefunction.Checked then
    PlotWaveFunction;
  if cbProbability.Checked then
    PlotProbability;
end;

procedure TForm1.DoMesh;
var
  i: integer;
  x: double;
begin
  for i:=0 to mesh-1 do
  begin
    x := xmin + dx * i;
    r[i] := exp(x) / zmesh;
    akar[i] := sqrt(r[i]);
    r2[i] := r[i] * r[i];

    //MemoDebug.Lines.Add('r[' + IntToStr(i) + '] = ' + FloatToStr(r[i]));
  end;
end;

procedure TForm1.InitPot;
var
  i: integer;
begin
  for i:=0 to mesh-1 do
  begin
    vpot[i] := -2 * zeta / r[i];

    //MemoDebug.Lines.Add('vpot[' + IntToStr(i) + '] = ' + FloatToStr(vpot[i]));
  end;
end;

procedure TForm1.SolveSheq;
var
  i,j,kkk,maxiter,icl,nodes,ncross: integer;
  ddx12, sqlhf, x2l2, ycusp, dfcusp, fac, norm, eup, elw, de, eps, tmp: double;
  f: array of double;
begin
  maxiter := 100;
  eps   := 1.0e-10;
  kkk := 0;
  icl := 0;
  de := 0;
  ncross := 0;
  nodes := 0;
  
  SetLength(f, mesh);
  ddx12 := (dx * dx) / 12.0;
  sqlhf := sqr((l + 0.5));
  x2l2  := 2 * l + 2;

  eup := vpot[mesh-1];
  elw := MaxDouble;
  for i := 0 to mesh-1 do
  begin
    tmp := sqlhf / r2[i] + vpot[i];
    if(tmp < elw) then
      elw := tmp;
  end;
  if(eup - elw < eps) then
  begin
    MemoDebug.Lines.Add('eup = ' + FloatToStr(eup));
    MemoDebug.Lines.Add('elw = ' + FloatToStr(elw));
    ShowMessage('solve_sheq: lower and upper bounds are equal');
    exit;
  end;
  e := 0.5 * (elw + eup);

  for kkk := 1 to maxiter do
  begin
    icl := -1;
    f[0] := ddx12 * (sqlhf + r2[0] * (vpot[0]-e));
    for i := 1 to mesh-1 do
    begin
      f[i] := ddx12 * (sqlhf + r2[i] * (vpot[i]-e));
      if(f[i] = 0.0) then f[i] := 1.0e-20;
      if(f[i] <> CekSign(f[i], f[i-1])) then icl := i;
    end;

    if((icl < 0) or (icl >= mesh-2)) then
    begin
      eup := e;
      e := 0.5 * (eup+elw);
      Continue;
    end;

    // init f and y
    for i := 0 to mesh-1 do
    begin
      f[i] := 1.0 - f[i];
      y[i] := 0;
    end;

    nodes := n - l - 1;
    y[0] := Power(r[0], l+1) * (1.0 - 2.0*zeta*r[0]/x2l2) / akar[0];
    y[1] := Power(r[1], l+1) * (1.0 - 2.0*zeta*r[1]/x2l2) / akar[1];

    // outward integration, count number of crossings
    ncross := 0;
    for i:=0 to icl-2 do
    begin
      y[i+1] := ((12.0-10.0*f[i])*y[i]-f[i-1]*y[i-1])/f[i+1];
      if(y[i] <> CekSign(y[i],y[i+1])) then ncross := ncross + 1;
    end;
    fac := y[icl];

    // check number of crossing
    if(ncross <> nodes) then
    begin
      if(ncross > nodes) then
        eup := e
      else
        elw := e;
      e := 0.5 * (eup + elw);
      continue;
    end;

    // determination of the wave-function in the last two points
    // assuming y(mesh+1) = 0 and y(mesh) = dx
    y[mesh-1] := dx;
    y[mesh-2] := (12.0-10.0*f[mesh-1])*y[mesh-1]/f[mesh-2];

    // inward integration
    for i := mesh-2 downto icl+2 do
    begin
      y[i-1] := ((12.0-10.0*f[i])*y[i]-f[i+1]*y[i+1])/f[i-1];
      if (y[i-1] > 1.0e10) then
        for j := mesh-1 downto i-1 do
          y[j] := y[j] / y[i-1];
    end;

    // rescale function to match at the classical turning point (icl)
    fac := fac / y[icl];
    for i:=icl to mesh-1 do
      y[i] := y[i] * fac;

    // normalization
    norm := 0;
    for i := 0 to mesh-1 do
      norm := norm + y[i]*y[i] * r2[i] * dx;
    norm := sqrt(norm);
    for i := 0 to mesh-1 do
      y[i] := y[i] / norm;

    // find the value of the cusp at the matching point (icl)
    i := icl;
    ycusp := (y[i-1]*f[i-1] + y[i+1]*f[i+1]+10.0*f[i]*y[i]) / 12.0;
    dfcusp := f[i] * (y[i] / ycusp - 1.0);

    // eigenvalue update using perturbation theory
    de := dfcusp / ddx12 * ycusp * ycusp * dx;
    if (de > 0.0) then elw := e;
    if (de < 0.0) then eup := e;

    // prevent e to go out of bounds, i.e. e > eup or e < elw
    e := max(min(e+de,eup),elw);

    // convergence check
    if(abs(de) < eps) then exit;
  end;

  // was convergence archieved?
  if(abs(de) > 1.0e-10) then
  begin
    if(ncross <> nodes) then
    begin
      MemoDebug.Lines.Add('e = ' + FloatToStr(e));
      MemoDebug.Lines.Add('elw = ' + FloatToStr(elw));
      MemoDebug.Lines.Add('eup = ' + FloatToStr(eup));
      MemoDebug.Lines.Add('ncross = ' + FloatToStr(ncross));
      MemoDebug.Lines.Add('nodes = ' + FloatToStr(nodes));
      MemoDebug.Lines.Add('icl = ' + FloatToStr(icl));
    end
    else
    begin
      MemoDebug.Lines.Add('e = ' + FloatToStr(e));
      MemoDebug.Lines.Add('de = ' + FloatToStr(de));
    end;
    MemoDebug.Lines.Add('error in solve_sheq: too many iteration');
  end
  else
  begin
    MemoDebug.Lines.Add('Convergence achieved at iter #' + IntToStr(kkk) + ', de = ' + FloatToStr(de));
  end;
  MemoDebug.Lines.Add('');
end;

procedure TForm1.PlotWaveFunction;
var
  i: integer;
  x: double;
begin
  Chart1.Series[0].Clear;
  MemoDebug.Lines.Add('------------------------');
  MemoDebug.Lines.Add(' Wavefunction data');
  MemoDebug.Lines.Add('------------------------');

  for i:=0 to mesh-1 do
  begin
    x := xmin + dx * i;
    Chart1.Series[0].AddXY(x, y[i]/akar[i]);
    MemoDebug.Lines.Add(IntToStr(i)+': '+FloatToStr(x)+','+FloatToStr(y[i]/akar[i]));
  end;
end;

procedure TForm1.PlotProbability;
var
  i: integer;
  x: double;
begin
  Chart1.Series[1].Clear;
  MemoDebug.Lines.Add('------------------------');
  MemoDebug.Lines.Add(' Wavefunction data');
  MemoDebug.Lines.Add('------------------------');

  for i:=0 to mesh-1 do
  begin
    x := xmin + dx * i;
    Chart1.Series[1].AddXY(x, power(y[i]*akar[i],2));
    MemoDebug.Lines.Add(IntToStr(i)+': '+FloatToStr(x)+','+FloatToStr(power(y[i]*akar[i],2)));
  end;
end;

end.
