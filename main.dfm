object Form1: TForm1
  Left = 594
  Top = 243
  Width = 1019
  Height = 465
  Caption = 'Atomic Computing Visualizer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDebug: TLabel
    Left = 808
    Top = 8
    Width = 32
    Height = 13
    Caption = 'Debug'
  end
  object AtomGroup: TRadioGroup
    Left = 8
    Top = 8
    Width = 169
    Height = 201
    Caption = 'Choose an atom'
    ItemIndex = 0
    Items.Strings = (
      'Hidrogen'
      'Helium'
      'Lithium'
      'Berilium'
      'Boron'
      'Carbon'
      'Nitrogen'
      'Oxygen')
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 216
    Width = 169
    Height = 81
    Caption = 'Quantum Number'
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 12
      Height = 13
      Caption = 'n: '
    end
    object Label2: TLabel
      Left = 8
      Top = 48
      Width = 8
      Height = 13
      Caption = 'l: '
    end
    object Edit1: TEdit
      Left = 24
      Top = 24
      Width = 137
      Height = 21
      BiDiMode = bdLeftToRight
      ParentBiDiMode = False
      TabOrder = 0
      Text = '1'
    end
    object Edit2: TEdit
      Left = 24
      Top = 48
      Width = 137
      Height = 21
      TabOrder = 1
      Text = '0'
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 304
    Width = 169
    Height = 81
    Caption = 'Plot Selection'
    TabOrder = 2
    object cbWafefunction: TCheckBox
      Left = 8
      Top = 24
      Width = 153
      Height = 17
      Caption = 'Wave Function'
      TabOrder = 0
    end
    object cbProbability: TCheckBox
      Left = 8
      Top = 48
      Width = 153
      Height = 17
      Caption = 'Probability Electron Density'
      TabOrder = 1
    end
  end
  object ButtonVisualize: TButton
    Left = 8
    Top = 392
    Width = 169
    Height = 25
    Caption = 'Visualize it!'
    TabOrder = 3
    OnClick = ButtonVisualizeClick
  end
  object Chart1: TChart
    Left = 192
    Top = 8
    Width = 609
    Height = 409
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      'Atomic Computing Visualizer')
    Legend.Alignment = laBottom
    View3D = False
    TabOrder = 4
    object Series1: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clRed
      Title = 'Wave Function'
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
    object Series2: TLineSeries
      Marks.ArrowLength = 8
      Marks.Visible = False
      SeriesColor = clGreen
      Title = 'Probability Electron Density'
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Y'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
  end
  object MemoDebug: TMemo
    Left = 808
    Top = 32
    Width = 185
    Height = 385
    TabOrder = 5
  end
end
