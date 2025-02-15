object MainForm: TMainForm
  Left = 348
  Top = 111
  Caption = 'KAlphaBitmap resample demo'
  ClientHeight = 755
  ClientWidth = 1289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    1289
    755)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 482
    Width = 54
    Height = 13
    Caption = 'New width:'
  end
  object Label2: TLabel
    Left = 132
    Top = 482
    Width = 58
    Height = 13
    Caption = 'New height:'
  end
  object Label3: TLabel
    Left = 213
    Top = 482
    Width = 73
    Height = 13
    Caption = 'Kernel window:'
  end
  object Label4: TLabel
    Left = 294
    Top = 482
    Width = 59
    Height = 13
    Caption = 'Kernel type:'
  end
  object Label5: TLabel
    Left = 16
    Top = 13
    Width = 68
    Height = 13
    Caption = 'Source image:'
  end
  object Label6: TLabel
    Left = 432
    Top = 13
    Width = 89
    Height = 13
    Caption = 'Destination image:'
  end
  object Label7: TLabel
    Left = 279
    Top = 529
    Width = 34
    Height = 32
    AutoSize = False
    Caption = 'Anim. cnt.:'
    WordWrap = True
  end
  object ScBIM2: TScrollBox
    Left = 432
    Top = 32
    Width = 825
    Height = 697
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 9
    object IM2: TImage
      Left = 0
      Top = 0
      Width = 393
      Height = 369
      AutoSize = True
    end
  end
  object BUResample: TButton
    Left = 8
    Top = 528
    Width = 105
    Height = 57
    Caption = 'Resample'
    TabOrder = 0
    OnClick = BUResampleClick
  end
  object EDW: TEdit
    Left = 8
    Top = 501
    Width = 75
    Height = 21
    TabOrder = 1
    Text = '100'
    OnExit = EDWExit
  end
  object EDH: TEdit
    Left = 132
    Top = 501
    Width = 75
    Height = 21
    TabOrder = 2
    Text = '100'
  end
  object LBLog: TListBox
    Left = 8
    Top = 598
    Width = 401
    Height = 137
    ItemHeight = 13
    TabOrder = 3
  end
  object EDWindow: TEdit
    Left = 213
    Top = 501
    Width = 73
    Height = 21
    TabOrder = 4
    Text = '3'
  end
  object BULoadFromFile: TButton
    Left = 7
    Top = 441
    Width = 138
    Height = 25
    Caption = 'Load from file'
    TabOrder = 5
    OnClick = BULoadFromFileClick
  end
  object BULoadTestBitmap: TButton
    Left = 231
    Top = 441
    Width = 138
    Height = 25
    Caption = 'Load test bitmap'
    TabOrder = 6
    OnClick = BULoadTestBitmapClick
  end
  object CoBType: TComboBox
    Left = 294
    Top = 501
    Width = 115
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 7
    Text = 'Bicubic'
    Items.Strings = (
      'Bicubic'
      'Lanczos'
      'Triangle')
  end
  object ScBIM1: TScrollBox
    Left = 16
    Top = 32
    Width = 401
    Height = 369
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    BorderStyle = bsNone
    TabOrder = 8
    object IM1: TImage
      Left = 0
      Top = 0
      Width = 265
      Height = 289
      AutoSize = True
    end
  end
  object EDFile: TKFileNameEdit
    Left = 8
    Top = 414
    Width = 280
    Height = 21
    ButtonWidth = 75
    Options = [foAddToList, foWarning]
    TabOrder = 10
    OnChange = EDFileChange
  end
  object BUHalf: TButton
    Left = 117
    Top = 528
    Width = 75
    Height = 25
    Caption = 'Half'
    TabOrder = 12
    OnClick = BUHalfClick
  end
  object BUDouble: TButton
    Left = 198
    Top = 528
    Width = 75
    Height = 25
    Caption = 'Double'
    TabOrder = 13
    OnClick = BUDoubleClick
  end
  object BUTriple: TButton
    Left = 117
    Top = 559
    Width = 75
    Height = 25
    Caption = 'Triple'
    TabOrder = 14
    OnClick = BUTripleClick
  end
  object BUXdYh: TButton
    Left = 198
    Top = 559
    Width = 75
    Height = 25
    Caption = 'Xdbl/Yhalf'
    TabOrder = 15
    OnClick = BUXdYhClick
  end
  object CBAspectRatio: TCheckBox
    Left = 87
    Top = 503
    Width = 42
    Height = 17
    Caption = 'A.R.'
    TabOrder = 16
    OnClick = CBAspectRatioClick
  end
  object EDAnimCnt: TEdit
    Left = 279
    Top = 561
    Width = 44
    Height = 21
    TabOrder = 17
    Text = '50'
  end
  object BUAnimDown: TButton
    Left = 324
    Top = 528
    Width = 85
    Height = 25
    Caption = 'Anim. down'
    TabOrder = 18
    OnClick = BUAnimDownClick
  end
  object BUAnimUp: TButton
    Left = 324
    Top = 559
    Width = 85
    Height = 25
    Caption = 'Anim. up'
    TabOrder = 19
    OnClick = BUAnimUpClick
  end
  object Log: TKLog
    InternalStorage = False
    ListBox = LBLog
    Left = 8
  end
  object Timer: TTimer
    Interval = 250
    OnTimer = TimerTimer
    Left = 56
  end
end
