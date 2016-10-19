object KMemoContainerForm: TKMemoContainerForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Insert/edit container (text box)'
  ClientHeight = 453
  ClientWidth = 524
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BUOk: TButton
    Left = 352
    Top = 420
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BUCancel: TButton
    Left = 433
    Top = 420
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GBPreview: TGroupBox
    Left = 210
    Top = 3
    Width = 298
    Height = 411
    Caption = 'Preview'
    TabOrder = 2
    object MEPreview: TKMemo
      Left = 2
      Top = 15
      Width = 294
      Height = 394
      Align = alClient
      ContentPadding.Left = 5
      ContentPadding.Top = 5
      ContentPadding.Right = 5
      ContentPadding.Bottom = 5
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ReadOnly = True
      TabOrder = 0
    end
  end
  object GBPosition: TGroupBox
    Left = 8
    Top = 3
    Width = 196
    Height = 98
    Caption = 'Position'
    TabOrder = 3
    object RBPositionRelative: TRadioButton
      Left = 14
      Top = 29
      Width = 90
      Height = 17
      Caption = 'Relative'
      TabOrder = 0
    end
    object RBPositionAbsolute: TRadioButton
      Left = 14
      Top = 69
      Width = 90
      Height = 17
      Caption = 'Absolute'
      TabOrder = 1
    end
    object EDOffsetX: TKNumberEdit
      Left = 126
      Top = 27
      Width = 43
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      Caption = 'Horz. offset:'
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 10000.000000000000000000
      Min = -10000.000000000000000000
      Precision = 1
      TabOrder = 2
    end
    object EDOffsetY: TKNumberEdit
      Left = 126
      Top = 67
      Width = 43
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      Caption = 'Vert. offset:'
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 10000.000000000000000000
      Min = -10000.000000000000000000
      Precision = 1
      TabOrder = 4
    end
  end
  object GBSize: TGroupBox
    Left = 8
    Top = 107
    Width = 196
    Height = 86
    Caption = 'Size'
    TabOrder = 4
    object EDWidth: TKNumberEdit
      Left = 14
      Top = 33
      Width = 51
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      Caption = 'Width:'
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Min = 1.000000000000000000
      Precision = 1
      Value = 100.000000000000000000
      OnUpDownChange = EDWidthExit
      TabOrder = 0
      OnExit = EDWidthExit
    end
    object EDHeight: TKNumberEdit
      Left = 117
      Top = 33
      Width = 52
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      Caption = 'Height:'
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Min = 1.000000000000000000
      Precision = 1
      Value = 100.000000000000000000
      OnUpDownChange = EDWidthExit
      TabOrder = 2
      OnExit = EDWidthExit
    end
    object CBAutoWidth: TCheckBox
      Left = 14
      Top = 60
      Width = 86
      Height = 17
      Caption = 'Auto width'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = EDWidthExit
    end
    object CBAutoHeight: TCheckBox
      Left = 100
      Top = 60
      Width = 83
      Height = 17
      Caption = 'Auto height'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = EDWidthExit
    end
  end
  object GBWrap: TGroupBox
    Left = 8
    Top = 199
    Width = 196
    Height = 114
    Caption = 'Content floating'
    TabOrder = 5
    object RBWrapAround: TRadioButton
      Left = 14
      Top = 20
      Width = 163
      Height = 17
      Caption = 'Float on both sides'
      TabOrder = 0
      OnClick = EDWidthExit
    end
    object RBWrapAroundLeft: TRadioButton
      Left = 14
      Top = 43
      Width = 155
      Height = 17
      Caption = 'Float on left side'
      TabOrder = 1
      OnClick = EDWidthExit
    end
    object RBWrapAroundRight: TRadioButton
      Left = 14
      Top = 66
      Width = 155
      Height = 17
      Caption = 'Float on right side'
      TabOrder = 2
      OnClick = EDWidthExit
    end
    object RBWrapTopBottom: TRadioButton
      Left = 14
      Top = 89
      Width = 163
      Height = 17
      Caption = 'No float'
      TabOrder = 3
      OnClick = EDWidthExit
    end
  end
  object GBShading: TGroupBox
    Left = 8
    Top = 319
    Width = 196
    Height = 126
    Caption = 'Borders and shading'
    TabOrder = 6
    object LBBorderWidth: TLabel
      Left = 18
      Top = 27
      Width = 65
      Height = 13
      Caption = 'Border width:'
    end
    object LBBorderColor: TLabel
      Left = 18
      Top = 56
      Width = 62
      Height = 13
      Caption = 'Border color:'
    end
    object LBShading: TLabel
      Left = 17
      Top = 87
      Width = 42
      Height = 13
      Caption = 'Shading:'
    end
    object EDBorderWidth: TKNumberEdit
      Left = 98
      Top = 24
      Width = 55
      Height = 21
      AcceptedFormats = [neafDec, neafFloat]
      CustomSuffix = 'pt'
      DecimalSeparator = ','
      Max = 20.000000000000000000
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      OnUpDownChange = EDWidthExit
      TabOrder = 0
      OnExit = EDWidthExit
    end
    object CLBBorder: TKColorButton
      Left = 98
      Top = 51
      Width = 70
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = EDWidthExit
      ColorDlgOptions = []
    end
    object CLBShading: TKColorButton
      Left = 98
      Top = 82
      Width = 70
      Height = 25
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = EDWidthExit
      ColorDlgOptions = []
    end
  end
end
