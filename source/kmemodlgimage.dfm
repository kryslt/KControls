object KMemoImageForm: TKMemoImageForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Insert/edit image'
  ClientHeight = 438
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
    Left = 360
    Top = 404
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BUCancel: TButton
    Left = 441
    Top = 404
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object BUBrowse: TButton
    Left = 8
    Top = 404
    Width = 116
    Height = 25
    Caption = 'Browse...'
    TabOrder = 2
    OnClick = BUBrowseClick
  end
  object PCMain: TPageControl
    Left = 8
    Top = 8
    Width = 217
    Height = 389
    ActivePage = TSAdvanced
    TabOrder = 3
    object TSBasic: TTabSheet
      Caption = 'Basic'
      object GBPosition: TGroupBox
        Left = 8
        Top = 3
        Width = 196
        Height = 98
        Caption = 'Position'
        TabOrder = 0
        object RBPositionText: TRadioButton
          Left = 14
          Top = 20
          Width = 90
          Height = 17
          Caption = 'In text'
          TabOrder = 0
          OnClick = RBPositionTextClick
        end
        object RBPositionRelative: TRadioButton
          Left = 14
          Top = 43
          Width = 90
          Height = 17
          Caption = 'Relative'
          TabOrder = 1
          OnClick = RBPositionTextClick
        end
        object RBPositionAbsolute: TRadioButton
          Left = 14
          Top = 66
          Width = 90
          Height = 17
          Caption = 'Absolute'
          TabOrder = 2
          OnClick = RBPositionTextClick
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
          TabOrder = 3
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
          TabOrder = 5
        end
      end
      object GBSize: TGroupBox
        Left = 8
        Top = 106
        Width = 196
        Height = 133
        Caption = 'Size'
        TabOrder = 1
        object EDScaleX: TKNumberEdit
          Left = 104
          Top = 33
          Width = 51
          Height = 21
          Caption = 'Horz. scale:'
          CustomSuffix = '%'
          DecimalSeparator = ','
          Min = 1.000000000000000000
          Value = 100.000000000000000000
          OnUpDownChange = EDScaleXExit
          TabOrder = 0
          OnChange = EDScaleXChange
          OnExit = EDScaleXExit
        end
        object EDScaleY: TKNumberEdit
          Left = 104
          Top = 73
          Width = 52
          Height = 21
          Caption = 'Vert. scale:'
          CustomSuffix = '%'
          DecimalSeparator = ','
          Min = 1.000000000000000000
          Value = 100.000000000000000000
          OnUpDownChange = EDScaleXExit
          TabOrder = 2
          OnChange = EDScaleYChange
          OnExit = EDScaleXExit
        end
        object CBProportional: TCheckBox
          Left = 103
          Top = 102
          Width = 93
          Height = 17
          Caption = 'Proportional'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = CBProportionalClick
        end
        object EDExplicitWidth: TKNumberEdit
          Left = 14
          Top = 33
          Width = 51
          Height = 21
          AcceptedFormats = [neafDec, neafFloat]
          Caption = 'Explicit width:'
          CustomSuffix = 'pt'
          DecimalSeparator = ','
          Options = [neoLowerCase, neoUseLabel, neoUsePrefix, neoUseUpDown]
          Precision = 1
          Value = 100.000000000000000000
          OnUpDownChange = EDScaleXExit
          TabOrder = 5
          OnChange = EDScaleXExit
          OnExit = EDScaleXExit
        end
        object EDExplicitHeight: TKNumberEdit
          Left = 14
          Top = 73
          Width = 51
          Height = 21
          AcceptedFormats = [neafDec, neafFloat]
          Caption = 'Explicit height:'
          CustomSuffix = 'pt'
          DecimalSeparator = ','
          Options = [neoLowerCase, neoUseLabel, neoUsePrefix, neoUseUpDown]
          Precision = 1
          Value = 100.000000000000000000
          OnUpDownChange = EDScaleXExit
          TabOrder = 7
          OnChange = EDScaleXExit
          OnExit = EDScaleXExit
        end
        object BUResetOriginalSize: TButton
          Left = 14
          Top = 100
          Width = 70
          Height = 22
          Caption = 'Reset'
          TabOrder = 9
          OnClick = BUResetOriginalSizeClick
        end
      end
      object GBWrap: TGroupBox
        Left = 8
        Top = 244
        Width = 196
        Height = 114
        Caption = 'Content floating'
        TabOrder = 2
        object RBWrapAround: TRadioButton
          Left = 14
          Top = 20
          Width = 163
          Height = 17
          Caption = 'Float on both sides'
          TabOrder = 0
          OnClick = EDScaleXExit
        end
        object RBWrapAroundLeft: TRadioButton
          Left = 14
          Top = 43
          Width = 155
          Height = 17
          Caption = 'Float on left side'
          TabOrder = 1
          OnClick = EDScaleXExit
        end
        object RBWrapAroundRight: TRadioButton
          Left = 14
          Top = 66
          Width = 155
          Height = 17
          Caption = 'Float on right side'
          TabOrder = 2
          OnClick = EDScaleXExit
        end
        object RBWrapTopBottom: TRadioButton
          Left = 14
          Top = 89
          Width = 163
          Height = 17
          Caption = 'No float'
          TabOrder = 3
          OnClick = EDScaleXExit
        end
      end
    end
    object TSAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 1
      ExplicitLeft = 8
      ExplicitTop = 29
      object GBShading: TGroupBox
        Left = 7
        Top = 9
        Width = 193
        Height = 129
        Caption = 'Borders and shading'
        TabOrder = 0
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
          Left = 18
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
          Precision = 1
          OnUpDownChange = EDScaleXExit
          TabOrder = 0
          OnExit = EDScaleXExit
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
          OnClick = EDScaleXExit
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
          OnClick = EDScaleXExit
          ColorDlgOptions = []
        end
      end
      object GBCrop: TGroupBox
        Left = 7
        Top = 143
        Width = 193
        Height = 162
        Caption = 'Crop'
        TabOrder = 1
        object LBCropLeft: TLabel
          Left = 18
          Top = 27
          Width = 47
          Height = 13
          Caption = 'Left crop:'
        end
        object LBCropRight: TLabel
          Left = 18
          Top = 57
          Width = 53
          Height = 13
          Caption = 'Right crop:'
        end
        object LBCropTop: TLabel
          Left = 18
          Top = 91
          Width = 46
          Height = 13
          Caption = 'Top crop:'
        end
        object LBCropBottom: TLabel
          Left = 18
          Top = 121
          Width = 62
          Height = 13
          Caption = 'Bottom crop:'
        end
        object EDCropLeft: TKNumberEdit
          Left = 98
          Top = 24
          Width = 55
          Height = 21
          AcceptedFormats = [neafDec, neafFloat]
          CustomSuffix = 'pt'
          DecimalSeparator = ','
          Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
          Precision = 1
          OnUpDownChange = EDScaleXExit
          TabOrder = 0
          OnExit = EDScaleXExit
        end
        object EDCropRight: TKNumberEdit
          Left = 98
          Top = 54
          Width = 55
          Height = 21
          AcceptedFormats = [neafDec, neafFloat]
          CustomSuffix = 'pt'
          DecimalSeparator = ','
          Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
          Precision = 1
          OnUpDownChange = EDScaleXExit
          TabOrder = 2
          OnExit = EDScaleXExit
        end
        object EDCropTop: TKNumberEdit
          Left = 98
          Top = 88
          Width = 55
          Height = 21
          AcceptedFormats = [neafDec, neafFloat]
          CustomSuffix = 'pt'
          DecimalSeparator = ','
          Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
          Precision = 1
          OnUpDownChange = EDScaleXExit
          TabOrder = 4
          OnExit = EDScaleXExit
        end
        object EDCropBottom: TKNumberEdit
          Left = 98
          Top = 118
          Width = 55
          Height = 21
          AcceptedFormats = [neafDec, neafFloat]
          CustomSuffix = 'pt'
          DecimalSeparator = ','
          Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
          Precision = 1
          OnUpDownChange = EDScaleXExit
          TabOrder = 6
          OnExit = EDScaleXExit
        end
      end
    end
  end
  object GBPreview: TGroupBox
    Left = 231
    Top = 8
    Width = 285
    Height = 389
    Caption = 'Image preview'
    TabOrder = 4
    object MEPreview: TKMemo
      Left = 2
      Top = 15
      Width = 281
      Height = 372
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
  object ODMain: TOpenPictureDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 296
    Top = 392
  end
end
