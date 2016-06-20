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
    Top = 399
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BUCancel: TButton
    Left = 441
    Top = 399
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object BUBrowse: TButton
    Left = 8
    Top = 399
    Width = 116
    Height = 25
    Caption = 'Browse...'
    TabOrder = 2
    OnClick = BUBrowseClick
  end
  object PCMain: TPageControl
    Left = 8
    Top = 8
    Width = 508
    Height = 385
    ActivePage = TSBasic
    TabOrder = 3
    object TSBasic: TTabSheet
      Caption = 'Basic'
      object GBPreview: TGroupBox
        Left = 210
        Top = 3
        Width = 285
        Height = 348
        Caption = 'Image preview'
        TabOrder = 0
        object IMPreview: TImage
          Left = 2
          Top = 15
          Width = 281
          Height = 331
          Align = alClient
          Center = True
          Proportional = True
          Stretch = True
          ExplicitLeft = 0
          ExplicitTop = 47
          ExplicitHeight = 316
        end
      end
      object GBPosition: TGroupBox
        Left = 8
        Top = 3
        Width = 196
        Height = 98
        Caption = 'Position'
        TabOrder = 1
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
          Caption = 'Horz. offset:'
          CustomSuffix = 'pt'
          DecimalSeparator = ','
          Max = 10000.000000000000000000
          Min = -10000.000000000000000000
          TabOrder = 3
        end
        object EDOffsetY: TKNumberEdit
          Left = 126
          Top = 67
          Width = 43
          Height = 21
          Caption = 'Vert. offset:'
          CustomSuffix = 'pt'
          DecimalSeparator = ','
          Max = 10000.000000000000000000
          Min = -10000.000000000000000000
          TabOrder = 5
        end
      end
      object GBSize: TGroupBox
        Left = 8
        Top = 107
        Width = 196
        Height = 86
        Caption = 'Size'
        TabOrder = 2
        object EDScaleX: TKNumberEdit
          Left = 14
          Top = 33
          Width = 51
          Height = 21
          Caption = 'Horz. scale:'
          CustomSuffix = '%'
          DecimalSeparator = ','
          Min = 1.000000000000000000
          Value = 100.000000000000000000
          TabOrder = 0
          OnChange = EDScaleXChange
        end
        object EDScaleY: TKNumberEdit
          Left = 117
          Top = 33
          Width = 52
          Height = 21
          Caption = 'Vert. scale:'
          CustomSuffix = '%'
          DecimalSeparator = ','
          Min = 1.000000000000000000
          Value = 100.000000000000000000
          TabOrder = 2
          OnChange = EDScaleYChange
        end
        object CBProportional: TCheckBox
          Left = 14
          Top = 60
          Width = 97
          Height = 17
          Caption = 'Proportional'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = EDScaleXChange
        end
      end
      object GBWrap: TGroupBox
        Left = 8
        Top = 199
        Width = 196
        Height = 114
        Caption = 'Content floating'
        TabOrder = 3
        object RBWrapAround: TRadioButton
          Left = 14
          Top = 20
          Width = 163
          Height = 17
          Caption = 'Float on both sides'
          TabOrder = 0
        end
        object RBWrapAroundLeft: TRadioButton
          Left = 14
          Top = 43
          Width = 155
          Height = 17
          Caption = 'Float on left side'
          TabOrder = 1
        end
        object RBWrapAroundRight: TRadioButton
          Left = 14
          Top = 66
          Width = 155
          Height = 17
          Caption = 'Float on right side'
          TabOrder = 2
        end
        object RBWrapTopBottom: TRadioButton
          Left = 14
          Top = 89
          Width = 163
          Height = 17
          Caption = 'No float'
          TabOrder = 3
        end
      end
    end
    object TSAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 1
      object GBShading: TGroupBox
        Left = 8
        Top = 8
        Width = 359
        Height = 97
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
          Left = 183
          Top = 56
          Width = 42
          Height = 13
          Caption = 'Shading:'
        end
        object EDBorderWidth: TKNumberEdit
          Left = 98
          Top = 24
          Width = 55
          Height = 21
          CustomSuffix = 'pt'
          DecimalSeparator = ','
          Max = 20.000000000000000000
          Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
          TabOrder = 0
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
          ColorDlgOptions = []
        end
        object CLBShading: TKColorButton
          Left = 271
          Top = 51
          Width = 70
          Height = 25
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          ColorDlgOptions = []
        end
      end
      object GBCrop: TGroupBox
        Left = 8
        Top = 111
        Width = 359
        Height = 100
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
          Left = 183
          Top = 27
          Width = 46
          Height = 13
          Caption = 'Top crop:'
        end
        object LBCropBottom: TLabel
          Left = 183
          Top = 57
          Width = 62
          Height = 13
          Caption = 'Bottom crop:'
        end
        object EDCropLeft: TKNumberEdit
          Left = 98
          Top = 24
          Width = 55
          Height = 21
          CustomSuffix = 'pt'
          DecimalSeparator = ','
          Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
          TabOrder = 0
        end
        object EDCropRight: TKNumberEdit
          Left = 98
          Top = 54
          Width = 55
          Height = 21
          CustomSuffix = 'pt'
          DecimalSeparator = ','
          Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
          TabOrder = 2
        end
        object EDCropTop: TKNumberEdit
          Left = 271
          Top = 24
          Width = 55
          Height = 21
          CustomSuffix = 'pt'
          DecimalSeparator = ','
          Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
          TabOrder = 4
        end
        object EDCropBottom: TKNumberEdit
          Left = 271
          Top = 54
          Width = 55
          Height = 21
          CustomSuffix = 'pt'
          DecimalSeparator = ','
          Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
          TabOrder = 6
        end
      end
    end
  end
  object ODMain: TOpenPictureDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 296
    Top = 392
  end
end
