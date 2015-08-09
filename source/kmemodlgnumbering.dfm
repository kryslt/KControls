object KMemoNumberingForm: TKMemoNumberingForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Bullets and numbering'
  ClientHeight = 298
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object BUOk: TButton
    Left = 211
    Top = 263
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BUCancel: TButton
    Left = 292
    Top = 263
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object RGNumbering: TRadioGroup
    Left = 8
    Top = 8
    Width = 359
    Height = 161
    Caption = 'Numbering style'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Bullets'
      'Arabic (1,2,3)'
      'Letter Lo (a,b,c)'
      'Letter Hi (A,B,C)'
      'Roman Lo (i,ii,iii)'
      'Roman Hi (I,II,III)')
    TabOrder = 2
  end
  object GBOptions: TGroupBox
    Left = 8
    Top = 175
    Width = 359
    Height = 82
    Caption = 'Options'
    TabOrder = 3
    object LBStartAt: TLabel
      Left = 169
      Top = 27
      Width = 53
      Height = 13
      Caption = 'Start from:'
    end
    object LBListLevel: TLabel
      Left = 9
      Top = 27
      Width = 45
      Height = 13
      Caption = 'List level:'
    end
    object EDStartAt: TKNumberEdit
      Left = 257
      Top = 24
      Width = 55
      Height = 21
      DecimalSeparator = ','
      Options = [neoLowerCase, neoUsePrefix, neoUseUpDown, neoWarning]
      TabOrder = 0
      Value = 1.000000000000000000
    end
    object CoBListLevel: TComboBox
      Left = 9
      Top = 43
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'first'
        'second'
        'third'
        'fourth'
        'fifth'
        'sixth'
        'seventh'
        'eighth'
        'ninth')
    end
  end
end
