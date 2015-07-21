object MainForm: TMainForm
  Left = 566
  Top = 256
  Caption = 'KMemoDemo'
  ClientHeight = 406
  ClientWidth = 980
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 500
    Top = 0
    Height = 406
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 500
    Height = 406
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 503
    Top = 0
    Width = 477
    Height = 406
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
end
