object MainForm: TMainForm
  Left = 613
  Height = 557
  Top = 261
  Width = 696
  Caption = 'KDBGrid demo'
  ClientHeight = 557
  ClientWidth = 696
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  Position = poScreenCenter
  LCLVersion = '1.4.2.0'
  object Label1: TLabel
    Left = 8
    Height = 13
    Top = 8
    Width = 88
    Caption = 'Connection string:'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label2: TLabel
    Left = 8
    Height = 13
    Top = 48
    Width = 30
    Caption = 'Table:'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object Label3: TLabel
    Left = 184
    Height = 13
    Top = 48
    Width = 90
    Caption = 'First column value:'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
  end
  object EDConnectionString: TEdit
    Left = 8
    Height = 21
    Top = 24
    Width = 630
    Anchors = [akTop, akLeft, akRight]
    TabOrder = 0
  end
  object EDTable: TEdit
    Left = 8
    Height = 21
    Top = 64
    Width = 156
    TabOrder = 1
  end
  object EDFirstCol: TDBEdit
    Left = 184
    Height = 21
    Top = 64
    Width = 121
    DataSource = DSMain
    CharCase = ecNormal
    MaxLength = 0
    TabOrder = 2
  end
  object BUOpen: TButton
    Left = 8
    Height = 25
    Top = 104
    Width = 75
    Action = ACOpen
    TabOrder = 3
  end
  object BUClose: TButton
    Left = 89
    Height = 25
    Top = 104
    Width = 75
    Action = ACClose
    TabOrder = 4
  end
  object DBNav: TDBNavigator
    Left = 184
    Height = 25
    Top = 104
    Width = 240
    BevelOuter = bvNone
    ChildSizing.EnlargeHorizontal = crsScaleChilds
    ChildSizing.EnlargeVertical = crsScaleChilds
    ChildSizing.ShrinkHorizontal = crsScaleChilds
    ChildSizing.ShrinkVertical = crsScaleChilds
    ChildSizing.Layout = cclLeftToRightThenTopToBottom
    ChildSizing.ControlsPerLine = 100
    ClientHeight = 25
    ClientWidth = 240
    DataSource = DSMain
    Options = []
    TabOrder = 5
  end
  object BUPrint: TButton
    Left = 536
    Height = 25
    Top = 73
    Width = 99
    Action = ACPrint
    TabOrder = 6
  end
  object BUAutoSize: TButton
    Left = 328
    Height = 25
    Top = 62
    Width = 89
    Caption = 'Autosize row'
    OnClick = BUAutoSizeClick
    TabOrder = 7
  end
  object DBGrid: TKDBGrid
    Left = 0
    Height = 415
    Top = 142
    Width = 696
    Align = alBottom
    Anchors = [akTop, akLeft, akRight, akBottom]
    DBOptions = [dboAutoMoveRecord, dboAutoSizeBooleanCells, dboDontClearFixedCells, dboImageHint, dboIndexFixedCol, dboIndicateActiveRecord]
    Columns = <    
      item
        Font.Color = clFuchsia
        Font.Height = -11
        Font.Name = 'Tahoma'
        HorzAlign = halCenter
        Title = 'Index'
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
      end    
      item
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        HorzAlign = halRight
        HorzPadding = 5
        FieldName = 'width'
        Title = 'Width'
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
      end    
      item
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        HorzAlign = halRight
        HorzPadding = 5
        FieldName = 'height'
        Title = 'Height'
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
      end    
      item
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        FieldName = 'image'
        Title = 'Image'
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
      end    
      item
        Extent = 200
        CellHint = True
        Font.Color = clGreen
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        FieldName = 'description'
        Title = 'Description'
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
      end>
    DataSource = DSMain
    Options = [goAlwaysShowEditor, goClippedCells, goColMoving, goColSizing, goDrawFocusSelected, goEditing, goEnterMoves, goFixedHorzLine, goFixedVertLine, goHeader, goHeaderAlignment, goHorzLine, goIndicateHiddenCells, goMouseCanHideCells, goMouseOverCells, goRowSelect, goRowSizing, goRowSorting, goTabs, goThemes, goThemedCells, goVertLine]
    OptionsEx = [gxEnterWraps, gxFixedCellClickSelect, gxTabWraps, gxMouseWheelScroll]
    RowCount = 2
    TabOrder = 8
    OnCustomSortRows = DBGridCustomSortRows
    OnDrawCell = DBGridDrawCell
    OnEditorCreate = DBGridEditorCreate
    ColWidths = (
      64
      64
      64
      64
      200
    )
    RowHeights = (
      21
      21
    )
  end
  object ALMain: TActionList
    left = 440
    top = 56
    object ACOpen: TAction
      Caption = 'Open table'
      OnExecute = ACOpenExecute
      OnUpdate = ACOpenUpdate
    end
    object ACClose: TAction
      Caption = 'Close table'
      OnExecute = ACCloseExecute
      OnUpdate = ACCloseUpdate
    end
    object ACPrint: TAction
      Caption = 'Print...'
      OnExecute = ACPrintExecute
      OnUpdate = ACPrintUpdate
    end
  end
  object DSMain: TDataSource
    left = 480
    top = 56
  end
  object PSDMain: TKPrintSetupDialog
    Control = DBGrid
    SelAvail = False
    left = 416
    top = 96
  end
end
