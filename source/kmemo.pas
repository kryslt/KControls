
{ @abstract(This unit contains native replacement for TMemo/TRichEdit components)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(28 Apr 2009)
  @lastmod(30 July 2015)

  Copyright © Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. All redistributions
  of the original or modified source code must retain the original copyright
  notice. The Author accepts no liability for any damage that may result
  from using this code.
}

unit kmemo; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Contnrs, Types, ActnList,
  ExtCtrls, StdCtrls, Forms, KFunctions, KControls, KGraphics, KEditCommon;

const
  { Minimum for the @link(TKCustomMemo.UndoLimit) property. }
  cUndoLimitMin = 100;
  { Maximum for the @link(TKCustomMemo.UndoLimit) property. }
  cUndoLimitMax = 10000;
  { Default value for the @link(TKCustomMemo.UndoLimit) property. }
  cUndoLimitDef = 1000;

  { Minimum for the @link(TKCustomMemo.ScrollPadding) property. }
  cScrollPaddingMin = 0;
  { Maximum for the @link(TKCustomMemo.ScrollPadding) property. }
  cScrollPaddingMax = 1000;
  { Default value for the @link(TKCustomMemo.ScrollPadding) property. }
  cScrollPaddingDef = 30;

  { Minimum for the @link(TKCustomMemo.ScrollSpeed) property. }
  cScrollSpeedMin = 50;
  { Maximum for the @link(TKCustomMemo.ScrollSpeed) property. }
  cScrollSpeedMax = 1000;
  { Default value for the @link(TKCustomMemo.ScrollSpeed) property. }
  cScrollSpeedDef = 100;

  { Default value for the @link(TKMemoColors.BkGnd) color property. }
  cBkGndDef = clWindow;
  { Default value for the @link(TKMemoColors.InactiveCaretBkGnd) color property. }
  cInactiveCaretBkGndDef = clBlack;
  { Default value for the @link(TKMemoColors.InactiveCaretSelBkGnd) color property. }
  cInactiveCaretSelBkGndDef = clBlack;
  { Default value for the @link(TKMemoColors.InactiveCaretSelText) color property. }
  cInactiveCaretSelTextDef = clYellow;
  { Default value for the @link(TKMemoColors.InactiveCaretText) color property. }
  cInactiveCaretTextDef = clYellow;
  { Default value for the @link(TKMemoColors.SelBkGnd) color property. }
  cSelBkGndDef = clGrayText;
  { Default value for the @link(TKMemoColors.SelBkGndFocused) color property. }
  cSelBkGndFocusedDef = clHighlight;
  { Default value for the @link(TKMemoColors.SelText) color property. }
  cSelTextDef = clHighlightText;
  { Default value for the @link(TKMemoColors.SelTextFocused) color property. }
  cSelTextFocusedDef = clHighlightText;

  { Index for the @link(TKMemoColors.BkGnd) color property. }
  ciBkGnd = TKColorIndex(0);
  { Index for the @link(TKMemoColors.InactiveCaretBkGnd) color property. }
  ciInactiveCaretBkGnd = TKColorIndex(1);
  { Index for the @link(TKMemoColors.InactiveCaretSelBkGnd) color property. }
  ciInactiveCaretSelBkGnd = TKColorIndex(2);
  { Index for the @link(TKMemoColors.InactiveCaretSelText) color property. }
  ciInactiveCaretSelText = TKColorIndex(3);
  { Index for the @link(TKMemoColors.InactiveCaretText) color property. }
  ciInactiveCaretText = TKColorIndex(4);
  { Index for the @link(TKMemoColors.SelBkGnd) color property. }
  ciSelBkGnd = TKColorIndex(5);
  { Index for the @link(TKMemoColors.SelBkGndFocused) color property. }
  ciSelBkGndFocused = TKColorIndex(6);
  { Index for the @link(TKMemoColors.SelText) color property. }
  ciSelText = TKColorIndex(7);
  { Index for the @link(TKMemoColors.SelTextFocused) color property. }
  ciSelTextFocused = TKColorIndex(8);
  { Maximum color array index. }
  ciMemoColorsMax = ciSelTextFocused;

  cInvalidListID = -1;

  cHorzScrollStepDef = 4;

  cVertScrollStepDef = 10;

  { Default value for the @link(TKMemo.Height) property. }
  cHeight = 200;

  { Default value for the @link(TKMemo.Width) property. }
  cWidth = 300;

  { Default value for the @link(TKMemo.MaxWordLength) property. }
  cMaxWordLengthDef = 50;

  { This is the character for paragraph visualisation. }
  cNewLineChar = #$B6;
  { This is the character for space visualisation. }
  cSpaceChar = #$B7;
  { This is the character for tab visualisation. }
  cTabChar = #$2192;
  { This is the character for standard bullet. }
  cBullet = #$2022;
  { This is the character for square bullet. }
  cSquareBullet = #$25AB;
  { This is the character for arrow bullet. }
  cArrowBullet = #$25BA;

  { Default characters used to break the text words. }
  cDefaultWordBreaks = [cNULL, cSPACE, '/', '\', ';', ':', '?', '!'];

  { Format for clipboard operations. }
  cRichText = 'Rich Text Format';

  { Default value for the @link(TKMemo.Options) property. }
  cKMemoOptionsDef = [eoGroupUndo, eoScrollWindow];

type
  TKCustomMemo = class;

  TKMemoLinePosition = (
    eolInside,
    eolEnd
  );

  TKMemoBlockPosition = (
    mbpText,
    mbpRelative,
    mbpAbsolute
  );

  { Declares memo states - possible values for the @link(TKCustomHexEditor.States) property (protected). }
  TKMemoState = (
    { Caret is created. }
    elCaretCreated,
    { Caret is visible. }
    elCaretVisible,
    { Caret is being updated. }
    elCaretUpdate,
    { Ignore following WM_CHAR message. }
    elIgnoreNextChar,
    { Buffer modified. }
    elModified,
    { Mouse captured. }
    elMouseCapture,
    { Mouse captured and dragging a block. }
    elMouseDrag,
    { Overwrite mode active. }
    elOverwrite,
    { Content is being printed or previewed. }
    elPrinting,
    { Read only editor. }
    elReadOnly
  );

  { Hex editor states can be arbitrary combined. }
  TKMemoStates = set of TKMemoState;

  TKMemoUpdateReason = (
    { recalculate line info and extent. }
    muContent,
    { recalculate extent. }
    muExtent,
    { selection changed. }
    muSelection,
    { selection changed and scroll operation is required to reflect the change. }
    muSelectionScroll
  );

  TKMemoUpdateReasons = set of TKMemoUpdateReason;

  TKMemoSparseItem = class(TKObject)
  private
    FIndex: Integer;
  public
    constructor Create; override;
    procedure Assign(ASource: TKObject); override;
    function EqualProperties(ASource: TKObject): Boolean; override;
    property Index: Integer read FIndex write FIndex;
  end;

  TKMemoSparseList = class(TKObjectList)
  private
    function GetItem(Index: Integer): TKMemoSparseItem;
    procedure SetItem(Index: Integer; const Value: TKMemoSparseItem);
  public
    procedure AddItem(AValue: Integer);
    procedure SetSize(ACount: Integer); virtual;
    property Items[Index: Integer]: TKMemoSparseItem read GetItem write SetItem; default;
  end;

  TKMemoSparseStack = class(TStack)
  public
    function Push(AObject: TKMemoSparseItem): TKMemoSparseItem;
    function Pop: TKMemoSparseItem;
    function Peek: TKMemoSparseItem;
    procedure PushValue(Value: Integer);
    function PopValue: Integer;
  end;

  TKMemoDictionaryItem = class(TKObject)
  private
    FIndex,
    FValue: Integer;
  public
    constructor Create; override;
    procedure Assign(ASource: TKObject); override;
    function EqualProperties(ASource: TKObject): Boolean; override;
    property Index: Integer read FIndex write FIndex;
    property Value: Integer read FValue write FValue;
  end;

  TKMemoDictionary = class(TKObjectList)
  private
    function GetItem(Index: Integer): TKMemoDictionaryItem;
    procedure SetItem(Index: Integer; const Value: TKMemoDictionaryItem);
  public
    procedure AddItem(AIndex, AValue: Integer);
    function FindItem(AIndex: Integer): TKMemoDictionaryItem;
    function GetValue(AIndex, ADefault: Integer): Integer;
    procedure SetValue(AIndex, AValue: Integer);
    property Items[Index: Integer]: TKMemoDictionaryItem read GetItem write SetItem; default;
  end;

  TKMemoParaNumbering = (pnuNone, pnuBullets, pnuArabic, pnuLetterLo, pnuLetterHi, pnuRomanLo, pnuRomanHi);

  TKMemoNumberingFormatItem = class(TKObject)
  private
    FLevel: Integer;
    FText: TKString;
  public
    constructor Create; override;
    procedure Assign(ASource: TKObject); override;
    property Level: Integer read FLevel write FLevel;
    property Text: TKString read FText write FText;
  end;

  TKMemoNumberingFormat = class(TKObjectList)
  private
    function GetItem(Index: Integer): TKMemoNumberingFormatItem;
    procedure SetItem(Index: Integer; const Value: TKMemoNumberingFormatItem);
    function GetLevelCount: Integer;
  public
    procedure AddItem(ALevel: Integer; const AText: TKString);
    procedure Defaults(ANumbering: TKMemoParaNumbering; ALevelIndex: Integer);
    procedure InsertItem(AAt, ALevel: Integer; const AText: TKString);
    property Items[Index: Integer]: TKMemoNumberingFormatItem read GetItem write SetItem; default;
    property LevelCount: Integer read GetLevelCount;
  end;

  TKMemoListLevels = class;

  TKMemoListLevel = class(TKObject)
  private
    FFirstIndent: Integer;
    FNumbering: TKMemoParaNumbering;
    FNumberingFont: TFont;
    FNumberingFormat: TKMemoNumberingFormat;
    FNumberStartAt: Integer;
    FLeftIndent: Integer;
    FLevelCounter: Integer;
    procedure SetNumbering(const Value: TKMemoParaNumbering);
    procedure SetNumberStartAt(const Value: Integer);
    procedure SetFirstIndent(const Value: Integer);
    procedure SetLeftPadding(const Value: Integer);
  protected
    FNumberingFontChanged: Boolean;
    procedure FontChanged(Sender: TObject);
    procedure Changed; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TKObject); override;
    property LevelCounter: Integer read FLevelCounter write FLevelCounter;
    property FirstIndent: Integer read FFirstIndent write SetFirstIndent;
    property LeftIndent: Integer read FLeftIndent write SetLeftPadding;
    property Numbering: TKMemoParaNumbering read FNumbering write SetNumbering;
    property NumberingFont: TFont read FNumberingFont;
    property NumberingFontChanged: Boolean read FNumberingFontChanged;
    property NumberingFormat: TKMemoNumberingFormat read FNumberingFormat;
    property NumberStartAt: Integer read FNumberStartAt write SetNumberStartAt;
  end;

  TKMemoList = class;

  TKMemoListLevels = class(TKObjectList)
  private
    FParent: TKMemoList;
    function GetItem(Index: Integer): TKMemoListLevel;
    procedure SetItem(Index: Integer; const Value: TKMemoListLevel);
  public
    constructor Create; override;
    procedure Changed(ALevel: TKMemoListLevel); virtual;
    procedure ClearLevelCounters(AFromLevel: Integer); virtual;
    property Items[Index: Integer]: TKMemoListLevel read GetItem write SetItem; default;
    property Parent: TKMemoList read FParent write FParent;
  end;

  TKMemoListTable = class;

  TKMemoList = class(TKObject)
  private
    FID: Integer;
    FLevels: TKMemoListLevels;
  protected
    procedure ParentChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TKObject); override;
    procedure LevelChanged(ALevel: TKMemoListLevel); virtual;
    property ID: Integer read FID write FID;
    property Levels: TKMemoListLevels read FLevels;
  end;

  TKMemoListChangedEvent = procedure(AList: TKMemoList; ALevel: TKMemoListLevel) of object;

  TKMemoListTable = class(TKObjectList)
  private
    FOnChanged: TKMemoListChangedEvent;
    function GetItem(Index: Integer): TKMemoList;
    procedure SetItem(Index: Integer; const Value: TKMemoList);
  protected
    FCallUpdate: Boolean;
    procedure CallAfterUpdate; override;
    procedure CallBeforeUpdate; override;
    procedure DoChanged(AList: TKMemoList; ALevel: TKMemoListLevel); virtual;
  public
    constructor Create; override;
    procedure ClearLevelCounters;
    function FindByID(AListID: Integer): TKMemoList;
    procedure ListChanged(AList: TKMemoList; ALevel: TKMemoListLevel); virtual;
    function ListByNumbering(AListID, ALevelIndex: Integer; ANumbering: TKMemoParaNumbering): TKMemoList; virtual;
    function NextID: Integer;
    property Items[Index: Integer]: TKMemoList read GetItem write SetItem; default;
    property OnChanged: TKMemoListChangedEvent read FOnChanged write FOnChanged;
  end;

  TKMemoBackground = class(TKPersistent)
  private
    FImage: TPicture;
    FRepeatX: Boolean;
    FRepeatY: Boolean;
    FColor: TColor;
    FOnChanged: TNotifyEvent;
    procedure SetImage(const Value: TPicture);
    procedure SetRepeatX(const Value: Boolean);
    procedure SetRepeatY(const Value: Boolean);
    procedure SetColor(const Value: TColor);
  protected
    procedure ImageChanged(Sender: TObject);
    procedure Update; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure Clear;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Color: TColor read FColor write SetColor default clNone;
    property Image: TPicture read FImage write SetImage;
    property RepeatX: Boolean read FRepeatX write SetRepeatX default True;
    property RepeatY: Boolean read FRepeatY write SetRepeatY default True;
  end;

  TKMemoScriptCapitals = (tcaNone, tcaNormal, tcaSmall);

  TKMemoScriptPosition = (tpoNormal, tpoSuperscript, tpoSubscript);

  { TKMemoTextStyle }

  TKMemoTextStyle = class(TKPersistent)
  private
    FAllowBrush: Boolean;
    FBrush: TBrush;
    FCapitals: TKMemoScriptCapitals;
    FChangeable: Boolean;
    FFont: TFont;
    FScriptPosition: TKMemoScriptPosition;
    FStyleChanged: Boolean;
    FOnChanged: TNotifyEvent;
    procedure SetAllowBrush(const Value: Boolean);
    procedure SetBrush(const Value: TBrush);
    procedure SetCapitals(const Value: TKMemoScriptCapitals);
    procedure SetFont(const Value: TFont);
    procedure SetScriptPosition(const Value: TKMemoScriptPosition);
  protected
    FBrushChanged: Boolean;
    FFontChanged: Boolean;
    procedure BrushChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure PropsChanged; virtual;
    procedure Update; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure AssignNC(ASource: TKMemoTextStyle); virtual;
    procedure Defaults; virtual;
    function EqualProperties(ASource: TKMemoTextStyle): Boolean; virtual;
    procedure NotifyChange(AValue: TKMemoTextStyle); virtual;
    property AllowBrush: Boolean read FAllowBrush write SetAllowBrush;
    property Capitals: TKMemoScriptCapitals read FCapitals write SetCapitals;
    property Changeable: Boolean read FChangeable write FChangeable;
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property ScriptPosition: TKMemoScriptPosition read FScriptPosition write SetScriptPosition;
    property StyleChanged: Boolean read FStyleChanged write FStyleChanged;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TKMemoBlockWrapMode = (
    { Text wraps around block bounding rectangle on both sides. }
    wrAround,
    { Text wraps around block bounding rectangle only on left side. }
    wrAroundLeft,
    { Text wraps around block bounding rectangle only on right side. }
    wrAroundRight,
    { Text wraps tightly around block. }
    wrTight,
    { Text wraps tightly around block only on left side. }
    wrTightLeft,
    { Text wraps tightly around block only on right side. }
    wrTightRight,
    { Text does not wrap around block on left or right side. }
    wrTopBottom,
    { Text wraps as block was not present. }
    wrNone,
    { Text wrap not specified. }
    wrUnknown
  );

  TKMemoBlockStyleChangedEvent = procedure(Sender: TObject; AReasons: TKMemoUpdateReasons) of object;

  { TKMemoBlockStyle }

  TKMemoBlockStyle = class(TKPersistent)
  private
    FBrush: TBrush;
    FBorderRadius: Integer;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FBorderWidths: TKRect;
    FChangeable: Boolean;
    FStyleChanged: Boolean;
    FContentMargin: TKRect;
    FContentPadding: TKRect;
    FFillBlip: TGraphic;
    FHAlign: TKHAlign;
    FWrapMode: TKMemoBlockWrapMode;
    FOnChanged: TKMemoBlockStyleChangedEvent;
    function GetBottomPadding: Integer;
    function GetLeftPadding: Integer;
    function GetRightPadding: Integer;
    function GetTopPadding: Integer;
    procedure SetBottomPadding(const Value: Integer);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Integer);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetBorderWidths(const Value: TKRect);
    procedure SetBrush(const Value: TBrush);
    procedure SetContentPadding(const Value: TKRect);
    procedure SetFillBlip(const Value: TGraphic);
    procedure SetHAlign(const Value: TKHAlign);
    procedure SetLeftPadding(const Value: Integer);
    procedure SetRightPadding(const Value: Integer);
    procedure SetTopPadding(const Value: Integer);
    procedure SetWrapMode(const Value: TKMemoBlockWrapMode);
    procedure SetContentMargin(const Value: TKRect);
    function GetBottomMargin: Integer;
    function GetLeftMargin: Integer;
    function GetRightMargin: Integer;
    function GetTopMargin: Integer;
    procedure SetBottomMargin(const Value: Integer);
    procedure SetLeftMargin(const Value: Integer);
    procedure SetRightMargin(const Value: Integer);
    procedure SetTopMargin(const Value: Integer);
    function GetBottomBorderWidth: Integer;
    function GetLeftBorderWidth: Integer;
    function GetRightBorderWidth: Integer;
    function GetTopBorderWidth: Integer;
    function GetAllPaddingsBottom: Integer;
    function GetAllPaddingsLeft: Integer;
    function GetAllPaddingsRight: Integer;
    function GetAllPaddingsTop: Integer;
  protected
    FUpdateReasons: TKMemoUpdateReasons;
    procedure BrushChanged(Sender: TObject);
    procedure PropsChanged(AReasons: TKMemoUpdateReasons); virtual;
    procedure Update; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure AssignNC(ASource: TKMemoBlockStyle); virtual;
    function BorderRect(const ARect: TRect): TRect; virtual;
    function InteriorRect(const ARect: TRect): TRect; virtual;
    procedure Defaults; virtual;
    function MarginRect(const ARect: TRect): TRect; virtual;
    procedure NotifyChange(AValue: TKMemoBlockStyle); virtual;
    procedure PaintBox(ACanvas: TCanvas; const ARect: TRect); virtual;
    property AllPaddingsBottom: Integer read GetAllPaddingsBottom;
    property AllPaddingsLeft: Integer read GetAllPaddingsLeft;
    property AllPaddingsRight: Integer read GetAllPaddingsRight;
    property AllPaddingsTop: Integer read GetAllPaddingsTop;
    property BottomBorderWidth: Integer read GetBottomBorderWidth;
    property BottomMargin: Integer read GetBottomMargin write SetBottomMargin;
    property BottomPadding: Integer read GetBottomPadding write SetBottomPadding;
    property BorderRadius: Integer read FBorderRadius write SetBorderRadius;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property BorderWidths: TKRect read FBorderWidths write SetBorderWidths;
    property Brush: TBrush read FBrush write SetBrush;
    property Changeable: Boolean read FChangeable write FChangeable;
    property ContentMargin: TKRect read FContentMargin write SetContentMargin;
    property ContentPadding: TKRect read FContentPadding write SetContentPadding;
    property FillBlip: TGraphic read FFillBlip write SetFillBlip;
    property HAlign: TKHAlign read FHAlign write SetHAlign;
    property LeftBorderWidth: Integer read GetLeftBorderWidth;
    property LeftMargin: Integer read GetLeftMargin write SetLeftMargin;
    property LeftPadding: Integer read GetLeftPadding write SetLeftPadding;
    property RightBorderWidth: Integer read GetRightBorderWidth;
    property RightMargin: Integer read GetRightMargin write SetRightMargin;
    property RightPadding: Integer read GetRightPadding write SetRightPadding;
    property StyleChanged: Boolean read FStyleChanged write FStyleChanged;
    property TopBorderWidth: Integer read GetTopBorderWidth;
    property TopMargin: Integer read GetTopMargin write SetTopMargin;
    property TopPadding: Integer read GetTopPadding write SetTopPadding;
    property WrapMode: TKMemoBlockWrapMode read FWrapMode write SetWrapMode;
    property OnChanged: TKMemoBlockStyleChangedEvent read FOnChanged write FOnChanged;
  end;

  TKMemoLineSpacingMode = (lsmFactor, lsmValue);

  TKMemoParaStyle = class(TKMemoBlockStyle)
  private
    FFirstIndent: Integer;
    FLineSpacingFactor: Double;
    FLineSpacingMode: TKMemoLineSpacingMode;
    FLineSpacingValue: Integer;
    FNumberingList: Integer;
    FNumberingListLevel: Integer;
    FNumberStartAt: Integer;
    FWordWrap: Boolean;
    procedure SetFirstIndent(const Value: Integer);
    procedure SetLineSpacingFactor(const Value: Double);
    procedure SetLineSpacingMode(const Value: TKMemoLineSpacingMode);
    procedure SetLineSpacingValue(const Value: Integer);
    procedure SetNumberingList(const Value: Integer);
    procedure SetNumberingListLevel(const Value: Integer);
    procedure SetNumberStartAt(const Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
  public
    procedure Assign(ASource: TPersistent); override;
    procedure Defaults; override;
    procedure SetNumberingListAndLevel(AListID, ALevelIndex: Integer); virtual;
    property FirstIndent: Integer read FFirstIndent write SetFirstIndent;
    property LineSpacingFactor: Double read FLineSpacingFactor write SetLineSpacingFactor;
    property LineSpacingMode: TKMemoLineSpacingMode read FLineSpacingMode write SetLineSpacingMode;
    property LineSpacingValue: Integer read FLineSpacingValue write SetLineSpacingValue;
    property NumberingList: Integer read FNumberingList write SetNumberingList;
    property NumberingListLevel: Integer read FNumberingListLevel write SetNumberingListLevel;
    property NumberStartAt: Integer read FNumberStartAt write SetNumberStartAt;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
  end;

  TKMemoLine = class(TObject)
  private
    FEndBlock: Integer;
    FEndIndex: Integer;
    FEndWord: Integer;
    FExtent: TPoint;
    FPosition: TPoint;
    FStartBlock: Integer;
    FStartIndex: Integer;
    FStartWord: Integer;
    function GetLineRect: TRect;
  public
    constructor Create;
    property EndBlock: Integer read FEndBlock write FEndBlock;
    property EndIndex: Integer read FEndIndex write FEndIndex;
    property EndWord: Integer read FEndWord write FEndWord;
    property Extent: TPoint read FExtent write FExtent;
    property LineRect: TRect read GetLineRect;
    property Position: TPoint read FPosition write FPosition;
    property StartBlock: Integer read FStartBlock write FStartBlock;
    property StartIndex: Integer read FStartIndex write FStartIndex;
    property StartWord: Integer read FStartWord write FStartWord;
  end;

  TKMemoLines = class(TObjectList)
  private
    function GetItem(Index: Integer): TKMemoLine;
    procedure SetItem(Index: Integer; const Value: TKMemoLine);
  public
    property Items[Index: Integer]: TKMemoLine read GetItem write SetItem; default;
  end;

  TKMemoWord = class(TObject)
  private
    FBaseLine: Integer;
    FBottomPadding: Integer;
    FClipped: Boolean;
    FExtent: TPoint;
    FEndIndex: Integer;
    FPosition: TPoint;
    FStartIndex: Integer;
    FTopPadding: Integer;
  public
    constructor Create;
    procedure Clear;
    property BaseLine: Integer read FBaseLine write FBaseLine;
    property BottomPadding: Integer read FBottomPadding write FBottomPadding;
    property Clipped: Boolean read FClipped write FClipped;
    property EndIndex: Integer read FEndIndex write FEndIndex;
    property Extent: TPoint read FExtent write FExtent;
    property Position: TPoint read FPosition write FPosition;
    property StartIndex: Integer read FStartIndex write FStartIndex;
    property TopPadding: Integer read FTopPadding write FTopPadding;
  end;

  TKMemoWordList = class(TObjectList)
  private
    function GetItem(Index: Integer): TKMemoWord;
    procedure SetItem(Index: Integer; const Value: TKMemoWord);
  public
    property Items[Index: Integer]: TKMemoWord read GetItem write SetItem; default;
  end;

  TKMemoBlock = class;

  TKMemoBlocks = class;

  IKMemoNotifier = interface(IInterface)
    procedure BlockFreeNotification(ABlock: TKMemoBlock);
    procedure BlocksFreeNotification(ABlocks: TKMemoBlocks);
    function EditBlock(AItem: TKMemoBlock): Boolean;
    function GetActiveBlocks: TKMemoBlocks;
    function GetDefaultTextStyle: TKMemoTextStyle;
    function GetDefaultParaStyle: TKMemoParaStyle;
    function GetDrawSingleChars: Boolean;
    function GetLinePosition: TKMemoLinePosition;
    function GetListTable: TKMemoListTable;
    function GetMemo: TKCustomMemo;
    function GetMaxWordLength: Integer;
    function GetPaintSelection: Boolean;
    function GetPrinting: Boolean;
    function GetReadOnly: Boolean;
    procedure GetSelColors(out Foreground, Background: TColor);
    function GetSelectedBlock: TKMemoBlock;
    function GetShowFormatting: Boolean;
    function GetWordBreaks: TKSysCharSet;
    function SelectBlock(AItem: TKMemoBlock; APosition: TKSizingGripPosition): Boolean;
    procedure SetReqMouseCursor(ACursor: TCursor);
  end;

  TKMemoMouseAction = (maMove, maLeftDown, maLeftUp, maRightDown, maRightUp, maMidDown, maMidUp);

  TKMemoBlockClass = class of TKMemoBlock;

  TKMemoBlock = class(TKObject)
  private
    FOffset: TPoint;
    FPosition: TKMemoBlockPosition;
    function GetBoundsRect: TRect;
    function GetMemoNotifier: IKMemoNotifier;
    function GetPaintSelection: Boolean;
    function GetParentBlocks: TKMemoBlocks;
    function GetPrinting: Boolean;
    function GetReadOnly: Boolean;
    function GetSelEnd: Integer;
    procedure SetPosition(const Value: TKMemoBlockPosition);
    function GetParentRootBlocks: TKMemoBlocks;
  protected
    procedure CallAfterUpdate; override;
    procedure CallBeforeUpdate; override;
    procedure SizingGripsDraw(ACanvas: TCanvas; const ARect: TRect); virtual;
    function SizingGripsCursor(const ARect: TRect; const APoint: TPoint): TCursor; virtual;
    function SizingGripsPosition(const ARect: TRect; const APoint: TPoint): TKSizingGripPosition; virtual;
    function ContentLength: Integer; virtual;
    function GetBottomPadding: Integer; virtual;
    function GetCanAddText: Boolean; virtual;
    function GetWrapMode: TKMemoBlockWrapMode; virtual;
    function GetDefaultTextStyle: TKMemoTextStyle; virtual;
    function GetDefaultParaStyle: TKMemoParaStyle; virtual;
    function GetHeight: Integer; virtual;
    function GetLeft: Integer; virtual;
    function GetParaStyle: TKMemoParaStyle; virtual;
    procedure GetSelColors(out Foreground, Background: TColor); virtual;
    function GetSelLength: Integer; virtual;
    function GetSelStart: Integer; virtual;
    function GetSelText: TKString; virtual;
    function GetShowFormatting: Boolean; virtual;
    function GetSizingRect: TRect; virtual;
    function GetText: TKString; virtual;
    function GetTop: Integer; virtual;
    function GetTopPadding: Integer; virtual;
    function GetWidth: Integer; virtual;
    function GetWordBaseLine(Index: Integer): Integer; virtual;
    function GetWordBottomPadding(Index: Integer): Integer; virtual;
    function GetWordBoundsRect(Index: Integer): TRect; virtual;
    function GetWordBreakable(Index: Integer): Boolean; virtual;
    function GetWordClipped(Index: Integer): Boolean; virtual;
    function GetWordCount: Integer; virtual;
    function GetWordHeight(Index: Integer): Integer; virtual;
    function GetWordLeft(Index: Integer): Integer; virtual;
    function GetWordLength(Index: Integer): Integer; virtual;
    function GetWordLengthWOWS(Index: Integer): Integer; virtual;
    function GetWords(Index: Integer): TKString; virtual;
    function GetWordTop(Index: Integer): Integer; virtual;
    function GetWordTopPadding(Index: Integer): Integer; virtual;
    function GetWordWidth(Index: Integer): Integer; virtual;
    procedure SetLeftOffset(const Value: Integer); virtual;
    procedure SetTopOffset(Value: Integer); virtual;
    procedure SetWordBaseLine(Index: Integer; const Value: Integer); virtual;
    procedure SetWordBottomPadding(Index: Integer; const Value: Integer); virtual;
    procedure SetWordClipped(Index: Integer; const Value: Boolean); virtual;
    procedure SetWordHeight(Index: Integer; const Value: Integer); virtual;
    procedure SetWordLeft(Index: Integer; const Value: Integer); virtual;
    procedure SetWordTop(Index: Integer; const Value: Integer); virtual;
    procedure SetWordTopPadding(Index: Integer; const Value: Integer); virtual;
    procedure SetWordWidth(Index: Integer; const Value: Integer); virtual;
    procedure Update(AReasons: TKMemoUpdateReasons); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ActiveBlocks: TKMemoBlocks; virtual;
    procedure Assign(ASource: TKObject); override;
    procedure AssignAttributes(AItem: TKMemoBlock); virtual;
    function CalcAscent(ACanvas: TCanvas): Integer; virtual;
    function CanAdd(AItem: TKMemoBlock): Boolean; virtual;
    procedure ClearSelection(ATextOnly: Boolean); virtual;
    function Concat(AItem: TKMemoBlock): Boolean; virtual;
    function EqualProperties(ASource: TKObject): Boolean; override;
    procedure GetWordIndexes(AIndex: Integer; out ASt, AEn: Integer); virtual;
    function IndexToRect(ACanvas: TCanvas; AIndex: Integer; ACaret: Boolean): TRect; virtual;
    function InsertParagraph(AIndex: Integer): Boolean; virtual;
    function InsertString(const AText: TKString; At: Integer = -1): Boolean; virtual;
    function MeasureExtent(ACanvas: TCanvas; ARequiredWidth: Integer): TPoint; virtual;
    procedure NotifyDefaultTextChange; virtual;
    procedure NotifyDefaultParaChange; virtual;
    procedure NotifyOptionsChange; virtual;
    procedure NotifyPrintBegin; virtual;
    procedure NotifyPrintEnd; virtual;
    procedure PaintToCanvas(ACanvas: TCanvas; ALeft, ATop: Integer); virtual;
    function PointToIndex(ACanvas: TCanvas; const APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; virtual;
    function RealLeftOffset: Integer;
    function RealTopOffset: Integer;
    procedure Resize(ANewWidth, ANewHeight: Integer); virtual;
    procedure SelectAll; virtual;
    function Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean): Boolean; virtual;
    function SelectableLength(ALocalCalc: Boolean = False): Integer; virtual;
    function SelectedBlock: TKMemoBlock; virtual;
    function Split(At: Integer; AllowEmpty: Boolean = False): TKMemoBlock; virtual;
    function WordIndexToRect(ACanvas: TCanvas; AWordIndex: Integer; AIndex: Integer; ACaret: Boolean): TRect; virtual;
    function WordMeasureExtent(ACanvas: TCanvas; AWordIndex, ARequiredWidth: Integer): TPoint; virtual;
    function WordMouseAction(ACanvas: TCanvas; AWordIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean; virtual;
    procedure WordPaintToCanvas(ACanvas: TCanvas; AWordIndex, ALeft, ATop: Integer); virtual;
    function WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint; AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; virtual;
    property BoundsRect: TRect read GetBoundsRect;
    property BottomPadding: Integer read GetBottomPadding;
    property CanAddText: Boolean read GetCanAddText;
    property DefaultTextStyle: TKMemoTextStyle read GetDefaultTextStyle;
    property DefaultParaStyle: TKMemoParaStyle read GetDefaultParaStyle;
    property Height: Integer read GetHeight;
    property Left: Integer read GetLeft;
    property LeftOffset: Integer read FOffset.X write SetLeftOffset;
    property MemoNotifier: IKMemoNotifier read GetMemoNotifier;
    property PaintSelection: Boolean read GetPaintSelection;
    property ParaStyle: TKMemoParaStyle read GetParaStyle;
    property ParentBlocks: TKMemoBlocks read GetParentBlocks;
    property ParentRootBlocks: TKMemoBlocks read GetParentRootBlocks;
    property Position: TKMemoBlockPosition read FPosition write SetPosition;
    property Printing: Boolean read GetPrinting;
    property ReadOnly: Boolean read GetReadOnly;
    property SelEnd: Integer read GetSelEnd;
    property SelLength: Integer read GetSelLength;
    property SelStart: Integer read GetSelStart;
    property SelText: TKString read GetSelText;
    property ShowFormatting: Boolean read GetShowFormatting;
    property SizingRect: TRect read GetSizingRect;
    property Text: TKString read GetText;
    property Top: Integer read GetTop;
    property TopOffset: Integer read FOffset.Y write SetTopOffset;
    property TopPadding: Integer read GetTopPadding;
    property Width: Integer read GetWidth;
    property WordCount: Integer read GetWordCount;
    property WordBaseLine[Index: Integer]: Integer read GetWordBaseLine write SetWordBaseLine;
    property WordBreakable[Index: Integer]: Boolean read GetWordBreakable;
    property WordBottomPadding[Index: Integer]: Integer read GetWordBottomPadding write SetWordBottomPadding;
    property WordBoundsRect[Index: Integer]: TRect read GetWordBoundsRect;
    property WordClipped[Index: Integer]: Boolean read GetWordClipped write SetWordClipped;
    property WordHeight[Index: Integer]: Integer read GetWordHeight write SetWordHeight;
    property WordLeft[Index: Integer]: Integer read GetWordLeft write SetWordLeft;
    property WordLength[Index: Integer]: Integer read GetWordLength;
    property WordLengthWOWS[Index: Integer]: Integer read GetWordLengthWOWS;
    property Words[Index: Integer]: TKString read GetWords;
    property WordTop[Index: Integer]: Integer read GetWordTop write SetWordTop;
    property WordTopPadding[Index: Integer]: Integer read GetWordTopPadding write SetWordTopPadding;
    property WordWidth[Index: Integer]: Integer read GetWordWidth write SetWordWidth;
    property WrapMode: TKMemoBlockWrapMode read GetWrapMode;
  end;

  TKMemoSingleton = class(TKMemoBlock)
  private
    FSelEnd: Integer;
    FSelStart: Integer;
  protected
    function GetSelLength: Integer; override;
    function GetSelStart: Integer; override;
  public
    constructor Create; override;
    function Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean): Boolean; override;
  end;

  TKMemoTextBlock = class(TKMemoSingleton)
  private
    FText: TKString;
    FTextStyle: TKMemoTextStyle;
    function GetWordBreaks: TKSysCharSet;
  protected
    FScriptVertOffset: Integer;
    FScriptFontHeight: Integer;
    FTextLength: Integer;
    FWordCount: Integer;
    FWords: TKMemoWordList;
    function ApplyFormatting(const AText: TKString): TKString;
    procedure ApplyTextStyle(ACanvas: TCanvas); virtual;
    function ContentLength: Integer; override;
    function DrawSingleChars: Boolean; virtual;
    function GetCanAddText: Boolean; override;
    function GetKerningDistance(ACanvas: TCanvas; const AChar1, AChar2: TKChar): Integer;
    function GetSelText: TKString; override;
    function GetText: TKString; override;
    function GetWordBaseLine(Index: Integer): Integer; override;
    function GetWordBottomPadding(Index: Integer): Integer; override;
    function GetWordBoundsRect(Index: Integer): TRect; override;
    function GetWordBreakable(Index: Integer): Boolean; override;
    function GetWordClipped(Index: Integer): Boolean; override;
    function GetWordCount: Integer; override;
    function GetWordHeight(Index: Integer): Integer; override;
    function GetWordLeft(Index: Integer): Integer; override;
    function GetWordLength(Index: Integer): Integer; override;
    function GetWordLengthWOWS(Index: Integer): Integer; override;
    function GetWords(Index: Integer): TKString; override;
    function GetWordTop(Index: Integer): Integer; override;
    function GetWordTopPadding(Index: Integer): Integer; override;
    function GetWordWidth(Index: Integer): Integer; override;
    function IndexToTextIndex(const AText: TKString; AIndex: Integer): Integer; virtual;
    function InternalTextExtent(ACanvas: TCanvas; const AText: TKString): TSize; virtual;
    procedure InternalTextOutput(ACanvas: TCanvas; ALeft, ATop: Integer; const AText: TKString); virtual;
    function ModifiedTextExtent(ACanvas: TCanvas; const AText: TKString): TPoint; virtual;
    procedure ParentChanged; override;
    procedure SetText(const Value: TKString); virtual;
    procedure SetWordBaseLine(Index: Integer; const Value: Integer); override;
    procedure SetWordBottomPadding(Index: Integer; const Value: Integer); override;
    procedure SetWordClipped(Index: Integer; const Value: Boolean); override;
    procedure SetWordHeight(Index: Integer; const Value: Integer); override;
    procedure SetWordLeft(Index: Integer; const Value: Integer); override;
    procedure SetWordTop(Index: Integer; const Value: Integer); override;
    procedure SetWordTopPadding(Index: Integer; const Value: Integer); override;
    procedure SetWordWidth(Index: Integer; const Value: Integer); override;
    class procedure SplitText(const ASource: TKString; At: Integer; out APart1, APart2: TKString);
    function TextIndexToIndex(var AText: TKString; ATextIndex: Integer): Integer; virtual;
    procedure TextStyleChanged(Sender: TObject);
    procedure UpdateWords; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TKObject); override;
    procedure AssignAttributes(AItem: TKMemoBlock); override;
    function CalcAscent(ACanvas: TCanvas): Integer; override;
    function CalcDescent(ACanvas: TCanvas): Integer; virtual;
    procedure ClearSelection(ATextOnly: Boolean); override;
    function Concat(AItem: TKMemoBlock): Boolean; override;
    function EqualProperties(ASource: TKObject): Boolean; override;
    procedure GetWordIndexes(AIndex: Integer; out ASt, AEn: Integer); override;
    function InsertString(const AText: TKString; At: Integer = -1): Boolean; override;
    procedure NotifyDefaultTextChange; override;
    procedure NotifyOptionsChange; override;
    procedure NotifyPrintBegin; override;
    procedure NotifyPrintEnd; override;
    function Split(At: Integer; AllowEmpty: Boolean = False): TKMemoBlock; override;
    function WordIndexToRect(ACanvas: TCanvas; AWordIndex: Integer; AIndex: Integer; ACaret: Boolean): TRect; override;
    function WordMeasureExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint; override;
    function WordMouseAction(ACanvas: TCanvas; AWordIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean; override;
    procedure WordPaintToCanvas(ACanvas: TCanvas; AWordIndex, ALeft, ATop: Integer); override;
    function WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint; AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; override;
    property Text: TKString read GetText write SetText;
    property TextStyle: TKMemoTextStyle read FTextStyle;
    property WordBreaks: TKSysCharSet read GetWordBreaks;
  end;

  TKMemoHyperlink = class(TKMemoTextBlock)
  private
    FURL: TKString;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
  protected
    FMouseCaptureWord: Integer;
    procedure Click; virtual;
    procedure DblClick; virtual;
  public
    constructor Create; override;
    procedure Assign(ASource: TKObject); override;
    procedure DefaultStyle; virtual;
    function WordMouseAction(ACanvas: TCanvas; AWordIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean; override;
    property URL: TKString read FURL write FURL;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
  end;

  TKMemoParagraph = class(TKMemoTextBlock)
  private
    FExtent: TPoint;
    FOrigin: TPoint;
    FParaStyle: TKMemoParaStyle;
    function GetNumbering: TKMemoParaNumbering;
    function GetNumberingList: TKMemoList;
    function GetNumberingListLevel: TKMemoListLevel;
    procedure SetNumbering(const Value: TKMemoParaNumbering);
  protected
    FNumberBlock: TKMemoTextBlock;
    function GetCanAddText: Boolean; override;
    function GetNumberBlock: TKMemoTextBlock; virtual;
    function GetParaStyle: TKMemoParaStyle; override;
    function GetWordBreakable(Index: Integer): Boolean; override;
    procedure ParaStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AssignAttributes(AItem: TKMemoBlock); override;
    function Concat(AItem: TKMemoBlock): Boolean; override;
    procedure NotifyDefaultParaChange; override;
    function Split(At: Integer; AllowEmpty: Boolean = False): TKMemoBlock; override;
    procedure WordPaintToCanvas(ACanvas: TCanvas; AWordIndex, ALeft, ATop: Integer); override;
    property Height: Integer read FExtent.Y write FExtent.Y;
    property Left: Integer read FOrigin.X write FOrigin.X;
    property Numbering: TKMemoParaNumbering read GetNumbering write SetNumbering;
    property NumberingList: TKMemoList read GetNumberingList;
    property NumberingListLevel: TKMemoListLevel read GetNumberingListLevel;
    property NumberBlock: TKMemoTextBlock read GetNumberBlock;
    property Top: Integer read FOrigin.Y write FOrigin.Y;
    property Width: Integer read FExtent.X write FExtent.X;
  end;

  TKMemoImageBlock = class(TKMemoSingleton)
  private
    FBaseLine: Integer;
    FCrop: TKRect;
    FImage: TPicture;
    FImageStyle: TKMemoBlockStyle;
    FExtent: TPoint; // extent given by word processor
    FOrigin: TPoint;
    FExplicitExtent: TPoint; // explicit extent
    FScale: TPoint; // scaled extent
    FCroppedImage: TKAlphaBitmap;
    FWordBottomPadding: Integer;
    FWordTopPadding: Integer;
    procedure SetCrop(const Value: TKRect);
    procedure SetImage(const Value: TPicture);
    procedure SetImagePath(const Value: TKString);
    procedure SetScaleHeight(const Value: Integer);
    procedure SetScaleWidth(const Value: Integer);
    procedure SetExplicitHeight(const Value: Integer);
    procedure SetExplicitWidth(const Value: Integer);
    function GetNativeOrExplicitHeight: Integer;
    function GetNativeOrExplicitWidth: Integer;
    function GetScaleHeight: Integer;
    function GetScaleWidth: Integer;
    procedure SetScaleX(const Value: Integer);
    procedure SetScaleY(const Value: Integer);
  protected
    FCalcBaseLine: Integer;
    FCreatingCroppedImage: Boolean;
    FMouseCapture: Boolean;
    FScaledRect: TRect;
    function ContentLength: Integer; override;
    procedure CropChanged(Sender: TObject);
    function GetWrapMode: TKMemoBlockWrapMode; override;
    function GetImageHeight: Integer; virtual;
    function GetImageWidth: Integer; virtual;
    function GetSizingRect: TRect; override;
    function GetWordBottomPadding(Index: Integer): Integer; override;
    function GetWordBoundsRect(Index: Integer): TRect; override;
    function GetWordCount: Integer; override;
    function GetWordHeight(Index: Integer): Integer; override;
    function GetWordLeft(Index: Integer): Integer; override;
    function GetWordLength(Index: Integer): Integer; override;
    function GetWords(Index: Integer): TKString; override;
    function GetWordTop(Index: Integer): Integer; override;
    function GetWordTopPadding(Index: Integer): Integer; override;
    function GetWordWidth(Index: Integer): Integer; override;
    procedure ImageChanged(Sender: TObject);
    procedure ImageStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
    function CroppedImage: TKAlphaBitmap; virtual;
    procedure SetWordBaseLine(Index: Integer; const Value: Integer); override;
    procedure SetWordBottomPadding(Index: Integer; const Value: Integer); override;
    procedure SetWordHeight(Index: Integer; const Value: Integer); override;
    procedure SetWordLeft(Index: Integer; const Value: Integer); override;
    procedure SetWordTop(Index: Integer; const Value: Integer); override;
    procedure SetWordTopPadding(Index: Integer; const Value: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TKObject); override;
    procedure AssignAttributes(AItem: TKMemoBlock); override;
    function CalcAscent(ACanvas: TCanvas): Integer; override;
    function OuterRect(ACaret: Boolean): TRect; virtual;
    procedure Resize(ANewWidth, ANewHeight: Integer); override;
    function WordIndexToRect(ACanvas: TCanvas; AWordIndex: Integer; AIndex: Integer; ACaret: Boolean): TRect; override;
    function WordMeasureExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint; override;
    function WordMouseAction(ACanvas: TCanvas; AWordIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean; override;
    function WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint; AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; override;
    procedure WordPaintToCanvas(ACanvas: TCanvas; AIndex, ALeft, ATop: Integer); override;
    property Crop: TKRect read FCrop write SetCrop;
    property Image: TPicture read FImage write SetImage;
    property ImageStyle: TKMemoBlockStyle read FImageStyle;
    property ImageHeight: Integer read GetImageHeight;
    property ImageWidth: Integer read GetImageWidth;
    property ExplicitHeight: Integer read FExplicitExtent.Y write SetExplicitHeight;
    property ExplicitWidth: Integer read FExplicitExtent.X write SetExplicitWidth;
    property NativeOrExplicitHeight: Integer read GetNativeOrExplicitHeight;
    property NativeOrExplicitWidth: Integer read GetNativeOrExplicitWidth;
    property Path: TKString write SetImagePath;
    property ScaleHeight: Integer read GetScaleHeight write SetScaleHeight;
    property ScaleWidth: Integer read GetScaleWidth write SetScaleWidth;
    property ScaleX: Integer read FScale.X write SetScaleX;
    property ScaleY: Integer read FScale.Y write SetScaleY;
  end;

  TKMemoContainer = class(TKMemoBlock)
  private
    FBlocks: TKMemoBlocks;
    FBlockStyle: TKMemoBlockStyle;
    FClip: Boolean;
    FCurrentRequiredWidth: Integer;
    FCurrentRequiredHeight: Integer;
    FFixedHeight: Boolean;
    FFixedWidth: Boolean;
    FOrigin: TPoint;
    FRequiredHeight: Integer;
    FRequiredWidth: Integer;
    FWordBottomPadding: Integer;
    FWordTopPadding: Integer;
    procedure SetFixedWidth(const Value: Boolean);
    procedure SetRequiredWidth(const Value: Integer);
    procedure SetRequiredHeight(const Value: Integer);
    procedure SetClip(const Value: Boolean);
    procedure SetFixedHeight(const Value: Boolean);
  protected
    function AddRectOffset(const ARect: TRect): TRect; virtual;
    procedure AddSingleLine; virtual;
    procedure AddBlockLine(AStartBlock, AStartIndex, AEndBlock, AEndIndex,
      ALeft, ATop, AWidth, AHeight: Integer); virtual;
    procedure BlockStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
    procedure ClearLines; virtual;
    function ContentLength: Integer; override;
    function GetBottomPadding: Integer; override;
    function GetWrapMode: TKMemoBlockWrapMode; override;
    function GetCanAddText: Boolean; override;
    function GetSelLength: Integer; override;
    function GetSelStart: Integer; override;
    function GetSelText: TKString; override;
    function GetText: TKString; override;
    function GetTopPadding: Integer; override;
    function GetTotalLineCount: Integer; virtual;
    function GetTotalLineRect(Index: Integer): TRect; virtual;
    function GetWordBottomPadding(Index: Integer): Integer; override;
    function GetWordBoundsRect(Index: Integer): TRect; override;
    function GetWordCount: Integer; override;
    function GetWordHeight(Index: Integer): Integer; override;
    function GetWordLeft(Index: Integer): Integer; override;
    function GetWordLength(Index: Integer): Integer; override;
    function GetWords(Index: Integer): TKString; override;
    function GetWordTop(Index: Integer): Integer; override;
    function GetWordTopPadding(Index: Integer): Integer; override;
    function GetWordWidth(Index: Integer): Integer; override;
    procedure ParentChanged; override;
    procedure RequiredHeightChanged; virtual;
    procedure RequiredWidthChanged; virtual;
    procedure SetWordBottomPadding(Index: Integer; const Value: Integer); override;
    procedure SetWordHeight(Index: Integer; const Value: Integer); override;
    procedure SetWordLeft(Index: Integer; const Value: Integer); override;
    procedure SetWordTop(Index: Integer; const Value: Integer); override;
    procedure SetWordTopPadding(Index: Integer; const Value: Integer); override;
    procedure SetWordWidth(Index: Integer; const Value: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TKObject); override;
    procedure AssignAttributes(AItem: TKMemoBlock); override;
    function CalcAscent(ACanvas: TCanvas): Integer; override;
    function CanAdd(AItem: TKMemoBlock): Boolean; override;
    procedure ClearSelection(ATextOnly: Boolean); override;
    function InsertParagraph(AIndex: Integer): Boolean; override;
    function InsertString(const AText: TKString; At: Integer = -1): Boolean; override;
    procedure NotifyDefaultParaChange; override;
    procedure NotifyDefaultTextChange; override;
    procedure NotifyPrintBegin; override;
    procedure NotifyPrintEnd; override;
    procedure Resize(ANewWidth, ANewHeight: Integer); override;
    function Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean): Boolean; override;
    procedure SetBlockExtent(AWidth, AHeight: Integer); virtual;
    procedure UpdateAttributes; virtual;
    function WordIndexToRect(ACanvas: TCanvas; AWordIndex: Integer; AIndex: Integer; ACaret: Boolean): TRect; override;
    function WordMeasureExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint; override;
    function WordMouseAction(ACanvas: TCanvas; AIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean; override;
    function WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint; AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; override;
    procedure WordPaintToCanvas(ACanvas: TCanvas; AIndex, ALeft, ATop: Integer); override;
    property Blocks: TKMemoBlocks read FBlocks;
    property BlockStyle: TKMemoBlockStyle read FBlockStyle;
    property Clip: Boolean read FClip write SetClip;
    property CurrentRequiredHeight: Integer read FCurrentRequiredHeight;
    property CurrentRequiredWidth: Integer read FCurrentRequiredWidth;
    property FixedHeight: Boolean read FFixedHeight write SetFixedHeight;
    property FixedWidth: Boolean read FFixedWidth write SetFixedWidth;
    property RequiredHeight: Integer read FRequiredHeight write SetRequiredHeight;
    property RequiredWidth: Integer read FRequiredWidth write SetRequiredWidth;
    property TotalLineCount: Integer read GetTotalLineCount;
    property TotalLineRect[Index: Integer]: TRect read GetTotalLineRect;
  end;

  TKMemoTable = class;

  TKMemoTableRow = class;

  TKMemoTableCell = class(TKMemoContainer)
  private
    FParaStyle: TKMemoParaStyle;
    FRequiredBorderWidths: TKRect;
    FSpan: TKCellSpan;
    function GetParentRow: TKMemoTableRow;
    function GetParentTable: TKMemoTable;
    procedure SetColSpan(Value: Integer);
    procedure SetRowSpan(Value: Integer);
    procedure SetSpan(const Value: TKCellSpan);
  protected
    function ContentLength: Integer; override;
    function GetParaStyle: TKMemoParaStyle; override;
    procedure ParaStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
    procedure RequiredBorderWidthsChanged(Sender: TObject);
    procedure RequiredWidthChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function PointToIndex(ACanvas: TCanvas; const APoint: TPoint; AFirstRow, ALastRow, AOutOfArea, ASelectionExpanding: Boolean;
      out APosition: TKMemoLinePosition): Integer; reintroduce; virtual;
    function WordMeasureExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint; override;
    procedure WordPaintToCanvas(ACanvas: TCanvas; AIndex, ALeft, ATop: Integer); override;
    property ParentRow: TKMemoTableRow read GetParentRow;
    property ParentTable: TKMemoTable read GetParentTable;
    property RequiredBorderWidths: TKRect read FRequiredBorderWidths;
    property Span: TKCellSpan read FSpan write SetSpan;
    property ColSpan: Integer read FSpan.ColSpan write SetColSpan;
    property RowSpan: Integer read FSpan.RowSpan write SetRowSpan;
  end;

  TKMemoTableRow = class(TKMemoContainer)
  private
    function GetCells(Index: Integer): TKMemoTableCell;
    function GetCellCount: Integer;
    procedure SetCellCount(const Value: Integer);
    function GetParentTable: TKMemoTable;
  protected
    function GetTotalLineCount: Integer; override;
    function GetTotalLineRect(Index: Integer): TRect; override;
    procedure RequiredHeightChanged; override;
    procedure RequiredWidthChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CanAdd(AItem: TKMemoBlock): Boolean; override;
    procedure UpdateRequiredWidth; virtual;
    property CellCount: Integer read GetCellCount write SetCellCount;
    property Cells[Index: Integer]: TKMemoTableCell read GetCells;
    property ParentTable: TKMemoTable read GetParentTable;
  end;

  TKMemoTable = class(TKMemoContainer)
  private
    FCellStyle: TKMemoBlockStyle;
    FColCount: Integer;
    FColWidths: TKMemoSparseList;
    function GetCells(ACol, ARow: Integer): TKMemoTableCell;
    function GetCellSpan(ACol, ARow: Integer): TKCellSpan;
    function GetColWidths(Index: Integer): Integer;
    function GetRows(Index: Integer): TKMemoTableRow;
    function GetRowCount: Integer;
    function GetRowHeights(Index: Integer): Integer;
    procedure SetCellSpan(ACol, ARow: Integer; Value: TKCellSpan);
    procedure SetColCount(const Value: Integer);
    procedure SetColWidths(Index: Integer; const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    procedure SetRowHeights(Index: Integer; const Value: Integer);
  protected
    procedure InternalSetCellSpan(ACol, ARow: Integer;
      const Value: TKCellSpan); virtual;
    procedure RequiredWidthChanged; override;
    procedure SetSize(AColCount, ARowCount: Integer); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ApplyDefaultCellStyle; virtual;
    function CanAdd(AItem: TKMemoBlock): Boolean; override;
    function CalcTotalCellWidth(ACol, ARow: Integer): Integer; virtual;
    function CellValid(ACol, ARow: Integer): Boolean; virtual;
    function CellVisible(ACol, ARow: Integer): Boolean; virtual;
    function ColValid(ACol: Integer): Boolean; virtual;
    procedure FindBaseCell(ACol, ARow: Integer; out BaseCol, BaseRow: Integer); virtual;
    function FindCell(ACell: TKMemoTableCell; out ACol, ARow: Integer): Boolean; virtual;
    procedure FixupBorders; virtual;
    procedure FixupCellSpan; virtual;
    procedure FixupCellSpanFromRTF; virtual;
    function RowValid(ARow: Integer): Boolean; virtual;
    function WordMeasureExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint; override;
    function WordMouseAction(ACanvas: TCanvas; AIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean; override;
    function WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint; AWordIndex: Integer;
      AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; override;
    property Cells[ACol, ARow: Integer]: TKMemoTableCell read GetCells;
    property CellSpan[ACol, ARow: Integer]: TKCellSpan read GetCellSpan write SetCellSpan;
    property CellStyle: TKMemoBlockStyle read FCellStyle;
    property ColCount: Integer read FColCount write SetColCount;
    property ColWidths[Index: Integer]: Integer read GetColWidths write SetColWidths;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property RowHeights[Index: Integer]: Integer read GetRowHeights write SetRowHeights;
    property Rows[Index: Integer]: TKMemoTableRow read GetRows;
  end;

  TKMemoUpdateEvent = procedure(Reasons: TKMemoUpdateReasons) of object;

  TKMemoBlocks = class(TKObjectList)
  private
    FExtent: TPoint;
    FIgnoreParaMark: Boolean;
    FMemoNotifier: IKMemoNotifier;
    FParent: TKMemoBlock;
    FSelectableLength: Integer;
    FSelEnd: Integer;
    FSelStart: Integer;
    FOnUpdate: TKMemoUpdateEvent;
    function GetBoundsRect: TRect;
    function GetEmpty: Boolean;
    function GetItem(Index: Integer): TKMemoBlock;
    function GetLineCount: Integer;
    function GetParentBlocks: TKMemoBlocks;
    function GetRealSelEnd: Integer;
    function GetRealSelLength: Integer;
    function GetRealSelStart: Integer;
    function GetSelLength: Integer;
    procedure SetIgnoreParaMark(const Value: Boolean);
    procedure SetItem(Index: Integer; const Value: TKMemoBlock);
    procedure SetMemoNotifier(const Value: IKMemoNotifier);
    function GetFirstBlock: TKMemoBlock;
    function GetLastBlock: TKMemoBlock;
    function GetParentMemo: TKCustomMemo;
  protected
    FLines: TKMemoLines;
    FRelPos: TKMemoSparseList;
    FRequiredWidth: Integer;
    FUpdateReasons: TKMemoUpdateReasons;
    procedure CallBeforeUpdate; override;
    procedure CallAfterUpdate; override;
    procedure DoUpdate(AReasons: TKMemoUpdateReasons);
    function EOLToNormal(var AIndex: Integer): Boolean; virtual;
    function GetDefaultTextStyle: TKMemoTextStyle; virtual;
    function GetDefaultParaStyle: TKMemoParaStyle; virtual;
    function GetLineBottom(ALineIndex: Integer): Integer; virtual;
    function GetLineEndIndex(ALineIndex: Integer): Integer; virtual;
    function GetLineFloat(ALineIndex: Integer): Boolean; virtual;
    function GetLineHeight(ALineIndex: Integer): Integer; virtual;
    function GetLineInfo(ALineIndex: Integer): TKMemoLine;
    function GetLineLeft(ALineIndex: Integer): Integer; virtual;
    function GetLinePosition: TKMemoLinePosition; virtual;
    function GetLineRect(ALineIndex: Integer): TRect; virtual;
    function GetLineRight(ALineIndex: Integer): Integer; virtual;
    function GetLineText(ALineIndex: Integer): TKString; virtual;
    function GetLineSize(ALineIndex: Integer): Integer; virtual;
    function GetLineStartIndex(ALineIndex: Integer): Integer; virtual;
    function GetLineTop(ALineIndex: Integer): Integer; virtual;
    function GetLineWidth(ALineIndex: Integer): Integer; virtual;
    function GetMaxWordLength: Integer; virtual;
    function GetSelectionHasPara: Boolean; virtual;
    function GetSelectionParaStyle: TKMemoParaStyle; virtual;
    function GetSelectionTextStyle: TKMemoTextStyle; virtual;
    function GetSelText: TKString; virtual;
    function GetShowFormatting: Boolean; virtual;
    function GetText: TKString; virtual;
    function GetTotalLeftOffset: Integer; virtual;
    function GetTotalLineCount: Integer; virtual;
    function GetTotalLineRect(Index: Integer): TRect; virtual;
    function GetTotalTopOffset: Integer; virtual;
    procedure GetWordIndexes(const ABlockIndex, ALineIndex: Integer; out AStart, AEnd: Integer); virtual;
    function LineToRect(ACanvas: TCanvas; AIndex, ALineIndex: Integer; ACaret: Boolean): TRect; virtual;
    function NormalToEOL(var AIndex: Integer): Boolean; virtual;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure PaintLineBackground(ACanvas: TCanvas; ALineIndex, ALeft, ATop: Integer); virtual;
    procedure PaintLineInfo(ACanvas: TCanvas; ALineIndex, ALeft, ATop: Integer); virtual;
    function Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean = True; ATextOnly: Boolean = False): Boolean; virtual;
    procedure SetLineText(ALineIndex: Integer; const AValue: TKString); virtual;
    procedure SetSelectionParaStyle(const Value: TKMemoParaStyle); virtual;
    procedure SetSelectionTextStyle(const Value: TKMemoTextStyle); virtual;
    procedure SetText(const AValue: TKString);
    procedure Update(AReasons: TKMemoUpdateReasons); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    function AddAt(AObject: TKMemoBlock; At: Integer = -1): Integer; virtual;
    function AddContainer(At: Integer = -1): TKMemoContainer;
    function AddHyperlink(const AText, AURL: TKString; At: Integer = -1): TKMemoHyperlink; overload;
    function AddHyperlink(AItem: TKMemoHyperlink; At: Integer = -1): TKMemoHyperlink; overload;
    function AddImageBlock(AImage: TPicture; At: Integer = -1): TKMemoImageBlock; overload;
    function AddImageBlock(const APath: TKString; At: Integer = -1): TKMemoImageBlock; overload;
    function AddParagraph(At: Integer = -1): TKMemoParagraph;
    function AddTable(At: Integer = -1): TKMemoTable;
    function AddTextBlock(const AText: TKString; At: Integer = -1): TKMemoTextBlock;
    function BlockToItem(ABlock: Integer): TKMemoBlock;
    procedure Clear; override;
    procedure ClearSelection(ATextOnly: Boolean = True); virtual;
    procedure ConcatEqualBlocks; virtual;
    procedure DeleteBOL(At: Integer); virtual;
    procedure DeleteChar(At: Integer); virtual;
    procedure DeleteEOL(At: Integer); virtual;
    procedure DeleteLastChar(At: Integer); virtual;
    procedure DeleteLine(At: Integer); virtual;
    procedure FixEmptyBlocks; virtual;
    procedure FixEOL(AIndex: Integer; AAdjust: Boolean; var ALinePos: TKMemoLinePosition); virtual;
    function GetLastItemByClass(AIndex: Integer; AClass: TKMemoBlockClass): TKMemoBlock; virtual;
    function GetNearestAnchorIndex(AIndex: Integer): Integer; virtual;
    function GetNearestParagraphBlock(AIndex: Integer): Integer;
    function GetNearestParagraphItem(AIndex: Integer): TKMemoParagraph;
    function GetNearestWordIndexes(AIndex: Integer; AAdjust: Boolean;
      AIncludeWhiteSpaces: Boolean; out AStart, AEnd: Integer): Boolean; virtual;
    function GetNextItemByClass(AIndex: Integer; AClass: TKMemoBlockClass): TKMemoBlock; virtual;
    function GetPageCount(APageHeight: Integer): Integer; virtual;
    procedure GetPageData(APageHeight, APage: Integer; out AOffset, AHeight: Integer); virtual;
    procedure GetSelColors(out TextColor, Background: TColor); virtual;
    function IndexAboveLastLine(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function IndexAtBeginningOfContainer(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function IndexAtEndOfContainer(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function IndexBelowFirstLine(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function IndexToBlock(AIndex: Integer; out ALocalIndex: Integer): Integer; virtual;
    function IndexToItems(AIndex: Integer; out ALocalIndex: Integer): TKMemoBlocks; virtual;
    function IndexToItem(AIndex: Integer; out ALocalIndex: Integer): TKMemoBlock; virtual;
    function IndexToLine(AIndex: Integer): Integer; virtual;
    function IndexToRect(ACanvas: TCanvas; AIndex: Integer; ACaret, AAdjust: Boolean): TRect; virtual;
    function InsideOfTable: Boolean; virtual;
    procedure InsertChar(At: Integer; const AValue: TKChar; AOverWrite: Boolean; ATextStyle: TKMemoTextStyle = nil); virtual;
    procedure InsertNewLine(At: Integer); virtual;
    procedure InsertPlainText(AIndex: Integer; const AValue: TKString); virtual;
    function InsertParagraph(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function InsertString(AIndex: Integer; AAdjust: Boolean; const AValue: TKString; ATextStyle: TKMemoTextStyle = nil): Boolean; virtual;
    function ItemToIndex(AItem: TKMemoBlock): Integer; virtual;
    function LastTextStyle(AIndex: Integer): TKMemoTextStyle; virtual;
    function LineEndIndexByIndex(AIndex: Integer; AAdjust, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function LineStartIndexByIndex(AIndex: Integer; AAdjust: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    procedure ListChanged(AList: TKMemoList; ALevel: TKMemoListLevel); virtual;
    procedure LoadFromRTFStream(AStream: TStream; AtIndex: Integer = -1); virtual;
    procedure MeasureExtent(ACanvas: TCanvas; ARequiredWidth: Integer); virtual;
    function MouseAction(AAction: TKMemoMouseAction; ACanvas: TCanvas; const APoint: TPoint; AShift: TShiftState): Boolean; virtual;
    procedure NotifyDefaultParaChange; virtual;
    procedure NotifyDefaultTextChange; virtual;
    procedure NotifyOptionsChange; virtual;
    procedure NotifyPrintBegin; virtual;
    procedure NotifyPrintEnd; virtual;
    function NextIndexByCharCount(AIndex, ACharCount: Integer): Integer;
    function NextIndexByHorzExtent(ACanvas: TCanvas; AIndex, AWidth: Integer; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function NextIndexByRowDelta(ACanvas: TCanvas; AIndex, ARowDelta, ALeftPos: Integer; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function NextIndexByVertExtent(ACanvas: TCanvas; AIndex, AHeight, ALeftPos: Integer; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function NextIndexByVertValue(ACanvas: TCanvas; AValue, ALeftPos: Integer; ADirection: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function PointToItem(const APoint: TPoint): TKMemoBlock; virtual;
    function PointToItems(const APoint: TPoint): TKMemoBlocks; virtual;
    procedure PaintToCanvas(ACanvas: TCanvas; ALeft, ATop: Integer; const ARect: TRect); virtual;
    function PointToIndex(ACanvas: TCanvas; const APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function PointToIndexOnLine(ACanvas: TCanvas; ALineIndex: Integer; const APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    procedure SaveToRTFStream(AStream: TStream; ASelectedOnly: Boolean = False); virtual;
    procedure SetExtent(AWidth, AHeight: Integer); virtual;
    procedure UpdateAttributes; virtual;
    property BoundsRect: TRect read GetBoundsRect;
    property DefaultTextStyle: TKMemoTextStyle read GetDefaultTextStyle;
    property DefaultParaStyle: TKMemoParaStyle read GetDefaultParaStyle;
    property Empty: Boolean read GetEmpty;
    property FirstBlock: TKMemoBlock read GetFirstBlock;
    property Height: Integer read FExtent.Y;
    property IgnoreParaMark: Boolean read FIgnoreParaMark write SetIgnoreParaMark;
    property Items[Index: Integer]: TKMemoBlock read GetItem write SetItem; default;
    property LastBlock: TKMemoBlock read GetLastBlock;
    property LineBottom[ALineIndex: Integer]: Integer read GetLineBottom;
    property LineCount: Integer read GetLineCount;
    property LineEndIndex[ALineIndex: Integer]: Integer read GetLineEndIndex;
    property LineFloat[ALineIndex: Integer]: Boolean read GetLineFloat;
    property LineInfo[ALineIndex: Integer]: TKMemoLine read GetLineInfo;
    property LineHeight[ALineIndex: Integer]: Integer read GetLineHeight;
    property LineLeft[ALineIndex: Integer]: Integer read GetLineLeft;
    property LineRight[ALineIndex: Integer]: Integer read GetLineRight;
    property LineTop[ALineIndex: Integer]: Integer read GetLineTop;
    property LineRect[ALineIndex: Integer]: TRect read GetLineRect;
    property LineText[ALineIndex: Integer]: TKString read GetLineText write SetLineText;
    property Lines: TKMemoLines read FLines;
    property LineSize[ALineIndex: Integer]: Integer read GetLineSize;
    property LineStartIndex[ALineIndex: Integer]: Integer read GetLineStartIndex;
    property LineWidth[ALineIndex: Integer]: Integer read GetLineWidth;
    property MemoNotifier: IKMemoNotifier read FMemoNotifier write SetMemoNotifier;
    property Parent: TKMemoBlock read FParent write FParent;
    property ParentBlocks: TKMemoBlocks read GetParentBlocks;
    property ParentMemo: TKCustomMemo read GetParentMemo;
    property RealSelEnd: Integer read GetRealSelEnd;
    property RealSelLength: Integer read GetRealSelLength;
    property RealSelStart: Integer read GetRealSelStart;
    property SelectableLength: Integer read FSelectableLength;
    property SelectionHasPara: Boolean read GetSelectionHasPara;
    property SelectionParaStyle: TKMemoParaStyle read GetSelectionParaStyle write SetSelectionParaStyle;
    property SelectionTextStyle: TKMemoTextStyle read GetSelectionTextStyle write SetSelectionTextStyle;
    property SelEnd: Integer read FSelEnd;
    property SelLength: Integer read GetSelLength;
    property SelStart: Integer read FSelStart;
    property SelText: TKString read GetSelText;
    property ShowFormatting: Boolean read GetShowFormatting;
    property Text: TKString read GetText write SetText;
    property TotalLeftOffset: Integer read GetTotalLeftOffset;
    property TotalLineCount: Integer read GetTotalLineCount;
    property TotalLineRect[Index: Integer]: TRect read GetTotalLineRect;
    property TotalTopOffset: Integer read GetTotalTopOffset;
    property Width: Integer read FExtent.X;
    property OnUpdate: TKMemoUpdateEvent read FOnUpdate write FOnUpdate;
  end;

  { @abstract(Container for all colors used by @link(TKCustomMemo) class)
    This container allows to group many colors into one item in object inspector.
    Colors are accessible via published properties or several public Color*
    properties. }
  TKMemoColors = class(TKCustomColors)
  private
  protected
    { Returns the specific color according to ColorScheme. }
    function InternalGetColor(Index: TKColorIndex): TColor; override;
    { Returns color specification structure for given index. }
    function GetColorSpec(Index: TKColorIndex): TKColorSpec; override;
    { Returns maximum color index. }
    function GetMaxIndex: Integer; override;
  published
    { Hex editor client area background. }
    property BkGnd: TColor index ciBkGnd read GetColor write SetColor default cBkGndDef;
    { Inactive (memo without focus) caret background color - caret mark is not part of a selection. }
    property InactiveCaretBkGnd: TColor index ciInactiveCaretBkGnd read GetColor write SetColor default cInactiveCaretBkGndDef;
    { Inactive (memo without focus) caret background color - caret mark is part of a selection. }
    property InactiveCaretSelBkGnd: TColor index ciInactiveCaretSelBkGnd read GetColor write SetColor default cInactiveCaretSelBkGndDef;
    { Inactive (memo without focus) caret text color - caret mark is part of a selection. }
    property InactiveCaretSelText: TColor index ciInactiveCaretSelText read GetColor write SetColor default cInactiveCaretSelTextDef;
    { Inactive (memo without focus) caret text color - caret mark is not part of a selection. }
    property InactiveCaretText: TColor index ciInactiveCaretText read GetColor write SetColor default cInactiveCaretTextDef;
    { Selection background - inactive edit area. }
    property SelBkGnd: TColor index ciSelBkGnd read GetColor write SetColor default cSelBkGndDef;
    { Selection background - active edit area. }
    property SelBkGndFocused: TColor index ciSelBkGndFocused read GetColor write SetColor default cSelBkGndFocusedDef;
    { Selection text - inactive edit area. }
    property SelText: TColor index ciSelText read GetColor write SetColor default cSelTextDef;
    { Selection text - active edit area. }
    property SelTextFocused: TColor index ciSelTextFocused read GetColor write SetColor default cSelTextFocusedDef;
  end;

  { Declares possible values for the ItemKind member of the @link(TKMemoChangeItem) structure. }
  TKMemoChangeKind = (
    { Save caret position only. }
    ckCaretPos,
    { Save inserted data to be able to delete it. }
    ckDelete,
    { Save deleted data to be able to insert it. }
    ckInsert
    );

  { @abstract(Declares @link(TKMemoChangeList.OnChange) event handler)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller</LI>
    <LI><I>ItemReason</I> - specifies the undo/redo reason</LI>
    </UL>
  }
  TKMemoUndoChangeEvent = procedure(Sender: TObject;
    ItemReason: TKMemoChangeKind) of object;

  { @abstract(Declares the undo/redo item description structure used by the @link(TKMemoChangeList) class)
    <UL>
    <LH>Members:</LH>
    <LI><I>Data</I> - string needed to execute this item</LI>
    <LI><I>EditArea</I> - active edit area at the time this item was recorded</LI>
    <LI><I>Group</I> - identifies the undo/redo group. Some editor modifications
      produce a sequence of 2 or more undo items. This sequence is called undo/redo
      group and is always interpreted as a single undo/redo item. Moreover,
      if there is @link(eoGroupUndo) among @link(TKCustomMemo.Options),
      a single ecUndo or ecRedo command manipulates all following undo groups
      of the same kind as if they were a single undo/redo item. </LI>
    <LI><I>GroupKind</I> - kind of this undo group</LI>
    <LI><I>ItemKind</I> - kind of this item</LI>
    <LI><I>SelEnd</I> - end of the selection at the time this item was recorded</LI>
    <LI><I>SelStart</I> - start of the selection at the time this item was recorded</LI>
    </UL>
  }
  TKMemoChangeItem = record
    Blocks: TKMemoBlocks;
    Group: Cardinal;
    GroupKind: TKMemoChangeKind;
    Inserted: Boolean;
    ItemKind: TKMemoChangeKind;
    Position: Integer;
  end;

  { Pointer to @link(TKMemoChangeItem). }
  PKMemoChangeItem = ^TKMemoChangeItem;

 { @abstract(Change (undo/redo item) list manager). }
  TKMemoChangeList = class(TList)
  private
    FEditor: TKCustomMemo;
    FGroup: Cardinal;
    FGroupUseLock: Integer;
    FGroupKind: TKMemoChangeKind;
    FIndex: Integer;
    FModifiedIndex: Integer;
    FLimit: Integer;
    FRedoList: TKMemoChangeList;
    FOnChange: TKMemoUndoChangeEvent;
    function GetModified: Boolean;
    procedure SetLimit(Value: Integer);
    procedure SetModified(Value: Boolean);
  protected
    { Redefined to properly destroy the items. }
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    { Performs necessary initializations
      <UL>
      <LH>Parameters:</LH>
      <LI><I>AEditor</I> - identifies the undo/redo list owner</LI>
      <LI><I>RedoList</I> - when this instance is used as undo list, specify
      a redo list to allow clear it at each valid AddChange call</LI>
      </UL> }
    constructor Create(AEditor: TKCustomMemo; RedoList: TKMemoChangeList);
    { Inserts a undo/redo item
      <UL>
      <LH>Parameters:</LH>
      <LI><I>ItemKind</I> - specifies the undo/redo item reason. The change list doesn't
      allow to insert succesive crCaretPos items unless Inserted is True</LI>
      <LI><I>Data</I> - specifies the item data. Some items (crCaretPos)
      don't need to supply any data</LI>
      <LI><I>Inserted</I> - for the urInsert* items, specifies whether the item
      was recorded with @link(TKCustomMemo.InsertMode) on (True) or
      off (False). See ItemKind for crCaretPos behavior.</LI>
      </UL> }
    procedure AddChange(ItemKind: TKMemoChangeKind; Inserted: Boolean = True); virtual;
    { Tells the undo list a new undo/redo group is about to be created. Each
      BeginGroup call must have a corresponding EndGroup call (use try-finally).
      BeginGroup calls may be nested, however, only the first call will create an
      undo/redo group. Use the GroupKind parameter to specify the reason of this group. }
    procedure BeginGroup(GroupKind: TKMemoChangeKind); virtual;
    { Informs whether there are any undo/redo items available - i.e. CanUndo/CanRedo. }
    function CanPeek: Boolean;
    { Clears the entire list - overriden to execute some adjustments. }
    procedure Clear; override;
    { Completes the undo/redo group. See @link(TKMemoChangeList.BeginGroup) for details. }
    procedure EndGroup; virtual;
    { Returns the topmost item to handle or inspect it. }
    function PeekItem: PKMemoChangeItem;
    { If there is no reason to handle an item returned by PeekItem, it has to be
    poked back with this function to become active for next undo/redo command. }
    procedure PokeItem;
    { For redo list only - each undo command creates a redo command with the same
      group information - see source. }
    procedure SetGroupData(Group: Integer; GroupKind: TKMemoChangeKind);
    { Specifies maximum number of items - not groups. }
    property Limit: Integer read FLimit write SetLimit;
    { For undo list only - returns True if undo list contains some items with regard
      to the @link(eoUndoAfterSave) option. }
    property Modified: Boolean read GetModified write SetModified;
    { Allows to call TKCustomMemo.@link(TKCustomMemo.OnChange) event. }
    property OnChange: TKMemoUndoChangeEvent read FOnChange write FOnChange;
  end;

  TKMemoRTFString = type string;

  TKMemoEditBlockEvent = procedure(AItem: TKMemoBlock; var Result: Boolean) of object;

  { @abstract(Multi line text editor base component). }
  TKCustomMemo = class(TKCustomControl, IKMemoNotifier)
  private
    FActiveBlocks: TKMemoBlocks;
    FBackground: TKMemoBackground;
    FBlocks: TKMemoBlocks;
    FColors: TKMemoColors;
    FContentPadding: TKRect;
    FDisabledDrawStyle: TKEditDisabledDrawStyle;
    FKeyMapping: TKEditKeyMapping;
    FLeftPos: Integer;
    FListTable: TKMemoListTable;
    FMaxWordLength: Integer;
    FMouseWheelAccumulator: Integer;
    FNewTextStyle: TKMemoTextStyle;
    FOptions: TKEditOptions;
    FParaStyle: TKMemoParaStyle;
    FRedoList: TKMemoChangeList;
    FRequiredContentWidth: Integer;
    FScrollBars: TScrollStyle;
    FScrollPadding: Integer;
    FScrollSpeed: Cardinal;
    FScrollTimer: TTimer;
    FStates: TKMemoStates;
    FTextStyle: TKMemoTextStyle;
    FTopPos: Integer;
    FUndoList: TKMemoChangeList;
    FUpdateLock: Integer;
    FWordBreaks: TKSysCharSet;
    FOnChange: TNotifyEvent;
    FOnDropFiles: TKEditDropFilesEvent;
    FOnEditBlock: TKMemoEditBlockEvent;
    FOnReplaceText: TKEditReplaceTextEvent;
    function GetActiveBlock: TKMemoBlock;
    function GetActiveInnerBlock: TKMemoBlock;
    function GetActiveInnerBlocks: TKMemoBlocks;
    function GetCaretVisible: Boolean;
    function GetContentHeight: Integer;
    function GetContentLeft: Integer;
    function GetContentRect: TRect;
    function GetContentTop: Integer;
    function GetContentWidth: Integer;
    function GetEmpty: Boolean;
    function GetInsertMode: Boolean;
    function GetModified: Boolean;
    function GetRequiredContentWidth: Integer;
    function GetRTF: TKMemoRTFString;
    function GetSelAvail: Boolean;
    function GetSelectableLength: Integer;
    function GetSelectionHasPara: Boolean;
    function GetSelectionParaStyle: TKMemoParaStyle;
    function GetSelectionTextStyle: TKMemoTextStyle;
    function GetSelEnd: Integer;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: TKString;
    function GetText: TKString;
    function GetUndoLimit: Integer;
    function IsOptionsStored: Boolean;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure SetActiveBlocks(const Value: TKMemoBlocks);
    procedure SetBackground(const Value: TKMemoBackground);
    procedure SetColors(Value: TKMemoColors);
    procedure SetContentPadding(const Value: TKRect);
    procedure SetDisabledDrawStyle(Value: TKEditDisabledDrawStyle);
    procedure SetLeftPos(Value: Integer);
    procedure SetMaxWordLength(const Value: Integer);
    procedure SetModified(Value: Boolean);
    procedure SetNewTextStyle(const Value: TKMemoTextStyle);
    procedure SetOptions(const Value: TKEditOptions);
    procedure SetReadOnly(Value: Boolean);
    procedure SetRequiredContentWidth(const Value: Integer);
    procedure SetRTF(const Value: TKMemoRTFString);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetScrollPadding(Value: Integer);
    procedure SetScrollSpeed(Value: Cardinal);
    procedure SetSelectionParaStyle(const Value: TKMemoParaStyle);
    procedure SetSelectionTextStyle(const Value: TKMemoTextStyle);
    procedure SetSelEnd(Value: Integer);
    procedure SetSelLength(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetText(const Value: TKString);
    procedure SetTopPos(Value: Integer);
    procedure SetUndoLimit(Value: Integer);
    procedure SetWordBreaks(const Value: TKSysCharSet);
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMSysColorChange(var Msg: TLMessage); message CM_SYSCOLORCHANGE;
  {$IFNDEF FPC}
    procedure EMGetSel(var Msg: TLMessage); message EM_GETSEL;
    procedure EMSetSel(var Msg: TLMessage); message EM_SETSEL;
  {$ENDIF}
    procedure WMClear(var Msg: TLMessage); message LM_CLEAR;
    procedure WMCopy(var Msg: TLMessage); message LM_COPY;
    procedure WMCut(var Msg: TLMessage); message LM_CUT;
  {$IFNDEF FPC}
    // no way to get filenames in Lazarus inside control (why??)
    procedure WMDropFiles(var Msg: TLMessage); message LM_DROPFILES;
  {$ENDIF}
    procedure WMEraseBkgnd(var Msg: TLMessage); message LM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TLMNoParams); message LM_GETDLGCODE;
    procedure WMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure WMKillFocus(var Msg: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMPaste(var Msg: TLMessage); message LM_PASTE;
    procedure WMSetFocus(var Msg: TLMSetFocus); message LM_SETFOCUS;
    procedure WMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
    function GetRelativeSelected: Boolean;
  protected
    FCaretRect: TRect;
    FDragCurPos: TPoint;
    FDragMode: TKSizingGripPosition;
    FDragOrigRect: TRect;
    FDragRect: TRect;
    FHorzExtent: Integer;
    FHorzScrollExtent: Integer;
    FHorzScrollStep: Integer;
    FLinePosition: TKMemoLinePosition;
    FNewTextStyleValid: Boolean;
    FOldCaretRect: TRect;
    FPreferredCaretPos: Integer;
    FRequiredMouseCursor: TCursor;
    FSelectedBlock: TKMemoBlock;
    FVertExtent: Integer;
    FVertScrollExtent: Integer;
    FVertScrollStep: Integer;
    { Inserts a single crCaretPos item into undo list. Unless Force is set to True,
      this change will be inserted only if previous undo item is not crCaretPos. }
    procedure AddUndoCaretPos(Force: Boolean = True); virtual;
    { Inserts a single character change into undo list.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>AItemKind</I> - specifies the undo/redo item reason - most likely
      crInsertChar or crDeleteChar.</LI>
      <LI><I>AData</I> - specifies the character needed to restore the original
      text state</LI>
      <LI><I>AInserted</I> - for the urInsert* items, specifies the current
      @link(TKCustomMemo.InsertMode) status.</LI>
      </UL> }
    procedure AddUndoChar(AItemKind: TKMemoChangeKind; AData: TKChar; AInserted: Boolean = True); virtual;
    { Inserts a string change into undo list.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>AItemKind</I> - specifies the undo/redo item reason - crInsert* or
      crDelete*.</LI>
      <LI><I>AData</I> - specifies the text string needed to restore the original
      text state</LI>
      <LI><I>AInserted</I> - for the urInsert* items, specifies the current
      @link(TKCustomMemo.InsertMode) status.</LI>
      </UL> }
    procedure AddUndoString(AItemKind: TKMemoChangeKind; const AData: TKString; AInserted: Boolean = True); virtual;
    { Notify control about main window background changes. }
    procedure BackgroundChanged(Sender: TObject); virtual;
    { Begins a new undo group. Use the GroupKind parameter to label it. }
    procedure BeginUndoGroup(AGroupKind: TKMemoChangeKind);
    { Converts a rectangle relative to active blocks to a rectangle relative to TKMemo. }
    function BlockRectToRect(const ARect: TRect): TRect; virtual;
    { IKMemoNotifier implementation. }
    procedure BlockFreeNotification(ABlock: TKMemoBlock);
    { IKMemoNotifier implementation. }
    procedure BlocksFreeNotification(ABlocks: TKMemoBlocks);
    { Update the editor after block changes. }
    procedure BlocksChanged(Reasons: TKMemoUpdateReasons);
    { Cancel block dragging. }
    procedure CancelDrag;
    { Determines whether an ecScroll* command can be executed. }
    function CanScroll(ACommand: TKEditCommand): Boolean; virtual;
    { Called by ContentPadding class to update the memo control. }
    procedure ContentPaddingChanged(Sender: TObject); virtual;
    { Overriden method - window handle has been created. }
    procedure CreateHandle; override;
    { Overriden method - defines additional styles for the memo window (scrollbars etc.). }
    procedure CreateParams(var Params: TCreateParams); override;
    { Overriden method - adjusts file drag&drop functionality. }
    procedure CreateWnd; override;
    { Overriden method - adjusts file drag&drop functionality. }
    procedure DestroyWnd; override;
    { Calls the @link(TKCustomMemo.OnChange) event. }
    procedure DoChange; virtual;
    { Performs the Copy command. }
    function DoCopy: Boolean; virtual;
    { Overriden method - handles mouse wheel messages. }
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    { Performs the Paste command. }
    function DoPaste: Boolean; virtual;
    { Performs the Redo command. }
    function DoRedo: Boolean; virtual;
    { Perforns the Search or Replace command. }
    function DoSearchReplace(AReplace: Boolean): Boolean; virtual;
    { Performs the Undo command. }
    function DoUndo: Boolean; virtual;
    { Performs block dragging. }
    procedure DragBlock;
    { IKMemoNotifier implementation. }
    function EditBlock(AItem: TKMemoBlock): Boolean;
    { Closes the undo group created by @link(TKCustomMemo.BeginUndoGroup). }
    procedure EndUndoGroup;
    { Notify blocks about memo font change. }
    procedure FontChange(Sender: TObject); virtual;
    { IKMemoNotifier implementation. }
    function GetActiveBlocks: TKMemoBlocks;
    { IKMemoNotifier implementation. }
    function GetDefaultTextStyle: TKMemoTextStyle;
    { IKMemoNotifier implementation. }
    function GetDefaultParaStyle: TKMemoParaStyle;
    { IKMemoNotifier implementation. }
    function GetDrawSingleChars: Boolean;
    { Returns actual scroll padding in horizontal direction. }
    function GetHorzScrollPadding: Integer; virtual;
    { IKMemoNotifier implementation. }
    function GetLinePosition: TKMemoLinePosition;
    { IKMemoNotifier implementation. }
    function GetListTable: TKMemoListTable;
    { IKMemoNotifier implementation. }
    function GetMemo: TKCustomMemo;
    { IKMemoNotifier implementation. }
    function GetMaxWordLength: Integer;
    { Return nearest paragraph. }
    function GetNearestParagraph: TKMemoParagraph; virtual;
    { Return block index of nearest paragraph. }
    function GetNearestParagraphIndex: Integer; virtual;
    { IKMemoNotifier implementation. }
    function GetPaintSelection: Boolean;
    { IKMemoNotifier implementation. }
    function GetPrinting: Boolean;
    { IKMemoNotifier implementation. }
    function GetReadOnly: Boolean;
    { IKMemoNotifier implementation. }
    procedure GetSelColors(out Foreground, Background: TColor);
    { IKMemoNotifier implementation. }
    function GetSelectedBlock: TKMemoBlock;
    { IKMemoNotifier implementation. }
    function GetShowFormatting: Boolean;
    { Returns "real" selection end - with always higher index value than selection start value. }
    function GetRealSelEnd: Integer; virtual;
    { Returns "real" selection length - always non-negative number. }
    function GetRealSelLength: Integer;
    { Returns "real" selection start - with always lower index value than selection end value. }
    function GetRealSelStart: Integer; virtual;
    { Returns actual scroll padding in vertical direction. }
    function GetVertScrollPadding: Integer; virtual;
    { IKMemoNotifier implementation. }
    function GetWordBreaks: TKSysCharSet;
    { Hides the caret. }
    procedure HideEditorCaret; virtual;
    { Overriden method - processes virtual key strokes according to current @link(TKCustomMemo.KeyMapping). }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
 {$IFDEF FPC}
    { Overriden method - processes character key strokes - data editing. }
    procedure UTF8KeyPress(var Key: TUTF8Char); override;
 {$ELSE}
    { Overriden method - processes character key strokes - data editing. }
    procedure KeyPress(var Key: Char); override;
 {$ENDIF}
    { Overriden method - processes virtual key strokes. }
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    { Update the editor after list table changes. }
    procedure ListChanged(AList: TKMemoList; ALevel: TKMemoListLevel); virtual;
    { Updates information about printed shape. }
    procedure MeasurePages(var Info: TKPrintMeasureInfo); override;
    { Overriden method - updates caret position/selection. }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - updates caret position/selection and initializes scrolling
      when needed. }
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - releases mouse capture acquired by MouseDown. }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Ends block dragging. }
    procedure MoveBlock;
    { Paints the document to specified canvas. }
    procedure PaintContent(ACanvas: TCanvas; const ARect: TRect; ALeftOfs, ATopOfs: Integer);
    { Paints a page to a printer/preview canvas. }
    procedure PaintPage; override;
    { Overriden method - calls PaintContent to paint the document into window client area. }
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    { Reacts on default paragraph style changes and notifies all paragraph blocks. }
    procedure ParaStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons); virtual;
    { Converts a point relative to TKMemo to a point relative to active blocks. }
    function PointToBlockPoint(const APoint: TPoint; ACalcActive: Boolean = True): TPoint; virtual;
    { Make the blocks ready for normal work. }
    procedure PrepareToPaint; virtual;
    { Make the blocks ready for preview or printing. }
    procedure PrepareToPrint(AScale: Double); virtual;
    { Overriden method - calls MeasureExtent to update document metrics for printing. }
    procedure PrintPaintBegin; override;
    { Overriden method - calls necessary functions to update document metrics for normal painting. }
    procedure PrintPaintEnd; override;
    { Grants the input focus to the control when possible and the control has had none before. }
    procedure SafeSetFocus;
    { Scrolls the text either horizontally by DeltaHorz scroll units or vertically
      by DeltaVert scroll units (lines) or in both directions. CodeHorz and CodeVert
      are the codes coming from WM_HSCROLL or WM_VSCROLL messages. }
    function Scroll(CodeHorz, CodeVert, DeltaHorz, DeltaVert: Integer; ACallScrollWindow: Boolean): Boolean;
    { Scrolls the memo window horizontaly by DeltaHorz scroll units and/or
      vertically by DeltaVert scroll units (lines). }
    function ScrollBy(DeltaHorz, DeltaVert: Integer; ACallScrollWindow: Boolean): Boolean;
    { Determines if a cell specified by ACol and ARow should be scrolled, i.e. is
      not fully visible. }
    function ScrollNeeded(AMousePos: PPoint; out DeltaCol, DeltaRow: Integer): Boolean; virtual;
    { Scrolls the memo so that caret will be in the center of client area. }
    procedure ScrollToClientAreaCenter;
    { Expands the current selection and performs all necessary adjustments. }
    procedure SelectionExpand(ASelEnd: Integer; ADoScroll: Boolean = True; APosition: TKMemoLinePosition = eolInside); overload; virtual;
    { Expands the current selection and performs all necessary adjustments. }
    procedure SelectionExpand(const APoint: TPoint; ADoScroll: Boolean = True); overload; virtual;
    { Initializes the current selection and performs all necessary adjustments. }
    procedure SelectionInit(ASelStart: Integer; ADoScroll: Boolean = True; APosition: TKMemoLinePosition = eolInside); overload; virtual;
    { Initializes the current selection and performs all necessary adjustments. }
    procedure SelectionInit(const APoint: TPoint; ADoScroll: Boolean = True); overload; virtual;
    { IKMemoNotifier implementation. }
    function SelectBlock(AItem: TKMemoBlock; APosition: TKSizingGripPosition): Boolean;
    { IKMemoNotifier implementation. }
    procedure SetReqMouseCursor(ACursor: TCursor);
    { Updates mouse cursor according to the state determined from current mouse
      position. Returns True if cursor has been changed. }
    function SetMouseCursor(X, Y: Integer): Boolean; override;
    { Shows the caret. }
    procedure ShowEditorCaret; virtual;
    { Reacts on default text style changes and notifies all text blocks. }
    procedure TextStyleChanged(Sender: TObject); virtual;
    { Calls the @link(TKCustomMemo.DoChange) method. }
    procedure UndoChange(Sender: TObject; ItemKind: TKMemoChangeKind);
    { Updates caret position, shows/hides caret according to the input focus
      <UL>
      <LH>Parameters:</LH>
      <LI><I>AShow</I> - set to True to show the caret</LI>
      </UL> }
    procedure UpdateEditorCaret(AShow: Boolean = True); virtual;
    { Update the mouse cursor. }
    procedure UpdateMouseCursor; virtual;
    { Update the preferred caret horizontal position. }
    procedure UpdatePreferredCaretPos; virtual;
    { Updates the scrolling range. }
    procedure UpdateScrollRange(CallInvalidate: Boolean); virtual;
    { Updates the grid size. }
    procedure UpdateSize; override;
    { Redo list manager - made accessible for descendant classes. }
    property RedoList: TKMemoChangeList read FRedoList;
    { States of this class - made accessible for descendant classes. }
    property States: TKMemoStates read FStates write FStates;
    { Undo list manager - made accessible for descendant classes. }
    property UndoList: TKMemoChangeList read FUndoList;
  public
    { Performs necessary initializations - default values to properties, create
      undo/redo list managers. }
    constructor Create(AOwner: TComponent); override;
    { Destroy instance, undo/redo list managers, dispose buffer... }
    destructor Destroy; override;
    { Takes property values from another TKCustomMemo class. }
    procedure Assign(Source: TPersistent); override;
    { Determines whether the caret is visible. }
    function CaretInView: Boolean; virtual;
    { Forces the caret position to become visible. }
    function ClampInView(AMousePos: PPoint; ACallScrollWindow: Boolean): Boolean; virtual;
    { Clears all blocks. Unlike @link(ecClearAll) clears everything inclusive undo a redo lists. }
    procedure Clear(AKeepOnePara: Boolean = True);
    { Deletes blocks or parts of blocks corresponding to the active selection.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>ATextOnly</I> - don't clear containers if True</LI>
      </UL> }
    procedure ClearSelection(ATextOnly: Boolean = True); virtual;
    { Clears undo (and redo) list. }
    procedure ClearUndo;
    { Determines whether given command can be executed at this time. Use this
      function in TAction.OnUpdate events.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Command</I> - specifies the command to inspect</LI>
      </UL> }
    function CommandEnabled(Command: TKEditCommand): Boolean; virtual;
    { Delete all characters from beginning of line to position given by At. }
    procedure DeleteBOL(At: Integer);
    { Delete character at position At (Delete key). }
    procedure DeleteChar(At: Integer); virtual;
    { Delete all characters from position given by At to the end of line. }
    procedure DeleteEOL(At: Integer);
    { Delete character before position At (Backspace key). }
    procedure DeleteLastChar(At: Integer); virtual;
    { Delete whole line at position At. }
    procedure DeleteLine(At: Integer);
    { Delete block previously selected with @link(TKCustomMemo.SelectBlock), if any. }
    procedure DeleteSelectedBlock; virtual;
    { Executes given command. This function first calls CommandEnabled to
      assure given command can be executed.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Command</I> - specifies the command to execute</LI>
      <LI><I>Data</I> - specifies the data needed for the command</LI>
      </UL> }
    function ExecuteCommand(Command: TKEditCommand; Data: Pointer = nil): Boolean; virtual;
    { Returns current maximum value for the @link(TKCustomMemo.LeftPos) property. }
    function GetMaxLeftPos: Integer; virtual;
    { Returns current maximum value for the @link(TKCustomMemo.TopPos) property. }
    function GetMaxTopPos: Integer; virtual;
    { Returns indexes corresponding to the word at position AIndex. }
    function GetNearestWordIndexes(AIndex: Integer; AIncludeWhiteSpaces: Boolean;
      out AStartIndex, AEndIndex: Integer): Boolean;
    { Converts a text buffer index into client area rectangle.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>AValue</I> - index to convert</LI>
      <LI><I>ACaret</I> - return caret rectangle</LI>
      </UL> }
    function IndexToRect(AValue: Integer; ACaret: Boolean): TRect; virtual;
    { Inserts a character at specified position.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>At</I> - position where the character should be inserted.</LI>
      <LI><I>AValue</I> - character</LI>
      </UL> }
    procedure InsertChar(At: Integer; const AValue: TKChar); virtual;
    { Inserts new line at specified position.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>At</I> - position where the new line should be inserted.</LI>
      </UL> }
    procedure InsertNewLine(At: Integer); virtual;
    { Inserts a string at specified position.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>At</I> - position where the string should be inserted.</LI>
      <LI><I>AValue</I> - inserted string</LI>
      </UL> }
    procedure InsertString(At: Integer; const AValue: TKString); virtual;
    { Load contents from a file. Chooses format automatically by extension.
      Text file is default format. }
    procedure LoadFromFile(const AFileName: TKString); virtual;
    { Load contents from a RTF file. }
    procedure LoadFromRTF(const AFileName: TKString); virtual;
    { Load contents from a RTF stream. }
    procedure LoadFromRTFStream(AStream: TStream; AtIndex: Integer = -1); virtual;
    { Load contents from a plain text file. }
    procedure LoadFromTXT(const AFileName: TKString); virtual;
    { Moves the caret nearest to current mouse position. }
    procedure MoveCaretToMouseCursor(AIfOutsideOfSelection: Boolean);
    { Converts client area coordinates into a text buffer index.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>APoint</I> - window client area coordinates</LI>
      <LI><I>AOutOfArea</I> - set to True to compute selection even if the
      the supplied coordinates are outside of the text space</LI>
      </UL> }
    function PointToIndex(APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    { Save contents to a file. Chooses format automatically by extension. Text file is default format. }
    procedure SaveToFile(const AFileName: TKString; ASelectedOnly: Boolean = False); virtual;
    { Save contents to a RTF file. }
    procedure SaveToRTF(const AFileName: TKString; ASelectedOnly: Boolean = False; AReadableOutput: Boolean = False); virtual;
    { Save contents to a RTF stream. }
    procedure SaveToRTFStream(AStream: TStream; ASelectedOnly: Boolean = False; AReadableOutput: Boolean = False); virtual;
    { Save contents to a plain text file. }
    procedure SaveToTXT(const AFileName: TKString; ASelectedOnly: Boolean = False); virtual;
    { Specifies the current selection. This is faster than combination of SelStart and SelLength. }
    procedure Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean = True); virtual;
    { Activates relative or absolute positioned container nearest to APoint.
      The container blocks will be accessible through ActiveBlocks. }
    procedure SetActiveBlocksForPoint(const APoint: TPoint); virtual;
    { Prepare to insert a new block at given position. Returns requested block index. }
    function SplitAt(AIndex: Integer): Integer; virtual;
    { Gives access to active memo block (the outermost block at caret position within ActiveBlocks). }
    property ActiveBlock: TKMemoBlock read GetActiveBlock;
    { Gives access to innermost active memo block (the innermost block at caret position within ActiveBlocks). }
    property ActiveInnerBlock: TKMemoBlock read GetActiveInnerBlock;
    { Gives access to active memo blocks - containers of texts, images etc..
      ActiveBlocks might be different from Blocks when editing the embedded text box etc. }
    property ActiveBlocks: TKMemoBlocks read FActiveBlocks write SetActiveBlocks;
    { Gives access to innermost active memo blocks - containers of texts, images etc..
      ActiveInnerBlocks might be different from ActiveBlocks when inside of a table etc. }
    property ActiveInnerBlocks: TKMemoBlocks read GetActiveInnerBlocks;
    { Gives access to memo blocks - containers of texts, images etc.. }
    property Blocks: TKMemoBlocks read FBlocks;
    { Main window background. }
    property Background: TKMemoBackground read FBackground write SetBackground;
    { Returns current caret position = selection end. }
    property CaretPos: Integer read GetSelEnd;
    { Returns True if caret is visible. }
    property CaretVisible: Boolean read GetCaretVisible;
    { Makes it possible to take all color properties from another TKCustomMemo class. }
    property Colors: TKMemoColors read FColors write SetColors;
    { Specifies the padding around the memo contents. }
    property ContentPadding: TKRect read FContentPadding write SetContentPadding;
    { Returns height of the memo contents. }
    property ContentHeight: Integer read GetContentHeight;
    { Returns the left position of the memo contents. }
    property ContentLeft: Integer read GetContentLeft;
    { Returns the bounding rectangle of the memo contents. }
    property ContentRect: TRect read GetContentRect;
    { Returns the top position of the memo contents. }
    property ContentTop: Integer read GetContentTop;
    { Returns width of the memo contents. }
    property ContentWidth: Integer read GetContentWidth;
    { Specifies the style how the outline is drawn when editor is disabled. }
    property DisabledDrawStyle: TKEditDisabledDrawStyle read FDisabledDrawStyle write SetDisabledDrawStyle default cEditDisabledDrawStyleDef;
    { Returns True if text buffer is empty. }
    property Empty: Boolean read GetEmpty;
    { Returns horizontal scroll padding - relative to client width. }
    property HorzScrollPadding: Integer read GetHorzScrollPadding;
    { Returns True if insert mode is on. }
    property InsertMode: Boolean read GetInsertMode;
    { Specifies the current key stroke mapping scheme. }
    property KeyMapping: TKEditKeyMapping read FKeyMapping;
    { Specifies the horizontal scroll position. }
    property LeftPos: Integer read FLeftPos write SetLeftPos;
    { Returns the numbering list table. }
    property ListTable: TKMemoListTable read FListTable;
    { Specifies the maximum allowed nonbreakable word length. }
    property MaxWordLength: Integer read FMaxWordLength write SetMaxWordLength default cMaxWordLengthDef;
    { Returns True if the buffer was modified - @link(eoUndoAfterSave) taken into
      account. }
    property Modified: Boolean read GetModified write SetModified;
    { Returns nearest paragraph to caret location. }
    property NearestParagraph: TKMemoParagraph read GetNearestParagraph;
    { Returns block index of nearest paragraph to caret location. }
    property NearestParagraphIndex: Integer read GetNearestParagraphIndex;
    { Specifies text style for newly entered character. }
    property NewTextStyle: TKMemoTextStyle read FNewTextStyle write SetNewTextStyle;
    { Indicates that style for newly entered text is valid and will be used for next character. }
    property NewTextStyleValid: Boolean read FNewTextStyleValid;
    { Specifies the editor options that do not affect painting. }
    property Options: TKEditOptions read FOptions write SetOptions stored IsOptionsStored;
    { Specifies default style for paragraphs. }
    property ParaStyle: TKMemoParaStyle read FParaStyle;
    { Specifies whether the editor has to be read only editor. }
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    { Returns "real" selection end - with always higher index value than selection start value. }
    property RealSelEnd: Integer read GetRealSelEnd;
    { Returns "real" selection lenth - always non-negative number. }
    property RealSelLength: Integer read GetRealSelLength;
    { Returns "real" selection start - with always lower index value than selection end value. }
    property RealSelStart: Integer read GetRealSelStart;
    { Returns true when a relative or absolute positioned block is selected. }
    property RelativeSelected: Boolean read GetRelativeSelected;
    { Specifies the required content width. }
    property RequiredContentWidth: Integer read GetRequiredContentWidth write SetRequiredContentWidth;
    { Allows to save and load the memo contents to/from RTF string.}
    property RTF: TKMemoRTFString read GetRTF write SetRTF;
    { Defines visible scrollbars - horizontal, vertical or both. }
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    { Specifies how fast the scrolling by timer should be. }
    property ScrollSpeed: Cardinal read FScrollSpeed write SetScrollSpeed default cScrollSpeedDef;
    { Specifies the padding in pixels to overscroll to show caret position or selection end. }
    property ScrollPadding: Integer read FScrollPadding write SetScrollPadding default cScrollPaddingDef;
    { Determines whether a selection is available. }
    property SelAvail: Boolean read GetSelAvail;
    { Returns selectable length. }
    property SelectableLength: Integer read GetSelectableLength;
    { Returns block previously selected with @link(TKCustomMemo.SelectBlock). }
    property SelectedBlock: TKMemoBlock read FSelectedBlock;
    { Determines whether a selection contains a paragraph. }
    property SelectionHasPara: Boolean read GetSelectionHasPara;
    { Specifies paragraph style for active selection. }
    property SelectionParaStyle: TKMemoParaStyle read GetSelectionParaStyle write SetSelectionParaStyle;
    { Specifies text style for active selection. }
    property SelectionTextStyle: TKMemoTextStyle read GetSelectionTextStyle write SetSelectionTextStyle;
    { Specifies the current selection end. }
    property SelEnd: Integer read GetSelEnd write SetSelEnd;
    { Specifies the current selection length. SelStart remains unchanged, SelEnd will be
      updated accordingly. To mark a selection, either set both SelStart and SelEnd properties
      or both SelStart and SelLength properties. }
    property SelLength: Integer read GetSelLength write SetSelLength;
    { Specifies the current selection start. }
    property SelStart: Integer read GetSelStart write SetSelStart;
    { Returns selected text. }
    property SelText: TKString read GetSelText;
    { If read, returns the textual part of the contents as a whole. If written, replace previous contents by a new one. }
    property Text: TKString read GetText write SetText;
    { Specifies default style for text. }
    property TextStyle: TKMemoTextStyle read FTextStyle;
    { Specifies the vertical scroll position. }
    property TopPos: Integer read FTopPos write SetTopPos;
    { Specifies the maximum number of undo items. Please note this value
      affects the undo item limit, not undo group limit. }
    property UndoLimit: Integer read GetUndoLimit write SetUndoLimit default cUndoLimitDef;
    { Returns vertical scroll padding - relative to client height. }
    property VertScrollPadding: Integer read GetVertScrollPadding;
    { Defines the characters that will be used to split text to breakable words. }
    property WordBreaks: TKSysCharSet read FWordBreaks write SetWordBreaks;
    { When assigned, this event will be invoked at each change made to the
      text buffer either by the user or programmatically by public functions. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { When assigned, this event will be invoked when the user drops any files onto
      the window. }
    property OnDropFiles: TKEditDropFilesEvent read FOnDropFiles write FOnDropFiles;
    { When assigned, this event will be invoked if some internal event in KMemo needs to edit
      a block externally. }
    property OnEditBlock: TKMemoEditBlockEvent read FOnEditBlock write FOnEditBlock;
    { When assigned, this event will be invoked at each prompt-forced search match. }
    property OnReplaceText: TKEditReplaceTextEvent read FOnReplaceText write FOnReplaceText;
  end;

  { @abstract(Memo design-time component) }
  TKMemo = class(TKCustomMemo)
  published
    { Inherited property - see Delphi help. }
    property Align;
    { Inherited property - see Delphi help. }
    property Anchors;
    { See TKCustomMemo.@link(TKCustomMemo.Background) for details. }
    property Background;
    { See TKCustomControl.@link(TKCustomControl.BorderStyle) for details. }
    property BorderStyle;
    { Inherited property - see Delphi help. }
    property BorderWidth;
    { See TKCustomMemo.@link(TKCustomMemo.Colors) for details. }
    property Colors;
    { Inherited property - see Delphi help. }
    property Constraints;
    { See TKCustomMemo.@link(TKCustomMemo.ContentPadding) for details. }
    property ContentPadding;
  {$IFNDEF FPC}
    { Inherited property - see Delphi help. }
    property Ctl3D;
  {$ENDIF}
    { See TKCustomMemo.@link(TKCustomMemo.DisabledDrawStyle) for details. }
    property DisabledDrawStyle;
    { Inherited property - see Delphi help. }
    property DragCursor;
    { Inherited property - see Delphi help. }
    property DragKind;
    { Inherited property - see Delphi help. }
    property DragMode;
    { Inherited property - see Delphi help. }
    property Enabled;
    { Inherited property - see Delphi help. Font pitch must always remain fpFixed
      - specify fixed fonts only. Font.Size will also be trimmed if too small or big. }
    property Font;
    { Inherited property - see Delphi help. }
    property Height default cHeight;
    { See TKCustomMemo.@link(TKCustomMemo.Options) for details. }
    property Options;
    { Inherited property - see Delphi help. }
    property ParentShowHint;
    { Inherited property - see Delphi help. }
    property PopupMenu;
    { See TKCustomMemo.@link(TKCustomMemo.ReadOnly) for details. }
    property ReadOnly;
    { See TKCustomMemo.@link(TKCustomMemo.ScrollBars) for details. }
    property ScrollBars;
    { See TKCustomMemo.@link(TKCustomMemo.ScrollPadding) for details. }
    property ScrollPadding;
    { See TKCustomMemo.@link(TKCustomMemo.ScrollSpeed) for details. }
    property ScrollSpeed;
    { Inherited property - see Delphi help. }
    property ShowHint;
    { Inherited property - see Delphi help. }
    property TabOrder;
    { Inherited property - see Delphi help. }
    property TabStop default True;
    { See TKCustomMemo.@link(TKCustomMemo.UndoLimit) for details. }
    property UndoLimit;
    { Inherited property - see Delphi help. }
    property Visible;
    { Inherited property - see Delphi help. }
    property Width default cWidth;
    { See TKCustomMemo.@link(TKCustomMemo.OnChange) for details. }
    property OnChange;
    { Inherited property - see Delphi help. }
    property OnClick;
    { Inherited property - see Delphi help. }
    property OnContextPopup;
    { Inherited property - see Delphi help. }
    property OnDblClick;
    { Inherited property - see Delphi help. }
    property OnDockDrop;
    { Inherited property - see Delphi help. }
    property OnDockOver;
    { Inherited property - see Delphi help. }
    property OnDragDrop;
    { Inherited property - see Delphi help. }
    property OnDragOver;
    { See TKCustomMemo.@link(TKCustomMemo.OnDropFiles) for details. }
    property OnDropFiles;
    { See TKCustomMemo.@link(TKCustomMemo.OnEditBlock) for details. }
    property OnEditBlock;
    { Inherited property - see Delphi help. }
    property OnEndDock;
    { Inherited property - see Delphi help. }
    property OnEndDrag;
    { Inherited property - see Delphi help. }
    property OnEnter;
    { Inherited property - see Delphi help. }
    property OnExit;
    { Inherited property - see Delphi help. }
    property OnGetSiteInfo;
    { Inherited property - see Delphi help. }
    property OnKeyDown;
    { Inherited property - see Delphi help. }
    property OnKeyPress;
    { Inherited property - see Delphi help. }
    property OnKeyUp;
    { Inherited property - see Delphi help. }
    property OnMouseDown;
  {$IFDEF COMPILER9_UP}
    { Inherited property - see Delphi help. }
    property OnMouseEnter;
    { Inherited property - see Delphi help. }
    property OnMouseLeave;
  {$ENDIF}
    { Inherited property - see Delphi help. }
    property OnMouseMove;
    { Inherited property - see Delphi help. }
    property OnMouseUp;
    { Inherited property - see Delphi help. }
    property OnMouseWheel;
    { Inherited property - see Delphi help. }
    property OnMouseWheelDown;
    { Inherited property - see Delphi help. }
    property OnMouseWheelUp;
    { See TKCustomMemo.@link(TKCustomMemo.OnReplaceText) for details. }
    property OnReplaceText;
    { Inherited property - see Delphi help. }
    property OnResize;
    { Inherited property - see Delphi help. }
    property OnStartDock;
    { Inherited property - see Delphi help. }
    property OnStartDrag;
    { Inherited property - see Delphi help. }
    property OnUnDock;
  end;

  { Base class for standard actions targeting TKMemo. }
  TKMemoEditAction = class(TAction)
  protected
    function GetEditCommand: TKEditCommand; virtual; abstract;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TKMemoEditCopyAction = class(TKMemoEditAction)
  protected
    function GetEditCommand: TKEditCommand; override;
  end;

  TKMemoEditCutAction = class(TKMemoEditAction)
  protected
    function GetEditCommand: TKEditCommand; override;
  end;

  TKMemoEditPasteAction = class(TKMemoEditAction)
  protected
    function GetEditCommand: TKEditCommand; override;
  end;

  TKMemoEditSelectAllAction = class(TKMemoEditAction)
  protected
    function GetEditCommand: TKEditCommand; override;
  end;

function NewLineChar: TKString;
function SpaceChar: TKString;
function TabChar: TKString;

implementation

uses
{$IFDEF USE_WINAPI}
  ShellApi,
{$ENDIF}
  ClipBrd, Printers,
{$IFDEF USE_THEMES}
  Themes,
{$ENDIF}
  Math, KMemoRTF;

{$IFDEF USE_WINAPI}
// this is better declaration of GetKerningPairs than in Windows.pas
type
  TKerningPair = packed record
    wFirst: Word;
    wSecond: Word;
    iKernAmount: Integer;
  end;

  TKerningPairs = array[0..MaxInt div SizeOf(TKerningPair) - 1] of TKerningPair;
  PKerningPairs = ^TKerningPairs;

function GetKerningPairs(DC: HDC; Count: DWORD; KerningPairs: PKerningPairs): DWORD; stdcall; external 'gdi32.dll' name 'GetKerningPairs';
{$ENDIF}

procedure DefaultsToBrush(ABrush: TBrush);
begin
  ABrush.Color := clWindow;
  ABrush.Style := bsClear;
end;

procedure DefaultsToFont(AFont: TFont);
begin
  AFont.Name := 'Arial';
  AFont.Charset := DEFAULT_CHARSET;
  AFont.Color := clWindowText;
  AFont.Size := 10;
  AFont.Style := [];
end;

function OppositeKind(ItemKind: TKMemoChangeKind): TKMemoChangeKind;
begin
  case ItemKind of
    ckDelete: Result := ckInsert;
    ckInsert: Result := ckDelete;
  else
    Result := ItemKind;
  end;
end;

function NewLineChar: TKString;
begin
  Result := UnicodeToNativeUTF(cNewlineChar);
end;

function SpaceChar: TKString;
begin
  Result := UnicodeToNativeUTF(cSpaceChar);
end;

function TabChar: TKString;
begin
  Result := UnicodeToNativeUTF(cTabChar);
end;

{ TKMemoSparseItem }

procedure TKMemoSparseItem.Assign(ASource: TKObject);
begin
  if ASource is TKMemoSparseitem then
    FIndex := TKMemoSparseItem(ASource).Index;
end;

constructor TKMemoSparseItem.Create;
begin
  inherited;
  FIndex := 0;
end;

function TKMemoSparseItem.EqualProperties(ASource: TKObject): Boolean;
begin
  if ASource is TKMemoSparseitem then
    Result :=
      (FIndex = TKMemoSparseItem(ASource).Index)
  else
    Result := False;
end;

{ TKMemoSparseList }

procedure TKMemoSparseList.AddItem(AValue: Integer);
var
  Item: TKMemoSparseItem;
begin
  Item := TKMemoSparseItem.Create;
  Item.Index := AValue;
  Add(Item);
end;

function TKMemoSparseList.GetItem(Index: Integer): TKMemoSparseItem;
begin
  Result := TKMemoSparseItem(inherited GetItem(Index));
end;

procedure TKMemoSparseList.SetItem(Index: Integer; const Value: TKMemoSparseItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TKMemoSparseList.SetSize(ACount: Integer);
var
  I: Integer;
begin
  if ACount <> Count then
  begin
    if ACount > Count then
    begin
      for I := Count to ACount - 1 do
        AddItem(0);
    end else
    begin
      for I := ACount to Count - 1 do
        Delete(ACount);
    end;
  end;
end;

{ TKMemoSparseStack }

function TKMemoSparseStack.Peek: TKMemoSparseItem;
begin
  Result := TKMemoSparseItem(inherited Peek);
end;

function TKMemoSparseStack.Pop: TKMemoSparseItem;
begin
  Result := TKMemoSparseItem(inherited Pop);
end;

function TKMemoSparseStack.PopValue: Integer;
var
  Item: TKMemoSparseItem;
begin
  if Peek <> nil then
  begin
    Item := Pop;
    Result := Item.Index;
    Item.Free;
  end else
    Result := 0;
end;

function TKMemoSparseStack.Push(AObject: TKMemoSparseItem): TKMemoSparseItem;
begin
  Result := TKMemoSparseItem(inherited Push(AObject));
end;

procedure TKMemoSparseStack.PushValue(Value: Integer);
var
  Item: TKMemoSparseItem;
begin
  Item := TKMemoSparseItem.Create;
  Item.Index := Value;
  Push(Item);
end;

{ TKMemoDictionaryItem }

constructor TKMemoDictionaryItem.Create;
begin
  inherited;
  FIndex := 0;
  FValue := 0;
end;

procedure TKMemoDictionaryItem.Assign(ASource: TKObject);
begin
  if ASource is TKMemoDictionaryItem then
  begin
    FIndex := TKMemoDictionaryItem(ASource).Index;
    FValue := TKMemoDictionaryItem(ASource).Value;
  end;
end;

function TKMemoDictionaryItem.EqualProperties(ASource: TKObject): Boolean;
begin
  if ASource is TKMemoDictionaryItem then
    Result :=
      (FIndex = TKMemoDictionaryItem(ASource).Index) and
      (FValue = TKMemoDictionaryItem(ASource).Value)
  else
    Result := False;
end;

{ TKMemoDictionary }

procedure TKMemoDictionary.AddItem(AIndex, AValue: Integer);
var
  Item: TKMemoDictionaryItem;
begin
  Item := TKMemoDictionaryItem.Create;
  Item.Index := AIndex;
  Item.Value := AValue;
  Add(Item);
end;

function TKMemoDictionary.FindItem(AIndex: Integer): TKMemoDictionaryItem;
var
  I: Integer;
begin
  // this is slow index based search but we don't need fast hash table here
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Index = AIndex then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TKMemoDictionary.GetValue(AIndex, ADefault: Integer): Integer;
var
  Item: TKMemoDictionaryItem;
begin
  Item := FindItem(AIndex);
  if Item <> nil then
    Result := Item.Value
  else
    Result := ADefault;
end;

function TKMemoDictionary.GetItem(Index: Integer): TKMemoDictionaryItem;
begin
  Result := TKMemoDictionaryItem(inherited GetItem(Index));
end;

procedure TKMemoDictionary.SetItem(Index: Integer; const Value: TKMemoDictionaryItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TKMemoDictionary.SetValue(AIndex, AValue: Integer);
var
  Item: TKMemoDictionaryItem;
begin
  Item := FindItem(AIndex);
  if Item <> nil then
    Item.Value := AValue
  else
    AddItem(AIndex, AValue);
end;

{ TKMemoNumberingFormatItem }

procedure TKMemoNumberingFormatItem.Assign(ASource: TKObject);
begin
  if ASource is TKMemoNumberingFormatItem then
  begin
    FLevel := TKMemoNumberingFormatItem(ASource).Level;
    FText := TKMemoNumberingFormatItem(ASource).Text;
  end;
end;

constructor TKMemoNumberingFormatItem.Create;
begin
  inherited;
  FLevel := -1;
  FText := '';
end;

{ TKMemoNumberingFormat }

procedure TKMemoNumberingFormat.AddItem(ALevel: Integer; const AText: TKString);
var
  Item: TKMemoNumberingFormatItem;
begin
  Item := TKMemoNumberingFormatItem.Create;
  Item.Level := ALevel;
  Item.Text := AText;
  Add(Item);
end;

procedure TKMemoNumberingFormat.Defaults(ANumbering: TKMemoParaNumbering; ALevelIndex: Integer);
var
  I: Integer;
begin
  Clear;
  case ANumbering of
    pnuBullets:
    begin
      AddItem(-1, UnicodeToNativeUTF(cBullet));
    end;
    pnuArabic, pnuLetterLo, pnuLetterHi, pnuRomanLo, pnuRomanHi:
    begin
      for I := 0 to ALevelIndex do
      begin
        AddItem(I, '');
        AddItem(-1, '.');
      end;
    end;
  end;
end;

function TKMemoNumberingFormat.GetItem(Index: Integer): TKMemoNumberingFormatItem;
begin
  Result := TKMemoNumberingFormatItem(inherited GetItem(Index));
end;

function TKMemoNumberingFormat.GetLevelCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if (Items[I].Level >= 0) and (Items[I].Text = '') then
      Inc(Result);
end;

procedure TKMemoNumberingFormat.InsertItem(AAt, ALevel: Integer; const AText: TKString);
var
  Item: TKMemoNumberingFormatItem;
begin
  Item := TKMemoNumberingFormatItem.Create;
  Item.Level := ALevel;
  Item.Text := AText;
  Insert(AAt, Item);
end;

procedure TKMemoNumberingFormat.SetItem(Index: Integer; const Value: TKMemoNumberingFormatItem);
begin
  inherited SetItem(Index, Value);
end;

{ TKMemoListLevel }

constructor TKMemoListLevel.Create;
begin
  inherited;
  FFirstIndent := 0;
  FLeftIndent := 0;
  FNumberingFont := TFont.Create;
  DefaultsToFont(FNumberingFont);
  FNumberingFont.OnChange := FontChanged;
  FNumberingFontChanged := False;
  FNumberingFormat := TKMemoNumberingFormat.Create;
  FNumberingFormat.Defaults(pnuNone, 0);
  FNumberStartAt := 1;
  FLevelCounter := FNumberStartAt - 1;
end;

destructor TKMemoListLevel.Destroy;
begin
  FNumberingFont.Free;
  FNumberingFormat.Free;
  inherited;
end;

procedure TKMemoListLevel.Assign(ASource: TKObject);
begin
  if ASource is TKMemoListLevel then
  begin
    FirstIndent := TKMemoListLevel(ASource).FirstIndent;
    LeftIndent := TKMemoListLevel(ASource).LeftIndent;
    Numbering := TKMemoListLevel(ASource).Numbering;
    NumberingFont.Assign(TKMemoListLevel(ASource).NumberingFont);
    FNumberingFontChanged := TKMemoListLevel(ASource).NumberingFontChanged;
    NumberingFormat.Assign(TKMemoListLevel(ASource).NumberingFormat);
    NumberStartAt := TKMemoListLevel(ASource).NumberStartAt;
  end;
end;

procedure TKMemoListLevel.Changed;
begin
  if Parent <> nil then
    TKMemoListLevels(Parent).Changed(Self);
end;

procedure TKMemoListLevel.FontChanged(Sender: TObject);
begin
  FNumberingFontChanged := True;
end;

procedure TKMemoListLevel.SetFirstIndent(const Value: Integer);
begin
  if Value <> FFirstIndent then
  begin
    FFirstIndent := Value;
    Changed;
  end;
end;

procedure TKMemoListLevel.SetLeftPadding(const Value: Integer);
begin
  if Value <> FLeftIndent then
  begin
    FLeftIndent := Value;
    Changed;
  end;
end;

procedure TKMemoListLevel.SetNumbering(const Value: TKMemoParaNumbering);
var
  LevelIndex: Integer;
begin
  if Value <> FNumbering then
  begin
    FNumbering := Value;
    if Parent <> nil then
      LevelIndex := Parent.IndexOf(Self)
    else
      Levelindex := 0;
    FNumberingFormat.Defaults(Value, LevelIndex);
    Changed;
  end;
end;

procedure TKMemoListLevel.SetNumberStartAt(const Value: Integer);
begin
  if Value <> FNumberStartAt then
  begin
    FNumberStartAt := Value;
    FLevelCounter := FNumberStartAt - 1;
    Changed;
  end;
end;

{ TKMemoListLevels }

constructor TKMemoListLevels.Create;
begin
  inherited;
  FParent := nil;
end;

procedure TKMemoListLevels.Changed(ALevel: TKMemoListLevel);
begin
  if FParent <> nil then
    FParent.LevelChanged(ALevel);
end;

procedure TKMemoListLevels.ClearLevelCounters(AFromLevel: Integer);
var
  I: Integer;
begin
  for I := AFromLevel to Count - 1 do
    Items[I].LevelCounter := Items[I].NumberStartAt - 1;
end;

function TKMemoListLevels.GetItem(Index: Integer): TKMemoListLevel;
begin
  Result := TKMemoListLevel(inherited GetItem(Index));
end;

procedure TKMemoListLevels.SetItem(Index: Integer;
  const Value: TKMemoListLevel);
begin
  inherited SetItem(Index, Value);
end;

{ TKMemoList }

constructor TKMemoList.Create;
begin
  inherited;
  FLevels := TKMemoListLevels.Create;
  FLevels.Parent := Self;
end;

destructor TKMemoList.Destroy;
begin
  FLevels.Free;
  inherited;
end;

procedure TKMemoList.Assign(ASource: TKObject);
begin
  if ASource is TKMemoList then
  begin
    ID := TKMemoList(ASource).ID;
    Levels.Assign(TKMemoList(ASource).Levels);
  end;
end;

procedure TKMemoList.LevelChanged(ALevel: TKMemoListLevel);
begin
  if Parent <> nil then
    TKMemoListTable(Parent).ListChanged(Self, ALevel);
end;

procedure TKMemoList.ParentChanged;
begin
  if Parent <> nil then
    FID := TKMemoListTable(Parent).NextID
  else
    FID := Random(MaxInt);
end;

{ TKMemoListTable }

constructor TKMemoListTable.Create;
begin
  inherited;
  FCallUpdate := False;
  FOnChanged := nil;
end;

procedure TKMemoListTable.CallAfterUpdate;
begin
  if FCallUpdate then
    DoChanged(nil, nil);
end;

procedure TKMemoListTable.CallBeforeUpdate;
begin
  FCallUpdate := False;
end;

procedure TKMemoListTable.ClearLevelCounters;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Levels.ClearLevelCounters(0);
end;

procedure TKMemoListTable.DoChanged(AList: TKMemoList; ALevel: TKMemoListLevel);
begin
  if Assigned(FOnChanged) then
    FOnChanged(AList, ALevel);
end;

function TKMemoListTable.FindByID(AListID: Integer): TKMemoList;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].ID = AListID then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TKMemoListTable.GetItem(Index: Integer): TKMemoList;
begin
  Result := TKMemoList(inherited GetItem(Index));
end;

procedure TKMemoListTable.ListChanged(AList: TKMemoList; ALevel: TKMemoListLevel);
begin
  if UpdateUnlocked then
    DoChanged(AList, ALevel)
  else
    FCallUpdate := True;
end;

function TKMemoListTable.ListByNumbering(AListID, ALevelIndex: Integer; ANumbering: TKMemoParaNumbering): TKMemoList;
var
  I, J: Integer;
  List: TKMemoList;
  ListLevel: TKMemoListLevel;
begin
  Result := nil;
  if ANumbering <> pnuNone then
  begin
    ALevelIndex := Max(ALevelIndex, 0);
    // search for existing list
    List := FindByID(AListID);
    if List <> nil then
    begin
      // list found, use it
      if ALevelIndex < List.Levels.Count then
      begin
        // level found, modify it
        ListLevel := List.Levels[ALevelIndex];
        ListLevel.Numbering := ANumbering;
      end else
      begin
        // add missing levels
        for J := List.Levels.Count to ALevelIndex do
        begin
          ListLevel := TKMemoListLevel.Create;
          List.Levels.Add(ListLevel);
          ListLevel.Numbering := ANumbering;
        end;
      end;
      Result := List;
    end else
    begin
      // list not found, so search list table, if some list has the wanted numbering at requested level
      for I := 0 to Count - 1 do
      begin
        List := Items[I];
        if ALevelIndex < List.Levels.Count then
        begin
          if List.Levels[ALevelIndex].Numbering = ANumbering then
          begin
            Result := List;
            Break;
          end;
        end else
        begin
          // use first available list and add all the missing levels
          for J := List.Levels.Count to ALevelIndex do
          begin
            ListLevel := TKMemoListLevel.Create;
            List.Levels.Add(ListLevel);
            ListLevel.Numbering := ANumbering;
          end;
          Result := List;
          Break;
        end;
      end;
    end;
    if Result = nil then
    begin
      // no suitable list found in table, so create new one
      List := TKMemoList.Create;
      for I := 0 to ALevelIndex do
      begin
        ListLevel := TKMemoListLevel.Create;
        List.Levels.Add(ListLevel);
        ListLevel.Numbering := ANumbering;
      end;
      Add(List);
      Result := List;
    end;
  end;
end;

function TKMemoListTable.NextID: Integer;
var
  I, MaxID: Integer;
begin
  MaxID := cInvalidListID;
  for I := 0 to Count - 1 do
    MaxID := Max(MaxID, Items[I].ID);
  Inc(MaxID); // assume there will never be an overflow
  Result := MaxID;
end;

procedure TKMemoListTable.SetItem(Index: Integer; const Value: TKMemoList);
begin
  inherited SetItem(Index, Value);
end;

{ TKMemoBackground }

constructor TKMemoBackground.Create;
begin
  inherited;
  FColor := clNone;
  FImage := TPicture.Create;
  FImage.OnChange := ImageChanged;
  FRepeatX := True;
  FRepeatY := True;
  FOnChanged := nil;
end;

destructor TKMemoBackground.Destroy;
begin
  FImage.Free;
  inherited;
end;

procedure TKMemoBackground.ImageChanged(Sender: TObject);
begin
  Changed;
end;

procedure TKMemoBackground.Assign(ASource: TPersistent);
begin
  if ASource is TKMemoTextStyle then
  begin
    Color := TKMemoBackground(ASource).Color;
    Image.Assign(TKMemoBackground(ASource).Image);
    RepeatX := TKMemoBackground(ASource).RepeatX;
    RepeatY := TKMemoBackground(ASource).RepeatY;
  end;
end;

procedure TKMemoBackground.Clear;
begin
  FImage.Graphic := nil;
  FColor := clNone;
end;

procedure TKMemoBackground.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TKMemoBackground.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
end;

procedure TKMemoBackground.SetRepeatX(const Value: Boolean);
begin
  if Value <> FRepeatX then
  begin
    FRepeatX := Value;
    Changed;
  end;
end;

procedure TKMemoBackground.SetRepeatY(const Value: Boolean);
begin
  if Value <> FRepeatY then
  begin
    FRepeatY := Value;
    Changed;
  end;
end;

procedure TKMemoBackground.Update;
begin
  inherited;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{ TKMemoTextStyle }

constructor TKMemoTextStyle.Create;
begin
  inherited;
  FChangeable := True;
  FBrush := TBrush.Create;
  FBrush.OnChange := BrushChanged;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FOnChanged := nil;
  Defaults;
end;

procedure TKMemoTextStyle.Defaults;
var
  OldState: Boolean;
begin
  FAllowBrush := True;
  FCapitals := tcaNone;
  FScriptPosition := tpoNormal;
  FStyleChanged := False;
  OldState := Changeable;
  Changeable := False;
  try
    DefaultsToBrush(FBrush);
    DefaultsToFont(FFont);
  finally
    Changeable := OldState;
  end;
end;

destructor TKMemoTextStyle.Destroy;
begin
  FBrush.Free;
  FFont.Free;
  inherited;
end;

procedure TKMemoTextStyle.Assign(ASource: TPersistent);
begin
  if ASource is TKMemoTextStyle then
  begin
    LockUpdate;
    try
      Brush.Assign(TKMemoTextStyle(ASource).Brush);
      Capitals := TKMemoTextStyle(ASource).Capitals;
      Font.Assign(TKMemoTextStyle(ASource).Font);
      ScriptPosition := TKMemoTextStyle(ASource).ScriptPosition;
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKMemoTextStyle.AssignNC(ASource: TKMemoTextStyle);
var
  OldState: Boolean;
begin
  OldState := Changeable;
  Changeable := False;
  try
    Assign(ASource);
  finally
    Changeable := OldState;
  end;
end;

procedure TKMemoTextStyle.BrushChanged(Sender: TObject);
begin
  if UpdateUnlocked then
    FBrushChanged := True;
  PropsChanged;
end;

function TKMemoTextStyle.EqualProperties(ASource: TKMemoTextStyle): Boolean;
begin
  if ASource <> nil then
  begin
    Result :=
      (ASource.AllowBrush = AllowBrush) and
      (ASource.Capitals = Capitals) and
      (ASource.ScriptPosition = ScriptPosition) and
      CompareBrushes(ASource.Brush, Brush) and
      CompareFonts(ASource.Font, Font);
  end else
    Result := False;
end;

procedure TKMemoTextStyle.FontChanged(Sender: TObject);
begin
  if UpdateUnlocked then
    FFontChanged := True;
  PropsChanged;
end;

procedure TKMemoTextStyle.NotifyChange(AValue: TKMemoTextStyle);
begin
  if not FStyleChanged and UpdateUnlocked then
    AssignNC(AValue);
end;

procedure TKMemoTextStyle.PropsChanged;
begin
  if Changeable then
    FStyleChanged := True;
  Changed;
end;

procedure TKMemoTextStyle.SetAllowBrush(const Value: Boolean);
begin
  if Value <> FAllowBrush then
  begin
    FAllowBrush := Value;
    PropsChanged;
  end;
end;

procedure TKMemoTextStyle.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TKMemoTextStyle.SetCapitals(const Value: TKMemoScriptCapitals);
begin
  if Value <> FCapitals then
  begin
    FCapitals := Value;
    PropsChanged;
  end;
end;

procedure TKMemoTextStyle.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TKMemoTextStyle.SetScriptPosition(const Value: TKMemoScriptPosition);
begin
  if Value <> FScriptPosition then
  begin
    FScriptPosition := Value;
    PropsChanged;
  end;
end;

procedure TKMemoTextStyle.Update;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{ TKMemoParagraphStyle }

constructor TKMemoBlockStyle.Create;
begin
  inherited;
  FChangeable := True;
  FBorderWidths := TKRect.Create;
  FBorderWidths.OnChanged := BrushChanged;
  FBrush := TBrush.Create;
  FBrush.OnChange := BrushChanged;
  FContentMargin := TKRect.Create;
  FContentMargin.OnChanged := BrushChanged;
  FContentPadding := TKRect.Create;
  FContentPadding.OnChanged := BrushChanged;
  FFillBlip := nil;
  FOnChanged := nil;
  Defaults;
end;

destructor TKMemoBlockStyle.Destroy;
begin
  FBorderWidths.Free;
  FBrush.Free;
  FContentPadding.Free;
  FContentMargin.Free;
  FFillBlip.Free;
  inherited;
end;

procedure TKMemoBlockStyle.Assign(ASource: TPersistent);
begin
  if ASource is TKMemoBlockStyle then
  begin
    LockUpdate;
    try
      BorderColor := TKMemoBlockStyle(ASource).BorderColor;
      BorderRadius := TKMemoBlockStyle(ASource).BorderRadius;
      BorderWidth := TKMemoBlockStyle(ASource).BorderWidth;
      BorderWidths.Assign(TKMemoBlockStyle(ASource).BorderWidths);
      Brush.Assign(TKMemoBlockStyle(ASource).Brush);
      WrapMode := TKMemoParaStyle(ASource).WrapMode;
      ContentMargin.Assign(TKMemoBlockStyle(ASource).ContentMargin);
      ContentPadding.Assign(TKMemoBlockStyle(ASource).ContentPadding);
      HAlign := TKMemoParaStyle(ASource).HAlign;
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKMemoBlockStyle.AssignNC(ASource: TKMemoBlockStyle);
var
  OldState: Boolean;
begin
  OldState := Changeable;
  Changeable := False;
  try
    Assign(ASource);
  finally
    Changeable := OldState;
  end;
end;

function TKMemoBlockStyle.BorderRect(const ARect: TRect): TRect;
begin
  Result := ARect;
  if FBorderWidths.NonZero then
  begin
    Inc(Result.Left, FBorderWidths.Left);
    Inc(Result.Top, FBorderWidths.Top);
    Dec(Result.Right, FBorderWidths.Right);
    Dec(Result.Bottom, FBorderWidths.Bottom);
  end else
    InflateRect(Result, -FBorderWidth, -FBorderWidth);
end;

procedure TKMemoBlockStyle.BrushChanged(Sender: TObject);
begin
  PropsChanged([muExtent]);
end;

procedure TKMemoBlockStyle.Defaults;
begin
  FBorderColor := clBlack;
  FBorderRadius := 0;
  FBorderWidth := 0;
  FBorderWidths.All := 0;
  FBrush.Style := bsClear;
  FContentPadding.All := 0;
  FContentMargin.All := 0;
  FHAlign := halLeft;
  FStyleChanged := False;
  FWrapMode := wrAround;
end;

function TKMemoBlockStyle.GetAllPaddingsBottom: Integer;
begin
  Result := BottomBorderWidth + BottomMargin + BottomPadding;
end;

function TKMemoBlockStyle.GetAllPaddingsLeft: Integer;
begin
  Result := LeftBorderWidth + LeftMargin + LeftPadding;
end;

function TKMemoBlockStyle.GetAllPaddingsRight: Integer;
begin
  Result := RightBorderWidth + RightMargin + RightPadding;
end;

function TKMemoBlockStyle.GetAllPaddingsTop: Integer;
begin
  Result := TopBorderWidth + TopMargin + TopPadding;
end;

function TKMemoBlockStyle.GetBottomBorderWidth: Integer;
begin
  Result := Max(FBorderWidths.Bottom, FBorderWidth);
end;

function TKMemoBlockStyle.GetBottomMargin: Integer;
begin
  Result := FContentMargin.Bottom;
end;

function TKMemoBlockStyle.GetBottomPadding: Integer;
begin
  Result := FContentPadding.Bottom;
end;

function TKMemoBlockStyle.GetLeftBorderWidth: Integer;
begin
  Result := Max(FBorderWidths.Left, FBorderWidth);
end;

function TKMemoBlockStyle.GetLeftMargin: Integer;
begin
  Result := FContentMargin.Left;
end;

function TKMemoBlockStyle.GetLeftPadding: Integer;
begin
  Result := FContentPadding.Left;
end;

function TKMemoBlockStyle.GetRightBorderWidth: Integer;
begin
  Result := Max(FBorderWidths.Right, FBorderWidth);
end;

function TKMemoBlockStyle.GetRightMargin: Integer;
begin
  Result := FContentMargin.Right;
end;

function TKMemoBlockStyle.GetRightPadding: Integer;
begin
  Result := FContentPadding.Right;
end;

function TKMemoBlockStyle.GetTopBorderWidth: Integer;
begin
  Result := Max(FBorderWidths.Top, FBorderWidth);
end;

function TKMemoBlockStyle.GetTopMargin: Integer;
begin
  Result := FContentMargin.Top;
end;

function TKMemoBlockStyle.GetTopPadding: Integer;
begin
  Result := FContentPadding.Top;
end;

function TKMemoBlockStyle.InteriorRect(const ARect: TRect): TRect;
begin
  Result := ARect;
  if FContentPadding.NonZero then
  begin
    Inc(Result.Left, FContentPadding.Left);
    Inc(Result.Top, FContentPadding.Top);
    Dec(Result.Right, FContentPadding.Right);
    Dec(Result.Bottom, FContentPadding.Bottom);
  end;
end;

function TKMemoBlockStyle.MarginRect(const ARect: TRect): TRect;
begin
  Result := ARect;
  if FContentMargin.NonZero then
  begin
    Inc(Result.Left, FContentMargin.Left);
    Inc(Result.Top, FContentMargin.Top);
    Dec(Result.Right, FContentMargin.Right);
    Dec(Result.Bottom, FContentMargin.Bottom);
  end;
end;

procedure TKMemoBlockStyle.NotifyChange(AValue: TKMemoBlockStyle);
begin
  if not FStyleChanged and UpdateUnlocked then
    AssignNC(Avalue);
end;

procedure TKMemoBlockStyle.PaintBox(ACanvas: TCanvas; const ARect: TRect);
var
  R, RB: TRect;
begin
  if (FBrush.Style <> bsClear) or (FBorderWidth > 0) or FBorderWidths.NonZero then with ACanvas do
  begin
    if FBorderWidths.NonZero or (FBorderWidth > 0) and (FBorderRadius = 0) then
    begin
      R := ARect;
      Pen.Style := psClear;
      Brush.Color := FBorderColor;
      Brush.Style := bsSolid;
      if LeftBorderWidth <> 0 then
      begin
        RB := ARect;
        RB.Right := RB.Left + LeftBorderWidth;
        R.Left := RB.Right;
        FillRect(RB);
      end;
      if TopBorderWidth <> 0 then
      begin
        RB := ARect;
        RB.Bottom := RB.Top + TopBorderWidth;
        R.Top := RB.Bottom;
        FillRect(RB);
      end;
      if RightBorderWidth <> 0 then
      begin
        RB := ARect;
        RB.Left := RB.Right - RightBorderWidth;
        R.Right := RB.Left;
        FillRect(RB);
      end;
      if BottomBorderWidth <> 0 then
      begin
        RB := ARect;
        RB.Top := RB.Bottom - BottomBorderWidth;
        R.Bottom := RB.Top;
        FillRect(RB);
      end;
      Brush.Assign(FBrush);
      // keep this here, some printers draw incorrectly for bsClear style
      if Brush.Style <> bsClear then
        FillRect(R);
    end else
    begin
      Brush.Assign(FBrush);
      Pen.Style := psSolid;
      Pen.Width := FBorderWidth;
      Pen.Color := FBorderColor;
      if FBorderRadius > 0 then
        RoundRectangle(ACanvas, ARect, FBorderRadius, FBorderRadius)
      else if FBorderWidth > 0 then
        Rectangle(ARect)
      else if Brush.Style <> bsClear then
        FillRect(ARect);
    end;
  end;
end;

procedure TKMemoBlockStyle.PropsChanged(AReasons: TKMemoUpdateReasons);
begin
  FUpdateReasons := AReasons;
  if Changeable then
    FStyleChanged := True;
  Changed;
end;

procedure TKMemoBlockStyle.SetBorderColor(const Value: TColor);
begin
  if Value <> FBorderColor then
  begin
    FBorderColor := Value;
    PropsChanged([muExtent]);
  end;
end;

procedure TKMemoBlockStyle.SetBorderRadius(const Value: Integer);
begin
  if Value <> FBorderRadius then
  begin
    FBorderRadius := Value;
    PropsChanged([muExtent]);
  end;
end;

procedure TKMemoBlockStyle.SetBorderWidth(const Value: Integer);
begin
  if Value <> FBorderWidth then
  begin
    FBorderWidth := Value;
    PropsChanged([muExtent]);
  end;
end;

procedure TKMemoBlockStyle.SetBorderWidths(const Value: TKRect);
begin
  FBorderWidths.Assign(Value);
end;

procedure TKMemoBlockStyle.SetBottomMargin(const Value: Integer);
begin
  FContentMargin.Bottom := Value;
end;

procedure TKMemoBlockStyle.SetBottomPadding(const Value: Integer);
begin
  FContentPadding.Bottom := Value;
end;

procedure TKMemoBlockStyle.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TKMemoBlockStyle.SetWrapMode(const Value: TKMemoBlockWrapMode);
begin
  if Value <> FWrapMode then
  begin
    FWrapMode := Value;
    PropsChanged([muExtent]);
  end;
end;

procedure TKMemoBlockStyle.SetContentMargin(const Value: TKRect);
begin
  FContentMargin.Assign(Value);
end;

procedure TKMemoBlockStyle.SetContentPadding(const Value: TKRect);
begin
  FContentPadding.Assign(Value);
end;

procedure TKMemoBlockStyle.SetFillBlip(const Value: TGraphic);
var
  Cls: TGraphicClass;
begin
  FreeAndNil(FFillBlip);
  if Value <> nil then
  begin
    Cls := TGraphicClass(Value.ClassType);
    FFillBlip := Cls.Create;
    FFillBlip.Assign(Value);
  end;
  PropsChanged([muExtent]);
end;

procedure TKMemoBlockStyle.SetHAlign(const Value: TKHAlign);
begin
  if Value <> FHAlign then
  begin
    FHAlign := Value;
    PropsChanged([muExtent]);
  end;
end;

procedure TKMemoBlockStyle.SetLeftMargin(const Value: Integer);
begin
  FContentMargin.Left := Value;
end;

procedure TKMemoBlockStyle.SetLeftPadding(const Value: Integer);
begin
  FContentPadding.Left := Value;
end;

procedure TKMemoBlockStyle.SetRightMargin(const Value: Integer);
begin
  FContentMargin.Right := Value;
end;

procedure TKMemoBlockStyle.SetRightPadding(const Value: Integer);
begin
  FContentPadding.Right := Value;
end;

procedure TKMemoBlockStyle.SetTopMargin(const Value: Integer);
begin
  FContentMargin.Top := Value;
end;

procedure TKMemoBlockStyle.SetTopPadding(const Value: Integer);
begin
  FContentPadding.Top := Value;
end;

procedure TKMemoBlockStyle.Update;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self, FUpdateReasons);
end;

{ TKMemoParagraphStyle }

procedure TKMemoParaStyle.Defaults;
begin
  inherited;
  FFirstIndent := 0;
  FLineSpacingFactor := 1;
  FLineSpacingMode := lsmFactor;
  FLineSpacingValue := 0;
  FNumberingList := cInvalidListID;
  FNumberingListLevel := -1;
  FNumberStartAt := 0;
  FWordWrap := True;
end;

procedure TKMemoParaStyle.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TKMemoParaStyle then
  begin
    FirstIndent := TKMemoParaStyle(ASource).FirstIndent;
    LineSpacingFactor := TKMemoParaStyle(ASource).LineSpacingFactor;
    LineSpacingMode := TKMemoParaStyle(ASource).LineSpacingMode;
    LineSpacingValue := TKMemoParaStyle(ASource).LineSpacingValue;
    NumberingListLevel := TKMemoParaStyle(ASource).NumberingListLevel;
    NumberingList := TKMemoParaStyle(ASource).NumberingList;
    NumberStartAt := TKMemoParaStyle(ASource).NumberStartAt;
    WordWrap := TKMemoParaStyle(ASource).WordWrap;
  end;
end;

procedure TKMemoParaStyle.SetFirstIndent(const Value: Integer);
begin
  if Value <> FFirstIndent then
  begin
    FFirstIndent := Value;
    PropsChanged([muExtent]);
  end;
end;

procedure TKMemoParaStyle.SetLineSpacingValue(const Value: Integer);
begin
  if Value <> FLineSpacingValue then
  begin
    FLineSpacingValue := Value;
    PropsChanged([muExtent]);
  end;
end;

procedure TKMemoParaStyle.SetLineSpacingFactor(const Value: Double);
begin
  if Value <> FLineSpacingFactor then
  begin
    FLineSpacingFactor := Value;
    PropsChanged([muExtent]);
  end;
end;

procedure TKMemoParaStyle.SetLineSpacingMode(const Value: TKMemoLineSpacingMode);
begin
  if Value <> FLineSpacingMode then
  begin
    FLineSpacingMode := Value;
    PropsChanged([muExtent]);
  end;
end;

procedure TKMemoParaStyle.SetNumberStartAt(const Value: Integer);
begin
  if Value <> FNumberStartAt then
  begin
    FNumberStartAt := Value;
    PropsChanged([muContent]);
  end;
end;

procedure TKMemoParaStyle.SetNumberingList(const Value: Integer);
begin
  if Value <> FNumberingList then
  begin
    FNumberingList := Value;
    if FNumberingList >= 0 then
      FNumberingListLevel := Max(FNumberingListLevel, 0)
    else
      FNumberingListLevel := -1;
    PropsChanged([muContent]);
  end;
end;

procedure TKMemoParaStyle.SetNumberingListAndLevel(AListID, ALevelIndex: Integer);
begin
  FNumberingList := AListID;
  FNumberingListLevel := ALevelIndex;
  PropsChanged([muContent]);
end;

procedure TKMemoParaStyle.SetNumberingListLevel(const Value: Integer);
begin
  if Value <> FNumberingListLevel then
  begin
    FNumberingListLevel := Value;
    PropsChanged([muContent]);
  end;
end;

procedure TKMemoParaStyle.SetWordWrap(const Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    PropsChanged([muExtent]);
  end;
end;

{ TKMemoLine }

constructor TKMemoLine.Create;
begin
  FPosition := CreateEmptyPoint;
  FExtent := CreateEmptyPoint;
  FEndBlock := 0;
  FEndIndex := 0;
  FEndWord := 0;
  FStartBlock := 0;
  FStartIndex := 0;
  FStartWord := 0;
end;

function TKMemoLine.GetLineRect: TRect;
begin
  Result := Rect(FPosition.X, FPosition.Y, FPosition.X + FExtent.X, FPosition.Y + FExtent.Y);
end;

{ TKMemoLines }

function TKMemoLines.GetItem(Index: Integer): TKMemoLine;
begin
  Result := TKMemoLine(inherited GetItem(Index));
end;

procedure TKMemoLines.SetItem(Index: Integer; const Value: TKMemoLine);
begin
  inherited SetItem(Index, Value);
end;

{ TKWord }

procedure TKMemoWord.Clear;
begin
  FBaseLine := 0;
  FBottomPadding := 0;
  FClipped := False;
  FExtent := CreateEmptyPoint;
  FEndIndex := 0;
  FPosition := CreateEmptyPoint;
  FStartIndex := 0;
  FTopPadding := 0;
end;

constructor TKMemoWord.Create;
begin
  Clear;
end;

{ TKWordList }

function TKMemoWordList.GetItem(Index: Integer): TKMemoWord;
begin
  Result := TKMemoWord(inherited GetItem(Index));
end;

procedure TKMemoWordList.SetItem(Index: Integer; const Value: TKMemoWord);
begin
  inherited SetItem(Index, Value);
end;

{ TKMemoColors }

function TKMemoColors.GetColorSpec(Index: TKColorIndex): TKColorSpec;
begin
  case Index of
    ciBkGnd: begin Result.Def := cBkGndDef; Result.Name := ''; end;
    ciInactiveCaretBkGnd: begin Result.Def := cInactiveCaretBkGndDef; Result.Name := ''; end;
    ciInactiveCaretSelBkGnd: begin Result.Def := cInactiveCaretSelBkGndDef; Result.Name := ''; end;
    ciInactiveCaretSelText: begin Result.Def := cInactiveCaretSelTextDef; Result.Name := ''; end;
    ciInactiveCaretText: begin Result.Def := cInactiveCaretTextDef; Result.Name := ''; end;
    ciSelBkGnd: begin Result.Def := cSelBkGndDef; Result.Name := ''; end;
    ciSelBkGndFocused: begin Result.Def := cSelBkGndFocusedDef; Result.Name := ''; end;
    ciSelText: begin Result.Def := cSelTextDef; Result.Name := ''; end;
    ciSelTextFocused: begin Result.Def := cSelTextFocusedDef; Result.Name := ''; end;
  else
    Result := inherited GetColorSpec(Index);
  end;
end;

function TKMemoColors.InternalGetColor(Index: TKColorIndex): TColor;
begin
  case FColorScheme of
    csGrayed: if Index = ciBkGnd then Result := clWindow else Result := clGrayText;
    csBright:
    begin
      if FBrightColors[Index] = clNone then
        FBrightColors[Index] := BrightColor(FColors[Index], 0.5, bsOfTop);
      Result := FBrightColors[Index];
    end;
    csGrayScale: Result := ColorToGrayScale(FColors[Index]);
  else
    Result := FColors[Index];
  end;
end;

function TKMemoColors.GetMaxIndex: Integer;
begin
  Result := ciMemoColorsMax;
end;

{ TKMemoChangeList }

constructor TKMemoChangeList.Create(AEditor: TKCustomMemo;
  RedoList: TKMemoChangeList);
begin
  inherited Create;
  FEditor := AEditor;
  FGroupUseLock := 0;
  FLimit := cUndoLimitDef;
  FIndex := -1;
  FModifiedIndex := FIndex;
  FRedoList := RedoList;
  FOnChange := nil;
end;

procedure TKMemoChangeList.AddChange(ItemKind: TKMemoChangeKind; Inserted: Boolean);
var
  P: PKMemoChangeItem;
begin
  // don't allow succesive crCaretPos
  if (ItemKind = ckCaretPos) and not Inserted and (FIndex >= 0) and
    (PKMemoChangeItem(Items[FIndex]).ItemKind = ckCaretPos) then
    Exit;
  if FIndex < FLimit - 1 then
  begin
    if FIndex < Count - 1 then
      Inc(FIndex)
    else
      FIndex := Add(New(PKMemoChangeItem));
    P := Items[FIndex];
    if FGroupUseLock > 0 then
    begin
      P.Group := FGroup;
      P.GroupKind := FGroupKind;
    end else
    begin
      P.Group := 0;
      P.GroupKind := ItemKind;
    end;
    P.ItemKind := ItemKind;
    P.Position := FEditor.SelStart;
//    P.Data := Data;
    P.Inserted := Inserted;
    if FRedoList <> nil then
      FRedoList.Clear;
    if Assigned(FOnChange) then
      FOnChange(Self, ItemKind);
  end;
end;

procedure TKMemoChangeList.BeginGroup(GroupKind: TKMemoChangeKind);
begin
  if FGroupUseLock = 0 then
  begin
    FGroupKind := GroupKind;
    Inc(FGroup);
    if FGroup = 0 then Inc(FGroup);
  end;
  Inc(FGroupUseLock);
end;

function TKMemoChangeList.CanPeek: Boolean;
begin
  Result := FIndex >= 0;
end;

procedure TKMemoChangeList.Clear;
begin
  inherited;
  FGroupUseLock := 0;
  FIndex := -1;
  FModifiedIndex := FIndex;
end;

procedure TKMemoChangeList.EndGroup;
begin
  if FGroupUseLock > 0 then
    Dec(FGroupUseLock);
end;

function TKMemoChangeList.GetModified: Boolean;

  function CaretPosOnly: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := FModifiedIndex + 1 to FIndex do
    begin
      if PKMemoChangeItem(Items[I]).ItemKind <> ckCaretPos then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;

begin
  Result := (FIndex > FModifiedIndex) and not CaretPosOnly;
end;

procedure TKMemoChangeList.Notify(Ptr: Pointer; Action: TListNotification);
var
  P: PKMemoChangeItem;
begin
  case Action of
    lnDeleted:
      if Ptr <> nil then
      begin
        P := Ptr;
        Dispose(P);
      end;
  end;
end;

function TKMemoChangeList.PeekItem: PKMemoChangeItem;
begin
  if CanPeek then
  begin
    Result := Items[FIndex];
    Dec(FIndex);
  end else
    Result := nil;
end;

procedure TKMemoChangeList.PokeItem;
begin
  if FIndex < Count - 1 then
    Inc(FIndex);
end;

procedure TKMemoChangeList.SetGroupData(Group: Integer; GroupKind: TKMemoChangeKind);
begin
  FGroup := Group;
  FGroupKind := GroupKind;
  FGroupUseLock := 1;
end;

procedure TKMemoChangeList.SetLimit(Value: Integer);
begin
  if Value <> FLimit then
  begin
    FLimit := MinMax(Value, cUndoLimitMin, cUndoLimitMax);
    while Count > FLimit do
      Delete(0);
    FIndex := Min(FIndex, FLimit - 1);
  end;
end;

procedure TKMemoChangeList.SetModified(Value: Boolean);
begin
  if not Value then
    FModifiedIndex := FIndex;
end;

{ TKCustomMemo }

constructor TKCustomMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWindow;
  ControlStyle := [csOpaque, csClickEvents, csDoubleClicks, csCaptureMouse];
  DoubleBuffered := True; // is needed
  Font.OnChange := FontChange;
  Height := cHeight;
  ParentColor := False;
  ParentFont := False;
  TabStop := True;
  Width := cWidth;
  FBackground := TKMemoBackground.Create;
  FBackground.OnChanged := BackgroundChanged;
  FBlocks := TKMemoBlocks.Create;
  FBlocks.MemoNotifier := Self;
  FBlocks.OnUpdate := BlocksChanged;
  FActiveBlocks := FBlocks;
  FCaretRect := CreateEmptyRect;
  FColors := TKMemoColors.Create(Self);
  FContentPadding := TKRect.Create;
  FContentPadding.All := 5;
  FContentPadding.OnChanged := ContentPaddingChanged;
  FDisabledDrawStyle := cEditDisabledDrawStyleDef;
  FDragMode := sgpNone;
  FDragRect := CreateEmptyRect;
  FHorzScrollStep := cHorzScrollStepDef;
  FLeftPos := 0;
  FLinePosition := eolInside;
  FListTable := TKMemoListTable.Create;
  FListTable.OnChanged := ListChanged;
  FMaxWordLength := cMaxWordLengthDef;
  FMouseWheelAccumulator := 0;
  FNewTextStyle := TKMemoTextStyle.Create;
  FNewTextStyle.Changeable := False;
  FNewTextStyleValid := False;
  FOldCaretRect := CreateEmptyRect;
  FOptions := cKMemoOptionsDef;
  FPreferredCaretPos := 0;
  FKeyMapping := TKEditKeyMapping.Create;
  FParaStyle := TKMemoParaStyle.Create;
  FParaStyle.Changeable := False;
  FParaStyle.OnChanged := ParaStyleChanged;
  FRedoList := TKMemoChangeList.Create(Self, nil);
  FRequiredContentWidth := 0;
  FRequiredMouseCursor := crIBeam;
  FScrollBars := ssBoth;
  FScrollPadding := cScrollPaddingDef;
  FScrollSpeed := cScrollSpeedDef;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := FScrollSpeed;
  FScrollTimer.OnTimer := ScrollTimerHandler;
  FSelectedBlock := nil;
  FStates := [];
  FTextStyle := TKMemoTextStyle.Create;
  FTextStyle.Changeable := False;
  FTextStyle.Font.Assign(Font);
  FTextStyle.UnlockUpdate;
  FTextStyle.OnChanged := TextStyleChanged;
  FTopPos := 0;
  FUndoList := TKMemoChangeList.Create(Self, FRedoList);
  FUndoList.OnChange := UndoChange;
  FUpdateLock := 0;
  FVertScrollStep := cVertScrollStepDef;
  FWordBreaks := cDefaultWordBreaks;
  FOnChange := nil;
  FOnDropFiles := nil;
  FOnEditBlock := nil;
  FOnReplaceText := nil;
  if csDesigning in ComponentState then
    Text := 'This is beta state control.'+cEOL+'You may already use it in your programs'+cEOL+'but some important functions may still be missing.'+cEOL
  else
    Text := cEOL;
  UpdateEditorCaret;
end;

destructor TKCustomMemo.Destroy;
begin
  Clear;
  FOnChange := nil;
  FUndoList.Free;
  FRedoList.Free;
  FParaStyle.Free;
  FNewTextStyle.Free;
  FKeyMapping.Free;
  FTextStyle.Free;
  FListTable.Free;
  FContentPadding.Free;
  FColors.Free;
  FBlocks.Free;
  FBackground.Free;
  inherited;
end;

procedure TKCustomMemo.AddUndoCaretPos(Force: Boolean);
begin
  FUndoList.AddChange(ckCaretPos, Force);
end;

procedure TKCustomMemo.AddUndoChar(AItemKind: TKMemoChangeKind; AData: TKChar; AInserted: Boolean = True);
begin
  FUndoList.AddChange(AItemKind, AInserted);
end;

procedure TKCustomMemo.AddUndoString(AItemKind: TKMemoChangeKind; const AData: TKString; AInserted: Boolean = True);
begin
  if AData <> '' then
    FUndoList.AddChange(AItemKind, AInserted);
end;

procedure TKCustomMemo.Assign(Source: TPersistent);
begin
  if Source is TKCustomMemo then with Source as TKCustomMemo do
  begin
    Self.LockUpdate;
    try
      Self.Align := Align;
      Self.Anchors := Anchors;
      Self.AutoSize := AutoSize;
      Self.BiDiMode := BiDiMode;
      Self.BorderStyle := BorderStyle;
      Self.BorderWidth := BorderWidth;
      Self.Color := Color;
      Self.Colors := Colors;
      Self.Constraints.Assign(Constraints);
    {$IFNDEF FPC}
      Self.Ctl3D := Ctl3D;
    {$ENDIF}
      Self.DisabledDrawStyle := DisabledDrawStyle;
      Self.DragCursor := DragCursor;
      Self.DragKind := DragKind;
      Self.DragMode := DragMode;
      Self.Enabled := Enabled;
      Self.Font := Font;
    {$IFNDEF FPC}
      Self.ImeMode := ImeMode;
      Self.ImeName := ImeName;
    {$ENDIF}
      Self.KeyMapping.Assign(KeyMapping);
      Self.Modified := False;
      Self.Options := Options;
      Self.ParaStyle.AssignNC(ParaStyle);
      Self.ParentBiDiMode := ParentBiDiMode;
      Self.ParentColor := ParentColor;
    {$IFNDEF FPC}
      Self.ParentCtl3D := ParentCtl3D;
    {$ENDIF}
      Self.ParentFont := ParentFont;
      Self.ParentShowHint := ParentShowHint;
      Self.PopupMenu := PopupMenu;
      Self.ScrollBars := ScrollBars;
      Self.SelEnd := SelEnd;
      Self.SelStart := SelStart;
      Self.ShowHint := ShowHint;
      Self.TabOrder := TabOrder;
      Self.TabStop := TabStop;
      Self.TextStyle.AssignNC(TextStyle);
      Self.Visible := Visible;
    finally
      Self.UnlockUpdate;
    end;
  end
  else
    inherited;
end;

procedure TKCustomMemo.BackgroundChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TKCustomMemo.BeginUndoGroup(AGroupKind: TKMemoChangeKind);
begin
  FUndoList.BeginGroup(AGroupKind);
end;

function TKCustomMemo.BlockRectToRect(const ARect: TRect): TRect;
begin
  Result := ARect;
  KFunctions.OffsetRect(Result, ContentLeft, ContentTop);
  if ActiveBlocks <> FBlocks then
  begin
    KFunctions.OffsetRect(Result, ActiveBlocks.TotalLeftOffset, ActiveBlocks.TotalTopOffset);
  end;
end;

procedure TKCustomMemo.BlockFreeNotification(ABlock: TKMemoBlock);
begin
  if ABlock = FSelectedBlock then
  begin
    CancelDrag;
    FSelectedBlock := nil;
  end;
end;

procedure TKCustomMemo.BlocksFreeNotification(ABlocks: TKMemoBlocks);
begin
  if ABlocks = ActiveBlocks then
    ActiveBlocks := FBlocks;
end;

procedure TKCustomMemo.BlocksChanged(Reasons: TKMemoUpdateReasons);
begin
  if HandleAllocated and UpdateUnlocked then
  begin
    if Reasons * [muContent, muExtent] <> [] then
      UpdateScrollRange(True)
    else if muSelectionScroll in Reasons then
    begin
      if not ClampInView(nil, False) then
      begin
        UpdateEditorCaret;
        Invalidate;
      end;
    end else
    begin
      UpdateEditorCaret;
      Invalidate;
    end;
    if muContent in Reasons then
      DoChange;
  end;
end;

procedure TKCustomMemo.CancelDrag;
begin
  if elMouseDrag in FStates then
  begin
    Exclude(FStates, elMouseDrag);
    Invalidate;
  end;
end;

function TKCustomMemo.CanScroll(ACommand: TKEditCommand): Boolean;
var
  R: TRect;
begin
  case ACommand of
    ecScrollUp:  Result := FTopPos > 0;
    ecScrollDown: Result := FTopPos < FVertScrollExtent - 1;
    ecScrollLeft: Result := FLeftPos > 0;
    ecScrollRight: Result := FLeftPos < FHorzScrollExtent - 1;
    ecScrollCenter:
    begin
      R := IndexToRect(SelEnd, True);
      R.Left := R.Left - ClientWidth div 2;
      R.Top := R.Top - ClientHeight div 2;
      Result :=
        (FLeftPos > 0) and (R.Left < 0) or
        (FLeftPos < FHorzScrollExtent - 1) and (R.Left > 0) or
        (FTopPos > 0) and (R.Top < 0) or
        (FTopPos < FVertScrollExtent - 1) and (R.Top > 0);
    end;
  else
    Result := False;
  end;
end;

function TKCustomMemo.CaretInView: Boolean;
begin
  Result := PtInRect(ClientRect, FCaretRect.TopLeft);
end;

function TKCustomMemo.ClampInView(AMousePos: PPoint; ACallScrollWindow: Boolean): Boolean;
var
  DeltaHorz, DeltaVert: Integer;
begin
  UpdateEditorCaret(False);
  Result := ScrollNeeded(AMousePos, DeltaHorz, DeltaVert);
  if Result then
  begin
    Result := Scroll(cScrollNoAction, cScrollNoAction, DeltaHorz, DeltaVert, ACallScrollWindow);
    if Result then
      FScrollTimer.Enabled := True;
  end;
end;

procedure TKCustomMemo.Clear(AKeepOnePara: Boolean);
begin
  FBlocks.LockUpdate;
  try
    FBlocks.Clear;
    if AKeepOnePara then
      FBlocks.FixEmptyBlocks;
    FTextStyle.Defaults;
    FTextStyle.Font.Assign(Font);
    FParaStyle.Defaults;
    {
    FColors.BkGnd := cBkGndDef;
    FBackground.Clear;
    }
    FListTable.Clear;
  finally
    FBlocks.UnlockUpdate;
  end;
end;

procedure TKCustomMemo.ClearSelection(ATextOnly: Boolean);
var
  Len: Integer;
begin
  Len := ActiveBlocks.SelLength;
  ActiveBlocks.ClearSelection(ATextOnly);
  if Len <> 0 then
    Modified := True;
end;

procedure TKCustomMemo.ClearUndo;
begin
  FUndoList.Clear;
  FRedoList.Clear;
end;

procedure TKCustomMemo.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited;
  UpdateEditorCaret;
  Invalidate;
end;

procedure TKCustomMemo.CMSysColorChange(var Msg: TLMessage);
begin
  inherited;
  FColors.ClearBrightColors;
end;

function TKCustomMemo.CommandEnabled(Command: TKEditCommand): Boolean;
var
  TmpSelEnd, TmpSelLength: Integer;
  TmpLinePos: TKMemoLinePosition;
begin
  if Enabled and Visible and not (csDesigning in ComponentState) then
  begin
    TmpSelEnd := SelEnd;
    TmpSelLength := RealSelLength;
    case Command of
      // movement commands
      ecLeft, ecSelLeft:
        Result := TmpSelEnd > 0;
      ecRight:
        Result := TmpSelEnd < ActiveBlocks.SelectableLength;
      ecSelRight:
        Result := TmpSelEnd < ActiveBlocks.SelectableLength;
      ecUp, ecSelUp, ecPageUp, ecSelPageUp:
        Result := ActiveBlocks.IndexBelowFirstLine(TmpSelEnd, True);
      ecDown, ecPagedown, ecSelDown, ecSelPageDown:
        Result := ActiveBlocks.IndexAboveLastLine(TmpSelEnd, True);
      ecLineStart, ecPageLeft, ecSelLineStart, ecSelPageLeft:
        Result := TmpSelEnd > ActiveBlocks.LineStartIndexByIndex(TmpSelEnd, True, TmpLinePos);
      ecLineEnd, ecPageRight:
        Result := TmpSelEnd < ActiveBlocks.LineEndIndexByIndex(TmpSelEnd, True, False, TmpLinePos);
      ecSelLineEnd, ecSelPageRight:
        Result := TmpSelEnd < ActiveBlocks.LineEndIndexByIndex(TmpSelEnd, True, True, TmpLinePos);
      ecPageTop, ecSelPageTop:
        Result := TmpSelEnd <> ActiveBlocks.NextIndexByVertValue(Canvas, -ContentTop + VertScrollPadding, FPreferredCaretPos, False, TmpLinePos);
      ecPageBottom, ecSelPageBottom:
        Result := TmpSelEnd <> ActiveBlocks.NextIndexByVertValue(Canvas, -ContentTop + VertScrollPadding + ClientHeight, FPreferredCaretPos, True, TmpLinePos);
      ecEditorTop, ecSelEditorTop:
        Result := TmpSelEnd > 0;
      ecEditorBottom, ecSelEditorBottom:
        Result := TmpSelEnd < ActiveBlocks.SelectableLength;
      ecGotoXY, ecSelGotoXY:
        Result := True;
      // scroll commands
      ecScrollUp, ecScrollDown, ecScrollLeft, ecScrollRight, ecScrollCenter:
        Result := CanScroll(Command);
      // editing commands
      ecUndo:
        Result := not ReadOnly and FUndoList.CanPeek;
      ecRedo:
        Result := not ReadOnly and FRedoList.CanPeek;
      ecCopy, ecCut:
        Result := not Empty and (not ReadOnly or (Command = ecCopy)) and ((TmpSelLength > 0) or RelativeSelected);
      ecPaste:
        Result := not ReadOnly and (ClipBoard.FormatCount > 0);
      ecInsertChar, ecInsertString, ecInsertNewLine:
        Result := not (ReadOnly or RelativeSelected);
      ecDeleteLastChar:
        Result := not (Empty or ReadOnly) and ((TmpSelLength > 0) or (TmpSelEnd > 0));
      ecDeleteChar:
        Result := not (Empty or ReadOnly) and ((TmpSelLength > 0) or (TmpSelEnd < ActiveBlocks.SelectableLength - 1));
      ecDeleteBOL:
        Result := not (Empty or ReadOnly or RelativeSelected) and ((TmpSelLength > 0) or (TmpSelEnd <> ActiveBlocks.LineStartIndexByIndex(TmpSelEnd, True, TmpLinePos)));
      ecDeleteEOL:
        Result := not (Empty or ReadOnly or RelativeSelected) and ((TmpSelLength > 0) or (TmpSelEnd <> ActiveBlocks.LineEndIndexByIndex(TmpSelEnd, True, True, TmpLinePos)));
      ecDeleteLine, ecClearAll, ecReplace:
        Result := not (Empty or ReadOnly or RelativeSelected);
      ecClearSelection:
        Result := not (Empty or ReadOnly or RelativeSelected) and (TmpSelLength > 0);
      ecSearch:
        Result := not Empty;
      ecSelectAll:
        Result := not Empty and (SelLength < SelectableLength);
      ecInsertMode:
        Result := elOverwrite in FStates;
      ecOverwriteMode:
        Result := not (elOverwrite in FStates);
      ecToggleMode:
        Result := not ReadOnly;
      ecGotFocus, ecLostFocus:
        Result := True;
    else
      Result := False;
    end;
  end else
    Result := False;
end;

procedure TKCustomMemo.ContentPaddingChanged(Sender: TObject);
begin
  BlocksChanged([muExtent]);
end;

procedure TKCustomMemo.CreateHandle;
begin
  inherited;
  UpdateScrollRange(True);
end;

procedure TKCustomMemo.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    if FScrollBars in [ssVertical, ssBoth] then Style := Style or WS_VSCROLL;
    if FScrollBars in [ssHorizontal, ssBoth] then Style := Style or WS_HSCROLL;
  end;
end;

procedure TKCustomMemo.CreateWnd;
begin
  inherited;
{$IFDEF USE_WINAPI}
  if (eoDropFiles in FOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, TRUE);
{$ENDIF}
end;

procedure TKCustomMemo.DeleteBOL(At: Integer);
begin
  ActiveBlocks.DeleteBOL(At);
  Modified := True;
end;

procedure TKCustomMemo.DeleteChar(At: Integer);
begin
  if RelativeSelected then
    DeleteSelectedBlock
  else
    ActiveBlocks.DeleteChar(At);
  Modified := True;
end;

procedure TKCustomMemo.DeleteEOL(At: Integer);
begin
  ActiveBlocks.DeleteEOL(At);
  Modified := True;
end;

procedure TKCustomMemo.DeleteLastChar(At: Integer);
begin
  if RelativeSelected then
    DeleteSelectedBlock
  else
    ActiveBlocks.DeleteLastChar(At);
  Modified := True;
end;

procedure TKCustomMemo.DeleteLine(At: Integer);
begin
  ActiveBlocks.DeleteLine(At);
  Modified := True;
end;

procedure TKCustomMemo.DeleteSelectedBlock;
var
  Blocks: TKMemoBlocks;
begin
  if FSelectedBlock <> nil then
  begin
    Blocks := FSelectedBlock.ParentBlocks;
    if Blocks <> nil then
    begin
      Blocks.Remove(FSelectedBlock);
      FSelectedBlock := nil;
    end;
  end;
end;

procedure TKCustomMemo.DestroyWnd;
begin
{$IFDEF USE_WINAPI}
  if (eoDropFiles in FOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, FALSE);
{$ENDIF}
  inherited;
end;

procedure TKCustomMemo.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TKCustomMemo.DoCopy: Boolean;
var
  Stream: TMemoryStream;
  S: TKString;
  TmpItems: TKMemoBlocks;
  TmpSelect: Boolean;
begin
  // temporary select entire FSelectedBlock (floating TKMemoContainer)
  TmpItems := ActiveBlocks;
  TmpSelect := (FSelectedBlock <> nil) and (FSelectedBlock.SelLength = 0);
  if TmpSelect then
  begin
    FSelectedBlock.Select(0, FSelectedBlock.SelectableLength(True), False);
    FActiveBlocks := FBlocks;
  end;
  // copy selected blocks as plain text and RTF to clipboard
  S := ActiveBlocks.SelText;
  Stream := TMemoryStream.Create;
  try
    SaveToRTFStream(Stream, True);
    Stream.SaveToFile('copied.rtf'); //debug line
    Result := ClipBoardSaveStreamAs(cRichText, Stream, S);
  finally
    Stream.Free;
    // clear selection
    if TmpSelect then
    begin
      FSelectedBlock.Select(-1, 0, False);
      FActiveBlocks := TmpItems;
    end;
  end;
end;

function TKCustomMemo.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
const
  WHEEL_DIVISOR = 120;
var
  LinesToScroll, WheelClicks: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    if ssCtrl in Shift then
      LinesToScroll := ClientHeight div FVertScrollStep
    else
      LinesToScroll := 3;
    Inc(FMouseWheelAccumulator, WheelDelta);
    WheelClicks := FMouseWheelAccumulator div WHEEL_DIVISOR;
    FMouseWheelAccumulator := FMouseWheelAccumulator mod WHEEL_DIVISOR;
    ScrollBy(0, - WheelClicks * LinesToScroll, eoScrollWindow in FOptions);
    Result := True;
  end;
end;

function TKCustomMemo.DoPaste: Boolean;
var
  Stream: TMemoryStream;
  S: TKString;
  OldSelectableLength, NewSelectableLength: Integer;
begin
  // load blocks from clipboard either as RTF or plain text
  Stream := TMemoryStream.Create;
  try
    Result := ClipBoardLoadStreamAs(cRichText, Stream, S);
    if Result then
    begin
      ExecuteCommand(ecClearSelection);
      OldSelectableLength := ActiveBlocks.SelectableLength;
      if Stream.Size > 0 then
      begin
        Stream.Seek(0, soFromBeginning);
        //Stream.SaveToFile('pasted.rtf'); //debug line
        LoadFromRTFStream(Stream, SelEnd);
      end else
        ActiveBlocks.InsertPlainText(SelEnd, S);
      NewSelectableLength := ActiveBlocks.SelectableLength;
      if NewSelectableLength > OldSelectableLength then
        Select(SelEnd + NewSelectableLength - OldSelectableLength, 0);
      Modified := True;
    end;
  finally
    Stream.Free;
  end;
end;

function TKCustomMemo.DoRedo: Boolean;
begin
  //TODO
  Result := False;
end;

function TKCustomMemo.DoSearchReplace(AReplace: Boolean): Boolean;
begin
  // TODO
  Result := False;
end;

function TKCustomMemo.DoUndo: Boolean;
begin
  //TODO
  Result := False;
end;

procedure TKCustomMemo.DragBlock;
var
  P: TPoint;
  DX, DY: Integer;
begin
  if elMouseDrag in FStates then
  begin
    P := ScreenToClient(Mouse.CursorPos);
    DX := P.X - FDragCurPos.X;
    DY := P.Y - FDragCurPos.Y;
    TKSizingGrips.ClsAffectRect(FDragMode, DX, DY, FDragRect);
    FDragCurPos := P;
    Invalidate;
  end;
end;

function TKCustomMemo.EditBlock(AItem: TKMemoBlock): Boolean;
begin
  Result := False;
  if Assigned(FOnEditBlock) and not ReadOnly then
    FOnEditBlock(AItem, Result);
end;

{$IFNDEF FPC}
procedure TKCustomMemo.EMGetSel(var Msg: TLMessage);
begin
  PInteger(Msg.WParam)^ := SelStart;
  PInteger(Msg.LParam)^ := SelEnd;
  Msg.Result := 1;
end;

procedure TKCustomMemo.EMSetSel(var Msg: TLMessage);
begin
  Select(Msg.WParam, Msg.LParam);
  Msg.Result := 1;
end;
{$ENDIF}

procedure TKCustomMemo.EndUndoGroup;
begin
  FUndoList.EndGroup;
end;

function TKCustomMemo.ExecuteCommand(Command: TKEditCommand; Data: Pointer): Boolean;
var
  TmpSelEnd, NewSelEnd: Integer;
  TmpPosition: TKMemoLinePosition;
begin                     
  Result := False;
  if CommandEnabled(Command) then
  begin
    Result := True;
    TmpSelEnd := SelEnd;
    TmpPosition := FLinePosition;
    case Command of
      // selection commands
      ecLeft:
      begin
        NewSelEnd := ActiveBlocks.NextIndexByCharCount(TmpSelEnd, -1);
        SelectionInit(NewSelEnd, True, eolInside);
      end;
      ecSelLeft:
      begin
        NewSelEnd := ActiveBlocks.NextIndexByCharCount(TmpSelEnd, -1);
        SelectionExpand(NewSelEnd, True, eolInside);
      end;
      ecRight:
      begin
        NewSelEnd := ActiveBlocks.NextIndexByCharCount(TmpSelEnd, 1);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelRight:
      begin
        NewSelEnd := ActiveBlocks.NextIndexByCharCount(TmpSelEnd, 1);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecUp: 
      begin
        NewSelEnd := ActiveBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, -1, FPreferredCaretPos, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelUp: 
      begin
        NewSelEnd := ActiveBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, -1, FPreferredCaretPos, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecDown: 
      begin
        NewSelEnd := ActiveBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, 1, FPreferredCaretPos, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelDown: 
      begin
        NewSelEnd := ActiveBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, 1, FPreferredCaretPos, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecLineStart: 
      begin
        NewSelEnd := ActiveBlocks.LineStartIndexByIndex(TmpSelEnd, True, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelLineStart: 
      begin
        NewSelEnd := ActiveBlocks.LineStartIndexByIndex(TmpSelEnd, True, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecLineEnd: 
      begin
        NewSelEnd := ActiveBlocks.LineEndIndexByIndex(TmpSelEnd, True, False, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelLineEnd: 
      begin
        NewSelEnd := ActiveBlocks.LineEndIndexByIndex(TmpSelEnd, True, True, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageUp: 
      begin
        NewSelEnd := ActiveBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, -ClientHeight, FPreferredCaretPos, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageUp: 
      begin
        NewSelEnd := ActiveBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, -ClientHeight, FPreferredCaretPos, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageDown:
      begin
        NewSelEnd := ActiveBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, ClientHeight, FPreferredCaretPos, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageDown: 
      begin
        NewSelEnd := ActiveBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, ClientHeight, FPreferredCaretPos, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageLeft:
      begin
        NewSelEnd := ActiveBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, -ClientWidth, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageLeft: 
      begin
        NewSelEnd := ActiveBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, -ClientWidth, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageRight: 
      begin
        NewSelEnd := ActiveBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, ClientWidth, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageRight:
      begin
        NewSelEnd := ActiveBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, ClientWidth, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageTop: 
      begin
        NewSelEnd := ActiveBlocks.NextIndexByVertValue(Canvas, -ContentTop + VertScrollPadding, FPreferredCaretPos, False, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageTop: 
      begin
        NewSelEnd := ActiveBlocks.NextIndexByVertValue(Canvas, FTopPos * FVertScrollStep + VertScrollPadding, FPreferredCaretPos, False, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageBottom: 
      begin
        NewSelEnd := ActiveBlocks.NextIndexByVertValue(Canvas, -ContentTop + ClientHeight - VertScrollPadding, FPreferredCaretPos, True, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageBottom: 
      begin
        NewSelEnd := ActiveBlocks.NextIndexByVertValue(Canvas, FTopPos * FVertScrollStep + ClientHeight - VertScrollPadding, FPreferredCaretPos, True, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecEditorTop: SelectionInit(0, True, eolInside);
      ecSelEditorTop: SelectionExpand(0, True, eolInside);
      ecEditorBottom: SelectionInit(ActiveBlocks.SelectableLength, True, eolEnd);
      ecSelEditorBottom: SelectionExpand(ActiveBlocks.SelectableLength, True, eolEnd);
      ecGotoXY: 
      begin
        NewSelEnd := PointToIndex(PPoint(Data)^, True, False, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelGotoXY: 
      begin
        NewSelEnd := PointToIndex(PPoint(Data)^, True, True, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      // scroll commands
      ecScrollUp:
      begin
        ScrollBy(0, -1, eoScrollWindow in FOptions);
        while CommandEnabled(ecUp) and (FCaretRect.Top + FCaretRect.Bottom > ClientHeight - VertScrollPadding) do
          ExecuteCommand(ecUp);
      end;
      ecScrollDown:
      begin
        ScrollBy(0, 1, eoScrollWindow in FOptions);
        while CommandEnabled(ecDown) and (FCaretRect.Top < VertScrollPadding) do
          ExecuteCommand(ecDown);
      end;
      ecScrollLeft:
      begin
        ScrollBy(-1, 0, eoScrollWindow in FOptions);
        while CommandEnabled(ecLeft) and (FCaretRect.Left + FCaretRect.Right > ClientWidth - HorzScrollPadding) do
          ExecuteCommand(ecLeft);
      end;
      ecScrollRight:
      begin
        ScrollBy(1, 0, eoScrollWindow in FOptions);
        while CommandEnabled(ecRight) and (FCaretRect.Left < HorzScrollPadding) do
          ExecuteCommand(ecRight);
      end;
      ecScrollCenter: ScrollToClientAreaCenter;
      // editing commands
      ecUndo: Result := DoUndo;
      ecRedo: Result := DoRedo;
      ecCopy: Result := DoCopy;
      ecCut:
      begin
        if DoCopy then
          ClearSelection;
      end;
      ecPaste: DoPaste;
      ecInsertChar: InsertChar(TmpSelEnd, PKChar(Data)^);
      ecInsertString: InsertString(TmpSelEnd, TKString(Data));
      ecInsertNewLine: InsertNewLine(TmpSelEnd);
      ecDeleteLastChar: DeleteLastChar(TmpSelEnd);
      ecDeleteChar: DeleteChar(TmpSelEnd);
      ecDeleteBOL: DeleteBOL(TmpSelEnd);
      ecDeleteEOL: DeleteEOL(TmpSelEnd);
      ecDeleteLine: DeleteLine(TmpSelEnd);
      ecSelectAll: Select(0, ActiveBlocks.SelectableLength);
      ecClearAll:
      begin
        Select(0, ActiveBlocks.SelectableLength);
        ClearSelection;
      end;
      ecClearSelection: ClearSelection;
      ecSearch, ecReplace: Result := DoSearchReplace(Command = ecReplace);
      ecInsertMode:
      begin
        Exclude(FStates, elOverwrite);
        UpdateEditorCaret;
      end;
      ecOverwriteMode:
      begin
        Include(FStates, elOverwrite);
        UpdateEditorCaret;
      end;
      ecToggleMode:
      begin
        if elOverwrite in FStates then
          Exclude(FStates, elOverwrite)
        else
          Include(FStates, elOverwrite);
        UpdateEditorCaret;
      end;
      // focus change
      ecGotFocus:
      begin
        UpdateEditorCaret;
        Invalidate;
      end;
      ecLostFocus:
      begin
        UpdateEditorCaret;
        Invalidate;
      end;
    end;
    case Command of
      ecLeft, ecRight, ecLineStart, ecLineEnd,  ecPageLeft, ecPageRight, ecGotoXY,
      ecSelLeft, ecSelRight, ecSelLineStart, ecSelLineEnd, ecSelPageLeft, ecSelPageRight, ecSelGotoXY,
      ecEditorTop, ecSelEditorTop, ecEditorBottom, ecSelEditorBottom,
      ecInsertChar, ecInsertString, ecInsertNewLine, ecDeleteLastChar, ecDeleteChar, ecDeleteBOL, ecDeleteEOL,
      ecDeleteLine, ecSelectAll, ecClearAll, ecClearSelection:
        UpdatePreferredCaretPos;
    end;
  end;
end;

procedure TKCustomMemo.FontChange(Sender: TObject);
begin
  FTextStyle.Font.Assign(Font);
end;

function TKCustomMemo.GetActiveBlock: TKMemoBlock;
var
  Dummy, Index: Integer;
begin
  Index := ActiveBlocks.SelEnd;
  if SelAvail and (ActiveBlocks.SelEnd > ActiveBlocks.SelStart) then
    Dec(Index);
  Result := ActiveBlocks.IndexToItem(Index, Dummy);
end;

function TKCustomMemo.GetActiveBlocks: TKMemoBlocks;
begin
  Result := ActiveBlocks;
end;

function TKCustomMemo.GetActiveInnerBlock: TKMemoBlock;
var
  Index, LocalIndex: Integer;
  Items: TKmemoBlocks;
begin
  Index := ActiveBlocks.SelEnd;
  if SelAvail and (ActiveBlocks.SelEnd > ActiveBlocks.SelStart) then
    Dec(Index);
  Items := ActiveBlocks.IndexToItems(Index, LocalIndex);
  if Items <> nil then
    Result := Items.IndexToItem(LocalIndex, LocalIndex)
  else
    Result := nil;
end;

function TKCustomMemo.GetActiveInnerBlocks: TKMemoBlocks;
var
  Dummy: Integer;
begin
  Result := ActiveBlocks.IndexToItems(ActiveBlocks.RealSelEnd, Dummy);
end;

function TKCustomMemo.GetCaretVisible: Boolean;
begin
  Result := elCaretVisible in FStates;
end;

function TKCustomMemo.GetContentHeight: Integer;
begin
  Result := FVertExtent * FVertScrollStep;
end;

function TKCustomMemo.GetContentLeft: Integer;
begin
  Result := -FLeftPos * FHorzScrollStep + FContentPadding.Left;
end;

function TKCustomMemo.GetContentRect: TRect;
begin
  Result.Left := ContentLeft;
  Result.Top := ContentTop;
  Result.Right := Result.Left + ContentWidth;
  Result.Bottom := Result.Top + ContentHeight;
end;

function TKCustomMemo.GetContentTop: Integer;
begin
  Result := -FTopPos * FVertScrollStep + FContentPadding.Top;
end;

function TKCustomMemo.GetContentWidth: Integer;
begin
  Result := FHorzExtent * FHorzScrollStep;
end;

function TKCustomMemo.GetDefaultTextStyle: TKMemoTextStyle;
begin
  Result := FTextStyle;
end;

function TKCustomMemo.GetDrawSingleChars: Boolean;
begin
  Result := (eoDrawSingleChars in FOptions) or (elPrinting in FStates);
end;

function TKCustomMemo.GetDefaultParaStyle: TKMemoParaStyle;
begin
  Result := FParaStyle;
end;

function TKCustomMemo.GetEmpty: Boolean;
begin
  Result := FBlocks.Empty;
end;

function TKCustomMemo.GetHorzScrollPadding: Integer;
begin
  Result := Min(FScrollPadding, ClientWidth div 8);
end;

function TKCustomMemo.GetInsertMode: Boolean;
begin
  Result := not (elOverwrite in FStates);
end;

function TKCustomMemo.GetLinePosition: TKMemoLinePosition;
begin
  Result := FLinePosition;
end;

function TKCustomMemo.GetListTable: TKMemoListTable;
begin
  Result := FListTable;
end;

function TKCustomMemo.GetModified: Boolean;
begin
  Result := (elModified in FStates) or FUndoList.Modified;
end;

function TKCustomMemo.GetNearestParagraph: TKMemoParagraph;
begin
  Result := ActiveBlocks.BlockToItem(GetNearestParagraphIndex) as TKMemoParagraph;
end;

function TKCustomMemo.GetNearestParagraphIndex: Integer;
var
  Block, LocalIndex: Integer;
begin
  Block := ActiveBlocks.IndexToBlock(RealSelEnd, LocalIndex);
  if Block >= 0 then
    Result := ActiveBlocks.GetNearestParagraphBlock(Block)
  else
    Result := -1;
end;

function TKCustomMemo.GetNearestWordIndexes(AIndex: Integer; AIncludeWhiteSpaces: Boolean;
  out AStartIndex, AEndIndex: Integer): Boolean;
begin
  Result := ActiveBlocks.GetNearestWordIndexes(AIndex, True, AIncludeWhiteSpaces,
    AStartIndex, AEndIndex);
end;

function TKCustomMemo.GetPaintSelection: Boolean;
begin
  if elPrinting in FStates then
    Result := poPaintSelection in PageSetup.Options
  else
    Result := True;
end;

function TKCustomMemo.GetPrinting: Boolean;
begin
  Result := elPrinting in FStates;
end;

function TKCustomMemo.GetMaxLeftPos: Integer;
begin
  Result := (FHorzExtent - ClientWidth) div FHorzScrollStep;
end;

function TKCustomMemo.GetMaxTopPos: Integer;
begin
  Result := (FVertExtent - ClientHeight) div FVertScrollStep;
end;

function TKCustomMemo.GetMaxWordLength: Integer;
begin
  Result := FMaxWordLength;
end;

function TKCustomMemo.GetMemo: TKCustomMemo;
begin
  Result := Self;
end;

function TKCustomMemo.GetReadOnly: Boolean;
begin
  Result := elReadOnly in FStates;
end;

function TKCustomMemo.GetRealSelEnd: Integer;
begin
  Result := ActiveBlocks.RealSelEnd;
end;

function TKCustomMemo.GetRealSelLength: Integer;
begin
  Result := RealSelEnd - RealSelStart;
end;

function TKCustomMemo.GetRealSelStart: Integer;
begin
  Result := ActiveBlocks.RealSelStart;
end;

function TKCustomMemo.GetRelativeSelected: Boolean;
begin
  Result := (FSelectedBlock <> nil) and (FSelectedBlock.Position <> mbpText);
end;

function TKCustomMemo.GetRequiredContentWidth: Integer;
begin
  if FRequiredContentWidth > 0 then
    Result := FRequiredContentWidth
  else
    Result := ClientWidth - FContentPadding.Left - FContentPadding.Right;
end;

function TKCustomMemo.GetRTF: TKMemoRTFString;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create{$IFnDEF COMPILER12_UP}(''){$ENDIF};
  try
    ActiveBlocks := FBlocks;
    SaveToRTFStream(Stream, False);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

function TKCustomMemo.GetSelAvail: Boolean;
begin
  Result := SelLength <> 0;
end;

procedure TKCustomMemo.GetSelColors(out Foreground, Background: TColor);
begin
  if Focused then
  begin
    Foreground := FColors.SelTextFocused;
    Background := FColors.SelBkGndFocused;
  end else
  begin
    Foreground := FColors.SelText;
    Background := FColors.SelBkGnd;
  end;
end;

function TKCustomMemo.GetSelectableLength: Integer;
begin
  Result := ActiveBlocks.SelectableLength;
end;

function TKCustomMemo.GetSelectedBlock: TKMemoBlock;
begin
  Result := FSelectedBlock;
end;

function TKCustomMemo.GetSelectionHasPara: Boolean;
begin
  Result := ActiveBlocks.SelectionHasPara;
end;

function TKCustomMemo.GetSelectionParaStyle: TKMemoParaStyle;
begin
  Result := ActiveBlocks.SelectionParaStyle;
  if Result = nil then
    Result := FParaStyle;
end;

function TKCustomMemo.GetSelectionTextStyle: TKMemoTextStyle;
begin
  Result := ActiveBlocks.SelectionTextStyle;
  if Result = nil then
    Result := FTextStyle;
end;

function TKCustomMemo.GetSelEnd: Integer;
begin
  Result := ActiveBlocks.SelEnd;
end;

function TKCustomMemo.GetSelLength: Integer;
begin
  Result := ActiveBlocks.SelLength;
end;

function TKCustomMemo.GetSelStart: Integer;
begin
  Result := ActiveBlocks.SelStart
end;

function TKCustomMemo.GetSelText: TKString;
begin
  Result := ActiveBlocks.SelText;
end;

function TKCustomMemo.GetShowFormatting: Boolean;
begin
  if elPrinting in States then
    Result := False
  else
    Result := eoShowFormatting in FOptions;
end;

function TKCustomMemo.GetText: TKString;
begin
  Result := ActiveBlocks.Text;
end;

function TKCustomMemo.GetUndoLimit: Integer;
begin
  Result := FUndoList.Limit;
end;

function TKCustomMemo.GetVertScrollPadding: Integer;
begin
  Result := Min(FScrollPadding, ClientHeight div 8);
end;

function TKCustomMemo.GetWordBreaks: TKSysCharSet;
begin
  Result := FWordBreaks;
end;

procedure TKCustomMemo.HideEditorCaret;
begin
  if HandleAllocated then
    HideCaret(Handle);
end;

function TKCustomMemo.IndexToRect(AValue: Integer; ACaret: Boolean): TRect;
begin
  Result := BlockRectToRect(ActiveBlocks.IndexToRect(Canvas, AValue, ACaret, ActiveBlocks.EOLToNormal(AValue)));
end;

procedure TKCustomMemo.InsertChar(At: Integer; const AValue: TKChar);
var
  Style: TKMemoTextStyle;
begin
  if FNewTextStyleValid then
  begin
    Style := FNewTextStyle;
    FNewTextStyleValid := False;
  end else
    Style := nil;
  ActiveBlocks.InsertChar(At, AValue, elOverwrite in FStates, Style);
  Modified := True;
end;

procedure TKCustomMemo.InsertNewLine(At: Integer);
begin
  ActiveBlocks.InsertNewLine(At);
  Modified := True;
end;

procedure TKCustomMemo.InsertString(At: Integer; const AValue: TKString);
begin
  if AValue <> '' then
  begin
    BeginUndoGroup(ckInsert);
    try
      if ActiveBlocks.SelLength > 0 then
      begin
        ActiveBlocks.ClearSelection;
        At := ActiveBlocks.SelEnd;
      end;
      // always insert (don't overwrite)
      ActiveBlocks.InsertString(At, True, AValue);
    finally
      EndUndoGroup;
    end;
    Modified := True;
  end
end;

function TKCustomMemo.IsOptionsStored: Boolean;
begin
  Result := FOptions <> cKMemoOptionsDef;
end;

procedure TKCustomMemo.KeyDown(var Key: Word; Shift: TShiftState);
var
  Cmd: TKEditCommand;
begin
  inherited;
  Exclude(FStates, elIgnoreNextChar);
  if not (csDesigning in ComponentState) then
  begin
    Cmd := FKeyMapping.FindCommand(Key, Shift);
    if Cmd <> ecNone then
    begin
      ExecuteCommand(Cmd);
      Key := 0;
      Include(FStates, elIgnoreNextChar);
    end;
    case Key of
      VK_ESCAPE:
      begin
        Include(FStates, elIgnoreNextChar);
        CancelDrag;
      end;
      VK_SHIFT, VK_CONTROL, VK_MENU:
      begin
        UpdateMouseCursor;
      end;
    end;
  end;
end;

{$IFDEF FPC}
procedure TKCustomMemo.UTF8KeyPress(var Key: TUTF8Char);
{$ELSE}
procedure TKCustomMemo.KeyPress(var Key: Char);
{$ENDIF}
var
  C: TKCHar;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if not (elIgnoreNextChar in FStates) then
    begin
    {$IF DEFINED(FPC) OR DEFINED(COMPILER12_UP)}
      C := Key;
    {$ELSE}
      C := AnsiStringToString(Key)[1];
    {$IFEND}
      ExecuteCommand(ecInsertChar, @C);
    end else
      Exclude(FStates, elIgnoreNextChar);
  end;
end;

procedure TKCustomMemo.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    case Key of
      VK_SHIFT, VK_CONTROL, VK_MENU: UpdateMouseCursor;
    end;
  end;
end;

procedure TKCustomMemo.ListChanged(AList: TKMemoList; ALevel: TKMemoListLevel);
begin
  FBlocks.ListChanged(AList, ALevel);
  if FBlocks.UpdateUnlocked then
    Modified := True;
end;

procedure TKCustomMemo.LoadFromFile(const AFileName: TKString);
var
  Ext: TKString;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  if Ext = '.rtf' then
    LoadFromRTF(AFileName)
  else
    LoadFromTXT(AFileName);
end;

procedure TKCustomMemo.LoadFromRTF(const AFileName: TKString);
var
  Reader: TKMemoRTFReader;
begin
  Reader := TKMemoRTFReader.Create(Self);
  try
    Clear(False);
    Reader.LoadFromFile(AFileName);
  finally
    Reader.Free;
  end;
end;

procedure TKCustomMemo.LoadFromRTFStream(AStream: TStream; AtIndex: Integer);
var
  Reader: TKMemoRTFReader;
begin
  Reader := TKMemoRTFReader.Create(Self);
  try
    Reader.LoadFromStream(AStream, AtIndex);
  finally
    Reader.Free;
  end;
end;

procedure TKCustomMemo.LoadFromTXT(const AFileName: TKString);
var
  List: TStringList;
begin
  if FileExists(AFileName) then
  begin
    Clear(False);
    List := TStringList.Create;
    try
      List.LoadFromFile(AFileName);
      Text := List.Text;
    finally
      List.Free;
    end;
  end;
end;

procedure TKCustomMemo.MeasurePages(var Info: TKPrintMeasureInfo);
var
  FitToPage: Boolean;
  Scale: Double;
  PageHeight, PageCount: Integer;
  APageSetup: TKPrintPageSetup;
begin
  APageSetup := PageSetup;
  FitToPage := poFitToPage in APageSetup.Options;
  Scale := APageSetup.Scale / 100;
  Info.ControlHorzPageCount := 1; // no horizontal page splitting yet, cut it
  if ContentWidth > 0 then
  begin
    if FitToPage then
      Scale := APageSetup.MappedControlPaintAreaWidth / ContentWidth;
    PageHeight := Round(APageSetup.MappedPaintAreaHeight / Scale);
    PrepareToPrint(Scale);
    PageCount := FBlocks.GetPageCount(PageHeight);
    Info.OutlineWidth := FBlocks.Width;
    Info.OutlineHeight := PageCount * PageHeight;
    Info.ControlVertPageCount := PageCount;
  end else
    Info.ControlVertPageCount := 1;
end;

procedure TKCustomMemo.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
  Action: TKMemoMouseAction;
  StartIndex, EndIndex: Integer;
begin
  inherited;
  if Enabled then
  begin
    SafeSetFocus;
    P := Point(X, Y);
    case Button of
      mbRight: Action := maRightDown;
      mbMiddle: Action := maMidDown;
    else
      Action := maLeftDown;
    end;
    if not FBlocks.MouseAction(Action, Canvas, PointToBlockPoint(P, False), Shift) then
    begin
      if Button = mbLeft then
      begin
        SetActiveBlocksForPoint(P);
        if ssDouble in Shift then
        begin
          GetNearestWordIndexes(SelEnd, False, StartIndex, EndIndex);
          Select(StartIndex, EndIndex - StartIndex, False);
        end else
        begin
          Include(FStates, elMouseCapture);
          SelectionInit(P, False);
        end;
        ClampInView(@P, eoScrollWindow in FOptions);
      end;
    end;
  end;
end;

procedure TKCustomMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  P := Point(X, Y);
  if elMouseDrag in FStates then
    DragBlock
  else if elMouseCapture in FStates then
  begin
    if not FScrollTimer.Enabled then
      SelectionExpand(P, True);
  end else
  begin
    UpdateMouseCursor;
  end;
end;

procedure TKCustomMemo.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Action: TKMemoMouseAction;
  P: TPoint;
begin
  inherited;
  Exclude(FStates, elMouseCapture);
  UpdateEditorCaret;
  if elMouseDrag in FStates then
    MoveBlock
  else
  begin
    case Button of
      mbRight: Action := maRightUp;
      mbMiddle: Action := maMidUp;
    else
      Action := maLeftUp;
    end;
    P := Point(X, Y);
    FBlocks.MouseAction(Action, Canvas, PointToBlockPoint(P, False), Shift);
  end;
  UpdateMouseCursor;
end;

procedure TKCustomMemo.MoveBlock;
begin
  if elMouseDrag in FStates then
  begin
    Exclude(FStates, elMouseDrag);
    FDragRect := NormalizeRect(FDragRect);
    if (FSelectedBlock <> nil) and not EqualRect(FDragRect, FDragOrigRect) then
    begin
      FSelectedBlock.LockUpdate;
      try
        FSelectedBlock.LeftOffset := FSelectedBlock.LeftOffset + FDragRect.Left - FDragOrigRect.Left;
        FSelectedBlock.TopOffset := FSelectedBlock.TopOffset + FDragRect.Top - FDragOrigRect.Top;
        FSelectedBlock.Resize(FDragRect.Right - FDragRect.Left, FDragRect.Bottom - FDragRect.Top);
      finally
        FSelectedBlock.UnlockUpdate;
      end;
      Modified := True;
    end;
  end;
end;

procedure TKCustomMemo.MoveCaretToMouseCursor(AIfOutsideOfSelection: Boolean);
var
  P: TPoint;
  Index: Integer;
  Move: Boolean;
begin
  if Enabled then
  begin
    SafeSetFocus;
    P := ScreenToClient(Mouse.CursorPos);
    if AIfOutsideOfSelection then
    begin
      Index := PointToIndex(P, True, False, FLinePosition);
      Move := (Index < RealSelStart) or (Index > RealSelEnd);
    end else
      Move := True;
    if Move then
    begin
      SetActiveBlocksForPoint(P);
      SelectionInit(P, False);
      ClampInView(@P, eoScrollWindow in FOptions);
    end;
  end;
end;

procedure TKCustomMemo.PaintContent(ACanvas: TCanvas; const ARect: TRect; ALeftOfs, ATopOfs: Integer);
var
  H, I, J, SaveIndex: Integer;
begin
  SaveIndex := SaveDC(ACanvas.handle); // don't delete
  try
    if (FColors.BkGnd <> clNone) or (FBackground.Color <> clNone) then
    begin
      if FBackground.Color <> clNone then
        Brush.Color := FBackground.Color
      else
        Brush.Color := FColors.BkGnd;
      Brush.Style := bsSolid;
      ACanvas.Brush.Assign(Brush);
      ACanvas.FillRect(ARect);
    end;
    if (FBackground.Image.Graphic <> nil) and not FBackground.Image.Graphic.Empty then
    begin
      I := ARect.Left + (ALeftOfs - FContentPadding.Left) mod FBackground.Image.Width;
      J := ARect.Top + (ATopOfs - FContentPadding.Top) mod FBackground.Image.Height;
      H := I;
      while J < ARect.Bottom do
      begin
        ACanvas.Draw(I, J, FBackground.Image.Graphic);
        Inc(I, FBackground.Image.Width);
        if not FBackground.RepeatX or (I >= ARect.Right) then
        begin
          if FBackground.RepeatY then
            Inc(J, FBackground.Image.Height)
          else
            J := ARect.Bottom;
          I := H;
        end;
      end;
    end;
    FBlocks.PaintToCanvas(ACanvas, ALeftOfs, ATopOfs, ARect);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TKCustomMemo.PaintPage;
var
  AreaWidth, AreaHeight, PageOffset, PageHeight: Integer;
  TmpRect, TmpRect1: TRect;
  APageSetup: TKPrintPageSetup;
begin
  // poSelOnly not supported
  // poUseColor not supported - always paints in colors
  APageSetup := PageSetup;
  AreaWidth := Round(APageSetup.MappedControlPaintAreaWidth / APageSetup.CurrentScale);
  AreaHeight := Round(APageSetup.MappedPaintAreaHeight / APageSetup.CurrentScale);
  TmpRect := Rect(0, 0, APageSetup.MappedOutlineWidth, APageSetup.MappedOutlineHeight);
  FBlocks.GetPageData(AreaHeight, APageSetup.CurrentPageControl - 1, PageOffset, PageHeight);
  TmpRect1 := Rect(0, 0, AreaWidth, PageHeight);
  IntersectRect(TmpRect, TmpRect, TmpRect1);
  TmpRect1 := TmpRect;
  TranslateRectToDevice(APageSetup.Canvas.Handle, TmpRect1);
  SelectClipRect(APageSetup.Canvas.Handle, TmpRect1);
  PaintContent(APageSetup.Canvas, TmpRect, 0, - PageOffset);
end;

procedure TKCustomMemo.PaintToCanvas(ACanvas: TCanvas);
begin
{$IFDEF FPC}
  if CaretVisible then
    HideEditorCaret;
  try
{$ENDIF}
    PrepareToPaint;
    PaintContent(ACanvas, ClientRect, ContentLeft, ContentTop);
    if elMouseDrag in FStates then
    begin
      SetBkColor(ACanvas.Handle, $FFFFFF);
      SetTextColor(ACanvas.Handle, 0);
      ACanvas.DrawFocusRect(NormalizeRect(FDragRect));
    end;
{$IFDEF FPC}
  finally
    if CaretVisible then
      ShowEditorCaret;
  end;
{$ENDIF}
end;

procedure TKCustomMemo.ParaStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
begin
  FBlocks.NotifyDefaultParaChange;
end;

function TKCustomMemo.PointToBlockPoint(const APoint: TPoint; ACalcActive: Boolean): TPoint;
begin
  Result.X := APoint.X - ContentLeft;
  Result.Y := APoint.Y - ContentTop;
  if (ActiveBlocks <> FBlocks) and ACalcActive then
  begin
    OffsetPoint(Result, -ActiveBlocks.TotalLeftOffset, -ActiveBlocks.TotalTopOffset);
  end;
end;

function TKCustomMemo.PointToIndex(APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer;
begin
  Result := ActiveBlocks.PointToIndex(Canvas, PointToBlockPoint(APoint), AOutOfArea, ASelectionExpanding, ALinePos);
end;

procedure TKCustomMemo.PrepareToPaint;
begin
  if elPrinting in FStates then
  begin
    Exclude(FStates, elPrinting);
    FBlocks.NotifyPrintEnd;
    FBlocks.MeasureExtent(Canvas, RequiredContentWidth);
  end;
end;

procedure TKCustomMemo.PrepareToPrint(AScale: Double);
begin
  if not (elPrinting in FStates) then
  begin
    Include(FStates, elPrinting);
    FBlocks.NotifyPrintBegin;
    FBlocks.MeasureExtent(Canvas, Round(PageSetup.MappedControlPaintAreaWidth / AScale));
  end;
end;

procedure TKCustomMemo.PrintPaintBegin;
begin
  PrepareToPrint(PageSetup.CurrentScale);
end;

procedure TKCustomMemo.PrintPaintEnd;
begin
end;

procedure TKCustomMemo.SafeSetFocus;
begin
  if not Focused and CanFocus and not (csDesigning in ComponentState) then SetFocus;
end;

procedure TKCustomMemo.SaveToFile(const AFileName: TKString;
  ASelectedOnly: Boolean);
var
  Ext: TKString;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  if Ext = '.rtf' then
    SaveToRTF(AFileName)
  else
    SaveToTXT(AFileName);
end;

procedure TKCustomMemo.SaveToRTF(const AFileName: TKString;
  ASelectedOnly: Boolean; AReadableOutput: Boolean);
var
  Writer: TKMemoRTFWriter;
begin
  ActiveBlocks := FBlocks;
  Writer := TKMemoRTFWriter.Create(Self);
  try
    Writer.ReadableOutput := AReadableOutput;
    Writer.SaveToFile(AFileName, ASelectedOnly);
  finally
    Writer.Free;
  end;
end;

procedure TKCustomMemo.SaveToRTFStream(AStream: TStream;
  ASelectedOnly: Boolean; AReadableOutput: Boolean);
var
  Writer: TKMemoRTFWriter;
begin
  Writer := TKMemoRTFWriter.Create(Self);
  try
    Writer.ReadableOutput := AReadableOutput;
    Writer.SaveToStream(AStream, ASelectedOnly);
  finally
    Writer.Free;
  end;
end;

procedure TKCustomMemo.SaveToTXT(const AFileName: TKString;
  ASelectedOnly: Boolean);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    if ASelectedOnly then
      List.Text := FBlocks.SelText
    else
      List.Text := FBlocks.Text;
    List.SaveToFile(AFileName);
  finally
    List.Free;
  end;
end;

function TKCustomMemo.Scroll(CodeHorz, CodeVert, DeltaHorz, DeltaVert: Integer; ACallScrollWindow: Boolean): Boolean;

  function Axis(Code: Cardinal; HasScrollBar: Boolean;
    ScrollCode: Cardinal; Delta, MaxScrollPos: Integer; var ScrollPos: Integer): Boolean;
  var
    OldPos, Pos: Integer;
    SI: TScrollInfo;
  begin
    Result := False;
    if HasScrollBar then
    begin
      SI.cbSize := SizeOf(TScrollInfo);
      SI.fMask := SIF_PAGE or SIF_RANGE or SIF_TRACKPOS;
      GetScrollInfo(Handle, Code, SI);
    end;
    Pos := ScrollPos;
    OldPos := Pos;
    if Delta <> 0 then
      Inc(Pos, Delta)
    else if HasScrollBar then
    case ScrollCode of
      SB_TOP: Pos := 0;
      SB_BOTTOM: Pos := SI.nMax;
      SB_LINEUP: Dec(Pos);
      SB_LINEDOWN: Inc(Pos);
      SB_PAGEUP: Dec(Pos, SI.nPage);
      SB_PAGEDOWN: Inc(Pos, SI.nPage);
      SB_THUMBTRACK: Pos := SI.nTrackPos;
    end;
    Pos := MinMax(Pos, 0, MaxScrollPos);
    if Pos <> OldPos then
    begin
      if HasScrollBar then
      begin
        SI.nPos := Pos;
        SI.fMask := SIF_POS;
        SetScrollInfo(Handle, Code, SI, True);
      end;
      ScrollPos := Pos;
      Result := True;
    end;
  end;

var
  OldLeftPos, OldTopPos: Integer;
  ScrollHorzAxis, ScrollVertAxis: Boolean;
begin
  OldLeftPos := FLeftPos;
  OldTopPos := FTopPos;
  ScrollHorzAxis := Axis(SB_HORZ, FScrollBars in [ssHorizontal, ssBoth], CodeHorz, DeltaHorz, FHorzScrollExtent, FLeftPos);
  ScrollVertAxis := Axis(SB_VERT, FScrollBars in [ssVertical, ssBoth], CodeVert, DeltaVert, FVertScrollExtent, FTopPos);
  Result := ScrollHorzAxis or ScrollVertAxis;
  if Result then
  begin
    if ACallScrollWindow then
      ScrollWindowEx(Handle, (OldLeftPos - FLeftPos) * FHorzScrollStep, (OldTopPos - FTopPos) * FVertScrollStep,
        nil, nil, 0, nil, SW_INVALIDATE)
    else
      Invalidate;
    UpdateEditorCaret;
    Inc(FPreferredCaretPos, (OldLeftPos - FLeftPos) * FHorzScrollStep);
  end;
end;

function TKCustomMemo.ScrollBy(DeltaHorz, DeltaVert: Integer; ACallScrollWindow: Boolean): Boolean;
begin
  Result := Scroll(cScrollNoAction, cScrollNoAction, DeltaHorz, DeltaVert, ACallScrollWindow);
end;

procedure TKCustomMemo.ScrollToClientAreaCenter;
var
  R: TRect;
begin
  R := IndexToRect(SelEnd, True);
  ScrollBy((R.Left - ClientWidth div 2) div FHorzScrollStep, (R.Top - ClientHeight div 2) div FVertScrollStep, eoScrollWindow in FOptions);
end;

function TKCustomMemo.ScrollNeeded(AMousePos: PPoint; out DeltaCol, DeltaRow: Integer): Boolean;
var
  HScrollPadding, VScrollPadding: Integer;
begin
  DeltaCol := 0;
  DeltaRow := 0;
  HScrollPadding := HorzScrollPadding;
  VScrollPadding := VertScrollPadding;
  if AMousePos <> nil then
  begin
    if AMousePos.X < HScrollPadding then
      DeltaCol := DivDown(AMousePos.X - HScrollPadding, FHorzScrollStep)
    else if AMousePos.X > ClientWidth - HScrollPadding then
      DeltaCol := DivUp(AMousePos.X - ClientWidth + HScrollPadding, FHorzScrollStep);
    if AMousePos.Y < VScrollPadding then
      DeltaRow := DivDown(AMousePos.Y - VScrollPadding, FVertScrollStep)
    else if AMousePos.Y > ClientHeight - VScrollPadding then
      DeltaRow := DivUp(AMousePos.Y - ClientHeight + VScrollPadding, FVertScrollStep);
  end;
  if (DeltaCol = 0) or (DeltaRow = 0) then
  begin
    if FCaretRect.Left < HScrollPadding then
      DeltaCol := DivDown(FCaretRect.Left - HScrollPadding, FHorzScrollStep)
    else if (FCaretRect.Left + FCaretRect.Right > ClientWidth - HScrollPadding){ and (FCaretRect.Left > FHorzScrollStep)} then
      DeltaCol := DivUp(FCaretRect.Left + FCaretRect.Right - ClientWidth + HScrollPadding, FHorzScrollStep);
    if FCaretRect.Top < VScrollPadding then
      DeltaRow := DivDown(FCaretRect.Top - VScrollPadding, FVertScrollStep)
    else if (FCaretRect.Top + FCaretRect.Bottom > ClientHeight - VScrollPadding){ and (FCaretRect.Top > FVertScrollStep)} then
      DeltaRow := DivUp(FCaretRect.Top + FCaretRect.Bottom - ClientHeight + VScrollPadding, FVertScrollStep);
  end;
  Result := (DeltaCol <> 0) or (DeltaRow <> 0);
end;

procedure TKCustomMemo.ScrollTimerHandler(Sender: TObject);
var
  DeltaHorz, DeltaVert: Integer;
  MousePos: TPoint;
begin
  if (elMouseCapture in FStates) and not Dragging then
  begin
    MousePos := ScreenToClient(Mouse.CursorPos);
    SelectionExpand(MousePos, False);
    if ScrollNeeded(@MousePos, DeltaHorz, DeltaVert) then
      Scroll(cScrollNoAction, cScrollNoAction, DeltaHorz, DeltaVert, False)
    else
      FScrollTimer.Enabled := False;
  end else
    FScrollTimer.Enabled := False;
end;

procedure TKCustomMemo.Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean);
begin
  if FSelectedBlock <> nil then
  begin
    FSelectedBlock.Select(0, 0, False);
    FSelectedBlock := nil;
  end;
  ActiveBlocks.Select(ASelStart, ASelLength, ADoScroll);
end;

procedure TKCustomMemo.SelectionExpand(ASelEnd: Integer; ADoScroll: Boolean; APosition: TKMemoLinePosition);
begin
  FLinePosition := APosition;
  Select(SelStart, ASelEnd - SelStart, ADoScroll);
end;

procedure TKCustomMemo.SelectionExpand(const APoint: TPoint; ADoScroll: Boolean);
var
  NewSelEnd: Integer;
begin
  NewSelEnd := PointToIndex(APoint, True, True, FLinePosition);
  Select(SelStart, NewSelEnd - SelStart, ADoScroll);
end;

procedure TKCustomMemo.SelectionInit(ASelStart: Integer; ADoScroll: Boolean; APosition: TKMemoLinePosition);
begin
  FNewTextStyleValid := False;
  FLinePosition := APosition;
  Select(ASelStart, 0, ADoScroll);
end;

procedure TKCustomMemo.SelectionInit(const APoint: TPoint; ADoScroll: Boolean);
var
  NewSelEnd: Integer;
begin
  FNewTextStyleValid := False;
  NewSelEnd := PointToIndex(APoint, True, False, FLinePosition);
  Select(NewSelEnd, 0, ADoScroll);
  UpdatePreferredCaretPos;
end;

function TKCustomMemo.SelectBlock(AItem: TKMemoBlock; APosition: TKSizingGripPosition): Boolean;
var
  StartIndex: Integer;
begin
  if AItem.Position = mbpText then
  begin
    ActiveBlocks := AItem.ParentRootBlocks;
    StartIndex := ActiveBlocks.ItemToIndex(AItem);
    Result := StartIndex >= 0;
    if Result and (FSelectedBlock <> AItem) then
    begin
      Select(StartIndex, AItem.SelectableLength);
      FSelectedBlock := AItem;
    end;
  end else
  begin
    // just select locally without scrolling and invalidate
    if AItem is TKMemoContainer then
    begin
      ActiveBlocks := TKmemoContainer(AItem).Blocks;
      FSelectedBlock := AItem;
      UpdateEditorCaret
    end else
    begin
      ActiveBlocks := FBlocks;
      Select(SelStart, 0, False); // clear any other selection
      FSelectedBlock := AItem;
      AItem.Select(0, AItem.SelectableLength(True), False);
    end;
    Result := True;
  end;
  // initialize dragging
  if not ReadOnly and ((AItem.Position <> mbpText) or (APosition <> sgpNone)) then
  begin
    FDragCurPos := ScreenToClient(Mouse.CursorPos);
    FDragMode := APosition;
    FDragRect := AItem.SizingRect;
    KFunctions.OffsetRect(FDragRect, ContentLeft, ContentTop);
    FDragOrigRect := FDragRect;
    Include(FStates, elMouseDrag);
  end;
end;

procedure TKCustomMemo.SetActiveBlocks(const Value: TKMemoBlocks);
begin
  if FActiveBlocks <> Value then
  begin
    Select(-1, 0, False);
    FActiveBlocks := Value;
  end;
end;

procedure TKCustomMemo.SetActiveBlocksForPoint(const APoint: TPoint);
var
  TmpBlocks: TKMemoBlocks;
begin
  TmpBlocks := FBlocks.PointToItems(PointToBlockPoint(APoint, False));
  if TmpBlocks <> nil then
    ActiveBlocks := TmpBlocks
  else
    ActiveBlocks := FBlocks;
end;

procedure TKCustomMemo.SetBackground(const Value: TKMemoBackground);
begin
  FBackground.Assign(Value);
end;

procedure TKCustomMemo.SetColors(Value: TKMemoColors);
begin
  FColors.Assign(Value);
end;

procedure TKCustomMemo.SetContentPadding(const Value: TKRect);
begin
  FContentPadding.Assign(Value);
end;

procedure TKCustomMemo.SetDisabledDrawStyle(Value: TKEditDisabledDrawStyle);
begin
  if Value <> FDisabledDrawStyle then
  begin
    FDisabledDrawStyle := Value;
    if not Enabled then
      Invalidate;
  end;
end;

procedure TKCustomMemo.SetLeftPos(Value: Integer);
begin
  Value := MinMax(Value, 0, FHorzScrollExtent - 1);
  if Value <> FLeftPos then
    ScrollBy(Value - FLeftPos, 0, eoScrollWindow in FOptions);
end;

procedure TKCustomMemo.SetMaxWordLength(const Value: Integer);
begin
  if Value <> FMaxWordLength then
  begin
    FMaxWordLength := Value;
    UpdateScrollRange(True);
  end;
end;

procedure TKCustomMemo.SetModified(Value: Boolean);
begin
  if Value <> GetModified then
  begin
    if Value then
      Include(FStates, elModified)
    else
    begin
      Exclude(FStates, elModified);
      if eoUndoAfterSave in FOptions then
        FUndoList.Modified := False
      else
      begin
        FUndoList.Clear;
        FRedoList.Clear;
      end;
    end;
  end;
end;

procedure TKCustomMemo.SetReqMouseCursor(ACursor: TCursor);
begin
  FRequiredMouseCursor := ACursor;
end;

function TKCustomMemo.SetMouseCursor(X, Y: Integer): Boolean;
var
  ACursor: TCursor;
  P: TPoint;
begin
  P := Point(X, Y);
  if PtInRect(ContentRect, P) then
  begin
    ACursor := FRequiredMouseCursor;
  end else
    ACursor := crDefault;
{$IFDEF FPC}
  FCursor := ACursor;
  SetTempCursor(ACursor);
{$ELSE}
  Windows.SetCursor(Screen.Cursors[ACursor]);
{$ENDIF}
  Result := True;
end;

procedure TKCustomMemo.SetNewTextStyle(const Value: TKMemoTextStyle);
begin
  FNewTextStyle.Assign(Value);
  FNewTextStyleValid := True;
end;

procedure TKCustomMemo.SetOptions(const Value: TKEditOptions);
var
  UpdateShowFormatting, UpdateSingleChars: Boolean;
{$IFDEF USE_WINAPI}
  UpdateDropFiles: Boolean;
{$ENDIF}
begin
  if Value <> FOptions then
  begin
    UpdateShowFormatting := (eoShowFormatting in Value) <> (eoShowFormatting in FOptions);
  {$IFDEF USE_WINAPI}
    UpdateDropFiles := (eoDropFiles in Value) <> (eoDropFiles in FOptions);
  {$ENDIF}
    UpdateSingleChars := (eoDrawSingleChars in Value) <> (eoDrawSingleChars in FOptions);
    FOptions := Value;
    FBlocks.NotifyOptionsChange;
    if UpdateShowFormatting or UpdateSingleChars then
      BlocksChanged([muExtent]);
  {$IFDEF USE_WINAPI}
    // (un)register HWND as drop target
    if UpdateDropFiles and not (csDesigning in ComponentState) and HandleAllocated then
      DragAcceptFiles(Handle, (eoDropFiles in fOptions));
  {$ENDIF}
  end;
end;

procedure TKCustomMemo.SetReadOnly(Value: Boolean);
begin
  if Value <> GetReadOnly then
  begin
    if Value then
      Include(FStates, elReadOnly)
    else
      Exclude(FStates, elReadOnly);
  end;
end;

procedure TKCustomMemo.SetRequiredContentWidth(const Value: Integer);
begin
  if Value <> FRequiredContentWidth then
  begin
    FRequiredContentWidth := Value;
    UpdateScrollRange(True);
  end;
end;

procedure TKCustomMemo.SetRTF(const Value: TKMemoRTFString);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create{$IFnDEF COMPILER12_UP}(''){$ENDIF};
  try
    Stream.WriteString(Value);
    Stream.Seek(0, soFromBeginning);
    LoadFromRTFStream(Stream, -1);
  finally
    Stream.Free;
  end;
end;

procedure TKCustomMemo.SetScrollBars(Value: TScrollStyle);
begin
  if Value <> FScrollBars then
  begin
    FScrollBars := Value;
  {$IFDEF FPC}
    UpdateSize;
  {$ELSE}
    RecreateWnd;
  {$ENDIF}
  end;
end;

procedure TKCustomMemo.SetScrollPadding(Value: Integer);
begin
  FScrollPadding := MinMax(Integer(Value), cScrollPaddingMin, cScrollPaddingMax);
end;

procedure TKCustomMemo.SetScrollSpeed(Value: Cardinal);
begin
  Value := MinMax(Integer(Value), cScrollSpeedMin, cScrollSpeedMax);
  if Value <> FScrollSpeed then
  begin
    FScrollSpeed := Value;
    FScrollTimer.Enabled := False;
    FScrollTimer.Interval := FScrollSpeed;
  end;
end;

procedure TKCustomMemo.SetSelectionParaStyle(const Value: TKMemoParaStyle);
begin
  ActiveBlocks.SelectionParaStyle := Value;
  Modified := True;
end;

procedure TKCustomMemo.SetSelectionTextStyle(const Value: TKMemoTextStyle);
begin
  ActiveBlocks.SelectionTextStyle := Value;
  Modified := True;
end;

procedure TKCustomMemo.SetSelEnd(Value: Integer);
begin
  Select(SelStart, Value - SelStart);
end;

procedure TKCustomMemo.SetSelLength(Value: Integer);
begin
  Select(SelStart, Value);
end;

procedure TKCustomMemo.SetSelStart(Value: Integer);
begin
  Select(Value, SelEnd - Value);
end;

procedure TKCustomMemo.SetText(const Value: TKString);
begin
  ActiveBlocks.LockUpdate;
  try
    ActiveBlocks.Clear;
    ActiveBlocks.Text := Value;
  finally
    ActiveBlocks.UnlockUpdate;
  end;
end;

procedure TKCustomMemo.SetTopPos(Value: Integer);
begin
  Value := MinMax(Value, 0, FVertScrollExtent - 1);
  if Value <> FTopPos then
    ScrollBy(0, Value - FTopPos, eoScrollWindow in FOptions);
end;

procedure TKCustomMemo.SetUndoLimit(Value: Integer);
begin
  Value := MinMax(Value, cUndoLimitMin, cUndoLimitMax);
  if Value <> FUndoList.Limit then
  begin
    FUndoList.Limit := Value;
    FRedoList.Limit := Value;
  end;
end;

procedure TKCustomMemo.SetWordBreaks(const Value: TKSysCharSet);
begin
  if Value <> FWordBreaks then
  begin
    FWordBreaks := Value;
    BlocksChanged([muExtent]);
  end;
end;

procedure TKCustomMemo.ShowEditorCaret;
begin
  if HandleAllocated then
  begin
  {$IFDEF FPC}
    SetCaretPosEx(Handle, FCaretRect.Left, FCaretRect.Top);
  {$ELSE}
    SetCaretPos(FCaretRect.Left, FCaretRect.Top);
  {$ENDIF}
    ShowCaret(Handle);
  end;
end;

function TKCustomMemo.SplitAt(AIndex: Integer): Integer;
var
  LocalIndex, InnerLocalIndex: Integer;
  Items: TKmemoBlocks;
  Item, NewItem: TKMemoBlock;
begin
  Items := ActiveBlocks.IndexToItems(AIndex, LocalIndex);
  if Items <> nil then
  begin
    Result := Items.IndexToBlock(LocalIndex, InnerLocalIndex);
    if InnerLocalIndex > 0 then
    begin
      Item := Items[Result];
      NewItem := Item.Split(InnerLocalIndex);
      if NewItem <> nil then
      begin
        Inc(Result);
        Items.AddAt(NewItem, Result);
      end;
    end;
  end else
    Result := 0;
end;

procedure TKCustomMemo.TextStyleChanged(Sender: TObject);
begin
  FBlocks.NotifyDefaultTextChange;
end;

procedure TKCustomMemo.UndoChange(Sender: TObject; ItemKind: TKMemoChangeKind);
begin
{  if (Sender = FUndoList) and (ItemKind <> ckCaretPos) then
    DoChange;}
end;

procedure TKCustomMemo.UpdateEditorCaret(AShow: Boolean);
begin
  if HandleAllocated then
  begin
    Include(FStates, elCaretUpdate);
    try
      if SelLength = 0 then
        ActiveBlocks.FixEOL(SelEnd, True, FLinePosition);
      FCaretRect := IndexToRect(SelEnd, True);
      Dec(FCaretRect.Right, FCaretRect.Left); // Right is width
      Dec(FCaretRect.Bottom, FCaretRect.Top); // Bottom is height

      if AShow then
      begin
        if Enabled and Focused and not (csDesigning in ComponentState) and (SelLength = 0)
          and not (eoDisableCaret in FOptions) and not RelativeSelected then
        begin
          if not (elOverwrite in FStates) then
            FCaretRect.Right := MinMax(FCaretRect.Bottom div 10, 2, 3);
          if not (elCaretCreated in FStates) or
            (FOldCaretRect.Right <> FCaretRect.Right) or (FOldCaretRect.Bottom <> FCaretRect.Bottom) then
          begin
            if CreateCaret(Handle, 0, FCaretRect.Right, FCaretRect.Bottom) then
            begin
              ShowEditorCaret;
              Include(FStates, elCaretVisible);
              Include(FStates, elCaretCreated);
            end;
          end
          else if (FOldCaretRect.Left <> FCaretRect.Left) or (FOldCaretRect.Top <> FCaretRect.Top) then
          begin
            ShowEditorCaret;
            Include(FStates, elCaretVisible);
          end;
          FOldCaretRect := FCaretRect;
        end
        else if elCaretCreated in FStates then
        begin
          HideEditorCaret;
        {$IFDEF FPC}
          DestroyCaret(Handle);
        {$ELSE}
          DestroyCaret;
        {$ENDIF}
          FStates := FStates - [elCaretCreated, elCaretVisible];
        end;
      end;
    finally
      Exclude(FStates, elCaretUpdate);
    end;
  end;
end;

procedure TKCustomMemo.UpdateMouseCursor;
var
  P: TPoint;
  OldCursor, NewCursor: TCursor;
  DoUpdate: Boolean;
begin
  if not (elMouseCapture in FStates) then
  begin
    DoUpdate := False;
    P := ScreenToClient(Mouse.CursorPos);
    NewCursor := crIBeam;
    if NewCursor <> FRequiredMouseCursor then
    begin
      FRequiredMouseCursor := NewCursor;
      DoUpdate := True;
    end;
    OldCursor := FRequiredMouseCursor;
    FBlocks.MouseAction(maMove, Canvas, PointToBlockPoint(P, False), GetShiftState);
    if FRequiredMouseCursor <> OldCursor then
      DoUpdate := True;
    if DoUpdate then
      SetMouseCursor(P.X, P.Y);
  end;
end;

procedure TKCustomMemo.UpdatePreferredCaretPos;
begin
  FPreferredCaretPos := FCaretRect.Left - ContentLeft;
  if ActiveBlocks <> FBlocks then
    Dec(FPreferredCaretPos, ActiveBlocks.TotalLeftOffset);
end;

procedure TKCustomMemo.UpdateScrollRange(CallInvalidate: Boolean);
var
  DeltaHorz, DeltaVert, ClientHorz, ClientVert: Integer;
  SI: TScrollInfo;
  Show: Boolean;
begin
  if HandleAllocated then
  begin
    FBlocks.MeasureExtent(Canvas, RequiredContentWidth);
    if FRequiredContentWidth > 0 then
      FHorzExtent := DivUp(FBlocks.Width + FContentPadding.Left + FContentPadding.Right, FHorzScrollStep)
    else
      FHorzExtent := DivDown(FBlocks.Width + FContentPadding.Left + FContentPadding.Right, FHorzScrollStep);
    FVertExtent := DivUp(FBlocks.Height + FContentPadding.Top + FContentPadding.Bottom, FVertScrollStep);
    ClientHorz := DivDown(ClientWidth, FHorzScrollStep);
    ClientVert := DivDown(ClientHeight, FVertScrollStep);
    DeltaHorz := Max(FLeftPos + ClientHorz - FHorzExtent - 1, 0);
    DeltaVert := Max(FTopPos + ClientVert - FVertExtent - 1, 0);
    FHorzScrollExtent := 0;
    FVertScrollExtent := 0;
    if FScrollBars in [ssBoth, ssHorizontal, ssVertical] then
    begin
      SI.cbSize := SizeOf(TScrollInfo);
      SI.fMask := SIF_RANGE or SIF_PAGE or SIF_POS {$IFDEF UNIX}or SIF_UPDATEPOLICY{$ENDIF};
      SI.nMin := 0;
    {$IFDEF UNIX}
      SI.nTrackPos := SB_POLICY_CONTINUOUS;
    {$ELSE}
      SI.nTrackPos := 0;
    {$ENDIF}
      if FScrollBars in [ssBoth, ssHorizontal] then
      begin
        SI.nMax := FHorzExtent{$IFnDEF FPC}- 1{$ENDIF};
        SI.nPage := ClientHorz;
        SI.nPos := FLeftPos;
        SetScrollInfo(Handle, SB_HORZ, SI, True);
        Show := Integer(SI.nPage) < FHorzExtent;
        FHorzScrollExtent := Max(FHorzExtent - Integer(SI.nPage), 0);
      end else
        Show := False;
      ShowScrollBar(Handle, SB_HORZ, Show);
      if FScrollBars in [ssBoth, ssVertical] then
      begin
        SI.nMax := FVertExtent{$IFnDEF FPC}- 1{$ENDIF};
        SI.nPage := ClientVert;
        SI.nPos := FTopPos;
        SetScrollInfo(Handle, SB_VERT, SI, True);
        Show := Integer(SI.nPage) < FVertExtent;
        FVertScrollExtent := Max(FVertExtent - Integer(SI.nPage), 0);
      end else
        Show := False;
      ShowScrollBar(Handle, SB_VERT, Show);
    end;
    if CallInvalidate then
    begin
      if not ScrollBy(-DeltaHorz, -DeltaVert, False) then
      begin
        UpdateEditorCaret;
        Invalidate;
      end;
    end;
    InvalidatePageSetup;
    PrepareToPaint;
  end;
end;

procedure TKCustomMemo.UpdateSize;
begin
  UpdateScrollRange(True);
end;

procedure TKCustomMemo.WMClear(var Msg: TLMessage);
begin
  ExecuteCommand(ecClearAll);
end;

procedure TKCustomMemo.WMCopy(var Msg: TLMessage);
begin
  ExecuteCommand(ecCopy);
end;

procedure TKCustomMemo.WMCut(var Msg: TLMessage);
begin
  ExecuteCommand(ecCut);
end;

{$IFNDEF FPC}
procedure TKCustomMemo.WMDropFiles(var Msg: TMessage);
var
  I, FileCount: Integer;
  PathName: array[0..260] of Char;
  Point: TPoint;
  FilesList: TStringList;
begin
  try
    if Assigned(FOnDropFiles) then
    begin
      FilesList := TStringList.Create;
      try
        FileCount := DragQueryFile(THandle(Msg.wParam), Cardinal(-1), nil, 0);
        DragQueryPoint(THandle(Msg.wParam), Point);
        for i := 0 to FileCount - 1 do
        begin
          DragQueryFile(THandle(Msg.wParam), I, PathName, SizeOf(PathName));
          FilesList.Add(PathName);
        end;
        FOnDropFiles(Self, Point.X, Point.Y, FilesList);
      finally
        FilesList.Free;
      end;
    end;
  finally
    Msg.Result := 0;
    DragFinish(THandle(Msg.wParam));
  end;
end;
{$ENDIF}

procedure TKCustomMemo.WMEraseBkgnd(var Msg: TLMessage);
begin
  Msg.Result := 1
end;

procedure TKCustomMemo.WMGetDlgCode(var Msg: TLMNoParams);
begin
  Msg.Result := DLGC_WANTARROWS;
  if eoWantTab in FOptions then
    Msg.Result := Msg.Result or DLGC_WANTTAB;
end;

procedure TKCustomMemo.WMHScroll(var Msg: TLMHScroll);
begin
  SafeSetFocus;
  Scroll(Msg.ScrollCode, cScrollNoAction, 0, 0, eoScrollWindow in FOptions);
end;

procedure TKCustomMemo.WMKillFocus(var Msg: TLMKillFocus);
begin
  inherited;
  ExecuteCommand(ecLostFocus);
end;

procedure TKCustomMemo.WMPaste(var Msg: TLMessage);
begin
  ExecuteCommand(ecPaste);
end;

procedure TKCustomMemo.WMSetFocus(var Msg: TLMSetFocus);
begin
  inherited;
  ExecuteCommand(ecGotFocus);
end;

procedure TKCustomMemo.WMVScroll(var Msg: TLMVScroll);
begin
  SafeSetFocus;
  Scroll(cScrollNoAction, Msg.ScrollCode, 0, 0, eoScrollWindow in FOptions);
end;

{ TKMemoBlock }

constructor TKMemoBlock.Create;
begin
  inherited;
  FOffset := CreateEmptyPoint;
end;

destructor TKMemoBlock.Destroy;
begin
  if MemoNotifier <> nil then
    MemoNotifier.BlockFreeNotification(Self);
  inherited Destroy;
end;

function TKMemoBlock.EqualProperties(ASource: TKObject): Boolean;
begin
  Result := False;
end;

function TKMemoBlock.ActiveBlocks: TKMemoBlocks;
var
  Notifier: IKMemoNotifier;
begin
  Notifier := MemoNotifier;
  if Notifier <> nil then
    Result := Notifier.GetActiveBlocks
  else
    Result := nil;
end;

procedure TKMemoBlock.Assign(ASource: TKObject);
begin
  if ASource is TKMemoBlock then
  begin
    Select(TKMemoBlock(ASource).SelStart, TKMemoBlock(ASource).SelLength, False);
    AssignAttributes(TKMemoBlock(ASource));
  end;
end;

procedure TKMemoBlock.AssignAttributes(AItem: TKMemoBlock);
begin
  if AItem <> nil then
  begin
    Position := AItem.Position;
    LeftOffset := AItem.LeftOffset;
    TopOffset := AItem.TopOffset;
  end;
end;

function TKMemoBlock.CalcAscent(ACanvas: TCanvas): Integer;
begin
  Result := 0;
end;

procedure TKMemoBlock.CallAfterUpdate;
begin
  if ParentBlocks <> nil then
    ParentBlocks.UnlockUpdate;
end;

procedure TKMemoBlock.CallBeforeUpdate;
begin
  if ParentBlocks <> nil then
    ParentBlocks.LockUpdate;
end;

function TKMemoBlock.CanAdd(AItem: TKMemoBlock): Boolean;
begin
  Result := False;
end;

procedure TKMemoBlock.ClearSelection(ATextOnly: Boolean);
begin
end;

function TKMemoBlock.Concat(AItem: TKMemoBlock): Boolean;
begin
  Result := False;
end;

function TKMemoBlock.ContentLength: Integer;
begin
  Result := 0;
end;

function TKMemoBlock.GetBottomPadding: Integer;
begin
  Result := 0;
end;

function TKMemoBlock.GetBoundsRect: TRect;
begin
  Result := Rect(Left, Top, Left + Width, Top + Height);
end;

function TKMemoBlock.GetCanAddText: Boolean;
begin
  Result := False;
end;

function TKMemoBlock.GetWrapMode: TKMemoBlockWrapMode;
begin
  Result := wrAround;
end;

function TKMemoBlock.GetDefaultParaStyle: TKMemoParaStyle;
begin
  if Parent <> nil then
    Result := ParentBlocks.DefaultParaStyle
  else
    Result := nil;
end;

function TKMemoBlock.GetDefaultTextStyle: TKMemoTextStyle;
begin
  if Parent <> nil then
    Result := ParentBlocks.DefaultTextStyle
  else
    Result := nil;
end;

function TKMemoBlock.GetHeight: Integer;
begin
  Result := WordHeight[0];
end;

function TKMemoBlock.GetLeft: Integer;
begin
  Result := WordLeft[0];
end;

function TKMemoBlock.GetMemoNotifier: IKMemoNotifier;
begin
  if Parent <> nil then
    Result := ParentBlocks.MemoNotifier
  else
    Result := nil;
end;

function TKMemoBlock.GetPaintSelection: Boolean;
var
  Notifier: IKMemoNotifier;
begin
  Notifier := MemoNotifier;
  if Notifier <> nil then
    Result := Notifier.GetPaintSelection
  else
    Result := False;
end;

function TKMemoBlock.GetParaStyle: TKMemoParaStyle;
begin
  Result := nil;
end;

function TKMemoBlock.GetParentBlocks: TKMemoBlocks;
begin
  Result := Parent as TKMemoBlocks;
end;

function TKMemoBlock.GetParentRootBlocks: TKMemoBlocks;
var
  Item: TKMemoBlock;
begin
  Result := ParentBlocks;

  if Result <> nil then
  begin
    Item := Result.Parent;
    while (Item <> nil) and (Item.Position = mbpText) do
    begin
      Result := Item.ParentBlocks;
      Item := Result.Parent;
    end;
  end;
end;

function TKMemoBlock.GetPrinting: Boolean;
var
  Notifier: IKMemoNotifier;
begin
  Notifier := MemoNotifier;
  if Notifier <> nil then
    Result := Notifier.GetPrinting
  else
    Result := False;
end;

function TKMemoBlock.GetReadOnly: Boolean;
var
  Notifier: IKMemoNotifier;
begin
  Notifier := MemoNotifier;
  if Notifier <> nil then
    Result := Notifier.GetReadOnly
  else
    Result := False;
end;

procedure TKMemoBlock.GetSelColors(out Foreground, Background: TColor);
begin
  Foreground := cSelTextFocusedDef;
  Background := cSelBkGndFocusedDef;
  if Parent <> nil then
    ParentBlocks.GetSelColors(Foreground, Background);
end;

function TKMemoBlock.GetSelEnd: Integer;
begin
  Result := SelStart + SelLength;
end;

function TKMemoBlock.GetSelLength: Integer;
begin
  Result := 0;
end;

function TKMemoBlock.GetSelStart: Integer;
begin
  Result := -1;
end;

function TKMemoBlock.GetSelText: TKString;
begin
  Result := '';
end;

function TKMemoBlock.GetShowFormatting: Boolean;
begin
  if Parent <> nil then
    Result := ParentBlocks.ShowFormatting
  else
    Result := False;
end;

function TKMemoBlock.GetSizingRect: TRect;
begin
  Result := BoundsRect;
  KFunctions.OffsetRect(Result, RealLeftOffset, RealTopOffset);
end;

function TKMemoBlock.GetText: TKString;
begin
  Result := '';
end;

function TKMemoBlock.GetTop: Integer;
begin
  Result := WordTop[0];
end;

function TKMemoBlock.GetTopPadding: Integer;
begin
  Result := 0;
end;

function TKMemoBlock.GetWidth: Integer;
begin
  Result := WordWidth[0];
end;

function TKMemoBlock.GetWordBaseLine(Index: Integer): Integer;
begin
  Result := 0;
end;

function TKMemoBlock.GetWordBottomPadding(Index: Integer): Integer;
begin
  Result := 0;
end;

function TKMemoBlock.GetWordBoundsRect(Index: Integer): TRect;
begin
  Result := CreateEmptyRect;
end;

function TKMemoBlock.GetWordBreakable(Index: Integer): Boolean;
begin
  Result := True;
end;

function TKMemoBlock.GetWordClipped(Index: Integer): Boolean;
begin
  Result := False;
end;

function TKMemoBlock.GetWordCount: Integer;
begin
  Result := 0;
end;

function TKMemoBlock.GetWordHeight(Index: Integer): Integer;
begin
  Result := 0;
end;

procedure TKMemoBlock.GetWordIndexes(AIndex: Integer; out ASt, AEn: Integer);
begin
  ASt := 0;
  AEn := 0;
end;

function TKMemoBlock.GetWordLeft(Index: Integer): Integer;
begin
  Result := 0;
end;

function TKMemoBlock.GetWordLength(Index: Integer): Integer;
begin
  Result := 0;
end;

function TKMemoBlock.GetWordLengthWOWS(Index: Integer): Integer;
begin
  Result := GetWordLength(Index);
end;

function TKMemoBlock.GetWords(Index: Integer): TKString;
begin
  Result := '';
end;

function TKMemoBlock.GetWordTop(Index: Integer): Integer;
begin
  Result := 0;
end;

function TKMemoBlock.GetWordTopPadding(Index: Integer): Integer;
begin
  Result := 0;
end;

function TKMemoBlock.GetWordWidth(Index: Integer): Integer;
begin
  Result := 0;
end;

function TKMemoBlock.IndexToRect(ACanvas: TCanvas; AIndex: Integer; ACaret: Boolean): TRect;
var
  Found: Boolean;
  I, WLen: Integer;
begin
  Result := CreateEmptyRect;
  Found := False;
  I := 0;
  WLen := 0;
  while not Found and (I < WordCount) do
  begin
    Result := WordIndexToRect(ACanvas, I, AIndex - WLen, ACaret);
    Inc(WLen, WordLength[I]);
    Inc(I);
  end;
end;

function TKMemoBlock.InsertParagraph(AIndex: Integer): Boolean;
var
  ParentIndex: Integer;
  NewItem: TKMemoBlock;
begin
  Result := False;
  if Parent <> nil then
  begin
    ParentIndex := ParentBlocks.IndexOf(Self);
    NewItem := Split(AIndex);
    if NewItem <> nil then
    begin
      ParentBlocks.AddAt(NewItem, ParentIndex + 1);
      ParentBlocks.AddParagraph(ParentIndex + 1);
    end else
    begin
      if AIndex = 0 then
        ParentBlocks.AddParagraph(ParentIndex)
      else
        ParentBlocks.AddParagraph(ParentIndex + 1);
    end;
    Result := True;
  end;
end;

function TKMemoBlock.InsertString(const AText: TKString; At: Integer): Boolean;
begin
  Result := False;
end;

function TKMemoBlock.RealLeftOffset: Integer;
begin
  if FPosition <> mbpText then
    Result := FOffset.X
  else
    Result := 0;
end;

function TKMemoBlock.RealTopOffset: Integer;
begin
  if FPosition <> mbpText then
    Result := FOffset.Y
  else
    Result := 0;
end;

procedure TKMemoBlock.Resize(ANewWidth, ANewHeight: Integer);
begin
end;

function TKMemoBlock.MeasureExtent(ACanvas: TCanvas; ARequiredWidth: Integer): TPoint;
var
  I: Integer;
  Extent: TPoint;
begin
  Result := CreateEmptyPoint;
  for I := 0 to WordCount - 1 do
  begin
    Extent := WordMeasureExtent(ACanvas, I, ARequiredWidth);
    Inc(Result.X, Extent.X);
    Result.Y := Max(Result.Y, Extent.Y);
  end;
end;

procedure TKMemoBlock.NotifyDefaultParaChange;
begin
end;

procedure TKMemoBlock.NotifyDefaultTextChange;
begin
end;

procedure TKMemoBlock.NotifyOptionsChange;
begin
end;

procedure TKMemoBlock.NotifyPrintBegin;
begin
end;

procedure TKMemoBlock.NotifyPrintEnd;
begin
end;

procedure TKMemoBlock.PaintToCanvas(ACanvas: TCanvas; ALeft, ATop: Integer);
var
  I: Integer;
begin
  for I := 0 to WordCount - 1 do
    WordPaintToCanvas(ACanvas, I, ALeft, ATop);
end;

function TKMemoBlock.PointToIndex(ACanvas: TCanvas; const APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer;
var
  I: Integer;
begin
  Result := -1;
  I := 0;
  while (Result < 0) and (I < WordCount) do
  begin
    Result := WordPointToIndex(ACanvas, APoint, I, AOutOfArea, ASelectionExpanding, APosition);
    Inc(I);
  end;
end;

function TKMemoBlock.WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint;
  AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer;
begin
  Result := -1;
end;

function TKMemoBlock.Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean): Boolean;
begin
  Result := False;
end;

function TKMemoBlock.SelectableLength(ALocalCalc: Boolean): Integer;
begin
  if (Position = mbpText) or ALocalCalc then
    Result := ContentLength
  else
    Result := 0;
end;

procedure TKMemoBlock.SelectAll;
begin
  Select(0, SelectableLength, False);
end;

function TKMemoBlock.SelectedBlock: TKMemoBlock;
var
  Notifier: IKMemoNotifier;
begin
  Notifier := MemoNotifier;
  if Notifier <> nil then
    Result := Notifier.GetSelectedBlock
  else
    Result := nil;
end;

procedure TKMemoBlock.SetLeftOffset(const Value: Integer);
begin
  if Value <> FOffset.X then
  begin
    FOffset.X := Value;
    Update([muExtent]);
  end;
end;

procedure TKMemoBlock.SetPosition(const Value: TKMemoBlockPosition);
begin
  if Value <> FPosition then
  begin
    FPosition := Value;
    Update([muContent]);
  end;
end;

procedure TKMemoBlock.SetTopOffset(Value: Integer);
var
  Index: Integer;
  Blocks: TKMemoBlocks;
  Item: TKMemoBlock;
begin
  if Value <> FOffset.Y then
  begin
    if (Value < 0) and (FPosition = mbpRelative) then
    begin
      Blocks := ParentBlocks;
      if Blocks <> nil then
      begin
        Blocks.LockUpdate;
        try
          // try to move block anchor first
          Index := Blocks.IndexOf(Self) - 1;
          if Index > 0 then
          begin
            Inc(Value, TopOffset);
            repeat
              Index := Blocks.GetNearestAnchorIndex(Index);
              if Index >= 0 then
              begin
                Item := Blocks[Index];
                Inc(Value, Item.Height);
                Dec(Index);
              end;
            until (Value >= 0) or (Index < 0);
            Inc(Index);
            Blocks.Extract(Self);
            Blocks.AddAt(Self, Index);
          end;
        finally
          Blocks.UnLockUpdate;
        end;
      end;
    end;
    FOffset.Y := Value;
    Update([muExtent]);
  end;
end;

procedure TKMemoBlock.SetWordBaseLine(Index: Integer; const Value: Integer);
begin
end;

procedure TKMemoBlock.SetWordBottomPadding(Index: Integer; const Value: Integer);
begin
end;

procedure TKMemoBlock.SetWordClipped(Index: Integer; const Value: Boolean);
begin

end;

procedure TKMemoBlock.SetWordHeight(Index: Integer; const Value: Integer);
begin
end;

procedure TKMemoBlock.SetWordLeft(Index: Integer; const Value: Integer);
begin
end;

procedure TKMemoBlock.SetWordTop(Index: Integer; const Value: Integer);
begin
end;

procedure TKMemoBlock.SetWordTopPadding(Index: Integer; const Value: Integer);
begin
end;

procedure TKMemoBlock.SetWordWidth(Index: Integer; const Value: Integer);
begin
end;

function TKMemoBlock.SizingGripsCursor(const ARect: TRect; const APoint: TPoint): TCursor;
var
  Grips: TKSizingGrips;
begin
  Grips := TKSizingGrips.Create;
  try
    Grips.BoundsRect := ARect;
    Result := Grips.CursorAt(APoint);
  finally
    Grips.Free;
  end;
end;

procedure TKMemoBlock.SizingGripsDraw(ACanvas: TCanvas; const ARect: TRect);
var
  Grips: TKSizingGrips;
begin
  Grips := TKSizingGrips.Create;
  try
    Grips.BoundsRect := ARect;
    Grips.DrawTo(ACanvas);
  finally
    Grips.Free;
  end;
end;

function TKMemoBlock.SizingGripsPosition(const ARect: TRect;
  const APoint: TPoint): TKSizingGripPosition;
var
  Grips: TKSizingGrips;
begin
  Grips := TKSizingGrips.Create;
  try
    Grips.BoundsRect := ARect;
    Result := Grips.HitTest(APoint);
  finally
    Grips.Free;
  end;
end;

function TKMemoBlock.Split(At: Integer; AllowEmpty: Boolean): TKMemoBlock;
begin
  Result := nil;
end;

procedure TKMemoBlock.Update(AReasons: TKMemoUpdateReasons);
begin
  if Parent <> nil then
    ParentBlocks.Update(AReasons);
end;

function TKMemoBlock.WordIndexToRect(ACanvas: TCanvas; AWordIndex: Integer;
  AIndex: Integer; ACaret: Boolean): TRect;
begin
  Result := CreateEmptyRect;
end;

function TKMemoBlock.WordMeasureExtent(ACanvas: TCanvas; AWordIndex, ARequiredWidth: Integer): TPoint;
begin
  Result := CreateEmptyPoint;
end;

function TKMemoBlock.WordMouseAction(ACanvas: TCanvas; AWordIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean;
begin
  Result := False;
end;

procedure TKMemoBlock.WordPaintToCanvas(ACanvas: TCanvas; AWordIndex, ALeft, ATop: Integer);
begin
end;

{ TKMemoSingleBlock }

constructor TKMemoSingleton.Create;
begin
  inherited;
  FSelEnd := -1;
  FSelStart := -1;
end;

function TKMemoSingleton.GetSelLength: Integer;
begin
  Result := FSelEnd - FSelStart;
end;

function TKMemoSingleton.GetSelStart: Integer;
begin
  Result := FSelStart;
end;

function TKMemoSingleton.Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean): Boolean;
var
  NewSelEnd, MaxLen: Integer;
begin
  NewSelEnd := ASelStart + ASelLength;
  if NewSelEnd < ASelStart then
    Exchange(ASelStart, NewSelEnd);
  MaxLen := SelectableLength(True);
  NewSelEnd := MinMax(NewSelEnd, -1, MaxLen);
  ASelStart := MinMax(ASelStart, -1, MaxLen);
  if (ASelStart <> FSelStart) or (NewSelEnd <> FSelEnd) then
  begin
    FSelEnd := NewSelEnd;
    FSelStart := ASelStart;
    if ADoScroll then
      Update([muSelectionScroll])
    else
      Update([muSelection]);
    Result := True;
  end else
    Result := False;
end;

{ TKTextMemoBlock }

constructor TKMemoTextBlock.Create;
begin
  FTextStyle := TKMemoTextStyle.Create;
  FTextStyle.OnChanged := TextStyleChanged;
  inherited;
  FText := '';
  FTextLength := 0;
  FWordCount := 0;
  FWords := TKMemoWordList.Create;
end;

destructor TKMemoTextBlock.Destroy;
begin
  FTextStyle.Free;
  FWords.Free;
  FWordCount := 0;
  inherited;
end;

function TKMemoTextBlock.DrawSingleChars: Boolean;
var
  Notifier: IKMemoNotifier;
begin
  Notifier := MemoNotifier;
  if Notifier <> nil then
    Result := Notifier.GetDrawSingleChars
  else
    Result := False;
end;

function TKMemoTextBlock.EqualProperties(ASource: TKObject): Boolean;
begin
  if ASource is TKMemoTextBlock then
  begin
    Result :=
      (TKMemoTextBlock(ASource).Text = Text) and
      TKMemoTextBlock(ASource).TextStyle.EqualProperties(TextStyle);
  end else
    Result := False;
end;

function TKMemoTextBlock.ApplyFormatting(const AText: TKString): TKString;
begin
  if ShowFormatting then
  begin
    Result := UnicodeStringReplace(AText, ' ', SpaceChar, [rfReplaceAll]);
  end else
  begin
    Result := UnicodeStringReplace(AText, NewLineChar, ' ', [rfReplaceAll]);
  end;
end;

procedure TKMemoTextBlock.ApplyTextStyle(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Font.Assign(FTextStyle.Font);
    FScriptFontHeight := FTextStyle.Font.Height;
    Font.Height := FScriptFontHeight;
    case FTextStyle.ScriptPosition of
      tpoSuperscript:
      begin
        FScriptVertOffset := GetFontAscent(ACanvas.Handle); // aligned to ascent line of original font
        FScriptFontHeight := MulDiv(FScriptFontHeight, 3, 5); // 60%
        Font.Height := FScriptFontHeight;
        Dec(FScriptVertOffset, GetFontAscent(ACanvas.Handle));
      end;
      tpoSubscript:
      begin
        FScriptVertOffset := -GetFontDescent(ACanvas.Handle); // aligned to descent line of original font
        FScriptFontHeight := MulDiv(FScriptFontHeight, 3, 5); // 60%
        Font.Height := FScriptFontHeight;
      end;
    else
      FScriptVertOffset := 0;
    end;
    if FTextStyle.AllowBrush then
      Brush.Assign(FTextStyle.Brush)
    else
    begin
      Brush.Style := bsClear;
      Font.Style := Font.Style - [fsUnderLine];
    end;
  end;
end;

procedure TKMemoTextBlock.Assign(ASource: TKObject);
begin
  inherited;
  if ASource is TKMemoTextBlock then
    Text := TKMemoTextBlock(ASource).Text;
end;

procedure TKMemoTextBlock.AssignAttributes(AItem: TKMemoBlock);
begin
  inherited;
  if AItem is TKMemoTextBlock then
    TextStyle.Assign(TKMemoTextBlock(AItem).TextStyle);
end;

function TKMemoTextBlock.CalcAscent(ACanvas: TCanvas): Integer;
begin
  ApplyTextStyle(ACanvas);
  Result := GetFontAscent(ACanvas.Handle);
end;

function TKMemoTextBlock.CalcDescent(ACanvas: TCanvas): Integer;
begin
  ApplyTextStyle(ACanvas);
  Result := GetFontDescent(ACanvas.Handle);
end;

procedure TKMemoBlocks.CallAfterUpdate;
begin
  if FUpdateReasons <> [] then
    Update(FUpdateReasons);
end;

procedure TKMemoBlocks.CallBeforeUpdate;
begin
  FUpdateReasons := [];
end;

procedure TKMemoBlocks.Clear;
begin
  LockUpdate;
  try
    inherited;
    FSelEnd := -1;
    FSelStart := -1;
  finally
    UnlockUpdate;
  end;  
end;

procedure TKMemoTextBlock.ClearSelection(ATextOnly: Boolean);
var
  S: TKString;
begin
  inherited;
  if SelLength <> 0 then
  begin
    S := Text;
    StringDelete(S, FSelStart + 1, FSelEnd - FSelStart);
    FSelEnd := FSelStart;
    Text := S;
  end;
end;

function TKMemoTextBlock.Concat(AItem: TKMemoBlock): Boolean;
begin
  Result := AItem is TKMemoTextBlock;
  if Result then
    InsertString(TKMemoTextBlock(AItem).Text, -1);
end;

function TKMemoTextBlock.ContentLength: Integer;
begin
  Result := FTextLength;
end;

function TKMemoTextBlock.GetCanAddText: Boolean;
begin
  Result := Position = mbpText;
end;

function TKMemoTextBlock.GetKerningDistance(ACanvas: TCanvas; const AChar1, AChar2: TKChar): Integer;
{$IFDEF USE_WINAPI}
var
  Cnt: Integer;
  Pairs: array of TKerningPair;
  C1, C2: WideChar;
  I: Integer;
begin
  Result := 0;
  Cnt := GetKerningPairs(ACanvas.Handle, 0, nil);
  if Cnt > 0 then
  begin
    SetLength(Pairs, Cnt);
    GetKerningPairs(ACanvas.Handle, Cnt, PKerningPairs(@Pairs[0]));
    C1 := NativeUTFToUnicode(AChar1);
    C2 := NativeUTFToUnicode(AChar2);
    for I := 0 to Cnt - 1 do
      if (Pairs[I].wFirst = Ord(C1)) and (Pairs[I].wSecond = Ord(C2)) then
      begin
        if Pairs[I].iKernAmount <> 0 then
        begin
          Result := Pairs[I].iKernAmount;
        end;
        Exit;
      end;
  end;
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}

function TKMemoTextBlock.GetSelText: TKString;
begin
  Result := StringCopy(Text, FSelStart + 1, FSelEnd - FSelStart);
end;

function TKMemoTextBlock.GetText: TKString;
begin
  Result := FText;
end;

function TKMemoTextBlock.GetWordBaseLine(Index: Integer): Integer;
begin
  Result := FWords[Index].BaseLine;
end;

function TKMemoTextBlock.GetWordBottomPadding(Index: Integer): Integer;
begin
  Result := FWords[Index].BottomPadding;
end;

function TKMemoTextBlock.GetWordClipped(Index: Integer): Boolean;
begin
  Result := FWords[Index].Clipped;
end;

function TKMemoTextBlock.GetWordCount: Integer;
begin
  Result := FWordCount;
end;

function TKMemoTextBlock.GetWordHeight(Index: Integer): Integer;
begin
  Result := FWords[Index].Extent.Y;
end;

procedure TKMemoTextBlock.GetWordIndexes(AIndex: Integer; out ASt,
  AEn: Integer);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FWordCount - 1 do
    if (AIndex >= FWords[I].StartIndex) and (AIndex < FWords[I].EndIndex) then
    begin
      ASt := FWords[I].StartIndex;
      AEn := FWords[I].EndIndex;
      Break;
    end;
end;

function TKMemoTextBlock.GetWordLeft(Index: Integer): Integer;
begin
  Result := FWords[Index].Position.X;
end;

function TKMemoTextBlock.GetWordLength(Index: Integer): Integer;
begin
  Result := FWords[Index].EndIndex - FWords[Index].StartIndex + 1;
end;

function TKMemoTextBlock.GetWordLengthWOWS(Index: Integer): Integer;
var
  I: Integer;
  S: TKString;
begin
  Result := GetWordLength(Index);
  S := Words[Index];
  I := Length(S);
  while (I >= 1) and CharInSetEx(S[I], [cTAB] + Wordbreaks) do
  begin
    Dec(Result);
    Dec(I);
  end;
end;

function TKMemoTextBlock.GetWordBoundsRect(Index: Integer): TRect;
begin
  Result.TopLeft := CreateEmptyPoint;
  Result.BottomRight := FWords[Index].Extent;
  KFunctions.OffsetRect(Result, FWords[Index].Position);
end;

function TKMemoTextBlock.GetWordBreakable(Index: Integer): Boolean;
var
  S: TKString;
begin
  S := Words[Index];
  if S <> '' then
    Result := CharInSetEx(S[Length(S)], [cTAB] + Wordbreaks)
  else
    Result := True;
end;

function TKMemoTextBlock.GetWordBreaks: TKSysCharSet;
var
  Notifier: IKMemoNotifier;
begin
  Notifier := MemoNotifier;
  if Notifier <> nil then
    Result := Notifier.GetWordBreaks
  else
    Result := cDefaultWordBreaks;
end;

function TKMemoTextBlock.GetWords(Index: Integer): TKString;
begin
  Result := StringCopy(Text, FWords[Index].StartIndex + 1, FWords[Index].EndIndex - FWords[Index].StartIndex + 1);
end;

function TKMemoTextBlock.GetWordTop(Index: Integer): Integer;
begin
  Result := FWords[Index].Position.Y;
end;

function TKMemoTextBlock.GetWordTopPadding(Index: Integer): Integer;
begin
  Result := FWords[Index].TopPadding;
end;

function TKMemoTextBlock.GetWordWidth(Index: Integer): Integer;
begin
  Result := FWords[Index].Extent.X;
end;

function TKMemoTextBlock.IndexToTextIndex(const AText: TKString; AIndex: Integer): Integer;
begin
  AIndex := MinMax(AIndex, 0, ContentLength);
  Result := StrCPIndexToByteIndex(AText, AIndex);
end;

function TKMemoTextBlock.InsertString(const AText: TKString; At: Integer): Boolean;
var
  S, T, Part1, Part2: TKString;
begin
  Result := False;
  S := Text;
  if At >= 0 then
  begin
    SplitText(S, At + 1, Part1, Part2);
    T := Part1 + AText + Part2;
  end
  else
    T := S + AText;
  if T <> S then
  begin
    Text := T;
    Result := True;
  end;
end;

function TKMemoTextBlock.InternalTextExtent(ACanvas: TCanvas; const AText: TKString): TSize;
begin
  Result := TKTextBox.TextExtent(ACanvas, AText, 1, Length(AText));
end;

procedure TKMemoTextBlock.InternalTextOutput(ACanvas: TCanvas; ALeft, ATop: Integer; const AText: TKString);
begin
  TKTextBox.TextOutput(ACanvas, ALeft, ATop, AText, 1, Length(AText));
end;

function TKMemoTextBlock.ModifiedTextExtent(ACanvas: TCanvas; const AText: TKString): TPoint;
var
  Size: TSize;
  C, CU, SU: TKString;
  I, SmallFontHeight, X, Y: Integer;
begin
  if Pos(cTab, AText) <> 0 then
  begin
    SU := UnicodeStringReplace(AText, cTab, TabChar, [rfReplaceAll]);
    Size := InternalTextExtent(ACanvas, SU);
    Result := Point(Size.cx, Size.cy);
  end
  else if FTextStyle.Capitals = tcaNone then
  begin
    Size := InternalTextExtent(ACanvas, AText);
    Result := Point(Size.cx, Size.cy);
  end else
  begin
    SU := UnicodeUpperCase(AText);
    if FTextStyle.Capitals = tcaNormal then
    begin
      Size := InternalTextExtent(ACanvas, SU);
      Result := Point(Size.cx, Size.cy);
    end else
    begin
      SmallFontHeight := MulDiv(FScriptFontHeight, 4, 5);
      X := 0; Y := 0;
      for I := 1 to StringLength(SU) do
      begin
        C := StringCopy(AText, I, 1);
        CU := StringCopy(SU, I, 1);
        if C <> CU then
          ACanvas.Font.Height := SmallFontheight
        else
          ACanvas.Font.Height := FScriptFontheight;
        Size := InternalTextExtent(ACanvas, CU);
        Inc(X, Size.cx);
        Y := Max(Y, Size.cy);
      end;
      Result := Point(X, Y);
    end;
  end;
end;

procedure TKMemoTextBlock.NotifyDefaultTextChange;
begin
  FTextStyle.NotifyChange(GetDefaultTextStyle);
end;

procedure TKMemoTextBlock.NotifyOptionsChange;
begin
  inherited;
  UpdateWords;
end;

procedure TKMemoTextBlock.NotifyPrintBegin;
begin
  inherited;
  UpdateWords; // update words to take each character separately
end;

procedure TKMemoTextBlock.NotifyPrintEnd;
begin
  inherited;
  UpdateWords; // restore words
end;

procedure TKMemoTextBlock.ParentChanged;
begin
  inherited;
  NotifyDefaultTextChange;
end;

procedure TKMemoTextBlock.SetText(const Value: TKString);
begin
  if FText <> Value then
  begin
    FText := Value;
    FTextLength := StringLength(Value);
    UpdateWords;
    Update([muContent]);
  end;
end;

procedure TKMemoTextBlock.SetWordBaseLine(Index: Integer; const Value: Integer);
begin
  FWords[Index].BaseLine := Value;
end;

procedure TKMemoTextBlock.SetWordBottomPadding(Index: Integer; const Value: Integer);
begin
  FWords[Index].BottomPadding := Value;
end;

procedure TKMemoTextBlock.SetWordClipped(Index: Integer; const Value: Boolean);
begin
  FWords[Index].Clipped := Value;
end;

procedure TKMemoTextBlock.SetWordHeight(Index: Integer; const Value: Integer);
var
  P: TPoint;
begin
  P := FWords[Index].Extent;
  P.Y := Value;
  FWords[Index].Extent := P;
end;

procedure TKMemoTextBlock.SetWordLeft(Index: Integer; const Value: Integer);
var
  P: TPoint;
begin
  P := FWords[Index].Position;
  P.X := Value;
  FWords[Index].Position := P;
end;

procedure TKMemoTextBlock.SetWordTop(Index: Integer; const Value: Integer);
var
  P: TPoint;
begin
  P := FWords[Index].Position;
  P.Y := Value;
  FWords[Index].Position := P;
end;

procedure TKMemoTextBlock.SetWordTopPadding(Index: Integer; const Value: Integer);
begin
  FWords[Index].TopPadding := Value;
end;

procedure TKMemoTextBlock.SetWordWidth(Index: Integer; const Value: Integer);
var
  P: TPoint;
begin
  P := FWords[Index].Extent;
  P.X := Value;
  FWords[Index].Extent := P;
end;

function TKMemoTextBlock.Split(At: Integer; AllowEmpty: Boolean): TKMemoBlock;
var
  Item: TKMemoTextBlock;
  S, Part1, Part2: TKString;
  Cls: TKMemoBlockClass;
begin
  if ((At > 0) or AllowEmpty and (At = 0)) and (At < ContentLength) then
  begin
    Cls := TKMemoBlockClass(Self.ClassType);
    Item := Cls.Create as TKMemoTextBlock;
    Item.Assign(Self);
    S := GetText;
    SplitText(S, At + 1, Part1, Part2);
    Text := Part1;
    Item.Text := Part2;
    Result := Item;
  end else
    Result := nil;
end;

class procedure TKMemoTextBlock.SplitText(const ASource: TKString; At: Integer; out APart1, APart2: TKString);
begin
  APart1 := StringCopy(ASource, 1, At - 1);
  APart2 := StringCopy(ASource, At, Length(ASource) - At + 1);
end;

function TKMemoTextBlock.TextIndexToIndex(var AText: TKString; ATextIndex: Integer): Integer;
begin
  if ATextIndex >= 0 then
    Result := StrByteIndexToCPIndex(AText, ATextIndex)
  else
    Result := -1;
end;

procedure TKMemoTextBlock.TextStyleChanged(Sender: TObject);
begin
  Update([muExtent]);
end;

procedure TKMemoTextBlock.UpdateWords;

  procedure AddWord(AStart, AEnd: Integer);
  var
    Word: TKMemoWord;
  begin
    if WordCount < FWords.Count then
    begin
      Word := FWords[WordCount];
      Word.Clear;
    end else
    begin
      Word := TKMemoWord.Create;
      FWords.Add(Word);
    end;
    Word.StartIndex := AStart - 1;
    Word.EndIndex := AEnd - 1;
    Inc(FWordCount);
  end;

var
  Index, PrevIndex, CharIndex: Integer;
  WasBreak, IsTab, WasTab, SingleChars: Boolean;
begin
  FWordCount := 0;
  if FText <> '' then
  begin
    CharIndex := 1;
    Index := 1;
    PrevIndex := 1;
    IsTab := False;
    WasBreak := False;
    SingleChars := DrawSingleChars;
    while Index <= FTextLength do
    begin
      if SingleChars then
        AddWord(Index, Index)
      else
      begin
        if CharInSetEx(FText[CharIndex], WordBreaks) then
          WasBreak := True
        else
        begin
          WasTab := IsTab;
          IsTab := CharInSetEx(FText[CharIndex], [cTab]);
          if WasBreak or (WasTab and not IsTab) or (IsTab and not WasTab) then
          begin
            AddWord(PrevIndex, Index - 1);
            PrevIndex := Index;
            WasBreak := False;
          end;
        end;
      end;
      Inc(Index);
      CharIndex := StrNextCharIndex(FText, CharIndex);
    end;
    if not SingleChars and (Index > PrevIndex) then
      AddWord(PrevIndex, Index - 1);
  end;
end;

function TKMemoTextBlock.WordIndexToRect(ACanvas: TCanvas; AWordIndex,
  AIndex: Integer; ACaret: Boolean): TRect;
var
  BaseLine, Y, DY: Integer;
  S, T: TKString;
  Ofs, Size: TPoint;
  Word: TKMemoWord;
begin
  Word := FWords[AWordIndex];
  if (AIndex >= 0) and (AIndex <= WordLength[AWordIndex]) then
  begin
    S := StringCopy(FText, Word.StartIndex + 1, AIndex);
    T := StringCopy(FText, Word.StartIndex + AIndex + 1, 1);
    ApplyTextStyle(ACanvas);
    with ACanvas do
    begin
      Ofs := ModifiedTextExtent(ACanvas, S);
      Size := ModifiedTextExtent(ACanvas, T);
    end;
    if ACaret then
    begin
      BaseLine := GetFontAscent(ACanvas.Handle);
      Y := Word.Position.Y + Word.TopPadding + Word.BaseLine - BaseLine - FScriptVertOffset;
      DY := Size.Y;
    end else
    begin
      Y := Word.Position.Y;
      DY := Word.Extent.Y;
    end;
    Result := Rect(Word.Position.X + Ofs.X, Y, Word.Position.X + Ofs.X + Size.X, Y + DY);
  end else
    Result := CreateEmptyRect;
end;

function TKMemoTextBlock.WordMeasureExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint;
var
  S: TKString;
begin
  S := Words[AIndex];
  with ACanvas do
  begin
    ApplyTextStyle(ACanvas);
    FWords[AIndex].Extent := ModifiedTextExtent(ACanvas, S);
    Result := FWords[AIndex].Extent;
  end;
end;

function TKMemoTextBlock.WordMouseAction(ACanvas: TCanvas; AWordIndex: Integer;
  AAction: TKMemoMouseAction; const APoint: TPoint;
  AShift: TShiftState): Boolean;
var
  R, R1, R2: TRect;
  Word: TKMemoWord;
  Notifier: IKMemoNotifier;
begin
  Result := False;
  Word := FWords[AWordIndex];
  if (SelLength > 0) and (SelStart <= Word.EndIndex) and (SelEnd > Word.StartIndex) then
  begin
    Notifier := MemoNotifier;
    if Notifier <> nil then
    begin
      case AAction of
        maMove:
        begin
          R1 := WordIndexToRect(ACanvas, AWordIndex, Max(SelStart, Word.StartIndex) - Word.StartIndex, False);
          R2 := WordIndexToRect(ACanvas, AWordIndex, Min(SelEnd - 1, Word.EndIndex) - Word.StartIndex, False);
          UnionRect(R, R1, R2);
          if PtInRect(R, APoint) then
          begin
            Notifier.SetReqMouseCursor(crDefault);
            Result := True;
          end;
        end;
      end;
    end;
  end;
end;

procedure TKMemoTextBlock.WordPaintToCanvas(ACanvas: TCanvas;
  AWordIndex: Integer; ALeft, ATop: Integer);

  function AdjustBaseLine(ABaseLine: Integer): Integer;
  begin
    Dec(ABaseline, GetFontAscent(ACanvas.Handle) + FScriptVertOffset);
    Result := ABaseLine;
  end;

  procedure TextDraw(const ARect: TRect; ABaseLine: Integer; const AText: TKString);
  var
    C, CU, SU: TKString;
    AdjBaseLine, I, SmallFontHeight, X: Integer;
    Size: TSize;
  begin
    with ACanvas do
    begin
      if Brush.Style <> bsClear then
        DrawFilledRectangle(ACanvas, ARect, clNone);
      SetBkMode(Handle, TRANSPARENT);
      AdjBaseLine := AdjustBaseLine(ABaseLine); // align to baseline
      if (Pos(cTab, AText) <> 0) and ShowFormatting then
      begin
        SU := UnicodeStringReplace(AText, cTab, TabChar, [rfReplaceAll]);
        InternalTextOutput(ACanvas, ARect.Left, AdjBaseLine, SU);
      end
      else if FTextStyle.Capitals = tcaNone then
      begin
        InternalTextOutput(ACanvas, ARect.Left, AdjBaseLine, AText);
      end else
      begin
        SU := UnicodeUpperCase(AText);
        if FTextStyle.Capitals = tcaNormal then
          InternalTextOutput(ACanvas, ARect.Left, AdjBaseLine, SU)
        else
        begin
          SmallFontHeight := MulDiv(FScriptFontHeight, 4, 5);
          X := ARect.Left;
          for I := 1 to StringLength(SU) do
          begin
            C := StringCopy(AText, I, 1);
            CU := StringCopy(SU, I, 1);
            if C <> CU then
              Font.Height := SmallFontHeight
            else
              Font.Height := FScriptFontHeight;
            AdjBaseLine := AdjustBaseLine(ABaseLine);
            InternalTextOutput(ACanvas, X, AdjBaseLine, CU);
            Size := InternalTextExtent(ACanvas, CU);
            Inc(X, Size.cx);
          end;
        end;
      end;
    end;
  end;

var
  W, X, Y, BaseLine: Integer;
  S, T, Part1, Part2, Part3: TKString;
  R, RClip: TRect;
  Word: TKMemoWord;
  Color, Bkgnd: TColor;
  MainClipRgn: HRGN;
  SaveIndex: Integer;
begin
  with ACanvas do
  begin
    ApplyTextStyle(ACanvas);
    S := ApplyFormatting(Words[AWordIndex]);
    Word := FWords[AWordIndex];
    X := Word.Position.X + ALeft + RealLeftOffset;
    Y := Word.Position.Y + ATop + RealTopOffset;
    BaseLine := Y + Word.TopPadding;
    if Position = mbpText then
      Inc(BaseLine, Word.BaseLine)
    else
      Inc(BaseLine, Word.Extent.Y);
    if PaintSelection and (FSelEnd > FSelStart) and (Word.EndIndex >= FSelStart) and (Word.StartIndex < FSelEnd) then
    begin
      GetSelColors(Color, BkGnd);
      if FSelStart > Word.StartIndex then
      begin
        W := FSelStart - Word.StartIndex;
        SplitText(S, W + 1, Part1, T);
      end else
      begin
        W := 0;
        T := S;
      end;
      SplitText(T, FSelEnd - Word.StartIndex - W + 1, Part2, Part3);
      if Part1 <> '' then
      begin
        W := ModifiedTextExtent(ACanvas, Part1).X;
        R := Rect(X, Y + Word.TopPadding, X + W, Y + Word.Extent.Y - Word.BottomPadding);
        TextDraw(R, BaseLine, Part1);
        Inc(X, W);
      end;
      if Part2 <> '' then
      begin
        Brush.Style := bsSolid;
        Brush.Color := Bkgnd;
        Font.Color := Color;
        W := ModifiedTextExtent(ACanvas, Part2).X;
        R := Rect(X, Y, X + W, Y + Word.Extent.Y);
        TextDraw(R, BaseLine, Part2);
        Inc(X, W);
      end;
      if Part3 <> '' then
      begin
        ApplyTextStyle(ACanvas);
        W := ModifiedTextExtent(ACanvas, Part3).X;
        R := Rect(X, Y + Word.TopPadding, X + W, Y + Word.Extent.Y - Word.BottomPadding);
        TextDraw(R, BaseLine, Part3);
      end;
    end else
    begin
      R := Rect(X, Y + Word.TopPadding, X + Word.Extent.X, Y + Word.Extent.Y - Word.BottomPadding);
      if FWords[AWordIndex].Clipped then
      begin
        SaveIndex := SaveDC(ACanvas.Handle);
        MainClipRgn := CreateEmptyRgn;
        try
          RClip := R;
          TranslateRectToDevice(ACanvas.Handle, RClip);
          if GetClipRgn(ACanvas.Handle, MainClipRgn) <> 1 then
          begin
            DeleteObject(MainClipRgn);
            MainClipRgn := CreateRectRgnIndirect(RClip);
          end;
          if ExtSelectClipRect(ACanvas.Handle, RClip, RGN_AND, MainClipRgn) then
            TextDraw(R, BaseLine, S);
        finally
          RgnSelectAndDelete(ACanvas.Handle, MainClipRgn);
          RestoreDC(ACanvas.Handle, SaveIndex);
        end;
        ACanvas.Refresh;
      end else
        TextDraw(R, BaseLine, S);
    end;
  end;
end;

function TKMemoTextBlock.WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint;
  AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer;
var
  I, WPos: Integer;
  S: TKString;
  Size: TPoint;
  R: TRect;
  Word: TKMemoWord;
begin
  Result := -1;
  Word := FWords[AWordIndex];
  R := Rect(Word.Position.X, Word.Position.Y, Word.Position.X + Word.Extent.X, Word.Position.Y + Word.Extent.Y);
  with ACanvas do
  begin
    if PtInRect(R, APoint) or (AOutOfArea and (APoint.X >= R.Left) and (APoint.X < R.Right)) then
    begin
      ApplyTextStyle(ACanvas);
      WPos := Word.Position.X;
      for I := Word.StartIndex to Word.EndIndex do
      begin
        S := ApplyFormatting(StringCopy(FText, I + 1, 1));
        Size := ModifiedTextExtent(ACanvas, S);
        R := Rect(WPos, Word.Position.Y, WPos + Size.X, Word.Position.Y + Word.Extent.Y);
        if PtInRect(R, APoint) or (AOutOfArea and (APoint.X >= R.Left) and (APoint.X < R.Right)) then
        begin
          Result := I - Word.StartIndex;
          Break;
        end;
        Inc(WPos, Size.X);
      end;
    end;
  end;
end;

{ TKMemoHyperlink }

constructor TKMemoHyperlink.Create;
begin
  inherited;
  FMouseCaptureWord := -1;
  FURL := '';
  FOnClick := nil;
  FOnDblClick := nil;
  DefaultStyle;
end;

procedure TKMemoHyperlink.Assign(ASource: TKObject);
begin
  inherited;
  if ASource is TKMemoHyperlink then
  begin
    FURL := TKMemoHyperlink(ASource).URL;
    FOnClick := TKMemoHyperlink(ASource).OnClick;
    FOnDblClick := TKMemoHyperlink(ASource).OnDblClick;
  end;
end;

procedure TKMemoHyperlink.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TKMemoHyperlink.DblClick;
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TKMemoHyperlink.DefaultStyle;
begin
  FTextStyle.Font.Color := clBlue;
  FTextStyle.Font.Style := FTextStyle.Font.Style + [fsUnderline];
end;

function TKMemoHyperlink.WordMouseAction(ACanvas: TCanvas; AWordIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean;
var
  R: TRect;
  Word: TKMemoWord;
  Notifier: IKMemoNotifier;
begin
  Result := inherited WordMouseAction(ACanvas, AWordIndex, AAction, APoint, AShift);
  if not Result then
  begin
    Word := FWords[AWordIndex];
    R := Rect(Word.Position.X, Word.Position.Y, Word.Position.X + Word.Extent.X, Word.Position.Y + Word.Extent.Y);
    if PtInRect(R, APoint) then
    begin
      Notifier := MemoNotifier;
      if Notifier <> nil then
      begin
        case AAction of
          maMove:
          begin
            if (ssCtrl in AShift) or ReadOnly then
              Notifier.SetReqMouseCursor(crHandPoint);
            Result := True;
          end;
          maLeftDown:
          begin
            FMouseCaptureWord := -1;
            if (ssCtrl in AShift) or ReadOnly then
            begin
              FMouseCaptureWord := AWordIndex;
              Result := True;
            end;
          end;
          maLeftUp:
          begin
            if FMouseCaptureWord >= 0 then
            begin
              FMouseCaptureWord := -1;
              if Assigned(FOnDblClick) and (ssDouble in AShift) then
              begin
                DblClick;
                Result := True;
              end
              else if Assigned(FOnClick) then
              begin
                Click;
                Result := True;
              end
              else if FURL <> '' then
              begin
                OpenURLWithShell(FURL);
                Result := True;
              end;
            end;
          end;
        end;
      end;
    end else
    begin
      case AAction of
        maLeftUp:
          if FMouseCaptureWord = AWordIndex then
            FMouseCaptureWord := -1;
      end;
    end;
  end;
end;

{ TKParagraph }

constructor TKMemoParagraph.Create;
begin
  inherited;
  FExtent := CreateEmptyPoint;
  FTextStyle.Changeable := False;
  try
    FTextStyle.AllowBrush := False;
  finally
    FTextStyle.Changeable := True;
  end;
  FNumberBlock := nil;
  FParaStyle := TKMemoParaStyle.Create;
  FParaStyle.OnChanged := ParaStyleChanged;
  FOrigin := CreateEmptyPoint;
  Text := NewLineChar;
end;

destructor TKMemoParagraph.Destroy;
begin
  FNumberBlock.Free;
  FParaStyle.Free;
  inherited;
end;

procedure TKMemoParagraph.AssignAttributes(AItem: TKMemoBlock);
begin
  inherited;
  if AItem is TKMemoParagraph then
    FParaStyle.Assign(TKMemoParagraph(AItem).ParaStyle);
end;

procedure TKMemoParagraph.ParaStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
begin
  Update(AReasons);
end;

function TKMemoParagraph.Concat(AItem: TKMemoBlock): Boolean;
begin
  Result := False;
end;

function TKMemoParagraph.GetCanAddText: Boolean;
begin
  Result := False;
end;

function TKMemoParagraph.GetNumberBlock: TKMemoTextBlock;
var
  ListTable: TKMemoListTable;
  Notifier: IKMemoNotifier;
begin
  Notifier := MemoNotifier;
  if Notifier <> nil then
  begin
    ListTable := Notifier.GetListTable;
    if ListTable.FindByID(FParaStyle.NumberingList) <> nil then
    begin
      if FNumberBlock = nil then
      begin
        FNumberBlock := TKMemoTextBlock.Create;
        FNumberBlock.Parent := Parent; // because of FMemoNotifier
      end;
    end else
      FreeAndNil(FNumberBlock);
  end else
    FreeAndNil(FNumberBlock);
  Result := FNumberBlock;
end;

function TKMemoParagraph.GetNumbering: TKMemoParaNumbering;
var
  ListLevel: TKMemoListLevel;
begin
  ListLevel := GetNumberingListLevel;
  if ListLevel <> nil then
    Result := ListLevel.Numbering
  else
    Result := pnuNone;
end;

function TKMemoParagraph.GetNumberingList: TKMemoList;
var
  ListTable: TKMemoListTable;
  Notifier: IKMemoNotifier;
begin
  Result := nil;
  if FParaStyle.NumberingList <> cInvalidListID then
  begin
    Notifier := MemoNotifier;
    if Notifier <> nil then
    begin
      ListTable := Notifier.GetListTable;
      Result := ListTable.FindByID(FParaStyle.NumberingList);
    end;
  end;
end;

function TKMemoParagraph.GetNumberingListLevel: TKMemoListLevel;
var
  List: TKMemoList;
begin
  List := GetNumberingList;
  if List <> nil then
    Result := List.Levels[FParaStyle.NumberingListLevel]
  else
    Result := nil;
end;

function TKMemoParagraph.GetParaStyle: TKMemoParaStyle;
begin
  Result := FParaStyle;
end;

function TKMemoParagraph.GetWordBreakable(Index: Integer): Boolean;
begin
  Result := True;
end;

procedure TKMemoParagraph.NotifyDefaultParaChange;
begin
  FParaStyle.NotifyChange(GetDefaultParaStyle);
end;

procedure TKMemoParagraph.SetNumbering(const Value: TKMemoParaNumbering);
var
  List: TKMemoList;
  ListLevel: TKMemoListLevel;
  ListTable: TKMemoListTable;
  Notifier: IKMemoNotifier;
  LevelIndex: Integer;
begin
  if Value <> GetNumbering then
  begin
    // here we try to set best numbering match from list table
    Notifier := MemoNotifier;
    if Notifier <> nil then
    begin
      ListTable := Notifier.GetListTable;
      LevelIndex := Max(FParaStyle.NumberingListLevel, 0);
      List := ListTable.ListByNumbering(FParaStyle.FNumberingList, LevelIndex, Value);
      ListLevel := List.Levels[LevelIndex];
      FParaStyle.NumberingList := List.ID;
      FParaStyle.NumberStartAt := 0;
      FParaStyle.FirstIndent := ListLevel.FirstIndent;
      FParaStyle.LeftPadding := ListLevel.LeftIndent;
    end;
  end;
end;

function TKMemoParagraph.Split(At: Integer; AllowEmpty: Boolean): TKMemoBlock;
begin
  Result := nil;
end;

procedure TKMemoParagraph.WordPaintToCanvas(ACanvas: TCanvas; AWordIndex, ALeft, ATop: Integer);
begin
  inherited;
end;

{ TKImageMemoBlock }

constructor TKMemoImageBlock.Create;
begin
  inherited;
  FBaseLine := 0;
  FWordBottomPadding := 0;
  FCreatingCroppedImage := False;
  FCrop := TKRect.Create;
  FCrop.OnChanged := CropChanged;
  FCalcBaseLine := 0;
  FExtent := CreateEmptyPoint;
  FImage := TPicture.Create;
  FImage.OnChange := ImageChanged;
  FImageStyle := TKMemoBlockStyle.Create;
  FImageStyle.ContentMargin.All := 5;
  FImageStyle.OnChanged := ImageStyleChanged;
  FMouseCapture := False;
  FExplicitExtent := CreateEmptyPoint;
  FOrigin := CreateEmptyPoint;
  FScale := Point(100, 100);
  FCroppedImage := nil;
  FWordTopPadding := 0;
end;

destructor TKMemoImageBlock.Destroy;
begin
  FCrop.Free;
  FImageStyle.Free;
  FImage.Free;
  FCroppedImage.Free;
  inherited;
end;

procedure TKMemoImageBlock.Assign(ASource: TKObject);
begin
  inherited;
  if ASource is TKMemoImageBlock then
    Image.Assign(TKMemoImageBlock(ASource).Image);
end;

procedure TKMemoImageBlock.AssignAttributes(AItem: TKMemoBlock);
begin
  inherited;
  if AItem is TKMemoImageBlock then
  begin
    Crop.Assign(TKMemoImageBlock(AItem).Crop);
    ImageStyle.Assign(TKMemoImageBlock(AItem).ImageStyle);
    ExplicitWidth := TKMemoImageBlock(AItem).ExplicitWidth;
    ExplicitHeight := TKMemoImageBlock(AItem).ExplicitHeight;
    ScaleX := TKMemoImageBlock(AItem).ScaleX;
    ScaleY := TKMemoImageBlock(AItem).ScaleY;
  end;
end;

function TKMemoImageBlock.CalcAscent(ACanvas: TCanvas): Integer;
var
  Item: TKMemoBlock;
  Index, Ascent, Descent: Integer;
begin
  if (Parent <> nil) and (Position = mbpText) then
  begin
    Result := FExtent.Y div 2;
    Index := ParentBlocks.IndexOf(Self);
    if Index >= 0 then
    begin
      Item := ParentBlocks.GetLastItemByClass(Index, TKMemoTextBlock);
      if Item = nil then
        Item := ParentBlocks.GetNextItemByClass(Index, TKMemoTextBlock);
      if Item <> nil then
      begin
        Ascent := TKMemoTextBlock(Item).CalcAscent(ACanvas);
        Descent := TKMemoTextBlock(Item).CalcDescent(ACanvas);
        Result := (FExtent.Y - (Ascent + Descent)) div 2 + Ascent;
      end else
      begin
        Item := ParentBlocks.GetNearestParagraphItem(Index);
        if Item <> nil then
        begin
          Descent := TKMemoTextBlock(Item).CalcDescent(ACanvas);
          Result := FExtent.Y - Descent;
        end;
      end;
    end;
  end else
    Result := 0;
  FCalcBaseLine := Result;
end;

function TKMemoImageBlock.ContentLength: Integer;
begin
  Result := 1;
end;

function TKMemoImageBlock.GetWrapMode: TKMemoBlockWrapMode;
begin
  Result := FImageStyle.WrapMode;
end;

function TKMemoImageBlock.GetImageHeight: Integer;
begin
  if FScale.Y <> 0 then
    Result := ScaleHeight
  else
    Result := FImage.Height;
  Dec(Result, FCrop.Top + FCrop.Bottom);
end;

function TKMemoImageBlock.GetImageWidth: Integer;
begin
  if FScale.X <> 0 then
    Result := ScaleWidth
  else
    Result := FImage.Width;
  Dec(Result, FCrop.Left + FCrop.Right);
end;

function TKMemoImageBlock.GetNativeOrExplicitHeight: Integer;
begin
  if FExplicitExtent.Y <> 0 then
    Result := FExplicitExtent.Y
  else
    Result := FImage.Height;
end;

function TKMemoImageBlock.GetNativeOrExplicitWidth: Integer;
begin
  if FExplicitExtent.X <> 0 then
    Result := FExplicitExtent.X
  else
    Result := FImage.Width;
end;

function TKMemoImageBlock.GetScaleHeight: Integer;
begin
  Result := MulDiv(NativeOrExplicitHeight, FScale.Y, 100);
end;

function TKMemoImageBlock.GetScaleWidth: Integer;
begin
  Result := MulDiv(NativeOrExplicitWidth, FScale.X, 100);
end;

function TKMemoImageBlock.GetSizingRect: TRect;
var
  ROuter: TRect;
begin
  ROuter := OuterRect(False);
  CroppedImage;
  Result := FScaledRect;
  KFunctions.OffsetRect(Result, ROuter.Left + FImageStyle.AllPaddingsLeft, ROuter.Top + FImageStyle.AllPaddingsTop + FWordTopPadding + FBaseLine - FCalcBaseLine);
end;

function TKMemoImageBlock.GetWordBottomPadding(Index: Integer): Integer;
begin
  Result := FWordBottomPadding;
end;

function TKMemoImageBlock.GetWordBoundsRect(Index: Integer): TRect;
begin
  Result.TopLeft := CreateEmptyPoint;
  Result.BottomRight := FExtent;
  KFunctions.OffsetRect(Result, FOrigin);
end;

function TKMemoImageBlock.GetWordCount: Integer;
begin
  Result := 1;
end;

function TKMemoImageBlock.GetWordHeight(Index: Integer): Integer;
begin
  Result := FExtent.Y;
end;

function TKMemoImageBlock.GetWordLeft(Index: Integer): Integer;
begin
  Result := FOrigin.X;
end;

function TKMemoImageBlock.GetWordLength(Index: Integer): Integer;
begin
  Result := 1;
end;

function TKMemoImageBlock.GetWords(Index: Integer): TKString;
begin
  Result := '';
end;

function TKMemoImageBlock.GetWordTop(Index: Integer): Integer;
begin
  Result := FOrigin.Y;
end;

function TKMemoImageBlock.GetWordTopPadding(Index: Integer): Integer;
begin
  Result := FWordTopPadding;
end;

function TKMemoImageBlock.GetWordWidth(Index: Integer): Integer;
begin
  Result := FExtent.X;
end;

procedure TKMemoImageBlock.ImageChanged(Sender: TObject);
begin
  if not FCreatingCroppedImage then
  begin
    FreeAndNil(FCroppedImage);
    Update([muContent]);
  end;
end;

procedure TKMemoImageBlock.ImageStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
begin
  Update(AReasons);
end;

function TKMemoImageBlock.OuterRect(ACaret: Boolean): TRect;
begin
  Result.TopLeft := FOrigin;
  Result.Right := Result.Left + FExtent.X;
  Result.Bottom := Result.Top + FExtent.Y;
  if ACaret then
  begin
    Inc(Result.Top, FWordTopPadding);
    Dec(Result.Bottom, FWordBottomPadding);
  end;
  KFunctions.OffsetRect(Result, RealLeftOffset, RealTopOffset);
end;

procedure TKMemoImageBlock.Resize(ANewWidth, ANewHeight: Integer);
begin
  ScaleWidth := ANewWidth;
  ScaleHeight := ANewHeight;
end;

procedure TKMemoImageBlock.CropChanged(Sender: TObject);
begin
  FreeAndNil(FCroppedImage);
  Update([muExtent]);
end;

function TKMemoImageBlock.CroppedImage: TKAlphaBitmap;
var
  ExtentX, ExtentY, NewExtentX: Integer;
  RatioX, RatioY: Double;
  OrigCrop: TRect;
begin
  if (FCroppedImage = nil) and (FImage.Graphic <> nil) and not FCreatingCroppedImage then
  begin
    FCreatingCroppedImage := True;
    try
      // get scaled image only on demand
      ExtentX := ScaleWidth;
      if ExtentX = 0 then
        ExtentX := FImage.Width;
      ExtentY := ScaleHeight;
      if ExtentY = 0 then
        ExtentY := FImage.Height;
      RatioX := ExtentX / FImage.Width;
      RatioY := ExtentY / FImage.Height;
      // crop in original units
      OrigCrop := Rect(Round(FCrop.Left / RatioX), Round(FCrop.Top / RatioY), Round(FCrop.Right / RatioX), Round(FCrop.Bottom / RatioY));
      // handle desired image size differently with respect to its position mode
      if Position = mbpText then
      begin
        // when image is placed in text it should be adjusted to page width
        NewExtentX := FExtent.X - FImageStyle.AllPaddingsLeft - FImageStyle.AllPaddingsRight;
        ExtentY := Min(MulDiv(NewExtentX, ExtentY, ExtentX),
          FExtent.Y - FImageStyle.AllPaddingsBottom - FImageStyle.AllPaddingsTop - FWordTopPadding - FWordBottomPadding);
        ExtentX := NewExtentX;
      end else
      begin
        // otherwise respect desired size
        Dec(ExtentX, FCrop.Left + FCrop.Right);
        Dec(ExtentY, FCrop.Top + FCrop.Bottom);
      end;
      FScaledRect := Rect(0, 0, ExtentX, ExtentY);
      if (ExtentX * ExtentY <> 0) and (FImage.Width * FImage.Height <> 0) then
      begin
        FCroppedImage := TKAlphaBitmap.Create;
        FCroppedImage.SetSize(FImage.Width - OrigCrop.Left - OrigCrop.Right, FImage.Height - OrigCrop.Top - OrigCrop.Bottom);
        FCroppedImage.DrawFrom(FImage.Graphic, -OrigCrop.Left, -OrigCrop.Top);
      {$IFDEF USE_PNG_SUPPORT}
        if not (FImage.Graphic is TKPngImage) then
      {$ENDIF}
          FCroppedImage.AlphaFill(255);
      end;
    finally
      FCreatingCroppedImage := False;
    end;
  end;
  Result := FCroppedImage;
end;

procedure TKMemoImageBlock.SetCrop(const Value: TKRect);
begin
  FCrop.Assign(Value);
end;

procedure TKMemoImageBlock.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
  FreeAndNil(FCroppedImage);
  Update([muContent]);
end;

procedure TKMemoImageBlock.SetImagePath(const Value: TKString);
begin
  FImage.LoadFromFile(Value);
  FreeAndNil(FCroppedImage);
  Update([muContent]);
end;

procedure TKMemoImageBlock.SetExplicitHeight(const Value: Integer);
begin
  FExplicitExtent.Y := Value;
  FreeAndNil(FCroppedImage);
  Update([muContent]);
end;

procedure TKMemoImageBlock.SetExplicitWidth(const Value: Integer);
begin
  FExplicitExtent.X := Value;
  FreeAndNil(FCroppedImage);
  Update([muContent]);
end;

procedure TKMemoImageBlock.SetScaleHeight(const Value: Integer);
begin
  if Value <> ScaleHeight then
  begin
    FScale.Y := MulDiv(Value, 100, NativeOrExplicitHeight);
    FreeAndNil(FCroppedImage);
    Update([muExtent]);
  end;
end;

procedure TKMemoImageBlock.SetScaleWidth(const Value: Integer);
begin
  if Value <> ScaleWidth then
  begin
    FScale.X := MulDiv(Value, 100, NativeOrExplicitWidth);
    FreeAndNil(FCroppedImage);
    Update([muExtent]);
  end;
end;

procedure TKMemoImageBlock.SetScaleX(const Value: Integer);
begin
  if Value <> FScale.X then
  begin
    FScale.X := Value;
    FreeAndNil(FCroppedImage);
    Update([muExtent]);
  end;
end;

procedure TKMemoImageBlock.SetScaleY(const Value: Integer);
begin
  if Value <> FScale.Y then
  begin
    FScale.Y := Value;
    FreeAndNil(FCroppedImage);
    Update([muExtent]);
  end;
end;

procedure TKMemoImageBlock.SetWordBaseLine(Index: Integer; const Value: Integer);
begin
  FBaseLine := Value;
end;

procedure TKMemoImageBlock.SetWordBottomPadding(Index: Integer; const Value: Integer);
begin
  FWordBottomPadding := Value;
end;

procedure TKMemoImageBlock.SetWordHeight(Index: Integer; const Value: Integer);
begin
  FExtent.Y := Value;
end;

procedure TKMemoImageBlock.SetWordLeft(Index: Integer; const Value: Integer);
begin
  FOrigin.X := Value;
end;

procedure TKMemoImageBlock.SetWordTop(Index: Integer; const Value: Integer);
begin
  FOrigin.Y := Value;
end;

procedure TKMemoImageBlock.SetWordTopPadding(Index: Integer; const Value: Integer);
begin
  FWordTopPadding := Value;
end;

function TKMemoImageBlock.WordIndexToRect(ACanvas: TCanvas; AWordIndex,
  AIndex: Integer; ACaret: Boolean): TRect;
begin
  Result := OuterRect(ACaret);
end;

function TKMemoImageBlock.WordMeasureExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint;
begin
  FreeAndNil(FCroppedImage);
  Result := Point(
    ImageWidth + FImageStyle.AllPaddingsLeft + FImageStyle.AllPaddingsRight,
    ImageHeight + FImageStyle.AllPaddingsTop + FImageStyle.AllPaddingsBottom);
  if (Position = mbpText) and (Result.X > ARequiredWidth) then
  begin
    // when image is placed in text it should be adjusted to page width
    Result.Y := MulDiv(Result.Y, ARequiredWidth, Result.X);
    Result.X := ARequiredWidth;
  end;
  FExtent := Result;
end;

function TKMemoImageBlock.WordMouseAction(ACanvas: TCanvas; AWordIndex: Integer;
  AAction: TKMemoMouseAction; const APoint: TPoint;
  AShift: TShiftState): Boolean;
var
  R: TRect;
  Notifier: IKMemoNotifier;
  Cursor: TCursor;
begin
  Result := False;
  R := GetSizingRect;
  if PtInRect(R, APoint) then
  begin
    Notifier := MemoNotifier;
    if Notifier <> nil then
    begin
      case AAction of
        maMove:
        begin
          if ReadOnly then
            Cursor := crDefault
          else
          begin
            if SelLength > 0 then
            begin
              Cursor := SizingGripsCursor(R, APoint);
              if Cursor = crDefault then
                Cursor := crSizeAll;
            end else
              Cursor := crSizeAll;
            if (Position = mbpText) and (Cursor = crSizeAll) then
              Cursor := crDefault;
          end;
          Notifier.SetReqMouseCursor(Cursor);
          Result := True;
        end;
        maLeftDown:
        begin
          if ssDouble in AShift then
            Notifier.EditBlock(Self)
          else
            Notifier.SelectBlock(Self, SizingGripsPosition(R, APoint));
          Result := True;
        end;
        maRightDown:
        begin
          Result := Notifier.SelectBlock(Self, sgpNone);
        end;
      end;
    end;
  end;
end;

procedure TKMemoImageBlock.WordPaintToCanvas(ACanvas: TCanvas; AIndex, ALeft, ATop: Integer);
var
  X, Y: Integer;
  R: TRect;
  ROuter: TRect;
  Bitmap: TKAlphaBitmap;
  Color, Bkgnd: TColor;
begin
  inherited;
  ROuter := OuterRect(False);
  KFunctions.OffsetRect(ROuter, ALeft, ATop);
  X := ROuter.Left + FImageStyle.AllPaddingsLeft;
  Y := ROuter.Top + FImageStyle.AllPaddingsTop + FWordTopPadding + FBaseLine - FCalcBaseLine;
  CroppedImage;
  R := FScaledRect;
  KFunctions.OffsetRect(R, X, Y);
  ROuter := ImageStyle.MarginRect(ROuter);
  if PaintSelection and (SelLength > 0) then
  begin
    GetSelColors(Color, BkGnd);
    ACanvas.Brush.Color := BkGnd;
    ACanvas.FillRect(ROuter);
    if FCroppedImage <> nil then
    begin
      Bitmap := TKAlphaBitmap.Create;
      try
        Bitmap.SetSize(FCroppedImage.Width, FCroppedImage.Height);
      {$IFDEF FPC}
        Bitmap.UpdateHandle;
      {$ENDIF}
        Bitmap.Canvas.Brush.Color := BkGnd;
        Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
        Bitmap.Canvas.Draw(0, 0, FCroppedImage);
      {$IFDEF FPC}
        Bitmap.UpdatePixels;
      {$ENDIF}
        Bitmap.AlphaFillPercent(50, True);
        ACanvas.StretchDraw(R, Bitmap);
        if not ReadOnly and (SelectedBlock = Self) then
          SizingGripsDraw(ACanvas, R);
      finally
        Bitmap.Free;
      end;
    end;
  end else
  begin
    FImageStyle.PaintBox(ACanvas, ROuter);
    if FCroppedImage <> nil then
      ACanvas.StretchDraw(R, FCroppedImage);
  end;
end;

function TKMemoImageBlock.WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint;
  AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer;
begin
  if PtInRect(OuterRect(False), APoint) then
    Result := 0
  else
    Result := -1;
end;

{ TKMemoContainer }

constructor TKMemoContainer.Create;
begin
  inherited;
  FBlocks := TKMemoBlocks.Create;
  FBlocks.Parent := Self;
  FBlocks.OnUpdate := Update;
  FBlockStyle := TKMemoBlockStyle.Create;
  FBlockStyle.OnChanged := BlockStyleChanged;
  FWordBottomPadding := 0;
  FClip := False;
  FCurrentRequiredHeight := 0;
  FCurrentRequiredWidth := 0;
  FFixedHeight := False;
  FFixedWidth := False;
  FOrigin := CreateEmptyPoint;
  FRequiredHeight := 0;
  FRequiredWidth := 0;
  FWordTopPadding := 0;
end;

destructor TKMemoContainer.Destroy;
begin
  FBlocks.Free;
  FBlockStyle.Free;
  inherited;
end;

function TKMemoContainer.AddRectOffset(const ARect: TRect): TRect;
begin
  Result := ARect;
  KFunctions.OffsetRect(Result,
    Left + RealLeftOffset + FBlockStyle.AllPaddingsLeft,
    Top + RealTopOffset + FBlockStyle.AllPaddingsTop + FWordTopPadding);
end;

procedure TKMemoContainer.AddSingleLine;
var
  Line: TKMemoLine;
begin
  if Position = mbpText then
  begin
    FBlocks.Lines.Clear;
    if FBlocks.Count > 0 then
    begin
      Line := TKMemoLine.Create;
      Line.StartBlock := 0;
      Line.StartWord := 0;
      Line.StartIndex := 0;
      Line.EndBlock := FBlocks.Count - 1;
      Line.EndWord := 0;
      Line.EndIndex := FBlocks.SelectableLength - 1;
      Line.Position := Point(0, 0);
      Line.Extent := Point(Width, Height);
      FBlocks.Lines.Add(Line);
    end;
  end;
end;

procedure TKMemoContainer.Assign(ASource: TKObject);
begin
  inherited;
  if ASource is TKMemoContainer then
    Blocks.Assign(TKMemoContainer(ASource).Blocks);
end;

procedure TKMemoContainer.AssignAttributes(AItem: TKMemoBlock);
begin
  inherited;
  if AItem is TKMemoContainer then
  begin
    LockUpdate;
    try
      BlockStyle.Assign(TKMemoContainer(AItem).BlockStyle);
      Clip := TKMemoContainer(AItem).Clip;
      FixedWidth := TKMemoContainer(AItem).FixedWidth;
      FixedHeight := TKMemoContainer(AItem).FixedHeight;
      RequiredWidth := TKMemoContainer(AItem).RequiredWidth;
      RequiredHeight := TKMemoContainer(AItem).RequiredHeight;
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKMemoContainer.AddBlockLine(AStartBlock, AStartIndex, AEndBlock, AEndIndex, ALeft, ATop, AWidth, AHeight: Integer);
var
  Line: TKMemoLine;
begin
  if Position = mbpText then
  begin
    if AEndBlock >= AStartBlock then
    begin
      Line := TKMemoLine.Create;
      Line.StartBlock := AStartBlock;
      Line.StartWord := 0;
      Line.StartIndex := AStartIndex;
      Line.EndBlock := AEndBlock;
      Line.EndWord := 0;
      Line.EndIndex := AEndIndex;
      Line.Position := Point(ALeft, ATop);
      Line.Extent := Point(AWidth, AHeight);
      FBlocks.Lines.Add(Line);
    end;
  end;
end;

procedure TKMemoContainer.BlockStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
begin
  Update(AReasons);
end;

function TKMemoContainer.CalcAscent(ACanvas: TCanvas): Integer;
var
  PA: TKMemoParagraph;
  ParaDescent: Integer;
begin
  Result := 0;
  if (Parent <> nil) and (Position = mbpText) then
  begin
    PA := ParentBlocks.GetNearestParagraphItem(ParentBlocks.IndexOf(Self));
    if PA <> nil then
    begin
      ParaDescent := PA.CalcDescent(ACanvas);
      Result := FBlocks.Height - ParaDescent;
    end;
  end;
end;

function TKMemoContainer.CanAdd(AItem: TKMemoBlock): Boolean;
begin
  // generic container cannot accept some kinds of subblocks
  Result := not (
    (AItem is TKMemoTableRow) or
    (AItem is TKMemoTableCell)
    );
end;

procedure TKMemoContainer.ClearLines;
begin
  FBlocks.Lines.Clear;
end;

procedure TKMemoContainer.ClearSelection(ATextOnly: Boolean);
begin
  FBlocks.ClearSelection(ATextOnly);
end;

function TKMemoContainer.ContentLength: Integer;
begin
  Result := FBlocks.SelectableLength;
end;

function TKMemoContainer.GetBottomPadding: Integer;
begin
  Result := FBlockStyle.BottomPadding;
end;

function TKMemoContainer.GetCanAddText: Boolean;
begin
  Result := True;
end;

function TKMemoContainer.GetWrapMode: TKMemoBlockWrapMode;
begin
  Result := FBlockStyle.WrapMode;
end;

function TKMemoContainer.GetSelLength: Integer;
begin
  Result := FBlocks.SelLength;
end;

function TKMemoContainer.GetSelStart: Integer;
begin
  Result := FBlocks.SelStart;
end;

function TKMemoContainer.GetSelText: TKString;
begin
  Result := FBlocks.SelText;
end;

function TKMemoContainer.GetText: TKString;
begin
  Result := FBlocks.Text;
end;

function TKMemoContainer.GetTopPadding: Integer;
begin
  Result := FBlockStyle.TopPadding;
end;

function TKMemoContainer.GetTotalLineCount: Integer;
begin
  Result := FBlocks.TotalLineCount;
end;

function TKMemoContainer.GetTotalLineRect(Index: Integer): TRect;
begin
  Result := AddRectOffset(FBlocks.TotalLineRect[Index]);
end;

function TKMemoContainer.GetWordBottomPadding(Index: Integer): Integer;
begin
  Result := FWordBottomPadding;
end;

function TKMemoContainer.GetWordBoundsRect(Index: Integer): TRect;
begin
  Result := Rect(0, 0, Width, Height);
  KFunctions.OffsetRect(Result, FOrigin);
end;

function TKMemoContainer.GetWordCount: Integer;
begin
  Result := 1;
end;

function TKMemoContainer.GetWordHeight(Index: Integer): Integer;
begin
  if FFixedHeight then
    Result := FRequiredHeight
  else
    Result := Max(FCurrentRequiredHeight, FBlocks.Height + FBlockStyle.AllPaddingsBottom + FBlockStyle.AllPaddingsTop);
end;

function TKMemoContainer.GetWordLeft(Index: Integer): Integer;
begin
  Result := FOrigin.X;
end;

function TKMemoContainer.GetWordLength(Index: Integer): Integer;
begin
  Result := ContentLength;
end;

function TKMemoContainer.GetWords(Index: Integer): TKString;
begin
  Result := FBlocks.Text;
end;

function TKMemoContainer.GetWordTop(Index: Integer): Integer;
begin
  Result := FOrigin.Y;
end;

function TKMemoContainer.GetWordTopPadding(Index: Integer): Integer;
begin
  Result := FWordTopPadding;
end;

function TKMemoContainer.GetWordWidth(Index: Integer): Integer;
begin
  if FFixedWidth then
    Result := FRequiredWidth
  else
    Result := Max(FCurrentRequiredWidth, FBlocks.Width + FBlockStyle.AllPaddingsLeft + FBlockStyle.AllPaddingsRight);
end;

function TKMemoContainer.InsertParagraph(AIndex: Integer): Boolean;
begin
  Result := FBlocks.InsertParagraph(AIndex, False);
end;

function TKMemoContainer.InsertString(const AText: TKString; At: Integer): Boolean;
begin
  if At < 0 then
    At := FBlocks.SelectableLength;
  Result := FBlocks.InsertString(At, False, AText);
end;

procedure TKMemoContainer.NotifyDefaultParaChange;
begin
  FBlocks.NotifyDefaultParaChange;
end;

procedure TKMemoContainer.NotifyDefaultTextChange;
begin
  FBlocks.NotifyDefaultTextChange;
end;

procedure TKMemoContainer.NotifyPrintBegin;
begin
  FBlocks.NotifyPrintBegin;
end;

procedure TKMemoContainer.NotifyPrintEnd;
begin
  FBlocks.NotifyPrintEnd;
end;

procedure TKMemoContainer.ParentChanged;
begin
  inherited;
  FBlocks.MemoNotifier := GetMemoNotifier;
end;

procedure TKMemoContainer.RequiredHeightChanged;
begin
end;

procedure TKMemoContainer.RequiredWidthChanged;
begin
end;

procedure TKMemoContainer.Resize(ANewWidth, ANewHeight: Integer);
begin
  FixedWidth := True;
  FixedHeight := True;
  RequiredWidth := ANewWidth;
  RequiredHeight := ANewHeight;
end;

function TKMemoContainer.Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean): Boolean;
begin
  Result := FBlocks.Select(ASelStart, ASelLength, ADoScroll);
end;

procedure TKMemoContainer.SetBlockExtent(AWidth, AHeight: Integer);
begin
  FBlocks.SetExtent(AWidth - FBlockStyle.AllPaddingsLeft - FBlockStyle.AllPaddingsRight,  AHeight - FBlockStyle.AllPaddingsTop - FBlockStyle.AllPaddingsBottom);
end;

procedure TKMemoContainer.SetClip(const Value: Boolean);
begin
  if Value <> FClip then
  begin
    FClip := Value;
    Update([muExtent]);
  end;
end;

procedure TKMemoContainer.SetFixedHeight(const Value: Boolean);
begin
  if Value <> FFixedHeight then
  begin
    FFixedHeight := Value;
    Update([muExtent]);
  end;
end;

procedure TKMemoContainer.SetFixedWidth(const Value: Boolean);
begin
  if Value <> FFixedWidth then
  begin
    FFixedWidth := Value;
    Update([muExtent]);
  end;
end;

procedure TKMemoContainer.SetRequiredHeight(const Value: Integer);
begin
  if Value <> FRequiredHeight then
  begin
    FRequiredHeight := Value;
    RequiredHeightChanged;
    Update([muExtent]);
  end;
end;

procedure TKMemoContainer.SetRequiredWidth(const Value: Integer);
begin
  if Value <> FRequiredWidth then
  begin
    FRequiredWidth := Value;
    RequiredWidthChanged;
    Update([muExtent]);
  end;
end;

procedure TKMemoContainer.SetWordBottomPadding(Index: Integer; const Value: Integer);
begin
  FWordBottomPadding := Value;
end;

procedure TKMemoContainer.SetWordHeight(Index: Integer; const Value: Integer);
begin
  FCurrentRequiredHeight := Value;
end;

procedure TKMemoContainer.SetWordLeft(Index: Integer; const Value: Integer);
begin
  FOrigin.X := Value;
end;

procedure TKMemoContainer.SetWordTop(Index: Integer; const Value: Integer);
begin
  FOrigin.Y := Value;
end;

procedure TKMemoContainer.SetWordTopPadding(Index: Integer; const Value: Integer);
begin
  FWordTopPadding := Value;
end;

procedure TKMemoContainer.SetWordWidth(Index: Integer; const Value: Integer);
begin
  FCurrentRequiredWidth := Value;
end;

procedure TKMemoContainer.UpdateAttributes;
begin
  FBlocks.UpdateAttributes;
end;

function TKMemoContainer.WordIndexToRect(ACanvas: TCanvas; AWordIndex, AIndex: Integer; ACaret: Boolean): TRect;
begin
  Result := FBlocks.IndexToRect(ACanvas, AIndex, ACaret, False);
  if not ACaret then
  begin
    // expand rect to enable vertical caret movement
    if Result.Top = 0 then
      Dec(Result.Top, FBlockStyle.AllPaddingsTop + FWordTopPadding);
    if Result.Bottom = FBlocks.Height then
      Inc(Result.Bottom, Height - FBlocks.Height - FBlockStyle.AllPaddingsTop - FWordTopPadding);
  end;
  Result := AddRectOffset(Result);
end;

function TKMemoContainer.WordMeasureExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint;
begin
  if FFixedWidth then
    FCurrentRequiredWidth := FRequiredWidth
  else
    FCurrentRequiredWidth := ARequiredWidth;
  FCurrentRequiredHeight := 0;
  FBlocks.MeasureExtent(ACanvas, Max(FCurrentRequiredWidth - FBlockStyle.AllPaddingsLeft - FBlockStyle.AllPaddingsRight, 0));
  Result := Point(Width, Height);
end;

function TKMemoContainer.WordMouseAction(ACanvas: TCanvas; AIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean;
var
  P: TPoint;
  R, RMargin, RInterior: TRect;
  Notifier: IKMemoNotifier;
  Cursor: TCursor;
begin
  Result := False;
  P := APoint;
  R := Rect(0, 0, Width, Height);
  OffsetPoint(P, -Left - RealLeftOffset, -Top - RealTopOffset - FWordTopPadding);
  RMargin := FBlockStyle.MarginRect(R);
  RInterior := FBlockStyle.InteriorRect(FBlockStyle.BorderRect(RMargin));
  if PtInRect(RInterior, P) then
  begin
    OffsetPoint(P, -FBlockStyle.AllPaddingsLeft, -FBlockStyle.AllPaddingsTop);
    Result := FBlocks.MouseAction(AAction, ACanvas, P, AShift);
  end
  else if (Position <> mbpText) and PtInRect(R, P) and (ActiveBlocks = FBlocks) then
  begin
    Notifier := MemoNotifier;
    if Notifier <> nil then
    begin
      case AAction of
        maMove:
        begin
          if ReadOnly then
            Cursor := crDefault
          else
          begin
            Cursor := SizingGripsCursor(RMargin, P);
            if Cursor = crDefault then
              Cursor := crSizeAll;
          end;
          Notifier.SetReqMouseCursor(Cursor);
          Result := True;
        end;
        maLeftDown:
        begin
          if ssDouble in AShift then
            Notifier.EditBlock(Self)
          else
            Notifier.SelectBlock(Self, SizingGripsPosition(RMargin, P));
          Result := True;
        end;
        maRightDown:
        begin
          Result := Notifier.SelectBlock(Self, sgpNone);
        end;
      end;
    end;
  end;
end;

procedure TKMemoContainer.WordPaintToCanvas(ACanvas: TCanvas; AIndex, ALeft, ATop: Integer);
var
  R, ClipRect: TRect;
  MainClipRgn: HRGN;
  SaveIndex: Integer;
begin
  R := Rect(0, 0, Width, Height);
  KFunctions.OffsetRect(R, Left + ALeft + RealLeftOffset, Top + ATop + RealTopOffset + FWordTopPadding);
  R := FBlockStyle.MarginRect(R);
  FBlockStyle.PaintBox(ACanvas, R);
  if not ReadOnly and (ActiveBlocks = FBlocks) then
    SizingGripsDraw(ACanvas, R);
  R := FBlockStyle.BorderRect(R);
  Inc(ALeft, Left + FBlockStyle.AllPaddingsLeft + RealLeftOffset);
  Inc(ATop, Top + FBlockStyle.AllPaddingsTop + RealTopOffset + FWordTopPadding);
  if FClip then
  begin
    SaveIndex := SaveDC(ACanvas.Handle);
    MainClipRgn := CreateEmptyRgn;
    try
      ClipRect := R;
      TranslateRectToDevice(ACanvas.Handle, ClipRect);
      if GetClipRgn(ACanvas.Handle, MainClipRgn) <> 1 then
      begin
        DeleteObject(MainClipRgn);
        MainClipRgn := CreateRectRgnIndirect(ClipRect);
      end;
      if ExtSelectClipRect(ACanvas.Handle, ClipRect, RGN_AND, MainClipRgn) then
      begin
        R := FBlockStyle.InteriorRect(R);
        FBlocks.PaintToCanvas(ACanvas, ALeft, ATop, R);
      end;
    finally
      RgnSelectAndDelete(ACanvas.Handle, MainClipRgn);
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
    ACanvas.Refresh;
  end else
  begin
    R := FBlockStyle.InteriorRect(R);
    FBlocks.PaintToCanvas(ACanvas, ALeft, ATop, R);
  end;
end;

function TKMemoContainer.WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint;
  AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer;
var
  P: TPoint;
  R: TRect;
begin
  P := APoint;
  R := Rect(0, 0, Width, Height);
  OffsetPoint(P, -Left - RealLeftOffset, -Top - RealTopOffset - FWordTopPadding);
  if PtInRect(R, P) or (AOutOfArea and (P.X >= R.Left) and (P.X < R.Right)) then
  begin
    OffsetPoint(P, -FBlockStyle.AllPaddingsLeft, -FBlockStyle.AllPaddingsTop);
    Result := FBlocks.PointToIndex(ACanvas, P, AOutOfArea, ASelectionExpanding, APosition);
  end else
    Result := -1;
end;

{ TKMemoTableCell }

constructor TKMemoTableCell.Create;
begin
  inherited;
  FClip := True;
  FParaStyle := TKMemoParaStyle.Create;
  FParaStyle.OnChanged := ParaStyleChanged;
  FRequiredBorderWidths := TKRect.Create;
  FRequiredBorderWidths.OnChanged := RequiredBorderWidthsChanged;
  FSpan := MakeCellSpan(1, 1);
end;

destructor TKMemoTableCell.Destroy;
begin
  FParaStyle.Free;
  FRequiredBorderWidths.Free;
  inherited;
end;

function TKMemoTableCell.ContentLength: Integer;
begin
  if (FSpan.ColSpan > 0) and (FSpan.RowSpan > 0) then
    Result := inherited ContentLength
  else
    Result := 0;
end;

function TKMemoTableCell.GetParaStyle: TKMemoParaStyle;
begin
  Result := FParaStyle;
end;

function TKMemoTableCell.GetParentRow: TKMemoTableRow;
begin
  if Parent <> nil then
    Result := ParentBlocks.Parent as TKMemoTableRow
  else
    Result := nil;
end;

function TKMemoTableCell.GetParentTable: TKMemoTable;
var
  Row: TKMemoTableRow;
begin
  Row := ParentRow;
  if Row <> nil then
    Result := Row.ParentTable
  else
    Result := nil;
end;

function TKMemoTableCell.PointToIndex(ACanvas: TCanvas; const APoint: TPoint;
  AFirstRow, ALastRow, AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer;
var
  P: TPoint;
  R: TRect;
begin
  P := APoint;
  R := Rect(0, 0, Width, Height);
  OffsetPoint(P, -Left - RealLeftOffset, -Top - RealTopOffset - FWordTopPadding);
  if PtInRect(R, P) or
    AFirstRow and (P.X >= R.Left) and (P.X < R.Right) and (P.Y < R.Bottom) or
    ALastRow and (P.X >= R.Left) and (P.X < R.Right) and (P.Y >= R.Top) then
  begin
    OffsetPoint(P, -FBlockStyle.AllPaddingsLeft, -FBlockStyle.AllPaddingsTop);
    Result := FBlocks.PointToIndex(ACanvas, P, AOutOfArea, ASelectionExpanding, APosition);
  end else
    Result := -1;
end;

procedure TKMemoTableCell.ParaStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
begin
  // take some default paragraph properties to cell style
  FBlockStyle.ContentPadding.Assign(FParaStyle.ContentPadding);
end;

procedure TKMemoTableCell.RequiredBorderWidthsChanged(Sender: TObject);
var
  Table: TKMemoTable;
begin
  Table := ParentTable;
  if (Table <> nil) and Table.UpdateUnlocked then
    Table.FixupBorders;
end;

procedure TKMemoTableCell.RequiredWidthChanged;
var
  Row: TKMemoTableRow;
begin
  Row := ParentRow;
  if Row <> nil then
    Row.UpdateRequiredWidth;
end;

procedure TKMemoTableCell.SetColSpan(Value: Integer);
var
  Table: TKMemoTable;
  ACol, ARow: Integer;
begin
  if Value <> FSpan.ColSpan then
  begin
    Table := ParentTable;
    if (Table <> nil) and Table.UpdateUnlocked and Table.FindCell(Self, ACol, ARow) then
      Table.CellSpan[ACol, ARow] := MakeCellSpan(Value, FSpan.RowSpan)
    else
      FSpan.ColSpan := Value;
  end;
end;

procedure TKMemoTableCell.SetRowSpan(Value: Integer);
var
  Table: TKMemoTable;
  ACol, ARow: Integer;
begin
  if Value <> FSpan.RowSpan then
  begin
    Table := ParentTable;
    if (Table <> nil) and Table.UpdateUnlocked and Table.FindCell(Self, ACol, ARow) then
      Table.CellSpan[ACol, ARow] := MakeCellSpan(FSpan.ColSpan, Value)
    else
      FSpan.RowSpan := Value;
  end;
end;

procedure TKMemoTableCell.SetSpan(const Value: TKCellSpan);
var
  Table: TKMemoTable;
  ACol, ARow: Integer;
begin
  if (Value.ColSpan <> FSpan.ColSpan) or (Value.RowSpan <> FSpan.RowSpan) then
  begin
    Table := ParentTable;
    if (Table <> nil) and Table.UpdateUnlocked and Table.FindCell(Self, ACol, ARow) then
      Table.CellSpan[ACol, ARow] := Value
    else
      FSpan := Value;
  end;
end;

function TKMemoTableCell.WordMeasureExtent(ACanvas: TCanvas; AIndex,
  ARequiredWidth: Integer): TPoint;
begin
  if (FSpan.ColSpan > 0) and (FSpan.RowSpan > 0) then
    Result := inherited WordMeasureExtent(ACanvas, AIndex, ARequiredWidth)
  else
    Result := CreateEmptyPoint;
end;

procedure TKMemoTableCell.WordPaintToCanvas(ACanvas: TCanvas; AIndex, ALeft,
  ATop: Integer);
begin
  if (FSpan.ColSpan > 0) and (FSpan.RowSpan > 0) then
    inherited;
end;

{ TKMemoTableRow }

constructor TKMemoTableRow.Create;
begin
  inherited;
end;

destructor TKMemoTableRow.Destroy;
begin
  inherited;
end;

function TKMemoTableRow.CanAdd(AItem: TKMemoBlock): Boolean;
begin
  // table row can only accept table cells
  Result := AItem is TKMemoTableCell;
end;

function TKMemoTableRow.GetParentTable: TKMemoTable;
begin
  if Parent <> nil then
    Result := ParentBlocks.Parent as TKMemoTable
  else
    Result := nil;
end;

function TKMemoTableRow.GetTotalLineCount: Integer;
begin
  Result := 1; // do not add lines in cells
end;

function TKMemoTableRow.GetTotalLineRect(Index: Integer): TRect;
begin
  Result := CreateEmptyRect; // use line info from Parent instead
end;

function TKMemoTableRow.GetCellCount: Integer;
begin
  Result := Blocks.Count;
end;

function TKMemoTableRow.GetCells(Index: Integer): TKMemoTableCell;
begin
  if (Index >= 0) and (Index < Blocks.Count) then
    Result := Blocks[Index] as TKMemoTableCell
  else
    Result := nil;
end;

procedure TKMemoTableRow.SetCellCount(const Value: Integer);
var
  I: Integer;
begin
  if Value <> CellCount then
  begin
    if Value > CellCount then
    begin
      for I := CellCount to Value - 1 do
        Blocks.Add(TKMemoTableCell.Create);
    end else
    begin
      for I := Value to CellCount - 1 do
        Blocks.Delete(Value);
    end;
  end;
end;

procedure TKMemoTableRow.UpdateRequiredWidth;
var
  I: Integer;
begin
  FRequiredWidth := 0;
  for I := 0 to CellCount - 1 do
    Inc(FRequiredWidth, Cells[I].RequiredWidth);
end;

procedure TKMemoTableRow.RequiredHeightChanged;
var
  I: Integer;
begin
  for I := 0 to CellCount - 1 do
    Cells[I].RequiredHeight := RequiredHeight;
end;

procedure TKMemoTableRow.RequiredWidthChanged;
var
  I, OldWidth, CellWidth: Integer;
  Ratio: Double;
begin
  OldWidth := FBlocks.Width;
  if OldWidth <= 0 then
  begin
    if CellCount > 0 then
    begin
      CellWidth := RequiredWidth div CellCount;
      for I := 0 to CellCount - 1 do
        Cells[I].RequiredWidth := CellWidth;
    end;
  end else
  begin
    Ratio := RequiredWidth / OldWidth;
    for I := 0 to CellCount - 1 do
      Cells[I].RequiredWidth := Round(Cells[I].RequiredWidth * Ratio);
  end;
end;

{ TKMemoTable }

constructor TKMemoTable.Create;
begin
  inherited;
  FCellStyle := TKMemoBlockStyle.Create;
  FColCount := 0;
  FColWidths := TKMemoSparseList.Create;
  FBlockStyle.WrapMode := wrNone;
end;

destructor TKMemoTable.Destroy;
begin
  FCellStyle.Free;
  FColWidths.Free;
  inherited;
end;

procedure TKMemoTable.ApplyDefaultCellStyle;
var
  Cell: TKMemoTableCell;
  Row: TKMemoTableRow;
  I, J, W: Integer;
begin
  for I := 0 to RowCount - 1 do
  begin
    Row := Rows[I];
    for J := 0 to Row.CellCount - 1 do
    begin
      Cell := Row.Cells[J];
      if (Cell.ColSpan > 0) and (Cell.RowSpan > 0) then
      begin
        W := FCellStyle.BorderWidth;
        Cell.BlockStyle.AssignNC(FCellStyle);
        Cell.BlockStyle.BorderWidth := 0;
        Cell.BlockStyle.BorderWidths.All := 0;
        Cell.RequiredBorderWidths.All := W;
      end;
    end;
  end;
  FixupBorders;
end;

function TKMemoTable.CalcTotalCellWidth(ACol, ARow: Integer): Integer;
var
  BaseCol, BaseRow: Integer;
  Cell: TKMemoTableCell;
  Row: TKMemoTableRow;
  I, W: Integer;
begin
  FindBaseCell(ACol, ARow, BaseCol, BaseRow);
  Row := Rows[BaseRow];
  Cell := Row.Cells[BaseCol];
  Result := 0;
  for I := BaseCol to BaseCol + Cell.ColSpan - 1 do
  begin
    Cell := Row.Cells[I];
    if Cell.RequiredWidth > 0 then
      W := Cell.RequiredWidth
    else
      W := Cell.Width;
    Inc(Result, W);
  end;
end;

function TKMemoTable.CanAdd(AItem: TKMemoBlock): Boolean;
begin
  // table can only accept table rows
  Result := AItem is TKMemoTableRow;
end;

function TKMemoTable.CellValid(ACol, ARow: Integer): Boolean;
begin
  Result := (ARow >= 0) and (ARow < RowCount) and (ACol >= 0) and (ACol < Rows[ARow].CellCount);
end;

function TKMemoTable.CellVisible(ACol, ARow: Integer): Boolean;
var
  Span: TKCellSpan;
begin
  Span := CellSpan[ACol, ARow];
  Result := (Span.ColSpan > 0) and (Span.RowSpan > 0);
end;

function TKMemoTable.ColValid(ACol: Integer): Boolean;
begin
  Result := (ACol >= 0) and (ACol < FColCount);
end;

procedure TKMemoTable.FindBaseCell(ACol, ARow: Integer; out BaseCol, BaseRow: Integer);
var
  Span: TKCellSpan;
begin
  BaseCol := ACol;
  BaseRow := ARow;
  Span := GetCellSpan(ACol, ARow);
  if (Span.ColSpan <= 0) and (Span.RowSpan <= 0) then
  begin
    BaseCol := ACol + Span.ColSpan;
    BaseRow := ARow + Span.RowSpan;
  end;
end;

function TKMemoTable.FindCell(ACell: TKMemoTableCell; out ACol, ARow: Integer): Boolean;
var
  I, J: Integer;
  Row: TKMemoTableRow;
begin
  Result := False;
  for I := 0 to RowCount - 1 do
  begin
    Row := Rows[I];
    for J := 0 to Row.CellCount - 1 do
      if Row.Cells[J] = ACell then
      begin
        ACol := J;
        ARow := I;
        Result := True;
        Exit;
      end;
  end;
end;

procedure TKMemoTable.FixupBorders;
var
  Cell: TKMemoTableCell;
  Row: TKMemoTableRow;
  PrevColCell: TKMemoTableCell;
  PrevRowCell: TKMemoTableCell;
  BaseCol, BaseRow, I, J, Width, Part, CurBaseCol, CurBaseRow: Integer;
begin
  for I := 0 to RowCount - 1 do
  begin
    Row := Rows[I];
    for J := 0 to Row.CellCount - 1 do
    begin
      Cell := Row.Cells[J];
      // find cells in previous row and column (or base cells corresponding to these positions)
      // these cells cannot be a part of merged area of cells to which the current cell also belongs
      FindBaseCell(J, I, CurBaseCol, CurBaseRow);
      PrevRowCell := nil;
      if I > 0 then
      begin
        FindBaseCell(J, I - 1, BaseCol, BaseRow);
        if (CurBaseCol <> BaseCol) or (CurBaseRow <> BaseRow) then
          PrevRowCell := Rows[BaseRow].Cells[BaseCol];
      end;
      PrevColCell := nil;
      if J > 0 then
      begin
        FindBaseCell(J - 1, I, BaseCol, BaseRow);
        if (CurBaseCol <> BaseCol) or (CurBaseRow <> BaseRow) then
          PrevColCell := Rows[BaseRow].Cells[BaseCol];
      end;
      // assign default cell borders
      Cell.BlockStyle.BorderWidth := 0;
      Cell.BlockStyle.BorderWidths.Assign(Cell.RequiredBorderWidths);
      if PrevColCell <> nil then
      begin
        // we split the border width among two neighbor cells in horizontal direction
        Width := Max(Cell.RequiredBorderWidths.Left, PrevColCell.RequiredBorderWidths.Right);
        if Width > 0 then
        begin
          Part := DivUp(Width, 2);
          Cell.BlockStyle.BorderWidths.Left := Part;
          PrevColCell.BlockStyle.BorderWidths.Right := Width - Part;
        end;
      end;
      if PrevRowCell <> nil then
      begin
        // we split the border width among two neighbor cells in vertical direction
        Width := Max(Cell.RequiredBorderWidths.Top, PrevRowCell.RequiredBorderWidths.Bottom);
        if Width > 0 then
        begin
          Part := DivUp(Width, 2);
          Cell.BlockStyle.BorderWidths.Top := Part;
          PrevRowCell.BlockStyle.BorderWidths.Bottom := Width - Part
        end;
      end;
    end;
  end;
end;

procedure TKMemoTable.FixupCellSpan;
var
  I, J: Integer;
  Span, RefSpan: TKCellSpan;
  Row: TKMemoTableRow;
  Cell: TKMemoTableCell;
begin
  LockUpdate;
  try
    RefSpan := MakeCellSpan(1, 1);
    // don't make this too complicated, but maybe it is little bit slower:
    // reset all negative spans
    for I := 0 to RowCount - 1 do
    begin
      Row := Rows[I];
      for J := 0 to Row.CellCount - 1 do
      begin
        Cell := Row.Cells[J];
        if (Cell.ColSpan <= 0) or (Cell.RowSpan <= 0) then
          Cell.Span := RefSpan;
      end;
    end;
    // create all spans
    for I := 0 to RowCount - 1 do
    begin
      Row := Rows[I];
      for J := 0 to Row.CellCount - 1 do
      begin
        Cell := Row.Cells[J];
        if (Cell.ColSpan > 1) or (Cell.RowSpan > 1) then
        begin
          Span := MakeCellSpan(Min(Cell.ColSpan, Row.CellCount - J), Min(Cell.RowSpan, RowCount - I));
          Cell.Span := RefSpan;
          InternalSetCellSpan(J, I, Span);
        end;
      end;
    end;
  finally
    UnlockUpdate;
  end;
end;

procedure TKMemoTable.FixupCellSpanFromRTF;

  function HasSomeCellZeroWidth(ARow: TKMemoTableRow): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to ARow.CellCount - 1 do
      if ARow.Cells[I].RequiredWidth = 0 then
      begin
        Result := True;
        Exit;
      end;
  end;

  function FindValidBaseCell(ACol, ARow: Integer; out AColDelta, ARowDelta: Integer): TKMemoTableCell;
  var
    I, J: Integer;
    Row: TKMemoTableRow;
    Cell: TKMemoTableCell;
  begin
    Result := nil;
    AColDelta := 0;
    ARowDelta := 0;
    Row := nil;
    for I := ARow downto 0 do
    begin
      Row := Rows[I];
      Cell := Row.Cells[ACol];
      if (Cell.ColSpan >= 0) and (Cell.RowSpan > 0) or (Cell.ColSpan < 0) and (Cell.RowSpan = 0) then
        Break
      else
        Inc(ARowDelta);
    end;
    if Row <> nil then
    begin
      for J := ACol downto 0 do
      begin
        Cell := Row.Cells[J];
        if (Cell.RowSpan >= 0) and (Cell.ColSpan > 0) or (Cell.RowSpan < 0) and (Cell.ColSpan = 0) then
        begin
          Result := Cell;
          Break;
        end else
          Inc(AColDelta);
      end;
    end;
  end;

var
  I, J, K, CellCnt, ColDelta, MaxXPos, RowDelta, RowXPos, WDelta, XPos: Integer;
  Row: TKMemoTableRow;
  Span: TKCellSpan;
  LastCell, Cell, TmpCell: TKMemoTableCell;
begin
  // this routine assumes the table was loaded from RTF
  LockUpdate;
  try
    { First update our FColCount and FColWidths properties.
      This is needed because horizontally merged table cells are stored as a single cell
      and the only distinguishing factor is their width. So go through the table
      and find every vertical cell split (end of each cell). }
    MaxXPos := 0;
    for I := 0 to RowCount - 1 do
    begin
      Row := Rows[I];
      XPos := 0;
      for J := 0 to Row.CellCount - 1 do
        Inc(XPos, Row.Cells[J].RequiredWidth);
      MaxXPos := Max(MaxXPos, XPos);
    end;
    XPos := 0;
    K := 0;
    while XPos < MaxXPos do
    begin
      WDelta := MaxInt;
      for I := 0 to RowCount - 1 do
      begin
        Row := Rows[I];
        J := 0;
        RowXPos := 0;
        while (J < Row.CellCount) and (RowXPos <= XPos) do
        begin
          Inc(RowXPos, Row.Cells[J].RequiredWidth);
          Inc(J);
        end;
        if RowXPos > XPos then
          WDelta := Min(RowXPos - XPos, WDelta);
      end;
      Inc(XPos, WDelta);
      if K < FColCount then
        FColWidths[K].Index := WDelta
      else
      begin
        Inc(FColCount);
        FColWidths.AddItem(WDelta);
      end;
      Inc(K);
    end;
    { Now fill the missing cells for each row that has horizontally merged cells. }
    for I := 0 to RowCount - 1 do
    begin
      Row := Rows[I];
      if (Row.CellCount < FColCount) or HasSomeCellZeroWidth(Row) then
      begin
        CellCnt := 0;
        LastCell := Row.Cells[CellCnt]; // this cell must always exist
        for J := 0 to FColCount - 1 do if LastCell <> nil then
        begin
          // fill the gaps for possibly merged cells
          WDelta := LastCell.RequiredWidth - FColWidths[J].Index;
          if WDelta = 0 then
          begin
            // OK, here this cell was certainly not merged so take next cell
            Inc(CellCnt);
            if CellCnt < Row.CellCount then
              LastCell := Row.Cells[CellCnt]
            else
              LastCell := nil;
          end
          else if WDelta > 0 then
          begin
            // the cell must have been merged here
            TmpCell := nil;
            if CellCnt < Row.CellCount - 1 then
            begin
              // first check if next cell has zero width
              TmpCell := Row.Cells[CellCnt + 1];
              if TmpCell.RequiredWidth > 0 then
                TmpCell := nil;
            end;
            if TmpCell = nil then
            begin
              // otherwise insert new cell
              TmpCell := TKMemoTableCell.Create;
              TmpCell.RowSpan := LastCell.RowSpan;
              Row.Blocks.Insert(CellCnt + 1, TmpCell);
            end;
            TmpCell.FixedWidth := True;
            TmpCell.RequiredWidth := WDelta;
            TmpCell.ColSpan := 0; // for later fixup
            LastCell.RequiredWidth := LastCell.RequiredWidth - WDelta;
            LastCell := TmpCell;
            Inc(CellCnt);
          end;
        end;
        // delete superfluous cells
        while Row.CellCount > FColCount do
          Row.Blocks.Delete(FColCount);
      end else
      begin
        for J := 0 to FColCount - 1 do
          Row.Cells[J].RequiredWidth := FColWidths[J].Index;
      end;
    end;
    // here we fixup the Span properties
    for I := 0 to RowCount - 1 do
    begin
      Row := Rows[I];
      for J := 0 to Row.CellCount - 1 do
      begin
        Cell := Row.Cells[J];
        Span := Cell.Span;
        if (Span.ColSpan = 0) or (Span.RowSpan = 0) then
        begin
          LastCell := FindValidBaseCell(J, I, ColDelta, RowDelta);
          if LastCell <> nil then
          begin
            LastCell.ColSpan := Max(LastCell.ColSpan, ColDelta + 1);
            LastCell.RowSpan := Max(LastCell.RowSpan, RowDelta + 1);
            Cell.ColSpan := -ColDelta;
            Cell.RowSpan := -RowDelta;
          end
        end;
        Cell.FixedWidth := False;
        Cell.FixedHeight := False;
        if (Span.ColSpan > 0) and (Span.RowSpan > 0) then
          Cell.Blocks.AddParagraph;
      end;
    end;
  finally
    UnlockUpdate;
  end;
end;

function TKMemoTable.GetCells(ACol, ARow: Integer): TKMemoTableCell;
begin
  if CellValid(ACol, ARow) then
    Result := Rows[ARow].Cells[ACol]
  else
    Result := nil;
end;

function TKMemoTable.GetCellSpan(ACol, ARow: Integer): TKCellSpan;
begin
  if CellValid(ACol, ARow) then
    Result := Rows[ARow].Cells[ACol].Span
  else
    Result := MakeCellSpan(1, 1);
end;

function TKMemoTable.GetColWidths(Index: Integer): Integer;
begin
  Result := FColWidths[Index].Index;
end;

function TKMemoTable.GetRowCount: Integer;
begin
  Result := Blocks.Count;
end;

function TKMemoTable.GetRowHeights(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < RowCount) then
    Result := Rows[Index].Height
  else
    Result := 0
end;

function TKMemoTable.GetRows(Index: Integer): TKMemoTableRow;
begin
  if (Index >= 0) and (Index < RowCount) then
    Result := Blocks[Index] as TKMemoTableRow
  else
    Result := nil;
end;

procedure TKMemoTable.InternalSetCellSpan(ACol, ARow: Integer; const Value: TKCellSpan);

  procedure Merge(ACol1, ARow1, ACol2, ARow2: Integer);
  var
    I, J: Integer;
    Row: TKMemoTableRow;
    Cell: TKMemoTableCell;
  begin
    for I := ARow1 to ARow2 - 1 do
    begin
      Row := Rows[I];
      for J := ACol1 to ACol2 - 1 do
      begin
        Cell := Row.Cells[J];
        if (I = ACol1) and (J = ARow1) then
          Cell.Span := MakeCellSpan(ACol2 - ACol1, ARow2 - ARow1)
        else
          Cell.Span := MakeCellSpan(ACol1 - J, ARow1 - I);
      end;
    end;
  end;

  procedure Split(ACol1, ARow1, ACol2, ARow2: Integer);
  var
    I, J: Integer;
    Row: TKMemoTableRow;
    Cell: TKMemoTableCell;
    RefSpan: TKCellSpan;
  begin
    RefSpan := MakeCellSpan(1, 1);
    for I := ARow1 to ARow2 - 1 do
    begin
      Row := Rows[I];
      for J := ACol1 to ACol2 - 1 do
      begin
        Cell := Row.Cells[J];
        Cell.Span := RefSpan;
      end;
    end;
  end;

var
  I, J, BaseCol, BaseRow: Integer;
  Span: TKCellSpan;
  Row: TKMemoTableRow;
  Cell: TKMemoTableCell;
begin
  LockUpdate;
  try
    Span := GetCellSpan(ACol, ARow);
    if (Span.ColSpan > 1) or (Span.RowSpan > 1) then
    begin
      // destroy previously merged area
      Split(ACol, ARow, ACol + Span.ColSpan, ARow + Span.RowSpan);
    end;
    for J := ARow to ARow + Value.RowSpan - 1 do
    begin
      Row := Rows[J];
      for I := ACol to ACol + Value.ColSpan - 1 do
      begin
        Cell := Row.Cells[I];
        Span := Cell.Span;
        if (Span.ColSpan <> 1) or (Span.RowSpan <> 1) then
        begin
          // adjust all four overlapping spans
          FindBaseCell(I, J, BaseCol, BaseRow);
          if (BaseCol <> ACol) or (BaseRow <> ARow) then
          begin
            Span := GetCellSpan(BaseCol, BaseRow);
            Split(Max(ACol, BaseCol), Max(ARow, BaseRow),
              Min(ACol + Value.ColSpan, BaseCol + Span.ColSpan), Min(ARow + Value.RowSpan, BaseRow + Span.RowSpan));
            Merge(BaseCol, BaseRow, BaseCol + Span.ColSpan, ARow);
            Merge(BaseCol, ARow + Value.RowSpan, BaseCol + Span.ColSpan, BaseRow + Span.RowSpan);
            Merge(BaseCol, Max(ARow, BaseRow), ACol, Min(ARow + Value.RowSpan, BaseRow + Span.RowSpan));
            Merge(ACol + Value.ColSpan, Max(ARow, BaseRow), BaseCol + Span.ColSpan, Min(ARow + Value.RowSpan, BaseRow + Span.RowSpan));
          end;
        end;
        if (I = ACol) and (J = ARow) then
          Cell.Span := Value
        else
          // negative cell span - means this cell is hidden
          // it indicates where base cell for this span is located
          Cell.Span := MakeCellSpan(ACol - I, ARow - J);
      end;
    end;
  finally
    UnlockUpdate;
  end;
end;

procedure TKMemoTable.RequiredWidthChanged;
var
  I: Integer;
begin
  for I := 0 to RowCount - 1 do
    Rows[I].RequiredWidth := RequiredWidth;
end;

function TKMemoTable.RowValid(ARow: Integer): Boolean;
begin
  Result := (ARow >= 0) and (ARow < RowCount);
end;

procedure TKMemoTable.SetCellSpan(ACol, ARow: Integer; Value: TKCellSpan);
var
  Span: TKCellSpan;
begin
  if CellValid(ACol, ARow) then
  begin
    Value.ColSpan := MinMax(Value.ColSpan, 1, Rows[ARow].CellCount - ACol);
    Value.RowSpan := MinMax(Value.RowSpan, 1, RowCount - ARow);
    Span := GetCellSpan(ACol, ARow);
    if (Span.ColSpan <> Value.ColSpan) or (Span.RowSpan <> Value.RowSpan) then
      InternalSetCellSpan(ACol, ARow, Value);
  end;
end;

procedure TKMemoTable.SetColCount(const Value: Integer);
begin
  SetSize(Value, RowCount);
end;

procedure TKMemoTable.SetColWidths(Index: Integer; const Value: Integer);
var
  I: Integer;
begin
  if Value <> ColWidths[Index] then
  begin
    FColWidths[Index].Index := Value;
    for I := 0 to RowCount - 1 do
      Rows[I].Cells[Index].RequiredWidth := Value;
  end;
end;

procedure TKMemoTable.SetRowCount(const Value: Integer);
begin
  SetSize(ColCount, Value);
end;

procedure TKMemoTable.SetRowHeights(Index: Integer; const Value: Integer);
begin
  if (Index >= 0) and (Index < RowCount) then
    Rows[Index].RequiredHeight := Value;
end;

procedure TKMemoTable.SetSize(AColCount, ARowCount: Integer);
var
  I, J: Integer;
  Row: TKMemoTableRow;
begin
  if AColCount <> FColCount then
  begin
    FColCount := AColCount;
    for I := 0 to RowCount - 1 do
      Rows[I].CellCount := FColCount;
    FColWidths.SetSize(FColCount);
  end;
  if ARowCount <> RowCount then
  begin
    if ARowCount > RowCount then
    begin
      for I := RowCount to ARowCount - 1 do
      begin
        Row := TKMemoTableRow.Create;
        Row.CellCount := FColCount;
        Row.RequiredWidth := RequiredWidth;
        for J := 0 to FColCount - 1 do
          if ColWidths[J] <> 0 then
            Row.Cells[J].RequiredWidth := ColWidths[J];
        Blocks.Add(Row);
      end;
    end else
    begin
      for I := ARowCount to RowCount - 1 do
        Blocks.Delete(RowCount - 1);
    end;
  end;
end;

function TKMemoTable.WordMeasureExtent(ACanvas: TCanvas; AIndex,
  ARequiredWidth: Integer): TPoint;

  function GetExtentSpanned(AExtents: TKMemoSparseList; AIndex, ASpan: Integer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := AIndex to AIndex + ASpan - 1 do
      if I < AExtents.Count then
        Result := Result + AExtents[I].Index;
  end;

const
  cMinColSize = 20;
var
  I, J, Len, RealColCount, ColWidth, DefColCount, DefSpace, DistSpace, MinSpace, OverflowSpace, UndefColCount, UndefColWidth, UndefSpace, TotalSpace, PosX, PosY, VDelta: Integer;
  Extent: TPoint;
  Row: TKMemoTableRow;
  Cell: TKmemoTableCell;
  MeasWidths, MinWidths, Heights: TKmemoSparseList;
begin
  // this is the table layout calculation
  if FFixedWidth then
    ARequiredWidth := FRequiredWidth;
  Dec(ARequiredWidth, FBlockStyle.LeftPadding + FBlockStyle.RightPadding);
  // calculate real column count, this may be different from FColCount
  RealColCount := 0;
  for I := 0 to RowCount - 1 do
    RealColCount := Max(RealColCount, Rows[I].CellCount);
  // calculate predefined column widths
  DefColCount := 0;
  DefSpace := 0;
  for I := 0 to FColCount - 1 do
  begin
    Inc(DefSpace, FColWidths[I].Index);
    if FColWidths[I].Index > 0 then
      Inc(DefColCount);
  end;
  if RealColCount > DefColCount then
  begin
    UndefColCount := RealColCount - DefColCount;
    UndefSpace := Max(ARequiredWidth - DefSpace, UndefColCount * cMinColSize);
    UndefColWidth := DivDown(UndefSpace, UndefColCount);
  end else
  begin
    UndefSpace := 0;
    UndefColWidth := cMinColSize;
  end;
  TotalSpace := DefSpace + UndefSpace;
  // now measure cells
  MeasWidths := TKMemoSparseList.Create;
  MinWidths := TKMemoSparseList.Create;
  Heights := TKMemoSparseList.Create;
  try
    MeasWidths.SetSize(RealColCount);
    MinWidths.SetSize(RealColCount);
    Heights.SetSize(RowCount);
    for J := 0 to RowCount - 1 do
      Heights[J].Index := 0;
    // first measure cells with predefined column widths
    for I := 0 to RealColCount - 1 do
    begin
      if (I < FColCount) and (FColWidths[I].Index > 0) then
        ColWidth := Max(MulDiv(FColWidths[I].Index, ARequiredWidth, TotalSpace), cMinColSize)
      else
        ColWidth := UndefColWidth;
      MeasWidths[I].Index := 0;
      MinWidths[I].Index := 0;
      for J := 0 to RowCount - 1 do
      begin
        Row := Rows[J];
        if I < Row.CellCount then
        begin
          Cell := Row.Cells[I];
          if Cell.ColSpan = 1 then
          begin
            Extent := Cell.WordMeasureExtent(ACanvas, 0, cMinColSize);
            MinWidths[I].Index := Max(MinWidths[I].Index, Extent.X);
            Extent := Cell.WordMeasureExtent(ACanvas, 0, ColWidth); // Extent.X can be bigger than ColWidth here
            MeasWidths[I].Index := Max(MeasWidths[I].Index, Extent.X);
            if Cell.RowSpan = 1 then
              Heights[J].Index := Max(Heights[J].Index, Extent.Y);
          end;
        end;
      end;
      if MeasWidths[I].Index = 0 then
        MeasWidths[I].Index := ColWidth;
      if MinWidths[I].Index = 0 then
        MinWidths[I].Index := ColWidth;
    end;
    // then, if some MeasWidths were bigger than ColWidth, recalculate remaining columns to fit required width
    OverflowSpace := 0;
    for I := 0 to RealColCount - 1 do
      Inc(OverflowSpace, MeasWidths[I].Index);
    Dec(OverflowSpace, ARequiredWidth);
    if OverflowSpace > 0 then
    begin
      MinSpace := 0;
      for I := 0 to RealColCount - 1 do
        if MeasWidths[I].Index > MinWidths[I].Index then
          Inc(MinSpace, MeasWidths[I].Index);
      if MinSpace > 0 then
      begin
        DistSpace := 0;
        for I := 0 to RealColCount - 1 do
          if (MeasWidths[I].Index > MinWidths[I].Index) and (MinSpace > 0) then
          begin
            VDelta := DivUp(MeasWidths[I].Index * OverflowSpace, MinSpace);
            Dec(MinSpace, MeasWidths[I].Index);
            if MeasWidths[I].Index - MinWidths[I].Index < VDelta then
            begin
              Dec(OverflowSpace, MeasWidths[I].Index - MinWidths[I].Index);
              Inc(DistSpace, MeasWidths[I].Index - MinWidths[I].Index);
              MeasWidths[I].Index := MinWidths[I].Index;
            end else
            begin
              Dec(OverflowSpace, VDelta);
              MeasWidths[I].Index := MeasWidths[I].Index - VDelta;
              Inc(DistSpace, VDelta);
            end;
          end;
      end;
    end;
    // then, measure again with maximum allowed column width and update vertical extents
    for I := 0 to RowCount - 1 do
    begin
      Row := Rows[I];
      for J := 0 to Row.CellCount - 1 do
      begin
        Cell := Row.Cells[J];
        if CellVisible(J, I) and ((MeasWidths[J].Index <> Cell.Width) or (Cell.ColSpan > 1) or (Cell.RowSpan > 1)) then
        begin
          Extent := Cell.WordMeasureExtent(ACanvas, 0, GetExtentSpanned(MeasWidths, J, Cell.ColSpan));
          // update vertical extents
          if Cell.RowSpan = 1 then
            Heights[I].Index := Max(Heights[I].Index, Extent.Y);
        end;
      end;
    end;
    // then, update vertical extents for row-spanned cells, because some of these cells might be taller than remaining cells
    for I := 0 to RowCount - 1 do
    begin
      Row := Rows[I];
      for J := 0 to Row.CellCount - 1 do
      begin
        Cell := Row.Cells[J];
        if CellVisible(J, I) and (Cell.RowSpan > 1) then
        begin
          VDelta := Cell.Height - GetExtentSpanned(Heights, I, Cell.RowSpan);
          if VDelta > 0 then
            Heights[I + Cell.RowSpan - 1].Index := Heights[I + Cell.RowSpan - 1].Index + VDelta;
        end;
      end;
    end;
    // finally, set cell/row positions and heights
    ClearLines;
    Extent.X := 0;
    for I := 0 to RealColCount - 1 do
      Inc(Extent.X, MeasWidths[I].Index);
    Len := 0;
    PosY := 0;
    for I := 0 to RowCount - 1 do
    begin
      Row := Rows[I];
      PosX := 0;
      for J := 0 to Row.CellCount - 1 do
      begin
        Cell := Row.Cells[J];
//          Cell.SetBlockExtent(Cell.WordWidth[0], VertExtents[I].Index); // No! Cell is measured by default way
        Cell.WordLeft[0] := PosX;
        Cell.WordTop[0] := 0;
        if CellVisible(J, I) then
        begin
          Cell.WordHeight[0] := GetExtentSpanned(Heights, I, Cell.RowSpan);
        end else
        begin
          Cell.WordHeight[0] := Heights[I].Index;
        end;
        Inc(PosX, MeasWidths[J].Index);
      end;
      Row.SetBlockExtent(Extent.X, Heights[I].Index);
      Row.WordLeft[0] := 0;
      Row.WordTop[0] := PosY;
      Row.WordHeight[0] := Heights[I].Index;
      Row.AddSingleLine;
      AddBlockLine(I, Len, I, Len + Row.ContentLength - 1, 0, PosY, Extent.X, Heights[I].Index);
      Inc(Len, Row.ContentLength);
      Inc(PosY, Heights[I].Index);
    end;
    Extent.Y := PosY;
    Inc(Extent.X, FBlockStyle.LeftPadding + FBlockStyle.RightPadding);
    Inc(Extent.Y, FBlockStyle.TopPadding + FBlockStyle.BottomPadding);
    SetBlockExtent(Extent.X, Extent.Y);
    WordLeft[0] := 0;
    WordTop[0] := 0;
    WordHeight[0] := 0;
  finally
    MeasWidths.Free;
    MinWidths.Free;
    Heights.Free;
  end;
  Result := Point(Extent.X, Extent.Y);
end;

function TKMemoTable.WordMouseAction(ACanvas: TCanvas; AIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean;
var
  P: TPoint;
  R: TRect;
  I, J: Integer;
  Row: TKMemoTableRow;
  Cell: TKMemoTableCell;
begin
  Result := False;
  P := APoint;
  R := Rect(0, 0, Width, Height);
  OffsetPoint(P, -Left - RealLeftOffset, -Top - RealTopOffset - FWordTopPadding);
  if PtInRect(R, P) then
  begin
    OffsetPoint(P, -FBlockStyle.AllPaddingsLeft, -FBlockStyle.AllPaddingsTop);
    for I := 0 to RowCount - 1 do
    begin
      Row := Rows[I];
      for J := 0 to Row.CellCount - 1 do
      begin
        Cell := Row.Cells[J];
        if (Cell.ColSpan > 0) and (Cell.RowSpan > 0) then
        begin
          Result := Result or Cell.WordMouseAction(ACanvas, 0, AAction, P, AShift);
        end;
      end;
      Dec(P.Y, Row.Height);
    end;
  end;
end;

function TKMemoTable.WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint;
  AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean;
  out APosition: TKMemoLinePosition): Integer;
var
  P: TPoint;
  R: TRect;
  I, J, Len: Integer;
  Row: TKMemoTableRow;
  Cell: TKMemoTableCell;
begin
  Result := -1;
  P := APoint;
  R := Rect(0, 0, Width, Height);
  OffsetPoint(P, -Left - RealLeftOffset, -Top - RealTopOffset - FWordTopPadding);
  if PtInRect(R, P) or (AOutOfArea and (P.X >= R.Left) and (P.X < R.Right)) then
  begin
    OffsetPoint(P, -FBlockStyle.AllPaddingsLeft, -FBlockStyle.AllPaddingsTop);
    Len := 0;
    I := 0;
    while (Result < 0) and (I < RowCount) do
    begin
      Row := Rows[I];
      J := 0;
      while (Result < 0) and (J < Row.CellCount) do
      begin
        Cell := Row.Cells[J];
        if (Cell.ColSpan > 0) and (Cell.RowSpan > 0) then
        begin
          Result := Cell.PointToIndex(ACanvas, P, I = 0, I + Cell.RowSpan >= RowCount, AOutOfArea, ASelectionExpanding, APosition);
          if Result < 0 then
            Inc(Len, Cell.ContentLength);
        end;
        Inc(J);
      end;
      Dec(P.Y, Row.Height);
      Inc(I);
    end;
    if Result >= 0 then
      Inc(Result, Len);
  end;
end;

{ TKMemoBlocks }

constructor TKMemoBlocks.Create;
begin
  inherited;
  OwnsObjects := True;
  FIgnoreParaMark := False;
  FLines := TKMemoLines.Create;
  FParent := nil;
  FRelPos := TKMemoSparseList.Create;
  FExtent := CreateEmptyPoint;
  FMemoNotifier := nil;
  FSelEnd := 0;
  FSelStart := 0;
  FUpdateLock := 0;
  FOnUpdate := nil;
  Update([muContent]);
end;

destructor TKMemoBlocks.Destroy;
begin
  FOnUpdate := nil;
  if FMemoNotifier <> nil then
    FMemoNotifier.BlocksFreeNotification(Self);
  FreeAndNil(FLines);
  FreeAndNil(FRelPos);
  Inc(FUpdateLock); // prevent calls of UpdateAttributes
  inherited;
end;

procedure TKMemoBlocks.DoUpdate(AReasons: TKMemoUpdateReasons);
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(AReasons);
end;

function TKMemoBlocks.AddAt(AObject: TKMemoBlock; At: Integer): Integer;
begin
  if AObject <> nil then
  begin
    // check if parent can add this item
    if (FParent = nil) or FParent.CanAdd(AObject) then
    begin
      if Empty and (Count > 0) then
        inherited Delete(0);
      if (At < 0) or (At >= Count) then
        Result := inherited Add(AObject)
      else
      begin
        inherited Insert(At, AObject);
        Result := At;
      end;
    end else
    begin
      AObject.Free;
      Result := -1;
    end;
  end else
    Result := -1;
end;

function TKMemoBlocks.AddContainer(At: Integer): TKMemoContainer;
begin
  LockUpdate;
  try
    Result := TKMemoContainer.Create;
    AddAt(Result, At);
  finally
    UnlockUpdate;
  end;
end;

function TKMemoBlocks.AddHyperlink(AItem: TKMemoHyperlink;
  At: Integer): TKMemoHyperlink;
begin
  Result := AItem;
  Result.TextStyle.Assign(LastTextStyle(At));
  Result.DefaultStyle;
  AddAt(Result, At);
end;

function TKMemoBlocks.AddHyperlink(const AText, AURL: TKString; At: Integer): TKMemoHyperlink;
begin
  Result := TKMemoHyperLink.Create;
  Result.TextStyle.Assign(LastTextStyle(At));
  Result.DefaultStyle;
  Result.Text := AText;
  Result.URL := AURL;
  AddAt(Result, At);
end;

function TKMemoBlocks.AddImageBlock(AImage: TPicture; At: Integer): TKMemoImageBlock;
begin
  Result := TKMemoImageBlock.Create;
  Result.Image := AImage;
  AddAt(Result, At);
end;

function TKMemoBlocks.AddImageBlock(const APath: TKString; At: Integer): TKMemoImageBlock;
begin
  Result := TKMemoImageBlock.Create;
  Result.SetImagePath(APath);
  AddAt(Result, At);
end;

function TKMemoBlocks.AddParagraph(At: Integer): TKMemoParagraph;
var
  PA: TKMemoParagraph;
begin
  Result := TKMemoParagraph.Create;
  PA := GetNearestParagraphItem(At);
  if PA <> nil then
  begin
    Result.AssignAttributes(PA);
    if PA.ParaStyle.NumberStartAt > 0 then
    begin
      PA.ParaStyle.NumberStartAt := 0;
    end;
  end else
  begin
    Result.TextStyle.Assign(LastTextStyle(At));
    Result.ParaStyle.Assign(GetDefaultParaStyle);
  end;
  AddAt(Result, At);
end;

function TKMemoBlocks.AddTable(At: Integer): TKMemoTable;
begin
  Result := TKMemoTable.Create;
  AddAt(Result, At);
end;

function TKMemoBlocks.AddTextBlock(const AText: TKString; At: Integer): TKMemoTextBlock;
begin
  Result := TKMemoTextBlock.Create;
  Result.TextStyle.Assign(LastTextStyle(At));
  Result.Text := AText;
  AddAt(Result, At);
end;

function TKMemoBlocks.BlockToItem(ABlock: Integer): TKMemoBlock;
begin
  if (ABlock >= 0) and (ABlock < Count) then
    Result := Items[ABlock]
  else
    Result := nil;
end;

procedure TKMemoBlocks.ClearSelection(ATextOnly: Boolean);
var
  I, First, Last: Integer;
  Item: TKMemoBlock;
begin
  LockUpdate;
  try
    I := 0;
    First := -1;
    Last := -1;
    while I < Count do
    begin
      Item := Items[I];
      if (Item.SelStart >= 0) and (Item.SelLength > 0) then
      begin
        if ATextOnly and (Item is TKMemoContainer) then
        begin
          TKMemoContainer(Item).ClearSelection(ATextOnly);
        end else
        begin
          if Item.ContentLength = 0 then
            Delete(I)
          else
          begin
            if Item.SelLength = Item.SelectableLength(True) then
            begin
              Delete(I);
              Dec(I);
            end else
            begin
              if First < 0 then
                First := I
              else
                Last := I;
            end;
          end;
        end;
      end;
      Inc(I);
    end;
    if Last >= 0 then
    begin
      Items[Last].ClearSelection(ATextOnly);
    end;
    if First >= 0 then
    begin
      Items[First].ClearSelection(ATextOnly);
    end;
    if FSelStart < FSelEnd then
      FSelEnd := FSelStart
    else
      FSelStart := FSelEnd;
    FSelStart := MinMax(FSelStart, 0, FSelectableLength);
    FSelEnd := MinMax(FSelEnd, 0, FSelectableLength);
    FixEmptyBlocks;
  finally
    UnlockUpdate;
  end;
end;

procedure TKMemoBlocks.ConcatEqualBlocks;
var
  I: Integer;
  Item, LastItem: TKMemoBlock;
begin
  // concat equal text blocks
  LockUpdate;
  try
    LastItem := nil;
    for I := 0 to Count - 1 do
    begin
      Item := Items[I];
      if Item is TKMemoContainer then
        TKmemoContainer(Item).Blocks.ConcatEqualBlocks
      else if (Item is TKMemoTextBlock) and (LastItem is TKMemoTextBlock) and not (Item is TKMemoParagraph) then
      begin
        if TKmemoTextBlock(Item).TextStyle.EqualProperties(TKMemoTextBlock(LastItem).TextStyle) then
        begin
          TKMemoTextBlock(LastItem).Concat(TKmemoTextBlock(Item));
          TKmemoTextBlock(Item).Text := '';
        end;
      end;
      LastItem := Item;
    end;
    // and delete empty blocks
    I := 0;
    while I < Count do
    begin
      Item := Items[I];
      if (Item is TKMemoTextBlock) and (Items[I].ContentLength = 0) then
        Delete(I)
      else
        Inc(I);
    end;
  finally
    UnlockUpdate;
  end;
end;

procedure TKMemoBlocks.DeleteBOL(At: Integer);
var
  LineStart: Integer;
  TmpPos: TKMemoLinePosition;
begin
  LineStart := LineStartIndexByIndex(At, True, TmpPos);
  Select(LineStart, At - LineStart);
  ClearSelection;
end;

procedure TKMemoBlocks.DeleteChar(At: Integer);
var
  NextIndex: Integer;
begin
  if SelLength <> 0 then
    ClearSelection
  else if not IndexAtEndOfContainer(At, True) then
  begin
    NextIndex := NextIndexByCharCount(At, 1);
    Select(At, NextIndex - At, True, True);
    ClearSelection;
  end;
end;

procedure TKMemoBlocks.DeleteEOL(At: Integer);
var
  LineEnd: Integer;
  TmpPos: TKMemoLinePosition;
begin
  LineEnd := LineEndIndexByIndex(At, True, True, TmpPos);
  Select(At, LineEnd - At);
  ClearSelection;
end;

procedure TKMemoBlocks.DeleteLastChar(At: Integer);
var
  LastIndex: Integer;
begin
  if SelLength <> 0 then
    ClearSelection
  else if not IndexAtBeginningOfContainer(At, True) then
  begin
    LastIndex := NextIndexByCharCount(At, -1);
    Select(LastIndex, At - LastIndex, True, True);
    ClearSelection;
  end;
end;

procedure TKMemoBlocks.DeleteLine(At: Integer);
var
  LineStart, LineEnd: Integer;
  TmpPos: TKMemoLinePosition;
begin
  LineStart := LineStartIndexByIndex(At, True, TmpPos);
  LineEnd := LineEndIndexByIndex(At, True, True, TmpPos);
  Select(LineStart, LineEnd - LineStart);
  ClearSelection;
end;

function TKMemoBlocks.EOLToNormal(var AIndex: Integer): Boolean;
begin
  Result := False;
  if GetLinePosition = eolEnd then
  begin
    Dec(AIndex);
    Result := True;
  end;
end;

procedure TKMemoBlocks.FixEmptyBlocks;
var
  I, SelectableCnt: Integer;
  Item: TKMemoBlock;
begin
  // do not leave empty blocks, always add single paragraph
  SelectableCnt := 0;
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if (Item.Position = mbpText) and not (Item is TKMemoContainer) then
      Inc(SelectableCnt);
  end;
  if SelectableCnt = 0 then
  begin
    AddParagraph;
    FSelEnd := 0;
    FSelStart := 0;
  end;
end;

procedure TKMemoBlocks.FixEOL(AIndex: Integer; AAdjust: Boolean; var ALinePos: TKMemoLinePosition);
var
  Item: TKMemoBlock;
  Line, LocalIndex: Integer;
begin
  if SelLength = 0 then
  begin
    if AAdjust then
      EOLToNormal(AIndex);
    if AIndex < 0 then
      ALinePos := eolInside
    else
    begin
      Item := IndexToItem(AIndex, LocalIndex);
      if Item is TKMemoContainer then
      begin
        TKMemoContainer(Item).Blocks.FixEOL(LocalIndex, False, ALinePos);
      end else
      begin
        Line := IndexToLine(AIndex);
        if (Line >= 0) and (AIndex >= LineEndIndex[Line]) then
        begin
          Item := Items[FLines[Line].EndBlock];
          if Item is TKMemoParagraph then
          begin
            ALinePos := eolInside;
          end
          else if AIndex > LineEndIndex[Line] then
            ALinePos := eolEnd;
        end;
      end;
    end;
    if ALinePos = eolInside then
    begin
      FSelStart := Min(FSelStart, FSelectableLength - 1);
      FSelEnd := Min(FSelEnd, FSelectableLength - 1);
    end;
  end;
end;

function TKMemoBlocks.GetBoundsRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

function TKMemoBlocks.GetDefaultTextStyle: TKMemoTextStyle;
begin
  if FParent <> nil then
    Result := FParent.DefaultTextStyle
  else if FMemoNotifier <> nil then
    Result := FMemoNotifier.GetDefaultTextStyle
  else
    Result := nil;
end;

function TKMemoBlocks.GetDefaultParaStyle: TKMemoParaStyle;
begin
  if FParent <> nil then
    Result := FParent.DefaultParaStyle
  else if FMemoNotifier <> nil then
    Result := FMemoNotifier.GetDefaultParaStyle
  else
    Result := nil;
end;

function TKMemoBlocks.GetEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TKMemoBlocks.GetFirstBlock: TKMemoBlock;
begin
  if Count > 0 then
    Result := Items[0]
  else
    Result := nil;
end;

function TKMemoBlocks.GetItem(Index: Integer): TKMemoBlock;
begin
  Result := TKMemoBlock(inherited GetItem(Index));
end;

function TKMemoBlocks.GetLastBlock: TKMemoBlock;
begin
  if Count > 0 then
    Result := Items[Count - 1]
  else
    Result := nil;
end;

function TKMemoBlocks.GetLastItemByClass(AIndex: Integer; AClass: TKMemoBlockClass): TKMemoBlock;
begin
  Result := nil;
  while (AIndex > 0) and (Result = nil) and not (Items[AIndex - 1] is TKMemoParagraph) do
  begin
    Dec(AIndex);
    if Items[AIndex] is AClass then
      Result := Items[AIndex];
  end;
end;

function TKMemoBlocks.GetLineBottom(ALineIndex: Integer): Integer;
begin
  Result := 0;
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
    Result := FLines[ALineIndex].Position.Y + FLines[ALineIndex].Extent.Y;
end;

function TKMemoBlocks.GetLineCount: Integer;
begin
  Result := FLines.Count;
end;

function TKMemoBlocks.GetLineEndIndex(ALineIndex: Integer): Integer;
begin
  Result := -1;
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
  begin
    Result := FLines[ALineIndex].EndIndex;
  end;
end;

function TKMemoBlocks.GetLineFloat(ALineIndex: Integer): Boolean;
var
  I: Integer;
  Item: TKMemoBlock;
begin
  Result := True;
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
  begin
    for I := FLines[ALineIndex].StartBlock to FLines[ALineIndex].EndBlock do
    begin
      Item := Items[I];
      if Item.WrapMode = wrNone then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function TKMemoBlocks.GetLineHeight(ALineIndex: Integer): Integer;
begin
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
    Result := FLines[ALineIndex].Extent.Y
  else
    Result := 0;
end;

function TKMemoBlocks.GetLineInfo(ALineIndex: Integer): TKMemoLine;
begin
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
    Result := FLines[ALineIndex]
  else
    Result := nil;
end;

function TKMemoBlocks.GetLineLeft(ALineIndex: Integer): Integer;
begin
  Result := 0;
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
    Result := FLines[ALineIndex].Position.X;
end;

function TKMemoBlocks.GetLinePosition: TKMemoLinePosition;
begin
  if FMemoNotifier <> nil then
    Result := FMemoNotifier.GetLinePosition
  else
    Result := eolInside;
end;

function TKMemoBlocks.GetLineRect(ALineIndex: Integer): TRect;
begin
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
    FLines[ALineIndex].LineRect
  else
    Result := CreateEmptyRect;
end;

function TKMemoBlocks.GetLineRight(ALineIndex: Integer): Integer;
begin
  Result := 0;
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
    Result := FLines[ALineIndex].Position.X + FLines[ALineIndex].Extent.X;
end;

function TKMemoBlocks.GetLineText(ALineIndex: Integer): TKString;
var
  I, J, St, En: Integer;
  Item: TKMemoBlock;
begin
  Result := '';
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
  begin
    for I := FLines[ALineIndex].StartBlock to FLines[ALineIndex].EndBlock do
    begin
      Item := Items[I];
      if Item.Position = mbpText then
      begin
        GetWordIndexes(I, ALineIndex, St, En);
        for J := St to En do
          Result := Result + Items[I].Words[J];
      end;
    end;
  end;
end;

function TKMemoBlocks.GetLineSize(ALineIndex: Integer): Integer;
var
  I, J, St, En: Integer;
  Item: TKMemoBlock;
begin
  Result := 0;
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
  begin
    for I := FLines[ALineIndex].StartBlock to FLines[ALineIndex].EndBlock do
    begin
      Item := Items[I];
      if Item.Position = mbpText then
      begin
        GetWordIndexes(I, ALineIndex, St, En);
        for J := St to En do
          Inc(Result, Items[I].WordLength[J]);
      end;
    end;
  end;
end;

function TKMemoBlocks.GetLineStartIndex(ALineIndex: Integer): Integer;
begin
  Result := -1;
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
  begin
    Result := FLines[ALineIndex].StartIndex;
  end;
end;

function TKMemoBlocks.GetLineTop(ALineIndex: Integer): Integer;
begin
  Result := 0;
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
    Result := FLines[ALineIndex].Position.Y;
end;

function TKMemoBlocks.GetLineWidth(ALineIndex: Integer): Integer;
begin
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
    Result := FLines[ALineIndex].Extent.X
  else
    Result := 0;
end;

function TKMemoBlocks.GetMaxWordLength: Integer;
begin
  if FMemoNotifier <> nil then
    Result := FMemoNotifier.GetMaxWordLength
  else
    Result := cMaxWordLengthDef;
end;

function TKMemoBlocks.GetNearestAnchorIndex(AIndex: Integer): Integer;
begin
  Result := -1;
  if AIndex >= 0 then
    while (Result < 0) and (AIndex >= 0) do
    begin
      if Items[AIndex] is TKMemoParagraph then
        Result := AIndex;
      Dec(AIndex);
    end;
end;

function TKMemoBlocks.GetNearestParagraphBlock(AIndex: Integer): Integer;
var
  Item: TKMemoBlock;
begin
  Result := -1;
  if AIndex >= 0 then
    while (Result < 0) and (AIndex < Count) do
    begin
      Item := Items[AIndex];
      if Item is TKMemoParagraph then
        Result := AIndex;
      Inc(AIndex);
    end;
end;

function TKMemoBlocks.GetNearestParagraphItem(AIndex: Integer): TKMemoParagraph;
var
  Block: Integer;
begin
  Block := GetNearestParagraphBlock(AIndex);
  if Block >= 0 then
    Result := BlockToItem(Block) as TKMemoParagraph
  else
    Result := nil;
end;

function TKMemoBlocks.GetNearestWordIndexes(AIndex: Integer; AAdjust: Boolean;
  AIncludeWhiteSpaces: Boolean; out AStart, AEnd: Integer): Boolean;
var
  I, J, CurIndex, LastIndex, BackupBlock, BackupWord, CurBlock, CurWord, WLen, WLenWOWS: Integer;
  IsBreakable: Boolean;
  Item: TKMemoBlock;
begin
  Result := False;
  if AAdjust then
    EOLToNormal(AIndex);
  if AIndex >= 0 then
  begin
    CurBlock := -1;
    CurWord := -1;
    I := 0;
    CurIndex := 0;
    LastIndex := 0;
    while (CurBlock < 0) and (I < Count) do
    begin
      Item := Items[I];
      LastIndex := CurIndex;
      Inc(CurIndex, Item.SelectableLength);
      if (AIndex >= LastIndex) and (AIndex < CurIndex) then
      begin
        CurBlock := I;
        if Item is TKMemoContainer then
        begin
          Result := TKMemoContainer(Item).Blocks.GetNearestWordIndexes(AIndex - LastIndex,
            False, AIncludeWhiteSpaces, AStart, AEnd);
          if Result then
          begin
            Inc(AStart, LastIndex);
            Inc(AEnd, LastIndex);
          end;
        end else
        begin
          J := 0;
          while (CurWord < 0) and (J < Item.WordCount) and (LastIndex <= AIndex) do
          begin
            WLen := Item.WordLength[J];
            if AIncludeWhiteSpaces then
              WLenWOWS := WLen
            else
              WLenWOWS := Item.WordLengthWOWS[J];
            if (AIndex >= LastIndex) and (AIndex < LastIndex + WLenWOWS) then
              CurWord := J
            else
              Inc(LastIndex, WLen);
            Inc(J);
          end;
        end;
      end;
      Inc(I);
    end;
    if (CurBlock >= 0) and (CurWord >= 0) then
    begin
      Result := True;
      BackupBlock := CurBlock;
      BackupWord := CurWord;
      AStart := LastIndex;
      AEnd := LastIndex;
      // we've found the word
      // go back and find first nonbreakable word
      Dec(CurWord);
      if CurWord < 0 then
        Dec(CurBlock);
      IsBreakable := False;
      while not IsBreakable and (CurBlock >= 0) do
      begin
        Item := Items[CurBlock];
        if CurWord < 0 then
          CurWord := Item.WordCount - 1;
        if not (Item is TKMemoTextBlock) then
          IsBreakable := True
        else
        begin
          while not IsBreakable and (CurWord >= 0) do
          begin
            IsBreakable := Item.WordBreakable[CurWord];
            if not Isbreakable then
            begin
              Dec(AStart, Item.WordLength[CurWord]);
              Dec(CurWord);
            end;
          end;
          CurWord := -1;
          Dec(CurBlock);
        end;
      end;
      // go forward and find first nonbreakable word
      CurBlock := BackupBlock;
      CurWord := BackupWord;
      IsBreakable := False;
      while not IsBreakable and (CurBlock < Count) do
      begin
        Item := Items[CurBlock];
        if not (Item is TKMemoTextBlock) then
          IsBreakable := True
        else
        begin
          while not IsBreakable and (CurWord < Item.WordCount) do
          begin
            IsBreakable := Item.WordBreakable[CurWord];
            if not IsBreakable or AIncludeWhiteSpaces then
              WLen := Item.WordLength[CurWord]
            else
              WLen := Item.WordLengthWOWS[CurWord];
            if not (Item is TKMemoParagraph) then              
              Inc(AEnd, WLen);
            Inc(CurWord);
          end;
          CurWord := 0;
          Inc(CurBlock);
        end;
      end;
    end;
  end;
end;

function TKMemoBlocks.GetNextItemByClass(AIndex: Integer; AClass: TKMemoBlockClass): TKMemoBlock;
begin
  Result := nil;
  while (AIndex < Count - 1) and (Result = nil) and not (Items[AIndex + 1] is TKMemoParagraph) do
  begin
    Inc(AIndex);
    if Items[AIndex] is AClass then
      Result := Items[AIndex];
  end;
end;

function TKMemoBlocks.GetPageCount(APageHeight: Integer): Integer;
var
  I, J, TmpPageBegin, MaxLine: Integer;
  R: TRect;
begin
  Result := 1; // always at least one page
  if FLines.Count > 0 then
    TmpPageBegin := FLines[0].Position.Y
  else
    TmpPageBegin := 0;
  I := 0;
  J := -1;
  MaxLine := TotalLineCount - 1;
  while I <= MaxLine do
  begin
    if I <> J then
    begin
      R := TotalLineRect[I];
      J := I;
    end;
    if R.Bottom - TmpPageBegin > APageHeight then
    begin
      TmpPageBegin := R.Top;
      if R.Bottom - R.Top > APageHeight then
        Inc(TmpPageBegin, APageHeight)
      else
        Inc(I);
      Inc(Result);
    end else
      Inc(I);
  end;
end;

procedure TKMemoBlocks.GetPageData(APageHeight, APage: Integer; out AOffset, AHeight: Integer);
var
  I, J, TmpPageBegin, TmpPage, MaxLine: Integer;
  R: TRect;
begin
  TmpPage := 0;
  if FLines.Count > 0 then
    TmpPageBegin := FLines[0].Position.Y
  else
    TmpPageBegin := 0;
  I := 0;
  J := -1;
  MaxLine := TotalLineCount - 1;
  while (I <= MaxLine) and (TmpPage <= APage) do
  begin
    if I <> J then
    begin
      R := TotalLineRect[I];
      J := I;
    end;
    if R.Bottom - TmpPageBegin > APageHeight then
    begin
      if TmpPage < APage then
      begin
        TmpPageBegin := R.Top;
        if R.Bottom - R.Top > APageHeight then
          Inc(TmpPageBegin, APageHeight)
        else
          Inc(I);
        Inc(TmpPage);
      end else
      begin
        // finished measuring current page
        R.Bottom := R.Top; // take bottom coord of previous line
        Break;
      end;
    end else
      Inc(I);
  end;
  AOffset := TmpPageBegin;
  AHeight := Min(R.Bottom - TmpPageBegin, APageHeight);
end;

function TKMemoBlocks.GetParentBlocks: TKMemoBlocks;
begin
  if FParent is TKMemoContainer then
    Result := TKMemoContainer(FParent).ParentBlocks
  else
    Result := nil;
end;

function TKMemoBlocks.GetParentMemo: TKCustomMemo;
begin
  if FMemoNotifier <> nil then
    Result := FMemoNotifier.GetMemo
  else
    Result := nil;
end;

function TKMemoBlocks.GetRealSelEnd: Integer;
begin
  if FSelStart <= FSelEnd then
    Result := FSelEnd
  else
    Result := FSelStart;
end;

function TKMemoBlocks.GetRealSelLength: Integer;
begin
  Result := RealSelEnd - RealSelStart;
end;

function TKMemoBlocks.GetRealSelStart: Integer;
begin
  if FSelStart <= FSelEnd then
    Result := FSelStart
  else
    Result := FSelEnd;
end;

procedure TKMemoBlocks.GetSelColors(out TextColor, Background: TColor);
begin
  if FMemoNotifier <> nil then
    FMemoNotifier.GetSelColors(TextColor, Background);
end;

function TKMemoBlocks.GetSelectionHasPara: Boolean;
var
  I, CurIndex, LastIndex, TmpSelEnd, TmpSelStart: Integer;
  Item: TKMemoBlock;
begin
  Result := False;
  if SelLength > 0 then
  begin
    TmpSelEnd := RealSelEnd;
    TmpSelStart := RealSelStart;
    I := 0;
    CurIndex := 0;
    while not Result and (I < Count) do
    begin
      Item := Items[I];
      LastIndex := CurIndex;
      Inc(CurIndex, Item.SelectableLength);
      if (LastIndex <= TmpSelEnd) and (TmpSelStart < CurIndex) then
      begin
        if Item is TKMemoContainer then
           Result := TKMemoContainer(Item).Blocks.SelectionHasPara
        else if Item is TKMemoParagraph then
          Result := True;
      end;
      Inc(I);
    end;
  end
end;

function TKMemoBlocks.GetSelectionParaStyle: TKMemoParaStyle;
var
  Item: TKMemoBlock;
  Block, LocalIndex: Integer;
begin
  Block := IndexToBlock(RealSelEnd, LocalIndex);
  if Block >= 0 then
  begin
    Item := Items[Block];
    if Item is TKMemoContainer then
      Result := TKMemoContainer(Item).Blocks.SelectionParaStyle
    else
    begin
      Item := GetNearestParagraphItem(Block);
      if Item <> nil then
        Result := Item.ParaStyle
      else
        Result := nil;
    end;
  end else
    Result := nil;
end;

function TKMemoBlocks.GetSelectionTextStyle: TKMemoTextStyle;
var
  Item, LastItem: TKmemoBlock;
  Block, LocalIndex: Integer;
begin
  Result := nil;
  Block := IndexToBlock(RealSelEnd, LocalIndex);
  if Block >= 0 then
  begin
    if Block > 0 then    
      LastItem := Items[Block - 1]
    else
      LastItem := nil;
    if (LocalIndex > 0) or (LastItem is TKMemoParagraph) or not (LastItem is TKMemoTextBlock) then
    begin    
      Item := Items[Block];
      if Item is TKMemoContainer then
        Result := TKMemoContainer(Item).Blocks.SelectionTextStyle
      else if Item is TKMemoTextBlock then
        Result := TKMemoTextBlock(Item).TextStyle;
    end 
    else if LastItem <> nil then
    begin
      Result := TKMemoTextBlock(LastItem).TextStyle
    end;
  end;
end;

function TKMemoBlocks.GetSelLength: Integer;
begin
  Result := FSelEnd - FSelStart;
end;

function TKMemoBlocks.GetSelText: TKString;
var
  I: Integer;
  Item: TKMemoBlock;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if Item.SelLength > 0 then
      Result := Result + Item.SelText;
  end;
  Result := UnicodeStringReplace(Result, NewLineChar, cEOL, [rfReplaceAll]);
end;

function TKMemoBlocks.GetShowFormatting: Boolean;
begin
  if FMemoNotifier <> nil then
    Result := FMemoNotifier.GetShowFormatting
  else
    Result := False;
end;

function TKMemoBlocks.GetText: TKString;
var
  I: Integer;
  Item: TKMemoBlock;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    Result := Result + Item.Text;
  end;
  Result := UnicodeStringReplace(Result, NewLineChar, cEOL, [rfReplaceAll]);
end;

function TKMemoBlocks.GetTotalLeftOffset: Integer;
begin
  if FParent <> nil then
  begin
    Result := FParent.Left + FParent.LeftOffset;
    if FParent is TKMemoContainer then
      Inc(Result, TKMemoContainer(FParent).BlockStyle.AllPaddingsLeft + TKMemoContainer(FParent).ParentBlocks.TotalLeftOffset);
  end else
    Result := 0;
end;

function TKMemoBlocks.GetTotalLineCount: Integer;
var
  I: Integer;
  Line: TKMemoLine;
  Item: TKMemoBlock;
begin
  Result := 0;
  for I := 0 to FLines.Count - 1 do
  begin
    Line := FLines[I];
    Item := Items[Line.StartBlock];
    if Item is TKMemoContainer then
      Inc(Result, TKMemoContainer(Item).TotalLineCount)
    else
      Inc(Result);
  end;
end;

function TKMemoBlocks.GetTotalLineRect(Index: Integer): TRect;
var
  I, TmpIndex, TmpCount: Integer;
  Line: TKMemoLine;
  Item: TKMemoBlock;
  R: TRect;
begin
  Result := CreateEmptyRect;
  TmpIndex := 0;
  for I := 0 to FLines.Count - 1 do
  begin
    Line := FLines[I];
    Item := Items[Line.StartBlock];
    if Item is TKMemoContainer then
    begin
      TmpCount := TKMemoContainer(Item).TotalLineCount;
      if (Index >= TmpIndex) and (Index < TmpIndex + TmpCount) then
      begin
        R := TKMemoContainer(Item).TotalLineRect[Index - TmpIndex];
        if R.Bottom = R.Top then // empty line info
          Result := Line.LineRect
        else
          Result := R;
        Break;
      end else
        Inc(TmpIndex, TmpCount);
    end else
    begin
      if Index = TmpIndex then
      begin
        Result := Line.LineRect;
        Break;
      end else
        Inc(TmpIndex);
    end;
  end;
end;

function TKMemoBlocks.GetTotalTopOffset: Integer;
begin
  if FParent <> nil then
  begin
    Result := FParent.Top + FParent.TopOffset + FParent.WordTopPadding[0];
    if FParent is TKMemoContainer then
      Inc(Result, TKMemoContainer(FParent).BlockStyle.AllPaddingsTop + TKMemoContainer(FParent).ParentBlocks.TotalTopOffset);
  end else
    Result := 0;
end;

procedure TKMemoBlocks.GetWordIndexes(const ABlockIndex, ALineIndex: Integer; out AStart,
  AEnd: Integer);
begin
  if ABlockIndex = FLines[ALineIndex].StartBlock then
    AStart := FLines[ALineIndex].StartWord
  else
    AStart := 0;
  if ABlockIndex = FLines[ALineIndex].EndBlock then
    AEnd := FLines[ALineIndex].EndWord
  else
    AEnd := Items[ABlockIndex].WordCount - 1;
end;

function TKMemoBlocks.IndexAboveLastLine(AIndex: Integer; AAdjust: Boolean): Boolean;
var
  Item: TKMemoBlock;
  Line, LocalIndex: Integer;
begin
  if AAdjust then  
    EOLToNormal(AIndex);
  Line := IndexToLine(AIndex);
  Result := Line < FLines.Count - 1;
  if not Result then
  begin
    Item := IndexToItem(AIndex, LocalIndex);
    if Item is TKMemoContainer then
      Result := TKMemoContainer(Item).Blocks.IndexAboveLastLine(LocalIndex, False)
  end;
end;

function TKMemoBlocks.IndexAtBeginningOfContainer(AIndex: Integer; AAdjust: Boolean): Boolean;
var
  Item: TKMemoBlock;
  LocalIndex: Integer;
begin
  if AAdjust then
    EOLToNormal(AIndex);
  Item := IndexToItem(AIndex, LocalIndex);
  if Item is TKMemoContainer then
    Result := TKMemoContainer(Item).Blocks.IndexAtBeginningOfContainer(LocalIndex, False)
  else
  begin
    NormalToEOL(AIndex);
    Result := AIndex <= 0;
  end;
end;

function TKMemoBlocks.IndexAtEndOfContainer(AIndex: Integer; AAdjust: Boolean): Boolean;
var
  Item: TKMemoBlock;
  LocalIndex: Integer;
begin
  if AAdjust then
    EOLToNormal(AIndex);
  Item := IndexToItem(AIndex, LocalIndex);
  if Item is TKMemoContainer then
    Result := TKMemoContainer(Item).Blocks.IndexAtEndOfContainer(LocalIndex, False)
  else
  begin
    NormalToEOL(AIndex);
    Result := AIndex >= FSelectableLength;
  end;
end;

function TKMemoBlocks.IndexBelowFirstLine(AIndex: Integer; AAdjust: Boolean): Boolean;
var
  Item: TKMemoBlock;
  Line, LocalIndex: Integer;
begin
  if AAdjust then
    EOLToNormal(AIndex);
  Line := IndexToLine(AIndex);
  Result := Line > 0;
  if not Result then
  begin
    Item := IndexToItem(AIndex, LocalIndex);
    if Item is TKMemoContainer then
      Result := TKMemoContainer(Item).Blocks.IndexBelowFirstLine(LocalIndex, False)
  end;
end;

function TKMemoBlocks.IndexToBlock(AIndex: Integer; out ALocalIndex: Integer): Integer;
var
  I, CurIndex, LastIndex: Integer;
begin
  Result := -1;
  ALocalIndex := -1;
  if AIndex >= 0 then
  begin
    if AIndex < FSelectableLength then
    begin
      I := 0;
      CurIndex := 0;
      while (Result < 0) and (I < Count) do
      begin
        LastIndex := CurIndex;
        Inc(CurIndex, Items[I].SelectableLength);
        if (AIndex >= LastIndex) and (AIndex < CurIndex) then
        begin
          Result := I;
          ALocalIndex := AIndex - LastIndex;
        end;
        Inc(I);
      end;
    end
    else if Count > 0 then
    begin
      Result := Count - 1;
      ALocalIndex := Items[Result].SelectableLength;
    end;
  end
end;

function TKMemoBlocks.IndexToItems(AIndex: Integer; out ALocalIndex: Integer): TKMemoBlocks;
var
  Item: TKMemoBlock;
  LocalIndex: Integer;
begin
  Item := IndexToItem(AIndex, LocalIndex);
  if Item is TKMemoContainer then
    Result := TKMemoContainer(Item).Blocks.IndexToItems(LocalIndex, ALocalIndex)
  else
  begin
    Result := Self;
    ALocalIndex := AIndex;
  end;
end;

function TKMemoBlocks.IndexToItem(AIndex: Integer; out ALocalIndex: Integer): TKMemoBlock;
var
  Block: Integer;
begin
  Block := IndexToBlock(AIndex, ALocalIndex);
  if Block >= 0 then
    Result := Items[Block]
  else
    Result := nil;
end;

function TKMemoBlocks.IndexToLine(AIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (AIndex >= 0) and (AIndex < FSelectableLength) then
  begin
    for I := 0 to LineCount - 1 do
    begin
      if (AIndex >= FLines[I].StartIndex) and (AIndex <= FLines[I].EndIndex) then
      begin
        Result := I;
        Break;
      end;
    end;
  end
  else if AIndex = FSelectableLength then
    Result := LineCount - 1;
end;

function TKMemoBlocks.IndexToRect(ACanvas: TCanvas; AIndex: Integer; ACaret, AAdjust: Boolean): TRect;
var
  Line, Tmp: Integer;
begin
  Line := IndexToLine(AIndex);
  if Line >= 0 then
  begin
    Result := LineToRect(ACanvas, AIndex, Line, ACaret);
    if AAdjust then
    begin
      // move the rectangle to the right
      Tmp := Result.Right - Result.Left;
      Result.Left := Result.Right;
      Result.Right := Result.Left + Tmp;
    end;
    if not ACaret then
    begin
      // expand rect to enable vertical caret movement
      if (Line > 0) and (Result.Top = LineTop[Line]) then
      begin
        Tmp := LineTop[Line] - LineBottom[Line - 1];
        if Tmp > 0 then
          Dec(Result.Top, Tmp);
      end;
      if (Line < LineCount - 1) and (Result.Bottom = LineBottom[Line]) then
      begin
        Tmp := LineTop[Line + 1] - LineBottom[Line];
        if Tmp > 0 then
          Inc(Result.Bottom, Tmp);
      end;
    end;
  end else
    Result := Rect(0, 0, 0, Abs(GetDefaultTextStyle.Font.Height));
end;

procedure TKMemoBlocks.InsertChar(At: Integer; const AValue: TKChar; AOverWrite: Boolean; ATextStyle: TKMemoTextStyle);
var
  NextIndex: Integer;
begin
  if SelLength <> 0 then
  begin
    ClearSelection;
    At := FSelEnd;
  end
  else if AOverwrite then
    DeleteChar(At);
  if InsertString(At, True, AValue, ATextStyle) then
  begin
    NextIndex := NextIndexByCharCount(At, 1);
    Select(NextIndex, 0, True, True);
  end;
end;

procedure TKMemoBlocks.InsertNewLine(At: Integer);
var
  NextIndex: Integer;
  AtEnd: Boolean;
begin
  if SelLength > 0 then
  begin
    ClearSelection;
    At := FSelEnd;
  end;
  AtEnd := IndexAtEndOfContainer(At, True);
  // always insert (don't overwrite)
  if InsertParagraph(At, True) and not AtEnd then
  begin
    NextIndex := NextIndexByCharCount(At, 1);
    Select(NextIndex, 0, True, True);
  end;
end;

function TKMemoBlocks.InsertParagraph(AIndex: Integer; AAdjust: Boolean): Boolean;
var
  Block, LocalIndex: Integer;
  Item: TKMemoBlock;
begin
  if AAdjust then
    EOLToNormal(AIndex);
  LockUpdate;
  try
    Block := IndexToBlock(AIndex, LocalIndex);
    if Block >= 0 then
    begin
      Item := Items[Block];
      if not (Item is TKMemoContainer) then
        NormalToEOL(LocalIndex);
      Result := Item.InsertParagraph(LocalIndex);
    end else
    begin
      AddParagraph;
      Result := True;
    end;
  finally
    UnlockUpdate;
  end;
end;

procedure TKMemoBlocks.InsertPlainText(AIndex: Integer; const AValue: TKString);
var
  I, Ln, St: Integer;
  S: TKString;
begin
  LockUpdate;
  try
    St := 1;
    I := 1;
    Ln := Length(Avalue);
    while I <= Ln do
    begin
      if AValue[I] = cLF then
      begin
        if I > St then
        begin
          S := Copy(AValue, St, I - St);
          S := UnicodeStringReplace(S, cCR, '', [rfReplaceAll]); // on Unix systems
          if (S <> '') and InsertString(AIndex, False, S) then
            Inc(AIndex, StringLength(S));
        end;
        if InsertParagraph(AIndex, True) then
          Inc(AIndex);
        St := I + 1;
      end;
      Inc(I);
    end;
    if I > St then
    begin
      S := Copy(AValue, St, I - St + 1);
      if S <> '' then
        InsertString(AIndex, True, S);
    end;
  finally
    UnlockUpdate;
  end;
end;

function TKMemoBlocks.InsertString(AIndex: Integer; AAdjust: Boolean; const AValue: TKString; ATextStyle: TKMemoTextStyle): Boolean;
var
  Block, LocalIndex: Integer;
  Item, NewItem, LastItem, NextItem: TKMemoBlock;
begin
  Result := False;
  if AAdjust then
    EOLToNormal(AIndex);
  LockUpdate;
  try
    Block := IndexToBlock(AIndex, LocalIndex);
    if Block >= 0 then
    begin
      Item := Items[Block];
      if not (Item is TKMemoContainer) then
        NormalToEOL(LocalIndex);
      NewItem := nil;
      // get last item
      if Block > 0 then
        LastItem := Items[Block - 1]
      else
        LastItem := nil;        
      // proceed with adding text
      if ATextStyle <> nil then
      begin
        if LocalIndex = 0 then
        begin
          // insert new text block
          NewItem := AddTextBlock(AValue, Block);
        end else
        begin
          // split current block to add new block with different text style in between
          NextItem := Item.Split(LocalIndex);
          AddAt(NextItem, Block + 1);
          NewItem := AddTextBlock(AValue, Block + 1);
        end;
      end
      else if LocalIndex = 0 then
      begin
        // we are at local position 0 so we can use previous text block or add new one
        if (LastItem is TKMemoTextBlock) and LastItem.CanAddText then
        begin
          // insert character at the end of last block
          Result := LastItem.InsertString(AValue);
        end
        else if Item.CanAddText then
        begin
          // insert character at the beginning of current block if previous one cannot be used
          Result := Item.InsertString(AValue, LocalIndex);
        end else
        begin
          // insert new text block if current block cannot add text
          NewItem := AddTextBlock(AValue, Block);
        end;
      end else
      begin
        // we are in the middle of current block
        if Item.CanAddText then
        begin
          // current block can insert text, so do it at given location
          Result := Item.InsertString(AValue, LocalIndex);
        end
        else if LocalIndex = Item.ContentLength then
        begin
          // current block cannot insert text, so insert new text block
          // but only when we are at the end of the current block
          NewItem := AddTextBlock(AValue, Block + 1);
        end;
      end;
      if NewItem <> nil then
      begin
        Block := IndexOf(NewItem);
        // get last and next items
        if Block > 0 then
          LastItem := Items[Block - 1]
        else
          LastItem := nil;
        if Block < Count - 1 then
          NextItem := Items[Block + 1]
        else
          NextItem := nil;
        // assign attributes from last or next block
        if LastItem is TKMemoParagraph then
        begin
          // beginning of new line, so take from next text block and,
          // if there is none, take from last or next paragraph
          Item := GetNextItemByClass(Block, TKMemoTextBlock);
          if Item <> nil then
            NewItem.AssignAttributes(Item)
          else if NextItem is TKMemoParagraph then
            NewItem.AssignAttributes(NextItem)
          else
            NewItem.AssignAttributes(LastItem);
        end else
        begin
          // otherwise take from last text block and, if there is none,
          // take from next text block
          Item := GetLastItemByClass(Block, TKMemoTextBlock);
          if Item <> nil then
            NewItem.AssignAttributes(Item)
          else
          begin
            Item := GetNextItemByClass(Block, TKMemoTextBlock);
            if Item <> nil then
              NewItem.AssignAttributes(Item)
            else if NextItem is TKMemoParagraph then
              NewItem.AssignAttributes(NextItem);
          end;
        end;
        if (ATextStyle <> nil) and (NewItem is TKMemoTextBlock) then
          TKMemoTextBlock(NewItem).TextStyle.Assign(ATextStyle);
        Result := True;
      end;
    end else
    begin
      AddTextBlock(AValue);
      Result := True;
    end;
  finally
    UnlockUpdate;
  end;
end;

function TKMemoBlocks.InsideOfTable: Boolean;
begin
  if FParent is TKMemoTable then
    Result := True
  else if FParent is TKMemoContainer then
    Result := TKMemoContainer(FParent).ParentBlocks.InsideOfTable
  else
    Result := False;
end;

function TKMemoBlocks.ItemToIndex(AItem: TKMemoBlock): Integer;
var
  I, LastIndex: Integer;
  Item: TKMemoBlock;
begin
  Result := -1;
  if AItem.Position = mbpText then
  begin
    LastIndex := 0;
    for I := 0 to Count - 1 do
    begin
      Item := Items[I];
      if AItem = Item then
      begin
        Result := LastIndex;
        Exit;
      end
      else if Item is TKMemoContainer then
      begin
        Result := TKMemoContainer(Item).Blocks.ItemToIndex(AItem);
        if Result >= 0 then
        begin
          Inc(Result, LastIndex);
          Exit;
        end;
      end;
      Inc(LastIndex, Item.SelectableLength);
    end;
  end;
end;

function TKMemoBlocks.LastTextStyle(AIndex: Integer): TKMemoTextStyle;
var
  Item: TKMemoBlock;
begin
  Item := GetLastItemByClass(AIndex, TKMemoTextBlock);
  if Item is TKMemoTextBlock then
    Result := TKMemoTextBlock(Item).TextStyle
  else
    Result := DefaultTextStyle;
end;

function TKMemoBlocks.LineEndIndexByIndex(AIndex: Integer; AAdjust, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer;
var
  Item: TKMemoBlock;
  Line, LocalIndex: Integer;
begin
  Result := -1;
  if AAdjust then
    EOLToNormal(AIndex);
  ALinePos := eolInside;
  Item := IndexToItem(AIndex, LocalIndex);
  if Item is TKMemoContainer then
  begin
    Result := TKMemoContainer(Item).Blocks.LineEndIndexByIndex(LocalIndex, False, ASelectionExpanding, ALinePos);
    if Result >= 0 then
      Inc(Result, AIndex - LocalIndex);
  end;
  if Result < 0 then
  begin
    Line := IndexToLine(AIndex);
    if Line >= 0 then
    begin
      Result := LineEndIndex[Line];
      Item := Items[FLines[Line].EndBlock];
      if ASelectionExpanding or not (Item is TKMemoParagraph) then
      begin
        ALinePos := eolEnd;
        Inc(Result);
      end;
    end else
      Result := 0;
  end;
end;

function TKMemoBlocks.LineStartIndexByIndex(AIndex: Integer; AAdjust: Boolean; out ALinePos: TKMemoLinePosition): Integer;
var
  Item: TKMemoBlock;
  Line, LocalIndex: Integer;
begin
  Result := -1;
  if AAdjust then
    EOLToNormal(AIndex);
  ALinePos := eolInside;
  Item := IndexToItem(AIndex, LocalIndex);
  if Item is TKMemoContainer then
  begin
    Result := TKMemoContainer(Item).Blocks.LineStartIndexByIndex(LocalIndex, False, ALinePos);
    if Result >= 0 then
      Inc(Result, AIndex - LocalIndex);
  end;
  if Result < 0 then
  begin
    Line := IndexToLine(AIndex);
    if Line >= 0 then
      Result := LineStartIndex[Line]
    else
      Result := 0;
  end;
end;

function TKMemoBlocks.LineToRect(ACanvas: TCanvas; AIndex, ALineIndex: Integer; ACaret: Boolean): TRect;
var
  I, J, LastIndex, CurIndex, St, En: Integer;
  Item: TKMemoBlock;
  TmpRect: TRect;
  Found, TmpFound: Boolean;
begin
  Result := CreateEmptyRect;
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
  begin
    Found := False;
    TmpFound := False;
    CurIndex := FLines[ALineIndex].StartIndex;
    I := Flines[ALineIndex].StartBlock;
    while not Found and (I <= FLines[ALineIndex].EndBlock) do
    begin
      Item := Items[I];
      if Item.Position = mbpText then
      begin
        GetWordIndexes(I, ALineIndex, St, En);
        J := St;
        while not Found and (J <= En) do
        begin
          LastIndex := CurIndex;
          Inc(CurIndex, Item.WordLength[J]);
          if (AIndex = CurIndex) and (Item is TKMemoTextBlock) then
          begin
            // take rectangle from last word
            TmpRect := Item.WordIndexToRect(ACanvas, J, AIndex - LastIndex - 1, ACaret);
            TmpFound := not IsRectEmpty(TmpRect);
          end
          else if (AIndex >= LastIndex) and (AIndex < CurIndex) then
          begin
            Result := Item.WordIndexToRect(ACanvas, J, AIndex - LastIndex, ACaret);
            if TmpFound and ACaret then
            begin
              // simulate caret height from last word
              // it is better for subscripts and superscripts
              Result.Top := TmpRect.Top;
              Result.Bottom := TmpRect.Bottom;
            end;
            Found := True;
          end;
          Inc(J);
        end;
      end;
      Inc(I);
    end;
  end;
end;

procedure TKMemoBlocks.ListChanged(AList: TKMemoList; ALevel: TKMemoListLevel);
var
  I: Integer;
  Item: TKMemoBlock;
  PA: TKMemoParagraph;
begin
  // update indentation for all list bound paragraphs according to list level info
  if (AList <> nil) and (ALevel <> nil) then
  begin
    LockUpdate;
    try
      for I := 0 to Count - 1 do
      begin
        Item := Items[I];
        if Item is TKMemoParagraph then
        begin
          PA := TKmemoParagraph(Item);
          if (PA.NumberingList = AList) and (ALevel = PA.NumberingListLevel) then
          begin
            PA.ParaStyle.FirstIndent := ALevel.FirstIndent;
            PA.ParaStyle.LeftPadding := ALevel.LeftIndent;
          end;
        end
        else if Item is TKMemoContainer then
          TKmemoContainer(Item).Blocks.ListChanged(AList, ALevel);
      end;
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKMemoBlocks.LoadFromRTFStream(AStream: TStream; AtIndex: Integer);
var
  Reader: TKMemoRTFReader;
begin
  Reader := TKMemoRTFReader.Create(ParentMemo);
  try
    Reader.LoadFromStream(AStream, AtIndex, Self);
  finally
    Reader.Free;
  end;
end;

procedure TKMemoBlocks.MeasureExtent(ACanvas: TCanvas; ARequiredWidth: Integer);
var
  PosX, PosY, Right, CurBlock, CurIndex, CurWord, CurTotalWord, LineHeight, ParaWidth, ParaPosY, LastBlock, LastIndex, LastWord, LastTotalWord: Integer;
  CurParaStyle: TKMemoParaStyle;
  CurParagraph: TKMemoParagraph;

  function GetParaStyle(AParagraph: TKMemoParagraph): TKMemoParaStyle;
{  var
    Para: TKMemoParagraph;}
  begin
{    if (ABlockIndex >= 0) and (ABlockIndex < Count) then
      Result := Items[ABlockIndex].ParaStyle
    else
      Result := nil;
    if Result = nil then
    begin
      Para := GetNearestParagraph(ABlockIndex);}
      if AParagraph <> nil then
        Result := AParagraph.ParaStyle
      else
        Result := DefaultParaStyle;
//    end;
  end;

  function RectCollidesWithNonText(const ARect: TRect; var ACollisionRect: TRect): Boolean;
  var
    I: Integer;
    Item: TKMemoBlock;
    DoCheck: Boolean;
  begin
    Result := False;
    I := 0;
    while not Result and (I < FRelPos.Count) do
    begin
      Item := Items[FRelPos[I].Index];
      //if (Item.Position = mbpAbsolute) or (CurBlock > FRelPos[I].Index) then
      begin
        ACollisionRect := Item.BoundsRect;
        KFunctions.OffsetRect(ACollisionRect, Item.LeftOffset, Item.TopOffset);
        DoCheck := True;
        case Item.WrapMode of
          wrAround, wrTight:;
          wrAroundLeft, wrTightLeft: ACollisionRect.Right := Right;
          wrAroundRight, wrTightRight: ACollisionRect.Left := CurParaStyle.LeftPadding;
          wrTopBottom:
          begin
            ACollisionRect.Left := CurParaStyle.LeftPadding;
            ACollisionRect.Right := Right;
          end;
        else
          DoCheck := False;
        end;
        if DoCheck then
          Result := RectInRect(ACollisionRect, ARect);
      end;
      Inc(I);
    end;
  end;

  function AddLine: Boolean;
  var
    NumberBlock: TKmemoTextBlock;

    procedure MoveWordsOnLine(ALineIndex, AStartPos, AEndPos, ADelta: Integer; var AChunkCnt: Integer);
    var
      I, J, St, En: Integer;
      Item: TKMemoBlock;
    begin
      for I := FLines[ALineIndex].StartBlock to FLines[ALineIndex].EndBlock do
      begin
        Item := Items[I];
        if Item.Position = mbpText then
        begin
          GetWordIndexes(I, ALineIndex, St, En);
          for J := St to En do
          begin
            if (Item.WordLeft[J] >= AStartPos) and (Item.WordLeft[J] + Item.WordWidth[J] <= AEndPos) then
              Item.WordLeft[J] := Item.WordLeft[J] + ADelta;
          end;
        end;
      end;
      if (AChunkCnt = 0) and (NumberBlock <> nil) then
      begin
        for J := 0 to NumberBlock.WordCount - 1 do
        begin
          NumberBlock.WordLeft[J] := NumberBlock.WordLeft[J] + ADelta;
        end;
      end;
      Inc(AChunkCnt);
    end;

  var
    Line, LastLine: TKMemoLine;
    EndItem, Item: TKMemoBlock;
    I, J, W, CurWordCopy, CurBlockCopy, CurIndexCopy, Delta, FirstIndent, ChunkCnt,
    LineIndex, LineLeft, LineRight, BaseLine, StPosX, St, En, ParaMarkWidth, BottomPadding, TopPadding: Integer;
    WasParagraph: Boolean;
    R, RW: TRect;
  begin
    Result := False;
    if LastTotalWord <> CurTotalWord then
    begin
      LastTotalWord := CurTotalWord;
      // create new line
      if FLines.Count > 0 then
        LastLine := FLines[FLines.Count - 1]
      else
        LastLine := nil;
      CurIndexCopy := CurIndex;
      CurWordCopy := CurWord;
      CurBlockCopy := CurBlock;
      if (CurWordCopy <= 0) or (CurBlockCopy >= Count) or (Items[CurBlockCopy].Position <> mbpText) then
      begin
        I := 0;
        while (CurBlockCopy > LastBlock) and ((CurBlockCopy >= Count) or (I = 0) or (Items[CurBlockCopy].Position <> mbpText)) do
        begin
          Dec(CurBlockCopy);
          Inc(I);
        end;
        CurWordCopy := Items[CurBlockCopy].WordCount - 1;
      end else
      begin
        Dec(CurWordCopy);
      end;
      Dec(CurIndexCopy);
      Line := TKMemoLine.Create;
      LineIndex := FLines.Add(Line);
      Line.StartBlock := LastBlock;
      Line.EndBlock := CurBlockCopy;
      Line.StartIndex := LastIndex;
      Line.EndIndex := CurIndexCopy;
      Line.StartWord := LastWord;
      Line.EndWord := CurWordCopy;

      EndItem := Items[Line.EndBlock];
      // get vertical paddings for this line and width of the paragraph mark (this cannot be included into line width)
      WasParagraph := (LastLine = nil) or (Items[LastLine.EndBlock] is TKMemoParagraph);
      if WasParagraph then
      begin
        FirstIndent := CurParaStyle.FirstIndent;
        TopPadding := CurParaStyle.TopPadding;
        if CurParagraph <> nil then
          NumberBlock := CurParagraph.NumberBlock
        else
          NumberBlock := nil;
      end else
      begin
        FirstIndent := 0;
        TopPadding := 0;
        NumberBlock := nil;
      end;
      if EndItem is TKMemoParagraph then
      begin
        BottomPadding := CurParaStyle.BottomPadding;
        ParaMarkWidth := EndItem.WordWidth[EndItem.WordCount - 1];
      end else
      begin
        BottomPadding := 0;
        ParaMarkWidth := 0;
      end;
      // get dominant base line
      BaseLine := 0;
      for I := Line.StartBlock to Line.EndBlock do
      begin
        Item := Items[I];
        if Item.Position = mbpText then
          BaseLine := Max(BaseLine, Item.CalcAscent(ACanvas));
      end;
      if NumberBlock <> nil then
        BaseLine := Max(BaseLine, NumberBlock.CalcAscent(ACanvas));
      // adjust line and paragraph heights
      case CurParaStyle.LineSpacingMode of
        lsmFactor:
        begin
          LineHeight := Round(CurParaStyle.LineSpacingFactor * LineHeight);
        end;
        lsmValue:
        begin
          if CurParaStyle.LineSpacingValue > 0 then
            LineHeight := Max(LineHeight, CurParaStyle.LineSpacingValue)
          else if CurParaStyle.LineSpacingValue < 0 then
            LineHeight := -CurParaStyle.LineSpacingValue;
        end;
      end;
      Inc(LineHeight, TopPadding + BottomPadding);
      // adjust all words horizontally
      if CurParaStyle.HAlign in [halCenter, halRight] then
      begin
        // reposition all line chunks like MS Word does it
        PosX := CurParaStyle.LeftPadding + FirstIndent;
        StPosX := PosX;
        W := 0;
        ChunkCnt := 0;
        for I := Line.StartBlock to Line.EndBlock do
        begin
          Item := Items[I];
          if Item.Position = mbpText then
          begin
            GetWordIndexes(I, LineIndex, St, En);
            for J := St to En do
            begin
              if Item.WordLeft[J] > PosX then
              begin
                // space here, get colliding rect
                RW := Rect(PosX, PosY, PosX + Item.WordWidth[J], PosY + LineHeight);
                if RectCollidesWithNonText(RW, R) then
                begin
                  Delta := R.Left - StPosX - W;
                  case CurParaStyle.HAlign of
                    halCenter: Delta := Delta div 2;
                  end;
                  MoveWordsOnLine(LineIndex, StPosX, R.Left, Delta, ChunkCnt);
                end;
                PosX := Item.WordLeft[J];
                StPosX := PosX;
                W := 0;
              end;
              Inc(PosX, Item.WordWidth[J]);
              Inc(W, Item.WordWidth[J]);
            end;
          end;
        end;
        RW := Rect(StPosX, PosY, Right + ParaMarkWidth, PosY + LineHeight);
        if RectCollidesWithNonText(RW, R) then
          Delta := R.Left - StPosX - W
        else
          Delta := Right + ParaMarkWidth - StPosX - W;
        case CurParaStyle.HAlign of
          halCenter: Delta := Delta div 2;
        end;
        MoveWordsOnLine(LineIndex, StPosX, Right + ParaMarkWidth, Delta, ChunkCnt);
      end;
      // adjust all words vertically, compute line extent
      LineRight := CurParaStyle.LeftPadding;
      LineLeft := Right;
      for I := Line.StartBlock to Line.EndBlock do
      begin
        Item := Items[I];
        if Item.Position = mbpText then
        begin
          GetWordIndexes(I, LineIndex, St, En);
          for J := St to En do
          begin
            Item.WordBaseLine[J] := BaseLine;
            Item.WordBottomPadding[J] := BottomPadding;
            Item.WordHeight[J] := LineHeight;
            Item.WordTopPadding[J] := TopPadding;
            R := Item.WordBoundsRect[J];
            LineLeft := Min(LineLeft, R.Left);
            LineRight := Max(LineRight, R.Right);
          end;
        end;
      end;
      if NumberBlock <> nil then
      begin
        for J := 0 to NumberBlock.WordCount - 1 do
        begin
          NumberBlock.WordBaseLine[J] := BaseLine;
          NumberBlock.WordBottomPadding[J] := BottomPadding;
          NumberBlock.WordHeight[J] := LineHeight;
          NumberBlock.WordTopPadding[J] := TopPadding;
          R := NumberBlock.WordBoundsRect[J];
          LineLeft := Min(LineLeft, R.Left);
          LineRight := Max(LineRight, R.Right);
        end;
      end;
      // adjust paragraph extent
      if LineRight > ARequiredWidth then
        Dec(LineRight, ParaMarkWidth);
      ParaWidth := Max(ParaWidth, LineRight - LineLeft);
      if EndItem is TKMemoParagraph then
      begin
        TKMemoParagraph(EndItem).Top := ParaPosY;
        TKmemoParagraph(EndItem).Width := ParaWidth;
        TKmemoParagraph(EndItem).Height := PosY + LineHeight - ParaPosY - CurParaStyle.BottomPadding;
        CurParagraph := GetNearestParagraphItem(CurBlock);
        CurParaStyle := GetParaStyle(CurParagraph);
        ParaWidth := 0;
        ParaPosY := PosY + LineHeight + CurParaStyle.TopPadding;
      end;
      // adjust line extent
      Line.Extent := Point(LineRight - LineLeft, LineHeight);
      Line.Position := Point(LineLeft, PosY);
      // other tasks
      FExtent.X := Max(FExtent.X, LineRight);
      PosX := CurParaStyle.LeftPadding;
      if EndItem is TKMemoParagraph then
      begin
        Inc(PosX, CurParaStyle.FirstIndent);
      end;
      Right := ARequiredWidth - CurParaStyle.RightPadding;
      Inc(PosY, LineHeight);
      FExtent.Y := Max(FExtent.Y, PosY);
      LastBlock := CurBlock;
      LastWord := CurWord;
      LastIndex := CurIndex;
      LineHeight := 0;
      Result := True;
    end;
  end;

  procedure MoveWordToFreeSpace(AWordWidth, AWordHeight: Integer);
  var
    TmpHeight: Integer;
    R: TRect;
  begin
    if AWordWidth <> 0 then
    begin
      TmpHeight := Max(LineHeight, AWordHeight);
      while RectCollidesWithNonText(Rect(PosX, PosY, PosX + AWordWidth, PosY + TmpHeight), R) do
      begin
        PosX := R.Right;
        if PosX + AWordWidth > Right then
        begin
          if not AddLine then
            Inc(PosY, 5);
          PosX := CurParaStyle.LeftPadding;
        end;
      end;
    end;
  end;

  function MeasureNextWords(ACanvas: TCanvas; ACurBlock, ACurWord, ARequiredWidth: Integer; IsBreakable: Boolean; var ANBExtent: TPoint): TPoint;
  var
    Item: TKMemoBlock;
    Extent: TPoint;
    WLen, MaxWLen: Integer;
  begin
    Item := Items[ACurBlock];
    Result := Item.WordMeasureExtent(ACanvas, ACurWord, ARequiredWidth);
    ANBExtent := Result;
    if not IsBreakable then
    begin
      Inc(ACurWord);
      if ACurWord >= Item.WordCount then
      begin
        ACurWord := 0;
        Inc(ACurBlock);
      end;
      WLen := 0;
      MaxWLen := getMaxWordLength;
      while not IsBreakable and (ACurBlock < Count) and (WLen < MaxWLen) do
      begin
        Item := Items[ACurBlock];
        if (Item is TKMemoParagraph) or not (Item is TKMemoTextBlock) then
          IsBreakable := True
        else
        begin
          while not IsBreakable and (ACurWord < Item.WordCount) do
          begin
            IsBreakable := Item.WordBreakable[ACurWord];
            Extent := Item.WordMeasureExtent(ACanvas, ACurWord, ARequiredWidth);
            Inc(ANBExtent.X, Extent.X);
            ANBExtent.Y := Max(ANBExtent.Y, Extent.Y);
            Inc(WLen, Item.WordLength[ACurWord]);
            Inc(ACurWord);
          end;
          ACurWord := 0;
          Inc(ACurBlock);
        end;
      end;
    end;
  end;

var
  Extent, NBExtent: TPoint;
  I, FirstIndent, WLen, PrevPosX, PrevPosY: Integer;
  IsBreakable, IsParagraph, OutSide, WasBreakable, WasParagraph: Boolean;
  Item: TKMemoBlock;
  NextParagraph: TKMemoParagraph;
  NextParaStyle: TKMemoParaStyle;
  S: TKString;
begin
  // this is the main word processing calculation
  FRequiredWidth := ARequiredWidth;
  FLines.Clear;
  FExtent := CreateEmptyPoint;
  LineHeight := 0;
  ParaWidth := 0;
  LastBlock := 0;
  LastIndex := 0;
  LastWord := 0;
  LastTotalWord := 0;
  CurTotalWord := 0;
  CurIndex := 0;
  CurParagraph := GetNearestParagraphItem(0);
  CurParaStyle := GetParaStyle(CurParagraph);
  PosX := CurParaStyle.LeftPadding + CurParaStyle.FirstIndent;
  PosY := 0;
  ParaPosY := CurParaStyle.TopPadding;
  Right := ARequiredWidth - CurParaStyle.RightPadding;
  // first measure all absolutely positioned items
  for CurBlock := 0 to FRelPos.Count - 1 do
  begin
    Item := Items[FRelPos.Items[CurBlock].Index];
    Item.MeasureExtent(ACanvas, ARequiredWidth);
  end;
  // then measure all other items
  CurBlock := 0;
  IsBreakable := True;
  IsParagraph := False;
  while CurBlock < Count do
  begin
    CurWord := 0;
    if IsParagraph or (CurBlock = 0) then
    begin
      NextParagraph := GetNearestParagraphItem(CurBlock);
      NextParaStyle := GetParaStyle(NextParagraph);
      if NextParagraph <> nil then
        Item := NextParagraph.NumberBlock
      else
        Item := nil;
      if Item <> nil then
      begin
        // we must include the nonselectable bullet/number block into normal text flow
        AddLine;
        IsParagraph := False;
        NBExtent.X := 0;
        for I := 0 to Item.WordCount - 1 do
        begin
          Extent := Item.WordMeasureExtent(ACanvas, I, 0);
          // align the text following the bullet/number within paragraph, if possible
          S := Item.Words[I];
          if (S = cTab) and (NBExtent.X < -NextParaStyle.FirstIndent) then
          begin
            Extent.X := -NextParaStyle.FirstIndent - NBExtent.X;
            Item.WordWidth[I] := Extent.X;
            Item.WordClipped[I] := True;
          end;
          MoveWordToFreeSpace(Extent.X, Extent.Y);
          Item.WordLeft[I] := PosX;
          Item.WordTop[I] := PosY;
          Inc(PosX, Extent.X);
          LineHeight := Max(LineHeight, Extent.Y);
          Inc(NBExtent.X, Extent.X);
        end;
      end
    end else
      NextParaStyle := CurParaStyle;
    Item := Items[CurBlock];
    case Item.Position of
      mbpText:
      begin
        while CurWord < Item.WordCount do
        begin
          WasParagraph := IsParagraph;
          IsParagraph := (Item is TKMemoParagraph) and (CurWord = Item.WordCount - 1);
          WLen := Item.WordLength[CurWord];
          WasBreakable := IsBreakable or not (Item is TKMemoTextBlock);
          IsBreakable := Item.WordBreakable[CurWord];
          if WasParagraph then
            FirstIndent := NextParaStyle.FirstIndent
          else
            FirstIndent := 0;
          Extent := MeasureNextWords(ACanvas, CurBlock, CurWord, ARequiredWidth - NextParaStyle.LeftPadding - NextParaStyle.RightPadding - FirstIndent, IsBreakable, NBExtent);
          OutSide := CurParaStyle.WordWrap and not IsParagraph and WasBreakable and (PosX + NBExtent.X > Right);
          if OutSide or WasParagraph then
            AddLine;
          MoveWordToFreeSpace(NBExtent.X, NBExtent.Y);
          Item.WordLeft[CurWord] := PosX;
          Item.WordTop[CurWord] := PosY;
          Inc(PosX, Extent.X);
          LineHeight := Max(LineHeight, Extent.Y);
          Inc(CurWord);
          Inc(CurIndex, WLen);
          Inc(CurTotalWord);
        end;
      end;
      mbpRelative:
      begin
        // position relative block correctly
        PrevPosX := PosX; PrevPosY := PosY;
        try
          // starting position for relative object is currently always: X by column (currently always 0), Y by paragraph
          // the object position offsets (LeftOffset, TopOffset) are always counted from this default position
          PosX := 0;
          PosY := ParaPosY;
          //MoveWordToFreeSpace(Item.Width, Item.Height);
          Item.WordLeft[0] := PosX;
          Item.WordTop[0] := PosY;
          FExtent.X := Max(FExtent.X, PosX + Item.Width + Item.LeftOffset);
          FExtent.Y := Max(FExtent.Y, PosY + Item.Height + Item.TopOffset);
        finally
          PosX := PrevPosX; PosY := PrevPosY;
        end;
        // always place object anchor to the beginning of new paragraph
        if IsParagraph then
        begin
          AddLine;
          IsParagraph := False;
        end;
      end;
      mbpAbsolute:
      begin
        FExtent.X := Max(FExtent.X, Item.Width + Item.LeftOffset);
        FExtent.Y := Max(FExtent.Y, Item.Height + Item.TopOffset);
      end;
    end;
    Inc(CurBlock);
  end;
  if CurIndex > LastIndex then
  begin
    CurWord := 0;
    AddLine;
  end;
end;

function TKMemoBlocks.MouseAction(AAction: TKMemoMouseAction; ACanvas: TCanvas; const APoint: TPoint; AShift: TShiftState): Boolean;
var
  I, J, K, St, En: Integer;
  Item: TKmemoBlock;
begin
  Result := False;
  for I := 0 to LineCount - 1 do
  begin
    if (LineTop[I] <= APoint.Y) and (APoint.Y < LineBottom[I]) or (AAction in [maLeftUp, maRightUp, maMidUp]) then
    begin
      for J := FLines[I].StartBlock to FLines[I].EndBlock do
      begin
        Item := Items[J];
        if Item.Position = mbpText then
        begin
          GetWordIndexes(J, I, St, En);
          for K := St to En do
          begin
            Result := Item.WordMouseAction(ACanvas, K, AAction, APoint, AShift);
            if Result then Exit;
          end;
        end;
      end;
    end;
  end;
  for I := FRelPos.Count - 1 downto 0 do // reversed Z-order here
  begin
    Item := Items[FRelPos[I].Index];
    Result := Item.WordMouseAction(ACanvas, 0, AAction, APoint, AShift);
    if Result then Exit;
  end;
end;

function TKMemoBlocks.NextIndexByCharCount(AIndex, ACharCount: Integer): Integer;
begin
  Result := AIndex + ACharCount;
end;

function TKMemoBlocks.NextIndexByHorzExtent(ACanvas: TCanvas; AIndex, AWidth: Integer; out ALinePos: TKMemoLinePosition): Integer;
var
  R: TRect;
  P: TPoint;
begin
  R := IndexToRect(ACanvas, AIndex, True, EOLToNormal(AIndex));
  Result := PointToIndex(ACanvas, Point(R.Left + AWidth, R.Top), True, False, ALinePos);
  if AIndex = Result then
  begin
    R := IndexToRect(ACanvas, AIndex, False, EOLToNormal(AIndex));
    if AWidth > 0 then
      P := Point(R.Right, R.Top)
    else
      P := Point(R.Left - 1, R.Top);
    Result := PointToIndex(ACanvas, P, True, False, ALinePos);
  end;
end;

function TKMemoBlocks.NextIndexByRowDelta(ACanvas: TCanvas; AIndex, ARowDelta, ALeftPos: Integer; out ALinePos: TKMemoLinePosition): Integer;
var
  R: TRect;
  Y: Integer;
begin
  R := IndexToRect(ACanvas, AIndex, False, EOLToNormal(AIndex));
  if ARowDelta >= 0 then
    Y := R.Bottom
  else
    Y := R.Top - 1;
  Result := PointToIndex(ACanvas, Point(ALeftPos, Y), True, False, ALinePos);
end;

function TKMemoBlocks.NextIndexByVertExtent(ACanvas: TCanvas; AIndex, AHeight, ALeftPos: Integer; out ALinePos: TKMemoLinePosition): Integer;
var
  R: TRect;
  P: TPoint;
begin
  R := IndexToRect(ACanvas, AIndex, True, EOLToNormal(AIndex));
  Result := PointToIndex(ACanvas, Point(ALeftPos, R.Top + AHeight), True, False, ALinePos);
  if AIndex = Result then
  begin
    R := IndexToRect(ACanvas, AIndex, False, EOLToNormal(AIndex));
    if AHeight > 0 then
      P := Point(ALeftPos, R.Bottom)
    else
      P := Point(ALeftPos, R.Top - 1);
    Result := PointToIndex(ACanvas, P, True, False, ALinePos);
  end;
end;

function TKMemoBlocks.NextIndexByVertValue(ACanvas: TCanvas; AValue, ALeftPos: Integer; ADirection: Boolean; out ALinePos: TKMemoLinePosition): Integer;
var
  Y: Integer;
  R: TRect;
begin
  R := CreateEmptyRect;
  Y := AValue;
  repeat
    if ADirection then
      Dec(Y, R.Bottom - R.Top)
    else
      Inc(Y, R.Bottom - R.Top);
    Result := PointToIndex(ACanvas, Point(ALeftPos, Y), True, False, ALinePos);
    R := IndexToRect(ACanvas, Result, True, False);
  until (ADirection and (R.Bottom <= AValue)) or (not ADirection and (R.Top >= AValue));
end;

function TKMemoBlocks.NormalToEOL(var AIndex: Integer): Boolean;
begin
  Result := False;
  if GetLinePosition = eolEnd then
  begin
    Inc(AIndex);
    Result := True;
  end;
end;

procedure TKMemoBlocks.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action in [lnAdded, lnDeleted] then
  begin
    Update([muContent]);
  end;
end;

procedure TKMemoBlocks.NotifyDefaultParaChange;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].NotifyDefaultParaChange;
end;

procedure TKMemoBlocks.NotifyDefaultTextChange;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].NotifyDefaultTextChange;
end;

procedure TKMemoBlocks.NotifyOptionsChange;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].NotifyOptionsChange;
end;

procedure TKMemoBlocks.NotifyPrintBegin;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].NotifyPrintBegin;
end;

procedure TKMemoBlocks.NotifyPrintEnd;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].NotifyPrintEnd;
end;

procedure TKMemoBlocks.PaintLineBackground(ACanvas: TCanvas; ALineIndex, ALeft, ATop: Integer);
var
  SaveIndex: Integer;
  R, RClip: TRect;
  PA: TKMemoParagraph;
  MainClipRgn: HRGN;
begin
  PA := GetNearestParagraphItem(FLines[ALineIndex].StartBlock);
  if (PA <> nil) and ((PA.ParaStyle.Brush.Style <> bsClear) or (PA.ParaStyle.BorderWidth > 0) or PA.ParaStyle.BorderWidths.NonZero) then
  begin
    R := Rect(0, 0, Max(FRequiredWidth, PA.Width), PA.Height);
    KFunctions.OffsetRect(R, PA.Left, PA.Top);
    RClip := R;
    RClip.Top := Max(RClip.Top, LineTop[ALineIndex]);
    RClip.Bottom := Min(RClip.Bottom, LineBottom[ALineIndex]);
    KFunctions.OffsetRect(R, ALeft, ATop);
    KFunctions.OffsetRect(RClip, ALeft, ATop);
    SaveIndex := SaveDC(ACanvas.Handle);
    MainClipRgn := CreateEmptyRgn;
    try
      TranslateRectToDevice(ACanvas.Handle, RClip);
      if GetClipRgn(ACanvas.Handle, MainClipRgn) <> 1 then
      begin
        DeleteObject(MainClipRgn);
        MainClipRgn := CreateRectRgnIndirect(RClip);
      end;
      if ExtSelectClipRect(ACanvas.Handle, RClip, RGN_AND, MainClipRgn) then
        PA.ParaStyle.PaintBox(ACanvas, R);
    finally
      RgnSelectAndDelete(ACanvas.Handle, MainClipRgn);
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
    ACanvas.Refresh;
  end;
end;

procedure TKMemoBlocks.PaintLineInfo(ACanvas: TCanvas; ALineIndex, ALeft, ATop: Integer);
var
  R: TRect;
begin
  R := LineRect[ALineIndex];
  KFunctions.OffsetRect(R, ALeft, ATop);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Size := 8;
  ACanvas.Font.Color := clWindowText;
  ACanvas.Font.Style := [];
  ACanvas.Rectangle(R);
  ACanvas.TextOut(R.Right, R.Top, IntToStr(Flines[ALineIndex].StartBlock));
  ACanvas.TextOut(R.Right + 20, R.Top, IntToStr(Flines[ALineIndex].EndBlock));
  ACanvas.TextOut(R.Right + 40, R.Top, IntToStr(Flines[ALineIndex].StartWord));
  ACanvas.TextOut(R.Right + 60, R.Top, IntToStr(Flines[ALineIndex].EndWord));
end;

procedure TKMemoBlocks.PaintToCanvas(ACanvas: TCanvas; ALeft, ATop: Integer; const ARect: TRect);
var
  I, J, K, St, En: Integer;
  R: TRect;
  Item: TKMemoBlock;
begin
  // paint text blocks
  for I := 0 to LineCount - 1 do
  begin
    if (LineBottom[I] + ATop >= ARect.Top) and (LineTop[I] + ATop < ARect.Bottom) then
    begin
      // fill areas under paragraphs
      if LineFloat[I] then
      begin
        PaintLineBackground(ACanvas, I, ALeft, ATop);
      end;
      // then paint text blocks
      for J := FLines[I].StartBlock to FLines[I].EndBlock do
      begin
        Item := Items[J];
        if Item.Position = mbpText then
        begin
          GetWordIndexes(J, I, St, En);
          for K := St to En do
            Item.WordPaintToCanvas(ACanvas, K, ALeft, ATop);
        end;
      end;
      // paint line info, only for debug purposes
      //PaintLineInfo(ACanvas, I, ALeft, ATop);
    end;
  end;
  // paint numbering blocks
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if Item is TKMemoParagraph then
    begin
      Item := TKMemoParagraph(Item).NumberBlock;
      if Item <> nil then
      begin
        R := Item.BoundsRect;
        KFunctions.OffsetRect(R, ALeft, ATop);
        if RectInRect(ARect, R) then
          Item.PaintToCanvas(ACanvas, ALeft, ATop);
      end;
    end;
  end;
  // paint relative or absolute blocks
  for I := 0 to FRelPos.Count - 1 do
  begin
    Item := Items[FRelPos[I].Index];
    R := Item.BoundsRect;
    KFunctions.OffsetRect(R, Item.LeftOffset + ALeft, Item.TopOffset + ATop);
    if RectInRect(ARect, R) then
      Item.PaintToCanvas(ACanvas, ALeft, ATop);
  end;
end;

function TKMemoBlocks.PointToIndex(ACanvas: TCanvas; const APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer;
var
  I: Integer;
begin
  Result := -1;
  if LineCount > 0 then
  begin
    I := 0;
    while (Result < 0) and (I < LineCount) do
    begin
      if (APoint.Y >= LineTop[I]) and (APoint.Y < LineBottom[I]) or AOutOfArea and (
         (I = 0) and (APoint.Y < LineTop[I]) or // point below first line
         (I = LineCount - 1) and (APoint.Y >= LineBottom[I]) or // point after last line
         (I > 0) and (APoint.Y < LineTop[I]) and (Apoint.Y >= LineBottom[I - 1]) // point between two lines
        ) then
      begin
        Result := PointToIndexOnLine(ACanvas, I, APoint, AOutOfArea, ASelectionExpanding, ALinePos)
      end;
      Inc(I);
    end;
  end;
end;

function TKMemoBlocks.PointToIndexOnLine(ACanvas: TCanvas; ALineIndex: Integer; const APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer;
var
  I, J, LocalIndex, Index, St, En, X, XOld: Integer;
  Item: TKMemoBlock;
begin
  Result := -1;
  ALinePos := eolInside;
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
  begin
    Index := FLines[ALineIndex].StartIndex;
    I := FLines[ALineIndex].StartBlock;
    X := LineLeft[ALineIndex];
    while (Result < 0) and (I <= FLines[ALineIndex].EndBlock) do
    begin
      Item := Items[I];
      if Item.Position = mbpText then
      begin
        GetWordIndexes(I, ALineIndex, St, En);
        J := St;
        while (Result < 0) and (J <= En) do
        begin
          XOld := X;
          X := Item.WordLeft[J];
          LocalIndex := Item.WordPointToIndex(ACanvas, APoint, J, AOutOfArea, ASelectionExpanding, ALinePos);
          if LocalIndex >= 0 then
          begin
            Result := Index + LocalIndex;
          end
          else if (XOld <= APoint.X) and (APoint.X < X) then
          begin
            // the point lies between words
            Result := Index;
          end;
          Inc(Index, Item.WordLength[J]);
          Inc(J);
        end;
      end;
      Inc(I);
    end;
    if (Result < 0) and AOutOfArea then
    begin
      if (APoint.X >= LineRight[ALineIndex]) then
      begin
        Result := LineEndIndex[ALineIndex];
        Item := Items[FLines[ALineIndex].EndBlock];
        if ASelectionExpanding or not ((Item is TKMemoContainer) or (Item is TKMemoParagraph)) then
        begin
          ALinePos := eolEnd;
          Inc(Result);
        end;
      end
      else if (APoint.X < LineLeft[ALineIndex]) then
      begin
        Result := LineStartIndex[ALineIndex];
      end else
      begin
        // this should not happen but we must handle this case
        Result := (LineStartIndex[ALineIndex] + LineEndIndex[ALineIndex]) div 2;
      end;
    end;
  end;
end;

function TKMemoBlocks.PointToItem(const APoint: TPoint): TKMemoBlock;
var
  I: Integer;
  Item: TKMemoBlock;
  R: TRect;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    R := Item.BoundsRect;
    if Item.Position <> mbpText then
      KFunctions.OffsetRect(R, Item.LeftOffset, Item.TopOffset);
    if PtInRect(R, APoint) then
    begin
      Result := Item;
      Exit;
    end;
  end;
end;

function TKMemoBlocks.PointToItems(const APoint: TPoint): TKMemoBlocks;
var
  I: Integer;
  Item: TKMemoBlock;
  R: TRect;
  P: TPoint;
begin
  Result := nil;
  for I := 0 to FRelPos.Count - 1 do
  begin
    Item := Items[FRelPos[I].Index];
    if Item is TKMemoContainer then
    begin
      R := Item.BoundsRect;
      KFunctions.OffsetRect(R, Item.LeftOffset, Item.TopOffset);
      if PtInRect(R, APoint) then
      begin
        Result := TKMemoContainer(Item).Blocks;
        Break;
      end;
    end;
  end;
  if Result = nil then
  begin
    for I := 0 to Count - 1 do
    begin
      Item := Items[I];
      if Item is TKMemoContainer then
      begin
        P := APoint;
        OffsetPoint(P, -TKMemoContainer(Item).Left - TKMemoContainer(Item).BlockStyle.AllPaddingsLeft, -TKMemoContainer(Item).Top - TKMemoContainer(Item).BlockStyle.AllPaddingsTop);
        Result := TKMemoContainer(Item).Blocks.PointToItems(P);
        if Result <> nil then
          break;
      end;
    end;
  end;
end;

procedure TKMemoBlocks.SaveToRTFStream(AStream: TStream; ASelectedOnly: Boolean);
var
  Writer: TKMemoRTFWriter;
begin
  Writer := TKMemoRTFWriter.Create(ParentMemo);
  try
    Writer.SaveToStream(AStream, ASelectedOnly, Self);
  finally
    Writer.Free;
  end;
end;

function TKMemoBlocks.Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean; ATextOnly: Boolean): Boolean;
var
  I, LastIndex, CurIndex, NewSelEnd, MaxIndex, LocalSelLength: Integer;
  Item: TKMemoBlock;
begin
  NewSelEnd := ASelStart + ASelLength;
  if FLines.Count > 0 then
    Item := Items[FLines[FLines.Count - 1].EndBlock]
  else
    Item := nil;
  if (ASelLength <> 0) or not (Item is TKMemoParagraph) then
    MaxIndex := FSelectableLength
  else
    MaxIndex := FSelectableLength - 1;
  NewSelEnd := MinMax(NewSelEnd, -1, MaxIndex);
  ASelStart := MinMax(ASelStart, -1, MaxIndex);
  if (ASelStart <> FSelStart) or (NewSelEnd <> FSelEnd) then
  begin
    FSelStart := ASelStart;
    FSelEnd := NewSelEnd;
    CurIndex := 0;
    LockUpdate;
    try
      // children have always FSelEnd >= FSelStart
      if NewSelEnd < ASelStart then
        KFunctions.Exchange(ASelStart, NewSelEnd);
      for I := 0 to Count - 1 do
      begin
        Item := Items[I];
        if Item.Position = mbpText then
        begin
          LastIndex := CurIndex;
          Inc(CurIndex, Item.SelectableLength);
          if (ASelStart >= LastIndex) and (NewSelEnd < CurIndex) then
          begin
            // selection within the same block
            Item.Select(ASelStart - LastIndex, NewSelEnd - ASelStart, ADoScroll);
          end
          else if (ASelStart >= LastIndex) and (ASelStart < CurIndex) and (NewSelEnd >= CurIndex) then
            // selection starts in this block
            Item.Select(ASelStart - LastIndex, CurIndex - ASelStart, ADoScroll)
          else if (ASelStart < LastIndex) and (NewSelEnd >= LastIndex) and (NewSelEnd < CurIndex) then
            // selection ends in this block
            Item.Select(0, NewSelEnd - LastIndex, ADoScroll)
          else if (ASelStart <= LastIndex) and (NewSelEnd >= CurIndex) then
          begin
            // selection goes through this block
            if not ATextOnly and ((ASelLength <> 0) and (Item.Position <> mbpText)) then
              LocalSelLength := Item.SelectableLength(True)
            else
              LocalSelLength := CurIndex - LastIndex;
            Item.Select(0, LocalSelLength, ADoScroll)
          end else
            Item.Select(-1, 0, ADoScroll);
        end;
      end;
      if ADoScroll then
      begin
        Exclude(FUpdateReasons, muSelection);
        Include(FUpdateReasons, muSelectionScroll)
      end else
      begin
        Include(FUpdateReasons, muSelection);
        Exclude(FUpdateReasons, muSelectionScroll);
      end;
    finally
      UnlockUpdate;
    end;
    Result := True
  end else
    Result := False;
end;

procedure TKMemoBlocks.SetExtent(AWidth, AHeight: Integer);
begin
  FExtent := Point(AWidth, AHeight);
end;

procedure TKMemoBlocks.SetIgnoreParaMark(const Value: Boolean);
begin
  if Value <> FIgnoreParaMark then
  begin
    FIgnoreParaMark := Value;
    Update([muExtent]);
  end;
end;

procedure TKMemoBlocks.SetItem(Index: Integer; const Value: TKMemoBlock);
begin
  inherited SetItem(Index, Value);
end;

procedure TKMemoBlocks.SetLineText(ALineIndex: Integer; const AValue: TKString);
var
  St, Len: Integer;
  PA: TKMemoParagraph;
begin
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
  begin
    LockUpdate;
    try
      St := FLines[ALineIndex].StartIndex;
      Len :=  FLines[ALineIndex].EndIndex - FLines[ALineIndex].StartIndex;
      Select(St, Len);
      ClearSelection;
      AddTextBlock(AValue, St);
      PA := AddParagraph(St + 1);
      PA.ParaStyle.WordWrap := False; // to allow multiple lines to be added
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKMemoBlocks.SetMemoNotifier(const Value: IKMemoNotifier);
var
  I: Integer;
begin
  FMemoNotifier := Value;
  for I := 0 to Count - 1 do
    if Items[I] is TKMemoContainer then
      TKMemoContainer(Items[I]).Blocks.MemoNotifier := FMemoNotifier;
end;

procedure TKMemoBlocks.SetSelectionParaStyle(const Value: TKMemoParaStyle);
var
  I, CurIndex, LastIndex, TmpSelStart, TmpSelEnd: Integer;
  Item: TKMemoBlock;
  WasSelection: Boolean;
begin
  if (SelLength <> 0) or (FSelEnd >= 0) and (FSelStart >= 0) then
  begin
    TmpSelStart := RealSelStart;
    TmpSelEnd := RealSelEnd;
    LockUpdate;
    try
      WasSelection := False;
      CurIndex := 0;
      for I := 0 to Count - 1 do
      begin
        Item := Items[I];
        LastIndex := CurIndex;
        Inc(CurIndex, Item.SelectableLength);
        if Item is TKMemoContainer then
          TKMemoContainer(Item).Blocks.SelectionParaStyle := Value
        else
        begin
          if (LastIndex <= TmpSelEnd) and (TmpSelStart < CurIndex) then
          begin
            WasSelection := True;
            if Item is TKMemoParagraph then
              // selection through this block
              TKMemoParagraph(Item).ParaStyle.Assign(Value);
          end
          else if WasSelection and (Item is TKMemoParagraph) then
          begin
            // this is the nearest paragraph
            TKMemoParagraph(Item).ParaStyle.Assign(Value);
            WasSelection := False;
          end;
        end;
      end;
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKMemoBlocks.SetSelectionTextStyle(const Value: TKMemoTextStyle);
var
  I, CurIndex, LastIndex, TmpSelStart, TmpSelEnd: Integer;
  Item, Item1, Item2: TKMemoBlock;
begin
  if (SelLength <> 0) or (FSelEnd >= 0) and (FSelStart >= 0) then
  begin
    TmpSelStart := RealSelStart;
    TmpSelEnd := RealSelEnd;
    LockUpdate;
    try
      I := 0;
      CurIndex := 0;
      while I < Count do
      begin
        Item := Items[I];
        LastIndex := CurIndex;
        Inc(CurIndex, Item.SelectableLength);
        if Item is TKMemoContainer then
          TKMemoContainer(Item).Blocks.SelectionTextStyle := Value
        else if (Item is TKMemoTextBlock) and (TmpSelEnd > TmpSelStart) then
        begin
          if (TmpSelStart <= LastIndex) and (TmpSelEnd >= CurIndex) then
          begin
            // selection goes though this block
            TKMemoTextBlock(Item).TextStyle.Assign(Value);
          end
          else if (TmpSelStart > LastIndex) and (TmpSelEnd < CurIndex) then
          begin
            // selection within the same block, split it to three parts
            Item1 := Item.Split(TmpSelStart - LastIndex);
            if Item1 <> nil then
            begin
              Inc(I);
              AddAt(Item1, I);
              Item2 := Item1.Split(TmpSelEnd - TmpSelStart);
              if Item2 <> nil then
              begin
                Inc(I);
                AddAt(Item2, I);
              end;
              TKMemoTextBlock(Item1).TextStyle.Assign(Value);
              if TmpSelEnd > TmpSelStart then
                Item1.SelectAll;
            end;
          end
          else if (TmpSelStart > LastIndex) and (TmpSelStart < CurIndex) and (TmpSelEnd >= CurIndex) then
          begin
            // selection starts in this block, split it to two parts
            Item1 := Item.Split(TmpSelStart - LastIndex);
            if Item1 <> nil then
            begin
              Inc(I);
              AddAt(Item1, I);
              TKMemoTextBlock(Item1).TextStyle.Assign(Value);
              Item1.SelectAll;
            end;
          end
          else if (TmpSelStart <= LastIndex) and (TmpSelEnd > LastIndex) and (TmpSelEnd < CurIndex) then
          begin
            // selection ends in this block, split it to two parts
            Item1 := Item.Split(TmpSelEnd - LastIndex);
            if Item1 <> nil then
            begin
              Inc(I);
              AddAt(Item1, I);
              TKMemoTextBlock(Item).TextStyle.Assign(Value);
              Item.SelectAll;
            end;
          end;
        end;
        Inc(I);
      end;
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKMemoBlocks.SetText(const AValue: TKString);
begin
  LockUpdate;
  try
    Clear;
    InsertPlainText(0, AValue);
    FixEmptyBlocks;
  finally
    UnlockUpdate;
  end;
end;

procedure TKMemoBlocks.Update(AReasons: TKMemoUpdateReasons);
begin
  if UpdateUnlocked then
  begin
    if muContent in AReasons then
      UpdateAttributes;
    DoUpdate(AReasons);
  end else
    FUpdateReasons := FUpdateReasons + AReasons;
end;

procedure TKMemoBlocks.UpdateAttributes;

  function NumberToText(ANumber: Integer; AStyle: TKMemoParaNumbering): TKString;
  begin
    Result := '';
    case AStyle of
      pnuArabic: Result := IntToStr(ANumber);
      pnuLetterLo, pnuLetterHi: Result := IntToLatin(ANumber, AStyle = pnuLetterHi);
      pnuRomanLo, pnuRomanHi: Result := IntToRoman(ANumber, AStyle = pnuRomanHi);
    end;
  end;

  procedure FormatNumberString(AListTable: TKMemoListTable; AStyle: TKMemoParaStyle; ANumberBlock: TKMemoTextBlock);
  var
    I, MaxLevel, LevelCount, LevelCounter: Integer;
    Item: TKMemoNumberingFormatItem;
    Numbering: TKMemoParaNumbering;
    List: TKMemoList;
    ListLevel, ItemLevel: TKMemoListLevel;
    S: TKString;
  begin
    // format using the numbering format
    List := AListTable.FindByID(AStyle.NumberingList);
    if List <> nil then
    begin
      S := '';
      MaxLevel := -1;
      ListLevel := List.Levels[AStyle.NumberingListLevel];
      LevelCount := ListLevel.NumberingFormat.LevelCount;
      for I := 0 to ListLevel.NumberingFormat.Count - 1 do
      begin
        Item := ListLevel.NumberingFormat.Items[I];
        if (Item.Level >= 0) and (Item.Text = '') then
        begin
          ItemLevel := List.Levels[Item.Level];
          Numbering := ItemLevel.Numbering;
          if not (Numbering in [pnuNone, pnuBullets]) then
          begin
            LevelCounter := ItemLevel.LevelCounter;
            if AStyle.NumberStartAt > 0 then
              LevelCounter := AStyle.NumberStartAt
            else if Item.Level >= LevelCount - 1 then
              // we only increase the last level
              Inc(LevelCounter);
            S := S + NumberToText(LevelCounter, Numbering);
            ItemLevel.LevelCounter := LevelCounter;
            MaxLevel := Item.Level;
          end;
        end else
          S := S + Item.Text;
      end;
      if Maxlevel >= 0 then
        // set all counters for subordinated list levels to zero
        List.Levels.ClearLevelCounters(MaxLevel + 1);
      S := S + cTab;
      ANumberBlock.Text := S;
      if ListLevel.NumberingFontChanged then
        ANumberBlock.TextStyle.Font.Assign(ListLevel.NumberingFont);
    end;
  end;

var
  I: Integer;
  Item: TKMemoBlock;
  PA: TKMemoParagraph;
  NumberBlock: TKMemoTextBlock;
  ListTable: TKMemoListTable;
begin
  Inc(FUpdateLock);
  try
    FRelPos.Clear;
    FSelectableLength := 0;
    if MemoNotifier <> nil then
    begin
      ListTable := MemoNotifier.GetListTable;
      ListTable.ClearLevelCounters;
    end else
      ListTable := nil;
    for I := 0 to Count - 1 do
    begin
      Item := Items[I];
      if Item.Position = mbpText then
      begin
        if Item is TKMemoParagraph then
        begin
          PA := TKMemoParagraph(Item);
          PA.AssignAttributes(GetLastItemByClass(I, TKMemoTextBlock)); // TODO: make this line optional
          if ListTable <> nil then
          begin
            NumberBlock := PA.NumberBlock;
            if NumberBlock <> nil then
            begin
              NumberBlock.TextStyle.Assign(PA.TextStyle);
              FormatNumberString(ListTable, PA.ParaStyle, NumberBlock);
            end;
          end;
        end;
        Inc(FSelectableLength, Item.SelectableLength);
      end else
      begin
        FRelPos.AddItem(I);
      end;
    end;
  finally
    Dec(FUpdateLock);
    FUpdateReasons := [];
  end;
  if Count > 0 then
  begin
    FSelStart := MinMax(FSelStart, 0, FSelectableLength);
    FSelEnd := MinMax(FSelEnd, 0, FSelectableLength);
  end else
  begin
    FSelStart := -1;
    FSelEnd := -1;
  end;
end;

{ TKMemoEditAction }

function TKMemoEditAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Target is TKCustomMemo;
end;

procedure TKMemoEditAction.ExecuteTarget(Target: TObject);
begin
  TKMemo(Target).ExecuteCommand(GetEditCommand);
end;

procedure TKMemoEditAction.UpdateTarget(Target: TObject);
begin
  Enabled := TKMemo(Target).CommandEnabled(GetEditCommand);
end;

{ TKMemoEditSelectAllAction }

function TKMemoEditSelectAllAction.GetEditCommand: TKEditCommand;
begin
  Result := ecSelectAll;
end;

{ TKMemoEditCopyAction }

function TKMemoEditCopyAction.GetEditCommand: TKEditCommand;
begin
  Result := ecCopy;
end;

{ TKMemoEditCutAction }

function TKMemoEditCutAction.GetEditCommand: TKEditCommand;
begin
  Result := ecCut;
end;

{ TKMemoEditPasteAction }

function TKMemoEditPasteAction.GetEditCommand: TKEditCommand;
begin
  Result := ecPaste;
end;

end.

