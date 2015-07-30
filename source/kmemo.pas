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

unit KMemo;

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

  cHorzScrollStepDef = 4;

  cVertScrollStepDef = 10;

  { Default value for the @link(TKMemo.Height) property. }
  cHeight = 200;

  { Default value for the @link(TKMemo.Width) property. }
  cWidth = 300;

  { This is the character for paragraph visualisation. }
  cNewLineChar = #$B6;
  { This is the character for space visualisation. }
  cSpaceChar = #$B7;
  { This is the character for tab visualisation. }
  cTabChar = #$2192;

  { Default characters used to break the text words. }
  cDefaultWordBreaks = [cNULL, cSPACE, '/', '\', ';', ':', '?', '!'];

  { Format for clipboard operations. }
  cRichText = 'Rich Text Format';

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
    { Overwrite mode active. }
    elOverwrite,
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

  TKMemoTextCapitals = (tcaNone, tcaNormal, tcaSmall);

  TKMemoTextStyle = class(TPersistent)
  private
    FBrush: TBrush;
    FFont: TFont;
    FStyleChanged: Boolean;
    FOnChanged: TNotifyEvent;
    FAllowBrush: Boolean;
    FCapitals: TKMemoTextCapitals;
    procedure SetBrush(const Value: TBrush);
    procedure SetFont(const Value: TFont);
    procedure SetAllowBrush(const Value: Boolean);
    procedure SetCapitals(const Value: TKMemoTextCapitals);
  protected
    FBrushChanged: Boolean;
    FFontChanged: Boolean;
    FLocked: Boolean;
    procedure BrushChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure Changed; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure Defaults; virtual;
    function EqualProperties(AValue: TKMemoTextStyle): Boolean; virtual;
    procedure NotifyChange(AValue: TKMemoTextStyle); virtual;
    property AllowBrush: Boolean read FAllowBrush write SetAllowBrush;
    property Capitals: TKMemoTextCapitals read FCapitals write SetCapitals;
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
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

  TKMemoBlockStyle = class(TPersistent)
  private
    FBrush: TBrush;
    FBorderRadius: Integer;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FBorderWidths: TKRect;
    FContentMargin: TKRect;
    FContentPadding: TKRect;
    FFillBlip: TGraphic;
    FHAlign: TKHAlign;
    FWrapMode: TKMemoBlockWrapMode;
    FOnChanged: TNotifyEvent;
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
    FChanged: Boolean;
    FLocked: Boolean;
    procedure BrushChanged(Sender: TObject);
    procedure Changed; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
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
    property TopBorderWidth: Integer read GetTopBorderWidth;
    property TopMargin: Integer read GetTopMargin write SetTopMargin;
    property TopPadding: Integer read GetTopPadding write SetTopPadding;
    property WrapMode: TKMemoBlockWrapMode read FWrapMode write SetWrapMode;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TKMemoParaStyle = class(TKMemoBlockStyle)
  private
    FFirstIndent: Integer;
    FWordWrap: Boolean;
    procedure SetFirstIndent(const Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
  public
    constructor Create; override;
    procedure Assign(ASource: TPersistent); override;
    procedure Defaults; override;
    property FirstIndent: Integer read FFirstIndent write SetFirstIndent;
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
  public
    constructor Create;
    property EndBlock: Integer read FEndBlock write FEndBlock;
    property EndIndex: Integer read FEndIndex write FEndIndex;
    property EndWord: Integer read FEndWord write FEndWord;
    property Extent: TPoint read FExtent write FExtent;
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
    FExtent: TPoint;
    FEndIndex: Integer;
    FPosition: TPoint;
    FStartIndex: Integer;
    FTopPadding: Integer;
  public
    constructor Create;
    property BaseLine: Integer read FBaseLine write FBaseLine;
    property BottomPadding: Integer read FBottomPadding write FBottomPadding;
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

  TKMemoSparseItem = class(TObject)
  private
    FIndex: Integer;
  public
    constructor Create;
    property Index: Integer read FIndex write FIndex;
  end;

  TKMemoSparseList = class(TObjectList)
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

  TKMemoBlocks = class;

  IKMemoNotifier = interface(IInterface)
    procedure BlocksFreeNotification(ABlocks: TKMemoBlocks);
    function GetDefaultTextStyle: TKMemoTextStyle;
    function GetDefaultParaStyle: TKMemoParaStyle;
    function GetLinePosition: TKMemoLinePosition;
    function GetPaintSelection: Boolean;
    function GetPrinting: Boolean;
    procedure GetSelColors(out Foreground, Background: TColor);
    function GetShowFormatting: Boolean;
    function GetWordBreaks: TKSysCharSet;
    procedure SetReqMouseCursor(ACursor: TCursor);
  end;

  TKMemoMouseAction = (maMove, maLeftDown, maLeftUp, maRightDown, maRightUp, maMidDown, maMidUp);

  TKMemoBlockClass = class of TKMemoBlock;

  TKMemoBlock = class(TObject)
  private
    FOffset: TPoint;
    FParent: TKMemoBlocks;
    FPosition: TKMemoBlockPosition;
    function GetBoundsRect: TRect;
    function GetMemoNotifier: IKMemoNotifier;
    function GetPaintSelection: Boolean;
    function GetPrinting: Boolean;
    procedure SetParent(AParent: TKMemoBlocks);
    procedure SetPosition(const Value: TKMemoBlockPosition);
  protected
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
    function GetText: TKString; virtual;
    function GetTop: Integer; virtual;
    function GetTopPadding: Integer; virtual;
    function GetWidth: Integer; virtual;
    function GetWordBaseLine(Index: Integer): Integer; virtual;
    function GetWordBottomPadding(Index: Integer): Integer; virtual;
    function GetWordBoundsRect(Index: Integer): TRect; virtual;
    function GetWordBreakable(Index: Integer): Boolean; virtual;
    function GetWordCount: Integer; virtual;
    function GetWordHeight(Index: Integer): Integer; virtual;
    function GetWordLeft(Index: Integer): Integer; virtual;
    function GetWordLength(Index: Integer): Integer; virtual;
    function GetWords(Index: Integer): TKString; virtual;
    function GetWordTop(Index: Integer): Integer; virtual;
    function GetWordTopPadding(Index: Integer): Integer; virtual;
    function GetWordWidth(Index: Integer): Integer; virtual;
    function InternalLeftOffset: Integer;
    function InternalTopOffset: Integer;
    procedure ParentChanged; virtual;
    procedure SetLeftOffset(const Value: Integer); virtual;
    procedure SetTopOffset(const Value: Integer); virtual;
    procedure SetWordBaseLine(Index: Integer; const Value: Integer); virtual;
    procedure SetWordBottomPadding(Index: Integer; const Value: Integer); virtual;
    procedure SetWordHeight(Index: Integer; const Value: Integer); virtual;
    procedure SetWordLeft(Index: Integer; const Value: Integer); virtual;
    procedure SetWordTop(Index: Integer; const Value: Integer); virtual;
    procedure SetWordTopPadding(Index: Integer; const Value: Integer); virtual;
    procedure SetWordWidth(Index: Integer; const Value: Integer); virtual;
    procedure Update(AReasons: TKMemoUpdateReasons); virtual;
  public
    constructor Create(AParent: TKMemoBlocks); virtual;
    procedure Assign(AItem: TKMemoBlock); virtual;
    procedure AssignAttributes(AItem: TKMemoBlock); virtual;
    function CalcBaseLine(ACanvas: TCanvas): Integer; virtual;
    function CanAdd(AItem: TKMemoBlock): Boolean; virtual;
    procedure ClearSelection(ATextOnly: Boolean); virtual;
    function Concat(AItem: TKMemoBlock): Boolean; virtual;
    function EqualProperties(AItem: TKMemoBlock): Boolean; virtual;
    procedure GetWordIndexes(AIndex: Integer; out ASt, AEn: Integer); virtual;
    function IndexToRect(ACanvas: TCanvas; AIndex: Integer; ACaret: Boolean): TRect; virtual;
    function InsertParagraph(AIndex: Integer): Boolean; virtual;
    function InsertString(const AText: TKString; At: Integer = -1): Boolean; virtual;
    function MeasureExtent(ACanvas: TCanvas; ARequiredWidth: Integer): TPoint; virtual;
    procedure NotifyDefaultTextChange; virtual;
    procedure NotifyDefaultParaChange; virtual;
    procedure PaintToCanvas(ACanvas: TCanvas; ALeft, ATop: Integer); virtual;
    function PointToIndex(ACanvas: TCanvas; const APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; virtual;
    procedure SelectAll; virtual;
    function Select(ASelStart, ASelLength: Integer): Boolean; virtual;
    function SelectableLength(ALocalCalc: Boolean = False): Integer; virtual;
    function Split(At: Integer; AllowEmpty: Boolean = False): TKMemoBlock; virtual;
    function WordIndexToRect(ACanvas: TCanvas; AWordIndex: Integer; AIndex: Integer; ACaret: Boolean): TRect; virtual;
    function WordMeasureExtent(ACanvas: TCanvas; AWordIndex, ARequiredWidth: Integer): TPoint; virtual;
    function WordMouseAction(AWordIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean; virtual;
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
    property Parent: TKMemoBlocks read FParent write SetParent;
    property Position: TKMemoBlockPosition read FPosition write SetPosition;
    property Printing: Boolean read GetPrinting;
    property SelLength: Integer read GetSelLength;
    property SelStart: Integer read GetSelStart;
    property SelText: TKString read GetSelText;
    property ShowFormatting: Boolean read GetShowFormatting;
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
    property WordHeight[Index: Integer]: Integer read GetWordHeight write SetWordHeight;
    property WordLeft[Index: Integer]: Integer read GetWordLeft write SetWordLeft;
    property WordLength[Index: Integer]: Integer read GetWordLength;
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
    constructor Create(AParent: TKMemoBlocks); override;
    function Select(ASelStart, ASelLength: Integer): Boolean; override;
  end;

  TKMemoTextBlock = class(TKMemoSingleton)
  private
    FText: TKString;
    FTextStyle: TKMemoTextStyle;
    function GetWordBreaks: TKSysCharSet;
  protected
    { Because of time optimization. }
    FTextLength: Integer;
    FWords: TKMemoWordList;
    function ApplyFormatting(const AText: TKString): TKString;
    procedure ApplyTextStyle(ACanvas: TCanvas); virtual;
    function ContentLength: Integer; override;
    function GetCanAddText: Boolean; override;
    function GetKerningDistance(ACanvas: TCanvas; const AChar1, AChar2: TKChar): Integer;
    function GetSelText: TKString; override;
    function GetText: TKString; override;
    function GetWordBaseLine(Index: Integer): Integer; override;
    function GetWordBottomPadding(Index: Integer): Integer; override;
    function GetWordBoundsRect(Index: Integer): TRect; override;
    function GetWordBreakable(Index: Integer): Boolean; override;
    function GetWordCount: Integer; override;
    function GetWordHeight(Index: Integer): Integer; override;
    function GetWordLeft(Index: Integer): Integer; override;
    function GetWordLength(Index: Integer): Integer; override;
    function GetWords(Index: Integer): TKString; override;
    function GetWordTop(Index: Integer): Integer; override;
    function GetWordTopPadding(Index: Integer): Integer; override;
    function GetWordWidth(Index: Integer): Integer; override;
    function IndexToTextIndex(const AText: TKString; AIndex: Integer): Integer; virtual;
    function ModifiedTextExtent(ACanvas: TCanvas; const AText: TKString): TPoint; virtual;
    procedure ParentChanged; override;
    procedure SetText(const Value: TKString); virtual;
    procedure SetWordBaseLine(Index: Integer; const Value: Integer); override;
    procedure SetWordBottomPadding(Index: Integer; const Value: Integer); override;
    procedure SetWordHeight(Index: Integer; const Value: Integer); override;
    procedure SetWordLeft(Index: Integer; const Value: Integer); override;
    procedure SetWordTop(Index: Integer; const Value: Integer); override;
    procedure SetWordTopPadding(Index: Integer; const Value: Integer); override;
    class procedure SplitText(const ASource: TKString; At: Integer; out APart1, APart2: TKString);
    function TextExtentDepOnKerning(ACanvas: TCanvas; const AText: TKString): TSize; virtual;
    function TextIndexToIndex(var AText: TKString; ATextIndex: Integer): Integer; virtual;
    procedure TextOutputDepOnKerning(ACanvas: TCanvas; ALeft, ATop: Integer; const AText: TKString); virtual;
    procedure TextStyleChanged(Sender: TObject);
    procedure UpdateWords; virtual;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    procedure Assign(AItem: TKMemoBlock); override;
    procedure AssignAttributes(AItem: TKMemoBlock); override;
    function CalcBaseLine(ACanvas: TCanvas): Integer; override;
    function CalcDescent(ACanvas: TCanvas): Integer; virtual;
    procedure ClearSelection(ATextOnly: Boolean); override;
    function Concat(AItem: TKMemoBlock): Boolean; override;
    function EqualProperties(AItem: TKMemoBlock): Boolean; override;
    procedure GetWordIndexes(AIndex: Integer; out ASt, AEn: Integer); override;
    function InsertString(const AText: TKString; At: Integer = -1): Boolean; override;
    procedure NotifyDefaultTextChange; override;
    function Split(At: Integer; AllowEmpty: Boolean = False): TKMemoBlock; override;
    function WordIndexToRect(ACanvas: TCanvas; AWordIndex: Integer; AIndex: Integer; ACaret: Boolean): TRect; override;
    function WordMeasureExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint; override;
    procedure WordPaintToCanvas(ACanvas: TCanvas; AWordIndex, ALeft, ATop: Integer); override;
    function WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint; AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; override;
    property Text: TKString read GetText write SetText;
    property TextStyle: TKMemoTextStyle read FTextStyle;
    property WordBreaks: TKSysCharSet read GetWordBreaks;
  end;

  TKMemoHyperlink = class(TKMemoTextBlock)
  private
    FURL: TKString;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    procedure Assign(AItem: TKMemoBlock); override;
    procedure DefaultStyle; virtual;
    function WordMouseAction(AWordIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean; override;
    property URL: TKString read FURL write FURL;
  end;

  TKMemoParagraph = class(TKMemoTextBlock)
  private
    FExtent: TPoint;
    FPosition: TPoint;
    FParaStyle: TKMemoParaStyle;
  protected
    function GetCanAddText: Boolean; override;
    function GetParaStyle: TKMemoParaStyle; override;
    function GetWordBreakable(Index: Integer): Boolean; override;
    procedure ParaStyleChanged(Sender: TObject);
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    procedure AssignAttributes(AItem: TKMemoBlock); override;
    function Concat(AItem: TKMemoBlock): Boolean; override;
    procedure NotifyDefaultParaChange; override;
    function Split(At: Integer; AllowEmpty: Boolean = False): TKMemoBlock; override;
    property Height: Integer read FExtent.Y write FExtent.Y;
    property Left: Integer read FPosition.X write FPosition.X;
    property Top: Integer read FPosition.Y write FPosition.Y;
    property Width: Integer read FExtent.X write FExtent.X;
  end;

  TKMemoImageBlock = class(TKMemoSingleton)
  private
    FBaseLine: Integer;
    FCrop: TKRect;
    FImage: TPicture;
    FImageStyle: TKMemoBlockStyle;
    FExtent: TPoint; // extent given by word processor
    FOriginalExtent: TPoint; // original extent
    FPosition: TPoint;
    FScale: TPoint; // scaled extent
    FScaledImage: TKAlphaBitmap;
    FWordBottomPadding: Integer;
    FWordTopPadding: Integer;
    procedure SetCrop(const Value: TKRect);
    procedure SetImage(const Value: TPicture);
    procedure SetImagePath(const Value: TKString);
    procedure SetScaleHeight(const Value: Integer);
    procedure SetScaleWidth(const Value: Integer);
    function GetOriginalHeight: Integer;
    function GetOriginalWidth: Integer;
    procedure SetOriginalHeight(const Value: Integer);
    procedure SetOriginalWidth(const Value: Integer);
    function GetScaleHeight: Integer;
    function GetScaleWidth: Integer;
    procedure SetScaleX(const Value: Integer);
    procedure SetScaleY(const Value: Integer);
  protected
    FCalcBaseLine: Integer;
    function ContentLength: Integer; override;
    function GetWrapMode: TKMemoBlockWrapMode; override;
    function GetImageHeight: Integer; virtual;
    function GetImageWidth: Integer; virtual;
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
    procedure ImageStyleChanged(Sender: TObject);
    function ScaledImage: TKAlphaBitmap; virtual;
    procedure SetWordBaseLine(Index: Integer; const Value: Integer); override;
    procedure SetWordBottomPadding(Index: Integer; const Value: Integer); override;
    procedure SetWordHeight(Index: Integer; const Value: Integer); override;
    procedure SetWordLeft(Index: Integer; const Value: Integer); override;
    procedure SetWordTop(Index: Integer; const Value: Integer); override;
    procedure SetWordTopPadding(Index: Integer; const Value: Integer); override;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    procedure Assign(AItem: TKMemoBlock); override;
    function CalcBaseLine(ACanvas: TCanvas): Integer; override;
    function OuterRect(ACaret: Boolean): TRect; virtual;
    function WordIndexToRect(ACanvas: TCanvas; AWordIndex: Integer; AIndex: Integer; ACaret: Boolean): TRect; override;
    function WordMeasureExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint; override;
    function WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint; AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; override;
    procedure WordPaintToCanvas(ACanvas: TCanvas; AIndex, ALeft, ATop: Integer); override;
    property Crop: TKRect read FCrop write SetCrop;
    property Image: TPicture read FImage write SetImage;
    property ImageStyle: TKMemoBlockStyle read FImageStyle;
    property ImageHeight: Integer read GetImageHeight;
    property ImageWidth: Integer read GetImageWidth;
    property OriginalHeight: Integer read GetOriginalHeight write SetOriginalHeight;
    property OriginalWidth: Integer read GetOriginalWidth write SetOriginalWidth;
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
    FPosition: TPoint;
    FWordBottomPadding: Integer;
    FWordTopPadding: Integer;
    procedure SetFixedWidth(const Value: Boolean);
    procedure SetRequiredWidth(const Value: Integer);
    procedure SetRequiredHeight(const Value: Integer);
    procedure SetClip(const Value: Boolean);
    procedure SetFixedHeight(const Value: Boolean);
  protected
    FRequiredHeight: Integer;
    FRequiredWidth: Integer;
    procedure AddSingleLine; virtual;
    procedure AddBlockLine(AStartBlock, AStartIndex, AEndBlock, AEndIndex,
      ALeft, ATop, AWidth, AHeight: Integer); virtual;
    procedure BlockStyleChanged(Sender: TObject);
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
    procedure RequiredHeightChanged; virtual;
    procedure RequiredWidthChanged; virtual;
    procedure SetWordBottomPadding(Index: Integer; const Value: Integer); override;
    procedure SetWordHeight(Index: Integer; const Value: Integer); override;
    procedure SetWordLeft(Index: Integer; const Value: Integer); override;
    procedure SetWordTop(Index: Integer; const Value: Integer); override;
    procedure SetWordTopPadding(Index: Integer; const Value: Integer); override;
    procedure SetWordWidth(Index: Integer; const Value: Integer); override;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    function CalcBaseLine(ACanvas: TCanvas): Integer; override;
    function CanAdd(AItem: TKMemoBlock): Boolean; override;
    procedure ClearSelection(ATextOnly: Boolean); override;
    function InsertParagraph(AIndex: Integer): Boolean; override;
    function InsertString(const AText: TKString; At: Integer = -1): Boolean; override;
    procedure NotifyDefaultParaChange; override;
    procedure NotifyDefaultTextChange; override;
    function Select(ASelStart, ASelLength: Integer): Boolean; override;
    procedure SetBlockExtent(AWidth, AHeight: Integer); virtual;
    procedure UpdateAttributes; virtual;
    function WordIndexToRect(ACanvas: TCanvas; AWordIndex: Integer; AIndex: Integer; ACaret: Boolean): TRect; override;
    function WordMeasureExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint; override;
    function WordMouseAction(AIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean; override;
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
    procedure ParaStyleChanged(Sender: TObject);
    procedure RequiredBorderWidthsChanged(Sender: TObject);
    procedure RequiredWidthChanged; override;
  public
    constructor Create(AParent: TKMemoBlocks); override;
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
    procedure RequiredHeightChanged; override;
    procedure RequiredWidthChanged; override;
  public
    constructor Create(AParent: TKMemoBlocks); override;
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
    FUpdateLock: Integer;
    procedure InternalSetCellSpan(ACol, ARow: Integer;
      const Value: TKCellSpan); virtual;
    procedure RequiredWidthChanged; override;
    procedure SetSize(AColCount, ARowCount: Integer); virtual;
  public
    constructor Create(AParent: TKMemoBlocks); override;
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
    procedure LockUpdate;
    function RowValid(ARow: Integer): Boolean; virtual;
    function WordMeasureExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint; override;
    function WordMouseAction(AIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean; override;
    function WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint; AWordIndex: Integer;
      AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; override;
    procedure UnlockUpdate;
    function UpdateUnlocked: Boolean;
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

  TKMemoBlocks = class(TObjectList)
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
    function GetRealSelStart: Integer;
    function GetSelLength: Integer;
    procedure SetIgnoreParaMark(const Value: Boolean);
    procedure SetItem(Index: Integer; const Value: TKMemoBlock);
  protected
    FLines: TKMemoLines;
    FRelPos: TKMemoSparseList;
    FRequiredWidth: Integer;
    FUpdateLock: Integer;
    FUpdateReasons: TKMemoUpdateReasons;
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
    function GetSelectionHasPara: Boolean; virtual;
    function GetSelectionParaStyle: TKMemoParaStyle; virtual;
    function GetSelectionTextStyle: TKMemoTextStyle; virtual;
    function GetSelText: TKString; virtual;
    function GetShowFormatting: Boolean; virtual;
    function GetText: TKString; virtual;
    function GetTotalLeftOffset: Integer; virtual;
    function GetTotalTopOffset: Integer; virtual;
    procedure GetWordIndexes(const ABlockIndex, ALineIndex: Integer; out AStart, AEnd: Integer); virtual;
    function LineToRect(ACanvas: TCanvas; AIndex, ALineIndex: Integer; ACaret: Boolean): TRect; virtual;
    function NormalToEOL(var AIndex: Integer): Boolean; virtual;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean = True; ATextOnly: Boolean = False): Boolean; virtual;
    procedure SetLineText(ALineIndex: Integer; const AValue: TKString); virtual;
    procedure SetSelectionParaStyle(const Value: TKMemoParaStyle); virtual;
    procedure SetSelectionTextStyle(const Value: TKMemoTextStyle); virtual;
    procedure SetText(const AValue: TKString);
    procedure Update(AReasons: TKMemoUpdateReasons); virtual;
  public
    constructor Create(AParent: TKMemoBlock); virtual;
    destructor Destroy; override;
    function AddAt(AObject: TKMemoBlock; At: Integer = -1): Integer; virtual;
    function AddContainer(At: Integer = -1): TKMemoContainer;
    function AddHyperlink(const AText, AURL: TKString; At: Integer = -1): TKMemoHyperlink; overload;
    function AddHyperlink(AItem: TKMemoHyperlink; At: Integer = -1): TKMemoHyperlink; overload;
    function AddImageBlock(const APath: TKString; At: Integer = -1): TKMemoImageBlock;
    function AddParagraph(At: Integer = -1): TKMemoParagraph;
    function AddTable(At: Integer = -1): TKMemoTable;
    function AddTextBlock(const AText: TKString; At: Integer = -1): TKMemoTextBlock;
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
    function GetNearestAnchorIndex(AIndex: Integer): Integer; virtual;
    function GetNearestParagraph(AIndex: Integer): TKMemoParagraph; virtual;
    function GetLastItemByClass(AIndex: Integer; AClass: TKMemoBlockClass): TKMemoBlock; virtual;
    function GetNextItemByClass(AIndex: Integer; AClass: TKMemoBlockClass): TKMemoBlock; virtual;
    procedure GetSelColors(out TextColor, Background: TColor); virtual;
    function IndexAboveLastLine(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function IndexAtBeginningOfContainer(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function IndexAtEndOfContainer(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function IndexBelowFirstLine(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function IndexToBlock(AIndex: Integer; out ALocalIndex: Integer): Integer; virtual;
    function IndexToBlocks(AIndex: Integer; out ALocalIndex: Integer): TKMemoBlocks; virtual;
    function IndexToItem(AIndex: Integer; out ALocalIndex: Integer): TKMemoBlock; virtual;
    function IndexToLine(AIndex: Integer): Integer; virtual;
    function IndexToRect(ACanvas: TCanvas; AIndex: Integer; ACaret, AAdjust: Boolean): TRect; virtual;
    function InsideOfTable: Boolean; virtual;
    procedure InsertChar(At: Integer; const AValue: TKChar; AOverWrite: Boolean); virtual;
    procedure InsertNewLine(At: Integer); virtual;
    procedure InsertPlainText(AIndex: Integer; const AValue: TKString); virtual;
    function InsertParagraph(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function InsertString(AIndex: Integer; AAdjust: Boolean; const AValue: TKString): Boolean; virtual;
    function LastTextStyle(AIndex: Integer): TKMemoTextStyle; virtual;
    function LineEndIndexByIndex(AIndex: Integer; AAdjust, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function LineStartIndexByIndex(AIndex: Integer; AAdjust: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    procedure LockUpdate;
    procedure MeasureExtent(ACanvas: TCanvas; ARequiredWidth: Integer); virtual;
    function MouseAction(AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean; virtual;
    procedure NotifyDefaultParaChange; virtual;
    procedure NotifyDefaultTextChange; virtual;
    function NextIndexByCharCount(AIndex, ACharCount: Integer): Integer;
    function NextIndexByHorzExtent(ACanvas: TCanvas; AIndex, AWidth: Integer; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function NextIndexByRowDelta(ACanvas: TCanvas; AIndex, ARowDelta, ALeftPos: Integer; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function NextIndexByVertExtent(ACanvas: TCanvas; AIndex, AHeight, ALeftPos: Integer; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function NextIndexByVertValue(ACanvas: TCanvas; AValue, ALeftPos: Integer; ADirection: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function PointToBlocks(ACanvas: TCanvas; const APoint: TPoint): TKMemoBlocks; virtual;
    procedure PaintToCanvas(ACanvas: TCanvas; ALeft, ATop: Integer; const ARect: TRect); virtual;
    function PointToIndex(ACanvas: TCanvas; const APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function PointToIndexOnLine(ACanvas: TCanvas; ALineIndex: Integer; const APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    procedure SetExtent(AWidth, AHeight: Integer); virtual;
    procedure UnlockUpdate;
    procedure UpdateAttributes; virtual;
    function UpdateUnlocked: Boolean;
    property BoundsRect: TRect read GetBoundsRect;
    property DefaultTextStyle: TKMemoTextStyle read GetDefaultTextStyle;
    property DefaultParaStyle: TKMemoParaStyle read GetDefaultParaStyle;
    property Empty: Boolean read GetEmpty;
    property Height: Integer read FExtent.Y;
    property IgnoreParaMark: Boolean read FIgnoreParaMark write SetIgnoreParaMark;
    property Items[Index: Integer]: TKMemoBlock read GetItem write SetItem; default;
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
    property MemoNotifier: IKMemoNotifier read FMemoNotifier write FMemoNotifier;
    property Parent: TKMemoBlock read FParent;
    property ParentBlocks: TKMemoBlocks read GetParentBlocks;
    property RealSelEnd: Integer read GetRealSelEnd;
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

  { @abstract(Multi line text editor base component). }
  TKCustomMemo = class(TKCustomControl, IKMemoNotifier)
  private
    FBackgroundImage: TPicture;
    FBlocks: TKMemoBlocks;
    FColors: TKMemoColors;
    FContentPadding: TKRect;
    FDisabledDrawStyle: TKEditDisabledDrawStyle;
    FKeyMapping: TKEditKeyMapping;
    FLeftPos: Integer;
    FMouseWheelAccumulator: Integer;
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
    function GetReadOnly: Boolean;
    function GetRequiredContentWidth: Integer;
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
    procedure SetColors(Value: TKMemoColors);
    procedure SetDisabledDrawStyle(Value: TKEditDisabledDrawStyle);
    procedure SetLeftPos(Value: Integer);
    procedure SetModified(Value: Boolean);
    procedure SetOptions(const Value: TKEditOptions);
    procedure SetReadOnly(Value: Boolean);
    procedure SetRequiredContentWidth(const Value: Integer);
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
  protected
    FActiveBlocks: TKMemoBlocks;
    FCaretRect: TRect;
    FHorzExtent: Integer;
    FHorzScrollExtent: Integer;
    FHorzScrollStep: Integer;
    FLinePosition: TKMemoLinePosition;
    FOldCaretRect: TRect;
    FPrinting: Boolean;
    FPreferredCaretPos: Integer;
    FRequiredMouseCursor: TCursor;
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
    { Begins a new undo group. Use the GroupKind parameter to label it. }
    procedure BeginUndoGroup(AGroupKind: TKMemoChangeKind);
    { Converts a rectangle relative to active blocks to a rectangle relative to TKMemo. }
    function BlockRectToRect(const ARect: TRect): TRect; virtual;
    { IKMemoNotifier implementation. }
    procedure BlocksFreeNotification(ABlocks: TKMemoBlocks);
    { Calls mouse move action for all blocks with current mouse coordinates and shift state. }
    procedure BlocksMouseMove; virtual;
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
    { Update the editor after block changes. }
    procedure DoUpdate(Reasons: TKMemoUpdateReasons);
    { Closes the undo group created by @link(TKCustomMemo.BeginUndoGroup). }
    procedure EndUndoGroup;
    { Notify blocks about memo font change. }
    procedure FontChange(Sender: TObject); virtual;
    { IKMemoNotifier implementation. }
    function GetDefaultTextStyle: TKMemoTextStyle;
    { IKMemoNotifier implementation. }
    function GetDefaultParaStyle: TKMemoParaStyle;
    { Returns actual scroll padding in horizontal direction. }
    function GetHorzScrollPadding: Integer; virtual;
    { IKMemoNotifier implementation. }
    function GetLinePosition: TKMemoLinePosition;
    { IKMemoNotifier implementation. }
    function GetPaintSelection: Boolean;
    { IKMemoNotifier implementation. }
    function GetPrinting: Boolean;
    { IKMemoNotifier implementation. }
    procedure GetSelColors(out Foreground, Background: TColor);
    { IKMemoNotifier implementation. }
    function GetShowFormatting: Boolean;
    { Returns "real" selection end - with always higher index value than selection start value. }
    function GetRealSelEnd: Integer; virtual;
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
    { Updates information about printed shape. }
    procedure MeasurePages(var Info: TKPrintMeasureInfo); override;
    { Overriden method - updates caret position/selection. }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - updates caret position/selection and initializes scrolling
      when needed. }
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - releases mouse capture acquired by MouseDown. }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Paints the document to specified canvas. }
    procedure PaintContent(ACanvas: TCanvas; const ARect: TRect; ALeftOfs, ATopOfs: Integer);
    { Paints a page to a printer/preview canvas. }
    procedure PaintPage; override;
    { Overriden method - calls PaintContent to paint the document into window client area. }
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    { Reacts on default paragraph style changes and notifies all paragraph blocks. }
    procedure ParaStyleChanged(Sender: TObject); virtual;
    { Converts a point relative to TKMemo to a point relative to active blocks. }
    function PointToBlockPoint(const APoint: TPoint; ACalcActive: Boolean = True): TPoint; virtual;
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
      <LI><I>Recreate</I> - set to True to recreate the caret after it has already
      been created and displayed</LI>
      </UL> }
    procedure UpdateEditorCaret(AShow: Boolean = True); virtual;
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
    procedure Clear;
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
    procedure LoadFromRTFStream(AStream: TStream; AtIndex: Integer); virtual;
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
    procedure SaveToRTF(const AFileName: TKString; ASelectedOnly: Boolean = False); virtual;
    { Save contents to a RTF stream. }
    procedure SaveToRTFStream(AStream: TStream; ASelectedOnly: Boolean = False); virtual;
    { Save contents to a plain text file. }
    procedure SaveToTXT(const AFileName: TKString; ASelectedOnly: Boolean = False); virtual;
    { Specifies the current selection. This is faster than combination of SelStart and SelLength. }
    procedure Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean = True); virtual;
    { Prepare to insert a new block at given position. Returns requested block index. }
    function SplitAt(AIndex: Integer): Integer; virtual;
    { Gives access to active memo block (the outermost block at caret position within ActiveBlocks). }
    property ActiveBlock: TKMemoBlock read GetActiveBlock;
    { Gives access to innermost active memo block (the innermost block at caret position within ActiveBlocks). }
    property ActiveInnerBlock: TKMemoBlock read GetActiveInnerBlock;
    { Gives access to active memo blocks - containers of texts, images etc..
      ActiveBlocks might be different from Blocks when editing the embedded text box etc. }
    property ActiveBlocks: TKMemoBlocks read FActiveBlocks;
    { Gives access to innermost active memo blocks - containers of texts, images etc..
      ActiveInnerBlocks might be different from ActiveBlocks when inside of a table etc. }
    property ActiveInnerBlocks: TKMemoBlocks read GetActiveInnerBlocks;
    { Gives access to memo blocks - containers of texts, images etc.. }
    property Blocks: TKMemoBlocks read FBlocks;
    { Background image. }
    property BackgroundImage: TPicture read FBackgroundImage;
    { Returns current caret position = selection end. }
    property CaretPos: Integer read GetSelEnd;
    { Returns True if caret is visible. }
    property CaretVisible: Boolean read GetCaretVisible;
    { Makes it possible to take all color properties from another TKCustomMemo class. }
    property Colors: TKMemoColors read FColors write SetColors;
    { Specifies the padding around the memo contents. }
    property ContentPadding: TKRect read FContentPadding;
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
    { Returns True if the buffer was modified - @link(eoUndoAfterSave) taken into
      account. }
    property Modified: Boolean read GetModified write SetModified;
    { Specifies the editor options that do not affect painting. }
    property Options: TKEditOptions read FOptions write SetOptions stored IsOptionsStored;
    { Specifies default style for paragraphs. }
    property ParaStyle: TKMemoParaStyle read FParaStyle;
    { Specifies whether the editor has to be read only editor. }
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    { Returns "real" selection end - with always higher index value than selection start value. }
    property RealSelEnd: Integer read GetRealSelEnd;
    { Returns "real" selection start - with always lower index value than selection end value. }
    property RealSelStart: Integer read GetRealSelStart;
    { Specifies the required content width. }
    property RequiredContentWidth: Integer read GetRequiredContentWidth write SetRequiredContentWidth;
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
    { See TKCustomMemo.@link(TKCustomMemo.Text) for details. }
    property Text;
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

{ TKMemoTextStyle }

constructor TKMemoTextStyle.Create;
begin
  inherited;
  FBrush := TBrush.Create;
  FBrush.OnChange := BrushChanged;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FOnChanged := nil;
  FLocked := False;
  Defaults;
end;

procedure TKMemoTextStyle.Defaults;
begin
  FAllowBrush := True;
  FBrush.Style := bsClear;
  FBrushChanged := False;
  FCapitals := tcaNone;
  FFont.Color := clWindowText;
  FFontChanged := False;
  FStyleChanged := False;
end;

destructor TKMemoTextStyle.Destroy;
begin
  FBrush.Free;
  FFont.Free;
  inherited;
end;

function TKMemoTextStyle.EqualProperties(AValue: TKMemoTextStyle): Boolean;
begin
  if AValue <> nil then
  begin
    Result :=
      (AValue.AllowBrush = AllowBrush) and
      (AValue.Capitals = Capitals) and
      CompareBrushes(AValue.Brush, Brush) and
      CompareFonts(AValue.Font, Font);
  end else
    Result := False;
end;

procedure TKMemoTextStyle.FontChanged(Sender: TObject);
begin
  if not FLocked then
  begin
    FFontChanged := True;
    Changed;
  end;
end;

procedure TKMemoTextStyle.NotifyChange(AValue: TKMemoTextStyle);
begin
  if not FLocked then
  begin
    FLocked := True;
    try
      if not FFontChanged then
        FFont.Assign(AValue.Font);
      if not FBrushChanged then
        FBrush.Assign(AValue.Brush);
    finally
      FLocked := False;
    end;
  end;
end;

procedure TKMemoTextStyle.Assign(ASource: TPersistent);
begin
  if ASource is TKMemoTextStyle then
  begin
    Brush.Assign(TKMemoTextStyle(ASource).Brush);
    Capitals := TKMemoTextStyle(ASource).Capitals;
    Font.Assign(TKMemoTextStyle(ASource).Font);
  end;
end;

procedure TKMemoTextStyle.BrushChanged(Sender: TObject);
begin
  if not FLocked then
  begin
    FBrushChanged := True;
    Changed;
  end;
end;

procedure TKMemoTextStyle.Changed;
begin
  FStyleChanged := True;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TKMemoTextStyle.SetAllowBrush(const Value: Boolean);
begin
  if Value <> FAllowBrush then
  begin
    FAllowBrush := Value;
    Changed;
  end;
end;

procedure TKMemoTextStyle.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TKMemoTextStyle.SetCapitals(const Value: TKMemoTextCapitals);
begin
  if Value <> FCapitals then
  begin
    FCapitals := Value;
    Changed;
  end;
end;

procedure TKMemoTextStyle.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

{ TKMemoParagraphStyle }

constructor TKMemoBlockStyle.Create;
begin
  inherited;
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
  FLocked := False;
  Defaults;
end;

procedure TKMemoBlockStyle.Defaults;
begin
  FBorderColor := clBlack;
  FBorderRadius := 0;
  FBorderWidth := 0;
  FBorderWidths.AssignFromValues(0, 0, 0, 0);
  FBrush.Style := bsClear;
  FContentPadding.AssignFromValues(0, 0, 0, 0);
  FContentMargin.AssignFromValues(0, 0, 0, 0);
  FChanged := False;
  FHAlign := halLeft;
  FWrapMode := wrAround;
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
    BorderColor := TKMemoBlockStyle(ASource).BorderColor;
    BorderRadius := TKMemoBlockStyle(ASource).BorderRadius;
    BorderWidth := TKMemoBlockStyle(ASource).BorderWidth;
    BorderWidths.Assign(TKMemoBlockStyle(ASource).BorderWidths);
    Brush.Assign(TKMemoBlockStyle(ASource).Brush);
    WrapMode := TKMemoParaStyle(ASource).WrapMode;
    ContentMargin.Assign(TKMemoBlockStyle(ASource).ContentMargin);
    ContentPadding.Assign(TKMemoBlockStyle(ASource).ContentPadding);
    HAlign := TKMemoParaStyle(ASource).HAlign;
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
  Changed;
end;

procedure TKMemoBlockStyle.Changed;
begin
  if not FLocked then
  begin
    FChanged := True;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
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
  if not (FChanged or FLocked) then
  begin
    FLocked := True;
    try
      Assign(AValue);
    finally
      FLocked := False;
    end;
  end;
end;

procedure TKMemoBlockStyle.PaintBox(ACanvas: TCanvas; const ARect: TRect);
var
  R, RB: TRect;
begin
  if (FBrush.Style <> bsClear) or (FBorderWidth > 0) or FBorderWidths.NonZero then with ACanvas do
  begin
    if FBorderWidth > 0 then
    begin
      Pen.Style := psSolid;
      Pen.Width := FBorderWidth;
      Pen.Color := FBorderColor;
    end else
      Pen.Style := psClear;
    if FBorderWidths.NonZero then
    begin
      R := ARect;
      Brush.Color := FBorderColor;
      Brush.Style := bsSolid;
      if FBorderWidths.Left <> 0 then
      begin
        RB := ARect;
        RB.Right := RB.Left + FBorderWidths.Left;
        R.Left := RB.Right;
        FillRect(RB);
      end;
      if FBorderWidths.Top <> 0 then
      begin
        RB := ARect;
        RB.Bottom := RB.Top + FBorderWidths.Top;
        R.Top := RB.Bottom;
        FillRect(RB);
      end;
      if FBorderWidths.Right <> 0 then
      begin
        RB := ARect;
        RB.Left := RB.Right - FBorderWidths.Right;
        R.Right := RB.Left;
        FillRect(RB);
      end;
      if FBorderWidths.Bottom <> 0 then
      begin
        RB := ARect;
        RB.Top := RB.Bottom - FBorderWidths.Bottom;
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
      if FBorderRadius > 0 then
        RoundRectangle(ACanvas, ARect, FBorderRadius, FBorderRadius)
      else if FBorderWidth > 0 then
        Rectangle(ARect)
      else if Brush.Style <> bsClear then
        FillRect(ARect);
    end;
  end;
end;

procedure TKMemoBlockStyle.SetBorderColor(const Value: TColor);
begin
  if Value <> FBorderColor then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TKMemoBlockStyle.SetBorderRadius(const Value: Integer);
begin
  if Value <> FBorderRadius then
  begin
    FBorderRadius := Value;
    Changed;
  end;
end;

procedure TKMemoBlockStyle.SetBorderWidth(const Value: Integer);
begin
  if Value <> FBorderWidth then
  begin
    FBorderWidth := Value;
    Changed;
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
    Changed;
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
  Changed;
end;

procedure TKMemoBlockStyle.SetHAlign(const Value: TKHAlign);
begin
  if Value <> FHAlign then
  begin
    FHAlign := Value;
    Changed;
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

{ TKMemoParagraphStyle }

constructor TKMemoParaStyle.Create;
begin
  inherited;
end;

procedure TKMemoParaStyle.Defaults;
begin
  inherited;
  FFirstIndent := 0;
  FWordWrap := True;
end;

procedure TKMemoParaStyle.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TKMemoParaStyle then
  begin
    FirstIndent := TKMemoParaStyle(ASource).FirstIndent;
    WordWrap := TKMemoParaStyle(ASource).WordWrap;
  end;
end;

procedure TKMemoParaStyle.SetFirstIndent(const Value: Integer);
begin
  if Value <> FFirstIndent then
  begin
    FFirstIndent := Value;
    Changed;
  end;
end;

procedure TKMemoParaStyle.SetWordWrap(const Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    Changed;
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

constructor TKMemoWord.Create;
begin
  FBaseLine := 0;
  FBottomPadding := 0;
  FExtent := CreateEmptyPoint;
  FEndIndex := 0;
  FPosition := CreateEmptyPoint;
  FStartIndex := 0;
  FTopPadding := 0;
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

{ TKMemoSparseItem }

constructor TKMemoSparseItem.Create;
begin
  inherited;
  FIndex := 0;
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
  FBackgroundImage := TPicture.Create;
  FBlocks := TKMemoBlocks.Create(nil);
  FBlocks.MemoNotifier := Self;
  FBlocks.OnUpdate := DoUpdate;
  FActiveBlocks := FBlocks;
  FRequiredMouseCursor := crIBeam;
  FCaretRect := CreateEmptyRect;
  FColors := TKMemoColors.Create(Self);
  FContentPadding := TKRect.Create;
  FContentPadding.Left := 10;
  FContentPadding.Right := 10;
  FContentPadding.Top := 10;
  FContentPadding.OnChanged := ContentPaddingChanged;
  FDisabledDrawStyle := cEditDisabledDrawStyleDef;
  FHorzScrollStep := cHorzScrollStepDef;
  FLeftPos := 0;
  FLinePosition := eolInside;
  FMouseWheelAccumulator := 0;
  FOldCaretRect := CreateEmptyRect;
  FOptions := [eoGroupUndo];
  FPrinting := False;
  FPreferredCaretPos := 0;
  FKeyMapping := TKEditKeyMapping.Create;
  FParaStyle := TKMemoParaStyle.Create;
  FParaStyle.OnChanged := ParaStyleChanged;
  FRedoList := TKMemoChangeList.Create(Self, nil);
  FRequiredContentWidth := 0;
  FScrollBars := ssBoth;
  FScrollPadding := cScrollPaddingDef;
  FScrollSpeed := cScrollSpeedDef;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := FScrollSpeed;
  FScrollTimer.OnTimer := ScrollTimerHandler;
  FStates := [];
  FTextStyle := TKMemoTextStyle.Create;
  FTextStyle.Font.Assign(Font);
  FTextStyle.OnChanged := TextStyleChanged;
  FTopPos := 0;
  FUndoList := TKMemoChangeList.Create(Self, FRedoList);
  FUndoList.OnChange := UndoChange;
  FUpdateLock := 0;
  FVertScrollStep := cVertScrollStepDef;
  FWordBreaks := cDefaultWordBreaks;
  FOnChange := nil;
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
  FKeyMapping.Free;
  FTextStyle.Free;
  FContentPadding.Free;
  FColors.Free;
  FBlocks.Free;
  FBackgroundImage.Free;
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
      Self.ParaStyle.Assign(ParaStyle);
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
      Self.TextStyle.Assign(TextStyle);
      Self.Visible := Visible;
    finally
      Self.UnlockUpdate;
    end;
  end
  else
    inherited;
end;

procedure TKCustomMemo.BeginUndoGroup(AGroupKind: TKMemoChangeKind);
begin
  FUndoList.BeginGroup(AGroupKind);
end;

function TKCustomMemo.BlockRectToRect(const ARect: TRect): TRect;
begin
  Result := ARect;
  OffsetRect(Result, ContentLeft, ContentTop);
  if FActiveBlocks <> FBlocks then
  begin
    OffsetRect(Result, FActiveBlocks.TotalLeftOffset, FActiveBlocks.TotalTopOffset);
  end;
end;

procedure TKCustomMemo.BlocksFreeNotification(ABlocks: TKMemoBlocks);
begin
  if ABlocks = FActiveBlocks then
    FActiveBlocks := FBlocks;
end;

procedure TKCustomMemo.BlocksMouseMove;
var
  P: TPoint;
  OldCursor: TCursor;
begin
  P := ScreenToClient(Mouse.CursorPos);
  OldCursor := FRequiredMouseCursor;
  FBlocks.MouseAction(maMove, PointToBlockPoint(P, False), GetShiftState);
  if FRequiredMouseCursor <> OldCursor then
    SetMouseCursor(P.X, P.Y);
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

procedure TKCustomMemo.Clear;
begin
  FBlocks.LockUpdate;
  try
    FBlocks.Clear;
    FBlocks.FixEmptyBlocks;
    FTextStyle.Defaults;
    FParaStyle.Defaults;
    FColors.BkGnd := cBkGndDef;
    FBackgroundImage.Graphic := nil;
  finally
    FBlocks.UnlockUpdate;
  end;
end;

procedure TKCustomMemo.ClearSelection(ATextOnly: Boolean);
var
  Len: Integer;
begin
  Len := FActiveBlocks.SelLength;
  FActiveBlocks.ClearSelection(ATextOnly);
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
    TmpSelLength := SelLength;
    case Command of
      // movement commands
      ecLeft, ecSelLeft:
        Result := TmpSelEnd > 0;
      ecRight:
        Result := TmpSelEnd < FActiveBlocks.SelectableLength;
      ecSelRight:
        Result := TmpSelEnd < FActiveBlocks.SelectableLength;
      ecUp, ecSelUp, ecPageUp, ecSelPageUp:
        Result := FActiveBlocks.IndexBelowFirstLine(TmpSelEnd, True);
      ecDown, ecPagedown, ecSelDown, ecSelPageDown:
        Result := FActiveBlocks.IndexAboveLastLine(TmpSelEnd, True);
      ecLineStart, ecPageLeft, ecSelLineStart, ecSelPageLeft:
        Result := TmpSelEnd > FActiveBlocks.LineStartIndexByIndex(TmpSelEnd, True, TmpLinePos);
      ecLineEnd, ecPageRight:
        Result := TmpSelEnd < FActiveBlocks.LineEndIndexByIndex(TmpSelEnd, True, False, TmpLinePos);
      ecSelLineEnd, ecSelPageRight:
        Result := TmpSelEnd < FActiveBlocks.LineEndIndexByIndex(TmpSelEnd, True, True, TmpLinePos);
      ecPageTop, ecSelPageTop:
        Result := TmpSelEnd <> FActiveBlocks.NextIndexByVertValue(Canvas, -ContentTop + VertScrollPadding, FPreferredCaretPos, False, TmpLinePos);
      ecPageBottom, ecSelPageBottom:
        Result := TmpSelEnd <> FActiveBlocks.NextIndexByVertValue(Canvas, -ContentTop + VertScrollPadding + ClientHeight, FPreferredCaretPos, True, TmpLinePos);
      ecEditorTop, ecSelEditorTop:
        Result := TmpSelEnd > 0;
      ecEditorBottom, ecSelEditorBottom:
        Result := TmpSelEnd < FActiveBlocks.SelectableLength;
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
        Result := not Empty and (not ReadOnly or (Command = ecCopy)) and (TmpSelLength <> 0);
      ecPaste:
        Result := not ReadOnly and (ClipBoard.FormatCount > 0);
      ecInsertChar, ecInsertString, ecInsertNewLine:
        Result := not ReadOnly;
      ecDeleteLastChar:
        Result := not (Empty or ReadOnly) and ((TmpSelLength > 0) or (TmpSelEnd > 0));
      ecDeleteChar:
        Result := not (Empty or ReadOnly) and ((TmpSelLength > 0) or (TmpSelEnd < FActiveBlocks.SelectableLength - 1));
      ecDeleteBOL:
        Result := not (Empty or ReadOnly) and ((TmpSelLength > 0) or (TmpSelEnd <> FActiveBlocks.LineStartIndexByIndex(TmpSelEnd, True, TmpLinePos)));
      ecDeleteEOL:
        Result := not (Empty or ReadOnly) and ((TmpSelLength > 0) or (TmpSelEnd <> FActiveBlocks.LineEndIndexByIndex(TmpSelEnd, True, True, TmpLinePos)));
      ecDeleteLine, ecClearAll, ecReplace:
        Result := not (Empty or ReadOnly);
      ecClearSelection:
        Result := not (Empty or ReadOnly) and (TmpSelLength > 0);
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
  DoUpdate([muExtent]);
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
  FActiveBlocks.DeleteBOL(At);
  Modified := True;
end;

procedure TKCustomMemo.DeleteChar(At: Integer);
begin
  FActiveBlocks.DeleteChar(At);
  Modified := True;
end;

procedure TKCustomMemo.DeleteEOL(At: Integer);
begin
  FActiveBlocks.DeleteEOL(At);
  Modified := True;
end;

procedure TKCustomMemo.DeleteLastChar(At: Integer);
begin
  FActiveBlocks.DeleteLastChar(At);
  Modified := True;
end;

procedure TKCustomMemo.DeleteLine(At: Integer);
begin
  FActiveBlocks.DeleteLine(At);
  Modified := True;
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
begin
  // copy selected blocks as plain text and RTF to clipboard
  S := FActiveBlocks.SelText;
  Stream := TMemoryStream.Create;
  try
    SaveToRTFStream(Stream, True);
    Result := ClipBoardSaveStreamAs(cRichText, Stream, S);
  finally
    Stream.Free;
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
    ScrollBy(0, - WheelClicks * LinesToScroll, True);
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
      OldSelectableLength := FActiveBlocks.SelectableLength;
      if Stream.Size > 0 then
      begin
        Stream.Seek(0, soFromBeginning);
        //Stream.SaveToFile('pasted.rtf'); debug line
        LoadFromRTFStream(Stream, SelEnd);
      end else
        FActiveBlocks.InsertPlainText(SelEnd, S);
      NewSelectableLength := FActiveBlocks.SelectableLength;
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

procedure TKCustomMemo.DoUpdate(Reasons: TKMemoUpdateReasons);
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
        NewSelEnd := FActiveBlocks.NextIndexByCharCount(TmpSelEnd, -1);
        SelectionInit(NewSelEnd, True, eolInside);
      end;
      ecSelLeft:
      begin
        NewSelEnd := FActiveBlocks.NextIndexByCharCount(TmpSelEnd, -1);
        SelectionExpand(NewSelEnd, True, eolInside);
      end;
      ecRight:
      begin
        NewSelEnd := FActiveBlocks.NextIndexByCharCount(TmpSelEnd, 1);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelRight:
      begin
        NewSelEnd := FActiveBlocks.NextIndexByCharCount(TmpSelEnd, 1);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecUp: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, -1, FPreferredCaretPos, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelUp: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, -1, FPreferredCaretPos, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecDown: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, 1, FPreferredCaretPos, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelDown: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, 1, FPreferredCaretPos, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecLineStart: 
      begin
        NewSelEnd := FActiveBlocks.LineStartIndexByIndex(TmpSelEnd, True, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelLineStart: 
      begin
        NewSelEnd := FActiveBlocks.LineStartIndexByIndex(TmpSelEnd, True, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecLineEnd: 
      begin
        NewSelEnd := FActiveBlocks.LineEndIndexByIndex(TmpSelEnd, True, False, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelLineEnd: 
      begin
        NewSelEnd := FActiveBlocks.LineEndIndexByIndex(TmpSelEnd, True, True, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageUp: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, -ClientHeight, FPreferredCaretPos, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageUp: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, -ClientHeight, FPreferredCaretPos, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageDown:
      begin
        NewSelEnd := FActiveBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, ClientHeight, FPreferredCaretPos, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageDown: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, ClientHeight, FPreferredCaretPos, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageLeft: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, -ClientWidth, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageLeft: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, -ClientWidth, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageRight: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, ClientWidth, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageRight:
      begin
        NewSelEnd := FActiveBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, ClientWidth, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageTop: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByVertValue(Canvas, -ContentTop + VertScrollPadding, FPreferredCaretPos, False, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageTop: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByVertValue(Canvas, FTopPos * FVertScrollStep + VertScrollPadding, FPreferredCaretPos, False, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageBottom: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByVertValue(Canvas, -ContentTop + ClientHeight - VertScrollPadding, FPreferredCaretPos, True, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageBottom: 
      begin
        NewSelEnd := FActiveBlocks.NextIndexByVertValue(Canvas, FTopPos * FVertScrollStep + ClientHeight - VertScrollPadding, FPreferredCaretPos, True, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecEditorTop: SelectionInit(0, True, eolInside);
      ecSelEditorTop: SelectionExpand(0, True, eolInside);
      ecEditorBottom: SelectionInit(FActiveBlocks.SelectableLength, True, eolEnd);
      ecSelEditorBottom: SelectionExpand(FActiveBlocks.SelectableLength, True, eolEnd);
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
        ScrollBy(0, -1, True);
        while CommandEnabled(ecUp) and (FCaretRect.Top + FCaretRect.Bottom > ClientHeight - VertScrollPadding) do
          ExecuteCommand(ecUp);
      end;
      ecScrollDown:
      begin
        ScrollBy(0, 1, True);
        while CommandEnabled(ecDown) and (FCaretRect.Top < VertScrollPadding) do
          ExecuteCommand(ecDown);
      end;
      ecScrollLeft:
      begin
        ScrollBy(-1, 0, True);
        while CommandEnabled(ecLeft) and (FCaretRect.Left + FCaretRect.Right > ClientWidth - HorzScrollPadding) do
          ExecuteCommand(ecLeft);
      end;
      ecScrollRight:
      begin
        ScrollBy(1, 0, True);
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
      ecSelectAll: Select(0, FActiveBlocks.SelectableLength);
      ecClearAll:
      begin
        Select(0, FActiveBlocks.SelectableLength);
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
  Dummy: Integer;
begin
  Result := FActiveBlocks.IndexToItem(FActiveBlocks.SelEnd, Dummy);
end;

function TKCustomMemo.GetActiveInnerBlock: TKMemoBlock;
var
  LocalIndex: Integer;
  Items: TKmemoBlocks;
begin
  Items := FActiveBlocks.IndexToBlocks(FActiveBlocks.SelEnd, LocalIndex);
  if Items <> nil then
    Result := Items.IndexToItem(LocalIndex, LocalIndex)
  else
    Result := nil;
end;

function TKCustomMemo.GetActiveInnerBlocks: TKMemoBlocks;
var
  Dummy: Integer;
begin
  Result := FActiveBlocks.IndexToBlocks(FActiveBlocks.SelEnd, Dummy);
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

function TKCustomMemo.GetModified: Boolean;
begin
  Result := (elModified in FStates) or FUndoList.Modified;
end;

function TKCustomMemo.GetPaintSelection: Boolean;
begin
  if FPrinting then
    Result := poPaintSelection in PageSetup.Options
  else
    Result := True;
end;

function TKCustomMemo.GetPrinting: Boolean;
begin
  Result := FPrinting;
end;

function TKCustomMemo.GetMaxLeftPos: Integer;
begin
  Result := (FHorzExtent - ClientWidth) div FHorzScrollStep;
end;

function TKCustomMemo.GetMaxTopPos: Integer;
begin
  Result := (FVertExtent - ClientHeight) div FVertScrollStep;
end;

function TKCustomMemo.GetReadOnly: Boolean;
begin
  Result := elReadOnly in FStates;
end;

function TKCustomMemo.GetRealSelEnd: Integer;
begin
  Result := FActiveBlocks.RealSelEnd;
end;

function TKCustomMemo.GetRealSelStart: Integer;
begin
  Result := FActiveBlocks.RealSelStart;
end;

function TKCustomMemo.GetRequiredContentWidth: Integer;
begin
  if FRequiredContentWidth > 0 then
    Result := FRequiredContentWidth
  else
    Result := ClientWidth - FContentPadding.Left - FContentPadding.Right;
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
  Result := FActiveBlocks.SelectableLength;
end;

function TKCustomMemo.GetSelectionHasPara: Boolean;
begin
  Result := FActiveBlocks.SelectionHasPara;
end;

function TKCustomMemo.GetSelectionParaStyle: TKMemoParaStyle;
begin
  Result := FActiveBlocks.SelectionParaStyle;
  if Result = nil then
    Result := FParaStyle;
end;

function TKCustomMemo.GetSelectionTextStyle: TKMemoTextStyle;
begin
  Result := FActiveBlocks.SelectionTextStyle;
  if Result = nil then
    Result := FTextStyle;
end;

function TKCustomMemo.GetSelEnd: Integer;
begin
  Result := FActiveBlocks.SelEnd;
end;

function TKCustomMemo.GetSelLength: Integer;
begin
  Result := FActiveBlocks.SelLength;
end;

function TKCustomMemo.GetSelStart: Integer;
begin
  Result := FActiveBlocks.SelStart
end;

function TKCustomMemo.GetSelText: TKString;
begin
  Result := FActiveBlocks.SelText;
end;

function TKCustomMemo.GetShowFormatting: Boolean;
begin
  if FPrinting then
    Result := False
  else
    Result := eoShowFormatting in FOptions;
end;

function TKCustomMemo.GetText: TKString;
begin
  Result := FActiveBlocks.Text;
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
  Result := BlockRectToRect(FActiveBlocks.IndexToRect(Canvas, AValue, ACaret, FActiveBlocks.EOLToNormal(AValue)));
end;

procedure TKCustomMemo.InsertChar(At: Integer; const AValue: TKChar);
begin
  FActiveBlocks.InsertChar(At, AValue, elOverwrite in FStates);
  Modified := True;
end;

procedure TKCustomMemo.InsertNewLine(At: Integer);
begin
  FActiveBlocks.InsertNewLine(At);
  Modified := True;
end;

procedure TKCustomMemo.InsertString(At: Integer; const AValue: TKString);
begin
  if AValue <> '' then
  begin
    BeginUndoGroup(ckInsert);
    try
      if FActiveBlocks.SelLength > 0 then
      begin
        FActiveBlocks.ClearSelection;
        At := FActiveBlocks.SelEnd;
      end;
      // always insert (don't overwrite)
      FActiveBlocks.InsertString(At, True, AValue);
    finally
      EndUndoGroup;
    end;
    Modified := True;
  end
end;

function TKCustomMemo.IsOptionsStored: Boolean;
begin
  Result := FOptions <> [eoGroupUndo];
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
      VK_ESCAPE: Include(FStates, elIgnoreNextChar);
      VK_SHIFT, VK_CONTROL, VK_MENU: BlocksMouseMove;
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
      VK_SHIFT, VK_CONTROL, VK_MENU: BlocksMouseMove;
    end;
  end;
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
    Reader.LoadFromStream(AStream, SelEnd);
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
    List := TStringList.Create;
    try
      List.LoadFromFile(AFileName);
      FBlocks.LockUpdate;
      try
        FBlocks.Clear;
        FBlocks.Text := List.Text;
      finally
        FBlocks.UnlockUpdate;
      end;
    finally
      List.Free;
    end;
  end;
end;

procedure TKCustomMemo.MeasurePages(var Info: TKPrintMeasureInfo);
var
  FitToPage: Boolean;
  Scale: Double;
  APageSetup: TKPrintPageSetup;
begin
  APageSetup := PageSetup;
  FitToPage := poFitToPage in APageSetup.Options;
  Scale := APageSetup.Scale / 100;
  Info.OutlineWidth := ContentWidth;
  Info.OutlineHeight := ContentHeight;
  Info.ControlHorzPageCount := 1; // cut text off
  if Info.OutlineWidth > 0 then
  begin
    if FitToPage then
      Scale := APageSetup.MappedControlPaintAreaWidth / Info.OutlineWidth;
    Info.ControlVertPageCount := DivUp(ContentHeight, Round(APageSetup.MappedPaintAreaHeight / Scale));
  end;
end;

procedure TKCustomMemo.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
  Action: TKMemoMouseAction;
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
    if not FBlocks.MouseAction(Action, PointToBlockPoint(P, False), Shift) then
    begin
      if (Button = mbLeft) and not (ssDouble in Shift) then
      begin
        Include(FStates, elMouseCapture);
        SelectionInit(P, False);
        ClampInView(@P, True);
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
  if (elMouseCapture in FStates) then
  begin
    if not FScrollTimer.Enabled then
    begin
      SelectionExpand(P, True);
    end;
  end else
  begin
    FRequiredMouseCursor := crIBeam;
    FBlocks.MouseAction(maMove, PointToBlockPoint(P, False), GetShiftState);
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
  case Button of
    mbRight: Action := maRightUp;
    mbMiddle: Action := maMidUp;
  else
    Action := maLeftUp;
  end;
  P := Point(X, Y);
  FBlocks.MouseAction(Action, PointToBlockPoint(P, False), Shift);
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
      SelectionInit(P, False);
      ClampInView(@P, True);
    end;
  end;
end;

procedure TKCustomMemo.PaintContent(ACanvas: TCanvas; const ARect: TRect; ALeftOfs, ATopOfs: Integer);
var
  H, I, J, SaveIndex: Integer;
begin
  SaveIndex := SaveDC(ACanvas.handle); // don't delete
  try
    if (FBackgroundImage.Graphic <> nil) and not FBackgroundImage.Graphic.Empty then
    begin
      I := ARect.Left + (ALeftOfs - FContentPadding.Left) mod FBackgroundImage.Width;
      J := ARect.Top + (ATopOfs - FContentPadding.Top) mod FbackgroundImage.Height;
      H := I;
      while J < ARect.Bottom do
      begin
        ACanvas.Draw(I, J, FBackgroundImage.Graphic);
        Inc(I, FBackgroundImage.Width);
        if I >= ARect.Right then
        begin
          Inc(J, FBackgroundImage.Height);
          I := H;
        end;
      end;
    end
    else if Color <> clNone then
    begin
      Brush.Color := FColors.BkGnd;
      Brush.Style := bsSolid;
      ACanvas.Brush.Assign(Brush);
      ACanvas.FillRect(ARect);
    end;
    FBlocks.PaintToCanvas(ACanvas, ALeftOfs, ATopOfs, ARect);
  finally
    RestoreDC(ACanvas.Handle, SaveIndex);
  end;
end;

procedure TKCustomMemo.PaintPage;
var
  AreaWidth, AreaHeight: Integer;
  TmpRect, TmpRect1: TRect;
  APageSetup: TKPrintPageSetup;
begin
  // poSelOnly not supported
  // poUseColor not supported - always paints in colors
  APageSetup := PageSetup;
  AreaWidth := Round(APageSetup.MappedControlPaintAreaWidth / APageSetup.CurrentScale);
  AreaHeight := Round(APageSetup.MappedPaintAreaHeight / APageSetup.CurrentScale);
  TmpRect := Rect(0, 0, APageSetup.MappedOutlineWidth, APageSetup.MappedOutlineHeight);
  TmpRect1 := Rect(0, 0, AreaWidth, AreaHeight);
  IntersectRect(TmpRect, TmpRect, TmpRect1);
  TmpRect1 := TmpRect;
  TranslateRectToDevice(APageSetup.Canvas.Handle, TmpRect1);
  SelectClipRect(APageSetup.Canvas.Handle, TmpRect1);

  FPrinting := True;
  try
    FBlocks.MeasureExtent(APageSetup.Canvas, RequiredContentWidth);
    PaintContent(APageSetup.Canvas, TmpRect,
      FContentPadding.Left,
      FContentPadding.Top - (APageSetup.CurrentPageControl - 1) * AreaHeight);
  finally
    FPrinting := False;
  end;
end;

procedure TKCustomMemo.PaintToCanvas(ACanvas: TCanvas);
begin
{$IFDEF FPC}
  if CaretVisible then
    HideEditorCaret;
  try
{$ENDIF}
    PaintContent(ACanvas, ClientRect, ContentLeft, ContentTop);
{$IFDEF FPC}
  finally
    if CaretVisible then
      ShowEditorCaret;
  end;
{$ENDIF}
end;

procedure TKCustomMemo.ParaStyleChanged(Sender: TObject);
begin
  FBlocks.NotifyDefaultParaChange;
end;

function TKCustomMemo.PointToBlockPoint(const APoint: TPoint; ACalcActive: Boolean): TPoint;
begin
  Result.X := APoint.X - ContentLeft;
  Result.Y := APoint.Y - ContentTop;
  if (FActiveBlocks <> FBlocks) and ACalcActive then
  begin
    OffsetPoint(Result, -FActiveBlocks.TotalLeftOffset, -FActiveBlocks.TotalTopOffset);
  end;
end;

function TKCustomMemo.PointToIndex(APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer;
begin
  Result := FActiveBlocks.PointToIndex(Canvas, PointToBlockPoint(APoint), AOutOfArea, ASelectionExpanding, ALinePos);
end;

procedure TKCustomMemo.PrintPaintBegin;
begin
end;

procedure TKCustomMemo.PrintPaintEnd;
begin
  FBlocks.MeasureExtent(Canvas, RequiredContentWidth)
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

procedure TKCustomMemo.SaveToRTF(const AFileName: TKString; ASelectedOnly: Boolean);
var
  Writer: TKMemoRTFWriter;
begin
  Writer := TKMemoRTFWriter.Create(Self);
  try
    Writer.SaveToFile(AFileName, ASelectedOnly);
  finally
    Writer.Free;
  end;
end;

procedure TKCustomMemo.SaveToRTFStream(AStream: TStream;
  ASelectedOnly: Boolean);
var
  Writer: TKMemoRTFWriter;
begin
  Writer := TKMemoRTFWriter.Create(Self);
  try
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
  ScrollBy((R.Left - ClientWidth div 2) div FHorzScrollStep, (R.Top - ClientHeight div 2) div FVertScrollStep, True);
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
  FActiveBlocks.Select(ASelStart, ASelLength, ADoScroll);
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
  FLinePosition := APosition;
  Select(ASelStart, 0, ADoScroll);
end;

procedure TKCustomMemo.SelectionInit(const APoint: TPoint; ADoScroll: Boolean);
var
  NewSelEnd: Integer;
begin
  FActiveBlocks := FBlocks.PointToBlocks(Canvas, PointToBlockPoint(APoint, False));
  if FActiveBlocks = nil then
    FActiveBlocks := FBlocks;
  NewSelEnd := PointToIndex(APoint, True, False, FLinePosition);
  Select(NewSelEnd, 0, ADoScroll);
  UpdatePreferredCaretPos;
end;

procedure TKCustomMemo.SetColors(Value: TKMemoColors);
begin
  FColors.Assign(Value);
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
    ScrollBy(Value - FLeftPos, 0, True);
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

procedure TKCustomMemo.SetOptions(const Value: TKEditOptions);
var
  UpdateShowFormatting: Boolean;
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
    FOptions := Value;
    if UpdateShowFormatting then
      DoUpdate([muExtent]);
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
  FActiveBlocks.SelectionParaStyle := Value;
  Modified := True;
end;

procedure TKCustomMemo.SetSelectionTextStyle(const Value: TKMemoTextStyle);
begin
  FActiveBlocks.SelectionTextStyle := Value;
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
  FActiveBlocks.LockUpdate;
  try
    FActiveBlocks.Clear;
    FActiveBlocks.Text := Value;
  finally
    FActiveBlocks.UnlockUpdate;
  end;
end;

procedure TKCustomMemo.SetTopPos(Value: Integer);
begin
  Value := MinMax(Value, 0, FVertScrollExtent - 1);
  if Value <> FTopPos then
    ScrollBy(0, Value - FTopPos, True);
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
    DoUpdate([muExtent]);
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
  Items := FActiveBlocks.IndexToBlocks(AIndex, LocalIndex);
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
        FActiveBlocks.FixEOL(SelEnd, True, FLinePosition);
      FCaretRect := IndexToRect(SelEnd, True);
      Dec(FCaretRect.Right, FCaretRect.Left); // Right is width
      Dec(FCaretRect.Bottom, FCaretRect.Top); // Bottom is height

      if AShow then
      begin
        if Enabled and Focused and not (csDesigning in ComponentState) and (SelLength = 0) then
        begin
          if not (elOverwrite in FStates) then
            FCaretRect.Right := MinMax(FCaretRect.Bottom div 10, 2, 4);
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

procedure TKCustomMemo.UpdatePreferredCaretPos;
begin
  FPreferredCaretPos := FCaretRect.Left - ContentLeft;
  if FActiveBlocks <> FBlocks then
    Dec(FPreferredCaretPos, FActiveBlocks.TotalLeftOffset);
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
      if Show then
      asm
        nop; // debug line
      end;
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
      if Show then
      asm
        nop; // debug line
      end;
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
  Scroll(Msg.ScrollCode, cScrollNoAction, 0, 0, True);
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
  Scroll(cScrollNoAction, Msg.ScrollCode, 0, 0, True);
end;

{ TKMemoBlock }

constructor TKMemoBlock.Create(AParent: TKMemoBlocks);
begin
  inherited Create;
  FOffset := CreateEmptyPoint;
  FParent := nil;
  Parent := AParent; // update default block properties!
end;

function TKMemoBlock.EqualProperties(AItem: TKMemoBlock): Boolean;
begin
  Result := False;
end;

procedure TKMemoBlock.Assign(AItem: TKMemoBlock);
begin
  Parent := AItem.Parent;
  Select(AItem.SelStart, AItem.SelLength);
  AssignAttributes(AItem);
end;

procedure TKMemoBlock.AssignAttributes(AItem: TKMemoBlock);
begin
end;

function TKMemoBlock.CalcBaseLine(ACanvas: TCanvas): Integer;
begin
  Result := 0;
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
  if FParent <> nil then
    Result := FParent.DefaultParaStyle
  else
    Result := nil;
end;

function TKMemoBlock.GetDefaultTextStyle: TKMemoTextStyle;
begin
  if FParent <> nil then
    Result := FParent.DefaultTextStyle
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
  if FParent <> nil then
    Result := FParent.MemoNotifier
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

procedure TKMemoBlock.GetSelColors(out Foreground, Background: TColor);
begin
  Foreground := cSelTextFocusedDef;
  Background := cSelBkGndFocusedDef;
  if FParent <> nil then
    FParent.GetSelColors(Foreground, Background);
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
  if FParent <> nil then
    Result := FParent.ShowFormatting
  else
    Result := False;
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
  if FParent <> nil then
  begin
    ParentIndex := FParent.IndexOf(Self);
    NewItem := Split(AIndex);
    if NewItem <> nil then
    begin
      FParent.AddAt(NewItem, ParentIndex + 1);
      FParent.AddParagraph(ParentIndex + 1);
    end else
    begin
      if AIndex = 0 then
        FParent.AddParagraph(ParentIndex)
      else
        FParent.AddParagraph(ParentIndex + 1);
    end;
    Result := True;
  end;
end;

function TKMemoBlock.InsertString(const AText: TKString; At: Integer): Boolean;
begin
  Result := False;
end;

function TKMemoBlock.InternalLeftOffset: Integer;
begin
  if FPosition <> mbpText then
    Result := FOffset.X
  else
    Result := 0;
end;

function TKMemoBlock.InternalTopOffset: Integer;
begin
  if FPosition <> mbpText then
    Result := FOffset.Y
  else
    Result := 0;
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

procedure TKMemoBlock.PaintToCanvas(ACanvas: TCanvas; ALeft, ATop: Integer);
var
  I: Integer;
begin
  for I := 0 to WordCount - 1 do
    WordPaintToCanvas(ACanvas, I, ALeft, ATop);
end;

procedure TKMemoBlock.ParentChanged;
begin
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

function TKMemoBlock.Select(ASelStart, ASelLength: Integer): Boolean;
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
  Select(0, SelectableLength);
end;

procedure TKMemoBlock.SetLeftOffset(const Value: Integer);
begin
  if Value <> FOffset.X then
  begin
    FOffset.X := Value;
    Update([muExtent]);
  end;
end;

procedure TKMemoBlock.SetParent(AParent: TKMemoBlocks);
begin
  if FParent <> AParent then
  begin
    FParent := AParent;
    ParentChanged;
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

procedure TKMemoBlock.SetTopOffset(const Value: Integer);
begin
  if Value <> FOffset.Y then
  begin
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

function TKMemoBlock.Split(At: Integer; AllowEmpty: Boolean): TKMemoBlock;
begin
  Result := nil;
end;

procedure TKMemoBlock.Update(AReasons: TKMemoUpdateReasons);
begin
  if FParent <> nil then
    FParent.Update(AReasons);
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

function TKMemoBlock.WordMouseAction(AWordIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean;
begin
  Result := False;
end;

procedure TKMemoBlock.WordPaintToCanvas(ACanvas: TCanvas; AWordIndex, ALeft, ATop: Integer);
begin
end;

{ TKMemoSingleBlock }

constructor TKMemoSingleton.Create(AParent: TKMemoBlocks);
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

function TKMemoSingleton.Select(ASelStart, ASelLength: Integer): Boolean;
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
    Update([muSelection]);
    Result := True;
  end else
    Result := False;
end;

{ TKTextMemoBlock }

constructor TKMemoTextBlock.Create(AParent: TKMemoBlocks);
begin
  FTextStyle := TKMemoTextStyle.Create;
  FTextStyle.OnChanged := TextStyleChanged;
  inherited;
  FText := '';
  FTextLength := 0;
  FWords := TKMemoWordList.Create;
end;

destructor TKMemoTextBlock.Destroy;
begin
  FTextStyle.Free;
  FWords.Free;
  inherited;
end;

function TKMemoTextBlock.EqualProperties(AItem: TKMemoBlock): Boolean;
begin
  if AItem is TKMemoTextBlock then
  begin
    Result :=
      (TKMemoTextBlock(AItem).Text = Text) and
      TKMemoTextBlock(AItem).TextStyle.EqualProperties(TextStyle);
  end else
    Result := False;
end;

function TKMemoTextBlock.ApplyFormatting(const AText: TKString): TKString;
begin
  if GetShowFormatting then
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
    if Printing then
      Font.Height := FTextStyle.Font.Height; // because of correct printing
    if FTextStyle.AllowBrush then
      Brush.Assign(FTextStyle.Brush)
    else
    begin
      Brush.Style := bsClear;
      Font.Style := Font.Style - [fsUnderLine];
    end;
  end;
end;

procedure TKMemoTextBlock.Assign(AItem: TKMemoBlock);
begin
  inherited;
  if AItem is TKMemoTextBlock then
    Text := TKMemoTextBlock(AItem).Text;
end;

procedure TKMemoTextBlock.AssignAttributes(AItem: TKMemoBlock);
begin
  inherited;
  if AItem is TKMemoTextBlock then
    TextStyle.Assign(TKMemoTextBlock(AItem).TextStyle);
end;

function TKMemoTextBlock.CalcBaseLine(ACanvas: TCanvas): Integer;
begin
  ApplyTextStyle(ACanvas);
  Result := GetFontAscent(ACanvas.Handle);
end;

function TKMemoTextBlock.CalcDescent(ACanvas: TCanvas): Integer;
begin
  ApplyTextStyle(ACanvas);
  Result := GetFontDescent(ACanvas.Handle);
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

function TKMemoTextBlock.GetWordCount: Integer;
begin
  Result := FWords.Count;
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
  for I := 0 to FWords.Count - 1 do
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
{$IFDEF FPC}
var
  I: Integer;
{$ENDIF}
begin
  AIndex := MinMax(AIndex, 0, ContentLength);
{$IFDEF FPC}
  Result := 1;
  for I := 0 to AIndex - 1 do
    Inc(Result, UTF8CharacterLength(@AText[Result]));
{$ELSE}
  Result := AIndex + 1;
{$ENDIF}
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

function TKMemoTextBlock.ModifiedTextExtent(ACanvas: TCanvas; const AText: TKString): TPoint;
var
  Size: TSize;
  C, CU, SU: TKString;
  I, SmallFontHeight, X, Y: Integer;
begin
  if Pos(#9, AText) <> 0 then
  begin
    SU := UnicodeStringReplace(AText, #9, TabChar, [rfReplaceAll]);
    Size := TextExtentDepOnKerning(ACanvas, SU);
    Result := Point(Size.cx, Size.cy);
  end
  else if FTextStyle.Capitals = tcaNone then
  begin
    Size := TextExtentDepOnKerning(ACanvas, AText);
    Result := Point(Size.cx, Size.cy);
  end else
  begin
    SU := UnicodeUpperCase(AText);
    if FTextStyle.Capitals = tcaNormal then
    begin
      Size := TextExtentDepOnKerning(ACanvas, SU);
      Result := Point(Size.cx, Size.cy);
    end else
    begin
      SmallFontHeight := MulDiv(FTextStyle.Font.Height, 4, 5);
      X := 0; Y := 0;
      for I := 1 to StringLength(SU) do
      begin
        C := StringCopy(AText, I, 1);
        CU := StringCopy(SU, I, 1);
        if C <> CU then
          ACanvas.Font.Height := SmallFontheight
        else
          ACanvas.Font.Height := FTextStyle.Font.Height;
        Size := TextExtentDepOnKerning(ACanvas, CU);
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

function TKMemoTextBlock.Split(At: Integer; AllowEmpty: Boolean): TKMemoBlock;
var
  Item: TKMemoTextBlock;
  S, Part1, Part2: TKString;
  Cls: TKMemoBlockClass;
begin
  if ((At > 0) or AllowEmpty and (At = 0)) and (At < ContentLength) then
  begin
    Cls := TKMemoBlockClass(Self.ClassType);
    Item := Cls.Create(FParent) as TKMemoTextBlock;
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

function TKMemoTextBlock.TextExtentDepOnKerning(ACanvas: TCanvas; const AText: TKString): TSize;
{$IFDEF KMEMO_DISABLE_KERNING}
var
  CharIndex, NewCharIndex: Integer;
  Size: TSize;
{$ENDIF}
begin
{$IFDEF KMEMO_DISABLE_KERNING}
  // character by character
  Result.cx := 0;
  Result.cy := 0;
  CharIndex := 1;
  while CharIndex <= Length(AText) do
  begin
    NewCharIndex := StrNextCharIndex(AText, CharIndex);
    Size := TKTextBox.TextExtent(ACanvas, AText, CharIndex, NewCharIndex - CharIndex);
    Inc(Result.cx, Size.cx);
    Result.cy := Max(Result.cy, Size.cy);
    CharIndex := NewCharIndex;
  end;
{$ELSE}
  // system default
  Result := TKTextBox.TextExtent(ACanvas, AText, 1, Length(AText));
{$ENDIF}
end;

function TKMemoTextBlock.TextIndexToIndex(var AText: TKString; ATextIndex: Integer): Integer;
{$IFDEF FPC}
var
  I: Integer;
{$ENDIF}
begin
  if ATextIndex >= 0 then
  begin
  {$IFDEF FPC}
    Result := 0;
    I := 1;
    while I < ATextIndex do
    begin
      Inc(I, UTF8CharacterLength(@AText[I]));
      Inc(Result);
    end;
  {$ELSE}
    Result := ATextIndex - 1;
  {$ENDIF}
  end else
    Result := -1;
end;

procedure TKMemoTextBlock.TextOutputDepOnKerning(ACanvas: TCanvas; ALeft, ATop: Integer; const AText: TKString);
{$IFDEF KMEMO_DISABLE_KERNING}
var
  CharIndex, NewCharIndex: Integer;
  Size: TSize;
{$ENDIF}
begin
{$IFDEF KMEMO_DISABLE_KERNING}
  // character by character
  CharIndex := 1;
  while CharIndex <= Length(AText) do
  begin
    NewCharIndex := StrNextCharIndex(AText, CharIndex);
    TKTextBox.TextOutput(ACanvas, ALeft, ATop, AText, CharIndex, NewCharIndex - CharIndex);
    Size := TKTextBox.TextExtent(ACanvas, AText, CharIndex, NewCharIndex - CharIndex);
    Inc(ALeft, Size.cx);
    CharIndex := NewCharIndex;
  end;
{$ELSE}
  // system default
  TKTextBox.TextOutput(ACanvas, ALeft, ATop, AText, 1, Length(AText));
{$ENDIF}
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
    Word := TKMemoWord.Create;
    Word.StartIndex := AStart - 1;
    Word.EndIndex := AEnd - 1;
    FWords.Add(Word);
  end;

var
  Index, PrevIndex, CharIndex: Integer;
  WasBreak, IsTab, WasTab: Boolean;
begin
  FWords.Clear;
  if FText <> '' then
  begin
    CharIndex := 1;
    Index := 1;
    PrevIndex := 1;
    IsTab := False;
    WasBreak := False;
    while Index <= FTextLength do
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
      Inc(Index);
      CharIndex := StrNextCharIndex(FText, CharIndex);
    end;
    if Index > PrevIndex then
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
      BaseLine := CalcBaseLine(ACanvas);
      Y := Word.Position.Y + Word.TopPadding + Word.BaseLine - BaseLine;
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
  S := ApplyFormatting(Words[AIndex]);
  with ACanvas do
  begin
    ApplyTextStyle(ACanvas);
    FWords[AIndex].Extent := ModifiedTextExtent(ACanvas, S);
    Result := FWords[AIndex].Extent;
  end;
end;

procedure TKMemoTextBlock.WordPaintToCanvas(ACanvas: TCanvas;
  AWordIndex: Integer; ALeft, ATop: Integer);

  function AdjustBaseLine(ABaseLine: Integer): Integer;
  begin
    Dec(ABaseline, GetFontAscent(ACanvas.Handle));
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
      if (Pos(#9, AText) <> 0) and ShowFormatting then
      begin
        SU := UnicodeStringReplace(AText, #9, TabChar, [rfReplaceAll]);
        TextOutputDepOnKerning(ACanvas, ARect.Left, AdjBaseLine, SU);
      end
      else if FTextStyle.Capitals = tcaNone then
      begin
        TextOutputDepOnKerning(ACanvas, ARect.Left, AdjBaseLine, AText);
      end else
      begin
        SU := UnicodeUpperCase(AText);
        if FTextStyle.Capitals = tcaNormal then
          TextOutputDepOnKerning(ACanvas, ARect.Left, AdjBaseLine, SU)
        else
        begin
          SmallFontHeight := MulDiv(FTextStyle.Font.Height, 4, 5);
          X := ARect.Left;
          for I := 1 to StringLength(SU) do
          begin
            C := StringCopy(AText, I, 1);
            CU := StringCopy(SU, I, 1);
            if C <> CU then
              Font.Height := SmallFontHeight
            else
              Font.Height := FTextStyle.Font.Height;
            AdjBaseLine := AdjustBaseLine(ABaseLine);
            TextOutputDepOnKerning(ACanvas, X, AdjBaseLine, CU);
            Size := TextExtentDepOnKerning(ACanvas, CU);
            Inc(X, Size.cx);
          end;
        end;
      end;
    end;
  end;

var
  W, X, Y, BaseLine: Integer;
  S, T, Part1, Part2, Part3: TKString;
  R: TRect;
  Word: TKMemoWord;
  Color, Bkgnd: TColor;
begin
  with ACanvas do
  begin
    ApplyTextStyle(ACanvas);
    S := ApplyFormatting(Words[AWordIndex]);
    Word := FWords[AWordIndex];
    X := Word.Position.X + ALeft + InternalLeftOffset;
    Y := Word.Position.Y + ATop + InternalTopOffset;
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

constructor TKMemoHyperlink.Create(AParent: TKMemoBlocks);
begin
  inherited;
  FURL := '';
  DefaultStyle;
end;

procedure TKMemoHyperlink.Assign(AItem: TKMemoBlock);
begin
  inherited;
  if AItem is TKMemoHyperlink then
    FURL := TKMemoHyperlink(AItem).URL;
end;

procedure TKMemoHyperlink.DefaultStyle;
begin
  FTextStyle.Font.Color := clBlue;
  FTextStyle.Font.Style := FTextStyle.Font.Style + [fsUnderline];
end;

function TKMemoHyperlink.WordMouseAction(AWordIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean;
var
  R: TRect;
  Word: TKMemoWord;
  Notifier: IKMemoNotifier;
begin
  Result := False;
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
          if ssCtrl in AShift then
            Notifier.SetReqMouseCursor(crHandPoint)
          else
            Notifier.SetReqMouseCursor(crIBeam);
          Result := True;
        end;
        maLeftDown:
        begin
          if (ssCtrl in AShift) and (FURL <> '') then
          begin
            OpenURLWithShell(FURL);
            Result := True;
          end;
        end;
      end;
    end;
  end;
end;

{ TKParagraph }

constructor TKMemoParagraph.Create(AParent: TKMemoBlocks);
begin
  inherited;
  FExtent := CreateEmptyPoint;
  FTextStyle.AllowBrush := False;
  FParaStyle := TKMemoParaStyle.Create;
  FParaStyle.OnChanged := ParaStyleChanged;
  FPosition := CreateEmptyPoint;
  Text := NewLineChar;
end;

destructor TKMemoParagraph.Destroy;
begin
  FParaStyle.Free;
  inherited;
end;

procedure TKMemoParagraph.AssignAttributes(AItem: TKMemoBlock);
begin
  inherited;
  if AItem is TKMemoParagraph then
    FParaStyle.Assign(TKMemoParagraph(AItem).ParaStyle);
end;

procedure TKMemoParagraph.ParaStyleChanged(Sender: TObject);
begin
  Update([muExtent]);
end;

function TKMemoParagraph.Concat(AItem: TKMemoBlock): Boolean;
begin
  Result := False;
end;

function TKMemoParagraph.GetCanAddText: Boolean;
begin
  Result := False;
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

function TKMemoParagraph.Split(At: Integer; AllowEmpty: Boolean): TKMemoBlock;
begin
  Result := nil;
end;

{ TKImageMemoBlock }

constructor TKMemoImageBlock.Create(AParent: TKMemoBlocks);
begin
  inherited;
  FBaseLine := 0;
  FWordBottomPadding := 0;
  FCrop := TKRect.Create;
  FCalcBaseLine := 0;
  FExtent := CreateEmptyPoint;
  FImage := TPicture.Create;
  FImageStyle := TKMemoBlockStyle.Create;
  FImageStyle.ContentMargin.AssignFromValues(5, 5, 5, 5);
  FImageStyle.OnChanged := ImageStyleChanged;
  FOriginalExtent := CreateEmptyPoint;
  FPosition := CreateEmptyPoint;
  FScale := Point(100, 100);
  FScaledImage := nil;
  FWordTopPadding := 0;
end;

destructor TKMemoImageBlock.Destroy;
begin
  FCrop.Free;
  FImageStyle.Free;
  FImage.Free;
  FScaledImage.Free;
  inherited;
end;

procedure TKMemoImageBlock.Assign(AItem: TKMemoBlock);
begin
  inherited;
  if AItem is TKMemoImageBlock then
  begin
    FCrop.Assign(TKMemoImageBlock(AItem).Crop);
    FImage.Assign(TKMemoImageBlock(AItem).Image);
    ScaleHeight := TKMemoImageBlock(AItem).ScaleHeight;
    ScaleWidth := TKMemoImageBlock(AItem).ScaleWidth;
  end;
end;

function TKMemoImageBlock.CalcBaseLine(ACanvas: TCanvas): Integer;
var
  Item: TKMemoBlock;
  Index, Ascent, Descent: Integer;
begin
  if (FParent <> nil) and (Position = mbpText) then
  begin
    Result := FExtent.Y div 2;
    Index := FParent.IndexOf(Self);
    if Index >= 0 then
    begin
      Item := FParent.GetLastItemByClass(Index, TKMemoTextBlock);
      if Item = nil then
        Item := FParent.GetNextItemByClass(Index, TKMemoTextBlock);
      if Item <> nil then
      begin
        Ascent := TKMemoTextBlock(Item).CalcBaseLine(ACanvas);
        Descent := TKMemoTextBlock(Item).CalcDescent(ACanvas);
        Result := (FExtent.Y - (Ascent + Descent)) div 2 + Ascent;
      end else
      begin
        Item := FParent.GetNearestParagraph(Index);
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

function TKMemoImageBlock.GetOriginalHeight: Integer;
begin
  if FOriginalExtent.Y <> 0 then
    Result := FOriginalExtent.Y
  else
    Result := FImage.Height;
end;

function TKMemoImageBlock.GetOriginalWidth: Integer;
begin
  if FOriginalExtent.X <> 0 then
    Result := FOriginalExtent.X
  else
    Result := FImage.Width;
end;

function TKMemoImageBlock.GetScaleHeight: Integer;
begin
  Result := MulDiv(OriginalHeight, FScale.Y, 100);
end;

function TKMemoImageBlock.GetScaleWidth: Integer;
begin
  Result := MulDiv(OriginalWidth, FScale.X, 100);
end;

function TKMemoImageBlock.GetWordBottomPadding(Index: Integer): Integer;
begin
  Result := FWordBottomPadding;
end;

function TKMemoImageBlock.GetWordBoundsRect(Index: Integer): TRect;
begin
  Result.TopLeft := CreateEmptyPoint;
  Result.BottomRight := FExtent;
  KFunctions.OffsetRect(Result, FPosition);
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
  Result := FPosition.X;
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
  Result := FPosition.Y;
end;

function TKMemoImageBlock.GetWordTopPadding(Index: Integer): Integer;
begin
  Result := FWordTopPadding;
end;

function TKMemoImageBlock.GetWordWidth(Index: Integer): Integer;
begin
  Result := FExtent.X;
end;

procedure TKMemoImageBlock.ImageStyleChanged(Sender: TObject);
begin
  Update([muExtent]);
end;

function TKMemoImageBlock.OuterRect(ACaret: Boolean): TRect;
begin
  Result.TopLeft := FPosition;
  Result.Right := Result.Left + FExtent.X;
  Result.Bottom := Result.Top + FExtent.Y;
  if ACaret then
  begin
    Inc(Result.Top, FWordTopPadding);
    Dec(Result.Bottom, FWordBottomPadding);
  end;
  OffsetRect(Result, InternalLeftOffset, InternalTopOffset);
end;

function TKMemoImageBlock.ScaledImage: TKAlphaBitmap;
var
  BM: TKAlphaBitmap;
  ExtentX, ExtentY, NewExtentX: Integer;
  RatioX, RatioY: Double;
  OrigCrop: TRect;
begin
  if (FScaledImage = nil) and (FImage.Graphic <> nil) then
  begin
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
    if (ExtentX * ExtentY <> 0) and (FImage.Width * FImage.Height <> 0) then
    begin
      FScaledImage := TKAlphaBitmap.Create;
      FScaledImage.DirectCopy := True;
      FScaledImage.SetSize(ExtentX, ExtentY);
      FScaledImage.Fill(MakeColorRec(255,255,255,255));
    {$IFDEF FPC}
      FScaledImage.UpdateHandle;
    {$ENDIF}
      SetStretchBltMode(FScaledImage.Canvas.Handle, HALFTONE);
      // we must scale bitmap to bitmap to use HALFTONE effect
      BM := TKAlphaBitmap.Create;
      try
        BM.SetSize(FImage.Width - OrigCrop.Left - OrigCrop.Right, FImage.Height - OrigCrop.Top - OrigCrop.Bottom);
        BM.Fill(MakeColorRec(255,255,255,255));
        BM.DrawFrom(FImage.Graphic, -OrigCrop.Left, -OrigCrop.Right);
        BM.DrawTo(FScaledImage.Canvas, Rect(0, 0, FScaledImage.Width, FScaledImage.Height));
      finally
        BM.Free;
      end;
    {$IFDEF FPC}
      FScaledImage.UpdatePixels;
    {$ENDIF}
    end;
  end;
  Result := FScaledImage;
end;

procedure TKMemoImageBlock.SetCrop(const Value: TKRect);
begin
  FCrop.Assign(Value);
end;

procedure TKMemoImageBlock.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
  FreeAndNil(FScaledImage);
  Update([muContent]);
end;

procedure TKMemoImageBlock.SetImagePath(const Value: TKString);
begin
  FImage.LoadFromFile(Value);
  FreeAndNil(FScaledImage);
  Update([muContent]);
end;

procedure TKMemoImageBlock.SetOriginalHeight(const Value: Integer);
begin
  FOriginalExtent.Y := Value;
end;

procedure TKMemoImageBlock.SetOriginalWidth(const Value: Integer);
begin
  FOriginalExtent.X := Value;
end;

procedure TKMemoImageBlock.SetScaleHeight(const Value: Integer);
begin
  if Value <> ScaleHeight then
  begin
    FScale.Y := MulDiv(Value, 100, OriginalHeight);
    FreeAndNil(FScaledImage);
    Update([muExtent]);
  end;
end;

procedure TKMemoImageBlock.SetScaleWidth(const Value: Integer);
begin
  if Value <> ScaleWidth then
  begin
    FScale.X := MulDiv(Value, 100, OriginalWidth);
    FreeAndNil(FScaledImage);
    Update([muExtent]);
  end;
end;

procedure TKMemoImageBlock.SetScaleX(const Value: Integer);
begin
  if Value <> FScale.X then
  begin
    FScale.X := Value;
    FreeAndNil(FScaledImage);
    Update([muExtent]);
  end;
end;

procedure TKMemoImageBlock.SetScaleY(const Value: Integer);
begin
  if Value <> FScale.Y then
  begin
    FScale.Y := Value;
    FreeAndNil(FScaledImage);
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
  FPosition.X := Value;
end;

procedure TKMemoImageBlock.SetWordTop(Index: Integer; const Value: Integer);
begin
  FPosition.Y := Value;
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
  FreeAndNil(FScaledImage);
  Result := Point(
    ImageWidth + FImageStyle.LeftPadding + FImageStyle.RightPadding + FImageStyle.LeftMargin + FImageStyle.RightMargin,
    ImageHeight + FImageStyle.TopPadding + FImageStyle.BottomPadding + FImageStyle.TopMargin + FImageStyle.BottomMargin);
  if (Position = mbpText) and (Result.X > ARequiredWidth) then
  begin
    // when image is placed in text it should be adjusted to page width
    Result.Y := MulDiv(Result.Y, ARequiredWidth, Result.X);
    Result.X := ARequiredWidth;
  end;
  FExtent := Result;
end;

procedure TKMemoImageBlock.WordPaintToCanvas(ACanvas: TCanvas; AIndex, ALeft, ATop: Integer);
var
  X, Y: Integer;
  ROuter: TRect;
  Bitmap: TKAlphaBitmap;
  Color, Bkgnd: TColor;
begin
  inherited;
  ROuter := OuterRect(False);
  OffsetRect(ROuter, ALeft, ATop);
  X := ROuter.Left + FImageStyle.LeftPadding + FImageStyle.LeftMargin;
  Y := ROuter.Top + FImageStyle.TopPadding + FImageStyle.TopMargin + FWordTopPadding + FBaseLine - FCalcBaseLine;
  if PaintSelection and (SelLength > 0) then
  begin
    GetSelColors(Color, BkGnd);
    ACanvas.Brush.Color := BkGnd;
    if Position <> mbpText then
      ROuter := ImageStyle.MarginRect(ROuter);
    ACanvas.FillRect(ROuter);
    if ScaledImage <> nil then
    begin
      Bitmap := TKAlphaBitmap.Create;
      try
        Bitmap.SetSize(ScaledImage.Width, ScaledImage.Height);
      {$IFDEF FPC}
        Bitmap.UpdateHandle;
      {$ENDIF}
        Bitmap.Canvas.Brush.Color := BkGnd;
        Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
        Bitmap.Canvas.Draw(0, 0, ScaledImage);
      {$IFDEF FPC}
        Bitmap.UpdatePixels;
      {$ENDIF}
        Bitmap.AlphaFillPercent(50, True);
        Bitmap.AlphaDrawTo(ACanvas, X, Y);
      finally
        Bitmap.Free;
      end;
    end;
  end else
  begin
    ROuter := ImageStyle.MarginRect(ROuter);
    FImageStyle.PaintBox(ACanvas, ROuter);
    if ScaledImage <> nil then
      ACanvas.Draw(X, Y, ScaledImage);
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

constructor TKMemoContainer.Create(AParent: TKMemoBlocks);
begin
  inherited;
  FBlocks := TKMemoBlocks.Create(Self);
  FBlocks.MemoNotifier := GetMemoNotifier;
  FBlocks.OnUpdate := Update;
  FBlockStyle := TKMemoBlockStyle.Create;
  FBlockStyle.OnChanged := BlockStyleChanged;
  FWordBottomPadding := 0;
  FClip := False;
  FCurrentRequiredHeight := 0;
  FCurrentRequiredWidth := 0;
  FFixedHeight := False;
  FFixedWidth := False;
  FPosition := CreateEmptyPoint;
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

procedure TKMemoContainer.BlockStyleChanged(Sender: TObject);
begin
  Update([muExtent]);
end;

function TKMemoContainer.CalcBaseLine(ACanvas: TCanvas): Integer;
var
  PA: TKMemoParagraph;
  ParaDescent: Integer;
begin
  Result := 0;
  if (FParent <> nil) and (Position = mbpText) then
  begin
    PA := FParent.GetNearestParagraph(FParent.IndexOf(Self));
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

function TKMemoContainer.GetWordBottomPadding(Index: Integer): Integer;
begin
  Result := FWordBottomPadding;
end;

function TKMemoContainer.GetWordBoundsRect(Index: Integer): TRect;
begin
  Result := Rect(0, 0, Width, Height);
  KFunctions.OffsetRect(Result, FPosition);
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
  Result := FPosition.X;
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
  Result := FPosition.Y;
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

procedure TKMemoContainer.RequiredHeightChanged;
begin
end;

procedure TKMemoContainer.RequiredWidthChanged;
begin
end;

function TKMemoContainer.Select(ASelStart, ASelLength: Integer): Boolean;
begin
  Result := FBlocks.Select(ASelStart, ASelLength);
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
  FPosition.X := Value;
end;

procedure TKMemoContainer.SetWordTop(Index: Integer; const Value: Integer);
begin
  FPosition.Y := Value;
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
  KFunctions.OffsetRect(Result,
    Left + InternalLeftOffset + FBlockStyle.AllPaddingsLeft,
    Top + InternalTopOffset + FBlockStyle.AllPaddingsTop + FWordTopPadding);
end;

function TKMemoContainer.WordMeasureExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint;
begin
  if FFixedWidth then
    FCurrentRequiredWidth := FRequiredWidth
  else
    FCurrentRequiredWidth := ARequiredWidth;
  FCurrentRequiredHeight := 0;
  if not ((Self is TKMemoTable) or (Self is TKMemoTableRow) or (Self is TKMemotableCell)) then
  asm
    nop; // debug line
  end;
  FBlocks.MeasureExtent(ACanvas, Max(FCurrentRequiredWidth - FBlockStyle.AllPaddingsLeft - FBlockStyle.AllPaddingsRight, 0));
  Result := Point(Width, Height);
end;

function TKMemoContainer.WordMouseAction(AIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean;
var
  P: TPoint;
  R: TRect;
begin
  Result := False;
  P := APoint;
  R := Rect(0, 0, Width, Height);
  OffsetPoint(P, -Left - InternalLeftOffset, -Top - InternalTopOffset - FWordTopPadding);
  if PtInRect(R, P) then
  begin
    OffsetPoint(P, -FBlockStyle.AllPaddingsLeft, -FBlockStyle.AllPaddingsTop);
    Result := FBlocks.MouseAction(AAction, P, AShift);
  end;
end;

procedure TKMemoContainer.WordPaintToCanvas(ACanvas: TCanvas; AIndex, ALeft, ATop: Integer);
var
  R, ClipRect: TRect;
  MainClipRgn: HRGN;
  SaveIndex: Integer;
begin
  R := Rect(0, 0, Width, Height);
  OffsetRect(R, Left + ALeft + InternalLeftOffset, Top + ATop + InternalTopOffset + FWordTopPadding);
  R := FBlockStyle.MarginRect(R);
  FBlockStyle.PaintBox(ACanvas, R);
  R := FBlockStyle.BorderRect(R);
  Inc(ALeft, Left + FBlockStyle.AllPaddingsLeft + InternalLeftOffset);
  Inc(ATop, Top + FBlockStyle.AllPaddingsTop + InternalTopOffset + FWordTopPadding);
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
  OffsetPoint(P, -Left - InternalLeftOffset, -Top - InternalTopOffset - FWordTopPadding);
  if PtInRect(R, P) or (AOutOfArea and (P.X >= R.Left) and (P.X < R.Right)) then
  begin
    OffsetPoint(P, -FBlockStyle.AllPaddingsLeft, -FBlockStyle.AllPaddingsTop);
    Result := FBlocks.PointToIndex(ACanvas, P, AOutOfArea, ASelectionExpanding, APosition);
  end else
    Result := -1;
end;

{ TKMemoTableCell }

constructor TKMemoTableCell.Create(AParent: TKMemoBlocks);
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
    Result := Parent.Parent as TKMemoTableRow
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
  OffsetPoint(P, -Left - InternalLeftOffset, -Top - InternalTopOffset - FWordTopPadding);
  if PtInRect(R, P) or
    AFirstRow and (P.X >= R.Left) and (P.X < R.Right) and (P.Y < R.Bottom) or
    ALastRow and (P.X >= R.Left) and (P.X < R.Right) and (P.Y >= R.Top) then
  begin
    OffsetPoint(P, -FBlockStyle.AllPaddingsLeft, -FBlockStyle.AllPaddingsTop);
    Result := FBlocks.PointToIndex(ACanvas, P, AOutOfArea, ASelectionExpanding, APosition);
  end else
    Result := -1;
end;

procedure TKMemoTableCell.ParaStyleChanged(Sender: TObject);
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

constructor TKMemoTableRow.Create(AParent: TKMemoBlocks);
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
    Result := Parent.Parent as TKMemoTable
  else
    Result := nil;
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
        Blocks.Add(TKMemoTableCell.Create(Blocks));
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

constructor TKMemoTable.Create(AParent: TKMemoBlocks);
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
        Cell.BlockStyle.Assign(FCellStyle);
        Cell.BlockStyle.BorderWidth := 0;
        Cell.BlockStyle.BorderWidths.AssignFromValues(0, 0, 0, 0);
        Cell.RequiredBorderWidths.AssignFromValues(W, W, W, W);
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
    if Cell.ColSpan > 1 then
    asm
      nop; // debug line
    end;
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
              TmpCell := TKMemoTableCell.Create(Row.Blocks);
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

procedure TKMemoTable.LockUpdate;
begin
  Inc(FUpdateLock);
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
        Row := TKMemoTableRow.Create(FBlocks);
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

procedure TKMemoTable.UnlockUpdate;
begin
  if FUpdateLock > 0 then
  begin
    Dec(FUpdateLock);
    if FUpdateLock = 0 then
      Update([muContent]);
  end;
end;

function TKMemoTable.UpdateUnlocked: Boolean;
begin
  Result := FUpdateLock <= 0;
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
        if OverflowSpace > DistSpace then
        asm
          nop // debug line
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
    if Extent.X > ARequiredWidth then
    asm
      nop // debug line
    end;
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

function TKMemoTable.WordMouseAction(AIndex: Integer; AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean;
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
  OffsetPoint(P, -Left - InternalLeftOffset, -Top - InternalTopOffset - FWordTopPadding);
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
          Result := Result or Cell.WordMouseAction(0, AAction, P, AShift);
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
  OffsetPoint(P, -Left - InternalLeftOffset, -Top - InternalTopOffset - FWordTopPadding);
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

constructor TKMemoBlocks.Create(AParent: TKMemoBlock);
begin
  inherited Create;
  OwnsObjects := True;
  FIgnoreParaMark := False;
  FLines := TKMemoLines.Create;
  FRelPos := TKMemoSparseList.Create;
  FExtent := CreateEmptyPoint;
  FMemoNotifier := nil;
  FParent := AParent;
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
    Result := TKMemoContainer.Create(Self);
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
  Result := TKMemoHyperLink.Create(Self);
  Result.TextStyle.Assign(LastTextStyle(At));
  Result.DefaultStyle;
  Result.Text := AText;
  Result.URL := AURL;
  AddAt(Result, At);
end;

function TKMemoBlocks.AddImageBlock(const APath: TKString; At: Integer): TKMemoImageBlock;
begin
  Result := TKMemoImageBlock.Create(Self);
  Result.SetImagePath(APath);
  AddAt(Result, At);
end;

function TKMemoBlocks.AddParagraph(At: Integer): TKMemoParagraph;
var
  PA: TKMemoParagraph;
begin
  Result := TKMemoParagraph.Create(Self);
  PA := GetNearestParagraph(At);
  if PA <> nil then
    Result.AssignAttributes(PA)
  else
  begin
    Result.TextStyle.Assign(LastTextStyle(At));
    Result.ParaStyle.Assign(GetDefaultParaStyle);
  end;
  AddAt(Result, At);
end;

function TKMemoBlocks.AddTable(At: Integer): TKMemoTable;
begin
  Result := TKMemoTable.Create(Self);
  AddAt(Result, At);
end;

function TKMemoBlocks.AddTextBlock(const AText: TKString; At: Integer): TKMemoTextBlock;
begin
  Result := TKMemoTextBlock.Create(Self);
  Result.TextStyle.Assign(LastTextStyle(At));
  Result.Text := AText;
  AddAt(Result, At);
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

function TKMemoBlocks.GetItem(Index: Integer): TKMemoBlock;
begin
  Result := TKMemoBlock(inherited GetItem(Index));
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
    for I := FLines[ALineIndex].StartBlock to FLines[ALineIndex].EndBlock do if I < Count then
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
    Result := Rect(
      FLines[ALineIndex].Position.X,
      FLines[ALineIndex].Position.Y,
      FLines[ALineIndex].Position.X + FLines[ALineIndex].Extent.X,
      FLines[ALineIndex].Position.Y + FLines[ALineIndex].Extent.Y)
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
  if Result >= 0 then
    Inc(Result);
end;

function TKMemoBlocks.GetNearestParagraph(AIndex: Integer): TKMemoParagraph;
var
  Item: TKMemoBlock;
begin
  Result := nil;
  if AIndex >= 0 then
    while (Result = nil) and (AIndex < Count) do
    begin
      Item := Items[AIndex];
      if Item is TKMemoParagraph then
        Result := Item as TKMemoParagraph;
      Inc(AIndex);
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

function TKMemoBlocks.GetParentBlocks: TKMemoBlocks;
begin
  if FParent is TKMemoContainer then
    Result := TKMemoContainer(FParent).Parent
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
      Item := GetNearestParagraph(Block);
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
  Item: TKmemoBlock;
  LocalIndex: Integer;
begin
  Item := IndexToItem(RealSelEnd - 1, LocalIndex);
  if Item is TKMemoContainer then
    Result := TKMemoContainer(Item).Blocks.SelectionTextStyle
  else if Item is TKMemoTextBlock then
    Result := TKMemoTextBlock(Item).TextStyle
  else
    Result := nil;
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
      Inc(Result, TKMemoContainer(FParent).BlockStyle.AllPaddingsLeft + TKMemoContainer(FParent).Parent.TotalLeftOffset);
  end else
    Result := 0;
end;

function TKMemoBlocks.GetTotalTopOffset: Integer;
begin
  if FParent <> nil then
  begin
    Result := FParent.Top + FParent.TopOffset + FParent.WordTopPadding[0];
    if FParent is TKMemoContainer then
      Inc(Result, TKMemoContainer(FParent).BlockStyle.AllPaddingsTop + TKMemoContainer(FParent).Parent.TotalTopOffset);
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

function TKMemoBlocks.IndexToBlocks(AIndex: Integer; out ALocalIndex: Integer): TKMemoBlocks;
var
  Item: TKMemoBlock;
  LocalIndex: Integer;
begin
  Item := IndexToItem(AIndex, LocalIndex);
  if Item is TKMemoContainer then
    Result := TKMemoContainer(Item).Blocks.IndexToBlocks(LocalIndex, ALocalIndex)
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

procedure TKMemoBlocks.InsertChar(At: Integer; const AValue: TKChar; AOverWrite: Boolean);
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
  if InsertString(At, True, AValue) then
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

function TKMemoBlocks.InsertString(AIndex: Integer; AAdjust: Boolean; const AValue: TKString): Boolean;
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
      if LocalIndex = 0 then
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
    Result := TKMemoContainer(FParent).Parent.InsideOfTable
  else
    Result := False;
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
  Found: Boolean;
begin
  Result := CreateEmptyRect;
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
  begin
    Found := False;
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
          if (AIndex >= LastIndex) and (AIndex < CurIndex) then
          begin
            Result := Item.WordIndexToRect(ACanvas, J, AIndex - LastIndex, ACaret);
            Found := True;
          end;
          Inc(J);
        end;
      end;
      Inc(I);
    end;
  end;
end;

procedure TKMemoBlocks.LockUpdate;
begin
  if FUpdateLock <= 0 then
    FUpdateReasons := [];
  Inc(FUpdateLock);
end;

procedure TKMemoBlocks.MeasureExtent(ACanvas: TCanvas; ARequiredWidth: Integer);
var
  PosX, PosY, Right, CurBlock, CurIndex, CurWord, CurTotalWord, LineHeight, ParaWidth, ParaPosY, LastBlock, LastIndex, LastWord, LastTotalWord: Integer;
  CurParaStyle: TKMemoParaStyle;

  function GetParaStyle(ABlockIndex: Integer): TKMemoParaStyle;
  var
    Para: TKMemoParagraph;
  begin
    if (ABlockIndex >= 0) and (ABlockIndex < Count) then
      Result := Items[ABlockIndex].ParaStyle
    else
      Result := nil;
    if Result = nil then
    begin
      Para := GetNearestParagraph(ABlockIndex);
      if Para <> nil then
        Result := Para.ParaStyle
      else
        Result := DefaultParaStyle;
    end;
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
      if (Item.Position = mbpAbsolute) or (CurBlock > FRelPos[I].Index) then
      begin
        ACollisionRect := Item.BoundsRect;
        OffsetRect(ACollisionRect, Item.LeftOffset, Item.TopOffset);
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

  procedure MoveWordsOnLine(ALineIndex, AStartPos, AEndPos, ADelta: Integer);
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
            Item.WordLeft[J] := item.WordLeft[J] + ADelta;
        end;
      end;
    end;
  end;

  function AddLine: Boolean;
  var
    Line, LastLine: TKMemoLine;
    EndItem, Item: TKMemoBlock;
    I, J, W, CurWordCopy, CurBlockCopy, CurIndexCopy, Delta, FirstIndent, LineIndex, LineLeft, LineRight, BaseLine, StPosX, St, En, ParaMarkWidth, BottomPadding, TopPadding: Integer;
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
        TopPadding := CurParaStyle.TopPadding
      end else
      begin
        FirstIndent := 0;
        TopPadding := 0;
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
          BaseLine := Max(BaseLine, Item.CalcBaseLine(ACanvas));
      end;
      // adjust line and paragraph heights
      Inc(LineHeight, TopPadding + BottomPadding);
      // adjust all words horizontally
      if CurParaStyle.HAlign in [halCenter, halRight] then
      begin
        // reposition all line chunks like MS Word does it
        PosX := CurParaStyle.LeftPadding + FirstIndent;
        StPosX := PosX;
        W := 0;
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
                  MoveWordsOnLine(LineIndex, StPosX, R.Left, Delta);
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
        MoveWordsOnLine(LineIndex, StPosX, Right + ParaMarkWidth, Delta);
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
      // adjust paragraph extent
      if LineRight > ARequiredWidth then
        Dec(LineRight, ParaMarkWidth);
      ParaWidth := Max(ParaWidth, LineRight - LineLeft);
      if EndItem is TKMemoParagraph then
      begin
        TKMemoParagraph(EndItem).Top := ParaPosY;
        TKmemoParagraph(EndItem).Width := ParaWidth;
        TKmemoParagraph(EndItem).Height := PosY + LineHeight - ParaPosY - CurParaStyle.BottomPadding;
        CurParaStyle := GetParaStyle(CurBlock);
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
      while not IsBreakable and (ACurBlock < Count) do
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
  WLen, PrevPosX, PrevPosY: Integer;
  IsBreakable, IsParagraph, OutSide, WasBreakable, WasParagraph: Boolean;
  Item: TKMemoBlock;
  NextParaStyle: TKMemoParaStyle;
begin
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
  CurParaStyle := GetParaStyle(0);
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
    Item := Items[CurBlock];
    case Item.Position of
      mbpText:
      begin
        CurWord := 0;
        while CurWord < Item.WordCount do
        begin
          WasParagraph := IsParagraph;
          IsParagraph := (Item is TKMemoParagraph) and (CurWord = Item.WordCount - 1);
          WLen := Item.WordLength[CurWord];
          if WasParagraph then
            NextParaStyle := GetParaStyle(CurBlock)
          else
            NextParaStyle := CurParaStyle;
          WasBreakable := IsBreakable or not (Item is TKMemoTextBlock);
          IsBreakable := Item.WordBreakable[CurWord];
          Extent := MeasureNextWords(ACanvas, CurBlock, CurWord, ARequiredWidth - NextParaStyle.LeftPadding - NextParaStyle.RightPadding - NextParaStyle.FirstIndent, IsBreakable, NBExtent);
          OutSide := CurParaStyle.WordWrap and not IsParagraph and WasBreakable and (PosX + NBExtent.X > Right);
          if OutSide or WasParagraph then
            AddLine;
          MoveWordToFreeSpace(Extent.X, Extent.Y);
          Item.WordLeft[CurWord] := PosX;
          Item.WordTop[CurWord] := PosY;
          Inc(PosX, Extent.X);
          LineHeight := Max(LineHeight, Extent.Y);
          Inc(CurIndex, WLen);
          Inc(CurWord);
          Inc(CurTotalWord);
        end;
      end;
      mbpRelative:
      begin
        if IsParagraph then
        begin
          AddLine;
          IsParagraph := False;
        end;
        // position relative block correctly
        PrevPosX := PosX; PrevPosY := PosY;
        try
          // starting position for relative object is currently always: X by column (currently always 0), Y by paragraph
          // the object position offsets (LeftOffset, TopOffset) are always counted from this default position
          PosX := 0;
          PosY := ParaPosY;
          MoveWordToFreeSpace(Item.Width, Item.Height);
          Item.WordLeft[0] := PosX;
          Item.WordTop[0] := PosY;
          FExtent.X := Max(FExtent.X, PosX + Item.Width + Item.LeftOffset);
          FExtent.Y := Max(FExtent.Y, PosY + Item.Height + Item.TopOffset);
        finally
          PosX := PrevPosX; PosY := PrevPosY;
        end;
      end;
    end;
    Inc(CurBlock);
  end;
  if CurIndex > LastIndex then
    AddLine;
end;

function TKMemoBlocks.MouseAction(AAction: TKMemoMouseAction; const APoint: TPoint; AShift: TShiftState): Boolean;
var
  I, J, K, St, En: Integer;
  Item: TKmemoBlock;
begin
  Result := False;
  for I := 0 to LineCount - 1 do
  begin
    if (LineTop[I] <= APoint.Y) and (APoint.Y < LineBottom[I]) then
    begin
      for J := FLines[I].StartBlock to FLines[I].EndBlock do
      begin
        Item := Items[J];
        if Item.Position = mbpText then
        begin
          GetWordIndexes(J, I, St, En);
          for K := St to En do
            Result := Result or Item.WordMouseAction(K, AAction, APoint, AShift);
        end;
      end;
    end;
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
    if Action = lnAdded then
      TKMemoBlock(Ptr).Parent := Self;
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

procedure TKMemoBlocks.PaintToCanvas(ACanvas: TCanvas; ALeft, ATop: Integer; const ARect: TRect);
var
  I, J, K, St, En: Integer;
  R: TRect;
  Item: TKMemoBlock;
  PA: TKMemoParagraph;
begin
  // paint text blocks
  for I := 0 to LineCount - 1 do
  begin
    if (LineBottom[I] + ATop >= ARect.Top) and (LineTop[I] + ATop < ARect.Bottom) then
    begin
      // fill areas under paragraphs
      if LineFloat[I] then
      begin
        PA := GetNearestParagraph(FLines[I].StartBlock);
        if (PA <> nil) and ((PA.ParaStyle.Brush.Style <> bsClear) or (PA.ParaStyle.BorderWidth > 0) or PA.ParaStyle.BorderWidths.NonZero) then
        begin
          R := Rect(0, 0, Max(FRequiredWidth, PA.Width), PA.Height);
          OffsetRect(R, PA.Left, PA.Top);
          R.Top := Max(R.Top, LineTop[I]);
          R.Bottom := Min(R.Bottom, LineBottom[I]);
          OffsetRect(R, ALeft, ATop);
          PA.ParaStyle.PaintBox(ACanvas, R);
        end;
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
    end;
  end;
  // paint relative or absolute blocks
  for I := 0 to FRelPos.Count - 1 do
  begin
    Item := Items[FRelPos[I].Index];
    R := Item.BoundsRect;
    OffsetRect(R, Item.LeftOffset + ALeft, Item.TopOffset + ATop);
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

function TKMemoBlocks.PointToBlocks(ACanvas: TCanvas; const APoint: TPoint): TKMemoBlocks;
var
  I: Integer;
  Item: TKMemoBlock;
  R: TRect;
  P: TPoint;
begin
  Result := nil;
  if FRelPos <> nil then
  begin
    for I := 0 to FRelPos.Count - 1 do
    begin
      Item := Items[FRelPos[I].Index];
      if Item is TKMemoContainer then
      begin
        R := Item.BoundsRect;
        OffsetRect(R, Item.LeftOffset, Item.TopOffset);
        if PtInRect(R, APoint) then
        begin
          Result := TKMemoContainer(Item).Blocks;
          Break;
        end;
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
        Result := TKMemoContainer(Item).Blocks.PointToBlocks(ACanvas, P);
        if Result <> nil then
          break;
      end;
    end;
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
        LastIndex := CurIndex;
        Inc(CurIndex, Item.SelectableLength);
        if (ASelStart >= LastIndex) and (NewSelEnd < CurIndex) then
        begin
          // selection within the same block
          Item.Select(ASelStart - LastIndex, NewSelEnd - ASelStart);
        end
        else if (ASelStart >= LastIndex) and (ASelStart < CurIndex) and (NewSelEnd >= CurIndex) then
          // selection starts in this block
          Item.Select(ASelStart - LastIndex, CurIndex - ASelStart)
        else if (ASelStart < LastIndex) and (NewSelEnd >= LastIndex) and (NewSelEnd < CurIndex) then
          // selection ends in this block
          Item.Select(0, NewSelEnd - LastIndex)
        else if (ASelStart <= LastIndex) and (NewSelEnd >= CurIndex) then
        begin
          // selection goes through this block
          if not ATextOnly and ((ASelLength <> 0) and (Item.Position <> mbpText)) then
            LocalSelLength := Item.SelectableLength(True)
          else
            LocalSelLength := CurIndex - LastIndex;
          Item.Select(0, LocalSelLength)
        end else
          Item.Select(-1, 0);
      end;
      if ADoScroll then
      begin
        Exclude(FUpdateReasons, muSelection);
        Include(FUpdateReasons, muSelectionScroll)
      end else
        Include(FUpdateReasons, muSelection);
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

procedure TKMemoBlocks.UnlockUpdate;
begin
  if FUpdateLock > 0 then
  begin
    Dec(FUpdateLock);
    if (FUpdateLock = 0) and (FUpdateReasons <> []) then
      Update(FUpdateReasons);
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
var
  I: Integer;
  Item: TKMemoBlock;
begin
  if FRelPos <> nil then
  begin
    Inc(FUpdateLock);
    try
      FRelPos.Clear;
      FSelectableLength := 0;
      for I := 0 to Count - 1 do
      begin
        Item := Items[I];
        if Item.Position = mbpText then
        begin
          if Item is TKMemoParagraph then
            Item.AssignAttributes(GetLastItemByClass(I, TKMemoTextBlock));
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
  end;
end;

function TKMemoBlocks.UpdateUnlocked: Boolean;
begin
  Result := FUpdateLock <= 0;
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

