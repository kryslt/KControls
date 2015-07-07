{ @abstract(This unit contains native replacement for TMemo/TRichEdit components)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(28 Apr 2009)
  @lastmod(6 Jul 2014)

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
  SysUtils, Classes, Graphics, Controls, Contnrs,
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
    procedure NotifyChange(AValue: TKMemoTextStyle); virtual;
    property AllowBrush: Boolean read FAllowBrush write SetAllowBrush;
    property Capitals: TKMemoTextCapitals read FCapitals write SetCapitals;
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property StyleChanged: Boolean read FStyleChanged write FStyleChanged;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TKMemoBlockStyle = class(TPersistent)
  private
    FBrush: TBrush;
    FBorderRadius: Integer;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FCancelFloat: Boolean;
    FContentPadding: TKRect;
    FOnChanged: TNotifyEvent;
    FBorderWidths: TKRect;
    function GetBottomPadding: Integer;
    function GetLeftPadding: Integer;
    function GetRightPadding: Integer;
    function GetTopPadding: Integer;
    procedure SetBottomPadding(const Value: Integer);
    procedure SetBorderRadius(const Value: Integer);
    procedure SetContentPadding(const Value: TKRect);
    procedure SetLeftPadding(const Value: Integer);
    procedure SetRightPadding(const Value: Integer);
    procedure SetTopPadding(const Value: Integer);
    procedure SetBrush(const Value: TBrush);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetBorderWidths(const Value: TKRect);
    procedure SetCancelFloat(const Value: Boolean);
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
    procedure NotifyChange(AValue: TKMemoBlockStyle); virtual;
    procedure PaintBox(ACanvas: TCanvas; const ARect: TRect); virtual;
    property BottomPadding: Integer read GetBottomPadding write SetBottomPadding;
    property BorderRadius: Integer read FBorderRadius write SetBorderRadius;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property BorderWidths: TKRect read FBorderWidths write SetBorderWidths;
    property Brush: TBrush read FBrush write SetBrush;
    property CancelFloat: Boolean read FCancelFloat write SetCancelFloat;
    property ContentPadding: TKRect read FContentPadding write SetContentPadding;
    property LeftPadding: Integer read GetLeftPadding write SetLeftPadding;
    property RightPadding: Integer read GetRightPadding write SetRightPadding;
    property TopPadding: Integer read GetTopPadding write SetTopPadding;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TKMemoParaStyle = class(TKMemoBlockStyle)
  private
    FFirstIndent: Integer;
    FHAlign: TKHAlign;
    FWordWrap: Boolean;
    FLineWrap: Boolean;
    procedure SetFirstIndent(const Value: Integer);
    procedure SetHAlign(const Value: TKHAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetLineWrap(const Value: Boolean);
  public
    constructor Create; override;
    procedure Assign(ASource: TPersistent); override;
    procedure Defaults; override;
    property FirstIndent: Integer read FFirstIndent write SetFirstIndent;
    property HAlign: TKHAlign read FHAlign write SetHAlign;
    property LineWrap: Boolean read FLineWrap write SetLineWrap;
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

  TKMemoBlocks = class;

  IKMemoNotifier = interface(IInterface)
    function GetDefaultTextStyle: TKMemoTextStyle;
    function GetDefaultParaStyle: TKMemoParaStyle;
    function GetLinePosition: TKMemoLinePosition;
    procedure GetSelColors(out Foreground, Background: TColor);
    function GetShowFormatting: Boolean;
  end;

  TKMemoBlock = class(TObject)
  private
    FOffset: TPoint;
    FParent: TKMemoBlocks;
    FPosition: TKMemoBlockPosition;
    function GetBoundsRect: TRect;
    function GetMemoNotifier: IKMemoNotifier;
    procedure SetParent(AParent: TKMemoBlocks);
    procedure SetPosition(const Value: TKMemoBlockPosition);
  protected
    function ContentLength: Integer; virtual;
    function GetBottomPadding: Integer; virtual;
    function GetCanAddText: Boolean; virtual;
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
    procedure BeforeDestruction; override;
    function CalcBaseLine(ACanvas: TCanvas): Integer; virtual;
    function CanAdd(AItem: TKMemoBlock): Boolean; virtual;
    procedure ClearSelection(ATextOnly: Boolean); virtual;
    function Concat(AItem: TKMemoBlock): Boolean; virtual;
    function IndexToRect(ACanvas: TCanvas; AIndex: Integer; ACaret: Boolean): TRect; virtual;
    function InsertParagraph(AIndex: Integer): Boolean; virtual;
    function InsertString(const AText: TKString; At: Integer = -1): Boolean; virtual;
    function MeasureExtent(ACanvas: TCanvas; ARequiredWidth: Integer): TPoint; virtual;
    function MeasureWordExtent(ACanvas: TCanvas; AWordIndex, ARequiredWidth: Integer): TPoint; virtual;
    procedure NotifyDefaultTextChange; virtual;
    procedure NotifyDefaultParaChange; virtual;
    procedure PaintToCanvas(ACanvas: TCanvas; ALeft, ATop: Integer); virtual;
    function PointToIndex(ACanvas: TCanvas; const APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; virtual;
    function Select(ASelStart, ASelLength: Integer): Boolean; virtual;
    function SelectableLength(ALocalCalc: Boolean = False): Integer; virtual;
    function Split(At: Integer): TKMemoBlock; virtual;
    function WordIndexToRect(ACanvas: TCanvas; AWordIndex: Integer; AIndex: Integer; ACaret: Boolean): TRect; virtual;
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
    property ParaStyle: TKMemoParaStyle read GetParaStyle;
    property Parent: TKMemoBlocks read FParent write SetParent;
    property Position: TKMemoBlockPosition read FPosition write SetPosition;
    property SelLength: Integer read GetSelLength;
    property SelStart: Integer read GetSelStart;
    property SelText: TKString read GetSelText;
    property Text: TKString read GetText;
    property Top: Integer read GetTop;
    property TopOffset: Integer read FOffset.Y write SetTopOffset;
    property TopPadding: Integer read GetTopPadding;
    property Width: Integer read GetWidth;
    property WordCount: Integer read GetWordCount;
    property WordBaseLine[Index: Integer]: Integer read GetWordBaseLine write SetWordBaseLine;
    property WordBottomPadding[Index: Integer]: Integer read GetWordBottomPadding write SetWordBottomPadding;
    property WordBoundsRect[Index: Integer]: TRect read GetWordBoundsRect;
    property WordHeight[Index: Integer]: Integer read GetWordHeight write SetWordHeight;
    property WordLeft[Index: Integer]: Integer read GetWordLeft write SetWordLeft;
    property WordLength[Index: Integer]: Integer read GetWordLength;
    property Words[Index: Integer]: TKString read GetWords;
    property WordTop[Index: Integer]: Integer read GetWordTop write SetWordTop;
    property WordTopPadding[Index: Integer]: Integer read GetWordTopPadding write SetWordTopPadding;
    property WordWidth[Index: Integer]: Integer read GetWordWidth write SetWordWidth;
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
    FWords: TKMemoWordList;
  protected
    { Because of time optimization. }
    FTextLength: Integer;
    function ApplyFormatting(const AText: TKString): TKString;
    procedure ApplyTextStyle(ACanvas: TCanvas); virtual;
    function ContentLength: Integer; override;
    class function CopyText(const ASource: TKString; At, Count: Integer): TKString;
    class procedure DeleteText(var ASource: TKString; At, Count: Integer);
    function GetCanAddText: Boolean; override;
    function GetSelText: TKString; override;
    function GetText: TKString; override;
    function GetWordBaseLine(Index: Integer): Integer; override;
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
    function TextIndexToIndex(var AText: TKString; ATextIndex: Integer): Integer; virtual;
    class function TextLength(const ASource: TKString): Integer; virtual;
    procedure TextStyleChanged(Sender: TObject);
    procedure UpdateWords; virtual;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    procedure Assign(AItem: TKMemoBlock); override;
    procedure AssignAttributes(AItem: TKMemoBlock); override;
    function CalcBaseLine(ACanvas: TCanvas): Integer; override;
    procedure ClearSelection(ATextOnly: Boolean); override;
    function Concat(AItem: TKMemoBlock): Boolean; override;
    function InsertString(const AText: TKString; At: Integer = -1): Boolean; override;
    function MeasureWordExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint; override;
    procedure NotifyDefaultTextChange; override;
    function Split(At: Integer): TKMemoBlock; override;
    function WordIndexToRect(ACanvas: TCanvas; AWordIndex: Integer; AIndex: Integer; ACaret: Boolean): TRect; override;
    procedure WordPaintToCanvas(ACanvas: TCanvas; AWordIndex, ALeft, ATop: Integer); override;
    function WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint; AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; override;
    property Text: TKString read GetText write SetText;
    property TextStyle: TKMemoTextStyle read FTextStyle;
  end;

  TKMemoParagraph = class(TKMemoTextBlock)
  private
    FExtent: TPoint;
    FPosition: TPoint;
    FParaStyle: TKMemoParaStyle;
  protected
    function GetCanAddText: Boolean; override;
    function GetParaStyle: TKMemoParaStyle; override;
    procedure ParaStyleChanged(Sender: TObject);
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    procedure AssignAttributes(AItem: TKMemoBlock); override;
    function Concat(AItem: TKMemoBlock): Boolean; override;
    procedure NotifyDefaultParaChange; override;
    function Split(At: Integer): TKMemoBlock; override;
    property Height: Integer read FExtent.Y write FExtent.Y;
    property Left: Integer read FPosition.X write FPosition.X;
    property Top: Integer read FPosition.Y write FPosition.Y;
    property ParaStyle: TKMemoParaStyle read GetParaStyle;
    property Width: Integer read FExtent.X write FExtent.X;
  end;

  TKMemoImageBlock = class(TKMemoSingleton)
  private
    FBottomPadding: Integer;
    FImage: TPicture;
    FImageStyle: TKMemoBlockStyle;
    FExtent: TPoint;
    FPosition: TPoint;
    FScaleExtent: TPoint;
    FTopPadding: Integer;
    procedure SetImage(const Value: TPicture);
    procedure SetImagePath(const Value: TKString);
    procedure SetScaleHeight(const Value: Integer);
    procedure SetScaleWidth(const Value: Integer);
    function GetImageHeight: Integer;
    function GetImageWidth: Integer;
  protected
    function ContentLength: Integer; override;
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
    procedure SetWordBottomPadding(Index: Integer; const Value: Integer); override;
    procedure SetWordHeight(Index: Integer; const Value: Integer); override;
    procedure SetWordLeft(Index: Integer; const Value: Integer); override;
    procedure SetWordTop(Index: Integer; const Value: Integer); override;
    procedure SetWordTopPadding(Index: Integer; const Value: Integer); override;
    procedure UpdateExtent;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    procedure Assign(AItem: TKMemoBlock); override;
    function MeasureWordExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint; override;
    function OuterRect(ACaret: Boolean): TRect; virtual;
    function WordIndexToRect(ACanvas: TCanvas; AWordIndex: Integer; AIndex: Integer; ACaret: Boolean): TRect; override;
    function WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint; AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; override;
    procedure WordPaintToCanvas(ACanvas: TCanvas; AIndex, ALeft, ATop: Integer); override;
    property Image: TPicture read FImage write SetImage;
    property ImageStyle: TKMemoBlockStyle read FImageStyle;
    property ImageHeight: Integer read GetImageHeight;
    property ImageWidth: Integer read GetImageWidth;
    property Path: TKString write SetImagePath;
    property ScaleHeight: Integer read FScaleExtent.Y write SetScaleHeight;
    property ScaleWidth: Integer read FScaleExtent.X write SetScaleWidth;
  end;

  TKMemoContainer = class(TKMemoBlock)
  private
    FBlocks: TKMemoBlocks;
    FBlockStyle: TKMemoBlockStyle;
    FFixedWidth: Boolean;
    FPosition: TPoint;
    FClip: Boolean;
    FCurrentRequiredWidth: Integer;
    FCurrentRequiredHeight: Integer;
    procedure SetFixedWidth(const Value: Boolean);
    procedure SetRequiredWidth(const Value: Integer);
    procedure SetRequiredHeight(const Value: Integer);
    procedure SetClip(const Value: Boolean);
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
    function GetCanAddText: Boolean; override;
    function GetSelLength: Integer; override;
    function GetSelStart: Integer; override;
    function GetTopPadding: Integer; override;
    function GetWordBoundsRect(Index: Integer): TRect; override;
    function GetWordCount: Integer; override;
    function GetWordHeight(Index: Integer): Integer; override;
    function GetWordLeft(Index: Integer): Integer; override;
    function GetWordLength(Index: Integer): Integer; override;
    function GetWords(Index: Integer): TKString; override;
    function GetWordTop(Index: Integer): Integer; override;
    function GetWordWidth(Index: Integer): Integer; override;
    procedure RequiredHeightChanged; virtual;
    procedure RequiredWidthChanged; virtual;
    procedure SetWordHeight(Index: Integer; const Value: Integer); override;
    procedure SetWordLeft(Index: Integer; const Value: Integer); override;
    procedure SetWordTop(Index: Integer; const Value: Integer); override;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    function CanAdd(AItem: TKMemoBlock): Boolean; override;
    procedure ClearSelection(ATextOnly: Boolean); override;
    function InsertParagraph(AIndex: Integer): Boolean; override;
    function InsertString(const AText: TKString; At: Integer = -1): Boolean; override;
    function MeasureWordExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint; override;
    procedure NotifyDefaultParaChange; override;
    procedure NotifyDefaultTextChange; override;
    function Select(ASelStart, ASelLength: Integer): Boolean; override;
    procedure SetBlockExtent(AWidth, AHeight: Integer); virtual;
    function WordIndexToRect(ACanvas: TCanvas; AWordIndex: Integer; AIndex: Integer; ACaret: Boolean): TRect; override;
    function WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint; AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer; override;
    procedure WordPaintToCanvas(ACanvas: TCanvas; AIndex, ALeft, ATop: Integer); override;
    property Blocks: TKMemoBlocks read FBlocks;
    property BlockStyle: TKMemoBlockStyle read FBlockStyle;
    property Clip: Boolean read FClip write SetClip;
    property CurrentRequiredHeight: Integer read FCurrentRequiredHeight;
    property CurrentRequiredWidth: Integer read FCurrentRequiredWidth;
    property FixedWidth: Boolean read FFixedWidth write SetFixedWidth;
    property RequiredHeight: Integer read FRequiredHeight write SetRequiredHeight;
    property RequiredWidth: Integer read FRequiredWidth write SetRequiredWidth;
  end;

  TKMemoTableRow = class;

  TKMemoTableCell = class(TKMemoContainer)
  private
    function GetParentRow: TKMemoTableRow;
  protected
    FParaStyle: TKMemoParaStyle;
    function GetParaStyle: TKMemoParaStyle; override;
    procedure RequiredWidthChanged; override;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    property ParentRow: TKMemoTableRow read GetParentRow;
  end;

  TKMemoTable = class;

  TKMemoTableRow = class(TKMemoContainer)
  private
    function GetCells(Index: Integer): TKMemoTableCell;
    function GetCellCount: Integer;
    procedure SetCellCount(const Value: Integer);
    function GetParentTable: TKMemoTable;
  protected
    FParaStyle: TKMemoParaStyle;
    function GetParaStyle: TKMemoParaStyle; override;
    procedure RequiredHeightChanged; override;
    procedure RequiredWidthChanged; override;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    procedure ApplyDefaultCellStyle; virtual;
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
    function GetColWidths(Index: Integer): Integer;
    function GetRowHeights(Index: Integer): Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetColWidths(Index: Integer; const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    procedure SetRowHeights(Index: Integer; const Value: Integer);
    function GetRows(Index: Integer): TKMemoTableRow;
    function GetRowCount: Integer;
  protected
    procedure RequiredWidthChanged; override;
    procedure SetSize(AColCount, ARowCount: Integer); virtual;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    procedure ApplyDefaultCellStyle; virtual;
    function CanAdd(AItem: TKMemoBlock): Boolean; override;
    function MeasureWordExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint; override;
    property CellStyle: TKMemoBlockStyle read FCellStyle;
    property ColCount: Integer read FColCount write SetColCount;
    property ColWidths[Index: Integer]: Integer read GetColWidths write SetColWidths;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property RowHeights[Index: Integer]: Integer read GetRowHeights write SetRowHeights;
    property Rows[Index: Integer]: TKMemoTableRow read GetRows;
  end;

  TKMemoBlockClass = class of TKMemoBlock;

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
    function GetSelText: TKString; virtual;
    function GetText: TKString; virtual;
    procedure GetWordIndexes(const ABlockIndex, ALineIndex: Integer; out AStart, AEnd: Integer); virtual;
    function LineToRect(ACanvas: TCanvas; AIndex, ALineIndex: Integer; ACaret: Boolean): TRect; virtual;
    function NormalToEOL(var AIndex: Integer): Boolean; virtual;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean = True; ATextOnly: Boolean = False): Boolean; virtual;
    procedure SetLineText(ALineIndex: Integer; const AValue: TKString); virtual;
    procedure SetText(const AValue: TKString);
    procedure Update(AReasons: TKMemoUpdateReasons); virtual;
    procedure UpdateAttributes; virtual;
  public
    constructor Create(AParent: TKMemoBlock); virtual;
    destructor Destroy; override;
    function AddAt(AObject: TKMemoBlock; At: Integer = -1): Integer; virtual;
    function AddContainer(At: Integer = -1): TKMemoContainer;
    function AddImageBlock(APath: TKString; At: Integer = -1): TKMemoImageBlock;
    function AddParagraph(At: Integer = -1): TKMemoParagraph;
    function AddTable(At: Integer = -1): TKMemoTable;
    function AddTextBlock(AText: TKString; At: Integer = -1): TKMemoTextBlock;
    procedure ClearSelection(ATextOnly: Boolean = True); virtual;
    procedure DeleteBOL(At: Integer); virtual;
    procedure DeleteChar(At: Integer); virtual;
    procedure DeleteEOL(At: Integer); virtual;
    procedure DeleteLastChar(At: Integer); virtual;
    procedure DeleteLine(At: Integer); virtual;
    procedure FixEOL(AIndex: Integer; AAdjust: Boolean; var ALinePos: TKMemoLinePosition); virtual;
    function GetNearestParagraph(AIndex: Integer): TKMemoParagraph; virtual;
    function GetShowFormatting: Boolean; virtual;
    function GetLastItemByClass(AIndex: Integer; AClass: TKMemoBlockClass): TKMemoBlock; virtual;
    function GetNextItemByClass(AIndex: Integer; AClass: TKMemoBlockClass): TKMemoBlock; virtual;
    procedure GetSelColors(out TextColor, Background: TColor); virtual;
    function IndexAboveLastLine(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function IndexAtBeginningOfContainer(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function IndexAtEndOfContainer(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function IndexBelowFirstLine(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function IndexToBlock(AIndex: Integer; out ALocalIndex: Integer): Integer; virtual;
    function IndexToItem(AIndex: Integer; out ALocalIndex: Integer): TKMemoBlock; virtual;
    function IndexToLine(AIndex: Integer): Integer; virtual;
    function IndexToRect(ACanvas: TCanvas; AIndex: Integer; ACaret, AAdjust: Boolean): TRect; virtual;
    procedure InsertChar(At: Integer; const AValue: TKChar; AOverWrite: Boolean); virtual;
    procedure InsertNewLine(At: Integer); virtual;
    function InsertParagraph(AIndex: Integer; AAdjust: Boolean): Boolean; virtual;
    function InsertString(AIndex: Integer; AAdjust: Boolean; const AValue: TKString): Boolean; virtual;
    function LineEndIndexByIndex(AIndex: Integer; AAdjust, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function LineStartIndexByIndex(AIndex: Integer; AAdjust: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    procedure LockUpdate;
    procedure MeasureExtent(ACanvas: TCanvas; ARequiredWidth: Integer); virtual;
    procedure NotifyDefaultParaChange; virtual;
    procedure NotifyDefaultTextChange; virtual;
    function NextIndexByCharCount(AIndex, ACharCount: Integer): Integer;
    function NextIndexByHorzExtent(ACanvas: TCanvas; AIndex, AWidth: Integer; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function NextIndexByRowDelta(ACanvas: TCanvas; AIndex, ARowDelta, ALeftPos: Integer; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function NextIndexByVertExtent(ACanvas: TCanvas; AIndex, AHeight, ALeftPos: Integer; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function NextIndexByVertValue(ACanvas: TCanvas; AValue, ALeftPos: Integer; ADirection: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    procedure PaintToCanvas(ACanvas: TCanvas; ALeft, ATop: Integer; const ARect: TRect); virtual;
    function PointToIndex(ACanvas: TCanvas; const APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    function PointToIndexOnLine(ACanvas: TCanvas; ALineIndex: Integer; const APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    procedure SetExtent(AWidth, AHeight: Integer); virtual;
    procedure UnlockUpdate;
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
    property RealSelEnd: Integer read GetRealSelEnd;
    property RealSelStart: Integer read GetRealSelStart;
    property SelectableLength: Integer read FSelectableLength;
    property SelEnd: Integer read FSelEnd;
    property SelLength: Integer read GetSelLength;
    property SelStart: Integer read FSelStart;
    property SelText: TKString read GetSelText;
    property Text: TKString read GetText write SetText;
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
    FHorzExtent: Integer;
    FHorzScrollStep: Integer;
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
    FVertExtent: Integer;
    FVertScrollStep: Integer;
    FOnChange: TNotifyEvent;
    FOnDropFiles: TKEditDropFilesEvent;
    FOnReplaceText: TKEditReplaceTextEvent;
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
    function GetSelectableLength: Integer;
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
    procedure SetSelEnd(Value: Integer);
    procedure SetSelLength(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetText(const Value: TKString);
    procedure SetTopPos(Value: Integer);
    procedure SetUndoLimit(Value: Integer);
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMSysColorChange(var Msg: TLMessage); message CM_SYSCOLORCHANGE;
  {$IFNDEF FPC}
    // no way to get filenames in Lazarus inside control (why??)
    procedure WMDropFiles(var Msg: TLMessage); message LM_DROPFILES;
  {$ENDIF}
    procedure WMEraseBkgnd(var Msg: TLMessage); message LM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TLMNoParams); message LM_GETDLGCODE;
    procedure WMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure WMKillFocus(var Msg: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TLMSetFocus); message LM_SETFOCUS;
    procedure WMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
  protected
    FCaretRect: TRect;
    FOldCaretRect: TRect;
    FLinePosition: TKMemoLinePosition;
    FPreferredCaretPos: Integer;
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
    function BlockRectToRect(const ARect: TRect): TRect; virtual;
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
    function DoSearchReplace(AReplace: Boolean): Boolean; virtual;
    { Performs the Undo command. }
    function DoUndo: Boolean; virtual;
    procedure DoUpdate(Reasons: TKMemoUpdateReasons);
    { Closes the undo group created by @link(TKCustomMemo.BeginUndoGroup). }
    procedure EndUndoGroup;
    { Notify blocks about memo font change. }
    procedure FontChange(Sender: TObject); virtual;
    function GetDefaultTextStyle: TKMemoTextStyle;
    function GetDefaultParaStyle: TKMemoParaStyle;
    function GetHorzScrollPadding: Integer; virtual;
    function GetLinePosition: TKMemoLinePosition;
    procedure GetSelColors(out Foreground, Background: TColor);
    function GetShowFormatting: Boolean;
    { Returns "real" selection end - with always higher index value than selection start value. }
    function GetRealSelEnd: Integer; virtual;
    { Returns "real" selection start - with always lower index value than selection end value. }
    function GetRealSelStart: Integer; virtual;
    function GetVertScrollPadding: Integer; virtual;
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
    { Overriden method - updates caret position/selection. }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - updates caret position/selection and initializes scrolling
      when needed. }
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - releases mouse capture acquired by MouseDown. }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - calls PaintLines to paint text lines into window client area. }
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    procedure ParaStyleChanged(Sender: TObject); virtual;
    function PointToBlockPoint(const APoint: TPoint): TPoint; virtual;
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
    function ScrollNeeded(out DeltaCol, DeltaRow: Integer): Boolean; virtual;
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
    { Updates mouse cursor according to the state determined from current mouse
      position. Returns True if cursor has been changed. }
    function SetMouseCursor(X, Y: Integer): Boolean; override;
    { Shows the caret. }
    procedure ShowEditorCaret; virtual;
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
    { Gives access to memo blocks - containers of texts, images etc.. }
    property Blocks: TKMemoBlocks read FBlocks;
    { Determines whether the caret is visible. }
    function CaretInView: Boolean;
    { Forces the caret position to become visible. }
    function ClampInView(CallScrollWindow: Boolean): Boolean;
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
    { Determines whether the given text buffer index is valid.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Value</I> - index to examine</LI>
      </UL> }
    function IndexValid(Value: Integer): Boolean; virtual;
    { Inserts a character at specified position.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>At</I> - position where the character should be inserted.</LI>
      <LI><I>AValue</I> - character</LI>
      </UL> }
    procedure InsertChar(At: Integer; AValue: TKChar); virtual;
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
    procedure LoadFromRTF(const AFileName: TKString); virtual;
    { Converts client area coordinates into a text buffer index.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>APoint</I> - window client area coordinates</LI>
      <LI><I>AOutOfArea</I> - set to True to compute selection even if the
      the supplied coordinates are outside of the text space</LI>
      </UL> }
    function PointToIndex(APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer; virtual;
    { Determines whether a selection is available. }
    function SelAvail: Boolean;
    { Specifies the current selection. This is faster than combination of SelStart and SelLength. }
    procedure Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean = True); virtual;
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
    property DisabledDrawStyle: TKEditDisabledDrawStyle read FDisabledDrawStyle write SetDisabledDrawStyle default cDisabledDrawStyleDef;
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
    { Returns selectable length. }
    property SelectableLength: Integer read GetSelectableLength;
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

function NewLineChar: TKString;
function SpaceChar: TKString;

implementation

uses
{$IFDEF USE_WINAPI}
  ShellApi,
{$ENDIF}
  ClipBrd, Printers,
{$IFDEF USE_THEMES}
  Themes,
{$ENDIF}
  Math, Types, KMemoRTF;

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
{$IFDEF FPC}
  Result := UnicodeToUTF8(Cardinal(cNewLineChar));
{$ELSE}
  Result := cNewLineChar;
{$ENDIF}
end;

function SpaceChar: TKString;
begin
{$IFDEF FPC}
  Result := UnicodeToUTF8(Cardinal(cSpaceChar));
{$ELSE}
  Result := cSpaceChar;
{$ENDIF}
end;

function TabChar: TKString;
begin
{$IFDEF FPC}
  Result := UnicodeToUTF8(Cardinal(cTabChar));
{$ELSE}
  Result := cTabChar;
{$ENDIF}
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
  FBrush := TBrush.Create;
  FBrush.OnChange := BrushChanged;
  FContentPadding := TKRect.Create;
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
  FCancelFloat := False;
  FContentPadding.AssignFromValues(0, 0, 0, 0);
  FChanged := False;
end;

destructor TKMemoBlockStyle.Destroy;
begin
  FBorderWidths.Free;
  FBrush.Free;
  FContentPadding.Free;
  inherited;
end;

procedure TKMemoBlockStyle.Assign(ASource: TPersistent);
begin
  if ASource is TKMemoBlockStyle then
  begin
    BorderColor := TKMemoBlockStyle(ASource).BorderColor;
    BorderRadius := TKMemoBlockStyle(ASource).BorderRadius;
    BorderWidth := TKMemoBlockStyle(ASource).BorderWidth;
    Brush.Assign(TKMemoBlockStyle(ASource).Brush);
    CancelFloat := TKMemoParaStyle(ASource).CancelFloat;
    ContentPadding.Assign(TKMemoBlockStyle(ASource).ContentPadding);
  end;
end;

function TKMemoBlockStyle.BorderRect(const ARect: TRect): TRect;
begin
  Result := ARect;
  if FBorderWidths.NonZero then
  begin
    if FBorderWidths.Left <> 0 then
      Inc(Result.Left, FBorderWidths.Left);
    if FBorderWidths.Top <> 0 then
      Inc(Result.Top, FBorderWidths.Top);
    if FBorderWidths.Right <> 0 then
      Dec(Result.Right, FBorderWidths.Right);
    if FBorderWidths.Bottom <> 0 then
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

function TKMemoBlockStyle.GetBottomPadding: Integer;
begin
  Result := FContentPadding.Bottom;
end;

function TKMemoBlockStyle.GetLeftPadding: Integer;
begin
  Result := FContentPadding.Left;
end;

function TKMemoBlockStyle.GetRightPadding: Integer;
begin
  Result := FContentPadding.Right;
end;

function TKMemoBlockStyle.GetTopPadding: Integer;
begin
  Result := FContentPadding.Top;
end;

function TKMemoBlockStyle.InteriorRect(const ARect: TRect): TRect;
begin
  Result := BorderRect(ARect);
  if FContentPadding.NonZero then
  begin
    if FContentPadding.Left <> 0 then
      Inc(Result.Left, FContentPadding.Left);
    if FContentPadding.Top <> 0 then
      Inc(Result.Top, FContentPadding.Top);
    if FContentPadding.Right <> 0 then
      Dec(Result.Right, FContentPadding.Right);
    if FContentPadding.Bottom <> 0 then
      Dec(Result.Bottom, FContentPadding.Bottom);
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
      DrawFilledRectangle(ACanvas, R, clNone);
    end else
    begin
      Brush.Assign(FBrush);
      if FBorderRadius > 0 then
        RoundRect(ARect, FBorderRadius, FBorderRadius)
      else if FBorderWidth > 0 then
        Rectangle(ARect)
      else
        DrawFilledRectangle(ACanvas, ARect, clNone);
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
  if not FBorderWidths.EqualProperties(Value) then
  begin
    FBorderWidths.Assign(Value);
    Changed;
  end;
end;

procedure TKMemoBlockStyle.SetBottomPadding(const Value: Integer);
begin
  if Value <> FContentPadding.Bottom then
  begin
    FContentPadding.Bottom := Value;
    Changed;
  end;
end;

procedure TKMemoBlockStyle.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TKMemoBlockStyle.SetCancelFloat(const Value: Boolean);
begin
  if Value <> FCancelFloat then
  begin
    FCancelFloat := Value;
    Changed;
  end;
end;

procedure TKMemoBlockStyle.SetContentPadding(const Value: TKRect);
begin
  if not FContentPadding.EqualProperties(Value) then
  begin
    FContentPadding.Assign(Value);
    Changed;
  end;
end;

procedure TKMemoBlockStyle.SetLeftPadding(const Value: Integer);
begin
  if Value <> FContentPadding.Left then
  begin
    FContentPadding.Left := Value;
    Changed;
  end;
end;

procedure TKMemoBlockStyle.SetRightPadding(const Value: Integer);
begin
  if Value <> FContentPadding.Right then
  begin
    FContentPadding.Right := Value;
    Changed;
  end;
end;

procedure TKMemoBlockStyle.SetTopPadding(const Value: Integer);
begin
  if Value <> FContentPadding.Top then
  begin
    FContentPadding.Top := Value;
    Changed;
  end;
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
  FHAlign := halLeft;
  FLineWrap := False;
  FWordWrap := True;
end;

procedure TKMemoParaStyle.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TKMemoParaStyle then
  begin
    FirstIndent := TKMemoParaStyle(ASource).FirstIndent;
    HAlign := TKMemoParaStyle(ASource).HAlign;
    LineWrap := TKMemoParaStyle(ASource).LineWrap;
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

procedure TKMemoParaStyle.SetHAlign(const Value: TKHAlign);
begin
  if Value <> FHAlign then
  begin
    FHAlign := Value;
    Changed;
  end;
end;

procedure TKMemoParaStyle.SetLineWrap(const Value: Boolean);
begin
  if Value <> FLineWrap then
  begin
    FLineWrap := Value;
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
  FCaretRect := CreateEmptyRect;
  FColors := TKMemoColors.Create(Self);
  FContentPadding := TKRect.Create;
  FContentPadding.Left := 10;
  FContentPadding.Right := 10;
  FContentPadding.Top := 10;
  FContentPadding.OnChanged := ContentPaddingChanged;
  FDisabledDrawStyle := cDisabledDrawStyleDef;
  FHorzScrollStep := cHorzScrollStepDef;
  FLeftPos := 0;
  FLinePosition := eolInside;
  FMouseWheelAccumulator := 0;
  FOldCaretRect := CreateEmptyRect;
  FOptions := [eoGroupUndo];
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
  FOnChange := nil;
  FOnReplaceText := nil;
  Text := 'This is early alpha state control.'+cEOL+'You may try the demo but do not use it in your programs yet.';
  UpdateEditorCaret;
end;

destructor TKCustomMemo.Destroy;
begin
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
end;

function TKCustomMemo.CanScroll(ACommand: TKEditCommand): Boolean;
var
  R: TRect;
begin
  case ACommand of
    ecScrollUp:  Result := FTopPos > 0;
    ecScrollDown: Result := FTopPos < FVertExtent - 1;
    ecScrollLeft: Result := FLeftPos > 0;
    ecScrollRight: Result := FLeftPos < FHorzExtent - 1;
    ecScrollCenter:
    begin
      R := IndexToRect(SelEnd, True);
      R.Left := R.Left - ClientWidth div 2;
      R.Top := R.Top - ClientHeight div 2;
      Result :=
        (FLeftPos > 0) and (R.Left < 0) or
        (FLeftPos < FHorzExtent - 1) and (R.Left > 0) or
        (FTopPos > 0) and (R.Top < 0) or
        (FTopPos < FVertExtent - 1) and (R.Top > 0);
    end;
  else
    Result := False;
  end;
end;

function TKCustomMemo.CaretInView: Boolean;
begin
  Result := PtInRect(ClientRect, FCaretRect.TopLeft);
end;

function TKCustomMemo.ClampInView(CallScrollWindow: Boolean): Boolean;
var
  DeltaHorz, DeltaVert: Integer;
begin
  UpdateEditorCaret(False);
  Result := ScrollNeeded(DeltaHorz, DeltaVert);
  if Result then
  begin
    Result := Scroll(cScrollNoAction, cScrollNoAction, DeltaHorz, DeltaVert, CallScrollWindow);
    if Result then
      FScrollTimer.Enabled := True;
  end;
end;

procedure TKCustomMemo.Clear;
begin
  FBlocks.Clear;
end;

procedure TKCustomMemo.ClearSelection(ATextOnly: Boolean);
begin
  FBlocks.ClearSelection(ATextOnly);
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
        Result := TmpSelEnd < FBlocks.SelectableLength;
      ecSelRight:
        Result := TmpSelEnd < FBlocks.SelectableLength;
      ecUp, ecSelUp, ecPageUp, ecSelPageUp:
        Result := FBlocks.IndexBelowFirstLine(TmpSelEnd, True);
      ecDown, ecPagedown, ecSelDown, ecSelPageDown:
        Result := FBlocks.IndexAboveLastLine(TmpSelEnd, True);
      ecLineStart, ecPageLeft, ecSelLineStart, ecSelPageLeft:
        Result := TmpSelEnd > FBlocks.LineStartIndexByIndex(TmpSelEnd, True, TmpLinePos);
      ecLineEnd, ecPageRight:
        Result := TmpSelEnd < FBlocks.LineEndIndexByIndex(TmpSelEnd, True, False, TmpLinePos);
      ecSelLineEnd, ecSelPageRight:
        Result := TmpSelEnd < FBlocks.LineEndIndexByIndex(TmpSelEnd, True, True, TmpLinePos);
      ecPageTop, ecSelPageTop:
        Result := TmpSelEnd <> FBlocks.NextIndexByVertValue(Canvas, -ContentTop + VertScrollPadding, FPreferredCaretPos, False, TmpLinePos);
      ecPageBottom, ecSelPageBottom:
        Result := TmpSelEnd <> FBlocks.NextIndexByVertValue(Canvas, -ContentTop + VertScrollPadding + ClientHeight, FPreferredCaretPos, True, TmpLinePos);
      ecEditorTop, ecSelEditorTop:
        Result := TmpSelEnd > 0;
      ecEditorBottom, ecSelEditorBottom:
        Result := TmpSelEnd < FBlocks.SelectableLength;
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
        Result := not (Empty or ReadOnly) and ((TmpSelLength > 0) or (TmpSelEnd < FBlocks.SelectableLength - 1));
      ecDeleteBOL:
        Result := not (Empty or ReadOnly) and ((TmpSelLength > 0) or (TmpSelEnd <> FBlocks.LineStartIndexByIndex(TmpSelEnd, True, TmpLinePos)));
      ecDeleteEOL:
        Result := not (Empty or ReadOnly) and ((TmpSelLength > 0) or (TmpSelEnd <> FBlocks.LineEndIndexByIndex(TmpSelEnd, True, True, TmpLinePos)));
      ecDeleteLine, ecSelectAll, ecClearAll, ecReplace:
        Result := not (Empty or ReadOnly);
      ecClearSelection:
        Result := not (Empty or ReadOnly) and (TmpSelLength > 0);
      ecSearch:
        Result := not Empty;
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
  FBlocks.DeleteBOL(At);
end;

procedure TKCustomMemo.DeleteChar(At: Integer);
begin
  FBlocks.DeleteChar(At);
end;

procedure TKCustomMemo.DeleteEOL(At: Integer);
begin
  FBlocks.DeleteEOL(At);
end;

procedure TKCustomMemo.DeleteLastChar(At: Integer);
begin
  FBlocks.DeleteLastChar(At);
end;

procedure TKCustomMemo.DeleteLine(At: Integer);
begin
  FBlocks.DeleteLine(At);
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
begin
  // TODO
  Result := False;
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
begin
  // TODO
  Result := False;
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
    if SelLength = 0 then
      FBlocks.FixEOL(SelEnd, True, FLinePosition);
    if Reasons * [muContent, muExtent] <> [] then
      UpdateScrollRange(True)
    else if muSelectionScroll in Reasons then
    begin
      if not ClampInView(False) then
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
        NewSelEnd := FBlocks.NextIndexByCharCount(TmpSelEnd, -1);
        SelectionInit(NewSelEnd, True, eolInside);
      end;
      ecSelLeft:
      begin
        NewSelEnd := FBlocks.NextIndexByCharCount(TmpSelEnd, -1);
        SelectionExpand(NewSelEnd, True, eolInside);
      end;
      ecRight:
      begin
        NewSelEnd := FBlocks.NextIndexByCharCount(TmpSelEnd, 1);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelRight:
      begin
        NewSelEnd := FBlocks.NextIndexByCharCount(TmpSelEnd, 1);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecUp: 
      begin
        NewSelEnd := FBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, -1, FPreferredCaretPos, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelUp: 
      begin
        NewSelEnd := FBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, -1, FPreferredCaretPos, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecDown: 
      begin
        NewSelEnd := FBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, 1, FPreferredCaretPos, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelDown: 
      begin
        NewSelEnd := FBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, 1, FPreferredCaretPos, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecLineStart: 
      begin
        NewSelEnd := FBlocks.LineStartIndexByIndex(TmpSelEnd, True, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelLineStart: 
      begin
        NewSelEnd := FBlocks.LineStartIndexByIndex(TmpSelEnd, True, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecLineEnd: 
      begin
        NewSelEnd := FBlocks.LineEndIndexByIndex(TmpSelEnd, True, False, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelLineEnd: 
      begin
        NewSelEnd := FBlocks.LineEndIndexByIndex(TmpSelEnd, True, True, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageUp: 
      begin
        NewSelEnd := FBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, -ClientHeight, FPreferredCaretPos, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageUp: 
      begin
        NewSelEnd := FBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, -ClientHeight, FPreferredCaretPos, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageDown: 
      begin
        NewSelEnd := FBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, ClientHeight, FPreferredCaretPos, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageDown: 
      begin
        NewSelEnd := FBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, ClientHeight, FPreferredCaretPos, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageLeft: 
      begin
        NewSelEnd := FBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, -ClientWidth, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageLeft: 
      begin
        NewSelEnd := FBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, -ClientWidth, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageRight: 
      begin
        NewSelEnd := FBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, ClientWidth, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageRight:
      begin
        NewSelEnd := FBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, ClientWidth, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageTop: 
      begin
        NewSelEnd := FBlocks.NextIndexByVertValue(Canvas, -ContentTop + VertScrollPadding, FPreferredCaretPos, False, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageTop: 
      begin
        NewSelEnd := FBlocks.NextIndexByVertValue(Canvas, FTopPos * FVertScrollStep + VertScrollPadding, FPreferredCaretPos, False, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecPageBottom: 
      begin
        NewSelEnd := FBlocks.NextIndexByVertValue(Canvas, -ContentTop + ClientHeight - VertScrollPadding, FPreferredCaretPos, True, TmpPosition);
        SelectionInit(NewSelEnd, True, TmpPosition);
      end;
      ecSelPageBottom: 
      begin
        NewSelEnd := FBlocks.NextIndexByVertValue(Canvas, FTopPos * FVertScrollStep + ClientHeight - VertScrollPadding, FPreferredCaretPos, True, TmpPosition);
        SelectionExpand(NewSelEnd, True, TmpPosition);
      end;
      ecEditorTop: SelectionInit(0, True, eolInside);
      ecSelEditorTop: SelectionExpand(0, True, eolInside);
      ecEditorBottom: SelectionInit(FBlocks.SelectableLength, True, eolEnd);
      ecSelEditorBottom: SelectionExpand(FBlocks.SelectableLength, True, eolEnd);
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
        ExecuteCommand(ecCopy);
        ExecuteCommand(ecClearSelection);
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
      ecSelectAll: Select(0, FBlocks.SelectableLength);
      ecClearAll:
      begin
        ExecuteCommand(ecSelectAll);
        ExecuteCommand(ecClearSelection);
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
        FPreferredCaretPos := FCaretRect.Left - ContentLeft;
    end;
  end;
end;

procedure TKCustomMemo.FontChange(Sender: TObject);
begin
  FTextStyle.Font.Assign(Font);
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
  Result := FBlocks.RealSelEnd;
end;

function TKCustomMemo.GetRealSelStart: Integer;
begin
  Result := FBlocks.RealSelStart;
end;

function TKCustomMemo.GetRequiredContentWidth: Integer;
begin
  if FRequiredContentWidth > 0 then
    Result := FRequiredContentWidth
  else
    Result := ClientWidth - FContentPadding.Left - FContentPadding.Right;
  Result := DivDown(Result, FHorzScrollStep) * FHorzScrollStep;
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
  Result := FBlocks.SelectableLength;
end;

function TKCustomMemo.GetSelEnd: Integer;
begin
  Result := FBlocks.SelEnd;
end;

function TKCustomMemo.GetSelLength: Integer;
begin
  Result := FBlocks.SelLength;
end;

function TKCustomMemo.GetSelStart: Integer;
begin
  Result := FBlocks.SelStart
end;

function TKCustomMemo.GetSelText: TKString;
begin
  Result := FBlocks.SelText;
end;

function TKCustomMemo.GetShowFormatting: Boolean;
begin
  Result := eoShowFormatting in FOptions;
end;

function TKCustomMemo.GetText: TKString;
begin
  Result := FBlocks.Text;
end;

function TKCustomMemo.GetUndoLimit: Integer;
begin
  Result := FUndoList.Limit;
end;

function TKCustomMemo.GetVertScrollPadding: Integer;
begin
  Result := Min(FScrollPadding, ClientHeight div 8);
end;

procedure TKCustomMemo.HideEditorCaret;
begin
  if HandleAllocated then
    HideCaret(Handle);
end;

function TKCustomMemo.IndexToRect(AValue: Integer; ACaret: Boolean): TRect;
begin
  Result := BlockRectToRect(FBlocks.IndexToRect(Canvas, AValue, ACaret, FBlocks.EOLToNormal(AValue)));
end;

function TKCustomMemo.IndexValid(Value: Integer): Boolean;
begin
  Result := False;
end;

procedure TKCustomMemo.InsertChar(At: Integer; AValue: TKChar);
begin
  FBlocks.InsertChar(At, AValue, elOverwrite in FStates);
end;

procedure TKCustomMemo.InsertNewLine(At: Integer);
begin
  FBlocks.InsertNewLine(At);
end;

procedure TKCustomMemo.InsertString(At: Integer; const AValue: TKString);
begin
  if AValue <> '' then
  begin
    BeginUndoGroup(ckInsert);
    try
      if FBlocks.SelLength > 0 then
      begin
        FBlocks.ClearSelection;
        At := FBlocks.SelEnd;
      end;
      // always insert (don't overwrite)
      FBlocks.InsertString(At, True, AValue);
    finally
      EndUndoGroup;
    end;
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
    if Key = VK_ESCAPE then
      Include(FStates, elIgnoreNextChar);
  end;
end;

{$IFDEF FPC}
procedure TKCustomMemo.UTF8KeyPress(var Key: TUTF8Char);
{$ELSE}
procedure TKCustomMemo.KeyPress(var Key: Char);
{$ENDIF}
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if not (elIgnoreNextChar in FStates) then
    begin
      ExecuteCommand(ecInsertChar, @Key);
    end else
      Exclude(FStates, elIgnoreNextChar);
  end;
end;

procedure TKCustomMemo.LoadFromRTF(const AFileName: TKString);
var
  Reader: TKMemoRTFReader;
begin
  Reader := TKMemoRTFReader.Create(Self);
  try
    Reader.Load(AFileName);
  finally
    Reader.Free;
  end;
end;

procedure TKCustomMemo.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  if Enabled and (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    SafeSetFocus;
    P := Point(X, Y);
    Include(FStates, elMouseCapture);
    SelectionInit(P, False);
  end;
end;

procedure TKCustomMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  if (elMouseCapture in FStates) then
  begin
    if not FScrollTimer.Enabled then
    begin
      P := Point(X, Y);
      SelectionExpand(P, True);
    end;
  end;
end;

procedure TKCustomMemo.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  Exclude(FStates, elMouseCapture);
  UpdateEditorCaret;
end;

procedure TKCustomMemo.PaintToCanvas(ACanvas: TCanvas);
var
  H, I, J: Integer;
begin
{$IFDEF FPC}
  if CaretVisible then
    HideEditorCaret;
  try
{$ENDIF}
    if (FBackgroundImage.Graphic <> nil) and not FBackgroundImage.Graphic.Empty then
    begin
      I := -FLeftPos * FHorzScrollStep mod FBackgroundImage.Width;
      J := -FTopPos * FVertScrollStep mod FbackgroundImage.Height;
      H := I;
      while J < ClientHeight do
      begin
        ACanvas.Draw(I, J, FBackgroundImage.Graphic);
        Inc(I, FBackgroundImage.Width);
        if I >= ClientWidth then
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
      ACanvas.FillRect(ClientRect);
    end;

    FBlocks.PaintToCanvas(ACanvas, ContentLeft, ContentTop, ClientRect);
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

function TKCustomMemo.PointToBlockPoint(const APoint: TPoint): TPoint;
begin
  Result.X := APoint.X - ContentLeft;
  Result.Y := APoint.Y - ContentTop;
end;

function TKCustomMemo.PointToIndex(APoint: TPoint; AOutOfArea, ASelectionExpanding: Boolean; out ALinePos: TKMemoLinePosition): Integer;
begin
  Result := FBlocks.PointToIndex(Canvas, PointToBlockPoint(APoint), AOutOfArea, ASelectionExpanding, ALinePos);
end;

procedure TKCustomMemo.SafeSetFocus;
begin
  if not Focused and CanFocus and not (csDesigning in ComponentState) then SetFocus;
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
      SB_BOTTOM: Pos := MaxScrollPos - Integer(SI.nPage) + 1;
      SB_LINEUP: Dec(Pos);
      SB_LINEDOWN: Inc(Pos);
      SB_PAGEUP: Dec(Pos, SI.nPage);
      SB_PAGEDOWN: Inc(Pos, SI.nPage);
      SB_THUMBTRACK: Pos := SI.nTrackPos;
    end;
    Pos := MinMax(Pos, 0, MaxScrollPos - Integer(SI.nPage) + 1);
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
  ScrollHorzAxis := Axis(SB_HORZ, FScrollBars in [ssHorizontal, ssBoth], CodeHorz, DeltaHorz, FHorzExtent, FLeftPos);
  ScrollVertAxis := Axis(SB_VERT, FScrollBars in [ssVertical, ssBoth], CodeVert, DeltaVert, FVertExtent, FTopPos);
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

function TKCustomMemo.ScrollNeeded(out DeltaCol, DeltaRow: Integer): Boolean;
var
  HScrollPadding, VScrollPadding: Integer;
begin
  HScrollPadding := HorzScrollPadding;
  VScrollPadding := VertScrollPadding;
  if FCaretRect.Left < HScrollPadding then
    DeltaCol := DivDown(FCaretRect.Left - HScrollPadding, FHorzScrollStep)
  else if (FCaretRect.Left + FCaretRect.Right > ClientWidth - HScrollPadding){ and (FCaretRect.Left > FHorzScrollStep)} then
    DeltaCol := DivUp(FCaretRect.Left + FCaretRect.Right - ClientWidth + HScrollPadding, FHorzScrollStep)
  else
    DeltaCol := 0;
  if FCaretRect.Top < VScrollPadding then
    DeltaRow := DivDown(FCaretRect.Top - VScrollPadding, FVertScrollStep)
  else if (FCaretRect.Top + FCaretRect.Bottom > ClientHeight - VScrollPadding){ and (FCaretRect.Top > FVertScrollStep)} then
    DeltaRow := DivUp(FCaretRect.Top + FCaretRect.Bottom - ClientHeight + VScrollPadding, FVertScrollStep)
  else
    DeltaRow := 0;
  Result := (DeltaCol <> 0) or (DeltaRow <> 0);
end;

procedure TKCustomMemo.ScrollTimerHandler(Sender: TObject);
var
  DeltaHorz, DeltaVert: Integer;
begin
  if (elMouseCapture in FStates) and not Dragging then
  begin
    SelectionExpand(ScreenToClient(Mouse.CursorPos), False);
    if ScrollNeeded(DeltaHorz, DeltaVert) then
      Scroll(cScrollNoAction, cScrollNoAction, DeltaHorz, DeltaVert, False)
    else
      FScrollTimer.Enabled := False;
  end else
    FScrollTimer.Enabled := False;
end;

function TKCustomMemo.SelAvail: Boolean;
begin
  Result := SelLength > 0;
end;

procedure TKCustomMemo.Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean);
begin
  FBlocks.Select(ASelStart, ASelLength, ADoScroll);
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
  NewSelEnd := PointToIndex(APoint, True, False, FLinePosition);
  Select(NewSelEnd, 0, ADoScroll);
  FPreferredCaretPos := FCaretRect.Left - ContentLeft;
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
  Value := MinMax(Value, 0, FHorzExtent - 1);
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

function TKCustomMemo.SetMouseCursor(X, Y: Integer): Boolean;
var
  ACursor: TCursor;
  P: TPoint;
begin
  P := Point(X, Y);
  if PtInRect(ContentRect, P) then
  begin
    ACursor := crIBeam;
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
  FBlocks.LockUpdate;
  try
    FBlocks.Clear;
    FBlocks.Text := Value;
  finally
    FBlocks.UnlockUpdate;
  end;
end;

procedure TKCustomMemo.SetTopPos(Value: Integer);
begin
  Value := MinMax(Value, 0, FVertExtent - 1);
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

procedure TKCustomMemo.UpdateScrollRange(CallInvalidate: Boolean);
var
  DeltaHorz, DeltaVert, ClientHorz, ClientVert: Integer;
  SI: TScrollInfo;
begin
  if HandleAllocated then
  begin
    FBlocks.MeasureExtent(Canvas, RequiredContentWidth);
    FHorzExtent := DivUp(FBlocks.Width + FContentPadding.Left + FContentPadding.Right, FHorzScrollStep);
    FVertExtent := DivUp(FBlocks.Height + FCOntentPadding.Top + FContentPadding.Bottom, FVertScrollStep);
    ClientHorz := ClientWidth div FHorzScrollStep;
    ClientVert := ClientHeight div FVertScrollStep;
    DeltaHorz := Max(ClientHorz + FLeftPos - FHorzExtent, 0);
    DeltaVert := Max(ClientVert + FTopPos - FVertExtent, 0);
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
        SI.nMax := FHorzExtent{$IFNDEF FPC}- 1{$ENDIF};
        SI.nPage := ClientHorz;
        SI.nPos := FLeftPos;
        SetScrollInfo(Handle, SB_HORZ, SI, True);
        ShowScrollBar(Handle, SB_HORZ, Integer(SI.nPage) < SI.nMax);
      end else
        ShowScrollBar(Handle, SB_HORZ, False);
      if FScrollBars in [ssBoth, ssVertical] then
      begin
        SI.nMax := FVertExtent{$IFNDEF FPC}- 1{$ENDIF};
        SI.nPage := ClientVert;
        SI.nPos := FTopPos;
        SetScrollInfo(Handle, SB_VERT, SI, True);
        ShowScrollBar(Handle, SB_VERT, Integer(SI.nPage) < SI.nMax);
      end else
        ShowScrollBar(Handle, SB_VERT, False);
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
//  _AddRef; // disable interface reference counting error
  FOffset := CreateEmptyPoint;
  FParent := nil;
  Parent := AParent; // update default block properties!
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

procedure TKMemoBlock.BeforeDestruction;
begin
  // disable interface reference counting error
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
    Result := FParent.GetDefaultTextStyle
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

function TKMemoBlock.GetParaStyle: TKMemoParaStyle;
begin
  Result := nil;
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
    Result := FParent.GetShowFormatting
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

function TKMemoBlock.GetWordCount: Integer;
begin
  Result := 0;
end;

function TKMemoBlock.GetWordHeight(Index: Integer): Integer;
begin
  Result := 0;
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
    Extent := MeasureWordExtent(ACanvas, I, ARequiredWidth);
    Inc(Result.X, Extent.X);
    Result.Y := Max(Result.Y, Extent.Y);
  end;
end;

function TKMemoBlock.MeasureWordExtent(ACanvas: TCanvas; AWordIndex, ARequiredWidth: Integer): TPoint;
begin
  Result := CreateEmptyPoint;
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

function TKMemoBlock.Split(At: Integer): TKMemoBlock;
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

function TKMemoTextBlock.ApplyFormatting(const AText: TKString): TKString;
begin
  if GetShowFormatting then
  begin
    Result := StringReplace(AText, ' ', SpaceChar, [rfReplaceAll]);
    Result := StringReplace(AText, #9, TabChar, [rfReplaceAll]);
  end else
  begin
    Result := StringReplace(AText, #9, ' ', [rfReplaceAll]);
    Result := StringReplace(AText, NewLineChar, ' ', [rfReplaceAll]);
  end;
end;

procedure TKMemoTextBlock.ApplyTextStyle(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Font.Assign(FTextStyle.Font);
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
var
  TM: TTextMetric;
begin
  ApplyTextStyle(ACanvas);
  GetTextMetrics(ACanvas.Handle, TM);
  Result := TM.tmAscent;
end;

procedure TKMemoTextBlock.ClearSelection(ATextOnly: Boolean);
var
  S: TKString;
begin
  inherited;
  if SelLength <> 0 then
  begin
    S := Text;
    DeleteText(S, FSelStart + 1, FSelEnd - FSelStart);
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

class function TKMemoTextBlock.CopyText(const ASource: TKString; At,
  Count: Integer): TKString;
begin
{$IFDEF FPC}
  Result := UTF8Copy(ASource, At, Count);
{$ELSE}
  Result := Copy(ASource, At, Count);
{$ENDIF}
end;

class procedure TKMemoTextBlock.DeleteText(var ASource: TKString; At,
  Count: Integer);
begin
{$IFDEF FPC}
  UTF8Delete(ASource, At, Count);
{$ELSE}
  Delete(ASource, At, Count);
{$ENDIF}
end;

function TKMemoTextBlock.GetCanAddText: Boolean;
begin
  Result := Position = mbpText;
end;

function TKMemoTextBlock.GetSelText: TKString;
begin
  Result := CopyText(Text, FSelStart + 1, FSelEnd - FSelStart);
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

function TKMemoTextBlock.GetWords(Index: Integer): TKString;
begin
  Result := CopyText(Text, FWords[Index].StartIndex + 1, FWords[Index].EndIndex - FWords[Index].StartIndex + 1);
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

function TKMemoTextBlock.MeasureWordExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint;
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

function TKMemoTextBlock.ModifiedTextExtent(ACanvas: TCanvas; const AText: TKString): TPoint;
var
  Size: TSize;
  SU: TKString;
  I, SmallFontSize, X, Y: Integer;
begin
  if FTextStyle.Capitals = tcaNone then
  begin
    Size := ACanvas.TextExtent(AText);
    Result := Point(Size.cx, Size.cy);
  end else
  begin
    SU := AnsiUpperCase(AText);
    if FTextStyle.Capitals = tcaNormal then
    begin
      Size := ACanvas.TextExtent(SU);
      Result := Point(Size.cx, Size.cy);
    end else
    begin
      SmallFontSize := MulDiv(FTextStyle.Font.Size, 4, 5);
      X := 0; Y := 0;
      for I := 1 to Length(SU) do
      begin
        if SU[I] <> AText[I] then
          ACanvas.Font.Size := SmallFontSize
        else
          ACanvas.Font.Size := FTextStyle.Font.Size;
        Size := ACanvas.TextExtent(SU[I]);
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
    FTextLength := TextLength(Value);
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

function TKMemoTextBlock.Split(At: Integer): TKMemoBlock;
var
  Item: TKMemoTextBlock;
  S, Part1, Part2: TKString;
begin
  if (At > 0) and (At < ContentLength) then
  begin
    Item := TKMemoTextBlock.Create(FParent);
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
{$IFDEF FPC}
  APart1 := UTF8Copy(ASource, 1, At - 1);
  APart2 := UTF8Copy(ASource, At, Length(ASource) - At + 1);
{$ELSE}
  APart1 := Copy(ASource, 1, At - 1);
  APart2 := Copy(ASource, At, Length(ASource) - At + 1);
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

class function TKMemoTextBlock.TextLength(const ASource: TKString): Integer;
begin
{$IFDEF FPC}
  Result := UTF8Length(ASource);
{$ELSE}
  Result := Length(ASource);
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

const
  cMemoWordBreaks = [cNULL, cTAB, cSPACE, '/', '\'];
var
  Index, PrevIndex, CharIndex: Integer;
  WasBreak: Boolean;
begin
  FWords.Clear;
  if FText <> '' then
  begin
    CharIndex := 1;
    Index := 1;
    PrevIndex := 1;
    WasBreak := False;
    while Index <= FTextLength do
    begin
      if CharInSetEx(FText[CharIndex], cMemoWordBreaks) then
        WasBreak := True
      else if WasBreak then
      begin
        AddWord(PrevIndex, Index - 1);
        PrevIndex := Index;
        WasBreak := False;
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
    S := CopyText(FText, Word.StartIndex + 1, AIndex);
    T := CopyText(FText, Word.StartIndex + AIndex + 1, 1);
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

procedure TKMemoTextBlock.WordPaintToCanvas(ACanvas: TCanvas;
  AWordIndex: Integer; ALeft, ATop: Integer);

  procedure TextDraw(const ARect: TRect; ABaseLine: Integer; const AText: TKString);
  var
    SU: TKString;
    I, SmallFontSize, X: Integer;
    Size: TSize;
  {$IFDEF FPC}
    TM: TLCLTextMetric;
  {$ENDIF}
  begin
    with ACanvas do
    begin
      if Brush.Style <> bsClear then
        DrawFilledRectangle(ACanvas, ARect, clNone);
    {$IFDEF FPC}
      GetTextMetrics(TM);
      Dec(ABaseline, TM.Ascender);
    {$ELSE}
      SetTextAlign(Handle, TA_BASELINE);
    {$ENDIF}
      SetBkMode(Handle, TRANSPARENT);
      if FTextStyle.Capitals = tcaNone then
        TextOut(ARect.Left, ABaseLine, AText)
      else
      begin
        SU := AnsiUpperCase(AText);
        if FTextStyle.Capitals = tcaNormal then
          TextOut(ARect.Left, ABaseLine, SU)
        else
        begin
          SmallFontSize := MulDiv(FTextStyle.Font.Size, 4, 5);
          X := ARect.Left;
          for I := 1 to Length(SU) do
          begin
            if SU[I] <> AText[I] then
              Font.Size := SmallFontSize
            else
              Font.Size := FTextStyle.Font.Size;
            TextOut(X, ABaseLine, SU[I]);
            Size := TextExtent(SU[I]);
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
    if (FSelEnd > FSelStart) and (Word.EndIndex >= FSelStart) and (Word.StartIndex < FSelEnd) then
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
        S := ApplyFormatting(CopyText(FText, I + 1, 1));
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

{ TKParagraph }

constructor TKMemoParagraph.Create(AParent: TKMemoBlocks);
begin
  inherited;
  FExtent := CreateEmptyPoint;
  FTextStyle.AllowBrush := False;
  FParaStyle := TKMemoParaStyle.Create;
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

procedure TKMemoParagraph.NotifyDefaultParaChange;
begin
  FParaStyle.NotifyChange(GetDefaultParaStyle);
end;

function TKMemoParagraph.Split(At: Integer): TKMemoBlock;
begin
  Result := nil;
end;

{ TKImageMemoBlock }

constructor TKMemoImageBlock.Create(AParent: TKMemoBlocks);
begin
  inherited;
  FImageStyle := TKMemoBlockStyle.Create;
  FImageStyle.ContentPadding.AssignFromValues(10, 10, 10, 10);
  FImageStyle.OnChanged := ImageStyleChanged;
  FBottomPadding := 0;
  FImage := TPicture.Create;
  FExtent := CreateEmptyPoint;
  FPosition := CreateEmptyPoint;
  FScaleExtent := CreateEmptyPoint;
  FTopPadding := 0;
end;

destructor TKMemoImageBlock.Destroy;
begin
  FImageStyle.Free;
  FImage.Free;
  inherited;
end;

procedure TKMemoImageBlock.Assign(AItem: TKMemoBlock);
begin
  inherited;
  if AItem is TKMemoImageBlock then
  begin
    FImage.Assign(TKMemoImageBlock(AItem).Image);
    ScaleHeight := TKMemoImageBlock(AItem).ScaleHeight;
    ScaleWidth := TKMemoImageBlock(AItem).ScaleWidth;
    UpdateExtent;
  end;
end;

function TKMemoImageBlock.ContentLength: Integer;
begin
  Result := 1;
end;

function TKMemoImageBlock.GetImageHeight: Integer;
begin
  if FScaleExtent.Y <> 0 then
    Result := FScaleExtent.Y
  else
    Result := FImage.Height
end;

function TKMemoImageBlock.GetImageWidth: Integer;
begin
  if FScaleExtent.X <> 0 then
    Result := FScaleExtent.X
  else
    Result := FImage.Width
end;

function TKMemoImageBlock.GetWordBottomPadding(Index: Integer): Integer;
begin
  Result := FBottomPadding;
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
  Result := FTopPadding;
end;

function TKMemoImageBlock.GetWordWidth(Index: Integer): Integer;
begin
  Result := FExtent.X;
end;

procedure TKMemoImageBlock.ImageStyleChanged(Sender: TObject);
begin
  Update([muExtent]);
end;

function TKMemoImageBlock.MeasureWordExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint;
begin
  Result := Point(
    ImageWidth + FImageStyle.ContentPadding.Left + FImageStyle.ContentPadding.Right,
    ImageHeight + FImageStyle.ContentPadding.Top + FImageStyle.ContentPadding.Bottom);
  FExtent := Result;
end;

function TKMemoImageBlock.OuterRect(ACaret: Boolean): TRect;
begin
  Result.TopLeft := FPosition;
  Result.Right := Result.Left + FExtent.X;
  Result.Bottom := Result.Top + FExtent.Y;
  if ACaret then
  begin
    Inc(Result.Top, FTopPadding);
    Dec(Result.Bottom, FBottomPadding);
  end;
  OffsetRect(Result, InternalLeftOffset, InternalTopOffset);
end;

procedure TKMemoImageBlock.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
//  UpdateExtent;
  Update([muContent]);
end;

procedure TKMemoImageBlock.SetImagePath(const Value: TKString);
begin
  FImage.LoadFromFile(Value);
//  UpdateExtent;
  Update([muContent]);
end;

procedure TKMemoImageBlock.SetScaleHeight(const Value: Integer);
begin
  if Value <> FScaleExtent.Y then
  begin
    FScaleExtent.Y := Value;
    Update([muExtent]);
  end;
end;

procedure TKMemoImageBlock.SetScaleWidth(const Value: Integer);
begin
  if Value <> FScaleExtent.X then
  begin
    FScaleExtent.X := Value;
    Update([muExtent]);
  end;
end;

procedure TKMemoImageBlock.SetWordBottomPadding(Index: Integer; const Value: Integer);
begin
  FBottomPadding := Value;
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
  FTopPadding := Value;
end;

procedure TKMemoImageBlock.UpdateExtent;
begin

end;

{procedure TKMemoImageBlock.UpdateExtent;
begin
  FExtent := Point(
    FImage.Width + FImageStyle.ContentPadding.Left + FImageStyle.ContentPadding.Right,
    FImage.Height + FImageStyle.ContentPadding.Top + FImageStyle.ContentPadding.Bottom);
end;}

function TKMemoImageBlock.WordIndexToRect(ACanvas: TCanvas; AWordIndex,
  AIndex: Integer; ACaret: Boolean): TRect;
begin
  Result := OuterRect(ACaret);
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
  X := ROuter.Left + FImageStyle.ContentPadding.Left;
  Y := ROuter.Top + FImageStyle.ContentPadding.Top + FTopPadding;
  if SelLength > 0 then
  begin
    GetSelColors(Color, BkGnd);
    ACanvas.Brush.Color := BkGnd;
    ACanvas.FillRect(ROuter);
    Bitmap := TKAlphaBitmap.Create;
    try
      Bitmap.SetSize(ImageWidth, ImageHeight);
    {$IFDEF FPC}
      Bitmap.UpdateHandle;
    {$ENDIF}
      Bitmap.Canvas.Brush.Color := BkGnd;
      Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
      Bitmap.Canvas.StretchDraw(Rect(0, 0, ImageWidth, ImageHeight), FImage.Graphic);
    {$IFDEF FPC}
      Bitmap.UpdatePixels;
    {$ENDIF}
      Bitmap.AlphaFillPercent(50, True);
      Bitmap.AlphaDrawTo(ACanvas, X, Y);
    finally
      Bitmap.Free;
    end;
  end else
  begin
    FImageStyle.PaintBox(ACanvas, ROuter);
    ACanvas.StretchDraw(Rect(X, Y, X + ImageWidth, Y + ImageHeight), FImage.Graphic);
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
  FClip := False;
  FCurrentRequiredHeight := 0;
  FCurrentRequiredWidth := 0;
  FFixedWidth := False;
  FPosition := CreateEmptyPoint;
  FRequiredHeight := 0;
  FRequiredWidth := 0;
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

procedure TKMemoContainer.AddBlockLine(AStartBlock, AStartIndex, AEndBlock, AEndIndex, ALeft, ATop, AWidth, AHeight: Integer);
var
  Line: TKMemoLine;
begin
  if Position = mbpText then
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

procedure TKMemoContainer.BlockStyleChanged(Sender: TObject);
begin
  Update([muExtent]);
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

function TKMemoContainer.GetSelLength: Integer;
begin
  Result := FBlocks.SelLength;
end;

function TKMemoContainer.GetSelStart: Integer;
begin
  Result := FBlocks.SelStart;
end;

function TKMemoContainer.GetTopPadding: Integer;
begin
  Result := FBlockStyle.TopPadding;
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
  Result := Max(Max(FRequiredHeight, FCurrentRequiredHeight), FBlocks.Height + FBlockStyle.TopPadding + FBlockStyle.BottomPadding);
end;

function TKMemoContainer.GetWordLeft(Index: Integer): Integer;
begin
  Result := FPosition.X;
end;

function TKMemoContainer.GetWordLength(Index: Integer): Integer;
begin
  Result := FBlocks.SelectableLength;
end;

function TKMemoContainer.GetWords(Index: Integer): TKString;
begin
  Result := FBlocks.Text;
end;

function TKMemoContainer.GetWordTop(Index: Integer): Integer;
begin
  Result := FPosition.Y;
end;

function TKMemoContainer.GetWordWidth(Index: Integer): Integer;
begin
  Result := Max(FCurrentRequiredWidth, FBlocks.Width + FBlockStyle.LeftPadding - FBlockStyle.RightPadding);
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

function TKMemoContainer.MeasureWordExtent(ACanvas: TCanvas; AIndex, ARequiredWidth: Integer): TPoint;
begin
  if FFixedWidth then
    FCurrentRequiredWidth := FRequiredWidth
  else
    FCurrentRequiredWidth := ARequiredWidth;
  FCurrentRequiredHeight := 0;
  FBlocks.MeasureExtent(ACanvas, Max(FCurrentRequiredWidth - FBlockStyle.LeftPadding - FBlockStyle.RightPadding, 0));
  Result := Point(Width, Height);
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
  FBlocks.SetExtent(AWidth - FBlockStyle.LeftPadding - FBlockStyle.RightPadding, AHeight - FBlockStyle.TopPadding - FBlockStyle.BottomPadding);
end;

procedure TKMemoContainer.SetClip(const Value: Boolean);
begin
  if Value <> FClip then
  begin
    FClip := Value;
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

function TKMemoContainer.WordIndexToRect(ACanvas: TCanvas; AWordIndex, AIndex: Integer; ACaret: Boolean): TRect;
begin
  Result := FBlocks.IndexToRect(ACanvas, AIndex, ACaret, False);
  if not ACaret then
  begin
    // expand rect to enable vertical caret movement
    if Result.Top = 0 then
      Dec(Result.Top, FBlockStyle.TopPadding);
    if Result.Bottom = FBlocks.Height then
      Inc(Result.Bottom, Height - FBlocks.Height - FBlockStyle.TopPadding);
  end;
  KFunctions.OffsetRect(Result, Left + InternalLeftOffset + FBlockStyle.LeftPadding, Top + InternalTopOffset + FBlockStyle.TopPadding);
end;

procedure TKMemoContainer.WordPaintToCanvas(ACanvas: TCanvas; AIndex, ALeft, ATop: Integer);
var
  R, ClipRect: TRect;
  MainClipRgn: HRGN;
  SaveIndex: Integer;
begin
  R := Rect(0, 0, Width, Height);
  OffsetRect(R, Left + ALeft + InternalLeftOffset, Top + ATop + InternalTopOffset);
  FBlockStyle.PaintBox(ACanvas, R);
  Inc(ALeft, Left + FBlockStyle.LeftPadding + InternalLeftOffset);
  Inc(ATop, Top + FBlockStyle.TopPadding + InternalTopOffset);
  if FClip then
  begin
    ClipRect := FBlockStyle.BorderRect(R);
    SaveIndex := SaveDC(ACanvas.Handle);
    MainClipRgn := CreateEmptyRgn;
    try
      TranslateRectToDevice(ACanvas.Handle, ClipRect);
      if GetClipRgn(ACanvas.Handle, MainClipRgn) <> 1 then
      begin
        DeleteObject(MainClipRgn);
        MainClipRgn := CreateRectRgnIndirect(ClipRect);
      end;
      if ExtSelectClipRect(ACanvas.Handle, ClipRect, RGN_AND, MainClipRgn) then
        FBlocks.PaintToCanvas(ACanvas, ALeft, ATop, R);
    finally
      RgnSelectAndDelete(ACanvas.Handle, MainClipRgn);
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
  end else
    FBlocks.PaintToCanvas(ACanvas, ALeft, ATop, R);
end;

function TKMemoContainer.WordPointToIndex(ACanvas: TCanvas; const APoint: TPoint;
  AWordIndex: Integer; AOutOfArea, ASelectionExpanding: Boolean; out APosition: TKMemoLinePosition): Integer;
var
  P: TPoint;
  R: TRect;
begin
  P := APoint;
  R := Rect(0, 0, Width, Height);
  OffsetPoint(P, -Left - InternalLeftOffset, -Top - InternalTopOffset);
  if PtInRect(R, P) or (AOutOfArea and (P.X >= R.Left) and (P.X < R.Right)) then
  begin
    OffsetPoint(P, -FBlockStyle.LeftPadding, -FBlockStyle.TopPadding);
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
  FParaStyle.WordWrap := False;
end;

destructor TKMemoTableCell.Destroy;
begin
  FParaStyle.Free;
  inherited;
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

procedure TKMemoTableCell.RequiredWidthChanged;
var
  Row: TKMemoTableRow;
begin
  Row := ParentRow;
  if Row <> nil then
    Row.UpdateRequiredWidth;
end;

{ TKMemoTableRow }

constructor TKMemoTableRow.Create(AParent: TKMemoBlocks);
begin
  inherited;
  FParaStyle := TKMemoParaStyle.Create;
  FParaStyle.WordWrap := False;
  FParaStyle.LineWrap := True;
end;

destructor TKMemoTableRow.Destroy;
begin
  FParaStyle.Free;
  inherited;
end;

procedure TKMemoTableRow.ApplyDefaultCellStyle;
var
  Table: TKMemoTable;
  Cell: TKMemoTableCell;
  LastCol, LastRow: Boolean;
  I: Integer;
begin
  Table := ParentTable;
  if Table <> nil then
  begin
    LastRow := Table.Blocks.IndexOf(Self) = Table.RowCount - 1;
    for I := 0 to Blocks.Count - 1 do
    begin
      LastCol := I = Blocks.Count - 1;
      Cell := Blocks[I] as TKMemoTableCell;
      Cell.BlockStyle.Assign(Table.CellStyle);
      Cell.BlockStyle.BorderWidth := 0;
      Cell.BlockStyle.BorderWidths.Left := Table.CellStyle.BorderWidth;
      Cell.BlockStyle.BorderWidths.Top := Table.CellStyle.BorderWidth;
      if LastCol then
        Cell.BlockStyle.BorderWidths.Right := Table.CellStyle.BorderWidth;
      if LastRow then
        Cell.BlockStyle.BorderWidths.Bottom := Table.CellStyle.BorderWidth;
    end;
  end;
end;

function TKMemoTableRow.CanAdd(AItem: TKMemoBlock): Boolean;
begin
  // table row can only accept table cells
  Result := AItem is TKMemoTableCell;
end;

function TKMemoTableRow.GetParaStyle: TKMemoParaStyle;
begin
  Result := FParaStyle;
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
    CellWidth := RequiredWidth div CellCount;
    for I := 0 to CellCount - 1 do
      Cells[I].RequiredWidth := CellWidth;
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
end;

destructor TKMemoTable.Destroy;
begin
  FCellStyle.Free;
  FColWidths.Free;
  inherited;
end;

procedure TKMemoTable.ApplyDefaultCellStyle;
var
  I: Integer;
begin
  for I := 0 to RowCount - 1 do
    Rows[I].ApplyDefaultCellStyle;
end;

function TKMemoTable.CanAdd(AItem: TKMemoBlock): Boolean;
begin
  // table can only accept table rows
  Result := AItem is TKMemoTableRow;
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
  if (Index >= 0) and (Index < Blocks.Count) then
    Result := Blocks[Index] as TKMemoTableRow
  else
    Result := nil;
end;

function TKMemoTable.MeasureWordExtent(ACanvas: TCanvas; AIndex,
  ARequiredWidth: Integer): TPoint;
const
  cMinColSize = 20;
var
  I, J, Len, ColWidth, DefColCount, DefSpace, MinSpace, OverflowSpace, UndefColCount, UndefColWidth, UndefSpace, TotalSpace, PosX, PosY: Integer;
  Ratio: Double;
  Extent: TPoint;
  Row: TKMemoTableRow;
  Cell: TKmemoTableCell;
  CalcHorzExtents, MeasHorzExtents, MinHorzExtents, VertExtents: TKmemoSparseList;
begin
  // this is the simple table layout calculation
  if FFixedWidth then
    ARequiredWidth := FRequiredWidth;
  // calculate predefined column widths
  DefColCount := 0;
  DefSpace := 0;
  for I := 0 to FColCount - 1 do
  begin
    Inc(DefSpace, FColWidths[I].Index);
    if FColWidths[I].Index > 0 then
      Inc(DefColCount);
  end;
  if FColCount > DefColCount then
  begin
    UndefColCount := FColCount - DefColCount;
    UndefSpace := Max(ARequiredWidth - DefSpace, UndefColCount * cMinColSize);
    UndefColWidth := DivUp(UndefColCount, UndefSpace);
  end else
    UndefSpace := 0;
  TotalSpace := DefSpace + UndefSpace;
  // now measure cells
  CalcHorzExtents := TKMemoSparseList.Create;
  MeasHorzExtents := TKMemoSparseList.Create;
  MinHorzExtents := TKMemoSparseList.Create;
  VertExtents := TKMemoSparseList.Create;
  try
    CalcHorzExtents.SetSize(FColCount);
    MeasHorzExtents.SetSize(FColCount);
    MinHorzExtents.SetSize(FColCount);
    VertExtents.SetSize(RowCount);
    for J := 0 to RowCount - 1 do
      VertExtents[J].Index := 0;
    // first measure with predefined column width
    for I := 0 to FColCount - 1 do
    begin
      if FColWidths[I].Index > 0 then
        CalcHorzExtents[I].Index := MulDiv(FColWidths[I].Index, ARequiredWidth, TotalSpace)
      else
        CalcHorzExtents[I].Index := UndefColWidth;
      MeasHorzExtents[I].Index := 0;
      MinHorzExtents[I].Index := 0;
      for J := 0 to RowCount - 1 do
      begin
        Row := Rows[J];
        if I < Row.CellCount then
        begin
          Extent := Row.Cells[I].MeasureWordExtent(ACanvas, 0, cMinColSize);
          MinHorzExtents[I].Index := Max(MinHorzExtents[I].Index, Extent.X);
          Extent := Row.Cells[I].MeasureWordExtent(ACanvas, 0, CalcHorzExtents[I].Index);
          MeasHorzExtents[I].Index := Max(MeasHorzExtents[I].Index, Extent.X);
          VertExtents[J].Index := Max(VertExtents[J].Index, Extent.Y);
        end;
      end;
    end;
    // if some MeasHorzExtents are bigger than CalcHorzExtents then recalculate remaining columns to fit required width
    OverflowSpace := 0;
    for I := 0 to FColCount - 1 do
      Inc(OverflowSpace, Max(MeasHorzExtents[I].Index - CalcHorzExtents[I].Index, 0));
    if OverflowSpace > 0 then
    begin
      MinSpace := 0;
      TotalSpace := 0;
      for I := 0 to FColCount - 1 do
        if MeasHorzExtents[I].Index <= CalcHorzExtents[I].Index then
        begin
          Inc(TotalSpace, Max(MeasHorzExtents[I].Index, CalcHorzExtents[I].Index));
          Inc(MinSpace, CalcHorzExtents[I].Index - MinHorzExtents[I].Index);
        end;
      if MinSpace > 0 then
      begin
        Ratio := (TotalSpace - OverflowSpace) / TotalSpace;
        for I := 0 to FColCount - 1 do
          if MeasHorzExtents[I].Index <= CalcHorzExtents[I].Index then
            MeasHorzExtents[I].Index := Max(Round(CalcHorzExtents[I].Index * Ratio), MinHorzExtents[I].Index);
      end;
    end;
    // second measure with maximum column width
    for I := 0 to RowCount - 1 do
    begin
      Row := Rows[I];
      for J := 0 to Row.CellCount - 1 do
      begin
        Cell := Row.Cells[J];
        if (J > FColCount) or (MeasHorzExtents[J].Index <> Cell.Width) then
        begin
          if J < FColCount then
            ColWidth := MeasHorzExtents[J].Index
          else
            ColWidth := MeasHorzExtents[FColCount - 1].Index;
          Extent := Cell.MeasureWordExtent(ACanvas, 0, ColWidth);
          VertExtents[I].Index := Max(VertExtents[I].Index, Extent.Y);
        end;
      end;
    end;
    // set positions and heights
    ClearLines;
    Len := 0;
    PosX := 0;
    PosY := 0;
    for I := 0 to RowCount - 1 do
    begin
      Row := Rows[I];
      PosX := 0;
      for J := 0 to FColCount - 1 do
      begin
        if J < Row.CellCount then
        begin
          Cell := Row.Cells[J];
//          Cell.SetBlockExtent(Cell.WordWidth[0], VertExtents[I].Index); // No! Cell is measured by default way
          Cell.WordLeft[0] := PosX;
          Cell.WordTop[0] := 0;
          Cell.WordHeight[0] := VertExtents[I].Index;
          Inc(PosX, Cell.WordWidth[0]);
        end;
      end;
      Row.SetBlockExtent(PosX, VertExtents[I].Index);
      Row.WordLeft[0] := 0;
      Row.WordTop[0] := PosY;
      Row.WordHeight[0] := VertExtents[I].Index;
      Row.AddSingleLine;
      AddBlockLine(I, Len, I, Len + Row.ContentLength - 1, 0, PosY, PosX, VertExtents[I].Index);
      Inc(Len, Row.ContentLength);
      Inc(PosY, VertExtents[I].Index);
    end;
    Inc(PosX, FBlockStyle.LeftPadding + FBlockStyle.RightPadding);
    Inc(PosY, FBlockStyle.TopPadding + FBlockStyle.BottomPadding);
    SetBlockExtent(PosX, PosY);
    WordLeft[0] := 0;
    WordTop[0] := 0;
    WordHeight[0] := 0;
  finally
    CalcHorzExtents.Free;
    MeasHorzExtents.Free;
    MinHorzExtents.Free;
    VertExtents.Free;
  end;
  Result := Point(PosX, PosY);
end;

procedure TKMemoTable.RequiredWidthChanged;
var
  I: Integer;
begin
  for I := 0 to RowCount - 1 do
    Rows[I].RequiredWidth := RequiredWidth;
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
    for I := 0 to FBlocks.Count - 1 do
      Rows[I].CellCount := FColCount;
    FColWidths.SetSize(FColCount);
  end;
  if ARowCount <> FBlocks.Count then
  begin
    if ARowCount > FBlocks.Count then
    begin
      for I := FBlocks.Count to ARowCount - 1 do
      begin
        Row := TKMemoTableRow.Create(Blocks);
        Row.CellCount := FColCount;
        Row.RequiredWidth := RequiredWidth;
        for J := 0 to FColCount - 1 do
          if ColWidths[J] <> 0 then
            Row.Cells[J].RequiredWidth := ColWidths[J];
        Blocks.Add(Row);
      end;
    end else
    begin
      for I := ARowCount to FBlocks.Count - 1 do
        Blocks.Delete(FBlocks.Count - 1);
    end;
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

function TKMemoBlocks.AddImageBlock(APath: TKString; At: Integer): TKMemoImageBlock;
begin
  LockUpdate;
  try
    Result := TKMemoImageBlock.Create(Self);
    Result.SetImagePath(APath);
    AddAt(Result, At);
  finally
    UnlockUpdate;
  end;
end;

function TKMemoBlocks.AddParagraph(At: Integer): TKMemoParagraph;
begin
  LockUpdate;
  try
    Result := TKMemoParagraph.Create(Self);
    Result.TextStyle.Assign(GetDefaultTextStyle);
    Result.ParaStyle.Assign(GetDefaultParaStyle);
    AddAt(Result, At);
  finally
    UnlockUpdate;
  end;
end;

function TKMemoBlocks.AddTable(At: Integer): TKMemoTable;
begin
  LockUpdate;
  try
    Result := TKMemoTable.Create(Self);
    AddAt(Result, At);
  finally
    UnlockUpdate;
  end;
end;

function TKMemoBlocks.AddTextBlock(AText: TKString; At: Integer): TKMemoTextBlock;
begin
  LockUpdate;
  try
    Result := TKMemoTextBlock.Create(Self);
    Result.TextStyle.Assign(GetDefaultTextStyle);
    Result.Text := AText;
    AddAt(Result, At);
  finally
    UnlockUpdate;
  end;
end;

procedure TKMemoBlocks.ClearSelection(ATextOnly: Boolean);
var
  I, First, Last, SelectableCnt: Integer;
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
      if ATextOnly and (Item is TKMemoContainer) then
      begin
        TKMemoContainer(Item).ClearSelection(ATextOnly);
      end else
      begin
        if Item.ContentLength = 0 then
          Delete(I)
        else if (Item.SelStart >= 0) and (Item.SelLength > 0) then
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
    // do not leave empty blocks, always add single paragraph
    SelectableCnt := 0;
    for I := 0 to Count - 1 do
    begin
      Item := Items[I];
      if (Item.Position = mbpText) and not (Item is TKMemoContainer) then
        Inc(SelectableCnt);
    end;
    if SelectableCnt = 0 then
      AddParagraph;
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

procedure TKMemoBlocks.FixEOL(AIndex: Integer; AAdjust: Boolean; var ALinePos: TKMemoLinePosition);
var
  Item: TKMemoBlock;
  Line, LocalIndex: Integer;
begin
  if SelLength = 0 then
  begin
    if AAdjust then
      EOLToNormal(AIndex);
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
end;

function TKMemoBlocks.GetBoundsRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

function TKMemoBlocks.GetDefaultTextStyle: TKMemoTextStyle;
begin
  if FParent <> nil then
    Result := FParent.GetDefaultTextStyle
  else if FMemoNotifier <> nil then
    Result := FMemoNotifier.GetDefaultTextStyle
  else
    Result := nil;
end;

function TKMemoBlocks.GetDefaultParaStyle: TKMemoParaStyle;
begin
  if FMemoNotifier <> nil then
    Result := FMemoNotifier.GetDefaultParaStyle
  else if FParent <> nil then
    Result := FParent.DefaultParaStyle
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

function TKMemoBlocks.GetNearestParagraph(AIndex: Integer): TKMemoParagraph;
begin
  Result := nil;
  if AIndex >= 0 then
    while (Result = nil) and (AIndex < Count) do
    begin
      if Items[AIndex] is TKMemoParagraph then
        Result := Items[AIndex] as TKMemoParagraph;
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

function TKMemoBlocks.GetRealSelEnd: Integer;
begin
  if FSelStart <= FSelEnd then
    Result := FSelEnd - FSelStart
  else
    Result := FSelStart - FSelEnd;
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
      Result := Item.InsertParagraph(LocalIndex);
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
      if Item.CanAddText then
      begin
        // we already have suitable block at given location
        Result := Item.InsertString(AValue, LocalIndex);
      end
      else if LocalIndex = 0 then
      begin
        // we are at local position 0 so we can use previous text block or add new one
        if (LastItem <> nil) and LastItem.CanAddText then
        begin
          // insert character at the end of this block
          Result := LastItem.InsertString(AValue);
        end else
        begin
          // insert new text block
          NewItem := AddTextBlock(AValue, Block);
        end;
      end
      else if LocalIndex = Item.ContentLength then
      begin
        // insert new text block
        NewItem := AddTextBlock(AValue, Block + 1);
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
    end;
  finally
    UnlockUpdate;
  end;
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
        Result := RectInRect(ACollisionRect, ARect);
      end;
      Inc(I);
    end;
  end;

  procedure MoveWords(ALineIndex, AStartPos, AEndPos, ADelta: Integer);
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
    Item: TKMemoBlock;
    I, J, W, CurWordCopy, CurBlockCopy, CurIndexCopy, Delta, FirstIndent, LineIndex, LineLeft, LineRight, BaseLine, StPosX, St, En, ParaMarkWidth, BottomPadding, TopPadding: Integer;
    IsParagraph, WasParagraph: Boolean;
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

      // get vertical paddings for this line and width of the paragraph mark (this cannot be included into line width)
      IsParagraph := Items[Line.EndBlock] is TKMemoParagraph;
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
      if IsParagraph then
      begin
        Item := Items[Line.EndBlock];
        BottomPadding := CurParaStyle.BottomPadding;
        ParaMarkWidth := Item.WordWidth[Item.WordCount - 1];
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
      if CurParaStyle.HAlign <> halLeft then
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
                  MoveWords(LineIndex, StPosX, R.Left, Delta);
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
        MoveWords(LineIndex, StPosX, Right + ParaMarkWidth, Delta);
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
      Item := Items[Line.EndBlock];
      if Item is TKMemoParagraph then
      begin
        TKMemoParagraph(Item).Top := ParaPosY;
        TKmemoParagraph(Item).Width := ParaWidth;
        TKmemoParagraph(Item).Height := PosY + LineHeight - ParaPosY - CurParaStyle.BottomPadding;
        ParaWidth := 0;
        ParaPosY := PosY + LineHeight + CurParaStyle.TopPadding;
      end;
      // adjust line extent
      Line.Extent := Point(LineRight - LineLeft, LineHeight);
      Line.Position := Point(LineLeft, PosY);
      // other tasks
      FExtent.X := Max(FExtent.X, LineRight);
      CurParaStyle := GetParaStyle(CurBlock);
      PosX := CurParaStyle.LeftPadding;
      if IsParagraph then
        Inc(PosX, CurParaStyle.FirstIndent);
      Right := ARequiredWidth - CurParaStyle.RightPadding;
      Inc(PosY, LineHeight);
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
        if (PosX + AWordWidth > Right) or CurParaStyle.CancelFloat then
        begin
          if not AddLine then
            Inc(PosY, 5);
          PosX := CurParaStyle.LeftPadding;
        end;
      end;
    end;
  end;

var
  Extent: TPoint;
  WLen: Integer;
  IsParagraph, OutSide, WasParagraph: Boolean;
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
          Extent := Item.MeasureWordExtent(ACanvas, CurWord, ARequiredWidth - NextParaStyle.LeftPadding - NextParaStyle.RightPadding - NextParaStyle.FirstIndent);
          OutSide := CurParaStyle.WordWrap and not IsParagraph and (PosX + Extent.X > Right);
          if OutSide or WasParagraph or CurParaStyle.LineWrap then
            AddLine;
          MoveWordToFreeSpace(Extent.X, Extent.Y);
          Item.WordLeft[CurWord] := PosX;
          Item.WordTop[CurWord] := PosY;
          Inc(PosX, Extent.X);
          LineHeight := Max(LineHeight, Extent.Y);
          Inc(CurIndex, WLen);
          Inc(CurWord);
          Inc(CurTotalWord);
          FExtent.Y := Max(FExtent.Y, PosY + Extent.Y);
        end;
      end;
      mbpRelative:
      begin
        if IsParagraph then
          AddLine;
        Item.WordLeft[0] := PosX;
        Item.WordTop[0] := PosY;
        FExtent.X := Max(FExtent.X, PosX + Item.Width);
        FExtent.Y := Max(FExtent.Y, PosY + Item.Height);
      end;
    end;
    Inc(CurBlock);
  end;
  if CurIndex > LastIndex then
    AddLine;
end;

function TKMemoBlocks.NextIndexByCharCount(AIndex, ACharCount: Integer): Integer;
begin
  Result := AIndex + ACharCount;
end;

function TKMemoBlocks.NextIndexByHorzExtent(ACanvas: TCanvas; AIndex, AWidth: Integer; out ALinePos: TKMemoLinePosition): Integer;
var
  R: TRect;
begin
  R := IndexToRect(ACanvas, AIndex, True, EOLToNormal(AIndex));
  Result := PointToIndex(ACanvas, Point(R.Left + AWidth, R.Top), True, False, ALinePos);
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
begin
  R := IndexToRect(ACanvas, AIndex, True, EOLToNormal(AIndex));
  Result := PointToIndex(ACanvas, Point(ALeftPos, R.Top + AHeight), True, False, ALinePos);
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
  PA, OldPA: TKMemoParagraph;
begin
  OldPa := nil;
  // paint text blocks
  for I := 0 to LineCount - 1 do
  begin
    if (LineBottom[I] + ATop >= ARect.Top) and (LineTop[I] + ATop < ARect.Bottom) then
    begin
      // fill areas under paragraphs
      PA := GetNearestParagraph(FLines[I].StartBlock);
      if (PA <> nil) and (PA <> OldPA) then
      begin
        R := Rect(ALeft, PA.Top + ATop, ALeft + Max(FRequiredWidth, PA.Width), PA.Top + PA.Height + ATop);
        PA.ParaStyle.PaintBox(ACanvas, R);
        OldPA := PA;
      end;
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
    end else
    asm
      nop
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
  I : Integer;
begin
  Result := -1;
  if LineCount > 0 then
  begin
    I := 0;
    while (Result < 0) and (I < LineCount) do
    begin
      if (APoint.Y >= LineTop[I]) and (APoint.Y < LineBottom[I]) or
        AOutOfArea and ((I = 0) and (APoint.Y < LineTop[I]) or (I = LineCount - 1) and (APoint.Y >= LineBottom[I])) then
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
  NewSelEnd := MinMax(NewSelEnd, 0, MaxIndex);
  ASelStart := MinMax(ASelStart, 0, MaxIndex);
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
begin
  if (ALineIndex >= 0) and (ALineIndex < LineCount) then
  begin
    LockUpdate;
    try
      // TODO
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKMemoBlocks.SetText(const AValue: TKString);
var
  I, Ln, St: Integer;
  S: TKString;
begin
  LockUpdate;
  try
    St := 1;
    I := 1;
    Ln := Length(Avalue);
    while I < Ln do
    begin
      if AValue[I] = cFirstEOL then
      begin
        if I > St then
        begin
          S := Copy(AValue, St, I - St);
          AddTextBlock(S);
        end;
        AddParagraph;
        St := I + 1;
      end
      else if CharInSetEx(AValue[I], cLineBreaks) then
        Inc(St);
      Inc(I);
    end;
    if I > St then
    begin
      S := Copy(AValue, St, I - St + 1);
      AddTextBlock(S);
      AddParagraph;
    end;
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
          FRelPos.AddItem(I);
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

end.

