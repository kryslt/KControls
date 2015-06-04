{ @abstract(This unit contains native replacement for TMemo/TRichEdit component)
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

  { This is the paragraph character. }
  cNewLineChar = #$B6;

type
  TKCustomMemo = class;

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
    { Attributes changed that do not affect extent and content. }
    muAttributes,
    { recalculate extent. }
    muExtent,
    { recalculate line info and extent. }
    muContent,
    { selection changed. }
    muSelection,
    { selection changed and scroll operation is required to reflect the change. }
    muSelectionScroll
  );

  TKMemoUpdateReasons = set of TKMemoUpdateReason;

  TKMemoLine = record
    StartBlock: Integer;
    EndBlock: Integer;
    StartIndex: Integer;
    EndIndex: Integer;
    Bounds: TRect;
  end;

  PKMemoLine = ^TKMemoLine;

  TKMemoLines = array of TKMemoLine;

  TKMemoBlocks = class;

  TKMemoBlock = class(TObject)
  private
    FBounds: TRect;
    FParent: TKMemoBlocks;
    FSelEnd: Integer;
    FSelStart: Integer;
    procedure SetBounds(const Value: TRect);
    procedure SetParent(AParent: TKMemoBlocks);
  protected
    function ContentLength: Integer; virtual;
    function GetCanAddText: Boolean; virtual;
    procedure GetSelColors(var TextColor, Background: TColor); virtual;
    function GetSelLength: Integer; virtual;
    function GetSelStart: Integer; virtual;
    function GetSelText: TKString; virtual;
    function GetText: TKString; virtual;
    procedure NotifyLineInfo(const ALineInfo: TKMemoLine); virtual;
    procedure ParentChanged; virtual;
    procedure SetOffset(ALeft, ATop: Integer); virtual;
    function Select(ASelStart, ASelLength: Integer): Boolean; virtual;
    function SelectableLength: Integer; virtual;
    procedure Update(AReasons: TKMemoUpdateReasons); virtual;
  public
    constructor Create(AParent: TKMemoBlocks); virtual;
    function AddText(const AText: TKString; At: Integer = -1): Boolean; virtual;
    procedure Assign(AItem: TKMemoBlock); virtual;
    procedure AssignAttributes(AItem: TKMemoBlock); virtual;
    procedure ClearSelection; virtual;
    function Concat(AItem: TKMemoBlock): Boolean; virtual;
    function IndexToRect(ACanvas: TCanvas; AIndex: Integer): TRect; virtual;
    procedure MeasureExtent(ACanvas: TCanvas); virtual;
    procedure NotifyFontChange(AFont: TFont); virtual;
    procedure PaintToCanvas(ACanvas: TCanvas); virtual;
    function PointToIndex(ACanvas: TCanvas; const APoint: TPoint): Integer; virtual;
    function ShowFormatting: Boolean; virtual;
    function Split(At: Integer): TKMemoBlock; virtual;
    property Bounds: TRect read FBounds write SetBounds;
    property CanAddText: Boolean read GetCanAddText;
    property Parent: TKMemoBlocks read FParent write SetParent;
    property SelLength: Integer read GetSelLength;
    property SelStart: Integer read GetSelStart;
    property SelText: TKString read GetSelText;
    property Text: TKString read GetText;
  end;

  TKTextMemoBlock = class(TKMemoBlock)
  private
    FBrush: TBrush;
    FFont: TFont;
    FHAlign: TKHAlign;
    FText: TKString;
    FTextAttributes: TKTextAttributes;
    FVAlign: TKVAlign;
    procedure SetHAlign(const Value: TKHAlign);
    procedure SetVAlign(const Value: TKVAlign);
    procedure SplitText(At: Integer; out APart1, APart2: TKString);
  protected
    FFontChanged: Boolean;
    { Because of time optimization. }
    FContentChanged: Boolean;
    FHeight: Integer;
    { Because of time optimization. }
    FTextLength: Integer;
    FWidth: Integer;
    function ContentLength: Integer; override;
    procedure FontChange(Sender: TObject); virtual;
    function GetCanAddText: Boolean; override;
    function GetSelText: TKString; override;
    function GetText: TKString; override;
    function IndexToTextIndex(var AText: TKString; AIndex: Integer): Integer; virtual;
    procedure ParentChanged; override;
    procedure SetText(const Value: TKString); virtual;
    procedure SetTextAttributes(const Value: TKTextAttributes); virtual;
    function TextIndexToIndex(var AText: TKString; ATextIndex: Integer): Integer; virtual;
    function TextLength: Integer; virtual;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    function AddText(const AText: TKString; At: Integer): Boolean; override;
    procedure Assign(AItem: TKMemoBlock); override;
    procedure AssignAttributes(AItem: TKMemoBlock); override;
    procedure ClearSelection; override;
    function Concat(AItem: TKMemoBlock): Boolean; override;
    function IndexToRect(ACanvas: TCanvas; AIndex: Integer): TRect; override;
    procedure MeasureExtent(ACanvas: TCanvas); override;
    procedure NotifyFontChange(AFont: TFont); override;
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    function PointToIndex(ACanvas: TCanvas; const APoint: TPoint): Integer; override;
    function Split(At: Integer): TKMemoBlock; override;
    property Brush: TBrush read FBrush;
    property Font: TFont read FFont;
    property HAlign: TKHAlign read FHAlign write SetHAlign;
    property Text: TKString read FText write SetText;
    property TextAttributes: TKTextAttributes read FTextAttributes write SetTextAttributes;
    property VAlign: TKVAlign read FVAlign write SetVAlign;
  end;

  TKImageMemoBlock = class(TKMemoBlock)
  private
    FImage: TPicture;
    procedure SetImage(const Value: TPicture);
    procedure SetImagePath(const Value: TKString);
  protected
    function ContentLength: Integer; override;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    procedure Assign(AItem: TKMemoBlock); override;
    function IndexToRect(ACanvas: TCanvas; AIndex: Integer): TRect; override;
    procedure MeasureExtent(ACanvas: TCanvas); override;
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    function PointToIndex(ACanvas: TCanvas; const APoint: TPoint): Integer; override;
    property Image: TPicture read FImage write SetImage;
    property Path: TKString write SetImagePath;
  end;

  TKNewLineMemoBlock = class(TKTextMemoBlock)
  private
  protected
    function GetCanAddText: Boolean; override;
    procedure SetText(const Value: TKString); override;
    procedure SetTextAttributes(const Value: TKTextAttributes); override;
    function SelectableLength: Integer; override;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    function AddText(const AText: TKString; At: Integer): Boolean; override;
    procedure ClearSelection; override;
    function Concat(AItem: TKMemoBlock): Boolean; override;
    function IndexToRect(ACanvas: TCanvas; AIndex: Integer): TRect; override;
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    function Split(At: Integer): TKMemoBlock; override;
  end;

  TKMemoBlocks = class(TObjectList)
  private
    FExtentX: Integer;
    FExtentY: Integer;
    FLineCount: Integer;
    FMemo: TKCustomMemo;
    FSelectableLength: Integer;
    FSelEnd: Integer;
    FSelStart: Integer;
    function GetRealSelEnd: Integer;
    function GetRealSelStart: Integer;
    function GetSelLength: Integer;
    function GetEmpty: Boolean;
  protected
    FLines: TKMemoLines;
    FUpdateLock: Integer;
    FUpdateReasons: TKMemoUpdateReasons;
    function AddLine: PKMemoLine;
    function GetDefaultFont: TFont; virtual;
    function GetLineBottom(AIndex: Integer): Integer; virtual;
    function GetLineEndIndex(AIndex: Integer): Integer; virtual;
    function GetLineInfo(AIndex: Integer): PKMemoLine;
    function GetLineLeft(AIndex: Integer): Integer; virtual;
    function GetLineRight(AIndex: Integer): Integer; virtual;
    function GetLines(AIndex: Integer): TKString; virtual;
    function GetLineSize(AIndex: Integer): Integer; virtual;
    function GetLineStartIndex(AIndex: Integer): Integer; virtual;
    function GetLineTop(AIndex: Integer): Integer; virtual;
    procedure GetSelColors(var TextColor, Background: TColor); virtual;
    function GetSelText: TKString; virtual;
    function GetText: TKString; virtual;
    function LineToRect(ACanvas: TCanvas; AIndex, ALine: Integer; ACaret: Boolean): TRect; virtual;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean = True): Boolean; virtual;
    procedure SetLines(AIndex: Integer; const AValue: TKString); virtual;
    procedure SetText(const AValue: TKString);
    procedure Update(AReasons: TKMemoUpdateReasons); virtual;
    procedure UpdateLineInfo; virtual;
  public
    constructor Create(AMemo: TKCustomMemo); virtual;
    destructor Destroy; override;
    function AddAt(AObject: TKMemoBlock; At: Integer = -1): Integer;
    function AddImageBlock(APath: TKString; At: Integer = -1): TKImageMemoBlock;
    function AddNewLineBlock(At: Integer = -1): TKNewLineMemoBlock;
    function AddTextBlock(AText: TKString; At: Integer = -1): TKTextMemoBlock;
    procedure ClearSelection; virtual;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function IndexToBlock(AIndex: Integer; out ALocalIndex: Integer): Integer; virtual;
    function IndexToLine(AIndex: Integer): Integer; virtual;
    function IndexToRect(ACanvas: TCanvas; AIndex: Integer; ACaret: Boolean): TRect; virtual;
    function InsertNewLine(AIndex: Integer): Boolean; virtual;
    function InsertString(AIndex: Integer; const AValue: TKString): Boolean; virtual;
    function LineToIndex(ALineIndex: Integer): Integer; virtual;
    procedure LockUpdate;
    procedure MeasureExtent(ACanvas: TCanvas; ALeft, ATop: Integer); virtual;
    function ModifiedLineStartIndex(AIndex: Integer; ACaret: Boolean): Integer; virtual;
    function ModifiedLineEndIndex(AIndex: Integer; ACaret: Boolean): Integer; virtual;
    procedure NotifyFontChange(AFont: TFont); virtual;
    function NextIndexByCharCount(AIndex, ACharCount: Integer; ACaret: Boolean): Integer;
    function NextIndexByHorzExtent(ACanvas: TCanvas; AIndex, AWidth: Integer; ACaret: Boolean): Integer; virtual;
    function NextIndexByRowDelta(ACanvas: TCanvas; AIndex, ARowDelta, ALeftPos: Integer; ACaret: Boolean): Integer; virtual;
    function NextIndexByVertExtent(ACanvas: TCanvas; AIndex, AHeight, ALeftPos: Integer; ACaret: Boolean): Integer; virtual;
    function NextIndexByVertValue(ACanvas: TCanvas; AIndex, AValue, ALeftPos: Integer;
      ADirection, ACaret: Boolean): Integer; virtual;
    procedure PaintToCanvas(ACanvas: TCanvas; AWidth, AHeight: Integer); virtual;
    function PointToIndex(ACanvas: TCanvas; const APoint: TPoint; AOutOfArea, ACaret: Boolean): Integer; virtual;
    function PointToLineIndex(ACanvas: TCanvas; ALine: Integer; const APoint: TPoint;
      AOutOfArea, ACaret: Boolean): Integer; virtual;
    function Remove(AObject: TKMemoBlock): Integer; overload;
    function ShowFormatting: Boolean; virtual;
    procedure UnlockUpdate;
    function UpdateUnlocked: Boolean;
    property DefaultFont: TFont read GetDefaultFont;
    property Empty: Boolean read GetEmpty;
    property ExtentX: Integer read FExtentX;
    property ExtentY: Integer read FExtentY;
    property LineBottom[Index: Integer]: Integer read GetLineBottom;
    property LineCount: Integer read FLineCount;
    property LineEndIndex[Index: Integer]: Integer read GetLineEndIndex;
    property LineInfo[Index: Integer]: PKMemoLine read GetLineInfo;
    property LineLeft[Index: Integer]: Integer read GetLineLeft;
    property LineRight[Index: Integer]: Integer read GetLineRight;
    property LineTop[Index: Integer]: Integer read GetLineTop;
    property Lines[Index: Integer]: TKString read GetLines write SetLines;
    property LineSize[Index: Integer]: Integer read GetLineSize;
    property LineStartIndex[Index: Integer]: Integer read GetLineStartIndex;
    property RealSelEnd: Integer read GetRealSelEnd;
    property RealSelStart: Integer read GetRealSelStart;
    property SelectableLength: Integer read FSelectableLength;
    property SelEnd: Integer read FSelEnd;
    property SelLength: Integer read GetSelLength;
    property SelStart: Integer read FSelStart;
    property SelText: TKString read GetSelText;
    property Text: TKString read GetText write SetText;
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
  TKCustomMemo = class(TKCustomControl)
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
    FRedoList: TKMemoChangeList;
    FScrollBars: TScrollStyle;
    FScrollPadding: Integer;
    FScrollSpeed: Cardinal;
    FScrollTimer: TTimer;
    FStates: TKMemoStates;
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
    { Called to reflect block changes. }
    procedure BlocksChanged(AReasons: TKMemoUpdateReasons); virtual;
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
    { Overriden method - handles mouse wheel messages. }
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    { Performs the undo command. }
    procedure DoUndo; virtual;
    { Closes the undo group created by @link(TKCustomMemo.BeginUndoGroup). }
    procedure EndUndoGroup;
    { Notify blocks about memo font change. }
    procedure FontChange(Sender: TObject); virtual;
    { Returns "real" selection end - with always higher index value than selection start value. }
    function GetRealSelEnd: Integer; virtual;
    { Returns "real" selection start - with always lower index value than selection end value. }
    function GetRealSelStart: Integer; virtual;
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
    { Grants the input focus to the control when possible and the control has had none before. }
    procedure SafeSetFocus;
    { Scrolls the text either horizontally by DeltaHorz scroll units or vertically
      by DeltaVert scroll units (lines) or in both directions. CodeHorz and CodeVert
      are the codes coming from WM_HSCROLL or WM_VSCROLL messages. }
    function Scroll(CodeHorz, CodeVert, DeltaHorz, DeltaVert: Integer; CallScrollWindow: Boolean): Boolean;
    { Scrolls the memo window horizontaly by DeltaHorz scroll units and/or
      vertically by DeltaVert scroll units (lines). }
    procedure ScrollBy(DeltaHorz, DeltaVert: Integer);
    { Determines if a cell specified by ACol and ARow should be scrolled, i.e. is
      not fully visible. }
    function ScrollNeeded(out DeltaCol, DeltaRow: Integer): Boolean; virtual;
    { Scrolls the memo so that caret will be in the center of client area. }
    procedure ScrollToClientAreaCenter;
    { Expands the current selection and performs all necessary adjustments. }
    procedure SelectionExpand(ASelEnd: Integer; ADoScroll: Boolean = True); overload; virtual;
    { Expands the current selection and performs all necessary adjustments. }
    procedure SelectionExpand(const APoint: TPoint; ADoScroll: Boolean = True); overload; virtual;
    { Initializes the current selection and performs all necessary adjustments. }
    procedure SelectionInit(ASelStart: Integer; ADoScroll: Boolean = True); overload; virtual;
    { Initializes the current selection and performs all necessary adjustments. }
    procedure SelectionInit(const APoint: TPoint; ADoScroll: Boolean = True); overload; virtual;
    { Updates mouse cursor according to the state determined from current mouse
      position. Returns True if cursor has been changed. }
    function SetMouseCursor(X, Y: Integer): Boolean; override;
    { Shows the caret. }
    procedure ShowEditorCaret; virtual;
    { Performs necessary adjustments if the text is modified programatically
      (not by user). }
    procedure TextChanged; virtual;
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
    { Deletes blocks or parts of blocks corresponding to the active selection. }
    procedure ClearSelection; virtual;
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
      <LI><I>ACaret</I> - return caret rectangle instead</LI>
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
    { Converts client area coordinates into a text buffer index.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>APoint</I> - window client area coordinates</LI>
      <LI><I>AOutOfArea</I> - set to True to compute selection even if the
      the supplied coordinates are outside of the text space</LI>
      <LI><I>ACaret</I> - set to True to compute caret index rather than selection index if AOutOfArea is True</LI>
      </UL> }
    function PointToIndex(APoint: TPoint; AOutOfArea, ACaret: Boolean): Integer; virtual;
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
    { Specifies whether the editor has to be read only editor. }
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    { Returns "real" selection end - with always higher index value than selection start value. }
    property RealSelEnd: Integer read GetRealSelEnd;
    { Returns "real" selection start - with always lower index value than selection end value. }
    property RealSelStart: Integer read GetRealSelStart;
    { Defines visible scrollbars - horizontal, vertical or both. }
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    { Specifies how fast the scrolling by timer should be. }
    property ScrollSpeed: Cardinal read FScrollSpeed write SetScrollSpeed default cScrollSpeedDef;
    { Specifies the padding in pixels to overscroll to show caret position or selection end. }
    property ScrollPadding: Integer read FScrollPadding write SetScrollPadding default cScrollPaddingDef;
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
    { Specifies the vertical scroll position. }
    property TopPos: Integer read FTopPos write SetTopPos;
    { Specifies the maximum number of undo items. Please note this value
      affects the undo item limit, not undo group limit. }
    property UndoLimit: Integer read GetUndoLimit write SetUndoLimit default cUndoLimitDef;
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

implementation

uses
{$IFDEF USE_WINAPI}
  ShellApi,
{$ENDIF}
  ClipBrd, Printers,
{$IFDEF USE_THEMES}
  Themes,
{$ENDIF}
  Math, Types;

function OppositeKind(ItemKind: TKMemoChangeKind): TKMemoChangeKind;
begin
  case ItemKind of
    ckDelete: Result := ckInsert;
    ckInsert: Result := ckDelete;
  else
    Result := ItemKind;
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
  FBlocks := TKMemoBlocks.Create(Self);
  FCaretRect := CreateEmptyRect;
  FOldCaretRect := CreateEmptyRect;
  FColors := TKMemoColors.Create(Self);
  FContentPadding := TKRect.Create;
  FContentPadding.OnChanged := ContentPaddingChanged;
  FDisabledDrawStyle := cDisabledDrawStyleDef;
  FHorzScrollStep := cHorzScrollStepDef;
  FLeftPos := 0;
  FMouseWheelAccumulator := 0;
  FOptions := [eoGroupUndo];
  FPreferredCaretPos := 0;
  FKeyMapping := TKEditKeyMapping.Create;
  FRedoList := TKMemoChangeList.Create(Self, nil);
  FScrollBars := ssBoth;
  FScrollPadding := cScrollPaddingDef;
  FScrollSpeed := cScrollSpeedDef;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := FScrollSpeed;
  FScrollTimer.OnTimer := ScrollTimerHandler;
  FStates := [];
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
  FKeyMapping.Free;
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

procedure TKCustomMemo.BlocksChanged(AReasons: TKMemoUpdateReasons);
begin
  if HandleAllocated then
  begin
    if AReasons * [muContent, muExtent] <> [] then
      UpdateScrollRange(True)
    else if muSelectionScroll in AReasons then
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
  end;
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
      R := IndexToRect(SelEnd, False);
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
  Result := PtInRect(ClientRect, IndexToRect(SelEnd, False).TopLeft);
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
  FBlocks.LockUpdate;
  try
    FBlocks.Select(0, 0);
    FBlocks.Clear;
  finally
    FBlocks.UnlockUpdate;
  end;
end;

procedure TKCustomMemo.ClearSelection;
begin
  FBlocks.ClearSelection;
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
        Result := TmpSelEnd < FBlocks.SelectableLength - 2;
      ecSelRight:
        Result := TmpSelEnd < FBlocks.SelectableLength - 1;
      ecUp, ecSelUp, ecPageUp, ecSelPageUp:
        Result := TmpSelEnd >= FBlocks.LineSize[0];
      ecDown, ecPagedown:
        Result := TmpSelEnd < FBlocks.SelectableLength - FBlocks.LineSize[FBlocks.LineCount - 1] - 1;
      ecSelDown, ecSelPageDown:
        Result := TmpSelEnd < FBlocks.SelectableLength - FBlocks.LineSize[FBlocks.LineCount - 1];
      ecLineStart, ecPageLeft:
        Result := TmpSelEnd > FBlocks.ModifiedLineStartIndex(TmpSelEnd, True);
      ecSelLineStart, ecSelPageLeft:
        Result := TmpSelEnd > FBlocks.ModifiedLineStartIndex(TmpSelEnd, False);
      ecLineEnd, ecPageRight:
        Result := TmpSelEnd < FBlocks.ModifiedLineEndIndex(TmpSelEnd, True);
      ecSelLineEnd, ecSelPageRight:
        Result := TmpSelEnd < FBlocks.ModifiedLineEndIndex(TmpSelEnd, False);
      ecPageTop, ecSelPageTop:
        Result := TmpSelEnd <> FBlocks.NextIndexByVertValue(Canvas, TmpSelEnd, 0, FPreferredCaretPos, False, Command = ecPageTop);
      ecPageBottom, ecSelPageBottom:
        Result := TmpSelEnd <> FBlocks.NextIndexByVertValue(Canvas, TmpSelEnd, ClientHeight, FPreferredCaretPos, True,
          Command = ecPageBottom);
      ecEditorTop, ecSelEditorTop:
        Result := TmpSelEnd > 0;
      ecEditorBottom:
        Result := TmpSelEnd < FBlocks.SelectableLength - 2;
      ecSelEditorBottom:
        Result := TmpSelEnd < FBlocks.SelectableLength - 1;
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
        Result := not (Empty or ReadOnly) and ((TmpSelLength > 0) or (TmpSelEnd <> FBlocks.LineStartIndex[FBlocks.IndexToLine(TmpSelEnd)]));
      ecDeleteEOL:
        Result := not (Empty or ReadOnly) and ((TmpSelLength > 0) or (TmpSelEnd <> FBlocks.LineEndIndex[FBlocks.IndexToLine(TmpSelEnd)]));
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
var
  LineStart: Integer;
begin
  BeginUndoGroup(ckDelete);
  FBlocks.LockUpdate;
  try
    LineStart := FBlocks.LineStartIndex[FBlocks.IndexToLine(At)];
    FBlocks.Select(LineStart, At - LineStart);
    FBlocks.ClearSelection;
  finally
    FBlocks.UnlockUpdate;
    EndUndoGroup;
  end;
end;

procedure TKCustomMemo.DeleteChar(At: Integer);
var
  NextIndex: Integer;
begin
  BeginUndoGroup(ckDelete);
  FBlocks.LockUpdate;
  try
    NextIndex := FBlocks.NextIndexByCharCount(At, 1, True);
    FBlocks.Select(At, NextIndex - At);
    FBlocks.ClearSelection;
  finally
    FBlocks.UnlockUpdate;
    EndUndoGroup;
  end;
end;

procedure TKCustomMemo.DeleteEOL(At: Integer);
var
  LineEnd: Integer;
begin
  BeginUndoGroup(ckDelete);
  FBlocks.LockUpdate;
  try
    LineEnd := FBlocks.LineEndIndex[FBlocks.IndexToLine(At)];
    FBlocks.Select(LineEnd, At - LineEnd);
    FBlocks.ClearSelection;
  finally
    FBlocks.UnlockUpdate;
    EndUndoGroup;
  end;
end;

procedure TKCustomMemo.DeleteLastChar(At: Integer);
var
  LastIndex: Integer;
begin
  BeginUndoGroup(ckDelete);
  FBlocks.LockUpdate;
  try
    LastIndex := FBlocks.NextIndexByCharCount(At, -1, True);
    FBlocks.Select(LastIndex, At - LastIndex);
    FBlocks.ClearSelection;
  finally
    FBlocks.UnlockUpdate;
    EndUndoGroup;
  end;
end;

procedure TKCustomMemo.DeleteLine(At: Integer);
var
  Line, LineStart, LineEnd: Integer;
begin
  BeginUndoGroup(ckDelete);
  FBlocks.LockUpdate;
  try
    AddUndoCaretPos;
    Line := FBlocks.IndexToLine(At);
    LineStart := FBlocks.LineStartIndex[Line];
    if Line < FBlocks.LineCount - 1 then
      LineEnd := FBlocks.LineStartIndex[Line + 1]
    else
      LineEnd := FBlocks.LineEndIndex[Line];
    FBlocks.Select(LineStart, LineEnd - LineStart);
    FBlocks.ClearSelection;
  finally
    FBlocks.UnlockUpdate;
    EndUndoGroup;
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
    ScrollBy(0, - WheelClicks * LinesToScroll);
    Result := True;
  end;
end;

procedure TKCustomMemo.DoUndo;
var
  PChI, PChI_First, PChI_Next: PKMemoChangeItem;
begin
  PChI := FUndoList.PeekItem;
  PChI_First := PChI;
  while PChI <> nil do
  begin
{    I := Length(PChI.Data);
    J := Min(I, FSize - PChI.SelEnd.Index);
    FRedoList.SetGroupData(PChI.Group, PChI.GroupKind);
    case PChI.ItemKind of
      ckCaretPos:
        FRedoList.AddChange(ckCaretPos, '');
      crDeleteChar, crDeleteDigits, crDeleteString:
      begin
        if FBuffer <> nil then
        begin
          SetLength(S, J);
          System.Move(FBuffer[PChI.SelEnd.Index], S[1], J);
        end else
          S := '';
        FRedoList.AddChange(OppositeKind(PChI.ItemKind), S, PChI.Inserted);
      end;
      crInsertChar, crInsertDigits, crInsertString:
        FRedoList.AddChange(OppositeKind(PChI.ItemKind), PChI.Data);
    end;
    FSelEnd := PChI.SelEnd;
    FSelStart := PChI.Position;
    FEditArea := PChI.EditArea;
    case PChI.ItemKind of
      crDeleteChar, crDeleteDigits, crDeleteString:
      begin
        if PChI.Inserted then
          ClearString(PChI.SelEnd.Index, I)
        else if FBuffer <> nil then
        begin
          System.Move(PChI.Data[1], FBuffer[PChI.SelEnd.Index], J);
          Invalidate;
        end;
      end;
      crInsertChar, crInsertDigits, crInsertString:
        InsertString(GetRealSelStart.Index, PChI.Data, I);
    end;
    EditAreaChanged;
    SelectionChanged(False, False);
    if PChI.ItemKind <> ckCaretPos then
      DoChange;}
    PChI_Next := FUndoList.PeekItem;
{    if (PChI_Next <> nil) and not ((PChI.Group <> 0) and (PChI.Group = PChI_Next.Group) or
      (eoGroupUndo in FOptions) and (PChI_First.GroupKind = PChI_Next.GroupKind)) then
    begin
      FUndoList.PokeItem;
      Break;
    end;}
    PChI := PChI_Next;
  end;
  if not CaretInView then
    ExecuteCommand(ecScrollCenter);
end;

procedure TKCustomMemo.EndUndoGroup;
begin
  FUndoList.EndGroup;
end;

function TKCustomMemo.ExecuteCommand(Command: TKEditCommand; Data: Pointer): Boolean;
var
{  I, J, K, M, N, O: Integer;
  CanInsert, MoreBytes, Found, PAbort, MatchCase: Boolean;
  C1, C2, C3: Char;
  S, S_FirstChar, S_LastChar, T: TKString;
  BA: PByteArray;
  P: TPoint;
  L, OldSelStart, OldSelEnd, Sel1, Sel2: Integer;
  PChI, PChI_First, PChI_Next: PKMemoChangeItem;
  ReplaceAction: TKEditReplaceAction;
  H: THandle;}
  TmpSelEnd, TmpSelLength, TmpSelStart: Integer;
begin
  Result := False;
  if CommandEnabled(Command) then
  begin
    Result := True;
    TmpSelEnd := SelEnd;
    TmpSelStart := SelStart;
    TmpSelLength := SelLength;
    case Command of
      ecLeft: SelectionInit(FBlocks.NextIndexByCharCount(TmpSelEnd, - 1, True));
      ecSelLeft: SelectionExpand(FBlocks.NextIndexByCharCount(TmpSelEnd, - 1, False));
      ecRight: SelectionInit(FBlocks.NextIndexByCharCount(TmpSelEnd, 1, True));
      ecSelRight: SelectionExpand(FBlocks.NextIndexByCharCount(TmpSelEnd, 1, False));
      ecUp: SelectionInit(FBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, -1, FPreferredCaretPos, True));
      ecSelUp: SelectionExpand(FBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, -1, FPreferredCaretPos, False));
      ecDown: SelectionInit(FBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, 1, FPreferredCaretPos, True));
      ecSelDown: SelectionExpand(FBlocks.NextIndexByRowDelta(Canvas, TmpSelEnd, 1, FPreferredCaretPos, False));
      ecLineStart: SelectionInit(FBlocks.ModifiedLineStartIndex(TmpSelEnd, True));
      ecSelLineStart: SelectionExpand(FBlocks.ModifiedLineStartIndex(TmpSelEnd, False));
      ecLineEnd: SelectionInit(FBlocks.ModifiedLineEndIndex(TmpSelEnd, True));
      ecSelLineEnd: SelectionExpand(FBlocks.ModifiedLineEndIndex(TmpSelEnd, False));
      ecPageUp: SelectionInit(FBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, -ClientHeight, FPreferredCaretPos, True));
      ecSelPageUp: SelectionExpand(FBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, -ClientHeight, FPreferredCaretPos, False));
      ecPageDown: SelectionInit(FBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, ClientHeight, FPreferredCaretPos, True));
      ecSelPageDown: SelectionExpand(FBlocks.NextIndexByVertExtent(Canvas, TmpSelEnd, ClientHeight, FPreferredCaretPos, False));
      ecPageLeft: SelectionInit(FBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, -ClientWidth, True));
      ecSelPageLeft: SelectionExpand(FBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, -ClientWidth, False));
      ecPageRight: SelectionInit(FBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, ClientWidth, True));
      ecSelPageRight: SelectionExpand(FBlocks.NextIndexByHorzExtent(Canvas, TmpSelEnd, ClientWidth, False));
      ecPageTop: SelectionInit(FBlocks.NextIndexByVertValue(Canvas, TmpSelEnd, 0, FPreferredCaretPos, False, True));
      ecSelPageTop: SelectionExpand(FBlocks.NextIndexByVertValue(Canvas, TmpSelEnd, 0, FPreferredCaretPos, False, False));
      ecPageBottom: SelectionInit(FBlocks.NextIndexByVertValue(Canvas, TmpSelEnd, ClientHeight, FPreferredCaretPos, True, True));
      ecSelPageBottom: SelectionExpand(FBlocks.NextIndexByVertValue(Canvas, TmpSelEnd, ClientHeight, FPreferredCaretPos, True, False));
      ecEditorTop: SelectionInit(0);
      ecSelEditorTop: SelectionExpand(0);
      ecEditorBottom: SelectionInit(FBlocks.SelectableLength - 2);
      ecSelEditorBottom: SelectionExpand(FBlocks.SelectableLength - 1);
      ecGotoXY: SelectionInit(FBlocks.PointToIndex(Canvas, PPoint(Data)^, True, True));
      ecSelGotoXY: SelectionExpand(FBlocks.PointToIndex(Canvas, PPoint(Data)^, True, False));
      // scroll commands
      ecScrollUp:
      if not ClampInView(True) then
      begin
        ScrollBy(0, -1);
        while CommandEnabled(ecUp) and (FBlocks.LineBottom[FBlocks.IndexToLine(TmpSelEnd)] > ClientHeight) do
          ExecuteCommand(ecUp);
      end;
      ecScrollDown:
      if not ClampInView(True) then
      begin
        ScrollBy(0, 1);
        while CommandEnabled(ecDown) and (FBlocks.LineTop[FBlocks.IndexToLine(TmpSelEnd)] < 0) do
          ExecuteCommand(ecDown);
      end;
      ecScrollLeft:
      if not ClampInView(True) then
      begin
        ScrollBy(-1, 0);
        while CommandEnabled(ecLeft) and (FCaretRect.Left + FCaretRect.Right > ClientWidth) do
          ExecuteCommand(ecLeft);
      end;
      ecScrollRight:
      if not ClampInView(True) then
      begin
        ScrollBy(1, 0);
        while CommandEnabled(ecRight) and (FCaretRect.Left < 0) do
          ExecuteCommand(ecRight);
      end;
      ecScrollCenter: ScrollToClientAreaCenter;
      // editing commands
      ecUndo: DoUndo;
{      ecRedo:
      begin
        PChI := FRedoList.PeekItem;
        PChI_First := PChI;
        while PChI <> nil do
        begin
          FUndoList.PokeItem;
          I := Length(PChI.Data);
          Sel1 := GetRealSelStart;
          case PChI.ItemReason of
            crInsertChar, crInsertDigits, crInsertString:
            begin
              if PChI.Inserted then
                InsertString(Sel1.Index, PChI.Data, I)
              else if FBuffer <> nil then
              begin
                System.Move(PChI.Data[1], FBuffer[Sel1.Index], Min(I, FSize - TmpSelEnd.Index));
                Invalidate;
              end;
            end;
            crDeleteChar, crDeleteDigits, crDeleteString:
              ClearString(Sel1.Index, I);
          end;
          TmpSelEnd := PChI.SelEnd;
          FSelStart := PChI.SelStart;
          FEditArea := PChI.EditArea;
          EditAreaChanged;
          SelectionChanged(False, False);
          if PChI.ItemReason <> crCaretPos then
            DoChange;
          PChI_Next := FRedoList.PeekItem;
          if (PChI_Next <> nil) and not ((PChI.Group <> 0) and (PChI.Group = PChI_Next.Group) or
            (eoGroupUndo in FOptions) and (PChI_First.GroupReason = PChI_Next.GroupReason)) then
          begin
            FRedoList.PokeItem;
            Break;
          end;
          PChI := PChI_Next;
        end;
        if not CaretInView then
          ExecuteCommand(ecScrollCenter);
      end;
      ecCopy:
      begin
        Sel1 := GetRealSelStart;
        Sel2 := GetRealSelEnd;
        if FEditArea = eaDigits then
          ClipBoard.AsText := BinaryToDigits(FBuffer, Sel1, Sel2)
        else if L.Index <> 0 then
        begin
          S := BinaryToText(FBuffer, Sel1.Index, Sel2.Index, @FCharMapping);
          H := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, L.Index);
          try
            BA := GlobalLock(H);
            try
              System.Move(FBuffer[Sel1.Index], BA^, L.Index);
            finally
              GlobalUnlock(H);
            end;
            ClipBoard.Open;
            try
              ClipBoard.SetAsHandle(CF_BINARY_DATA, H);
              ClipBoard.AsText := S;
            finally
              ClipBoard.Close;
            end;
          except
            GlobalFree(H);
          end;
        end;
      end;
      ecCut:
      begin
        ExecuteCommand(ecCopy);
        ExecuteCommand(ecClearSelection);
      end;
      ecPaste:
      begin
        if L.Index > 0 then
          ExecuteCommand(ecClearSelection);
        if ClipBoard.FormatCount > 0 then
        begin
          S := '';
          // paste as binary data
          if ClipBoard.HasFormat(CF_BINARY_DATA) then
            H := ClipBoard.GetAsHandle(CF_BINARY_DATA)
          else if ClipBoard.HasFormat(CF_TEXT) then
          begin
            H := 0;
            S := ClipBoard.AsText;
            if S <> '' then
            begin
              M := Length(S);
              if (FEditArea = eaDigits) and ExecuteCommand(ecInsertDigits, Pointer(S)) then
              begin
                S := '';
                if M >= cDigitCount then
                begin
                  Inc(TmpSelEnd.Index, M div cDigitCount)
                end else
                begin
                  Inc(TmpSelEnd.Digit, M);
                  if TmpSelEnd.Digit >= cDigitCount then
                  begin
                    Inc(TmpSelEnd.Index);
                    TmpSelEnd.Digit := TmpSelEnd.Digit mod cDigitCount;
                  end;
                end;
                SelectionChanged(True);
              end else
                ExecuteCommand(ecInsertString, Pointer(S));
            end;
          end else
            H := ClipBoard.GetAsHandle(ClipBoard.Formats[0]);
          if H <> 0 then
          begin
            BA := GlobalLock(H);
            try
              I := GlobalSize(H);
              if I > 0 then
              begin
                SetLength(S, I);
                System.Move(BA^, S[1], I);
              end;
            finally
              GlobalUnlock(H);
            end;
            if S <> '' then
              ExecuteCommand(ecInsertString, Pointer(S));
          end;
          if S <> '' then
          begin
            Inc(TmpSelEnd.Index, Length(S));
            TmpSelEnd.Digit := 0;
            SelectionChanged(True);
          end;
        end;
      end;}
      ecInsertChar: InsertChar(TmpSelEnd, PKChar(Data)^);
      ecInsertString: InsertString(TmpSelEnd, TKString(Data));
      ecInsertNewLine: InsertNewLine(TmpSelEnd);
      ecDeleteLastChar:
      begin
        if SelLength <> 0 then
          ClearSelection
        else
          DeleteLastChar(TmpSelEnd);
      end;
      ecDeleteChar:
      begin
        if SelLength <> 0 then
          ClearSelection
        else
          DeleteChar(TmpSelEnd);
      end;
      ecDeleteBOL:
      begin
        if SelLength <> 0 then
          ClearSelection
        else
          DeleteBOL(TmpSelEnd);
      end;
      ecDeleteEOL:
      begin
        if SelLength <> 0 then
          ClearSelection
        else
          DeleteEOL(TmpSelEnd);
      end;
      ecDeleteLine:
      begin
        if SelLength <> 0 then
          ClearSelection
        else
          DeleteLine(TmpSelEnd);
      end;
      ecSelectAll: Select(0, FBlocks.SelectableLength);
      ecClearAll:
      begin
        ExecuteCommand(ecSelectAll);
        ExecuteCommand(ecClearSelection);
      end;
      ecClearSelection: ClearSelection;
{      ecSearch, ecReplace:
      begin
        // doesn't search for single digits
        PSD := Data;
        if PSD <> nil then
        begin
          PSD.ErrorReason := eseOk;
          S := PSD.TextToFind;
          if Command = ecReplace then
          begin
            T := PSD.TextToReplace;
            ReplaceAction := eraYes;
          end;
          if esoSelectedOnly in PSD.Options then
            if esoFirstSearch in PSD.Options then
            begin
              PSD.SelStart := GetRealSelStart.Index;
              PSD.SelEnd := GetRealSelEnd.Index;
            end else
            begin
              PSD.SelStart := MinMax(PSD.SelStart, 0, FSize);
              PSD.SelEnd := MinMax(PSD.SelEnd, 0, FSize);
            end;
          if esoFirstSearch in PSD.Options then
            Exclude(PSD.Options, esoWereDigits);
          if esoTreatAsDigits in PSD.Options then
          begin
            if DigitsToBinStr(S) then
            begin
              S := BinStrToBinary(S);
              if Command = ecReplace then
              begin
                if DigitsToBinStr(T) then
                begin
                  T := BinStrToBinary(T);
                  PSD.TextToFind := S;
                  PSD.TextToReplace := T;
                  Exclude(PSD.Options, esoTreatAsDigits);
                  Include(PSD.Options, esoWereDigits);
                end else
                  PSD.ErrorReason := eseNoDigitsReplace;
              end else
              begin
                PSD.TextToFind := S;
                Exclude(PSD.Options, esoTreatAsDigits);
                Include(PSD.Options, esoWereDigits);
              end;
            end else
              PSD.ErrorReason := eseNoDigitsFind;
          end;
          if PSD.ErrorReason = eseOk then
          begin
            N := Length(S);
            if esoBackwards in PSD.Options then
            begin
              O := -1;
              if (esoEntireScope in PSD.Options) and (esoFirstSearch in PSD.Options) then
                I := FSize
              else
                I := GetRealSelStart.Index - 1;
              if esoSelectedOnly in PSD.Options then
              begin
                M := PSD.SelStart;
                if esoFirstSearch in PSD.Options then
                  I := PSD.SelEnd
              end else
                M := 0;
              I := Min(I, FSize - N);
              if I < M then
                PSD.ErrorReason := eseNoMatch
            end else
            begin
              O := 1;
              if (esoEntireScope in PSD.Options) and (esoFirstSearch in PSD.Options) then
                I := 0
              else
                I := GetRealSelEnd.Index;
              if esoSelectedOnly in PSD.Options then
              begin
                M := PSD.SelEnd;
                if esoFirstSearch in PSD.Options then
                  I := PSD.SelStart
              end else
                M := FSize;
              M := Min(M, FSize - N);
              if I >= M then
                PSD.ErrorReason := eseNoMatch
            end;
            if PSD.ErrorReason = eseOk then
            begin
              Found := False;
              MatchCase := PSD.Options * [esoMatchCase, esoWereDigits] <> [];
              if MatchCase then
                C1 := S[1]
              else
                C1 := UpCase(S[1]);
              I := MinMax(I, 0, FSize - 1);
              while I <> M do
              begin
                if MatchCase then
                  C2 := Char(FBuffer[I])
                else
                  C2 := UpCase(Char(FBuffer[I]));
                if C1 = C2 then
                begin
                  if FSize - I >= N then
                  begin
                    J := 2;
                    Dec(I);
                    while (J <= N) do
                    begin
                      if MatchCase then
                      begin
                        C2 := Char(FBuffer[I + J]);
                        C3 := S[J];
                      end else
                      begin
                        C2 := Upcase(Char(FBuffer[I + J]));
                        C3 := Upcase(S[J]);
                      end;
                      if C2 = C3 then
                        Inc(J)
                      else
                        Break;
                    end;
                    Inc(I);
                    if J = N + 1 then
                    begin
                      Found := True;
                      FSelStart := MakeSelection(I, 0);
                      TmpSelEnd := MakeSelection(I + N, 0);
                      if Command = ecReplace then
                      begin
                        if (esoPrompt in PSD.Options) and Assigned(FOnReplaceText) then
                        begin
                          SelectionChanged(False, False);
                          if not CaretInView then
                            ExecuteCommand(ecScrollCenter);
                          FOnReplaceText(Self, S, T, ReplaceAction)
                        end else
                          ReplaceAction := eraYes;
                        case ReplaceAction of
                          eraCancel: Break;
                          eraYes, eraAll:
                          begin
                            if T = '' then
                              ExecuteCommand(ecClearIndexSelection)
                            else
                              ExecuteCommand(ecInsertString, Pointer(T));
                            TmpSelEnd := MakeSelection(I + Length(T), 0);
                            AddUndoCaretPos;
                            if ReplaceAction  = eraAll then
                              Include(PSD.Options, esoAll);
                          end;
                        end;
                        if not (esoAll in PSD.Options) then
                          Break;
                      end else
                        Break;
                    end
                  end;
                end;
                Inc(I, O);
              end;
              if Found then
              begin
                SelectionChanged(False, False);
                if not CaretInView then
                  ExecuteCommand(ecScrollCenter);
              end else
                PSD.ErrorReason := eseNoMatch;
            end;
          end;
          Exclude(PSD.Options, esoFirstSearch);
        end else
          Result := False;
      end;
      ecPreview, ecPrint:
      begin
        PPD := Data;
        if (PPD <> nil) and ((PPD.Settings <> nil) or GetPrintSettings(PPD^, PS, Command = ecPrint)) then
        begin
          if PPD.Settings <> nil then
            PS := PPD.Settings^;
          with PCInfo do
          begin
            Page := 0;
            Pages := PPD.EndPage - PPD.StartPage + 1;
            Copy := 0;
            Copies := PPD.Copies;
          end;
          with PaintData do
          begin
            CharWidth := PS.CharWidth;
            CharHeight := PS.CharHeight;
            PaintRect.Top := PS.PrintMargins.Top; // remains unchanged
            LeftChar := 0;
            Printing := True;
            PrintAll := PPD.Range <> eprSelectedOnly;
            PrintColors := epoUseColor in PPD.Options;
            if Command = ecPrint then
            begin
              PAbort := False;
              Canvas := Printer.Canvas;
              Printer.Title := PPD.Title;
              Printer.BeginDoc;
              try
                DoPrintNotify(epsBegin, PCInfo, PAbort);
                if not PAbort then
                begin
                  for I := 1 to PPD.Copies do if not PAbort then
                  begin
                    PCInfo.Copy := I;
                    for J := PPD.StartPage to PPD.EndPage do if not PAbort then
                    begin
                      PCInfo.Page := J;
                      PrintPage(PPD, PaintData, PS, PCInfo);
                      DoPrintNotify(epsNewPage, PCInfo, PAbort);
                      if ((J < PPD.EndPage) or (I < PPD.Copies)) and not PAbort then
                        Printer.NewPage;
                    end;
                  end;
                end;
                PCInfo.Page := 0;
                PCInfo.Copy := 0;
                DoPrintNotify(epsEnd, PCInfo, PAbort);
              finally
                Printer.EndDoc;
              end;
            end else
            begin
              Canvas := PPD.PreviewCanvas;
              PCInfo.Copy := 1;
              PCInfo.Page := PPD.PreviewPage;
              PrintPage(PPD, PaintData, PS, PCInfo);
            end;
          end;
        end else
          Result := False;
      end;}
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
        FPreferredCaretPos := FCaretRect.Left;
    end;
  end;
end;

procedure TKCustomMemo.FontChange(Sender: TObject);
begin
  FBlocks.NotifyFontChange(Font);
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

function TKCustomMemo.GetEmpty: Boolean;
begin
  Result := FBlocks.Empty;
end;

function TKCustomMemo.GetInsertMode: Boolean;
begin
  Result := not (elOverwrite in FStates);
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

function TKCustomMemo.GetText: TKString;
begin
  Result := FBlocks.Text;
end;

function TKCustomMemo.GetUndoLimit: Integer;
begin
  Result := FUndoList.Limit;
end;

procedure TKCustomMemo.HideEditorCaret;
begin
  if HandleAllocated then
    HideCaret(Handle);
end;

function TKCustomMemo.IndexToRect(AValue: Integer; ACaret: Boolean): TRect;
begin
  Result := FBlocks.IndexToRect(Canvas, AValue, ACaret);
end;

function TKCustomMemo.IndexValid(Value: Integer): Boolean;
begin
  Result := False;
end;

procedure TKCustomMemo.InsertChar(At: Integer; AValue: TKChar);
begin
  BeginUndoGroup(ckInsert);
  try
    if FBlocks.SelLength <> 0 then
    begin
      FBlocks.ClearSelection;
      At := FBlocks.SelEnd;
    end;
    if elOverwrite in FStates then
      DeleteChar(At);
    if FBlocks.InsertString(At, AValue) then
      ExecuteCommand(ecRight);
  finally
    EndUndoGroup;
  end;
end;

procedure TKCustomMemo.InsertNewLine(At: Integer);
begin
  BeginUndoGroup(ckInsert);
  try
    if FBlocks.SelLength > 0 then
    begin
      FBlocks.ClearSelection;
      At := FBlocks.SelEnd;
    end;
    // always insert (don't overwrite)
    if FBlocks.InsertNewLine(At) then
      ExecuteCommand(ecRight);
  finally
    EndUndoGroup;
  end;
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
      FBlocks.InsertString(At, AValue);
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
  I: Integer;
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

    FBlocks.PaintToCanvas(ACanvas, ClientWidth, ClientHeight);
{$IFDEF FPC}
  finally
    if CaretVisible then
      ShowEditorCaret;
  end;
{$ENDIF}
end;

function TKCustomMemo.PointToIndex(APoint: TPoint; AOutOfArea, ACaret: Boolean): Integer;
begin
  Result := FBlocks.PointToIndex(Canvas, APoint, AOutOfArea, ACaret);
end;

procedure TKCustomMemo.SafeSetFocus;
begin
  if not Focused and CanFocus and not (csDesigning in ComponentState) then SetFocus;
end;

function TKCustomMemo.Scroll(CodeHorz, CodeVert, DeltaHorz, DeltaVert: Integer; CallScrollWindow: Boolean): Boolean;

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
    FBlocks.MeasureExtent(Canvas, ContentLeft, ContentTop);
    if CallScrollWindow then
      ScrollWindowEx(Handle, (OldLeftPos - FLeftPos) * FHorzScrollStep, (OldTopPos - FTopPos) * FVertScrollStep,
        nil, nil, 0, nil, SW_INVALIDATE)
    else
      Invalidate;
    UpdateEditorCaret;
    Inc(FPreferredCaretPos, (OldLeftPos - FLeftPos) * FHorzScrollStep);
  end;
end;

procedure TKCustomMemo.ScrollBy(DeltaHorz, DeltaVert: Integer);
begin
  Scroll(cScrollNoAction, cScrollNoAction, DeltaHorz, DeltaVert, True);
end;

procedure TKCustomMemo.ScrollToClientAreaCenter;
var
  R: TRect;
begin
  R := IndexToRect(SelEnd, False);
  ScrollBy((R.Left - ClientWidth div 2) div FHorzScrollStep, (R.Top - ClientHeight div 2) div FVertScrollStep);
end;

function TKCustomMemo.ScrollNeeded(out DeltaCol, DeltaRow: Integer): Boolean;
var
  HScrollPadding, VScrollPadding: Integer;
begin
  // for small window sizes, decrease scroll padding dynamically, formula taken taken from experience
  HScrollPadding := Min(FScrollPadding, ClientWidth div 8);
  VScrollPadding := Min(FScrollPadding, ClientHeight div 8);
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

procedure TKCustomMemo.SelectionExpand(ASelEnd: Integer; ADoScroll: Boolean);
begin
  Select(SelStart, ASelEnd - SelStart, ADoScroll);
end;

procedure TKCustomMemo.SelectionExpand(const APoint: TPoint; ADoScroll: Boolean);
var
  NewSelEnd: Integer;
begin
  NewSelEnd := FBlocks.PointToIndex(Canvas, APoint, True, False);
  Select(SelStart, NewSelEnd - SelStart, ADoScroll);
end;

procedure TKCustomMemo.SelectionInit(ASelStart: Integer; ADoScroll: Boolean);
begin
  Select(ASelStart, 0, ADoScroll);
end;

procedure TKCustomMemo.SelectionInit(const APoint: TPoint; ADoScroll: Boolean);
begin
  Select(FBlocks.PointToIndex(Canvas, APoint, True, True), 0, ADoScroll);
  FPreferredCaretPos := FCaretRect.Left;
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
    ScrollBy(Value - FLeftPos, 0);
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
  R: TRect;
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
{$IFDEF USE_WINAPI}
var
  UpdateDropFiles: Boolean;
{$ENDIF}
begin
  if Value <> FOptions then
  begin
  {$IFDEF USE_WINAPI}
    UpdateDropFiles := (eoDropFiles in Value) <> (eoDropFiles in FOptions);
    FOptions := Value;
    // (un)register HWND as drop target
    if UpdateDropFiles and not (csDesigning in ComponentState) and HandleAllocated then
      DragAcceptFiles(Handle, (eoDropFiles in fOptions));
  {$ELSE}
    FOptions := Value;
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
    FBlocks.Select(0, 0);
    FBlocks.Clear;
    FBlocks.Text := Value;
    TextChanged;
  finally
    FBlocks.UnlockUpdate;
  end;
end;

procedure TKCustomMemo.SetTopPos(Value: Integer);
begin
  Value := MinMax(Value, 0, FVertExtent - 1);
  if Value <> FTopPos then
    ScrollBy(0, Value - FTopPos);
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

procedure TKCustomMemo.TextChanged;
begin
  Select(0, 0, False);
  FUndoList.Clear;
  FRedoList.Clear;
  UpdateScrollRange(False);
  FPreferredCaretPos := 0;
  DoChange;
end;

procedure TKCustomMemo.UndoChange(Sender: TObject; ItemKind: TKMemoChangeKind);
begin
  if (Sender = FUndoList) and (ItemKind <> ckCaretPos) then
    DoChange;
end;

procedure TKCustomMemo.UpdateEditorCaret(AShow: Boolean);
begin
  if HandleAllocated then
  begin
    Include(FStates, elCaretUpdate);
    try
      FCaretRect := FBlocks.IndexToRect(Canvas, SelEnd, True);
      Dec(FCaretRect.Right, FCaretRect.Left); // Right is width
      Dec(FCaretRect.Bottom, FCaretRect.Top); // Bottom is height

      if AShow then
      begin
        if Enabled and Focused and not (csDesigning in ComponentState) and (SelLength = 0) then
        begin
          if not (elOverwrite in FStates) then
            FCaretRect.Right := MinMax(FCaretRect.Bottom div 10, 1, 3);
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
    FBlocks.MeasureExtent(Canvas, ContentLeft, ContentTop);
    FHorzExtent := DivUp(FBlocks.ExtentX + FContentPadding.Left + FContentPadding.Right, FHorzScrollStep);
    FVertExtent := DivUp(FBlocks.ExtentY + FCOntentPadding.Top + FContentPadding.Bottom, FVertScrollStep);
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
      SI.ntrackPos := SB_POLICY_CONTINUOUS;
    {$ENDIF}
      if FScrollBars in [ssBoth, ssHorizontal] then
      begin
        SI.nMax := FHorzExtent;
        SI.nPage := ClientHorz;
        SI.nPos := FLeftPos;
        SetScrollInfo(Handle, SB_HORZ, SI, True);
        ShowScrollBar(Handle, SB_HORZ, Integer(SI.nPage) < SI.nMax);
      end else
        ShowScrollBar(Handle, SB_HORZ, False);
      if FScrollBars in [ssBoth, ssVertical] then
      begin
        SI.nMax := FVertExtent;
        SI.nPage := ClientVert;
        SI.nPos := FTopPos;
        SetScrollInfo(Handle, SB_VERT, SI, True);
        ShowScrollBar(Handle, SB_VERT, Integer(SI.nPage) < SI.nMax);
      end else
        ShowScrollBar(Handle, SB_VERT, False);
    end;
    if CallInvalidate then
    begin
      UpdateEditorCaret;
      Invalidate;
    end else
      ScrollBy(-DeltaHorz, -DeltaVert);
    InvalidatePageSetup;
  end;
end;

procedure TKCustomMemo.UpdateSize;
begin
  UpdateScrollRange(False);
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
  FBounds := CreateEmptyRect;
  FParent := nil;
  FSelEnd := -1;
  FSelStart := -1;
  Parent := AParent; // to update default block properties!
end;

function TKMemoBlock.AddText(const AText: TKString; At: Integer): Boolean;
begin
  Result := False;
end;

procedure TKMemoBlock.Assign(AItem: TKMemoBlock);
begin
  Bounds := AItem.Bounds;
  Parent := AItem.Parent;
  Select(AItem.SelStart, AItem.SelLength);
  AssignAttributes(AItem);
end;

procedure TKMemoBlock.AssignAttributes(AItem: TKMemoBlock);
begin
  Update([muContent]);
end;

procedure TKMemoBlock.ClearSelection;
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

function TKMemoBlock.GetCanAddText: Boolean;
begin
  Result := False;
end;

procedure TKMemoBlock.GetSelColors(var TextColor, Background: TColor);
begin
  TextColor := cSelTextFocusedDef;
  Background := cSelBkGndFocusedDef;
  if FParent <> nil then
    FParent.GetSelColors(TextColor, Background);
end;

function TKMemoBlock.GetSelLength: Integer;
begin
  Result := FSelEnd - FSelStart;
end;

function TKMemoBlock.GetSelStart: Integer;
begin
  Result := FSelStart;
end;

function TKMemoBlock.GetSelText: TKString;
begin
  Result := '';
end;

function TKMemoBlock.GetText: TKString;
begin
  Result := '';
end;

function TKMemoBlock.IndexToRect(ACanvas: TCanvas; AIndex: Integer): TRect;
begin
  Result := FBounds;
end;

procedure TKMemoBlock.MeasureExtent(ACanvas: TCanvas);
begin
  FBounds.BottomRight := FBounds.TopLeft;
end;

procedure TKMemoBlock.NotifyFontChange(AFont: TFont);
begin
end;

procedure TKMemoBlock.NotifyLineInfo(const ALineInfo: TKMemoLine);
begin
  FBounds.Bottom := ALineInfo.Bounds.Bottom;
end;

procedure TKMemoBlock.PaintToCanvas(ACanvas: TCanvas);
begin
end;

procedure TKMemoBlock.ParentChanged;
begin
end;

function TKMemoBlock.PointToIndex(ACanvas: TCanvas; const APoint: TPoint): Integer;
begin
  Result := -1;
end;

function TKMemoBlock.Select(ASelStart, ASelLength: Integer): Boolean;
var
  NewSelEnd, MaxLen: Integer;
begin
  NewSelEnd := ASelStart + ASelLength;
  if NewSelEnd < ASelStart then
    Exchange(ASelStart, NewSelEnd);
  MaxLen := SelectableLength;
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

function TKMemoBlock.SelectableLength: Integer;
begin
  Result := ContentLength;
end;

procedure TKMemoBlock.SetBounds(const Value: TRect);
begin
  if not EqualRect(Value, FBounds) then
  begin
    FBounds := Value;
    Update([muExtent]);
  end;
end;

procedure TKMemoBlock.SetOffset(ALeft, ATop: Integer);
begin
  FBounds.Left := ALeft;
  FBounds.Top := ATop;
end;

procedure TKMemoBlock.SetParent(AParent: TKMemoBlocks);
begin
  if FParent <> AParent then
  begin
    FParent := AParent;
    ParentChanged;
  end;
end;

function TKMemoBlock.ShowFormatting: Boolean;
begin
  if FParent <> nil then
    Result := FParent.ShowFormatting
  else
    Result := False;
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

{ TKTextMemoBlock }

constructor TKTextMemoBlock.Create(AParent: TKMemoBlocks);
begin
  FBrush := TBrush.Create;
  FBrush.Color := clNone;
  FFont := TFont.Create;
  FFont.Color := clWindowText;
  FFont.OnChange := FontChange;
  FFontChanged := False;
  inherited;
  FContentChanged := True;
  FHAlign := halLeft;
  FHeight := 0;
  FText := '';
  FTextLength := TextLength;
  FVAlign := valCenter;
  FWidth := 0;
end;

destructor TKTextMemoBlock.Destroy;
begin
  FBrush.Free;
  FFont.Free;
  inherited;
end;

procedure TKTextMemoBlock.FontChange(Sender: TObject);
begin
  FFontChanged := True;
end;

function TKTextMemoBlock.AddText(const AText: TKString; At: Integer): Boolean;
var
  S, Part1, Part2: TKString;
begin
  Result := False;
  Inc(At); // At is zero based
  if At > 0 then
  begin
    SplitText(At, Part1, Part2);
    S := Part1 + AText + Part2;
  end
  else
    S := FText + AText;
  if S <> FText then
  begin
    FText := S;
    FTextLength := TextLength;
    FContentChanged := True;
    Update([muContent]);
    Result := True;
  end;
end;

procedure TKTextMemoBlock.Assign(AItem: TKMemoBlock);
begin
  if AItem is TKTextMemoBlock then
  begin
    Text := TKTextMemoBlock(AItem).Text;
  end;
  inherited;
end;

procedure TKTextMemoBlock.AssignAttributes(AItem: TKMemoBlock);
begin
  if AItem is TKTextMemoBlock then
  begin
    Brush.Assign(TKTextMemoBlock(AItem).Brush);
    Font.Assign(TKTextMemoBlock(AItem).Font);
    HAlign := TKTextMemoBlock(AItem).HAlign;
    VAlign := TKTextMemoBlock(AItem).VAlign;
  end;
  inherited;
end;

procedure TKTextMemoBlock.ClearSelection;
begin
  inherited;
  if SelLength <> 0 then
  begin
  {$IFDEF FPC}
    UTF8Delete(FText, FSelStart + 1, FSelEnd - FSelStart);
  {$ELSE}
    Delete(FText, FSelStart + 1, FSelEnd - FSelStart);
  {$ENDIF}
    FTextLength := TextLength;
    FContentChanged := True;
    FSelEnd := FSelStart;
    Update([muContent]);
  end;
end;

function TKTextMemoBlock.Concat(AItem: TKMemoBlock): Boolean;
begin
  Result := AItem is TKTextMemoBlock;
  if Result then
    AddText(TKTextMemoBlock(AItem).Text, -1);
end;

function TKTextMemoBlock.ContentLength: Integer;
begin
  Result := FTextLength;
end;

function TKTextMemoBlock.GetCanAddText: Boolean;
begin
  Result := True;
end;

function TKTextMemoBlock.GetSelText: TKString;
begin
{$IFDEF FPC}
  Result := UTF8Copy(FText, FSelStart + 1, FSelEnd - FSelStart);
{$ELSE}
  Result := Copy(FText, FSelStart + 1, FSelEnd - FSelStart);
{$ENDIF}
end;

function TKTextMemoBlock.GetText: TKString;
begin
  Result := FText;
end;

function TKTextMemoBlock.IndexToRect(ACanvas: TCanvas; AIndex: Integer): TRect;
var
  TextBox: TKTextBox;
begin
  ACanvas.Font.Assign(FFont);
  ACanvas.Brush.Assign(FBrush);
  TextBox := TKTextBox.Create;
  try
    TextBox.Attributes := FTextAttributes;
    TextBox.HAlign := FHAlign;
    TextBox.VAlign := FVAlign;
    TextBox.Text := FText;
    Result := TextBox.IndexToRect(ACanvas, FBounds, IndexToTextIndex(FText, AIndex));
  finally
    TextBox.Free;
  end;
end;

function TKTextMemoBlock.IndexToTextIndex(var AText: TKString; AIndex: Integer): Integer;
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

procedure TKTextMemoBlock.MeasureExtent(ACanvas: TCanvas);
var
  TextBox: TKTextBox;
  Width, Height: Integer;
begin
  if FContentChanged then
  begin
    ACanvas.Font.Assign(FFont);
    ACanvas.Brush.Assign(FBrush);
    TextBox := TKTextBox.Create;
    try
      TextBox.Attributes := FTextAttributes;
      TextBox.HAlign := FHAlign;
      TextBox.VAlign := FVAlign;
      TextBox.Text := FText;
      TextBox.Measure(ACanvas, FBounds, FWidth, FHeight);
    finally
      TextBox.Free;
    end;
    FContentChanged := False;
  end;
  FBounds.Right := FBounds.Left + FWidth;
  FBounds.Bottom := FBounds.Top + FHeight;
end;

procedure TKTextMemoBlock.NotifyFontChange(AFont: TFont);
begin
  if not FFontChanged then
  begin
    FFont.Assign(AFont);
    FFontChanged := False;
  end;
end;

procedure TKTextMemoBlock.PaintToCanvas(ACanvas: TCanvas);
var
  TextBox: TKTextBox;
  Color, Bkgnd: TColor;
begin
  inherited;
  ACanvas.Font.Assign(FFont);
  ACanvas.Brush.Assign(FBrush);
  TextBox := TKTextBox.Create;
  try
    TextBox.Attributes := FTextAttributes;
    TextBox.HAlign := FHAlign;
    TextBox.VAlign := FVAlign;
    TextBox.Text := FText;
    if SelLength > 0 then
    begin
      GetSelColors(Color, BkGnd);
      TextBox.SelColor := Color;
      TextBox.SelBkgnd := BkGnd;
      TextBox.SelStart := IndexToTextIndex(FText, FSelStart);
      TextBox.SelEnd := IndexToTextIndex(FText, FSelEnd);
    end;
    if FBrush.Color <> clNone then
      TextBox.Attributes := TextBox.Attributes + [taFillRect];
    TextBox.Draw(ACanvas, FBounds);
  finally
     TextBox.Free;
  end;
end;

procedure TKTextMemoBlock.ParentChanged;
begin
  inherited;
  if not FFontChanged and (FParent <> nil) then
    FFont.Assign(FParent.DefaultFont);
end;

function TKTextMemoBlock.PointToIndex(ACanvas: TCanvas; const APoint: TPoint): Integer;
var
  TextBox: TKTextBox;
begin
  ACanvas.Font.Assign(FFont);
  ACanvas.Brush.Assign(FBrush);
  TextBox := TKTextBox.Create;
  try
    TextBox.Attributes := FTextAttributes;
    TextBox.HAlign := FHAlign;
    TextBox.VAlign := FVAlign;
    TextBox.Text := FText;
    TextBox.Attributes := TextBox.Attributes + [taIncludePadding];
    Result := TextIndexToIndex(FText, TextBox.PointToIndex(ACanvas, FBounds, APoint));
  finally
    TextBox.Free;
  end;
end;

procedure TKTextMemoBlock.SetHAlign(const Value: TKHAlign);
begin
  if FHAlign <> Value then
  begin
    FHAlign := Value;
    FContentChanged := True;
    Update([muAttributes]);
  end;
end;

procedure TKTextMemoBlock.SetText(const Value: TKString);
begin
  if FText <> Value then
  begin
    FText := Value;
    FTextLength := TextLength;
    FContentChanged := True;
    Update([muContent]);
  end;
end;

procedure TKTextMemoBlock.SetTextAttributes(const Value: TKTextAttributes);
begin
  if Value <> FTextAttributes then
  begin
    FTextAttributes := Value;
    FContentChanged := True;
    Update([muAttributes]);
  end;
end;

procedure TKTextMemoBlock.SetVAlign(const Value: TKVAlign);
begin
  if FVAlign <> Value then
  begin
    FVAlign := Value;
    FContentChanged := True;
    Update([muAttributes]);
  end;
end;

function TKTextMemoBlock.Split(At: Integer): TKMemoBlock;
var
  Item: TKTextMemoBlock;
  Part1, Part2: TKString;
begin
  if (At > 0) and (At < ContentLength) then
  begin
    Item := TKTextMemoBlock.Create(FParent);
    Item.Assign(Self);
    SplitText(At + 1, Part1, Part2);
    FText := Part1;
    FTextLength := TextLength;
    FContentChanged := True;
    Item.Text := Part2;
    Result := Item;
    Update([muContent]);
  end else
    Result := nil;
end;

procedure TKTextMemoBlock.SplitText(At: Integer; out APart1, APart2: TKString);
begin
{$IFDEF FPC}
  APart1 := UTF8Copy(FText, 1, At - 1);
  APart2 := UTF8Copy(FText, At, Length(FText) - At + 1);
{$ELSE}
  APart1 := Copy(FText, 1, At - 1);
  APart2 := Copy(FText, At, Length(FText) - At + 1);
{$ENDIF}
end;

function TKTextMemoBlock.TextIndexToIndex(var AText: TKString; ATextIndex: Integer): Integer;
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

function TKTextMemoBlock.TextLength: Integer;
begin
{$IFDEF FPC}
  Result := UTF8Length(FText);
{$ELSE}
  Result := Length(FText);
{$ENDIF}
end;

{ TKImageMemoBlock }

constructor TKImageMemoBlock.Create(AParent: TKMemoBlocks);
begin
  inherited;
  FImage := TPicture.Create;
end;

destructor TKImageMemoBlock.Destroy;
begin
  FImage.Free;
  inherited;
end;

procedure TKImageMemoBlock.Assign(AItem: TKMemoBlock);
begin
  if AItem is TKImageMemoBlock then
  begin
    FImage.Assign(TKImageMemoBlock(AItem).Image);
  end;
  inherited;
end;

function TKImageMemoBlock.ContentLength: Integer;
begin
  Result := 1;
end;

function TKImageMemoBlock.IndexToRect(ACanvas: TCanvas; AIndex: Integer): TRect;
begin
  Result := FBounds;
end;

procedure TKImageMemoBlock.MeasureExtent(ACanvas: TCanvas);
begin
  FBounds.Right := FBounds.Left + FImage.Width;
  FBounds.Bottom := FBounds.Top + FImage.Height;
end;

procedure TKImageMemoBlock.PaintToCanvas(ACanvas: TCanvas);
var
  Bitmap: TKAlphaBitmap;
  Color, Bkgnd: TColor;
  IsPng: Boolean;
begin
  inherited;
  if SelLength > 0 then
  begin
    GetSelColors(Color, BkGnd);
    ACanvas.Brush.Color := BkGnd;
    ACanvas.FillRect(FBounds);
    Bitmap := TKAlphaBitmap.Create;
    try
      Bitmap.SetSize(FImage.Width, FImage.Height);
    {$IFDEF FPC}
      Bitmap.UpdateHandle;
    {$ENDIF}
      Bitmap.Canvas.Brush.Color := BkGnd;
      Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
      Bitmap.Canvas.Draw(0, 0, FImage.Graphic);
    {$IFDEF FPC}
      Bitmap.UpdatePixels;
    {$ENDIF}
    {$IFDEF USE_PNG_SUPPORT}
      IsPng := (FImage.Graphic is TKPngImage);
    {$ElSE}
      IsPng := False;
    {$ENDIF}
      Bitmap.AlphaFillPercent(50, True);
      Bitmap.AlphaDrawTo(ACanvas, FBounds.Left, FBounds.Top);
    finally
      Bitmap.Free;
    end;
  end else
    ACanvas.Draw(FBounds.Left, FBounds.Top, FImage.Graphic);
end;

function TKImageMemoBlock.PointToIndex(ACanvas: TCanvas; const APoint: TPoint): Integer;
begin
  if PtInRect(FBounds, APoint) then
    Result := 0
  else
    Result := -1;
end;

procedure TKImageMemoBlock.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
  Update([muContent]);
end;

procedure TKImageMemoBlock.SetImagePath(const Value: TKString);
begin
  FImage.LoadFromFile(Value);
  Update([muContent]);
end;

{ TKNewLineMemoBlock }

constructor TKNewLineMemoBlock.Create(AParent: TKMemoBlocks);
begin
  inherited;
{$IFDEF FPC}
  FText := UnicodeToUTF8(Cardinal(cNewLineChar));
{$ELSE}
  FText := cNewLineChar;
{$ENDIF}
  FTextLength := TextLength;
end;

function TKNewLineMemoBlock.AddText(const AText: TKString; At: Integer): Boolean;
begin
  Result := False;
end;

procedure TKNewLineMemoBlock.ClearSelection;
begin
  // ignore
end;

function TKNewLineMemoBlock.Concat(AItem: TKMemoBlock): Boolean;
begin
  Result := False;
end;

function TKNewLineMemoBlock.GetCanAddText: Boolean;
begin
  Result := False;
end;

function TKNewLineMemoBlock.IndexToRect(ACanvas: TCanvas; AIndex: Integer): TRect;
var
  BeyondText: Boolean;
  Width: Integer;
begin
  BeyondText := AIndex >= ContentLength;
  if BeyondText then
    Dec(AIndex);
  Result := inherited IndexToRect(ACanvas, AIndex);
  if BeyondText then
  begin
    Width := Result.Right - Result.Left;
    Result.Left := Result.Right;
    Inc(Result.Right, Width);
  end;
end;

procedure TKNewLineMemoBlock.PaintToCanvas(ACanvas: TCanvas);
var
  Color, BkGnd: TColor;
begin
  if ShowFormatting then
    inherited
  else
  begin
    ACanvas.Brush.Assign(FBrush);
    if (SelLength > 0) and (SelStart = 0) then
    begin
      GetSelColors(Color, BkGnd);
      ACanvas.Brush.Color := BkGnd;
    end;
    if ACanvas.Brush.Color <> clNone then
      ACanvas.FillRect(FBounds);
  end
end;

function TKNewLineMemoBlock.SelectableLength: Integer;
begin
  Result := ContentLength + 1;
end;

procedure TKNewLineMemoBlock.SetText(const Value: TKString);
begin
  // ignore
end;

procedure TKNewLineMemoBlock.SetTextAttributes(const Value: TKTextAttributes);
begin
  // ignore
end;

function TKNewLineMemoBlock.Split(At: Integer): TKMemoBlock;
begin
  Result := nil;
end;

{ TKMemoBlocks }

constructor TKMemoBlocks.Create(AMemo: TKCustomMemo);
begin
  inherited Create;
  FLines := nil;
  FLineCount := 0;
  FMemo := AMemo;
  FExtentX := 0;
  FExtentY := 0;
  FSelEnd := 0;
  FSelStart := 0;
  FUpdateLock := 0;
  Update([muContent]);
end;

destructor TKMemoBlocks.Destroy;
begin
  Flines := nil;
  inherited;
end;

function TKMemoBlocks.AddAt(AObject: TKMemoBlock; At: Integer): Integer;
begin
  if AObject <> nil then
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
    Result := -1;
end;

function TKMemoBlocks.AddImageBlock(APath: TKString; At: Integer): TKImageMemoBlock;
begin
  LockUpdate;
  try
    Result := TKImageMemoBlock.Create(Self);
    Result.SetImagePath(APath);
    AddAt(Result, At);
  finally
    UnlockUpdate;
  end;
end;

function TKMemoBlocks.AddLine: PKMemoLine;
const
  cGrow = 100;
var
  ALen: Integer;
begin
  ALen := Length(FLines);
  if FLineCount >= ALen then
    SetLength(FLines, ALen + cGrow);
  Result := @FLines[FLineCount];
  Inc(FLineCount);
  FillChar(Result^, SizeOf(TKMemoLine), 0);
end;

function TKMemoBlocks.AddNewLineBlock(At: Integer): TKNewLineMemoBlock;
begin
  LockUpdate;
  try
    Result := TKNewLineMemoBlock.Create(Self);
    At := AddAt(Result, At);
    if At > 0 then
      Result.AssignAttributes(TKMemoBlock(Items[At - 1]));
  finally
    UnlockUpdate;
  end;
end;

function TKMemoBlocks.AddTextBlock(AText: TKString; At: Integer): TKTextMemoBlock;
begin
  LockUpdate;
  try
    Result := TKTextMemoBlock.Create(Self);
    Result.Text := AText;
    AddAt(Result, At);
  finally
    UnlockUpdate;
  end;
end;

procedure TKMemoBlocks.Clear;
begin
  inherited;
  if (FMemo <> nil) and not (csDestroying in FMemo.ComponentState) then
    AddNewLineBlock;
end;

procedure TKMemoBlocks.ClearSelection;
var
  I, First, Last: Integer;
  Item, AuxItem: TKMemoBlock;
  SelectionComplete: Boolean;
begin
  SelectionComplete := False;
  LockUpdate;
  try
    I := 0;
    First := -1;
    Last := -1;
    while I < Count do
    begin
      Item := TKMemoBlock(Items[I]);
      if Item.ContentLength = 0 then
        Delete(I)
      else if (Item.SelStart >= 0) and (Item.SelLength > 0) then
      begin
        if (Item.SelLength = Item.SelectableLength) or (Item is TKNewLineMemoBlock) then
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
      Inc(I);
    end;
    if Last >= 0 then
    begin
      TKMemoBlock(Items[Last]).ClearSelection;
    end;
    if First >= 0 then
    begin
      TKMemoBlock(Items[First]).ClearSelection;
    end;
    if FSelStart < FSelEnd then
      FSelEnd := FSelStart
    else
      FSelStart := FSelEnd;
  finally
    UnlockUpdate;
  end;
end;

procedure TKMemoBlocks.Delete(Index: Integer);
begin
  inherited;
  if Count = 0 then
    AddNewLineBlock;
end;

function TKMemoBlocks.GetDefaultFont: TFont;
begin
  if FMemo <> nil then
    Result := FMemo.Font
  else
    Result := nil;
end;

function TKMemoBlocks.GetEmpty: Boolean;
begin
  Result := (Count = 0) or (Count = 1) and (Items[0] is TKNewLineMemoBlock);
end;

function TKMemoBlocks.GetLineBottom(AIndex: Integer): Integer;
begin
  Result := 0;
  if (AIndex >= 0) and (AIndex < FLineCount) then
    Result := FLines[AIndex].Bounds.Bottom;
end;

function TKMemoBlocks.GetLineEndIndex(AIndex: Integer): Integer;
begin
  Result := -1;
  if (AIndex >= 0) and (AIndex < FLineCount) then
    Result := FLines[AIndex].EndIndex;
end;

function TKMemoBlocks.GetLineInfo(AIndex: Integer): PKMemoLine;
begin
  if (AIndex >= 0) and (AIndex < FLineCount) then
    Result := @FLines[AIndex]
  else
    Result := nil;
end;

function TKMemoBlocks.GetLineLeft(AIndex: Integer): Integer;
begin
  Result := 0;
  if (AIndex >= 0) and (AIndex < FLineCount) then
    Result := FLines[AIndex].Bounds.Left;
end;

function TKMemoBlocks.GetLineRight(AIndex: Integer): Integer;
begin
  Result := 0;
  if (AIndex >= 0) and (AIndex < FLineCount) then
    Result := FLines[AIndex].Bounds.Right;
end;

function TKMemoBlocks.GetLines(AIndex: Integer): TKString;
var
  I: Integer;
  Item: TKMemoBlock;
begin
  Result := '';
  if (AIndex >= 0) and (AIndex < FLineCount) then
  begin
    for I := FLines[AIndex].StartBlock to FLines[AIndex].EndBlock do
    begin
      Item := TKMemoBlock(Items[I]);
      Result := Result + Item.Text;
    end;
  end;
end;

function TKMemoBlocks.GetLineSize(AIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (AIndex >= 0) and (AIndex < FLineCount) then
  begin
    for I := FLines[AIndex].StartBlock to FLines[AIndex].EndBlock do
      Inc(Result, TKMemoBlock(Items[I]).SelectableLength);
  end;
end;

function TKMemoBlocks.GetLineStartIndex(AIndex: Integer): Integer;
begin
  Result := -1;
  if (AIndex >= 0) and (AIndex < FLineCount) then
    Result := FLines[AIndex].StartIndex;
end;

function TKMemoBlocks.GetLineTop(AIndex: Integer): Integer;
begin
  Result := 0;
  if (AIndex >= 0) and (AIndex < FLineCount) then
    Result := FLines[AIndex].Bounds.Top;
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

procedure TKMemoBlocks.GetSelColors(var TextColor, Background: TColor);
begin
  if FMemo <> nil then
  begin
    if FMemo.Focused then
    begin
      TextColor := FMemo.Colors.SelTextFocused;
      Background := FMemo.Colors.SelBkGndFocused;
    end else
    begin
      TextColor := FMemo.Colors.SelText;
      Background := FMemo.Colors.SelBkGnd;
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
    Item := TKMemoBlock(Items[I]);
    if Item.SelLength > 0 then
    begin
      if Item is TKNewLineMemoBlock then
        Result := Result + cEOL
      else
        Result := Result + Item.SelText;
    end;
  end;
end;

function TKMemoBlocks.GetText: TKString;
var
  I: Integer;
  Item: TKMemoBlock;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    Item := TKMemoBlock(Items[I]);
    if Item is TKNewLineMemoBlock then
      Result := Result + cEOL
    else
      Result := Result + Item.Text;
  end;
end;

function TKMemoBlocks.IndexToBlock(AIndex: Integer; out ALocalIndex: Integer): Integer;
var
  I, J, CurIndex, LastIndex: Integer;
begin
  Result := -1;
  ALocalIndex := -1;
  if (AIndex >= 0) and (AIndex < FSelectableLength) then
  begin
    I := 0;
    while (Result < 0) and (I < FLineCount) do
    begin
      if (AIndex >= FLines[I].StartIndex) and (AIndex <= FLines[I].EndIndex) then
      begin
        J := FLines[I].StartBlock;
        CurIndex := FLines[I].StartIndex;
        while (J <= FLines[I].EndBlock) and (Result < 0) do
        begin
          LastIndex := CurIndex;
          Inc(CurIndex, TKMemoBlock(Items[J]).SelectableLength);
          if (AIndex >= LastIndex) and (AIndex < CurIndex) then
          begin
            Result := J;
            ALocalIndex := AIndex - LastIndex;
          end;
          Inc(J);
        end;
      end;
      Inc(I);
    end;
  end;
end;

function TKMemoBlocks.IndexToLine(AIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (AIndex >= 0) and (AIndex < FSelectableLength) then
  begin
    for I := 0 to FLineCount - 1 do
    begin
      if (AIndex >= FLines[I].StartIndex) and (AIndex <= FLines[I].EndIndex) then
      begin
        Result := I;
        Break;
      end;
    end;
  end
  else if AIndex = FSelectableLength then
    Result := FLineCount;
end;

function TKMemoBlocks.IndexToRect(ACanvas: TCanvas; AIndex: Integer; ACaret: Boolean): TRect;
var
  Line: Integer;
begin
  Line := IndexToLine(AIndex);
  Result := LineToRect(ACanvas, AIndex, Line, ACaret);
end;

function TKMemoBlocks.InsertNewLine(AIndex: Integer): Boolean;
var
  Block, LocalIndex: Integer;
  Item, NewItem: TKMemoBlock;
begin
  Result := False;
  LockUpdate;
  try
    Block := IndexToBlock(AIndex, LocalIndex);
    if Block >= 0 then
    begin
      Item := TKMemoBlock(Items[Block]);
      NewItem := Item.Split(LocalIndex);
      if NewItem <> nil then
      begin
        AddNewLineBlock(Block + 1);
        AddAt(NewItem, Block + 2);
      end
      else if LocalIndex = 0 then
        AddNewLineBlock(Block)
      else
        AddNewLineBlock(Block + 1);
      Result := True;
    end;
  finally
    UnlockUpdate;
  end;
end;

function TKMemoBlocks.InsertString(AIndex: Integer; const AValue: TKString): Boolean;
var
  Block, LocalIndex: Integer;
  Item, NewItem: TKMemoBlock;
begin
  Result := False;
  LockUpdate;
  try
    Block := IndexToBlock(AIndex, LocalIndex);
    if Block >= 0 then
    begin
      Item := TKMemoBlock(Items[Block]);
      NewItem := nil;
      if Item.CanAddText then
      begin
        // we already have suitable block at given location
        Result := TKMemoBlock(Item).AddText(AValue, LocalIndex);
      end
      else if LocalIndex = 0 then
      begin
        // we are at local position 0 so we can use previous text block or add new one
        if Block > 0 then
        begin
          // try the last block
          Dec(Block);
          Item := TKMemoBlock(Items[Block]);
          if Item.CanAddText then
          begin
            // insert character at the end of this block
            Result := Item.AddText(AValue);
          end else
          begin
            // if still no text block, insert new one
            NewItem := AddTextBlock(AValue, Block + 1);
          end;
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
        Dec(Block);
        while Block >= 0 do
        begin
          Item := TKMemoBlock(Items[Block]);
          if Item is TKTextMemoBlock then
          begin
            NewItem.AssignAttributes(Item);
            Block := 0;
          end;
          Dec(Block);
        end;
        Result := True;
      end;
    end;
  finally
    UnlockUpdate;
  end;
end;

function TKMemoBlocks.LineToIndex(ALineIndex: Integer): Integer;
begin
  Result := -1;
  if (ALineIndex >= 0) and (ALineIndex < FLineCount) then
    Result := FLines[ALineIndex].StartIndex;
end;

function TKMemoBlocks.LineToRect(ACanvas: TCanvas; AIndex, ALine: Integer; ACaret: Boolean): TRect;
var
  I, LastIndex, CurIndex, Bk: Integer;
  Item, LastItem: TKMemoBlock;
begin
  Result := CreateEmptyRect;
  if (ALine >= 0) and (ALine < FLineCount) then
  begin
    Item := nil;
    CurIndex := FLines[ALine].StartIndex;
    for I := Flines[ALine].StartBlock to FLines[ALine].EndBlock do
    begin
      LastItem := Item;
      Item := TKMemoBlock(Items[I]);
      LastIndex := CurIndex;
      Inc(CurIndex, Item.SelectableLength);
      if (AIndex >= LastIndex) and (AIndex < CurIndex) then
      begin
        if ACaret and not (Item is TKNewLineMemoBlock) and (AIndex = LastIndex) and (LastItem <> nil) and LastItem.CanAddText then
        begin
          Result := LastItem.IndexToRect(ACanvas, LastItem.SelectableLength - 1);
          // return rectangle after the last character in a line
          Bk := Result.Right - Result.Left;
          Result.Left := Result.Right;
          Result.Right := Result.Left + Bk;
        end else
          Result := Item.IndexToRect(ACanvas, AIndex - LastIndex);
        Break;
      end;
    end;
  end;
end;

procedure TKMemoBlocks.LockUpdate;
begin
  if FUpdateLock <= 0 then
    FUpdateReasons := [];
  Inc(FUpdateLock);
end;

procedure TKMemoBlocks.MeasureExtent(ACanvas: TCanvas; ALeft, ATop: Integer);
var
  I, J, OriginX, OriginY, LineHeight: Integer;
  R: TRect;
  Item: TKMemoBlock;
begin
  FExtentX := 0;
  FExtentY := 0;
  OriginY := ATop;
  for I := 0 to FLineCount - 1 do
  begin
    LineHeight := 0;
    OriginX := ALeft;
    for J := FLines[I].StartBlock to FLines[I].EndBlock do
    begin
      Item := TKMemoBlock(Items[J]);
      Item.SetOffset(OriginX, OriginY);
      Item.MeasureExtent(ACanvas);
      R := Item.Bounds;
      LineHeight := Max(LineHeight, R.Bottom - R.Top);
      Inc(OriginX, R.Right - R.Left);
    end;
    FExtentX := Max(FExtentX, OriginX);
    FLines[I].Bounds := Rect(ALeft, OriginY, OriginX, OriginY + LineHeight);
    Inc(OriginY, LineHeight);
    for J := FLines[I].StartBlock to FLines[I].EndBlock do
      TKMemoBlock(Items[J]).NotifyLineInfo(Flines[I]);
  end;
  FExtentY := OriginY - ATop;
  Dec(FExtentX, ALeft);
end;

function TKMemoBlocks.ModifiedLineEndIndex(AIndex: Integer; ACaret: Boolean): Integer;
var
  Line: Integer;
begin
  Line := IndexToLine(AIndex);
  if Line < FLineCount then
  begin
    Result := FLines[Line].EndIndex;
    if ACaret then
      Dec(Result);
  end else
    Result := 0;
end;

function TKMemoBlocks.ModifiedLineStartIndex(AIndex: Integer; ACaret: Boolean): Integer;
var
  Line: Integer;
begin
  Line := IndexToLine(AIndex);
  if Line < FLineCount then
  begin
    Result := FLines[Line].StartIndex;
  end else
    Result := 0;
end;

function TKMemoBlocks.NextIndexByCharCount(AIndex, ACharCount: Integer; ACaret: Boolean): Integer;
var
  IndexCopy, Block, LocalIndex: Integer;
  Item: TKMemoBlock;
begin
  IndexCopy := AIndex;
  if not ACaret and (ACharCount > 0) then
    Dec(IndexCopy);
  Block := IndexToBlock(IndexCopy + ACharCount, LocalIndex);
  if Block >= 0 then
  begin
    Item := TKMemoBlock(Items[Block]);
    if Item is TKNewLineMemoBlock then
    begin
      if LocalIndex >= Item.ContentLength then
        if ACharCount > 0 then
          Inc(ACharCount)
        else if ACharCount < 0 then
          Dec(ACharCount)
    end;
  end;
  Result := AIndex + ACharCount;
end;

function TKMemoBlocks.NextIndexByHorzExtent(ACanvas: TCanvas; AIndex, AWidth: Integer; ACaret: Boolean): Integer;
var
  Line: Integer;
  R: TRect;
begin
  Line := IndexToLine(AIndex);
  R := LineToRect(ACanvas, AIndex, Line, ACaret);
  Result := PointToLineIndex(ACanvas, Line, Point(R.Left + AWidth, LineTop[Line]), True, ACaret);
end;

function TKMemoBlocks.NextIndexByRowDelta(ACanvas: TCanvas; AIndex, ARowDelta, ALeftPos: Integer; ACaret: Boolean): Integer;
var
  Line, TopPos: Integer;
begin
  Line := MinMax(IndexToLine(AIndex) + ARowDelta, 0, FLineCount - 1);
  Result := PointToIndex(ACanvas, Point(ALeftPos, LineTop[Line]), True, ACaret);
end;

function TKMemoBlocks.NextIndexByVertExtent(ACanvas: TCanvas; AIndex, AHeight, ALeftPos: Integer; ACaret: Boolean): Integer;
var
  Line, Extent: Integer;
begin
  Line := IndexToLine(AIndex);
  Extent := 0;
  if AHeight > 0 then
  begin
    while (Extent < AHeight) and (Line < FLineCount - 1) do
    begin
      Inc(Extent, FLines[Line].Bounds.Bottom - FLines[Line].Bounds.Top);
      Inc(Line);
    end;
  end else
  begin
    while (Extent > AHeight) and (Line > 0) do
    begin
      Dec(Extent, FLines[Line].Bounds.Bottom - FLines[Line].Bounds.Top);
      Dec(Line);
    end;
  end;
  Result := PointToIndex(ACanvas, Point(ALeftPos, LineTop[Line]), True, ACaret);
end;

function TKMemoBlocks.NextIndexByVertValue(ACanvas: TCanvas; AIndex, AValue, ALeftPos: Integer;
  ADirection, ACaret: Boolean): Integer;
var
  Line: Integer;
begin
  Line := IndexToLine(AIndex);
  if ADirection then
  begin
    while (FLines[Line].Bounds.Bottom < AValue) and (Line < FLineCount - 1) do
      Inc(Line);
    if (FLines[Line].Bounds.Bottom > AValue) and (Line > 0) then
      Dec(Line);
  end else
  begin
    while (FLines[Line].Bounds.Top > AValue) and (Line > 0) do
      Dec(Line);
    if (FLines[Line].Bounds.Top < AValue) and (Line < FLineCount - 1) then
      Inc(Line);
  end;
  Result := PointToIndex(ACanvas, Point(ALeftPos, LineTop[Line]), True, ACaret);
end;

procedure TKMemoBlocks.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action in [lnAdded, lnDeleted] then
  begin
    if Action = lnDeleted then
      TKMemoBlock(Ptr).Free
    else
      TKMemoBlock(Ptr).Parent := Self;
    Update([muContent]);
  end else
    inherited;
end;

procedure TKMemoBlocks.NotifyFontChange(AFont: TFont);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TKMemoBlock(Items[I]).NotifyFontChange(AFont);
end;

procedure TKMemoBlocks.PaintToCanvas(ACanvas: TCanvas; AWidth, AHeight: Integer);
var
  I, J: Integer;
begin
  for I := 0 to FLineCount - 1 do
  begin
    if (Flines[I].Bounds.Bottom >= 0) and (Flines[I].Bounds.Top < AHeight) then
      for J := FLines[I].StartBlock to FLines[I].EndBlock do
        TKMemoBlock(Items[J]).PaintToCanvas(ACanvas);
  end;
end;

function TKMemoBlocks.PointToIndex(ACanvas: TCanvas; const APoint: TPoint; AOutOfArea, ACaret: Boolean): Integer;
var
  I : Integer;
begin
  Result := -1;
  if FLineCount > 0 then
  begin
    if AOutOfArea and (APoint.Y < FLines[0].Bounds.Top) then
      Result := 0
    else
    begin
      I := 0;
      while (Result < 0) and (I < FLineCount) do
      begin
        if (APoint.Y >= Flines[I].Bounds.Top) and (APoint.Y < Flines[I].Bounds.Bottom) then
          Result := PointToLineIndex(ACanvas, I, APoint, AOutOfArea, ACaret);
        Inc(I);
      end;
    end;
    if (Result < 0) and AOutOfArea and (APoint.Y >= FLines[0].Bounds.Top + FExtentY) then
    begin
      Result := FSelectableLength;
      if ACaret then
        Dec(Result);
    end;
  end;
end;

function TKMemoBlocks.PointToLineIndex(ACanvas: TCanvas; ALine: Integer; const APoint: TPoint;
  AOutOfArea, ACaret: Boolean): Integer;
var
  I, MinExtent, MaxExtent, LocalIndex, LineIndex: Integer;
  Item: TKMemoBlock;
begin
  Result := -1;
  if (ALine >= 0) and (ALine < FLineCount) then
  begin
    MaxExtent := 0;
    MinExtent := MaxInt;
    I := FLines[ALine].StartBlock;
    LineIndex := FLines[ALine].StartIndex;
    while (Result < 0) and (I <= FLines[ALine].EndBlock) do
    begin
      Item := TKMemoBlock(Items[I]);
      LocalIndex := Item.PointToIndex(ACanvas, APoint);
      if LocalIndex >= 0 then
        Result := LineIndex + LocalIndex
      else if PtInRect(Item.Bounds, APoint) then
        Result := LineIndex + Item.SelectableLength;
      MaxExtent := Max(MaxExtent, Item.Bounds.Right);
      MinExtent := Min(MinExtent, Item.Bounds.Left);
      Inc(LineIndex, Item.SelectableLength);
      Inc(I);
    end;
    if (Result < 0) and AOutOfArea then
    begin
      if (APoint.X < MinExtent) then
        Result := FLines[ALine].StartIndex
      else if (APoint.X >= MaxExtent) then
      begin
        Result := FLines[ALine].EndIndex;
        if ACaret then
          Dec(Result);
      end;
    end;
  end;
end;

function TKMemoBlocks.Remove(AObject: TKMemoBlock): Integer;
begin
  Result := inherited Remove(AObject);
  if Count = 0 then
    AddNewLineBlock;
end;

function TKMemoBlocks.Select(ASelStart, ASelLength: Integer; ADoScroll: Boolean): Boolean;
var
  I, LastIndex, CurIndex, NewSelEnd: Integer;
  Item: TKMemoBlock;
begin
  NewSelEnd := ASelStart + ASelLength;
  NewSelEnd := MinMax(NewSelEnd, 0, FSelectableLength - 1);
  ASelStart := MinMax(ASelStart, 0, FSelectableLength - 1);
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
        Item := TKMemoBlock(Items[I]);
        LastIndex := CurIndex;
        Inc(CurIndex, Item.SelectableLength);
        if (ASelStart >= LastIndex) and (NewSelEnd < CurIndex) then
        begin
          // selection within the same block
          Item.Select(ASelStart - LastIndex, NewSelEnd - ASelStart)
        end
        else if (ASelStart >= LastIndex) and ((ASelStart < CurIndex) or (CurIndex = LastIndex)) and (NewSelEnd >= CurIndex) then
          // selection starts in this block
          Item.Select(ASelStart - LastIndex, CurIndex - ASelStart)
        else if (ASelStart < LastIndex) and (NewSelEnd >= LastIndex) and (NewSelEnd < CurIndex) then
          // selection ends in this block
          Item.Select(0, NewSelEnd - LastIndex)
        else if (ASelStart < LastIndex) and (NewSelEnd >= CurIndex) then
          // selection goes through this block
          Item.Select(0, CurIndex - LastIndex)
        else
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

procedure TKMemoBlocks.SetLines(AIndex: Integer; const AValue: TKString);
var
  I: Integer;
begin
  if (AIndex >= 0) and (AIndex < FLineCount) then
  begin
    LockUpdate;
    try
      for I := FLines[AIndex].StartBlock to FLines[AIndex].EndBlock - 1 do
        Delete(FLines[AIndex].StartBlock);
      AddTextBlock(AValue, FLines[AIndex].StartBlock);
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKMemoBlocks.SetText(const AValue: TKString);
var
  List: TStringList;
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
        AddNewLineBlock;
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
      AddNewLineBlock;
    end;
  finally
    UnlockUpdate;
  end;
end;

function TKMemoBlocks.ShowFormatting: Boolean;
begin
  if FMemo <> nil then
    Result := eoShowFormatting in FMemo.Options
  else
    Result := False;
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
    begin
      // avoid possible infinite loops
      Inc(FUpdateLock);
      try
        UpdateLineInfo;
      finally
        Dec(FUpdateLock);
      end;
    end;
    if FMemo <> nil then
      FMemo.BlocksChanged(AReasons);
  end else
    FUpdateReasons := FUpdateReasons + AReasons;
end;

procedure TKMemoBlocks.UpdateLineInfo;
var
  I, LineIndex, LastIndex, CurIndex: Integer;
  IsPara: Boolean;
  Line: PKMemoLine;
  Item: TKMemoBlock;
begin
  LastIndex := 0;
  CurIndex := 0;
  LineIndex := 0;
  FLineCount := 0;
  I := 0;
  while I < Count do
  begin
    Item := TKMemoBlock(Items[I]);
    Inc(CurIndex, Item.SelectableLength);
    if Item is TKNewLineMemoBlock then
    begin
      Line := AddLine;
      Line.StartBlock := LineIndex;
      Line.EndBlock := I;
      Line.StartIndex := LastIndex;
      Line.EndIndex := CurIndex - 1;
      LineIndex := I + 1;
      LastIndex := CurIndex;
    end;
    Inc(I);
  end;
  if I > LineIndex then
  begin
    Line := AddLine;
    Line.StartBlock := LineIndex;
    Line.EndBlock := I - 1;
    Line.StartIndex := LastIndex;
    Line.EndIndex := CurIndex - 1;
  end;
  FSelectableLength := CurIndex;
end;

function TKMemoBlocks.UpdateUnlocked: Boolean;
begin
  Result := FUpdateLock <= 0;
end;

end.

