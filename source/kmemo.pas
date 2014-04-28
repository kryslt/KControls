{ @abstract(This unit contains native replacement for TMemo/TRichEdit component)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(28 Apr 2008)
  @lastmod(10 May 2008)

  Copyright © 2008 Tomas Krysl (tk@@tkweb.eu)<BR><BR>

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

{$include KControls.inc}

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

  { Minimum for the @link(TKCustomMemo.ScrollSpeed) property. }
  cScrollSpeedMin = 50;
  { Maximum for the @link(TKCustomMemo.ScrollSpeed) property. }
  cScrollSpeedMax = 1000;
  { Default value for the @link(TKCustomMemo.ScrollSpeed) property. }
  cScrollSpeedDef = 100;

  { Default value for the @link(TKMemoColors.BkGnd) color property }
  cBkGndDef = clWindow;
  { Default value for the @link(TKMemoColors.InactiveCaretBkGnd) color property }
  cInactiveCaretBkGndDef = clBlack;
  { Default value for the @link(TKMemoColors.InactiveCaretSelBkGnd) color property }
  cInactiveCaretSelBkGndDef = clBlack;
  { Default value for the @link(TKMemoColors.InactiveCaretSelText) color property }
  cInactiveCaretSelTextDef = clYellow;
  { Default value for the @link(TKMemoColors.InactiveCaretText) color property }
  cInactiveCaretTextDef = clYellow;
  { Default value for the @link(TKMemoColors.SelBkGnd) color property }
  cSelBkGndDef = clGrayText;
  { Default value for the @link(TKMemoColors.SelBkGndFocused) color property }
  cSelBkGndFocusedDef = clHighlight;
  { Default value for the @link(TKMemoColors.SelText) color property }
  cSelTextDef = clHighlightText;
  { Default value for the @link(TKMemoColors.SelTextFocused) color property }
  cSelTextFocusedDef = clHighlightText;

  { Index for the @link(TKMemoColors.BkGnd) color property }
  ciBkGnd = TKColorIndex(0);
  { Index for the @link(TKMemoColors.InactiveCaretBkGnd) color property }
  ciInactiveCaretBkGnd = TKColorIndex(1);
  { Index for the @link(TKMemoColors.InactiveCaretSelBkGnd) color property }
  ciInactiveCaretSelBkGnd = TKColorIndex(2);
  { Index for the @link(TKMemoColors.InactiveCaretSelText) color property }
  ciInactiveCaretSelText = TKColorIndex(3);
  { Index for the @link(TKMemoColors.InactiveCaretText) color property }
  ciInactiveCaretText = TKColorIndex(4);
  { Index for the @link(TKMemoColors.SelBkGnd) color property }
  ciSelBkGnd = TKColorIndex(5);
  { Index for the @link(TKMemoColors.SelBkGndFocused) color property }
  ciSelBkGndFocused = TKColorIndex(6);
  { Index for the @link(TKMemoColors.SelText) color property }
  ciSelText = TKColorIndex(7);
  { Index for the @link(TKMemoColors.SelTextFocused) color property }
  ciSelTextFocused = TKColorIndex(8);
  { Maximum color array index }
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

  { Declares memo states - possible values for the @link(TKCustomHexEditor.States) property
    (protected) }
  TKMemoState = (
    { Caret is created }
    elCaretCreated,
    { Caret is visible }
    elCaretVisible,
    { Caret is being updated }
    elCaretUpdate,
    { Ignore following WM_CHAR message }
    elIgnoreNextChar,
    { Buffer modified }
    elModified,
    { Mouse captured }
    elMouseCapture,
    { Overwrite mode active }
    elOverwrite,
    { Read only editor }
    elReadOnly
  );

  { Hex editor states can be arbitrary combined }
  TKMemoStates = set of TKMemoState;

  TKMemoUpdateReason = (
    // attributes changed that do not affect extent and contents
    muAttributes,
    // recalculate extents
    muExtent,
    // recalculate line info and extents
    muContents,
    // selection changed
    muSelection
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
    FNewLine: Boolean;
    FParent: TKMemoBlocks;
    FSelEnd: Integer;
    FSelStart: Integer;
    procedure SetBounds(const Value: TRect);
    procedure SetNewLine(const Value: Boolean);
    procedure SetParent(AParent: TKMemoBlocks);
  protected
    function ContentLength: Integer; virtual;
    function GetCanAddText: Boolean; virtual;
    procedure GetSelColors(var TextColor, Background: TColor); virtual;
    function GetSelLength: Integer; virtual;
    function GetSelStart: Integer; virtual;
    function GetSelText: TString; virtual;
    function GetText: TString; virtual;
    procedure NotifyLineInfo(const ALineInfo: TKMemoLine); virtual;
    procedure ParentChanged; virtual;
    procedure SetOffset(ALeft, ATop: Integer); virtual;
    procedure Select(ASelStart, ASelLength: Integer); virtual;
    function SelectableLength: Integer; virtual;
    procedure Update(AReasons: TKMemoUpdateReasons); virtual;
  public
    constructor Create(AParent: TKMemoBlocks); virtual;
    function AddText(const AText: TString; At: Integer = -1): Boolean; virtual;
    procedure Assign(AItem: TKMemoBlock); virtual;
    procedure ClearSelection; virtual;
    function Concat(AItem: TKMemoBlock): Boolean; virtual;
    function IndexToRect(ACanvas: TCanvas; AIndex: Integer): TRect; virtual;
    procedure MeasureExtent(ACanvas: TCanvas); virtual;
    function NewLineSelected: Boolean; virtual;
    procedure NotifyFontChange(AFont: TFont); virtual;
    procedure PaintToCanvas(ACanvas: TCanvas); virtual;
    function PointToIndex(ACanvas: TCanvas; const APoint: TPoint): Integer; virtual;
    function Split(At: Integer): TKMemoBlock; virtual;
    property Bounds: TRect read FBounds write SetBounds;
    property CanAddText: Boolean read GetCanAddText;
    property NewLine: Boolean read FNewLine write SetNewLine;
    property Parent: TKMemoBlocks read FParent write SetParent;
    property SelLength: Integer read GetSelLength;
    property SelStart: Integer read GetSelStart;
    property SelText: TString read GetSelText;
    property Text: TString read GetText;
  end;

  { TKTextMemoBlock }

  TKTextMemoBlock = class(TKMemoBlock)
  private
    FBrush: TBrush;
    FFont: TFont;
    FHAlign: TKHAlign;
    FText: TString;
    FVAlign: TKVAlign;
    procedure SetText(const Value: TString);
    procedure SetHAlign(const Value: TKHAlign);
    procedure SetVAlign(const Value: TKVAlign);
    procedure SplitText(At: Integer; out APart1, APart2: TString);
  protected
    FFontChanged: Boolean;
    function ContentLength: Integer; override;
    procedure FontChange(Sender: TObject); virtual;
    function GetCanAddText: Boolean; override;
    function GetSelText: TString; override;
    function GetText: TString; override;
    class function IndexToTextIndex(var AText: TString; AIndex: Integer): Integer; virtual;
    procedure ParentChanged; override;
    class function TextIndexToIndex(var AText: TString; ATextIndex: Integer): Integer; virtual;
  public
    constructor Create(AParent: TKMemoBlocks); override;
    destructor Destroy; override;
    function AddText(const AText: TString; At: Integer): Boolean; override;
    procedure Assign(AItem: TKMemoBlock); override;
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
    property Text: TString read FText write SetText;
    property VAlign: TKVAlign read FVAlign write SetVAlign;
  end;

  TKImageMemoBlock = class(TKMemoBlock)
  private
    FImage: TPicture;
    procedure SetImage(const Value: TPicture);
    procedure SetImagePath(const Value: TString);
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
    property Path: TString write SetImagePath;
  end;

  TKMemoBlocks = class(TObjectList)
  private
    FExtentX: Integer;
    FExtentY: Integer;
    FMemo: TKCustomMemo;
  protected
    FLines: TKMemoLines;
    FLineCount: Integer;
    FSelectableLength: Integer;
    FUpdateLock: Integer;
    FUpdateReasons: TKMemoUpdateReasons;
    function AddLine: PKMemoLine;
    function GetDefaultFont: TFont; virtual;
    function GetLineBottom(AIndex: Integer): Integer; virtual;
    function GetLineEndIndex(AIndex: Integer): Integer; virtual;
    function GetLineInfo(AIndex: Integer): PKMemoLine;
    function GetLineLeft(AIndex: Integer): Integer; virtual;
    function GetLineRight(AIndex: Integer): Integer; virtual;
    function GetLines(AIndex: Integer): TString; virtual;
    function GetLineSize(AIndex: Integer): Integer; virtual;
    function GetLineStartIndex(AIndex: Integer): Integer; virtual;
    function GetLineTop(AIndex: Integer): Integer; virtual;
    procedure GetSelColors(var TextColor, Background: TColor); virtual;
    function GetSelText: TString; virtual;
    function GetText: TString; virtual;
    function LineToRect(ACanvas: TCanvas; AIndex, ALine: Integer; ACaret: Boolean): TRect; virtual;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure Select(ASelStart, ASelLength: Integer); virtual;
    procedure SetLines(AIndex: Integer; const AValue: TString); virtual;
    procedure SetText(const AValue: TString);
    procedure Update(AReasons: TKMemoUpdateReasons); virtual;
    procedure UpdateLineInfo; virtual;
  public
    constructor Create(AMemo: TKCustomMemo); virtual;
    destructor Destroy; override;
    function AddAt(AObject: TKMemoBlock; At: Integer = -1): Integer;
    function AddImageBlock(APath: TString; At: Integer = -1): TKImageMemoBlock;
    function AddTextBlock(AText: TString; At: Integer = -1): TKTextMemoBlock;
    procedure ClearSelection; virtual;
    function IndexToBlock(AIndex: Integer; out ALocalIndex: Integer): Integer; virtual;
    function IndexToLine(AIndex: Integer): Integer; virtual;
    function IndexToRect(ACanvas: TCanvas; AIndex: Integer; ACaret: Boolean): TRect; virtual;
    function InsertNewLine(AIndex: Integer): Boolean; virtual;
    function InsertString(AIndex: Integer; const AValue: TString): Boolean; virtual;
    function LineToIndex(ALineIndex: Integer): Integer; virtual;
    procedure LockUpdate;
    procedure MeasureExtent(ACanvas: TCanvas; ALeft, ATop: Integer); virtual;
    procedure NotifyFontChange(AFont: TFont); virtual;
    function NextIndexByHorzExtent(ACanvas: TCanvas; AIndex, AWidth: Integer;
      ACaret: Boolean): Integer; virtual;
    function NextIndexByRowDelta(ACanvas: TCanvas; AIndex, ARowDelta, ALeftPos: Integer;
      ACaret: Boolean): Integer; virtual;
    function NextIndexByVertExtent(ACanvas: TCanvas; AIndex, AHeight, ALeftPos: Integer;
      ACaret: Boolean): Integer; virtual;
    function NextIndexByVertValue(ACanvas: TCanvas; AIndex, AValue, ALeftPos: Integer;
      ADirection, ACaret: Boolean): Integer; virtual;
    procedure PaintToCanvas(ACanvas: TCanvas; AWidth, AHeight: Integer); virtual;
    function PointToIndex(ACanvas: TCanvas; const APoint: TPoint; AOutOfArea, ACaret: Boolean): Integer; virtual;
    function PointToLineIndex(ACanvas: TCanvas; ALine: Integer; const APoint: TPoint;
      AOutOfArea, ACaret: Boolean): Integer; virtual;
    procedure UnlockUpdate;
    function UpdateUnlocked: Boolean;
    property DefaultFont: TFont read GetDefaultFont;
    property ExtentX: Integer read FExtentX;
    property ExtentY: Integer read FExtentY;
    property LineBottom[Index: Integer]: Integer read GetLineBottom;
    property LineCount: Integer read FLineCount;
    property LineEndIndex[Index: Integer]: Integer read GetLineEndIndex;
    property LineInfo[Index: Integer]: PKMemoLine read GetLineInfo;
    property LineLeft[Index: Integer]: Integer read GetLineLeft;
    property LineRight[Index: Integer]: Integer read GetLineRight;
    property LineTop[Index: Integer]: Integer read GetLineTop;
    property Lines[Index: Integer]: TString read GetLines write SetLines;
    property LineSize[Index: Integer]: Integer read GetLineSize;
    property LineStartIndex[Index: Integer]: Integer read GetLineStartIndex;
    property Text: TString read GetText write SetText;
    property SelectableLength: Integer read FSelectableLength;
    property SelText: TString read GetSelText;
  end;

  { @abstract(Container for all colors used by @link(TKCustomMemo) class)
    This container allows to group many colors into one item in object inspector.
    Colors are accessible via published properties or several public Color*
    properties.
  }
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
    { Hex editor client area background }
    property BkGnd: TColor index ciBkGnd read GetColor write SetColor default cBkGndDef;
    { Inactive (memo without focus) caret background color - caret mark is not part of a selection }
    property InactiveCaretBkGnd: TColor index ciInactiveCaretBkGnd read GetColor write SetColor default cInactiveCaretBkGndDef;
    { Inactive (memo without focus) caret background color - caret mark is part of a selection }
    property InactiveCaretSelBkGnd: TColor index ciInactiveCaretSelBkGnd read GetColor write SetColor default cInactiveCaretSelBkGndDef;
    { Inactive (memo without focus) caret text color - caret mark is part of a selection }
    property InactiveCaretSelText: TColor index ciInactiveCaretSelText read GetColor write SetColor default cInactiveCaretSelTextDef;
    { Inactive (memo without focus) caret text color - caret mark is not part of a selection }
    property InactiveCaretText: TColor index ciInactiveCaretText read GetColor write SetColor default cInactiveCaretTextDef;
    { Selection background - inactive edit area }
    property SelBkGnd: TColor index ciSelBkGnd read GetColor write SetColor default cSelBkGndDef;
    { Selection background - active edit area }
    property SelBkGndFocused: TColor index ciSelBkGndFocused read GetColor write SetColor default cSelBkGndFocusedDef;
    { Selection text - inactive edit area }
    property SelText: TColor index ciSelText read GetColor write SetColor default cSelTextDef;
    { Selection text - active edit area }
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
      </UL>}
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
      </UL>}
    procedure AddChange(ItemKind: TKMemoChangeKind; Inserted: Boolean = True); virtual;
    { Tells the undo list a new undo/redo group is about to be created. Each
      BeginGroup call must have a corresponding EndGroup call (use try-finally).
      BeginGroup calls may be nested, however, only the first call will create an
      undo/redo group. Use the GroupKind parameter to specify the reason of this group. }
    procedure BeginGroup(GroupKind: TKMemoChangeKind); virtual;
    { Informs whether there are any undo/redo items available - i.e. CanUndo/CanRedo}
    function CanPeek: Boolean;
    { Clears the entire list - overriden to execute some adjustments. }
    procedure Clear; override;
    { Completes the undo/redo group. See @link(TKMemoChangeList.BeginGroup) for details. }
    procedure EndGroup; virtual;
    { Returns the topmost item to handle or inspect it}
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
    { Allows to call TKCustomMemo.@link(TKCustomMemo.OnChange) event}
    property OnChange: TKMemoUndoChangeEvent read FOnChange write FOnChange;
  end;

 { @abstract(Multi line text editor base component). }

  { TKCustomMemo }

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
    FScrollSpeed: Cardinal;
    FScrollTimer: TTimer;
    FSelEnd: Integer;
    FSelStart: Integer;
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
    function GetContentLeft: Integer;
    function GetContentTop: Integer;
    function GetEmpty: Boolean;
    function GetInsertMode: Boolean;
    function GetModified: Boolean;
    function GetReadOnly: Boolean;
    function GetSelLength: Integer;
    function GetSelText: TString;
    function GetText: TString;
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
    procedure SetScrollSpeed(Value: Cardinal);
    procedure SetSelEnd(Value: Integer);
    procedure SetSelLength(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetText(const Value: TString);
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
      </UL>}
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
      </UL>}
    procedure AddUndoString(AItemKind: TKMemoChangeKind; const AData: TString; AInserted: Boolean = True); virtual;
    { Begins a new undo group. Use the GroupKind parameter to label it. }
    procedure BeginUndoGroup(AGroupKind: TKMemoChangeKind);
    { Called to reflect block changes. }
    procedure BlocksChanged(AReasons: TKMemoUpdateReasons); virtual;
    { Determines whether an ecScroll* command can be executed. }
    function CanScroll(ACommand: TKEditCommand): Boolean; virtual;
    { Called by ContentPadding class to update the memo control. }
    procedure ContentPaddingChanged(Sender: TObject); virtual;
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
    { Returns the current key stroke mapping scheme. }
    function GetKeyMapping: TKEditKeyMapping;
    { Hides the caret. }
    procedure HideEditorCaret; virtual;
    { Overriden method - processes virtual key strokes according to current @link(TKCustomMemo.KeyMapping). }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
 {$IFDEF FPC}
    { Overriden method - processes character key strokes - data editing }
    procedure UTF8KeyPress(var Key: TUTF8Char); override;
 {$ELSE}
    { Overriden method - processes character key strokes - data editing }
    procedure KeyPress(var Key: Char); override;
 {$ENDIF}
    procedure LimitSelection(var ASelStart, ASelEnd: Integer);
    { Overriden method - updates caret position/selection. }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - updates caret position/selection and initializes scrolling
      when needed. }
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - releases mouse capture acquired by MouseDown }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - calls PaintLines to paint text lines into window client area }
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    { Grants the input focus to the control when possible and the control has had none before }
    procedure SafeSetFocus;
    { Scrolls the text either horizontally by DeltaHorz scroll units or vertically
      by DeltaVert scroll units (lines) or in both directions. CodeHorz and CodeVert
      are the codes coming from WM_HSCROLL or WM_VSCROLL messages. }
    procedure Scroll(CodeHorz, CodeVert, DeltaHorz, DeltaVert: Integer; CallScrollWindow: Boolean); virtual;
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
    procedure UpdateEditorCaret; virtual;
    { Updates the scrolling range }
    procedure UpdateScrollRange(CallInvalidate: Boolean); virtual;
    { Updates the grid size. }
    procedure UpdateSize; override;
    { Redo list manager - made accessible for descendant classes }
    property RedoList: TKMemoChangeList read FRedoList;
    { States of this class - made accessible for descendant classes }
    property States: TKMemoStates read FStates write FStates;
    { Undo list manager - made accessible for descendant classes }
    property UndoList: TKMemoChangeList read FUndoList;
  public
    { Performs necessary initializations - default values to properties, create
      undo/redo list managers }
    constructor Create(AOwner: TComponent); override;
    { Destroy instance, undo/redo list managers, dispose buffer... }
    destructor Destroy; override;
    { Takes property values from another TKCustomMemo class }
    procedure Assign(Source: TPersistent); override;
    { Gives access to memo blocks - containers of texts, images etc.. }
    property Blocks: TKMemoBlocks read FBlocks;
    { Determines whether the caret is visible }
    function CaretInView: Boolean;
    { Forces the caret position to become visible. }
    function ClampInView: Boolean;
    { Clears all blocks. Unlike @link(ecClearAll) clears everything inclusive undo a redo lists. }
    procedure Clear;
    { Deletes blocks or parts of blocks corresponding to the active selection. }
    procedure ClearSelection; virtual;
    { Clears undo (and redo) list }
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
    { Returns "real" selection end - with always higher index value than selection
      start value }
    function GetRealSelEnd: Integer;
    { Returns "real" selection start - with always lower index value than selection
      end value }
    function GetRealSelStart: Integer;
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
    procedure InsertString(At: Integer; const AValue: TString); virtual;
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
    { Returns current caret position = selection end }
    property CaretPos: Integer read FSelEnd;
    { Returns True if caret is visible }
    property CaretVisible: Boolean read GetCaretVisible;
    { Makes it possible to take all color properties from another TKCustomMemo class }
    property Colors: TKMemoColors read FColors write SetColors;
    { Specifies the padding around the memo contents. }
    property ContentPadding: TKRect read FContentPadding;
    { Returns the left position of the memo contents. }
    property ContentLeft: Integer read GetContentLeft;
    { Returns the top position of the memo contents. }
    property ContentTop: Integer read GetContentTop;
    { Specifies the style how the outline is drawn when editor is disabled. }
    property DisabledDrawStyle: TKEditDisabledDrawStyle read FDisabledDrawStyle write SetDisabledDrawStyle default cDisabledDrawStyleDef;
    { Returns True if text buffer is empty. }
    property Empty: Boolean read GetEmpty;
    { Returns True if insert mode is on }
    property InsertMode: Boolean read GetInsertMode;
    { Specifies the current key stroke mapping scheme }
    property KeyMapping: TKEditKeyMapping read FKeyMapping;
    { Specifies the horizontal scroll position }
    property LeftPos: Integer read FLeftPos write SetLeftPos;
    { Returns True if the buffer was modified - @link(eoUndoAfterSave) taken into
      account }
    property Modified: Boolean read GetModified write SetModified;
    { Specifies the editor options that do not affect painting }
    property Options: TKEditOptions read FOptions write SetOptions stored IsOptionsStored;
    { Specifies whether the editor has to be read only editor }
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    { Defines visible scrollbars - horizontal, vertical or both }
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    { Specifies how fast the scrolling by timer should be }
    property ScrollSpeed: Cardinal read FScrollSpeed write SetScrollSpeed default cScrollSpeedDef;
    { Specifies the current selection end }
    property SelEnd: Integer read FSelEnd write SetSelEnd;
    { Specifies the current selection length. SelStart remains unchanged, SelEnd will be
      updated accordingly. To mark a selection, either set both SelStart and SelEnd properties
      or both SelStart and SelLength properties }
    property SelLength: Integer read GetSelLength write SetSelLength;
    { Specifies the current selection start }
    property SelStart: Integer read FSelStart write SetSelStart;
    { Returns selected text }
    property SelText: TString read GetSelText;
    { If read, returns the textual part of the contents as a whole. If written, replace previous contents by a new one. }
    property Text: TString read GetText write SetText;
    { Specifies the vertical scroll position }
    property TopPos: Integer read FTopPos write SetTopPos;
    { Specifies the maximum number of undo items. Please note this value
      affects the undo item limit, not undo group limit. }
    property UndoLimit: Integer read GetUndoLimit write SetUndoLimit default cUndoLimitDef;
    { When assigned, this event will be invoked at each change made to the
      text buffer either by the user or programmatically by public functions. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { When assigned, this event will be invoked when the user drops any files onto
      the window }
    property OnDropFiles: TKEditDropFilesEvent read FOnDropFiles write FOnDropFiles;
    { When assigned, this event will be invoked at each prompt-forced search match }
    property OnReplaceText: TKEditReplaceTextEvent read FOnReplaceText write FOnReplaceText;
  end;

  { @abstract(Memo design-time component) }
  TKMemo = class(TKCustomMemo)
  published
    { Inherited property - see Delphi help }
    property Align;
    { Inherited property - see Delphi help }
    property Anchors;
    { See TKCustomControl.@link(TKCustomControl.BorderStyle) for details }
    property BorderStyle;
    { Inherited property - see Delphi help }
    property BorderWidth;
    { See TKCustomMemo.@link(TKCustomMemo.Colors) for details }
    property Colors;
    { Inherited property - see Delphi help }
    property Constraints;
    { See TKCustomMemo.@link(TKCustomMemo.ContentPadding) for details }
    property ContentPadding;
  {$IFNDEF FPC}
    { Inherited property - see Delphi help. }
    property Ctl3D;
  {$ENDIF}
    { See TKCustomMemo.@link(TKCustomMemo.DisabledDrawStyle) for details }
    property DisabledDrawStyle;
    { Inherited property - see Delphi help }
    property DragCursor;
    { Inherited property - see Delphi help }
    property DragKind;
    { Inherited property - see Delphi help }
    property DragMode;
    { Inherited property - see Delphi help }
    property Enabled;
    { Inherited property - see Delphi help. Font pitch must always remain fpFixed
      - specify fixed fonts only. Font.Size will also be trimmed if too small or big }
    property Font;
    { Inherited property - see Delphi help }
    property Height default cHeight;
    { See TKCustomMemo.@link(TKCustomMemo.Options) for details }
    property Options;
    { Inherited property - see Delphi help }
    property ParentShowHint;
    { Inherited property - see Delphi help }
    property PopupMenu;
    { See TKCustomMemo.@link(TKCustomMemo.ReadOnly) for details }
    property ReadOnly;
    { See TKCustomMemo.@link(TKCustomMemo.ScrollBars) for details }
    property ScrollBars;
    { See TKCustomMemo.@link(TKCustomMemo.ScrollSpeed) for details }
    property ScrollSpeed;
    { Inherited property - see Delphi help }
    property ShowHint;
    { Inherited property - see Delphi help }
    property TabOrder;
    { Inherited property - see Delphi help }
    property TabStop default True;
    { See TKCustomMemo.@link(TKCustomMemo.Text) for details }
    property Text;
    { See TKCustomMemo.@link(TKCustomMemo.UndoLimit) for details }
    property UndoLimit;
    { Inherited property - see Delphi help }
    property Visible;
    { Inherited property - see Delphi help }
    property Width default cWidth;
    { See TKCustomMemo.@link(TKCustomMemo.OnChange) for details }
    property OnChange;
    { Inherited property - see Delphi help }
    property OnClick;
    { Inherited property - see Delphi help }
    property OnContextPopup;
    { Inherited property - see Delphi help }
    property OnDblClick;
    { Inherited property - see Delphi help }
    property OnDockDrop;
    { Inherited property - see Delphi help }
    property OnDockOver;
    { Inherited property - see Delphi help }
    property OnDragDrop;
    { Inherited property - see Delphi help }
    property OnDragOver;
    { See TKCustomMemo.@link(TKCustomMemo.OnDropFiles) for details }
    property OnDropFiles;
    { Inherited property - see Delphi help }
    property OnEndDock;
    { Inherited property - see Delphi help }
    property OnEndDrag;
    { Inherited property - see Delphi help }
    property OnEnter;
    { Inherited property - see Delphi help }
    property OnExit;
    { Inherited property - see Delphi help }
    property OnGetSiteInfo;
    { Inherited property - see Delphi help }
    property OnKeyDown;
    { Inherited property - see Delphi help }
    property OnKeyPress;
    { Inherited property - see Delphi help }
    property OnKeyUp;
    { Inherited property - see Delphi help }
    property OnMouseDown;
    { Inherited property - see Delphi help }
    property OnMouseMove;
    { Inherited property - see Delphi help }
    property OnMouseUp;
    { Inherited property - see Delphi help }
    property OnMouseWheel;
    { Inherited property - see Delphi help }
    property OnMouseWheelDown;
    { Inherited property - see Delphi help }
    property OnMouseWheelUp;
    { See TKCustomMemo.@link(TKCustomMemo.OnReplaceText) for details }
    property OnReplaceText;
    { Inherited property - see Delphi help }
    property OnResize;
    { Inherited property - see Delphi help }
    property OnStartDock;
    { Inherited property - see Delphi help }
    property OnStartDrag;
    { Inherited property - see Delphi help }
    property OnUnDock;
  end;

implementation

uses
  ShellApi, ClipBrd, Printers,
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
    Result := inherited;
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
  DoubleBuffered := True;
  Font.OnChange := FontChange;
  Height := cHeight;
  ParentColor := False;
  ParentFont := False;
  TabStop := True;
  Width := cWidth;
  FBackgroundImage := TPicture.Create;
  FBlocks := TKMemoBlocks.Create(Self);
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
  FScrollSpeed := cScrollSpeedDef;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := FScrollSpeed;
  FScrollTimer.OnTimer := ScrollTimerHandler;
  FSelStart := 0;
  FSelEnd := 0;
  FStates := [];
  FTopPos := 0;
  FUndoList := TKMemoChangeList.Create(Self, FRedoList);
  FUndoList.OnChange := UndoChange;
  FUpdateLock := 0;
  FVertScrollStep := cVertScrollStepDef;
  FOnChange := nil;
  FOnReplaceText := nil;
  Clear;
  UpdateEditorCaret;
end;

destructor TKCustomMemo.Destroy;
begin
  FOnChange := nil;
  FUndoList.Free;
  FRedoList.Free;
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

procedure TKCustomMemo.AddUndoString(AItemKind: TKMemoChangeKind; const AData: TString; AInserted: Boolean = True);
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
      Self.KeyMapping.Assign(KeyMapping);
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
    if AReasons * [muContents, muExtent] <> [] then
    begin
      LimitSelection(FSelStart, FSelEnd);
      UpdateScrollRange(True);
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
      R := IndexToRect(FSelEnd, False);
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
  Result := PtInRect(ClientRect, IndexToRect(FSelEnd, False).TopLeft);
end;

function TKCustomMemo.ClampInView: Boolean;
var
  DeltaHorz, DeltaVert: Integer;
begin
  Result := ScrollNeeded(DeltaHorz, DeltaVert);
  if Result then
  begin
    Scroll(cScrollNoAction, cScrollNoAction, DeltaHorz, DeltaVert, True);
    FScrollTimer.Enabled := True;
  end;
end;

procedure TKCustomMemo.Clear;
begin
  FBlocks.LockUpdate;
  try
    Select(0, 0, False);
    FBlocks.Clear;
    TextChanged;
  finally
    FBlocks.UnlockUpdate;
  end;
end;

procedure TKCustomMemo.ClearSelection;
begin
  AddUndoString(ckInsert, FBlocks.SelText);
  FBlocks.ClearSelection;
  Select(GetRealSelStart, 0);
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
  SelLen: Integer;
begin
  if Enabled and Visible and not (csDesigning in ComponentState) then
  begin
    SelLen := SelLength;
    case Command of
      // movement commands
      ecLeft, ecSelLeft:
        Result := FSelEnd > 0;
      ecRight:
        Result := FSelEnd < FBlocks.SelectableLength - 1;
      ecSelRight:
        Result := FSelEnd < FBlocks.SelectableLength;
      ecUp, ecSelUp, ecPageUp, ecSelPageUp:
        Result := FSelEnd >= FBlocks.LineSize[0];
      ecDown, ecPagedown:
        Result := FSelEnd < FBlocks.SelectableLength - FBlocks.LineSize[FBlocks.LineCount - 1];
      ecSelDown, ecSelPageDown:
        Result := FSelEnd <= FBlocks.SelectableLength - FBlocks.LineSize[FBlocks.LineCount - 1];
      ecLineStart, ecPageLeft:
        Result := FSelEnd > FBlocks.LineStartIndex[FBlocks.IndexToLine(FSelEnd)];
      ecSelLineStart, ecSelPageLeft:
        Result := FSelEnd > FBlocks.LineStartIndex[FBlocks.IndexToLine(Max(FSelEnd - 1, 0))];
      ecLineEnd, ecPageRight:
        Result := FSelEnd < FBlocks.LineEndIndex[FBlocks.IndexToLine(FSelEnd)];
      ecSelLineEnd, ecSelPageRight:
        Result := FSelEnd <= FBlocks.LineEndIndex[FBlocks.IndexToLine(FSelEnd)];
      ecPageTop, ecSelPageTop:
        Result := FSelEnd <> FBlocks.NextIndexByVertValue(Canvas, FSelEnd, 0, FPreferredCaretPos, False, Command = ecPageTop);
      ecPageBottom, ecSelPageBottom:
        Result := FSelEnd <> FBlocks.NextIndexByVertValue(Canvas, FSelEnd, ClientHeight, FPreferredCaretPos, True,
          Command = ecPageBottom);
      ecEditorTop, ecSelEditorTop:
        Result := FSelEnd > 0;
      ecEditorBottom:
        Result := FSelEnd < FBlocks.SelectableLength - 1;
      ecSelEditorBottom:
        Result := FSelEnd < FBlocks.SelectableLength;
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
        Result := not Empty and (not ReadOnly or (Command = ecCopy)) and (SelLen <> 0);
      ecPaste:
        Result := not ReadOnly and (ClipBoard.FormatCount > 0);
      ecInsertChar, ecInsertString, ecInsertNewLine:
        Result := not ReadOnly;
      ecDeleteLastChar:
        Result := not (Empty or ReadOnly) and ((SelLen > 0) or (FSelEnd > 0));
      ecDeleteChar:
        Result := not (Empty or ReadOnly) and ((SelLen > 0) or (FSelEnd < FBlocks.SelectableLength - 1));
      ecDeleteBOL:
        Result := not (Empty or ReadOnly) and ((SelLen > 0) or (FSelEnd <> FBlocks.LineStartIndex[FBlocks.IndexToLine(FSelEnd)]));
      ecDeleteEOL:
        Result := not (Empty or ReadOnly) and ((SelLen > 0) or (FSelEnd <> FBlocks.LineEndIndex[FBlocks.IndexToLine(FSelEnd)]));
      ecDeleteLine, ecSelectAll, ecClearAll, ecReplace:
        Result := not (Empty or ReadOnly);
      ecClearSelection:
        Result := not (Empty or ReadOnly) and (SelLen > 0);
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
  if (eoDropFiles in FOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, TRUE);
end;

procedure TKCustomMemo.DeleteBOL(At: Integer);
var
  LineStart: Integer;
begin
  BeginUndoGroup(ckDelete);
  try
    AddUndoCaretPos;
    LineStart := FBlocks.LineStartIndex[FBlocks.IndexToLine(At)];
    Select(LineStart, At - LineStart);
    ClearSelection;
  finally
    EndUndoGroup;
  end;
end;

procedure TKCustomMemo.DeleteChar(At: Integer);
begin
  BeginUndoGroup(ckDelete);
  try
    AddUndoCaretPos;
    Select(At + 1, -1);
    ClearSelection;
  finally
    EndUndoGroup;
  end;
end;

procedure TKCustomMemo.DeleteEOL(At: Integer);
var
  LineEnd: Integer;
begin
  BeginUndoGroup(ckDelete);
  try
    AddUndoCaretPos;
    LineEnd := FBlocks.LineEndIndex[FBlocks.IndexToLine(At)];
    Select(LineEnd, At - LineEnd);
    ClearSelection;
  finally
    EndUndoGroup;
  end;
end;

procedure TKCustomMemo.DeleteLastChar(At: Integer);
begin
  BeginUndoGroup(ckDelete);
  try
    AddUndoCaretPos;
    Select(At - 1, 1);
    ClearSelection;
  finally
    EndUndoGroup;
  end;
end;

procedure TKCustomMemo.DeleteLine(At: Integer);
var
  Line, LineStart, LineEnd: Integer;
begin
  BeginUndoGroup(ckDelete);
  try
    AddUndoCaretPos;
    Line := FBlocks.IndexToLine(At);
    LineStart := FBlocks.LineStartIndex[Line];
    if Line < FBlocks.LineCount - 1 then
      LineEnd := FBlocks.LineStartIndex[Line + 1]
    else
      LineEnd := FBlocks.LineEndIndex[Line];
    Select(LineStart, LineEnd - LineStart);
    ClearSelection;
  finally
    EndUndoGroup;
  end;
end;

procedure TKCustomMemo.DestroyWnd;
begin
  if (eoDropFiles in FOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, FALSE);
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
  I, J, K, M, N, O: Integer;
  CanInsert, MoreBytes, Found, PAbort, MatchCase: Boolean;
  C1, C2, C3: Char;
  S, S_FirstChar, S_LastChar, T: TString;
  BA: PByteArray;
  P: TPoint;
  L, OldSelStart, OldSelEnd, Sel1, Sel2: Integer;
  PChI, PChI_First, PChI_Next: PKMemoChangeItem;
  ReplaceAction: TKEditReplaceAction;
  H: THandle;
begin
  Result := False;
  if CommandEnabled(Command) then
  begin
    Result := True;
    L := SelLength;
    case Command of
      ecLeft..ecSelGotoXY, ecSelectAll:
        AddUndoCaretPos(False);
    end;
    case Command of
      ecLeft: SelectionInit(FSelEnd - 1);
      ecSelLeft: SelectionExpand(FSelEnd - 1);
      ecRight: SelectionInit(FSelEnd + 1);
      ecSelRight: SelectionExpand(FSelEnd + 1);
      ecUp: SelectionInit(FBlocks.NextIndexByRowDelta(Canvas, FSelEnd, -1, FPreferredCaretPos, True));
      ecSelUp: SelectionExpand(FBlocks.NextIndexByRowDelta(Canvas, FSelEnd, -1, FPreferredCaretPos, False));
      ecDown: SelectionInit(FBlocks.NextIndexByRowDelta(Canvas, FSelEnd, 1, FPreferredCaretPos, True));
      ecSelDown: SelectionExpand(FBlocks.NextIndexByRowDelta(Canvas, FSelEnd, 1, FPreferredCaretPos, False));
      ecLineStart: SelectionInit(FBlocks.LineStartIndex[FBlocks.IndexToLine(FSelEnd)]);
      ecSelLineStart: SelectionExpand(FBlocks.LineStartIndex[FBlocks.IndexToLine(Max(FSelEnd - 1, 0))]);
      ecLineEnd: SelectionInit(FBlocks.LineEndIndex[FBlocks.IndexToLine(FSelEnd)]);
      ecSelLineEnd: SelectionExpand(FBlocks.LineEndIndex[FBlocks.IndexToLine(FSelEnd)] + 1);
      ecPageUp: SelectionInit(FBlocks.NextIndexByVertExtent(Canvas, FSelEnd, -ClientHeight, FPreferredCaretPos, True));
      ecSelPageUp: SelectionExpand(FBlocks.NextIndexByVertExtent(Canvas, FSelEnd, -ClientHeight, FPreferredCaretPos, False));
      ecPageDown: SelectionInit(FBlocks.NextIndexByVertExtent(Canvas, FSelEnd, ClientHeight, FPreferredCaretPos, True));
      ecSelPageDown: SelectionExpand(FBlocks.NextIndexByVertExtent(Canvas, FSelEnd, ClientHeight, FPreferredCaretPos, False));
      ecPageLeft: SelectionInit(FBlocks.NextIndexByHorzExtent(Canvas, FSelEnd, -ClientWidth, True));
      ecSelPageLeft: SelectionExpand(FBlocks.NextIndexByHorzExtent(Canvas, FSelEnd, -ClientWidth, False));
      ecPageRight: SelectionInit(FBlocks.NextIndexByHorzExtent(Canvas, FSelEnd, ClientWidth, True));
      ecSelPageRight: SelectionExpand(FBlocks.NextIndexByHorzExtent(Canvas, FSelEnd, ClientWidth, False));
      ecPageTop: SelectionInit(FBlocks.NextIndexByVertValue(Canvas, FSelEnd, 0, FPreferredCaretPos, False, True));
      ecSelPageTop: SelectionExpand(FBlocks.NextIndexByVertValue(Canvas, FSelEnd, 0, FPreferredCaretPos, False, False));
      ecPageBottom: SelectionInit(FBlocks.NextIndexByVertValue(Canvas, FSelEnd, ClientHeight, FPreferredCaretPos, True, True));
      ecSelPageBottom: SelectionExpand(FBlocks.NextIndexByVertValue(Canvas, FSelEnd, ClientHeight, FPreferredCaretPos, True, False));
      ecEditorTop: SelectionInit(0);
      ecSelEditorTop: SelectionExpand(0);
      ecEditorBottom: SelectionInit(FBlocks.SelectableLength - 1);
      ecSelEditorBottom: SelectionExpand(FBlocks.SelectableLength);
      ecGotoXY: SelectionInit(FBlocks.PointToIndex(Canvas, PPoint(Data)^, True, True));
      ecSelGotoXY: SelectionExpand(FBlocks.PointToIndex(Canvas, PPoint(Data)^, True, False));
      // scroll commands
      ecScrollUp:
      if not ClampInView then
      begin
        ScrollBy(0, -1);
        while CommandEnabled(ecUp) and (FBlocks.LineBottom[FBlocks.IndexToLine(FSelEnd)] > ClientHeight) do
          ExecuteCommand(ecUp);
      end;
      ecScrollDown:
      if not ClampInView then
      begin
        ScrollBy(0, 1);
        while CommandEnabled(ecDown) and (FBlocks.LineTop[FBlocks.IndexToLine(FSelEnd)] < 0) do
          ExecuteCommand(ecDown);
      end;
      ecScrollLeft:
      if not ClampInView then
      begin
        ScrollBy(-1, 0);
        while CommandEnabled(ecLeft) and (FCaretRect.Left + FCaretRect.Right > ClientWidth) do
          ExecuteCommand(ecLeft);
      end;
      ecScrollRight:
      if not ClampInView then
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
                System.Move(PChI.Data[1], FBuffer[Sel1.Index], Min(I, FSize - FSelEnd.Index));
                Invalidate;
              end;
            end;
            crDeleteChar, crDeleteDigits, crDeleteString:
              ClearString(Sel1.Index, I);
          end;
          FSelEnd := PChI.SelEnd;
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
                  Inc(FSelEnd.Index, M div cDigitCount)
                end else
                begin
                  Inc(FSelEnd.Digit, M);
                  if FSelEnd.Digit >= cDigitCount then
                  begin
                    Inc(FSelEnd.Index);
                    FSelEnd.Digit := FSelEnd.Digit mod cDigitCount;
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
            Inc(FSelEnd.Index, Length(S));
            FSelEnd.Digit := 0;
            SelectionChanged(True);
          end;
        end;
      end;}
      ecInsertChar:
      begin
        ClearSelection;
        InsertChar(FSelEnd, PKChar(Data)^);
      end;
      ecInsertString:
      begin
        ClearSelection;
        InsertString(FSelEnd, TString(Data));
      end;
      ecInsertNewLine:
      begin
        ClearSelection;
        InsertNewLine(FSelEnd);
      end;
      ecDeleteLastChar:
      begin
        if SelLength <> 0 then
          ClearSelection
        else
          DeleteLastChar(FSelEnd);
      end;
      ecDeleteChar:
      begin
        if SelLength <> 0 then
          ClearSelection
        else
          DeleteChar(FSelEnd);
      end;
      ecDeleteBOL:
      begin
        if SelLength <> 0 then
          ClearSelection
        else
          DeleteBOL(FSelEnd);
      end;
      ecDeleteEOL:
      begin
        if SelLength <> 0 then
          ClearSelection
        else
          DeleteEOL(FSelEnd);
      end;
      ecDeleteLine:
      begin
        if SelLength <> 0 then
          ClearSelection
        else
          DeleteLine(FSelEnd);
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
                      FSelEnd := MakeSelection(I + N, 0);
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
                            FSelEnd := MakeSelection(I + Length(T), 0);
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
      ecInsertChar, ecInsertString, ecInsertNewLine, ecDeleteLastChar, ecDeleteChar, ecDeleteBOL, ecDeleteEOL,
      ecDeleteLine, ecSelectAll, ecClearAll, ecClearSelection:
        FPreferredCaretPos := FCaretRect.Left;
    end;
{    if (OldSelStart.Index <> OldSelEnd.Index) or (FSelStart.Index <> FSelEnd.Index) or
      (OldSelStart.Digit <> OldSelEnd.Digit) or (FSelStart.Digit <> FSelEnd.Digit) or
      not (elCaretVisible in FStates) and (edInactiveCaret in FDrawStyles) and
      ((FSelStart.Index <> OldSelStart.Index) or (FSelStart.Digit <> OldSelStart.Digit) or
      (FSelEnd.Index <> OldSelEnd.Index) or (FSelEnd.Digit <> OldSelEnd.Digit)) then
      Invalidate;}
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

function TKCustomMemo.GetContentLeft: Integer;
begin
  Result := -FLeftPos * FHorzScrollStep + FContentPadding.Left;
end;

function TKCustomMemo.GetContentTop: Integer;
begin
  Result := -FTopPos * FVertScrollStep + FContentPadding.Top;
end;

function TKCustomMemo.GetEmpty: Boolean;
begin
  Result := FBlocks.Count = 0;
end;

function TKCustomMemo.GetInsertMode: Boolean;
begin
  Result := not (elOverwrite in FStates);
end;

function TKCustomMemo.GetKeyMapping: TKEditKeyMapping;
begin
  Result := FKeyMapping;
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
  if FSelStart <= FSelEnd then
    Result := FSelEnd
  else
    Result := FSelStart;
end;

function TKCustomMemo.GetRealSelStart: Integer;
begin
  if FSelStart <= FSelEnd then
    Result := FSelStart
  else
    Result := FSelEnd;
end;

function TKCustomMemo.GetSelLength: Integer;
begin
  if FSelStart <= FSelEnd then
    Result := FSelEnd - FSelStart
  else
    Result := FSelStart - FSelEnd;
end;

function TKCustomMemo.GetSelText: TString;
begin
  Result := FBlocks.SelText;
end;

function TKCustomMemo.GetText: TString;
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
    if elOverwrite in FStates then
      DeleteChar(At);
    if FBlocks.InsertString(At, AValue) then
    begin
      AddUndoChar(ckDelete, AValue, not (elOverwrite in FStates));
      ExecuteCommand(ecRight);
    end;
  finally
    EndUndoGroup;
  end;
end;

procedure TKCustomMemo.InsertNewLine(At: Integer);
begin
  BeginUndoGroup(ckInsert);
  try
    // always insert (don't overwrite)
    if FBlocks.InsertNewLine(At) then
    begin
//      AddUndoChar(crDeleteChar, AValue, False);
      ExecuteCommand(ecRight);
    end;
  finally
    EndUndoGroup;
  end;
end;

procedure TKCustomMemo.InsertString(At: Integer; const AValue: TString);
begin
  if AValue <> '' then
  begin
    BeginUndoGroup(ckInsert);
    try
      // always insert (don't overwrite)
      if FBlocks.InsertString(At, AValue) then
      begin
        AddUndoString(ckDelete, AValue);
      end;
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

procedure TKCustomMemo.LimitSelection(var ASelStart, ASelEnd: Integer);
var
  SelectableLength: Integer;
begin
  SelectableLength := FBlocks.SelectableLength;
  ASelStart := MinMax(ASelStart, 0, SelectableLength);
  ASelEnd := MinMax(ASelEnd, 0, SelectableLength);
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

procedure TKCustomMemo.Scroll(CodeHorz, CodeVert, DeltaHorz, DeltaVert: Integer; CallScrollWindow: Boolean);

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
  if ScrollHorzAxis or ScrollVertAxis then
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
  R := IndexToRect(FSelEnd, False);
  ScrollBy((R.Left - ClientWidth div 2) div FHorzScrollStep, (R.Top - ClientHeight div 2) div FVertScrollStep);
end;

function TKCustomMemo.ScrollNeeded(out DeltaCol, DeltaRow: Integer): Boolean;
begin
  if FCaretRect.Left < 0 then
    DeltaCol := DivDown(FCaretRect.Left, FHorzScrollStep)
  else if (FCaretRect.Left + FCaretRect.Right > ClientWidth) and (FCaretRect.Left > FHorzScrollStep) then
    DeltaCol := DivUp(FCaretRect.Left + FCaretRect.Right - ClientWidth, FHorzScrollStep)
  else
    DeltaCol := 0;
  if FCaretRect.Top < 0 then
    DeltaRow := DivDown(FCaretRect.Top, FVertScrollStep)
  else if (FCaretRect.Top + FCaretRect.Bottom > ClientHeight) and (FCaretRect.Top > FVertScrollStep) then
    DeltaRow := DivUp(FCaretRect.Top + FCaretRect.Bottom - ClientHeight, FVertScrollStep)
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
var
  NewSelEnd: Integer;
begin
  NewSelEnd := ASelStart + ASelLength;
  LimitSelection(ASelStart, NewSelEnd);
  if (ASelStart <> FSelStart) or (NewSelEnd <> FSelEnd) then
  begin
    FSelStart := ASelStart;
    FSelEnd := NewSelEnd;
    FBlocks.Select(GetRealSelStart, GetSelLength);
    if ASelLength = 0 then
      UpdateEditorCaret;
    if ADoScroll then
      ClampInView;
  end;
end;

procedure TKCustomMemo.SelectionExpand(ASelEnd: Integer; ADoScroll: Boolean);
begin
  Select(FSelStart, ASelEnd - FSelStart, ADoScroll);
end;

procedure TKCustomMemo.SelectionExpand(const APoint: TPoint; ADoScroll: Boolean);
var
  NewSelEnd: Integer;
begin
  NewSelEnd := FBlocks.PointToIndex(Canvas, APoint, True, False);
  Select(FSelStart, NewSelEnd - FSelStart, ADoScroll);
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
begin
  P := Point(X, Y);
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
  UpdateDropFiles: Boolean;
begin
  if Value <> FOptions then
  begin
    UpdateDropFiles := (eoDropFiles in Value) <> (eoDropFiles in FOptions);
    FOptions := Value;
    // (un)register HWND as drop target
    if UpdateDropFiles and not (csDesigning in ComponentState) and HandleAllocated then
      DragAcceptFiles(Handle, (eoDropFiles in fOptions));
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
  if Value <> FSelEnd then
  begin
    Select(FSelStart, Value - FSelStart);
  end;
end;

procedure TKCustomMemo.SetSelLength(Value: Integer);
begin
  if Value <> GetSelLength then
  begin
    Select(FSelStart, Value);
  end;
end;

procedure TKCustomMemo.SetSelStart(Value: Integer);
begin
  if Value <> FSelStart then
  begin
    Select(Value, FSelEnd - Value);
  end;
end;

procedure TKCustomMemo.SetText(const Value: TString);
begin
  Clear;
  FBlocks.Text := Value;
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

procedure TKCustomMemo.UpdateEditorCaret;
var
  R: TRect;
begin
  Include(FStates, elCaretUpdate);
  try
    if Enabled and Focused and not (csDesigning in ComponentState) then
    begin
      R := FCaretRect;
      FCaretRect := FBlocks.IndexToRect(Canvas, FSelEnd, True);
      Dec(FCaretRect.Right, FCaretRect.Left); // Right is width
      Dec(FCaretRect.Bottom, FCaretRect.Top); // Bottom is height
      if not (elOverwrite in FStates) then
        FCaretRect.Right := MinMax(FCaretRect.Bottom div 10, 1, 3);
      if HandleAllocated then
      begin
        if (SelLength = 0) then
        begin
          if not (elCaretCreated in FStates) or (R.Right <> FCaretRect.Right) or (R.Bottom <> FCaretRect.Bottom) then
          begin
            if CreateCaret(Handle, 0, FCaretRect.Right, FCaretRect.Bottom) then
            begin
              ShowEditorCaret;
              FStates := FStates + [elCaretCreated, elCaretVisible];
            end;
          end
          else if not (elCaretVisible in FStates) or (R.Left <> FCaretRect.Left) or (R.Top <> FCaretRect.Top) then
          begin
            ShowEditorCaret;
            Include(FStates, elCaretVisible);
          end;
        end
        else if elCaretVisible in FStates then
        begin
          HideEditorCaret;
          Exclude(FStates, elCaretVisible);
        end;
      end;
    end
    else if HandleAllocated and (elCaretCreated in FStates) then
    begin
      HideEditorCaret;
    {$IFDEF FPC}
      DestroyCaret(Handle);
    {$ELSE}
      DestroyCaret;
    {$ENDIF}
      FStates := FStates - [elCaretCreated, elCaretVisible];
    end;
  finally
    Exclude(FStates, elCaretUpdate);
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
    UpdateEditorCaret;
    if CallInvalidate then
      Invalidate
    else
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
  FNewLine := False;
  FParent := nil;
  FSelEnd := -1;
  FSelStart := -1;
  Parent := AParent; // to update default block properties!
end;

function TKMemoBlock.AddText(const AText: TString; At: Integer): Boolean;
begin
  Result := False;
end;

procedure TKMemoBlock.Assign(AItem: TKMemoBlock);
begin
  FBounds := AItem.Bounds;
  FNewLine := AItem.NewLine;
  FParent := AItem.Parent;
  FSelEnd := AItem.SelStart + AItem.SelLength;
  FSelStart := AItem.SelStart;
  Update([muContents]);
end;

procedure TKMemoBlock.ClearSelection;
begin
  if FNewLine and (FSelEnd = SelectableLength) then
  begin
    FNewLine := False;
    Dec(FSelEnd);
    FSelStart := Min(FSelStart, FSelEnd);
    Update([muContents]);
  end;
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

function TKMemoBlock.GetSelText: TString;
begin
  Result := '';
end;

function TKMemoBlock.GetText: TString;
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

function TKMemoBlock.NewLineSelected: Boolean;
begin
  Result := FNewLine and (FSelEnd >= SelectableLength);
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

procedure TKMemoBlock.Select(ASelStart, ASelLength: Integer);
var
  ASelEnd, MaxLen: Integer;
begin
  if ASelLength < 0 then
    Dec(ASelStart, ASelLength);
  ASelEnd := ASelStart + ASelLength;
  MaxLen := SelectableLength;
  ASelEnd := MinMax(ASelEnd, -1, MaxLen);
  ASelStart := MinMax(ASelStart, -1, MaxLen);
  if (ASelStart <> FSelStart) or (ASelEnd <> FSelEnd) then
  begin
    FSelEnd := ASelEnd;
    FSelStart := ASelStart;
    Update([muSelection]);
  end;
end;

function TKMemoBlock.SelectableLength: Integer;
begin
  Result := ContentLength;
  if FNewLine then
    Inc(Result);
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

procedure TKMemoBlock.SetNewLine(const Value: Boolean);
begin
  if FNewLine <> Value then
  begin
    FNewLine := Value;
    Update([muContents]);
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
  FText := '';
  FHAlign := halLeft;
  FVAlign := valCenter;
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

function TKTextMemoBlock.AddText(const AText: TString; At: Integer): Boolean;
var
  S, Part1, Part2: TString;
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
    Update([muContents]);
    Result := True;
  end;
end;

procedure TKTextMemoBlock.Assign(AItem: TKMemoBlock);
begin
  inherited;
  if AItem is TKTextMemoBlock then
  begin
    FBrush.Assign(TKTextMemoBlock(AItem).Brush);
    FFont.Assign(TKTextMemoBlock(AItem).Font);
    FHAlign := TKTextMemoBlock(AItem).HAlign;
    FText := TKTextMemoBlock(AItem).Text;
    FVAlign := TKTextMemoBlock(AItem).VAlign;
  end;
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
    FSelEnd := FSelStart;
    Update([muContents]);
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
{$IFDEF FPC}
  Result := UTF8Length(FText);
{$ELSE}
  Result := Length(FText);
{$ENDIF}
end;

function TKTextMemoBlock.GetCanAddText: Boolean;
begin
  Result := True;
end;

function TKTextMemoBlock.GetSelText: TString;
begin
{$IFDEF FPC}
  Result := UTF8Copy(FText, FSelStart + 1, FSelEnd - FSelStart);
{$ELSE}
  Result := Copy(FText, FSelStart + 1, FSelEnd - FSelStart);
{$ENDIF}
end;

function TKTextMemoBlock.GetText: TString;
begin
  Result := FText;
end;

function TKTextMemoBlock.IndexToRect(ACanvas: TCanvas; AIndex: Integer): TRect;
var
  TextBox: TKTextBox;
  S: TString;
begin
  if FNewLine then
    S := FText + cNewLineChar
  else
    S := FText;
  ACanvas.Font.Assign(FFont);
  ACanvas.Brush.Assign(FBrush);
  TextBox := TKTextBox.Create;
  try
    TextBox.HAlign := FHAlign;
    TextBox.VAlign := FVAlign;
    TextBox.Text := S;
    Result := TextBox.IndexToRect(ACanvas, FBounds, IndexToTextIndex(S, AIndex));
  finally
    TextBox.Free;
  end;
end;

class function TKTextMemoBlock.IndexToTextIndex(var AText: TString; AIndex: Integer): Integer;
{$IFDEF FPC}
var
  I: Integer;
{$ENDIF}
begin
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
  S: TString;
begin
  if FNewLine then
    S := FText + cNewLineChar
  else
    S := FText;
  ACanvas.Font.Assign(FFont);
  ACanvas.Brush.Assign(FBrush);
  TextBox := TKTextBox.Create;
  try
    TextBox.HAlign := FHAlign;
    TextBox.VAlign := FVAlign;
    TextBox.Text := S;
    TextBox.Measure(ACanvas, FBounds, Width, Height);
    FBounds.Right := FBounds.Left + Width;
    FBounds.Bottom := FBounds.Top + Height;
  finally
    TextBox.Free;
  end;
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
  S: TString;
begin
  inherited;
  if FNewLine then
    S := FText + cNewLineChar
  else
    S := FText;
  ACanvas.Font.Assign(FFont);
  ACanvas.Brush.Assign(FBrush);
  TextBox := TKTextBox.Create;
  try
  {  ACanvas.Brush.Color := clLime;
    ACanvas.FillRect(FBounds);
    DEc(FBounds.Right, 59);
    ACanvas.Brush.Color := clYellow;
    ACanvas.FillRect(FBounds);
    Attr := [taPathEllipsis];}
  //  Dec(FBounds.Right, 600);
  //  Inc(Fbounds.Bottom, 100);
  //  Attr := [taWordBreak];
    TextBox.HAlign := FHAlign;
    TextBox.VAlign := FVAlign;
    TextBox.Text := S;
    if SelLength > 0 then
    begin
      GetSelColors(Color, BkGnd);
      TextBox.SelColor := Color;
      TextBox.SelBkgnd := BkGnd;
      TextBox.SelStart := IndexToTextIndex(S, Min(FSelStart, SelectableLength));
      TextBox.SelEnd := IndexToTextIndex(S, Min(FSelEnd, SelectableLength));
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
  S: TString;
begin
  if FNewLine then
    S := FText + cNewLineChar
  else
    S := FText;
  ACanvas.Font.Assign(FFont);
  ACanvas.Brush.Assign(FBrush);
  TextBox := TKTextBox.Create;
  try
    TextBox.HAlign := FHAlign;
    TextBox.VAlign := FVAlign;
    TextBox.Text := S;
    TextBox.Attributes := TextBox.Attributes + [taIncludePadding];
    Result := TextIndexToIndex(S, TextBox.PointToIndex(ACanvas, FBounds, APoint));
  finally
    TextBox.Free;
  end;
end;

procedure TKTextMemoBlock.SetHAlign(const Value: TKHAlign);
begin
  if FHAlign <> Value then
  begin
    FHAlign := Value;
    Update([muAttributes]);
  end;
end;

procedure TKTextMemoBlock.SetText(const Value: TString);
begin
  if FText <> Value then
  begin
    FText := Value;
    Update([muContents]);
  end;
end;

procedure TKTextMemoBlock.SetVAlign(const Value: TKVAlign);
begin
  if FVAlign <> Value then
  begin
    FVAlign := Value;
    Update([muAttributes]);
  end;
end;

function TKTextMemoBlock.Split(At: Integer): TKMemoBlock;
var
  Item: TKTextMemoBlock;
  Part1, Part2: TString;
begin
  if (At >= 0) and (At < SelectableLength) then
  begin
    Item := TKTextMemoBlock.Create(FParent);
    Item.Assign(Self);
    SplitText(At + 1, Part1, Part2);
    FText := Part1;
    Item.Text := Part2;
    Result := Item;
    Update([muContents]);
  end else
    Result := nil;
end;

procedure TKTextMemoBlock.SplitText(At: Integer; out APart1, APart2: TString);
begin
{$IFDEF FPC}
  APart1 := UTF8Copy(FText, 1, At - 1);
  APart2 := UTF8Copy(FText, At, Length(FText) - At + 1);
{$ELSE}
  APart1 := Copy(FText, 1, At - 1);
  APart2 := Copy(FText, At, Length(FText) - At + 1);
{$ENDIF}
end;

class function TKTextMemoBlock.TextIndexToIndex(var AText: TString; ATextIndex: Integer): Integer;
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
  inherited;
  if AItem is TKImageMemoBlock then
  begin
    FImage.Assign(TKImageMemoBlock(AItem).Image);
  end;
end;

function TKImageMemoBlock.ContentLength: Integer;
begin
  Result := 1;
end;

function TKImageMemoBlock.IndexToRect(ACanvas: TCanvas; AIndex: Integer): TRect;
begin
  if FNewLine and (AIndex >= ContentLength) then
  begin
    if FParent <> nil then
      ACanvas.Font.Assign(FParent.DefaultFont);
    Result := Rect(FBounds.Right, FBounds.Top, FBounds.Right + ACanvas.TextWidth(cNewLineChar), FBounds.Bottom);
  end else
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
begin
  inherited;
  if SelLength > 0 then
  begin
    GetSelColors(Color, BkGnd);
    Bitmap := TKAlphaBitmap.Create;
    try
      Bitmap.SetSize(FImage.Width, FImage.Height);
      Bitmap.Canvas.Draw(0, 0, FImage.Graphic);
      Bitmap.AlphaFillPercent(50, not (FImage.Graphic is TKPngImage));
      ACanvas.Brush.Color := BkGnd;
      ACanvas.FillRect(FBounds);
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
  Update([muContents]);
end;

procedure TKImageMemoBlock.SetImagePath(const Value: TString);
begin
  FImage.LoadFromFile(Value);
  Update([muContents]);
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
  FUpdateLock := 0;
  Update([muContents]);
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

function TKMemoBlocks.AddImageBlock(APath: TString; At: Integer): TKImageMemoBlock;
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

function TKMemoBlocks.AddTextBlock(AText: TString; At: Integer): TKTextMemoBlock;
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

procedure TKMemoBlocks.ClearSelection;
var
  I, First, Last: Integer;
  Item, AuxItem: TKMemoBlock;
  SelectionComplete, NewLineSelected: Boolean;
begin
  SelectionComplete := False;
  LockUpdate;
  try
    I := 0;
    First := -1;
    Last := -1;
    while (I < Count) and not SelectionComplete do
    begin
      Item := TKMemoBlock(Items[I]);
      if (Item.SelStart >= 0) and (Item.SelLength > 0) then
      begin
        if Item.SelLength = Item.SelectableLength then
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
      end else
      begin
        if First >= 0 then
          SelectionComplete := True;
      end;
      Inc(I);
    end;
    if Last >= 0 then
      TKMemoBlock(Items[Last]).ClearSelection;
    if First >= 0 then
    begin
      Item := TKMemoBlock(Items[First]);
      if Item.NewLine then
      begin
        if (Item.SelStart = 0) and (Item.SelLength >= Item.ContentLength) then
        begin
          // entire block selected, delete
          Delete(First);
          if First > 0 then
          begin
            AuxItem := TKMemoBlock(Items[First - 1]);
            if AuxItem.NewLine then
            begin
              // if we are at the beginning of a line, then we must preserve this line by inserting a new block
              AuxItem := AuxItem.Split(AuxItem.ContentLength);
              AddAt(AuxItem, First);
            end;
            AuxItem.NewLine := True
          end
        end
        else if First < Count - 1 then
        begin
          AuxItem := TKMemoBlock(Items[First + 1]);
          if AuxItem.ContentLength = 0 then
            Delete(First + 1)
          else
            Item.ClearSelection;
        end else
          Item.ClearSelection;
      end else
        Item.ClearSelection;
    end;
  finally
    UnlockUpdate;
  end;
end;

function TKMemoBlocks.GetDefaultFont: TFont;
begin
  if FMemo <> nil then
    Result := FMemo.Font
  else
    Result := nil;
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

function TKMemoBlocks.GetLines(AIndex: Integer): TString;
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
      if Item.NewLine then
        Result := Result + cCR + cLF;
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

function TKMemoBlocks.GetSelText: TString;
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
      Result := Result + Item.SelText;
      if Item.NewLine then
        Result := Result + cCR + cLF;
    end;
  end;
end;

function TKMemoBlocks.GetText: TString;
var
  I: Integer;
  Item: TKMemoBlock;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    Item := TKMemoBlock(Items[I]);
    Result := Result + Item.Text;
    if Item.NewLine then
      Result := Result + cCR + cLF;
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
          if (AIndex >= LastIndex) and (AIndex <= CurIndex) then
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
        Item.NewLine := True;
        AddAt(NewItem, Block + 1);
      end
      else if Item.NewLine then
      begin
        NewItem := AddTextBlock('', Block + 1);
        NewItem.NewLine := True;
      end else
        Item.NewLine := True;
      Result := True;
    end;
  finally
    UnlockUpdate;
  end;
end;

function TKMemoBlocks.InsertString(AIndex: Integer; const AValue: TString): Boolean;
var
  Block, LocalIndex: Integer;
  Item: TKMemoBlock;
begin
  Result := False;
  LockUpdate;
  try
    Block := IndexToBlock(AIndex, LocalIndex);
    if Block >= 0 then
    begin
      Item := TKMemoBlock(Items[Block]);
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
          if Item.CanAddText and not Item.NewLine then
          begin
            // insert character at the end of this block
            Result := Item.AddText(AValue);
          end else
          begin
            // if still no text block, insert new one
            Result := AddTextBlock(AValue, Block + 1) <> nil;
          end;
        end else
        begin
          // insert new text block
          Result := AddTextBlock(AValue, Block) <> nil;
        end;
      end
      else if LocalIndex = Item.ContentLength then
      begin
        if Item.NewLine then
        begin
          // we are at the end of line
          Item.NewLine := False;
          // insert new text block and new line
          Item := AddTextBlock(AValue, Block + 1);
          if Item <> nil then
          begin
            Item.NewLine := True;
            Result := True;
          end;
        end else
          // insert new text block
          Result := AddTextBlock(AValue, Block + 1) <> nil;
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
        if ACaret and (AIndex = LastIndex) and (LastItem <> nil) and LastItem.CanAddText and not LastItem.NewLine then
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
  end
  else if ALine = FLineCount then
  begin
    Result := FLines[FLineCount - 1].Bounds;
    Result.Left := Result.Right + 1;
    Result.Right := Result.Left;
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

function TKMemoBlocks.NextIndexByHorzExtent(ACanvas: TCanvas; AIndex, AWidth: Integer; ACaret: Boolean): Integer;
var
  Line: Integer;
  R: TRect;
begin
  Line := IndexToLine(AIndex);
  R := LineToRect(ACanvas, AIndex, Line, False);
  Result := PointToLineIndex(ACanvas, Line, Point(R.Left + AWidth, LineTop[Line]), True, ACaret);
end;

function TKMemoBlocks.NextIndexByRowDelta(ACanvas: TCanvas; AIndex, ARowDelta, ALeftPos: Integer; ACaret: Boolean): Integer;
var
  Line, TopPos: Integer;
begin
  Line := IndexToLine(AIndex) + ARowDelta;
  if Line >= FLineCount then
    TopPos := LineBottom[Line - 1] + 1
  else
    TopPos := LineTop[Line];
  Result := PointToIndex(ACanvas, Point(ALeftPos, TopPos), True, ACaret);
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
    Update([muContents]);
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
      if not ACaret then
        Inc(Result);
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
        if not ACaret then
          Inc(Result);
      end;
    end;
  end;
end;

procedure TKMemoBlocks.Select(ASelStart, ASelLength: Integer);
var
  I, LastIndex, CurIndex, ASelEnd: Integer;
  Item: TKMemoBlock;
begin
  LockUpdate;
  try
    CurIndex := 0;
    ASelEnd := ASelStart + ASelLength;
    for I := 0 to Count - 1 do
    begin
      Item := TKMemoBlock(Items[I]);
      LastIndex := CurIndex;
      Inc(CurIndex, Item.SelectableLength);
      if (ASelStart >= LastIndex) and (ASelEnd < CurIndex) then
        // selection within the same block
        Item.Select(ASelStart - LastIndex, ASelEnd - ASelStart)
      else if (ASelStart >= LastIndex) and ((ASelStart < CurIndex) or (CurIndex = LastIndex)) and (ASelEnd >= CurIndex) then
        // selection starts in this block
        Item.Select(ASelStart - LastIndex, CurIndex - ASelStart)
      else if (ASelStart < LastIndex) and (ASelEnd >= LastIndex) and (ASelEnd < CurIndex) then
        // selection ends in this block
        Item.Select(0, ASelEnd - LastIndex)
      else if (ASelStart < LastIndex) and (ASelEnd >= CurIndex) then
        // selection goes through this block
        Item.Select(0, CurIndex - LastIndex)
      else
        Item.Select(-1, 0);
    end;
  finally
    UnlockUpdate;
  end;
end;

procedure TKMemoBlocks.SetLines(AIndex: Integer; const AValue: TString);
var
  I: Integer;
  Item: TKMemoBlock;
begin
  if (AIndex >= 0) and (AIndex < FLineCount) then
  begin
    LockUpdate;
    try
      for I := FLines[AIndex].StartBlock to FLines[AIndex].EndBlock do
        Delete(FLines[AIndex].StartBlock);
      Item := AddTextBlock(AValue, FLines[AIndex].StartBlock);
      Item.NewLine := True;
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKMemoBlocks.SetText(const AValue: TString);
var
  List: TStringList;
  I: Integer;
  Item: TKMemoBlock;
begin
  LockUpdate;
  try
    List := TStringList.Create;
    try
      List.Text := AValue;
      for I := 0 to List.Count - 1 do
      begin
        Item := AddTextBlock(List[I]);
        Item.NewLine := True;
      end;
    finally
      List.Free;
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
var
  Item: TKMemoBlock;
begin
  if UpdateUnlocked then
  begin
    if muContents in AReasons then
    begin
      // avoid possible infinite loops
      Inc(FUpdateLock);
      try
        // if list is empty always assure at least one text block
        if Count = 0 then
          AddTextBlock('');
        // ensure last block has always new line
        Item := TKMemoBlock(Items[Count - 1]);
        Item.NewLine := True;
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
    IsPara := Item.NewLine;
    Inc(CurIndex, Item.SelectableLength);
    if IsPara then
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

