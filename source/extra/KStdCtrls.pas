{ @abstract(This unit contains native replacements of Windows standard controls)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(28 Apr 2008)
  @lastmod(10 May 2008)

  This unit provides native Unicode enabled counterparts for the Windows standard
  controls usable under any Windows OS above Win95:

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

unit KStdCtrls;

{$include KControls.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Contnrs,
  Forms, StdCtrls, ExtCtrls, KFunctions;

type
  { Declares possible values for the @link(TKCustomMemo.ExecuteCommand) method. }
  TKEditCommand = (
    { Move caret left one char. }
    ecLeft,
    { Move caret right one char. }
    ecRight,
    { Move caret up one line. }
    ecUp,
    { Move caret down one line. }
    ecDown,
    { Move caret to beginning of line. }
    ecLineStart,
    { Move caret to end of line. }
    ecLineEnd,
    { Move caret up one page. }
    ecPageUp,
    { Move caret down one page. }
    ecPageDown,
    { Move caret left one page. }
    ecPageLeft,
    { Move caret right one page. }
    ecPageRight,
    { Move caret to top of page. }
    ecPageTop,
    { Move caret to bottom of page. }
    ecPageBottom,
    { Move caret to absolute beginning. }
    ecEditorTop,
    { Move caret to absolute end. }
    ecEditorBottom,
    { Move caret to specific coordinates, Data = ^TPoint. }
    ecGotoXY,
    { Move caret left one char. }
    ecSelLeft,
    { Move caret right one char, affecting selection. }
    ecSelRight,
    { Move caret up one line, affecting selection. }
    ecSelUp,
    { Move caret down one line, affecting selection. }
    ecSelDown,
    { Move caret to beginning of line, affecting selection. }
    ecSelLineStart,
    { Move caret to end of line, affecting selection. }
    ecSelLineEnd,
    { Move caret up one page, affecting selection. }
    ecSelPageUp,
    { Move caret down one page, affecting selection. }
    ecSelPageDown,
    { Move caret left one page, affecting selection. }
    ecSelPageLeft,
    { Move caret right one page, affecting selection. }
    ecSelPageRight,
    { Move caret to top of page, affecting selection. }
    ecSelPageTop,
    { Move caret to bottom of page, affecting selection. }
    ecSelPageBottom,
    { Move caret to absolute beginning, affecting selection. }
    ecSelEditorTop,
    { Move caret to absolute end, affecting selection. }
    ecSelEditorBottom,
    { Move caret to specific coordinates, affecting selection, Data = ^TPoint. }
    ecSelGotoXY,
    { Scroll up one line leaving caret position unchanged. }
    ecScrollUp,
    { Scroll down one line leaving caret position unchanged. }
    ecScrollDown,
    { Scroll left one char leaving caret position unchanged. }
    ecScrollLeft,
    { Scroll right one char leaving caret position unchanged. }
    ecScrollRight,
    { Scroll to center the caret position within client area. }
    ecScrollCenter,
    { Undo previous action. }
    ecUndo,
    { Redo last undone action. }
    ecRedo,
    { Copy selection to clipboard. }
    ecCopy,
    { Cut selection to clipboard. }
    ecCut,
    { Paste clipboard to current position. }
    ecPaste,
    { Insert character at current position, Data = ^Char. }
    ecInsertChar,
    { Insert digits (digit string) at current position, Data = ^string (must contain digits only). }
    ecInsertDigits,
    { Insert string (multiple characters) at current position, Data = ^string. }
    ecInsertString,
    { Delete last byte (i.e. backspace key). }
    ecDeleteLastByte,
    { Delete byte at caret (i.e. delete key). }
    ecDeleteByte,
    { Delete from caret to beginning of line. }
    ecDeleteBOL,
    { Delete from caret to end of line. }
    ecDeleteEOL,
    { Delete current line. }
    ecDeleteLine,
    { Select everything. }
    ecSelectAll,
    { Delete everything. }
    ecClearAll,
    { Delete selection (no digit selection). }
    ecClearIndexSelection,
    { Delete selection (digit selection as well). }
    ecClearSelection,
    { Search for text/digits. }
    ecSearch,
    { Replace text/digits. }
    ecReplace,
    { Preview printed page(s). }
    ecPreview,
    { Print buffer contents. }
    ecPrint,
    { Set insert mode. }
    ecInsertMode,
    { Set overwrite mode. }
    ecOverwriteMode,
    { Toggle insert/overwrite mode. }
    ecToggleMode,
    { Adjust editor when getting input focus. }
    ecGotFocus,
    { Adjust editor when losing input focus. }
    ecLostFocus
  );

  { Declares possible values for the @link(TKCustomMemo.DisabledDrawStyle) property. }
  TKEditDisabledDrawStyle = (
    { The lines will be painted with brighter colors when editor is disabled. }
    eddFaded,
    { The lines will be painted with gray text and white background when editor is disabled. }
    eddGrayed,
    { The lines will be painted normally when editor is disabled. }
    eddNormal
  );

  { @abstract(Declares the keystroke information structure for the Key member
    of the @link(TKEditCommandAssignment) structure)
    <UL>
    <LH>Members:</LH>
    <LI><I>Key</I> - virtual key code</LI>
    <LI><I>Shift</I> - shift state that belongs to that key code</LI>
    </UL>
  }
  TKEditKey = record
    Key: Word;
    Shift: TShiftState;
  end;

  { @abstract(Declares the @link(TKEditKeyMapping) array item)
    <UL>
    <LH>Members:</LH>
    <LI><I>Command</I> - command that is about to be executed</LI>
    <LI><I>Key</I> - key combination necessary to execute that command</LI>
    </UL>
  }
  TKEditCommandAssignment = record
    Command: TKEditCommand;
    Key: TKEditKey;
  end;

  { @abstract(Declares @link(TKCustomMemo.OnDropFiles) event handler)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller</LI>
    <LI><I>X, Y</I> - mouse cursor coordinates (relative to the caller's window)</LI>
    <LI><I>Files</I> - list of file names that were dropped on the caller's window)</LI>
    </UL>
  }
  TKEditDropFilesEvent = procedure(Sender: TObject; X, Y: integer;
    Files: TStrings) of object;

  { Declares key mapping array for the @link(TKCustomMemo.KeyMapping) property. }
  TKEditKeyMapping = array of TKEditCommandAssignment;

  { Declares options - possible values for the @link(TKCustomMemo.Options) property. }
  TKEditOption = (
    { The editor will receive dropped files. }
    eoDropFiles,
    { All undo/redo operations of the same kind will be grouped together. }
    eoGroupUndo,
    { The editor allows undo/redo operations after the @link(TKCustomMemo.Modified) property
      has been set to False. }
    eoUndoAfterSave
  );

  { Options can be arbitrary combined. }
  TKEditOptions = set of TKEditOption;

  { @abstract(Declares the page/copy information structure e.g. for the @link(TKEditPrintNotifyEvent) event)
    <UL>
    <LH>Members:</LH>
    <LI><I>Page</I> - currently printed page</LI>
    <LI><I>Pages</I> - total number of pages for 1 copy</LI>
    <LI><I>Copy</I> - currently printed copy</LI>
    <LI><I>Copies</I> - total number of copies</LI>
    </UL>
  }
  TKEditPageCopyInfo = record
    Page,
    Pages,
    Copy,
    Copies: Integer;
  end;

  { @abstract(Declares the paint data structure for the @link(TKCustomMemo.PaintLines) method)
    <UL>
    <LH>Members:</LH>
    <LI><I>Canvas</I> - destination canvas</LI>
    <LI><I>PainRect</I> - bounding rectangle for painted lines (no clipping necessary,
    this is performed by window/page client area)</LI>
    <LI><I>TopLine</I> - first line painted (vertical scroll offset)</LI>
    <LI><I>BottomLine</I> - last line painted</LI>
    <LI><I>LeftChar</I> - first character painted (horizontal scroll offset)</LI>
    <LI><I>CharWidth</I> - character width in pixels for supplied canvas</LI>
    <LI><I>CharHeight</I> - character height in pixels for supplied canvas</LI>
    <LI><I>CharSpacing</I> - inter-character spacing in pixels for supplied canvas</LI>
    <LI><I>Printing</I> - determines whether normal painting or page printing should be performed</LI>
    <LI><I>PrintAll</I> - when Printing is True, specifies whether all data or selection only
    should be printed, this applies only to the first and/or last printed line</LI>
    <LI><I>PrintColors</I> - when Printing is True, specifies whether to print a color or
    black/white page</LI>
    </UL>
  }
  TKEditPaintData = record
    Canvas: TCanvas;
    PaintRect: TRect;
    TopLine,
    BottomLine,
    LeftChar,
    CharWidth,
    CharHeight,
    CharSpacing: Integer;
    Printing,
    PrintAll,
    PrintColors: Boolean;
  end;

  TKMemoParagraphInfo = record
    ParaIndex: Integer;
    BlockIndex: Integer;
  end;

  { @abstract(Declares the printer/page information structure e.g. for the @link(TKCustomMemo.GetPrintSettings) method)
    <UL>
    <LH>Members:</LH>
    <LI><I>LogPelsX, LogPelsY</I> - VGA canvas pixels per inch (horizontal and vertical)</LI>
    <LI><I>PrnLogPelsX, PrnLogPelsY</I> - printer canvas pixels per inch (horizontal and vertical)</LI>
    <LI><I>PageWidth</I> - page width in printer pixels</LI>
    <LI><I>PageHeight</I> - page height in printer pixels</LI>
    <LI><I>CharWidth</I> - character width in printer pixels</LI>
    <LI><I>CharHeight</I> - character height in printer pixels</LI>
    <LI><I>FontHeight</I> - character font height in printer pixels</LI>
    <LI><I>TotalWidth</I> - total printed outline width in printer pixels</LI>
    <LI><I>PageLines</I> - number of lines that fit onto 1 page</LI>
    <LI><I>PageCount</I> - number of pages for 1 copy</LI>
    <LI><I>PageOffset</I> - very internal parameter - applies only when printing a selection</LI>
    <LI><I>LastLine</I> - internal parameter - the very last line printed</LI>
    <LI><I>PrintMargins</I> - print margins in printer pixels</LI>
    <LI><I>RatioX</I> - horizontal printer pixels/horizontal VGA pixels</LI>
    <LI><I>RatioY</I> - vertical printer pixels/vertical VGA pixels</LI>
    <LI><I>RatioScale</I> - additional scale of the printed outline (see @link(epoFitToPage) or
    @link(TKEditPrintData).Scale</LI>
    </UL>
  }
  TKEditPrintSettings = record
    LogPelsX,
    LogPelsY,
    PrnLogPelsX,
    PrnLogPelsY,
    PageWidth,
    PageHeight,
    TotalWidth,
    PageLines,
    PageCount,
    PageOffset,
    LastLine: Integer;
    PrintMargins: TRect;
    RatioX,
    RatioY,
    RatioScale: Single;
  end;

  { Declares print options - possible values for the Options field in the @link(TKMemoPrintData) structure. }
  TKEditPrintOption = (
    { The printed outline will be scaled to fit on page. }
    epoFitToPage,
    { Every even page will be printed with mirrored (swapped) margins. }
    epoMirrorMargins,
    { Page numbers will be added to the bottom of each printed page. }
    epoPageNumbers,
    { Color page will be printed instead of B/W page. }
    epoUseColor
  );

  { Print options can be arbitrary combined. }
  TKEditPrintOptions = set of TKEditPrintOption;

  { Declares possible values for the Range field in the @link(TKMemoPrintData) structure. }
  TKEditPrintRange = (
    { All pages will be printed. }
    eprAll,
    { Only selected block will be printed. }
    eprSelectedOnly,
    { Only given range of pages will be printed. }
    eprRange
  );

  { @abstract(Declares the print job description structure for the @link(ecPrint) command)
    <UL>
    <LH>Members:</LH>
    <LI><I>MarginUnits</I> - measurement units for print margins - Margins field</LI>
    <LI><I>Margins</I> - print margins</LI>
    <LI><I>Options</I> - print options</LI>
    <LI><I>Range</I> - print range</LI>
    <LI><I>StartPage</I> - specifies first page printed if Range is eprRange</LI>
    <LI><I>EndPage</I> - specifies last page printed if Range is eprRange</LI>
    <LI><I>Copies</I> - specifies the number of copies to print</LI>
    <LI><I>Scale</I> - specifies the scale of the printed outline when
    Options contain no epoFitToPage element</LI>
    <LI><I>Title</I> - ecPrint only: specifies the document title (appears in printer manager)</LI>
    <LI><I>PreviewCanvas</I> - ecPreview only: specifies the canvas where to paint the page</LI>
    <LI><I>PreviewPage</I> - ecPreview only: specifies the page to paint</LI>
    <LI><I>Settings</I> - deliver valid print settings here to avoid repetitive
    GetPrintSettings calls (ecPreview). This parameter can be nil.</LI>
    </UL>
  }
  TKEditPrintData = record
    MarginUnits: TKMeasUnits;
    Margins: TKPrintMargins;
    Options: TKEditPrintOptions;
    Range: TKEditPrintRange;
    StartPage,
    EndPage,
    Copies,
    Scale: Integer;
    Title: string;
    PreviewCanvas: TCanvas;
    PreviewPage: Integer;
    Settings: ^TKEditPrintSettings;
  end;

  { Pointer to @link(TKEditCharMapping). }
  PKEditPrintData = ^TKEditPrintData;

  { Declares possible values for the Status parameter in the @link(TKEditPrintNotifyEvent) event. }
  TKEditPrintStatus = (
    { This event occurs at the beginning of the print job - you may show an Abort dialog here. }
    epsBegin,
    { This event occurs after each page has been printed - you may update the Page/Copy information
      in the Abort dialog. }
    epsNewPage,
    { This event occurs at the end of the print job - you may hide the Abort dialog here. }
    epsEnd
  );

  { @abstract(Declares @link(TKCustomMemo.OnPrintNotify) event handler)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller</LI>
    <LI><I>Status</I> - specifies the event type</LI>
    <LI><I>Info</I> - provides information about current(all) page(s) and copy(ies)</LI>
    <LI><I>Abort</I> - set to True to abort the print job</LI>
    </UL>
    Remark: At certain time slots, the print spooler allows the message queue
    to be processed for the thread where the print job is running. This e.g. allows
    the user to press a button on the Abort dialog. Because this message loop can be invoked
    e.g. during a Printer.Canvas.TextRect function and any painting messages may hover in
    the message queue, any functions used both to print a job and to process particular
    messages should be reentrant to avoid conflicts. Perhaps should print jobs be run
    in seperate threads?
  }
  TKEditPrintNotifyEvent = procedure(Sender: TObject; Status: TKEditPrintStatus;
    const Info: TKEditPageCopyInfo; var Abort: Boolean) of object;

  { @abstract(Declares @link(TKCustomMemo.OnPrintSpace) event handler)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller</LI>
    <LI><I>PrintData</I> - print data for the current print job</LI>
    <LI><I>HeaderSpace</I> - additional space reserved for custom page header -
    enter in MarginUnits</LI>
    <LI><I>FooterSpace</I> - additional space reserved for custom page footer -
    enter in MarginUnits</LI>
    </UL>
  }
  TKEditPrintSpaceEvent = procedure(Sender: TObject;
    const PrintData: TKEditPrintData; var HeaderSpace,
    FooterSpace: Double) of object;

  { @abstract(Declares @link(TKCustomMemo.OnPrintPaint) event handler)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller</LI>
    <LI><I>Info</I> - provides information about current(all) page(s) and copy(ies)</LI>
    <LI><I>PaintData</I> - paint settings for the current page</LI>
    <LI><I>Settings</I> - print settings for the current print job</LI>
    </UL>
  }
  TKEditPrintPaintEvent = procedure(Sender: TObject; const Info: TKEditPageCopyInfo;
    const PaintData: TKEditPaintData; const Settings: TKEditPrintSettings) of object;

  { Declares possible values for the Action parameter in the @link(TKMemoReplaceTextEvent) event. }
  TKEditReplaceAction = (
    { Quit replace sequence. }
    eraCancel,
    { Replace this occurence. }
    eraYes,
    { Don't replace this occurence. }
    eraNo,
    { Replace all following occurences without prompting. }
    eraAll
  );

  { @abstract(Declares @link(TKCustomMemo.OnReplaceText) event handler)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller</LI>
    <LI><I>TextToFind</I> - current search string</LI>
    <LI><I>TextToReplace</I> - current replace string</LI>
    <LI><I>Action</I> - specifies how the replace function should continue</LI>
    </UL>
  }
  TKEditReplaceTextEvent = procedure(Sender: TObject; const TextToFind, TextToReplace:
    string; var Action: TKEditReplaceAction) of object;

  { Declares possible values for the ErrorReason member of the @link(TKEditSearchData) structure. }
  TKEditSearchError = (
    { No error occured. }
    eseOk,
    { There is a character in the search string that cannot be interpreted as hexadecimal digits}
    eseNoDigitsFind,
    { There is a character in the replace string that cannot be interpreted as hexadecimal digits}
    eseNoDigitsReplace,
    { No other search string found. }
    eseNoMatch
  );

  { Declares search options - possible values for the Options member of the @link(TKEditSearchData) structure. }
  TKEditSearchOption = (
    { Replace all occurences. }
    esoAll,
    { Search backwards. }
    esoBackwards,
    { Search entire scope instead from current caret position. }
    esoEntireScope,
    { Include to identify search - this element will be automatically cleared
      to provide the @link(TKEditSearchData) structure for additional search. }
    esoFirstSearch,
    { Match case when a binary search should be executed. }
    esoMatchCase,
    { Prompt user before a string is about to be replaced. This assumes @link(OnReplaceText)
      is assigned. }
    esoPrompt,
    { Search the current selection only. }
    esoSelectedOnly
  );

  { Search options can be arbitrary combined. }
  TKEditSearchOptions = set of TKEditSearchOption;

  { @abstract(Declares the search/replace description structure for the @link(ecSearch)
    and @link(ecReplace) commands)
    <UL>
    <LH>Members:</LH>
    <LI><I>ErrorReason</I> - upon @link(ExecuteCommand)(ecSearch) or
    ExecuteCommand(ecReplace), inspect this member to inform user about
    search/replace result</LI>
    <LI><I>Options</I> - defines search/replace options</LI>
    <LI><I>SelStart, SelEnd</I> - internal parameters, don't modify</LI>
    <LI><I>TextToFind</I> - search string</LI>
    <LI><I>TextToReplace</I> - replace string</LI>
    </UL>
  }
  TKEditSearchData = record
    ErrorReason: TKEditSearchError;
    Options: TKEditSearchOptions;
    SelStart,
    SelEnd: Integer;
    TextToFind,
    TextToReplace: string;
  end;

  { Pointer to @link(TKEditSearchData). }
  PKEditSearchData = ^TKEditSearchData;

  { Declares possible values for the Stage parameter in the @link(TKGridSelectionExpandEvent)
    event handler or @link(TKCustomMemo.SelectionMove) method. }
  TKEditSelectionStage = (
    { The selection moves entirely - the selection base cell changes. }
    ssInit,
    { The selection expands - the selection base cell remains unchanged. }
    ssExpand
  );

  { Declares possible indexes e.g. for the @link(TKEditColors.Color) property. }
  TKEditColorIndex = (
    ciSelectedBkGnd,
    ciSelectedBkGndFocused,
    ciSelectedText,
    ciSelectedTextFocused,
    ciText,
    ciBkGnd
  );

  { This array serves as storage for all hex editor colors. }
  TKEditColorArray = array[Low(TKEditColorIndex)..High(TKEditColorIndex)] of TColor;

  { @abstract(Declares the color description structure returned by @link(TKEditColors.ColorData) property)
    <UL>
    <LH>Members:</LH>
    <LI><I>Index</I> - color index</LI>
    <LI><I>Color</I> - current color value</LI>
    <LI><I>Default</I> - default color value</LI>
    <LI><I>Name</I> - color name</LI>
    </UL>
  }
  TKEditColorData = record
    Index: TKEditColorIndex;
    Color: TColor;
    Default: TColor;
    Name: string;
  end;

  { Declares possible values for the @link(TKMemoColors.ColorScheme) property. }
  TKEditColorScheme = (
    { GetColor returns normal color currently defined for each item. }
    ecsNormal,
    { GetColor returns black for text and line colors and white for background colors. }
    ecsBW,
    { GetColor returns gray for text and line colors and white for background colors. }
    ecsGrayed,
    { GetColor returns brighter version of normal color. }
    ecsBright
  );

const
  { Caret is visible. }
  cEF_CaretVisible = $0001;
  { Caret is being updated. }
  cEF_CaretUpdate = $0002;
  { Ignore following WM_CHAR message. }
  cEF_IgnoreNextChar = $0004;
  { Content modified. }
  cEF_Modified = $0008;
  { Overwrite mode active. }
  cEF_Overwrite = $0010;
  { Read only editor. }
  cEF_ReadOnly = $0020;

  { Minimum for the @link(TKCustomMemo.LineHeightPercent) property. }
  cLineHeightPercentMin = 10;
  { Maximum for the @link(TKCustomMemo.LineHeightPercent) property. }
  cLineHeightPercentMax = 1000;
  { Default value for the @link(TKCustomMemo.LineHeightPercent) property. }
  cLineHeightPercentDef = 130;

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

  { Minimum for the Scale member of the @link(TKMemoPrintData) record. }
  cPrintScaleDef = 100;
  { Maximum for the Scale member of the @link(TKMemoPrintData) record. }
  cPrintScaleMin = 10;
  { Default value for the Scale member of the @link(TKMemoPrintData) record. }
  cPrintScaleMax = 500;

  { Minimum for the Copies member of the @link(TKMemoPrintData) record. }
  cPrintCopiesMin = 1;
  { Maximum for the Copies member of the @link(TKMemoPrintData) record. }
  cPrintCopiesMax = 100;
  { Default value for the Copies member of the @link(TKMemoPrintData) record. }
  cPrintCopiesDef = 1;

  { Default value for the @link(TKMemoColors.SelectedBkGnd) color property. }
  cSelectedBkGndDef = clGrayText;
  { Default value for the @link(TKMemoColors.SelectedBkGndFocused) color property. }
  cSelectedBkGndFocusedDef = clHighlight;
  { Default value for the @link(TKMemoColors.SelectedText) color property. }
  cSelectedTextDef = clHighlightText;
  { Default value for the @link(TKMemoColors.SelectedTextFocused) color property. }
  cSelectedTextFocusedDef = clHighlightText;
  { Default value for the @link(TKMemoColors.Text) color property. }
  cTextDef = clWindowText;
  { Default value for the @link(TKMemoColors.Bkgnd) color property. }
  cBkgndDef = clWindow;

  { Default value for the @link(TKCustomMemo.DisabledDrawStyle) property. }
  cDisabledDrawStyleDef = eddFaded;

  { Declares the Index member of the @link(TKMemoSelection) record invalid}
  cAll = -1;

  cHorzScrollStepDef = 4;

  cVertScrollStepDef = 20;

  { Default value for the @link(TKMemo.Height) property. }
  cHeight = 200;

  { Default value for the @link(TKMemo.Width) property. }
  cWidth = 300;

type
  TKCustomMemo = class;

  { @abstract(Container for all colors used by @link(TKCustomMemo) class)
    This container allows to group many colors into one item in object inspector.
    Colors are accessible via published properties or several public Color*
    properties.
  }
  TKEditColors = class(TPersistent)
  private
    FMemo: TKCustomMemo;
    FBrightColors: TKEditColorArray;
    FColorScheme: TKEditColorScheme;
    FColors: TKEditColorArray;
    function GetColor(Index: TKEditColorIndex): TColor;
    function GetColorEx(Index: TKEditColorIndex): TColor;
    procedure SetColor(Index: TKEditColorIndex; Value: TColor);
    procedure SetColorEx(Index: TKEditColorIndex; Value: TColor);
    procedure SetColors(const Value: TKEditColorArray);
  public
    { Creates the instance. You can create a custom instance and pass it
      e.g. to a @link(TKCustomMemo.Colors) property. The AMemo parameter has no meaning
      in this case and you may set it to nil. }
    constructor Create(AMemo: TKCustomMemo);
    { Copies the properties of another instance that inherits from
      TPersistent into this TKEditColors instance. }
    procedure Assign(Source: TPersistent); override;
    { Clears cached brighter colors. }
    procedure ClearBrightColors;
    { Specifies color scheme for reading of published properties - see GetColor in source code}
    property ColorScheme: TKEditColorScheme read FColorScheme write FColorScheme;
    { Returns always normal color - regardless of the ColorScheme setting. }
    property Color[Index: TKEditColorIndex]: TColor read GetColorEx write SetColorEx;
    { Returns array of normal colors. }
    property Colors: TKEditColorArray read FColors write SetColors;
  published
    { Selection background - inactive edit area. }
    property SelectedBkGnd: TColor index ciSelectedBkGnd read GetColor write SetColor default cSelectedBkGndDef;
    { Selection background - active edit area. }
    property SelectedBkGndFocused: TColor index ciSelectedBkGndFocused read GetColor write SetColor default cSelectedBkGndFocusedDef;
    { Selection text - inactive edit area. }
    property SelectedText: TColor index ciSelectedText read GetColor write SetColor default cSelectedTextDef;
    { Selection text - active edit area. }
    property SelectedTextFocused: TColor index ciSelectedTextFocused read GetColor write SetColor default cSelectedTextFocusedDef;
    { Text area text color. }
    property Text: TColor index ciText read GetColor write SetColor default cTextDef;
    { Text area background color. }
    property Bkgnd: TColor index ciBkgnd read GetColor write SetColor default cBkGndDef;
  end;

  { Declares possible values for the ItemReason member of the @link(TKEditChangeItem) structure. }
  TKEditChangeReason = (
    { Save caret position only. }
    crCaretPos,
    { Save inserted character to be able to delete it. }
    crDeleteChar,
    { Save inserted binary string to be able to delete it. }
    crDeleteString,
    { Save deleted character to be able to insert it. }
    crInsertChar,
    { Save deleted binary string to be able to insert it. }
    crInsertString
    );

  { @abstract(Declares @link(TKEditChangeList.OnChange) event handler)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller</LI>
    <LI><I>ItemReason</I> - specifies the undo/redo reason</LI>
    </UL>
  }
  TKEditUndoChangeEvent = procedure(Sender: TObject;
    ItemReason: TKEditChangeReason) of object;

  { @abstract(Declares the undo/redo item description structure used by the @link(TKEditChangeList) class)
    <UL>
    <LH>Members:</LH>
    <LI><I>Data</I> - characters (binary or digit string) needed to execute this item</LI>
    <LI><I>EditArea</I> - active edit area at the time this item was recorded</LI>
    <LI><I>Group</I> - identifies the undo/redo group. Some editor modifications
      produce a sequence of 2 or more undo items. This sequence is called undo/redo
      group and is always interpreted as a single undo/redo item. Moreover,
      if there is @link(eoGroupUndo) among @link(TKCustomMemo.Options),
      a single ecUndo or ecRedo command manipulates all following undo groups
      of the same kind (reason) as if they were a single undo/redo item. </LI>
    <LI><I>GroupReason</I> - reason (kind) of this undo group</LI>
    <LI><I>ItemReason</I> - reason (kind) of this item</LI>
    <LI><I>SelEnd</I> - end of the selection at the time this item was recorded</LI>
    <LI><I>SelStart</I> - start of the selection at the time this item was recorded</LI>
    </UL>
  }
  TKEditChangeItem = record
    Data: string;
    Group: Cardinal;
    GroupReason: TKEditChangeReason;
    Inserted: Boolean;
    ItemReason: TKEditChangeReason;
    SelLength: Integer;
    SelStart: Integer;
  end;

  { Pointer to @link(TKEditChangeItem). }
  PKHexEditorChangeItem = ^TKEditChangeItem;

 { @abstract(Change (undo/redo item) list manager). }
  TKEditChangeList = class(TList)
  private
    FEditor: TKCustomMemo;
    FGroup: Cardinal;
    FGroupUseLock: Integer;
    FGroupReason: TKEditChangeReason;
    FIndex: Integer;
    FModifiedIndex: Integer;
    FLimit: Integer;
    FRedoList: TKEditChangeList;
    FOnChange: TKEditUndoChangeEvent;
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
    constructor Create(AEditor: TKCustomMemo; RedoList: TKEditChangeList);
    { Inserts a undo/redo item
      <UL>
      <LH>Parameters:</LH>
      <LI><I>ItemReason</I> - specifies the undo/redo item reason. The change list doesn't
      allow to insert succesive crCaretPos items unless Inserted is True</LI>
      <LI><I>Data</I> - specifies the item data. Some items (crCaretPos)
      don't need to supply any data</LI>
      <LI><I>Inserted</I> - for the urInsert* items, specifies whether the item
      was recorded with @link(TKCustomMemo.InsertMode) on (True) or
      off (False). See ItemReason for crCaretPos behavior.</LI>
      </UL>}
    procedure AddChange(ItemReason: TKEditChangeReason; const Data: string = '';
      Inserted: Boolean = True); virtual;
    { Tells the undo list a new undo/redo group is about to be created. Each
      BeginGroup call must have a corresponding EndGroup call (use try-finally).
      BeginGroup calls may be nested, however, only the first call will create an
      undo/redo group. Use the GroupReason parameter to specify the reason of this group. }
    procedure BeginGroup(GroupReason: TKEditChangeReason); virtual;
    { Informs whether there are any undo/redo items available - i.e. CanUndo/CanRedo}
    function CanPeek: Boolean;
    { Clears the entire list - overriden to execute some adjustments. }
    procedure Clear; override;
    { Completes the undo/redo group. See @link(TKEditChangeList.BeginGroup) for details. }
    procedure EndGroup; virtual;
    { Returns the topmost item to handle or inspect it}
    function PeekItem: PKHexEditorChangeItem;
    { If there is no reason to handle an item returned by PeekItem, it has to be
    poked back with this function to become active for next undo/redo command. }
    procedure PokeItem;
    { For redo list only - each undo command creates a redo command with the same
      group information - see source. }
    procedure SetGroupData(Group: Integer; GroupReason: TKEditChangeReason);
    { Specifies maximum number of items - not groups. }
    property Limit: Integer read FLimit write SetLimit;
    { For undo list only - returns True if undo list contains some items with regard
      to the @link(eoUndoAfterSave) option. }
    property Modified: Boolean read GetModified write SetModified;
    { Allows to call TKCustomMemo.@link(TKCustomMemo.OnChange) event}
    property OnChange: TKEditUndoChangeEvent read FOnChange write FOnChange;
  end;

  TKMemoBlock = class(TObject)
  private
    FBlockIndex: Integer;
    procedure SetBlockIndex(Value: Integer);
  public
    constructor Create; virtual;
    property BlockIndex: Integer read FBlockIndex write SetBlockIndex;
  end;

  TKMemoParagraph = class(TObject)
  private
    FBlocks: array of TKMemoBlock;
    FExtent: TSize;
    FMemo: TKCustomMemo;
    FText: WideString;
    function GetIndent: Integer;
    procedure SetIndent(const Value: Integer);
    procedure SetText(const Value: WideString);
  protected
    function GetBlockCount: Integer; virtual;
    function GetLineCount: Integer; virtual;
    procedure Update; virtual;
  public
    constructor Create(AMemo: TKCustomMemo);
    function BlockRect(BlockIndex: Integer): TRect; virtual;
    procedure DeleteBlock(BlockIndex, BlockCount: Integer); virtual;
    function IndexToLine(BlockIndex: Integer): Integer; virtual;
    procedure InsertText(BlockIndex: Integer; const Value: WideString); virtual;
    procedure InsertBlock(BlockIndex: Integer; Value: TKMemoBlock); virtual;
    function LineAt(BlockIndex: Integer): WideString; virtual;
    procedure Paint(const Origin: TPoint); virtual;
    procedure PaintTo(ACanvas: TCanvas; const Origin: TPoint); virtual;
    property BlockCount: Integer read GetBlockCount;
    property Extent: TSize read FExtent;
    property Indent: Integer read GetIndent write SetIndent;
    property LineCount: Integer read GetLineCount;
    property Memo: TKCustomMemo read FMemo;
    property Text: WideString read FText write SetText;
  end;

  TKMemoParagraphs = class(TObjectList)
  private
    function GetParagraph(Index: Integer): TKMemoParagraph;
    procedure SetParagraph(Index: Integer; const Value: TKMemoParagraph);
  protected
    function GetExtent: TSize; virtual;
  public
    function BlockRect(Index: Integer; ACanvas: TCanvas): TRect; virtual;
    procedure DeleteBlocks(Index, Count: Integer); virtual;
    function IndexToParagraphInfo(Index: Integer): TKMemoParagraphInfo; virtual;
    procedure InsertText(Index: Integer; const Value: WideString); virtual;
    procedure InsertObject(Index: Integer; const Value: WideString); virtual;
    function LineToParagraphInfo(LineIndex: Integer): TKMemoParagraphInfo; virtual;
    function ParagraphInfoToIndex(const Info: TKMemoParagraphInfo): Integer; virtual;
    function ParagraphInfoToLine(const Info: TKMemoParagraphInfo): Integer; virtual;
    procedure ReplaceText(Index, Count: Integer; const Value: WideString); virtual;
    function RetrieveText(Index: Integer; ALength: Integer): WideString; virtual;
    property Paragraph[ParaIndex: Integer]: TKMemoParagraph read GetParagraph write SetParagraph; default;
    property Extent: TSize read GetExtent;
  end;

 { @abstract(Multi line text editor base component). }
  TKCustomMemo = class(TCustomControl)
  private
    FBorderStyle: TBorderStyle;
    FColors: TKEditColors;
    FDisabledDrawStyle: TKEditDisabledDrawStyle;
    FFlags: Cardinal;
    FHorzExtent: Integer;
    FHorzScrollStep: Integer;
    FKeyMapping: TKEditKeyMapping;
    FLeftPos: Integer;
    FLineHeightPercent: Integer;
    FLines: TWideStrings;
    FMouseWheelAccumulator: Integer;
    FParagraphs: TKMemoParagraphs;
    FOptions: TKEditOptions;
    FRedoList: TKEditChangeList;
    FScrollBars: TScrollStyle;
    FScrollDeltaX: Integer;
    FScrollDeltaY: Integer;
    FScrollSpeed: Cardinal;
    FScrollTimer: TTimer;
    FSelEnd: Integer;
    FSelStart: Integer;
    FSize: Integer;
    FText: PWideChar;
    FTopPos: Integer;
    FUndoList: TKEditChangeList;
    FUpdateLock: Integer;
    FVertExtent: Integer;
    FVertScrollStep: Integer;
    FOnChange: TNotifyEvent;
    FOnDropFiles: TKEditDropFilesEvent;
    FOnPrintSpace: TKEditPrintSpaceEvent;
    FOnPrintNotify: TKEditPrintNotifyEvent;
    FOnPrintPaint: TKEditPrintPaintEvent;
    FOnReplaceText: TKEditReplaceTextEvent;
    function GetCommandKey(Index: TKEditCommand): TKEditKey;
    function GetCaretVisible: Boolean;
    function GetText: WideString;
    function GetEmpty: Boolean;
    function GetInsertMode: Boolean;
    function GetLineCount: Integer;
    function GetModified: Boolean;
    function GetReadOnly: Boolean;
    function GetSelLength: Integer;
    function GetSelText: WideString;
    function GetUndoLimit: Integer;
    function IsOptionsStored: Boolean;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColors(Value: TKEditColors);
    procedure SetCommandKey(Index: TKEditCommand; Value: TKEditKey);
    procedure SetDisabledDrawStyle(Value: TKEditDisabledDrawStyle);
    procedure SetKeyMapping(Value: TKEditKeyMapping);
    procedure SetLeftPos(Value: Integer);
    procedure SetLines(Value: TWideStrings);
    procedure SetModified(Value: Boolean);
    procedure SetOptions(const Value: TKEditOptions);
    procedure SetParagraphs(Value: TKMemoParagraphs);
    procedure SetReadOnly(Value: Boolean);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetScrollSpeed(Value: Cardinal);
    procedure SetSelEnd(Value: Integer);
    procedure SetSelLength(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetTopPos(Value: Integer);
    procedure SetUndoLimit(Value: Integer);
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMNCPaint(var Msg: TWMNCPaint); message WM_NCPAINT;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  {$IFDEF USE_THEMES}
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
  {$ENDIF}
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
  protected
    { Inserts a single crCaretPos item into undo list. Unless Force is set to True,
      this change will be inserted only if previous undo item is not crCaretPos. }
    procedure AddUndoCaretPos(Force: Boolean = True); dynamic;
    { Inserts a single character change into undo list.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>ItemReason</I> - specifies the undo/redo item reason - most likely
      crInsertChar or crDeleteChar.</LI>
      <LI><I>Data</I> - specifies the character needed to restore the original
      text state</LI>
      <LI><I>Inserted</I> - for the urInsert* items, specifies the current
      @link(TKCustomMemo.InsertMode) status.</LI>
      </UL>}
    procedure AddUndoChar(ItemReason: TKEditChangeReason; Data: WideChar;
      Inserted: Boolean = True); dynamic;
    { Inserts a string change into undo list.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>ItemReason</I> - specifies the undo/redo item reason - crInsert* or
      crDelete*.</LI>
      <LI><I>Data</I> - specifies the text string needed to restore the original
      text state</LI>
      <LI><I>Inserted</I> - for the urInsert* items, specifies the current
      @link(TKCustomMemo.InsertMode) status.</LI>
      </UL>}
    procedure AddUndoString(ItemReason: TKEditChangeReason; const Data: WideString;
      Inserted: Boolean = True); dynamic;
    { Begins a new undo group. Use the GroupReason parameter to label it. }
    procedure BeginUndoGroup(GroupReason: TKEditChangeReason);
    { Determines whether an ecScroll* command can be executed. }
    function CanScroll(Command: TKEditCommand): Boolean; virtual;
    { Overriden method - defines additional styles for the hex editor window (scrollbars etc.). }
    procedure CreateParams(var Params: TCreateParams); override;
    { Overriden method - adjusts file drag&drop functionality. }
    procedure CreateWnd; override;
    { Overriden method - adjusts file drag&drop functionality. }
    procedure DestroyWnd; override;
    { Calls the @link(TKCustomMemo.OnChange) event. }
    procedure DoChange; virtual;
    { Overriden method - handles mouse wheel messages. }
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    { Calls the @link(TKCustomMemo.OnPrintNotify) event. }
    procedure DoPrintNotify(Status: TKEditPrintStatus;
      const Info: TKEditPageCopyInfo; var Abort: Boolean); virtual;
    { Calls the @link(TKCustomMemo.OnPrintPaint) event. }
    procedure DoPrintPaint(const Info: TKEditPageCopyInfo;
      const PaintData: TKEditPaintData;
      const Settings: TKEditPrintSettings); virtual;
    { Closes the undo group created by @link(TKCustomMemo.BeginUndoGroup). }
    procedure EndUndoGroup;
    { If Value is True, includes the flag specified by AFLag to @link(FGridFlags).
      If Value is False, excludes the flag specified by AFLag from @link(FGridFlags). }
    procedure FlagAssign(AFlag: Cardinal; Value: Boolean);
    { Excludes the flag specified by AFLag from @link(FGridFlags). }
    procedure FlagClear(AFlag: Cardinal);
    { Includes the flag specified by AFLag to @link(FGridFlags). }
    procedure FlagSet(AFlag: Cardinal);
    { If the flag specified by AFLag is included in @link(FGridFlags), FlagToggle
      excludes it and vice versa. }
    procedure FlagToggle(AFlag: Cardinal);
    { Ensures that Font.Size is not too small or big. }
    procedure FontChange(Sender: TObject); virtual;
    { Inserts a character at specified position.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>At</I> - position where the character should be inserted.</LI>
      <LI><I>Value</I> - character</LI>
      </UL> }
    procedure InsertChar(At: Integer; Value: WideChar);
    { Inserts a string at specified position.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>At</I> - position where the string should be inserted.</LI>
      <LI><I>Value</I> - string</LI>
      </UL> }
    procedure InsertString(At: Integer; const Value: WideString);
    { Overriden method - processes virtual key strokes according to current @link(TKCustomMemo.KeyMapping). }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    { Overriden method - updates caret position/selection. }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - updates caret position/selection and initializes scrolling
      when needed. }
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - releases mouse capture acquired by MouseDown }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Overriden method - calls PaintLines to paint text lines into window client area }
    procedure Paint; override;
    { Prints a page (given by Info.Page) to a printer/preview canvas.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>PrintData</I> - print job description structure</LI>
      <LI><I>PaintData</I> - paint settings</LI>
      <LI><I>PrintSettings</I> - printer settings</LI>
      <LI><I>Info</I> - page/copy information structure</LI>
      </UL> }
    procedure PrintPage(const PrintData: TKEditPrintData; const PaintData: TKEditPaintData;
      const PrintSettings: TKEditPrintSettings; const Info: TKEditPageCopyInfo); virtual;
    { Grants the input focus to the control when possible and the control has had none before }
    procedure SafeSetFocus;
    { Scrolls the text either horizontally by DeltaHorz scroll units or vertically
      by DeltaVert scroll units (lines) or in both directions. CodeHorz and CodeVert
      are the codes coming from WM_HSCROLL or WM_VSCROLL messages. }
    procedure Scroll(CodeHorz, CodeVert, DeltaHorz, DeltaVert: Integer;
      CallScrollWindow: Boolean); dynamic;
    { Scrolls the hex editor window horizontaly by DeltaHorz scroll units and/or
      vertically by DeltaVert scroll units (lines). }
    procedure ScrollBy(DeltaHorz, DeltaVert: Integer);
    { Determines if a cell specified by ACol and ARow should be scrolled, i.e. is
      not fully visible. }
    function ScrollNeeded(Index: Integer; out DeltaCol, DeltaRow: Integer): Boolean; dynamic;
    { Initializes or expands the current selection and performs all necessary adjustments. }
    function SelectionMove(NewSelStart, NewSelEnd: Integer): Boolean; dynamic;
    { Performs necessary adjustments if the text is modified programatically
      (not by user). }
    procedure TextChanged; dynamic;
    { Calls the @link(TKCustomMemo.DoChange) method. }
    procedure UndoChange(Sender: TObject; ItemReason: TKEditChangeReason);
    { Updates caret position, shows/hides caret according to the input focus
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Recreate</I> - set to True to recreate the caret after it has already
      been created and displayed</LI>
      </UL> }
    procedure UpdateCaret(Recreate: Boolean = False); virtual;
    { Updates the scrolling range }
    procedure UpdateScrollRange(CallInvalidate: Boolean); virtual;
    property Paragraphs: TKMemoParagraphs read FParagraphs write SetParagraphs;
    { Redo list manager - made accessible for descendant classes }
    property RedoList: TKEditChangeList read FRedoList;
    { Undo list manager - made accessible for descendant classes }
    property UndoList: TKEditChangeList read FUndoList;
  public
    { Performs necessary initializations - default values to properties, create
      undo/redo list managers }
    constructor Create(AOwner: TComponent); override;
    { Destroy instance, undo/redo list managers, dispose buffer... }
    destructor Destroy; override;
    { Takes property values from another TKCustomMemo class }
    procedure Assign(Source: TPersistent); override;
    { Determines whether the caret is visible }
    function CaretInView: Boolean;
    function ClampInView(Index: Integer): Boolean;
    { Clears entire data buffer. Unlike @link(ecClearAll) clears everything
      inclusive undo a redo lists. }
    procedure Clear;
    { Clears undo (and redo) list }
    procedure ClearUndo;
    { Determines whether given command can be executed at this time. Use this
      function in TAction.OnUpdate events.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Command</I> - specifies the command to inspect</LI>
      </UL> }
    function CommandEnabled(Command: TKEditCommand): Boolean; virtual;
    { Executes given command. This function first calls CommandEnabled to
      assure given command can be executed.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Command</I> - specifies the command to execute</LI>
      <LI><I>Data</I> - specifies the data needed for the command</LI>
      </UL> }
    function ExecuteCommand(Command: TKEditCommand; Data: Pointer = nil): Boolean; virtual;
    { Determines whether a flag specified by AFLag is included in @link(FGridFlags). }
    function Flag(AFlag: Cardinal): Boolean;
    { Returns current maximum value for the @link(TKCustomMemo.LeftPos) property. }
    function GetMaxLeftPos: Integer; virtual;
    { Returns current maximum value for the @link(TKCustomMemo.TopPos) property. }
    function GetMaxTopPos: Integer; virtual;
    { Returns print settings for the given print data. Also validates the print data.
      This function fails if no printer is installed. }
    function GetPrintSettings(var Data: TKEditPrintData;
      var Settings: TKEditPrintSettings; Print: Boolean = False): Boolean; virtual;
    { Returns page count  for the given print data }
    function GetPageCount(var Data: TKEditPrintData): Integer;
    { Returns "real" selection end - with always higher index value than selection
      start value }
    function GetRealSelEnd: Integer;
    { Returns "real" selection start - with always lower index value than selection
      end value }
    function GetRealSelStart: Integer;
    { Converts a text buffer index into client area coordinates.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Value</I> - index to convert</LI>
      </UL> }
    function IndexToPoint(Value: Integer): TPoint; virtual;
    { Determines whether the given text buffer index is valid.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Value</I> - index to examine</LI>
      </UL> }
    function IndexValid(Value: Integer): Boolean; virtual;
    procedure LockUpdate; virtual;
    { Converts client area coordinates into a text buffer index.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>P</I> - window client area coordinates</LI>
      <LI><I>OutOfText</I> - set to True to compute selection even if the
      the supplied coordinates are outside of the text space</LI>
      </UL> }
    function PointToIndex(P: TPoint; OutOfArea: Boolean): Integer; virtual;
    { Determines whether a seletion is available. }
    function SelAvail: Boolean;
    procedure UnlockUpdate; virtual;
    { Returns True if control updating is not locked, i.e. there is no open
      LockUpdate and UnlockUpdate pair. }
    function UpdateUnlocked: Boolean; virtual;
    { Validates a text buffer index.
      <UL>
      <LH>Parameters:</LH>
      <LI><I>Value</I> - index to validate</LI>
      </UL> }
    procedure ValidateIndex(var Value: Integer); virtual;
    { Specifies the appearance of the control border }
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    { Returns current caret position = selection end }
    property CaretPos: Integer read FSelEnd;
    { Returns True if caret is visible }
    property CaretVisible: Boolean read GetCaretVisible;
    { Makes it possible to take all color properties from another TKCustomMemo class }
    property Colors: TKEditColors read FColors write SetColors;
    { Specifies a new key stroke combination for given command. }
    property CommandKey[Index: TKEditCommand]: TKEditKey read GetCommandKey write SetCommandKey;
    { Specifies the style how the outline is drawn when editor is disabled. }
    property DisabledDrawStyle: TKEditDisabledDrawStyle read FDisabledDrawStyle write SetDisabledDrawStyle default cDisabledDrawStyleDef;
    { Returns True if text buffer is empty. }
    property Empty: Boolean read GetEmpty;
    { Returns True if insert mode is on }
    property InsertMode: Boolean read GetInsertMode;
    { Specifies the current key stroke mapping scheme }
    property KeyMapping: TKEditKeyMapping read FKeyMapping write SetKeyMapping;
    { Specifies the horizontal scroll position }
    property LeftPos: Integer read FLeftPos write SetLeftPos;
    {  }
    property LineCount: Integer read GetLineCount;
    property Lines: TWideStrings read FLines write SetLines;
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
    { Returns selected text in many different formats }
    property SelText: WideString read GetSelText;
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
    { When assigned, this event will be invoked to preserve space for custom header
      and/or footer }
    property OnPrintSpace: TKEditPrintSpaceEvent read FOnPrintSpace write FOnPrintSpace;
    { When assigned, this event will be invoked at certain phases of the actually running
      print job }
    property OnPrintNotify: TKEditPrintNotifyEvent read FOnPrintNotify write FOnPrintNotify;
    { When assigned, this event will be invoked before anything is drawn onto the
      printer canvas }
    property OnPrintPaint: TKEditPrintPaintEvent read FOnPrintPaint write FOnPrintPaint;
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
    { See TKCustomMemo.@link(TKCustomMemo.BorderStyle) for details }
    property BorderStyle;
    { Inherited property - see Delphi help }
    property BorderWidth;
    { See TKCustomMemo.@link(TKCustomMemo.Colors) for details }
    property Colors;
    { Inherited property - see Delphi help }
    property Constraints;
    { Inherited property - see Delphi help }
    property Ctl3D;
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
    { See TKCustomMemo.@link(TKCustomMemo.OnPrintSpace) for details }
    property OnPrintSpace;
    { See TKCustomMemo.@link(TKCustomMemo.OnPrintNotify) for details }
    property OnPrintNotify;
    { See TKCustomMemo.@link(TKCustomMemo.OnPrintPaint) for details }
    property OnPrintPaint;
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

{ Fills the given search data structure with default values }
procedure DefaultSearchData(var Data: TKEditSearchData);

{ Fills the given print data structure with default values }
procedure DefaultPrintData(var Data: TKEditPrintData);

var
  { Defines the default key stroke mapping scheme }
  DefaultKeyMapping: TKEditKeyMapping;

implementation

uses
  ShellApi, ClipBrd, Printers,
{$IFDEF USE_THEMES}
  Themes,
{$ENDIF}
  Types, KGraphics;

type
  TKMemoStrings = class(TWideStrings)
  private
    FMemo: TKCustomMemo;
  protected
    function Get(Index: Integer): WideString; override;
    function GetCount: Integer; override;
    procedure Put(Index: Integer; const S: WideString); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: WideString); override;
  end;

procedure DefaultSearchData(var Data: TKEditSearchData);
begin
  with Data do
  begin
    ErrorReason := eseOk;
    Options := [esoAll, esoFirstSearch, esoPrompt];
    SelStart := 0;
    SelEnd := 0;
    TextToFind := '';
    TextToReplace := '';
  end;
end;

procedure DefaultPrintData(var Data: TKEditPrintData);
begin
  with Data do
  begin
    MarginUnits := muCM;
    Margins.Left := 1;
    Margins.Top := 1;
    Margins.Right := 1;
    Margins.Bottom := 1;
    Options := [epoPageNumbers, epoFitToPage];
    Range := eprAll;
    StartPage := 1;
    EndPage := 0;
    Copies := 1;
    Scale := 100;
    Title := '';
    Settings := nil;
  end;
end;

function OppositeReason(ItemReason: TKEditChangeReason): TKEditChangeReason;
begin
  case ItemReason of
    crDeleteChar: Result := crInsertChar;
    crDeleteString: Result := crInsertString;
    crInsertChar: Result := crDeleteChar;
    crInsertString: Result := crDeleteString;
  else
    Result := ItemReason;
  end;
end;

{ TKEditColors }

constructor TKEditColors.Create(AMemo: TKCustomMemo);
begin
  FMemo := AMemo;
  FColors[ciSelectedBkGnd] := cSelectedBkGndDef;
  FColors[ciSelectedBkGndFocused] := cSelectedBkGndFocusedDef;
  FColors[ciSelectedText] := cSelectedTextDef;
  FColors[ciSelectedTextFocused] := cSelectedTextFocusedDef;
  FColors[ciText] := cTextDef;
  FColors[ciBkGnd] := cBkGndDef;
  ClearBrightColors;
end;

procedure TKEditColors.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TKEditColors then
  begin
    Colors := TKEditColors(Source).Colors;
    FMemo.Invalidate;
  end
end;

procedure TKEditColors.ClearBrightColors;
var
  I: TKEditColorIndex;
begin
  for I := Low(TKEditColorIndex) to High(TKEditColorIndex) do
    FBrightColors[I] := clNone;
end;

function TKEditColors.GetColor(Index: TKEditColorIndex): TColor;
const
  BkGndSet = [ciSelectedBkGnd, ciSelectedBkGndFocused, ciBkgnd];
begin
  case FColorScheme of
    ecsBright:
    begin
      if FBrightColors[Index] = clNone then
        FBrightColors[Index] := KGraphics_JCL.BrightColor(FColors[Index], 0.5, bsOfTop);
      Result := FBrightColors[Index];
    end;
    ecsBW: if Index in BkGndSet then Result := clWhite else Result := clBlack;
    ecsGrayed: if Index in BkGndSet then Result := clWindow else Result := clGrayText;
  else
    Result := FColors[Index];
  end;
end;

function TKEditColors.GetColorEx(Index: TKEditColorIndex): TColor;
begin
  Result := FColors[Index];
end;

procedure TKEditColors.SetColor(Index: TKEditColorIndex; Value: TColor);
begin
  if FColors[Index] <> Value then
  begin
    FColors[Index] := Value;
    FBrightColors[Index] := clNone;
    if not (csLoading in FMemo.ComponentState) and FMemo.HandleAllocated then
      FMemo.Invalidate;
  end;
end;

procedure TKEditColors.SetColorEx(Index: TKEditColorIndex; Value: TColor);
begin
  if FColors[Index] <> Value then
  begin
    FColors[Index] := Value;
    FBrightColors[Index] := clNone;
  end;
end;

procedure TKEditColors.SetColors(const Value: TKEditColorArray);
begin
  FColors := Value;
  ClearBrightColors;
end;

{ TKEditChangeList }

constructor TKEditChangeList.Create(AEditor: TKCustomMemo;
  RedoList: TKEditChangeList);
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

procedure TKEditChangeList.AddChange(ItemReason: TKEditChangeReason;
  const Data: string; Inserted: Boolean);
var
  P: PKHexEditorChangeItem;
begin
  // don't allow succesive crCaretPos
  if (ItemReason = crCaretPos) and not Inserted and (FIndex >= 0) and
    (PKHexEditorChangeItem(Items[FIndex]).ItemReason = crCaretPos) then
    Exit;
  if FIndex < FLimit - 1 then
  begin
    if FIndex < Count - 1 then
      Inc(FIndex)
    else
      FIndex := Add(New(PKHexEditorChangeItem));
    P := Items[FIndex];
    if FGroupUseLock > 0 then
    begin
      P.Group := FGroup;
      P.GroupReason := FGroupReason;
    end else
    begin
      P.Group := 0;
      P.GroupReason := ItemReason;
    end;
    P.ItemReason := ItemReason;
    P.SelLength := FEditor.SelLength;
    P.SelStart := FEditor.SelStart;
    P.Data := Data;
    P.Inserted := Inserted;
    if FRedoList <> nil then
      FRedoList.Clear;
    if Assigned(FOnChange) then
      FOnChange(Self, ItemReason);
  end;
end;

procedure TKEditChangeList.BeginGroup(GroupReason: TKEditChangeReason);
begin
  if FGroupUseLock = 0 then
  begin
    FGroupReason := GroupReason;
    Inc(FGroup);
    if FGroup = 0 then Inc(FGroup);
  end;
  Inc(FGroupUseLock);
end;

function TKEditChangeList.CanPeek: Boolean;
begin
  Result := FIndex >= 0;
end;

procedure TKEditChangeList.Clear;
begin
  inherited;
  FGroupUseLock := 0;
  FIndex := -1;
  FModifiedIndex := FIndex;
end;

procedure TKEditChangeList.EndGroup;
begin
  if FGroupUseLock > 0 then
    Dec(FGroupUseLock);
end;

function TKEditChangeList.GetModified: Boolean;

  function CaretPosOnly: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := FModifiedIndex + 1 to FIndex do
    begin
      if PKHexEditorChangeItem(Items[I]).ItemReason <> crCaretPos then
      begin
        Result := False;
        Exit;
      end;  
    end;
  end;

begin
  Result := (FIndex > FModifiedIndex) and not CaretPosOnly;
end;

procedure TKEditChangeList.Notify(Ptr: Pointer; Action: TListNotification);
var
  P: PKHexEditorChangeItem;
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

function TKEditChangeList.PeekItem: PKHexEditorChangeItem;
begin
  if CanPeek then
  begin
    Result := Items[FIndex];
    Dec(FIndex);
  end else
    Result := nil;
end;

procedure TKEditChangeList.PokeItem;
begin
  if FIndex < Count - 1 then
    Inc(FIndex);
end;

procedure TKEditChangeList.SetGroupData(Group: Integer;
  GroupReason: TKEditChangeReason);
begin
  FGroup := Group;
  FGroupReason := GroupReason;
  FGroupUseLock := 1;
end;

procedure TKEditChangeList.SetLimit(Value: Integer);
begin
  if Value <> FLimit then
  begin
    FLimit := MinMax(Value, cUndoLimitMin, cUndoLimitMax);
    while Count > FLimit do
      Delete(0);
    FIndex := Min(FIndex, FLimit - 1);
  end;
end;

procedure TKEditChangeList.SetModified(Value: Boolean);
begin
  if not Value then
    FModifiedIndex := FIndex;
end;

{ TKMemoStrings }

function TKMemoStrings.Get(Index: Integer): WideString;
var
  PI: TKMemoParagraphInfo;
begin
  PI := FMemo.Paragraphs.LineToParagraphInfo(Index);
  Result := FMemo.Paragraphs[PI.ParaIndex].LineAt(PI.BlockIndex);
end;

function TKMemoStrings.GetCount: Integer;
begin
  Result := FMemo.LineCount;
end;

procedure TKMemoStrings.Put(Index: Integer; const S: WideString);
var
  PI: TKMemoParagraphInfo;
begin
  PI := FMemo.Paragraphs.LineToParagraphInfo(Index);
  FMemo.Paragraphs[PI.ParaIndex].Text := S;
end;

procedure TKMemoStrings.Insert(Index: Integer; const S: WideString);
var
  PI: TKMemoParagraphInfo;
  P: TKMemoParagraph;
begin
  PI := FMemo.Paragraphs.LineToParagraphInfo(Index);
  P := TKMemoParagraph.Create(FMemo);
  FMemo.Paragraphs.Insert(PI.ParaIndex, P);
  P.Text := S;
end;

procedure TKMemoStrings.Delete(Index: Integer);
var
  PI: TKMemoParagraphInfo;
begin
  PI := FMemo.Paragraphs.LineToParagraphInfo(Index);
  FMemo.Paragraphs.Delete(PI.ParaIndex);
end;

procedure TKMemoStrings.Clear;
begin
  FMemo.Clear;
end;

procedure TKMemoStrings.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    FMemo.LockUpdate
  else
    FMemo.UnlockUpdate;
end;

{ TKCustomMemo }

constructor TKCustomMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWindow;
  ControlStyle := [csOpaque, csClickEvents, csDoubleClicks, csCaptureMouse];
  Font.OnChange := FontChange;
  Height := cHeight;
  ParentColor := False;
  ParentFont := False;
  TabStop := True;
  Width := cWidth;
  FBorderStyle := bsSingle;
  FColors := TKEditColors.Create(Self);
  FDisabledDrawStyle := cDisabledDrawStyleDef;
  FLeftPos := 0;
  FLineHeightPercent := cLineHeightPercentDef;
  FLines := TKMemoStrings.Create;
  FMouseWheelAccumulator := 0;
  FOptions := [eoGroupUndo];
  FKeyMapping := DefaultKeyMapping;
  FParagraphs := TKMemoParagraphs.Create;
  FRedoList := TKEditChangeList.Create(Self, nil);
  FScrollBars := ssBoth;
  FScrollSpeed := cScrollSpeedDef;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := FScrollSpeed;
  FScrollTimer.OnTimer := ScrollTimerHandler;
  FSelStart := 0;
  FSelEnd := 0;
  FTopPos := 0;
  FUndoList := TKEditChangeList.Create(Self, FRedoList);
  FUndoList.OnChange := UndoChange;
  FUpdateLock := 0;
  FOnChange := nil;
  FOnPrintNotify := nil;
  FOnPrintPaint := nil;
  FOnReplaceText := nil;
end;

destructor TKCustomMemo.Destroy;
begin
  inherited;
  FOnChange := nil;
  Font.OnChange := nil;
  FColors.Free;
  FLines.Free;
  FParagraphs.Free;
  FRedoList.Free;
  FUndoList.Free;
end;

procedure TKCustomMemo.AddUndoCaretPos(Force: Boolean);
begin
  FUndoList.AddChange(crCaretPos, '', Force);
end;

procedure TKCustomMemo.AddUndoChar(ItemReason: TKEditChangeReason; Data: WideChar;
  Inserted: Boolean = True);
var
  S: string;
begin
  SetLength(S, 1);
  S[1] := Char(Data);
  FUndoList.AddChange(ItemReason, S, Inserted);
end;

procedure TKCustomMemo.AddUndoString(ItemReason: TKEditChangeReason;
  const Data: WideString; Inserted: Boolean = True);
begin
  if Data <> '' then
    FUndoList.AddChange(ItemReason, Data, Inserted);
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
      Self.Ctl3D := Ctl3D;
      Self.DisabledDrawStyle := DisabledDrawStyle;
      Self.DragCursor := DragCursor;
      Self.DragKind := DragKind;
      Self.DragMode := DragMode;
      Self.Enabled := Enabled;
      Self.Font := Font;
      Self.ImeMode := ImeMode;
      Self.ImeName := ImeName;
      Self.KeyMapping := KeyMapping;
      Self.Modified := False;
      Self.Options := Options;
      Self.Paragraphs := Paragraphs;
      Self.ParentBiDiMode := ParentBiDiMode;
      Self.ParentColor := ParentColor;
      Self.ParentCtl3D := ParentCtl3D;
      Self.ParentFont := ParentFont;
      Self.ParentShowHint := ParentShowHint;
      Self.PopupMenu := PopupMenu;
      Self.ScrollBars := ScrollBars;
      Self.SelLength := SelLength;
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

procedure TKCustomMemo.BeginUndoGroup(GroupReason: TKEditChangeReason);
begin
  FUndoList.BeginGroup(GroupReason);
end;

function TKCustomMemo.CanScroll(Command: TKEditCommand): Boolean;
var
  P: TPoint;
begin
  case Command of
    ecScrollUp:  Result := FTopPos > 0;
    ecScrollDown: Result := FTopPos < FVertExtent - 1;
    ecScrollLeft: Result := FLeftPos > 0;
    ecScrollRight: Result := FLeftPos < FHorzExtent - 1;
    ecScrollCenter:
    begin
      P := IndexToPoint(FSelEnd);
      P.X := P.X - ClientWidth div 2;
      P.Y := P.Y - ClientHeight div 2;
      Result :=
        (FLeftPos > 0) and (P.X < 0) or
        (FLeftPos < FHorzExtent - 1) and (P.X > 0) or
        (FTopPos > 0) and (P.Y < 0) or
        (FTopPos < FVertExtent - 1) and (P.Y > 0);
    end;
  else
    Result := False;
  end;
end;

function TKCustomMemo.CaretInView: Boolean;
begin
  Result := PtInRect(ClientRect, IndexToPoint(FSelEnd));
end;

function TKCustomMemo.ClampInView(Index: Integer): Boolean;
var
  DeltaHorz, DeltaVert: Integer;
begin
  Result := ScrollNeeded(Index, DeltaHorz, DeltaVert);
  if Result then
  begin
    Scroll(cNoScrollCode, cNoScrollCode, DeltaHorz, DeltaVert, True);
    FScrollTimer.Enabled := True;
  end;
end;

procedure TKCustomMemo.Clear;
begin
  FParagraphs.Clear;
end;

procedure TKCustomMemo.ClearUndo;
begin
  FUndoList.Clear;
  FRedoList.Clear;
end;

procedure TKCustomMemo.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  RecreateWnd;
end;

procedure TKCustomMemo.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  UpdateCaret;
  Invalidate;
end;

procedure TKCustomMemo.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  FColors.ClearBrightColors;
end;

function TKCustomMemo.CommandEnabled(Command: TKEditCommand): Boolean;
var
  L: Integer;
begin
  if Enabled and Visible and not (csDesigning in ComponentState) then
  begin
    L := SelLength;
    case Command of
      // movement commands
      ecLeft, ecSelLeft: Result := FSelEnd > 0;
{      ecRight, ecSelRight: Result := FSelEnd < FSize;
      ecUp, ecSelUp: Result := FSelEnd >= FLineSize;
      ecDown, ecSelDown: Result := (FEditArea <> eaNone) and (FSelEnd.Index < FSize);
      ecLineStart, ecSelLineStart: Result := (FEditArea <> eaNone) and (FSelEnd.Index mod FLineSize > 0);
      ecLineEnd, ecSelLineEnd: Result := (FEditArea <> eaNone) and (FSelEnd.Index mod FLineSize < Min(FLineSize - 1, FSize));
      ecPageUp, ecSelPageUp: Result := FSelEnd.Index >= FlineSize;
      ecPageDown, ecSelPageDown: Result := (FEditArea <> eaNone) and (FSelEnd.Index < FSize div FLineSize * FLineSize);
      ecPageLeft, ecSelPageLeft: Result := (FEditArea <> eaNone) and (GetPageHorz > 0) and (FSelEnd.Index mod FLineSize > 0);
      ecPageRight, ecSelPageRight: Result := (FEditArea <> eaNone) and (GetPageHorz > 0) and (FSelEnd.Index mod FLineSize < Min(FLineSize - 1, FSize));
      ecPageTop, ecSelPageTop: Result := (FEditArea <> eaNone) and (FSelEnd.Index > 0) and (SelToPoint(MakeSelection(FSelEnd.Index, 0), FEditArea).Y div FCharHeight <> 0);
      ecPageBottom, ecSelPageBottom: Result := (FEditArea <> eaNone) and (FSelEnd.Index < FSize) and ((ClientHeight - SelToPoint(MakeSelection(FSelEnd.Index, 0), FEditArea).Y) div FCharHeight - 1 <> 0);
      ecEditorTop, ecSelEditorTop: Result := FSelEnd.Index > 0;
      ecEditorBottom, ecSelEditorBottom: Result := (FEditArea <> eaNone) and (FSelEnd.Index < FSize);
      ecGotoXY, ecSelGotoXY: Result := True;
      // scroll commands
      ecScrollUp, ecScrollDown, ecScrollLeft, ecScrollRight, ecScrollCenter: Result := CanScroll(Command);
      // editing commands
      ecUndo: Result := not ReadOnly and FUndoList.CanPeek;
      ecRedo: Result := not ReadOnly and FRedoList.CanPeek;
      ecCopy, ecCut: Result := not Empty and (not ReadOnly or (Command = ecCopy)) and ((L.Index <> 0) or (L.Digit <> 0));
      ecPaste: Result := not ReadOnly and (FEditArea <> eaNone) and (ClipBoard.FormatCount > 0);
      ecInsertChar: Result := not ReadOnly and (FEditArea <> eaNone);
      ecInsertDigits: Result := not ReadOnly and (FEditArea = eaDigits);
      ecInsertString: Result := not ReadOnly and (FEditArea <> eaNone);
      ecDeleteLastByte: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone) and ((L.Index > 0) or (FSelEnd.Index > 0));
      ecDeleteByte: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone) and ((L.Index > 0) or (FSelEnd.Index < FSize));
      ecDeleteBOL: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone) and ((L.Index > 0) or (FSelEnd.Index mod FLineSize > 0));
      ecDeleteEOL: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone) and ((L.Index > 0) or (FSelEnd.Index mod FLineSize < Min(FLineSize, FSize)));
      ecDeleteLine: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone) and ((L.Index > 0) or (FSelEnd.Index mod FLineSize > 0) or (FSelEnd.Index < FSize));
      ecSelectAll: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone);
      ecClearAll: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone);
      ecClearIndexSelection, ecClearSelection: Result := not (Empty or ReadOnly) and (FEditArea <> eaNone) and (L.Index > 0);
      ecSearch: Result := not Empty;
      ecReplace: Result := not (Empty or ReadOnly);
      ecInsertMode: Result := elOverwrite in FStates;
      ecOverwriteMode: Result := not (elOverwrite in FStates);}
    else
      Result := True;
    end;
  end else
    Result := False;
end;

procedure TKCustomMemo.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    if FScrollBars in [ssVertical, ssBoth] then Style := Style or WS_VSCROLL;
    if FScrollBars in [ssHorizontal, ssBoth] then Style := Style or WS_HSCROLL;
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
      begin
        Style := Style and not WS_BORDER;
        ExStyle := ExStyle or WS_EX_CLIENTEDGE;
      end
      else
        Style := Style or WS_BORDER;
  end;
end;

procedure TKCustomMemo.CreateWnd;
begin
  inherited;
  if (eoDropFiles in FOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, TRUE);
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

procedure TKCustomMemo.DoPrintNotify(Status: TKEditPrintStatus;
  const Info: TKEditPageCopyInfo; var Abort: Boolean);
begin
  if Assigned(FOnPrintNotify) then
    FOnPrintNotify(Self, Status, Info, Abort);
end;

procedure TKCustomMemo.DoPrintPaint(const Info: TKEditPageCopyInfo;
  const PaintData: TKEditPaintData; const Settings: TKEditPrintSettings);
begin
  if Assigned(FOnPrintPaint) then
    FOnPrintPaint(Self, Info, PaintData, Settings);
end;

procedure TKCustomMemo.EndUndoGroup;
begin
  FUndoList.EndGroup;
end;

function TKCustomMemo.ExecuteCommand(Command: TKEditCommand;
  Data: Pointer): Boolean;
{var
  I, J, K, M, N, O: Integer;
  CanInsert, MoreBytes, Found, PAbort, MatchCase: Boolean;
  C1, C2, C3: Char;
  S, S_FirstChar, S_LastChar, T: string;
  BA: PByteArray;
  P: TPoint;
  L, OldSelStart, OldSelEnd, Sel1, Sel2: Integer;
  PChI, PChI_First, PChI_Next: PKHexEditorChangeItem;
  PSD: PKHexEditorSearchData;
  PPD: PKHexEditorPrintData;
  PS: TKEditPrintSettings;
  PCInfo: TKEditPageCopyInfo;
  PaintData: TKEditPaintData;
  ReplaceAction: TKEditReplaceAction;
  H: THandle;}
begin
  Result := False;
  if CommandEnabled(Command) then
  begin
    Result := True;
{    L := SelLength;
    OldSelEnd := FSelEnd;
    OldSelStart := FSelStart;
    case Command of
      ecLeft..ecSelGotoXY: AddUndoCaretPos(False);
    end;  
    case Command of
      ecLeft, ecSelLeft:
      begin
        InternalMoveLeft;
        SelectionChanged(Command <> ecSelLeft);
      end;
      ecRight, ecSelRight:
      begin
        InternalMoveRight;
        SelectionChanged(Command <> ecSelRight);
      end;
      ecUp, ecSelUp:
      begin
        Dec(FSelEnd.Index, FLineSize);
        SelectionChanged(Command <> ecSelUp);
      end;
      ecDown, ecSelDown:
      begin
        Inc(FSelEnd.Index, FLineSize);
        SelectionChanged(Command <> ecSelDown);
      end;
      ecLineStart, ecSelLineStart:
      begin
        FSelEnd := MakeSelection((FSelEnd.Index div FLineSize) * FLineSize, 0);
        SelectionChanged(Command <> ecSelLineStart);
      end;
      ecLineEnd, ecSelLineEnd:
      begin
        FSelEnd := MakeSelection((FSelEnd.Index div FLineSize) * FLineSize + FLineSize - 1, cDigitCount - 1);
        SelectionChanged(Command <> ecSelLineEnd);
      end;
      ecPageUp, ecSelPageUp:
      begin
        Dec(FSelEnd.Index, Min(ClientHeight div FCharHeight, FSelEnd.Index div FLineSize) * FLineSize);
        SelectionChanged(Command <> ecSelPageUp);
      end;
      ecPageDown, ecSelPageDown:
      begin
        Inc(FSelEnd.Index, Min(ClientHeight div FCharHeight, (FSize - FSelEnd.Index) div FLineSize) * FLineSize);
        SelectionChanged(Command <> ecSelPageDown);
      end;
      ecPageLeft, ecSelPageLeft:
      begin
        Dec(FSelEnd.Index, Min(GetPageHorz, FSelEnd.Index mod FLineSize));
        SelectionChanged(Command <> ecSelPageLeft);
      end;
      ecPageRight, ecSelPageRight:
      begin
        Inc(FSelEnd.Index, Min(GetPageHorz, FLineSize - 1 - FSelEnd.Index mod FLineSize));
        SelectionChanged(Command <> ecSelPageRight);
      end;
      ecPageTop, ecSelPageTop:
      begin
        P := SelToPoint(MakeSelection(FSelEnd.Index, 0), FEditArea);
        Dec(FSelEnd.Index, P.Y div FCharHeight * FLineSize);
        SelectionChanged(Command <> ecSelPageTop);
      end;
      ecPageBottom, ecSelPageBottom:
      begin
        P := SelToPoint(MakeSelection(FSelEnd.Index, 0), FEditArea);
        Inc(FSelEnd.Index, ((ClientHeight - P.Y) div FCharHeight - 1) * FLineSize);
        SelectionChanged(Command <> ecSelPageBottom);
      end;
      ecEditorTop, ecSelEditorTop:
      begin
        FSelEnd := MakeSelection(0, 0);
        SelectionChanged(Command <> ecSelEditorTop);
      end;
      ecEditorBottom, ecSelEditorBottom:
      begin
        FSelEnd := MakeSelection(FSize, 0);
        SelectionChanged(Command <> ecSelEditorBottom);
      end;
      ecGotoXY, ecSelGotoXY:
      begin
        Sel1 := PointToSel(PPoint(Data)^, False, Area);
        if Area <> eaNone then
        begin
          FSelEnd := Sel1;
          FEditArea := Area;
          SelectionChanged(Command <> ecSelGotoXY);
        end else
          Result := False;
      end;
      // scroll commands
      ecScrollUp:
      begin
        if (FEditArea <> eaNone) and (SelToPoint(FSelEnd, FEditArea).Y >= GetModifiedClientRect.Bottom - FCharHeight) then
        begin
          ScrollBy(0, -1, False);
          Dec(FSelEnd.Index, FLineSize);
          SelectionChanged(True, False);
          Invalidate;
        end else
          ScrollBy(0, -1, True);
      end;
      ecScrollDown:
      begin
        if (FEditArea <> eaNone) and (SelToPoint(FSelEnd, FEditArea).Y <= GetModifiedClientRect.Top) then
        begin
          ScrollBy(0, 1, False);
          Inc(FSelEnd.Index, FLineSize);
          SelectionChanged(True, False);
          Invalidate;
        end else
          ScrollBy(0, 1, True);
      end;
      ecScrollLeft:
      begin
        if FEditArea <> eaNone then
        begin
          // overscroll check
          P := SelToPoint(MakeSelection(0, 0), FEditArea);
          if P.X < GetModifiedClientRect.Right - FCharWidth then
          begin
            ScrollBy(-1, 0, True);
            P := SelToPoint(FSelEnd, FEditArea);
            if (P.X >= GetModifiedClientRect.Right) and ((FSelEnd.Index mod FLineSize > 0) or (FSelEnd.Digit > 0)) then
              ExecuteCommand(ecLeft)
          end;
        end else
          ScrollBy(-1, 0, True);
      end;
      ecScrollRight:
      begin
        if FEditArea <> eaNone then
        begin
          // overscroll check
          P := SelToPoint(MakeSelection(FLineSize - 1, cDigitCount - 1), FEditArea);
          if P.X > 0 then
          begin
            ScrollBy(1, 0, True);
            P := SelToPoint(FSelEnd, FEditArea);
            if (P.X < 0) and ((FSelEnd.Index mod FLineSize < FLineSize - 1) or (FSelEnd.Digit < cDigitCount - 1)) then
              ExecuteCommand(ecRight)
          end;
        end else
          ScrollBy(1, 0, True);
      end;
      ecScrollCenter:
      begin
        P := SelToPoint(FSelEnd, FEditArea);
        I := (P.X - ClientWidth div 2) div FCharWidth;
        J := (P.Y - ClientHeight div 2) div FCharHeight;
        ScrollBy(I, J, True);
      end;
      // editing commands
      ecUndo:
      begin
        PChI := FUndoList.PeekItem;
        PChI_First := PChI;
        while PChI <> nil do
        begin
          I := Length(PChI.Data);
          J := Min(I, FSize - PChI.SelEnd.Index);
          FRedoList.SetGroupData(PChI.Group, PChI.GroupReason);
          case PChI.ItemReason of
            crCaretPos:
              FRedoList.AddChange(crCaretPos, '');
            crDeleteChar, crDeleteDigits, crDeleteString:
            begin
              if FBuffer <> nil then
              begin
                SetLength(S, J);
                System.Move(FBuffer[PChI.SelEnd.Index], S[1], J);
              end else
                S := '';
              FRedoList.AddChange(OppositeReason(PChI.ItemReason), S, PChI.Inserted);
            end;
            crInsertChar, crInsertDigits, crInsertString:
              FRedoList.AddChange(OppositeReason(PChI.ItemReason), PChI.Data);
          end;
          FSelEnd := PChI.SelEnd;
          FSelStart := PChI.SelStart;
          FEditArea := PChI.EditArea;
          case PChI.ItemReason of
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
          if PChI.ItemReason <> crCaretPos then
            DoChange;
          PChI_Next := FUndoList.PeekItem;
          if (PChI_Next <> nil) and not ((PChI.Group <> 0) and (PChI.Group = PChI_Next.Group) or
            (eoGroupUndo in FOptions) and (PChI_First.GroupReason = PChI_Next.GroupReason)) then
          begin
            FUndoList.PokeItem;
            Break;
          end;
          PChI := PChI_Next;
        end;
        if not CaretInView then
          ExecuteCommand(ecScrollCenter);
      end;
      ecRedo:
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
      end;
      ecInsertChar:
      begin
        BeginUndoGroup(crInsertChar);
        try
          N := PByte(Data)^;
          if L.Index > 0 then
            ExecuteCommand(ecClearSelection);
          ValidateSelection(FSelEnd, FEditArea);
          if FBuffer <> nil then
            I := FBuffer[FSelEnd.Index]
          else
            I := 0;
          CanInsert := (FBuffer = nil) or (FSelEnd.Digit = 0) and
            (not (elOverwrite in FStates) or (FSelEnd.Index = FSize));
          AddUndoByte(crDeleteChar, I, CanInsert);
          if CanInsert then
            InsertChar(FSelEnd.Index, 0)
          else
            Invalidate;
          case FEditArea of
            eaDigits:
            begin
              FBuffer[FSelEnd.Index] := ReplaceDigit(FBuffer[FSelEnd.Index], N, FSelEnd.Digit);
              InternalMoveRight;
            end;
            eaText:
            begin
              FBuffer[FSelEnd.Index] := N;
              InternalMoveRight;
            end;
          end;
          SelectionChanged(True);
        finally
          EndUndoGroup;
        end;
      end;
      ecInsertDigits:
      begin
        S := string(Data);
        if (S <> '') and DigitsToBinStr(S) then
        begin
          BeginUndoGroup(crInsertDigits);
          try
            if L.Index > 0 then
              ExecuteCommand(ecClearSelection);
            ValidateSelection(FSelEnd, FEditArea);
            MoreBytes := Length(S) >= cDigitCount;
            if MoreBytes then
              // we don't move digit positions of the remaining block
              SetLength(S, Length(S) div cDigitCount * cDigitCount);
            J := 0;
            if (FBuffer <> nil) and (not MoreBytes or (FSelEnd.Digit > 0)) then
            begin
              I := FBuffer[FSelEnd.Index];
              S_FirstChar := Char(I);
              S_LastChar := S_FirstChar;
              // split current byte
              AddUndoByte(crInsertChar, I);
              ClearChar(FSelEnd.Index);
              N := Length(S);
              for I := FSelEnd.Digit to cDigitCount - 1 do
              begin
                if J < N then
                begin
                  Inc(J);
                  S_FirstChar := Char(ReplaceDigit(Ord(S_FirstChar[1]), Ord(S[J]), I));
                end else
                  Break;
              end;
              K := Length(S);
              if K > J then
                for I := FSelEnd.Digit - 1 downto 0 do
                begin
                  if K > J then
                  begin
                    S_LastChar := Char(ReplaceDigit(Ord(S_LastChar[1]), Ord(S[K]), I));
                    Dec(K);
                  end else
                    Break;
                end
              else
                S_LastChar := '';
              O := cDigitCount;
            end else
            begin
              S_FirstChar := '';
              S_LastChar := '';
              O := 0;
            end;
            T := '';
            if MoreBytes then
            begin
              N := Length(S) - O;
              O := J;
              for I := 0 to N div cDigitCount - 1 do
              begin
                K := 0;
                for J := 1 to cDigitCount do
                begin
                  K := K * cBase;
                  M := I * 2 + J + O;
                  Inc(K, Ord(S[M]));
                end;
                T := Format('%s%s', [T, Char(K)]);
              end;
            end;
            S := S_FirstChar + T + S_LastChar;
            // always insert (don't overwrite)
            AddUndoString(crDeleteDigits, S);
            InsertString(FSelEnd.Index, S, Length(S));
            SelectionChanged(True);
          finally
            EndUndoGroup;
          end;
        end else
          Result := False;
      end;
      ecInsertString:
      begin
        S := string(Data);
        if S <> '' then
        begin
          BeginUndoGroup(crInsertString);
          try
            if L.Index > 0 then
              ExecuteCommand(ecClearIndexSelection);
            // always insert (don't overwrite)
            AddUndoString(crDeleteString, S);
            InsertString(FSelEnd.Index, S, Length(S));
            SelectionChanged(True);
          finally
            EndUndoGroup;
          end;
        end else
          Result := False;
      end;
      ecDeleteLastByte:
      begin
        if L.Index <> 0 then ExecuteCommand(ecClearSelection) else
        begin
          BeginUndoGroup(crDeleteString);
          try
            AddUndoCaretPos;
            FSelStart.Index := FSelEnd.Index - 1;
            ExecuteCommand(ecClearIndexSelection)
          finally
            EndUndoGroup;
          end;
        end;
      end;
      ecDeleteByte:
      begin
        if L.Index <> 0 then ExecuteCommand(ecClearSelection) else
        begin
          BeginUndoGroup(crDeleteString);
          try
            AddUndoCaretPos;
            FSelStart.Index := FSelEnd.Index + 1;
            ExecuteCommand(ecClearIndexSelection)
          finally
            EndUndoGroup;
          end;
        end;
      end;
      ecDeleteBOL:
      begin
        if L.Index <> 0 then ExecuteCommand(ecClearSelection) else
        begin
          BeginUndoGroup(crDeleteString);
          try
            AddUndoCaretPos;
            FSelStart.Index := (FSelEnd.Index div FLineSize) * FLineSize;
            ExecuteCommand(ecClearIndexSelection)
          finally
            EndUndoGroup;
          end;
        end;
      end;
      ecDeleteEOL:
      begin
        if L.Index <> 0 then ExecuteCommand(ecClearSelection) else
        begin
          BeginUndoGroup(crDeleteString);
          try
            AddUndoCaretPos;
            FSelStart.Index := Min((FSelEnd.Index div FLineSize + 1) * FLineSize, FSize);
            ExecuteCommand(ecClearIndexSelection)
          finally
            EndUndoGroup;
          end;
        end;
      end;
      ecDeleteLine:
      begin
        if L.Index <> 0 then ExecuteCommand(ecClearSelection) else
        begin
          BeginUndoGroup(crDeleteString);
          try
            AddUndoCaretPos;
            FSelStart.Index := (FSelEnd.Index div FLineSize) * FLineSize;
            FSelEnd.Index := Min(FSelStart.Index + FLineSize, FSize);
            ExecuteCommand(ecClearIndexSelection)
          finally
            EndUndoGroup;
          end;
        end;
      end;
      ecSelectAll:
      begin
        AddUndoCaretPos;
        FSelStart := MakeSelection(0, 0);
        FSelEnd := MakeSelection(FSize, 0);
        SelectionChanged(False);
      end;
      ecClearAll:
      begin
        ExecuteCommand(ecSelectAll);
        ExecuteCommand(ecClearIndexSelection);
      end;
      ecClearIndexSelection:
      begin
        I := GetRealSelStart.Index;
        AddUndoBytes(crInsertString, PByteArray(@FBuffer[I]), L.Index, True);
        ClearString(I, L.Index);
        FSelEnd := MakeSelection(I, 0);
        SelectionChanged(True);
      end;
      ecClearSelection:
      begin
        Sel1 := GetRealSelStart;
        Sel2 := GetRealSelEnd;
        if Sel1.Digit > 0 then
        begin
          BeginUndoGroup(crDeleteDigits);
          try
            // digit clear mode
            AddUndoCaretPos;
            FSelEnd := MakeSelection(Sel1.Index + 1, 0);
            FSelStart := FSelEnd;
            if Sel2.Digit = 0 then
            begin
              Dec(L.Index);
              N := FBuffer[Sel2.Index - 1];
            end else
              N := FBuffer[Sel2.Index];
            AddUndoBytes(crInsertDigits, PByteArray(@FBuffer[FSelEnd.Index]), L.Index, True);
            ClearString(FSelEnd.Index, L.Index);
            FSelEnd := Sel1;
            AddUndoByte(crDeleteChar, FBuffer[Sel1.Index], False);
            for I := Sel1.Digit to cDigitCount - 1 do
            begin
              FBuffer[Sel1.Index] := ReplaceDigit(FBuffer[Sel1.Index], N mod cBase, I);
              N := N div cBase;
            end;
            SelectionChanged(True);
          finally
            EndUndoGroup;
          end;
        end else
          ExecuteCommand(ecClearIndexSelection);
      end;
      ecSearch, ecReplace:
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
      end;
      ecInsertMode:
      begin
        Exclude(FStates, elOverwrite);
        UpdateCaret(True);
      end;
      ecOverwriteMode:
      begin
        Include(FStates, elOverwrite);
        UpdateCaret(True);
      end;
      ecToggleMode:
      begin
        if elOverwrite in FStates then
          Exclude(FStates, elOverwrite)
        else
          Include(FStates, elOverwrite);
        UpdateCaret(True);
      end;
      // focus change
      ecGotFocus,
      ecLostFocus:
      begin
        UpdateCaret;
        Invalidate;
      end;
    end;
    if (OldSelStart.Index <> OldSelEnd.Index) or (FSelStart.Index <> FSelEnd.Index) or
      (OldSelStart.Digit <> OldSelEnd.Digit) or (FSelStart.Digit <> FSelEnd.Digit) or
      not (elCaretVisible in FStates) and (edInactiveCaret in FDrawStyles) and
      ((FSelStart.Index <> OldSelStart.Index) or (FSelStart.Digit <> OldSelStart.Digit) or
      (FSelEnd.Index <> OldSelEnd.Index) or (FSelEnd.Digit <> OldSelEnd.Digit)) then
      Invalidate;}
  end;
end;

procedure TKCustomMemo.FlagAssign(AFlag: Cardinal; Value: Boolean);
begin
  if Value then
    FlagSet(AFlag)
  else
    FlagClear(AFlag);
end;  

procedure TKCustomMemo.FlagClear(AFlag: Cardinal);
begin
  FFlags := FFlags and not AFlag;
end;

procedure TKCustomMemo.FlagSet(AFlag: Cardinal);
begin
  FFlags := FFlags or AFlag;
end;

procedure TKCustomMemo.FlagToggle(AFlag: Cardinal);
begin
  FFlags := FFlags xor AFlag;
end;

function TKCustomMemo.Flag(AFlag: Cardinal): Boolean;
begin
  Result := FFlags and AFlag <> 0;
end;

procedure TKCustomMemo.FontChange(Sender: TObject);
begin
  
end;

function TKCustomMemo.GetCaretVisible: Boolean;
begin
  Result := Flag(cEF_CaretVisible);
end;

function TKCustomMemo.GetCommandKey(Index: TKEditCommand): TKEditKey;
var
  I: Integer;
begin
  Result.Key := 0;
  Result.Shift := [];
  for I := 0 to Length(FKeyMapping) - 1 do
    if FKeyMapping[I].Command = Index then
    begin
      Result := FKeyMapping[I].Key;
      Exit;
    end;
end;

function TKCustomMemo.GetEmpty: Boolean;
begin
  Result := FParagraphs.Count = 0;
end;

function TKCustomMemo.GetInsertMode: Boolean;
begin
  Result := not Flag(cEF_Overwrite);
end;

function TKCustomMemo.GetLineCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FParagraphs.Count - 1 do
    Inc(Result, FParagraphs[I].LineCount);
end;

function TKCustomMemo.GetModified: Boolean;
begin
  Result := Flag(cEF_Modified) or FUndoList.Modified;
end;

function TKCustomMemo.GetMaxLeftPos: Integer;
begin
  Result := (FHorzExtent - ClientWidth) div FHorzScrollStep;
end;

function TKCustomMemo.GetMaxTopPos: Integer;
begin
  Result := (FVertExtent - ClientHeight) div FVertScrollStep;
end;

function TKCustomMemo.GetPageCount(var Data: TKEditPrintData): Integer;
var
  PS: TKEditPrintSettings;
begin
  if GetPrintSettings(Data, PS) then
    Result := PS.PageCount
  else
    Result := 0;
end;

function TKCustomMemo.GetPrintSettings(var Data: TKEditPrintData;
  var Settings: TKEditPrintSettings; Print: Boolean): Boolean;
var
  I, J: Integer;
  PrinterInstalled: Boolean;
  D, HeaderSpace, FooterSpace: Double;
  DC: HDC;
  M: TKPrintMargins;
  TM: TTextMetric;
begin
  Printer.Refresh;
  PrinterInstalled := Printer.Printers.Count > 0;
  Result := PrinterInstalled or not Print;
  if Result then
  begin
    if PrinterInstalled then
    begin
      Settings.PageWidth := Printer.PageWidth;
      Settings.PageHeight := Printer.PageHeight;
      Settings.PrnLogPelsX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
      Settings.PrnLogPelsY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
    end else
    begin
      // fake printer metrics if no printer installed
      Settings.PageWidth := 2360;
      Settings.PageHeight := 3400;
      Settings.PrnLogPelsX := 300;
      Settings.PrnLogPelsY := 300;
    end;
    // limit copies and Scale
    Data.Copies := MinMax(Data.Copies, cPrintCopiesMin, cPrintCopiesMax);
    Data.Scale := MinMax(Data.Scale, cPrintScaleMin, cPrintScaleMax);
    // get pixels per inch for display and printer
    DC := GetDC(Handle);
    try
      Settings.LogPelsX := GetDeviceCaps(DC, LOGPIXELSX);
      Settings.LogPelsY := GetDeviceCaps(DC, LOGPIXELSY);
    finally
      ReleaseDC(Handle, DC);
    end;
    // get horizontal and vertical ratio
    Settings.RatioX := Settings.PrnLogPelsX / Settings.LogPelsX;
    Settings.RatioY := Settings.PrnLogPelsY / Settings.LogPelsY;
    // limit and convert horizontal margins (1 margin unit = inch/100)
    D := Settings.PageWidth * 40 / Settings.PrnLogPelsX; // 40% of the page
    M.Left := MinMax(ValueToHOI(Data.MarginUnits, Data.Margins.Left), 0, D);
    M.Right := MinMax(ValueToHOI(Data.MarginUnits, Data.Margins.Right), 0, D);
    // limit and convert vertical margins (1 margin unit = inch/100)
    D := Settings.PageHeight * 40 / Settings.PrnLogPelsY; // 40% of the page
    HeaderSpace := 0; FooterSpace := 0;
    if Assigned(FOnPrintSpace) then
      FOnPrintSpace(Self, Data, HeaderSpace, FooterSpace);
    M.Top := MinMax(ValueToHOI(Data.MarginUnits, Data.Margins.Top + HeaderSpace), 0, D);
    M.Bottom := MinMax(ValueToHOI(Data.MarginUnits, Data.Margins.Bottom + FooterSpace), 0, D);
    // get margins in printer pixels
    Settings.PrintMargins := Rect(
      Round(M.Left * Settings.PrnLogPelsX / 100),
      Round(M.Top * Settings.PrnLogPelsY / 100),
      Round(M.Right * Settings.PrnLogPelsX / 100),
      Round(M.Bottom * Settings.PrnLogPelsY / 100));
    // get shape width in printer pixels
    Settings.TotalWidth := Round(FHorzExtent * Settings.RatioX);
    // get horizontal scaling
    if epoFitToPage in Data.Options then
      Settings.RatioScale := (Settings.PageWidth - Settings.PrintMargins.Left -
        Settings.PrintMargins.Right) / Settings.TotalWidth
    else
      Settings.RatioScale := Data.Scale / cPrintScaleDef;
    // get char width in printer pixels (significant rounding error here)
//    Settings.CharWidth := Round(I * Settings.RatioX * Settings.RatioScale);
    // get char height in printer pixels
//    Settings.CharHeight := Round((TM.tmHeight * FLineHeightPercent div 100) *
//      Settings.RatioY * Settings.RatioScale);
//    Settings.FontHeight := Round(Abs(Font.Height) *
//      Settings.RatioY * Settings.RatioScale);
{    if epoPageNumbers in Data.Options then
      I := Settings.FontHeight * 4 div 3
    else
      I := 0;}
    Settings.LastLine := LineCount;
//    Settings.PageLines := (Settings.PageHeight - Settings.PrintMargins.Top -
  //    Settings.PrintMargins.Bottom - I) div Settings.CharHeight;
    Settings.PageCount := DivUp(Settings.LastLine, Settings.PageLines);
    // limit starting and ending page
    Settings.PageOffset := 0;
    if Settings.PageCount > 0 then
    begin
      if (Data.Range = eprSelectedOnly) and (SelLength = 0) then
        Data.Range := eprAll;
      case Data.Range of
        eprAll:
        begin
          Data.StartPage := 1;
          Data.EndPage := Settings.PageCount;
        end;
        eprRange:
        begin
          Data.StartPage := MinMax(Data.StartPage, 1, Settings.PageCount);
          if Data.EndPage = 0 then
            Data.EndPage := Settings.PageCount
          else
            Data.EndPage := MinMax(Data.EndPage, Data.StartPage, Settings.PageCount);
        end;
        eprSelectedOnly:
        begin
          I := GetRealSelStart;
          J := GetRealSelEnd;
//          Settings.PageOffset := I div FLineSize mod Settings.PageLines;
//          Settings.LastLine := Min(Settings.LastLine, DivUp(J, FLineSize));
//          Data.StartPage := DivUp(I div FLineSize, Settings.PageLines);
//          Data.EndPage := DivUp(J div FLineSize - Settings.PageOffset, Settings.PageLines);
        end;
      end;
    end;
    Data.PreviewPage := MinMax(Data.PreviewPage, Data.StartPage, Data.EndPage);
  end;  
end;

function TKCustomMemo.GetReadOnly: Boolean;
begin
  Result := Flag(cEF_ReadOnly);
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

function TKCustomMemo.GetSelText: WideString;
begin
  Result := FParagraphs.RetrieveText(GetRealSelStart, SelLength);
end;

function TKCustomMemo.GetText: WideString;
begin
  Result := FParagraphs.RetrieveText(0, cAll);
end;

function TKCustomMemo.GetUndoLimit: Integer;
begin
  Result := FUndoList.Limit;
end;

function TKCustomMemo.IndexToPoint(Value: Integer): TPoint;
begin

end;

function TKCustomMemo.IndexValid(Value: Integer): Boolean;
begin

end;

procedure TKCustomMemo.InsertChar(At: Integer; Value: WideChar);
begin
  FParagraphs.InsertText(At, Value);
end;

procedure TKCustomMemo.InsertString(At: Integer; const Value: WideString);
begin
  FParagraphs.InsertText(At, Value);
end;

function TKCustomMemo.IsOptionsStored: Boolean;
begin
  Result := FOptions <> [eoGroupUndo];
end;

procedure TKCustomMemo.KeyDown(var Key: Word; Shift: TShiftState);
var
  I: Integer;
  HK: TKEditKey;
begin
  inherited;
  FlagClear(cEF_IgnoreNextChar);
  if not (csDesigning in ComponentState) then
  begin
    for I := 0 to Length(FKeyMapping) - 1 do
    begin
      HK := FKeyMapping[I].Key;
      if (HK.Key = Key) and (HK.Shift = Shift) then
      begin
        ExecuteCommand(FKeyMapping[I].Command);
        Key := 0;
        FlagSet(cEF_IgnoreNextChar);
        Exit;
      end;
    end;
    if Key = VK_ESCAPE then
      FlagSet(cEF_IgnoreNextChar);
  end;
end;

procedure TKCustomMemo.LockUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TKCustomMemo.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
  Command: TKEditCommand;
begin
  inherited;
  if Enabled and (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    SafeSetFocus;
    P := Point(X, Y);
    if ssShift in Shift then
      Command := ecSelGotoXY
    else
      Command := ecGotoXY;
    if ExecuteCommand(Command, @P) then
    ;
  end;
end;

procedure TKCustomMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Index: Integer;
begin
  inherited;
  if MouseCapture then
  begin
    P := Point(X, Y);
    Index := PointToIndex(P, True);
    ClampInView(Index);
  end;
end;

procedure TKCustomMemo.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
end;

procedure TKCustomMemo.Paint;
begin
end;

function TKCustomMemo.PointToIndex(P: TPoint; OutOfArea: Boolean): Integer;
begin

end;

procedure TKCustomMemo.PrintPage(const PrintData: TKEditPrintData;
  const PaintData: TKEditPaintData; const PrintSettings: TKEditPrintSettings;
  const Info: TKEditPageCopyInfo);
var
  S: string;
  TM: TTextMetric;
begin
{  with PaintData do
  begin
    TopLine := (Info.Page - 1) * PrintSettings.PageLines + PrintSettings.PageOffset;
    BottomLine := Min(TopLine + PrintSettings.PageLines, PrintSettings.LastLine) - 1;
    // adjust left and right print margins
    if (epoMirrorMargins in PrintData.Options) and (Info.Page mod 2 = 0) then
    begin
      PaintRect.Left := PrintSettings.PrintMargins.Right;
      PaintRect.Right := PrintSettings.PageWidth - PrintSettings.PrintMargins.Left;
    end else
    begin
      PaintRect.Left := PrintSettings.PrintMargins.Left;
      PaintRect.Right := PrintSettings.PageWidth - PrintSettings.PrintMargins.Right;
    end;
    // the bottom of paintrect should align to the last line
    PaintRect.Bottom := PaintRect.Top + (BottomLine - TopLine + 1) * CharHeight;
    // allow custom painting
    DoPrintPaint(Info, PaintData, PrintSettings);
    // new font assignment
    Canvas.Font := Font;
    Canvas.Font.Height := PrintSettings.FontHeight;
    PaintLines(PaintData);
    if epoPageNumbers in PrintData.Options then
    begin
      with Canvas do
      begin
        Brush.Style := bsClear;
        Font.Style := [fsBold];
        Font.Color := clBlack;
        SetTextCharacterExtra(Handle, 0);
        S := Format('- %d -', [Info.Page]);
        TextOut(PaintRect.Left + (PaintRect.Right - PaintRect.Left - TextWidth(S)) div 2,
          PrintSettings.PageHeight - PrintSettings.FontHeight - PrintSettings.PrintMargins.Bottom, S);
      end;
    end;
  end;}
end;

procedure TKCustomMemo.SafeSetFocus;
begin
  if not Focused and CanFocus and not (csDesigning in ComponentState) then SetFocus;
end;

procedure TKCustomMemo.Scroll(CodeHorz, CodeVert, DeltaHorz,
  DeltaVert: Integer; CallScrollWindow: Boolean);

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
      SI.fMask := SIF_PAGE or SIF_TRACKPOS;
      GetScrollInfo(Handle, Code, SI);
    end;
    Pos := ScrollPos;
    OldPos := Pos;
    if Delta <> 0 then
      Inc(Pos, Delta)
    else if HasScrollBar then
    case ScrollCode of
      SB_TOP: Pos := 0;
      SB_BOTTOM: Pos := MaxScrollPos;
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
begin
  OldLeftPos := FLeftPos;
  OldTopPos := FTopPos;
  if Axis(SB_HORZ, FScrollBars in [ssHorizontal, ssBoth],
    CodeHorz, DeltaHorz, FHorzExtent, FLeftPos) or
    Axis(SB_VERT, FScrollBars in [ssVertical, ssBoth],
    CodeVert, DeltaVert, FVertExtent, FTopPos) then
  begin
    if CallScrollWindow then
      ScrollWindowEx(Handle, OldLeftPos - FLeftPos, OldTopPos - FTopPos,
        nil, nil, 0, nil, SW_INVALIDATE)
    else
      Invalidate;  
    UpdateCaret;
  end;
end;

procedure TKCustomMemo.ScrollBy(DeltaHorz, DeltaVert: Integer);
begin
  Scroll(cNoScrollCode, cNoScrollCode, DeltaHorz, DeltaVert, True);
end;

function TKCustomMemo.ScrollNeeded(Index: Integer;
  out DeltaCol, DeltaRow: Integer): Boolean;
begin
  Result := False;
end;

procedure TKCustomMemo.ScrollTimerHandler(Sender: TObject);
var
  Index, DeltaHorz, DeltaVert: Integer;
begin
  if MouseCapture and not Dragging then
  begin
    Index := PointToIndex(ScreenToClient(Mouse.CursorPos), True);
    if ScrollNeeded(Index, DeltaHorz, DeltaVert) then
      Scroll(cNoScrollCode, cNoScrollCode, DeltaHorz, DeltaVert, False)
    else
      FScrollTimer.Enabled := False;
  end else
    FScrollTimer.Enabled := False;
end;

function TKCustomMemo.SelAvail: Boolean;
begin
  Result := SelLength > 0;
end;

function TKCustomMemo.SelectionMove(NewSelStart, NewSelEnd: Integer): Boolean;
begin

end;

procedure TKCustomMemo.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TKCustomMemo.SetColors(Value: TKEditColors);
begin
  FColors.Assign(Value);
end;

procedure TKCustomMemo.SetCommandKey(Index: TKEditCommand; Value: TKEditKey);
var
  I: Integer;
begin
  for I := 0 to Length(FKeyMapping) - 1 do
    if FKeyMapping[I].Command = Index then
    begin
      FKeyMapping[I].Key := Value;
      Exit;
    end;
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

procedure TKCustomMemo.SetKeyMapping(Value: TKEditKeyMapping);
begin
  FKeyMapping := Copy(Value);
end;

procedure TKCustomMemo.SetLeftPos(Value: Integer);
begin
  Value := MinMax(Value, 0, FHorzExtent - 1);
  if Value <> FLeftPos then
    ScrollBy(Value - FLeftPos, 0);
end;

procedure TKCustomMemo.SetLines(Value: TWideStrings);
begin
  FLines.Assign(Value);
end;

procedure TKCustomMemo.SetModified(Value: Boolean);
begin
  if Value <> GetModified then
  begin
    FlagAssign(cEF_Modified, Value);
    if not Value then
    begin
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

procedure TKCustomMemo.SetParagraphs(Value: TKMemoParagraphs);
begin

end;

procedure TKCustomMemo.SetReadOnly(Value: Boolean);
begin
  if Value <> GetReadOnly then
    FlagAssign(cEF_ReadOnly, Value);
end;

procedure TKCustomMemo.SetScrollBars(Value: TScrollStyle);
begin
  if Value <> FScrollBars then
  begin
    FScrollBars := Value;
    RecreateWnd;
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
    SelectionMove(FSelStart, Value);
end;

procedure TKCustomMemo.SetSelLength(Value: Integer);
begin
  if Value <> GetSelLength then
    SelectionMove(FSelStart, FSelStart + Value);
end;

procedure TKCustomMemo.SetSelStart(Value: Integer);
begin
  if Value <> FSelStart then
    SelectionMove(Value, FSelEnd);
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

procedure TKCustomMemo.TextChanged;
begin
  SelectionMove(0, 0);
  FUndoList.Clear;
  FRedoList.Clear;
  UpdateScrollRange(True);
  DoChange;
end;

procedure TKCustomMemo.UndoChange(Sender: TObject; ItemReason: TKEditChangeReason);
begin
  if (Sender = FUndoList) and (ItemReason <> crCaretPos) then
    DoChange;
end;

procedure TKCustomMemo.UnlockUpdate;
begin
  if FUpdateLock > 0 then
  begin
    Dec(FUpdateLock);
    UpdateScrollRange(True);
  end;
end;

procedure TKCustomMemo.UpdateCaret(Recreate: Boolean = False);
var
  CW, CH: Integer;
  Size: TSize;
  Info: TKMemoParagraphInfo;
  R: TRect;
begin
  FlagSet(cEF_CaretUpdate);
  try
    if Enabled and Focused and not (csDesigning in ComponentState) then
    begin
      R := FParagraphs.BlockRect(FSelEnd, Canvas);
      if Windows.CreateCaret(Handle, 0, R.Right - R.Left, R.Bottom - R.Top) then
        FlagSet(cEF_CaretVisible);
      if Flag(cEF_CaretVisible) then
      begin
        Windows.SetCaretPos(R.Left, R.Top);
        Windows.ShowCaret(Handle);
      end;
    end
    else if Flag(cEF_CaretVisible) then
    begin
      if Windows.HideCaret(Handle) then
        Windows.DestroyCaret;
      FlagClear(cEF_CaretVisible);
    end;
  finally
    FlagClear(cEF_CaretUpdate);
  end;
end;

procedure TKCustomMemo.UpdateScrollRange(CallInvalidate: Boolean);
var
  Extent: TSize;
  DeltaHorz, DeltaVert, ClientHorz, ClientVert: Integer;
  SI: TScrollInfo;
begin
  if HandleAllocated then
  begin
    Extent := FParagraphs.Extent;
    ClientHorz := ClientWidth;
    ClientVert := ClientHeight;
    DeltaHorz := (ClientHorz + FLeftPos - Extent.cx) div cHorzScrollStepDef;
    DeltaVert := (ClientVert + FTopPos - Extent.cy) div cVertScrollStepDef;
    if CallInvalidate then
      Invalidate
    else
      ScrollBy(DeltaHorz, DeltaVert);
    if FScrollBars in [ssBoth, ssHorizontal, ssVertical] then
    begin
      SI.cbSize := SizeOf(TScrollInfo);
      SI.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
      SI.nMin := 0;
      if FScrollBars in [ssBoth, ssHorizontal] then
      begin
        SI.nMax := DivUp(Extent.cx, cHorzScrollStepDef);
        SI.nPage := ClientHorz div cHorzScrollStepDef;
        SI.nPos := FLeftPos;
        SetScrollInfo(Handle, SB_HORZ, SI, True);
      end;
      if FScrollBars in [ssBoth, ssVertical] then
      begin
        SI.nMax := DivUp(Extent.cy, cHorzScrollStepDef);
        SI.nPage := ClientVert div cHorzScrollStepDef;
        SI.nPos := FTopPos;
        SetScrollInfo(Handle, SB_VERT, SI, True);
      end;
    end;
  end;
end;

function TKCustomMemo.UpdateUnlocked: Boolean;
begin
  Result := FUpdateLock = 0;
end;

procedure TKCustomMemo.ValidateIndex(var Value: Integer);
begin

end;

procedure TKCustomMemo.WMChar(var Msg: TWMChar);
var
  C: WideChar;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if not Flag(cEF_IgnoreNextChar) then
    begin
      C := WideChar(Msg.CharCode);
      ExecuteCommand(ecInsertChar, @C);
    end else
      FlagClear(cEF_IgnoreNextChar);
  end;
end;

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

procedure TKCustomMemo.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TKCustomMemo.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TKCustomMemo.WMHScroll(var Msg: TWMHScroll);
begin
  SafeSetFocus;
  Scroll(Msg.ScrollCode, cNoScrollCode, 0, 0, True);
end;

procedure TKCustomMemo.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  ExecuteCommand(ecLostFocus);
end;

procedure TKCustomMemo.WMNCPaint(var Msg: TWMNCPaint);
{$IFDEF USE_THEMES}
var
  R: TRect;
  ExStyle: Integer;
  TempRgn: HRGN;
  BorderWidth,
  BorderHeight: Integer;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  with ThemeServices do if ThemesEnabled then
  begin
    // If theming is enabled and the client edge border is set for the window then prevent the default window proc
    // from painting the old border to avoid flickering.
    ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if (ExStyle and WS_EX_CLIENTEDGE) <> 0 then
    begin
      GetWindowRect(Handle, R);
      // Determine width of the client edge.
      BorderWidth := GetSystemMetrics(SM_CXEDGE);
      BorderHeight := GetSystemMetrics(SM_CYEDGE);
      InflateRect(R, -BorderWidth, -BorderHeight);
      TempRgn := CreateRectRgnIndirect(R);
      // Exclude the border from the message region if there is one. Otherwise just use the inflated
      // window area region.
      if Msg.Rgn <> 1 then
        CombineRgn(TempRgn, Msg.Rgn, TempRgn, RGN_AND);
      DefWindowProc(Handle, Msg.Msg, Integer(TempRgn), 0);
      DeleteObject(TempRgn);
      PaintBorder(Self, True);
    end else
      inherited;
  end else
{$ENDIF}
    inherited;
end;

procedure TKCustomMemo.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  ExecuteCommand(ecGotFocus);
end;

procedure TKCustomMemo.WMSize(var Msg: TWMSize);
begin
  if (Msg.Width <> Width) or (Msg.Height <> Height) then
  begin
    inherited;
    UpdateScrollRange(False);
  end;  
end;

{$IFDEF USE_THEMES}
procedure TKCustomMemo.WMThemeChanged(var Message: TMessage);
begin
  inherited;
  ThemeServices.UpdateThemes;
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_VALIDATE or RDW_FRAME);
end;
{$ENDIF}

procedure TKCustomMemo.WMVScroll(var Msg: TWMVScroll);
begin
  SafeSetFocus;
  Scroll(cNoScrollCode, Msg.ScrollCode, 0, 0, True);
end;

procedure CreateDefaultKeyMapping;

  procedure AddKey(Command: TKEditCommand; Key: Word; Shift: TShiftState);
  var
    I: Integer;
  begin
    I := Length(DefaultKeyMapping);
    SetLength(DefaultKeyMapping, I + 1);
    DefaultKeyMapping[I].Command := Command;
    DefaultKeyMapping[I].Key.Key := Key;
    DefaultKeyMapping[I].Key.Shift := Shift;
  end;

begin
  AddKey(ecLeft, VK_LEFT, []);
  AddKey(ecRight, VK_RIGHT, []);
  AddKey(ecRight, VK_RETURN, []);
  AddKey(ecUp, VK_UP, []);
  AddKey(ecDown, VK_DOWN, []);
  AddKey(ecLineStart, VK_HOME, []);
  AddKey(ecLineEnd, VK_END, []);
  AddKey(ecPageUp, VK_PRIOR, []);
  AddKey(ecPageDown, VK_NEXT, []);
  AddKey(ecPageLeft, VK_LEFT, [ssCtrl, ssAlt]);
  AddKey(ecPageRight, VK_RIGHT, [ssCtrl, ssAlt]);
  AddKey(ecPageTop, VK_PRIOR, [ssCtrl]);
  AddKey(ecPageBottom, VK_NEXT, [ssCtrl]);
  AddKey(ecEditorTop, VK_HOME, [ssCtrl]);
  AddKey(ecEditorBottom, VK_END, [ssCtrl]);
  AddKey(ecSelLeft, VK_LEFT, [ssShift]);
  AddKey(ecSelRight, VK_RIGHT, [ssShift]);
  AddKey(ecSelUp, VK_UP, [ssShift]);
  AddKey(ecSelDown, VK_DOWN, [ssShift]);
  AddKey(ecSelLineStart, VK_HOME, [ssShift]);
  AddKey(ecSelLineEnd, VK_END, [ssShift]);
  AddKey(ecSelPageUp, VK_PRIOR, [ssShift]);
  AddKey(ecSelPageDown, VK_NEXT, [ssShift]);
  AddKey(ecSelPageLeft, VK_LEFT, [ssShift, ssCtrl, ssAlt]);
  AddKey(ecSelPageRight, VK_RIGHT, [ssShift, ssCtrl, ssAlt]);
  AddKey(ecSelPageTop, VK_PRIOR, [ssShift, ssCtrl]);
  AddKey(ecSelPageBottom, VK_NEXT, [ssShift, ssCtrl]);
  AddKey(ecSelEditorTop, VK_HOME, [ssShift, ssCtrl]);
  AddKey(ecSelEditorBottom, VK_END, [ssShift, ssCtrl]);
  AddKey(ecScrollUp, VK_UP, [ssCtrl]);
  AddKey(ecScrollDown, VK_DOWN, [ssCtrl]);
  AddKey(ecScrollLeft, VK_LEFT, [ssCtrl]);
  AddKey(ecScrollRight, VK_RIGHT, [ssCtrl]);
  AddKey(ecScrollCenter, VK_RETURN, [ssCtrl]);
  AddKey(ecUndo, ord('Z'), [ssCtrl]);
  AddKey(ecUndo, VK_BACK, [ssAlt]);
  AddKey(ecRedo, ord('Z'), [ssShift, ssCtrl]);
  AddKey(ecRedo, VK_BACK, [ssShift, ssAlt]);
  AddKey(ecCopy, ord('C'), [ssCtrl]);
  AddKey(ecCopy, VK_INSERT, [ssCtrl]);
  AddKey(ecCut, ord('X'), [ssCtrl]);
  AddKey(ecCut, VK_DELETE, [ssShift]);
  AddKey(ecPaste, ord('V'), [ssCtrl]);
  AddKey(ecPaste, VK_INSERT, [ssShift]);
  AddKey(ecDeleteLastByte, VK_BACK, []);
  AddKey(ecDeleteLastByte, VK_BACK, [ssShift]);
  AddKey(ecDeleteByte, VK_DELETE, []);
  AddKey(ecDeleteEOL, ord('Y'), [ssCtrl,ssShift]);
  AddKey(ecDeleteLine, ord('Y'), [ssCtrl]);
  AddKey(ecSelectAll, ord('A'), [ssCtrl]);
  AddKey(ecToggleMode, VK_INSERT, []);
end;

procedure DestroyDefaultKeyMapping;
begin
  Finalize(DefaultKeymapping);
end;

{ TKMemoParagraph }

constructor TKMemoParagraph.Create(AMemo: TKCustomMemo);
begin
  FMemo := AMemo;
  FExtent.cx := 0;
  FExtent.cy := 0;
end;

procedure TKMemoParagraph.DeleteBlock(BlockIndex, BlockCount: Integer);
begin
  // empty
end;

function TKMemoParagraph.GetBlockCount: Integer;
begin
  Result := Length(FBlocks);
end;

function TKMemoParagraph.BlockRect(BlockIndex: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TKMemoParagraph.GetIndent: Integer;
begin
  Result := 0;
end;

function TKMemoParagraph.GetLineCount: Integer;
begin
  Result := 0;
end;

function TKMemoParagraph.IndexToLine(BlockIndex: Integer): Integer;
begin
  Result := 0;
end;

procedure TKMemoParagraph.InsertBlock(BlockIndex: Integer; Value: TKMemoBlock);
begin

end;

procedure TKMemoParagraph.InsertText(BlockIndex: Integer; const Value: WideString);
begin
  // empty
end;

function TKMemoParagraph.LineAt(BlockIndex: Integer): WideString;
begin

end;

procedure TKMemoParagraph.Paint(const Origin: TPoint);
begin

end;

procedure TKMemoParagraph.PaintTo(ACanvas: TCanvas; const Origin: TPoint);
begin
  // empty
end;

procedure TKMemoParagraph.SetIndent(const Value: Integer);
begin
  // empty
end;

procedure TKMemoParagraph.SetText(const Value: WideString);
begin
//  if Value// empty
end;

procedure TKMemoParagraph.Update;
var
  ArrayIndex, ArrayLen, I, ClipWidth, LastIndex, LineWidth, LineHeight, PrevPos: Integer;
  Size: TSize;
  DC: HDC;
begin
{  FLineCount := 0;
  DC := Canvas.Handle;
  ClipWidth := FMemo.ClientWidth;
  LineWidth := 0;
  LineHeight := 0;
  LastIndex := Length(FText) - 1;
  PrevPos := 0;
  FWidth := 0;
  FHeight := 0;
  for I := 0 to LastIndex do
  begin
    if(FText[I] = WideSpace) or (FText[I] = WideTabulator) or (I = LastIndex) then
    begin
      Windows.GetTextExtentPoint32W(DC, PWideChar(FText[PrevPos]), I - PrevPos, Size);
      if ((I = LastIndex) or (LineWidth + Size.cx > ClipWidth)) and (LineWidth > 0) then
      begin
        ArrayLen := Length(FLineBegins);
        if FLineCount >= ArrayLen then
          SetLength(FLineBegins, ArrayLen + 1);
        FLineBegins[FLineCount] := PrevPos;
        Inc(FLineCount);
        Inc(FHeight, LineHeight);
        LineHeight := Size.cy;
        if LineWidth > FWidth then
          FWidth := LineWidth;
        LineWidth := Size.cx;
      end else
        Inc(LineWidth, Size.cx);
      PrevPos := I;
    end;
  end;
  if FLineCount > 0 then
    Dec(FLineCount);}
end;

{ TKMemoParagraphs }

function TKMemoParagraphs.BlockRect(Index: Integer; ACanvas: TCanvas): TRect;
begin

end;

procedure TKMemoParagraphs.DeleteBlocks(Index, Count: Integer);
var
  I, Len: Integer;
  PI: TKMemoParagraphInfo;
  P: TKMemoParagraph;
begin
  if (Index >= 0) and (Count > 0) then
  begin
{    PI := IndexToParagraphInfo(At);
    LockUpdate;
    try
      I := PI.BlockIndex;
      while (Size > 0) and (PI.ParaIndex < FParagraphs.Count) do
      begin
        P := FParagraphs[PI.ParaIndex];
        Len := Min(Size, Length(P.Text) - I);
        P.Delete(I, Len);
        Dec(Size, Len);
        if I > 0 then
        begin
          I := 0;
          Inc(PI.ParaIndex);
        end;
      end;
    finally
      UnlockUpdate;
    end;}
  end;
end;

function TKMemoParagraphs.GetExtent: TSize;
begin

end;

function TKMemoParagraphs.GetParagraph(Index: Integer): TKMemoParagraph;
begin
  Result := TKMemoParagraph(Items[Index]);
end;

function TKMemoParagraphs.IndexToParagraphInfo(
  Index: Integer): TKMemoParagraphInfo;
begin

end;

procedure TKMemoParagraphs.InsertObject(Index: Integer;
  const Value: WideString);
begin

end;

procedure TKMemoParagraphs.InsertText(Index: Integer; const Value: WideString);
begin

end;

function TKMemoParagraphs.LineToParagraphInfo(
  LineIndex: Integer): TKMemoParagraphInfo;
begin

end;

function TKMemoParagraphs.ParagraphInfoToIndex(
  const Info: TKMemoParagraphInfo): Integer;
begin

end;

function TKMemoParagraphs.ParagraphInfoToLine(
  const Info: TKMemoParagraphInfo): Integer;
begin

end;

procedure TKMemoParagraphs.ReplaceText(Index, Count: Integer; const Value: WideString);
begin

end;

function TKMemoParagraphs.RetrieveText(Index, ALength: Integer): WideString;
begin

end;

procedure TKMemoParagraphs.SetParagraph(Index: Integer;
  const Value: TKMemoParagraph);
begin
  Items[Index] := TObject(Value);
end;

{ TKMemoTextParagraph }

{constructor TKMemoTextParagraph.Create(AMemo: TKCustomMemo);
begin
  inherited;
  FHeight := 0;
  SetLength(FLineBegins, 1);
  FLineBegins[0] := 0;
  FLineCount := 0;
  FWidth := 0;
  FText := '';
end;

procedure TKMemoTextParagraph.Delete(Index, Size: Integer);
begin
  if (Size > 0) and (Index < Length(FText)) then
  begin
    System.Delete(FText, Index, Size);
    Update;
  end;
end;

function TKMemoTextParagraph.GetBlockCount: Integer;
begin
  Result := Length(FText);
end;

function TKMemoTextParagraph.BlockExtent(Index: Integer): TSize;
begin
  if Index < Length(FText) then
    Windows.GetTextExtentPoint32W(Canvas.Handle, PWideChar(FText[Index]), 1, Result)
  else
    Result := inherited BlockExtent(Index);
end;

function TKMemoTextParagraph.Extent: TSize;
begin
  Result.cx := FWidth;
  Result.cy := FHeight;
end;

function TKMemoTextParagraph.GetLineCount: Integer;
begin
  Result := FLineCount + 1;
end;

function TKMemoTextParagraph.GetLines(Index: Integer): WideString;
var
  LineCharCount: Integer;
begin
  if Index < FLineCount then
  begin
    if Index = FLineCount - 1 then
      LineCharCount := Length(FText) - FLineBegins[Index]
    else
      LineCharCount := FLineBegins[Index + 1] - FLineBegins[Index];
    Result := Copy(FText, FLineBegins[FLineCount], LineCharCount);
  end;
end;

function TKMemoTextParagraph.GetText: WideString;
begin
  Result := FText;
end;

function TKMemoTextParagraph.IndexToLine(Index: Integer): Integer;
begin

end;

procedure TKMemoTextParagraph.InsertText(Index: Integer;
  const Value: WideString);
begin
  inherited;

end;

procedure TKMemoTextParagraph.PaintTo(const Origin: TPoint);
var
  I: Integer;
  R: TRect;
  Size: TSize;
begin
  Size := Extent;
  R := Rect(Origin.X, Origin.Y, Origin.X + Size.cx, Origin.Y + Size.cy);
  Canvas.FillRect(R);
  Windows.ExtTextOutW(Canvas.Handle, Origin.X, Origin.Y, ETO_CLIPPED,
    @R, PWideChar(FText), Length(FText), nil);
end;

procedure TKMemoTextParagraph.SetText(const Value: WideString);
begin
  if Value <> FText then
  begin
    FText := Value;
    Update;
  end;
end;

procedure TKMemoTextParagraph.Update;
begin
end;}

{ TKMemoBlock }

constructor TKMemoBlock.Create;
begin

end;

procedure TKMemoBlock.SetBlockIndex(Value: Integer);
begin

end;

initialization
  CreateDefaultKeyMapping;
finalization
  DestroyDefaultKeyMapping;
end.

{
var
  I, Width: Integer;
  Extent: TSize;
  P: TKMemoParagraph;
begin
  Result.cx := 0;
  Result.cy := 0;
  for I := 0 to Count - 1 do
  begin
    P := TKMemoParagraph(Items[I]);
    Extent := P.Extent;
    Width := P.Indent + Extent.cx;
    if Width > Result.cx then Result.cx := Width;
    Inc(Result.cy, Extent.cy);
  end;


var
  I, CharCount, Len, LineCount: Integer;
  P: TKMemoParagraph;
begin
  Result.ParaIndex := 0;
  Result.LineIndex := 0;
  Result.BlockIndex := 0;
  CharCount := 0;
  LineCount := 0;
  for I := 0 to Paragraphs.Count - 1 do
  begin
    P := Paragraphs[I];
    Len := P.BlockCount;
    if Value < CharCount + Len then
    begin
      Result.ParaIndex := I;
      Result.BlockIndex := Value - CharCount;
      Result.LineIndex := LineCount + P.IndexToLine(Result.BlockIndex);
      Exit;
    end else
    begin
      Inc(LineCount, P.LineCount);
      Inc(CharCount, Len);
    end;
  end;}

