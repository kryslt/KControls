{ @abstract(This unit contains the common declarations for all edit controls.)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(18 Sep 2009)
  @lastmod(6 Jul 2014)

  This unit defines common types and functions for all edit controls. <BR><BR>

  Copyright © Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }

unit KEditCommon;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms;

const
  cCharMappingSize = 256;

type
  { Declares possible values for the edit control commands. }
  TKEditCommand = (
    { No command }
    ecNone,
    { Move caret left one char }
    ecLeft,
    { Move caret right one char }
    ecRight,
    { Move caret up one line }
    ecUp,
    { Move caret down one line }
    ecDown,
    { Move caret to beginning of line }
    ecLineStart,
    { Move caret to end of line }
    ecLineEnd,
    { Move caret up one page }
    ecPageUp,
    { Move caret down one page }
    ecPageDown,
    { Move caret left one page }
    ecPageLeft,
    { Move caret right one page }
    ecPageRight,
    { Move caret to top of page }
    ecPageTop,
    { Move caret to bottom of page }
    ecPageBottom,
    { Move caret to absolute beginning }
    ecEditorTop,
    { Move caret to absolute end }
    ecEditorBottom,
    { Move caret to specific coordinates, Data = ^TPoint }
    ecGotoXY,
    { Move caret left one char }
    ecSelLeft,
    { Move caret right one char, affecting selection }
    ecSelRight,
    { Move caret up one line, affecting selection }
    ecSelUp,
    { Move caret down one line, affecting selection }
    ecSelDown,
    { Move caret to beginning of line, affecting selection }
    ecSelLineStart,
    { Move caret to end of line, affecting selection }
    ecSelLineEnd,
    { Move caret up one page, affecting selection }
    ecSelPageUp,
    { Move caret down one page, affecting selection }
    ecSelPageDown,
    { Move caret left one page, affecting selection }
    ecSelPageLeft,
    { Move caret right one page, affecting selection }
    ecSelPageRight,
    { Move caret to top of page, affecting selection }
    ecSelPageTop,
    { Move caret to bottom of page, affecting selection }
    ecSelPageBottom,
    { Move caret to absolute beginning, affecting selection }
    ecSelEditorTop,
    { Move caret to absolute end, affecting selection }
    ecSelEditorBottom,
    { Move caret to specific coordinates, affecting selection, Data = ^TPoint }
    ecSelGotoXY,
    { Scroll up one line leaving caret position unchanged }
    ecScrollUp,
    { Scroll down one line leaving caret position unchanged }
    ecScrollDown,
    { Scroll left one char leaving caret position unchanged }
    ecScrollLeft,
    { Scroll right one char leaving caret position unchanged }
    ecScrollRight,
    { Scroll to center the caret position within client area }
    ecScrollCenter,
    { Undo previous action }
    ecUndo,
    { Redo last undone action }
    ecRedo,
    { Copy selection to clipboard }
    ecCopy,
    { Cut selection to clipboard }
    ecCut,
    { Paste clipboard to current position }
    ecPaste,
    { Insert character at current position, Data = ^Char }
    ecInsertChar,
    { Insert digits (digit string) at current position, Data = ^string
      (must contain digits only), TKCustomHexEditor only }
    ecInsertDigits,
    { Insert string (multiple characters) at current position, Data = ^string }
    ecInsertString,
    { Insert new line }
    ecInsertNewLine,
    { Delete last character (i.e. backspace key) }
    ecDeleteLastChar,
    { Delete character at caret (i.e. delete key) }
    ecDeleteChar,
    { Delete from caret to beginning of line }
    ecDeleteBOL,
    { Delete from caret to end of line }
    ecDeleteEOL,
    { Delete current line }
    ecDeleteLine,
    { Select everything }
    ecSelectAll,
    { Delete everything }
    ecClearAll,
    { Delete selection (no digit selection), TKCustomHexEditor only }
    ecClearIndexSelection,
    { Delete selection (digit selection as well) }
    ecClearSelection,
    { Search for text/digits }
    ecSearch,
    { Replace text/digits }
    ecReplace,
    { Set insert mode }
    ecInsertMode,
    { Set overwrite mode }
    ecOverwriteMode,
    { Toggle insert/overwrite mode }
    ecToggleMode,
    { Adjust editor when getting input focus }
    ecGotFocus,
    { Adjust editor when losing input focus }
    ecLostFocus
  );

  { Declares possible values for control's DisabledDrawStyle property. }
  TKEditDisabledDrawStyle = (
    { The lines will be painted with brighter colors when editor is disabled. }
    eddBright,
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
    Key: TKEditKey;
    Command: TKEditCommand;
  end;

  TKEditCommandMap = array of TKEditCommandAssignment;

  { @abstract(Declares OnDropFiles event handler)
    <UL>
    <LH>Parameters:</LH>
    <LI><I>Sender</I> - identifies the event caller</LI>
    <LI><I>X, Y</I> - mouse cursor coordinates (relative to the caller's window)</LI>
    <LI><I>Files</I> - list of file names that were dropped on the caller's window)</LI>
    </UL>
  }
  TKEditDropFilesEvent = procedure(Sender: TObject; X, Y: integer;
    Files: TStrings) of object;

  { Declares key mapping class for the KeyMapping property }
  TKEditKeyMapping = class(TObject)
  private
    function GetAssignment(AIndex: Integer): TKEditCommandAssignment;
    function GetKey(AIndex: TKEditCommand): TKEditKey;
    procedure SetKey(AIndex: TKEditCommand; const AValue: TKEditKey);
  protected
    FMap: TKEditCommandMap;
    procedure CreateMap; virtual;
  public
    constructor Create;
    procedure Assign(Source: TKEditKeyMapping); virtual;
    procedure AddKey(Command: TKEditCommand; Key: Word; Shift: TShiftState);
    class function EmptyMap: TKEditCommandAssignment;
    function FindCommand(AKey: Word; AShift: TShiftState): TKEditCommand;
    property Assignment[Index: Integer]: TKEditCommandAssignment read GetAssignment;
    property Key[AIndex: TKEditCommand]: TKEditKey read GetKey write SetKey;
    property Map: TKEditCommandMap read FMap;
  end;

  { Declares character mapping array for the edit control's CharMapping property }
  TKEditCharMapping = array of AnsiChar;

  { Pointer to @link(TKEditCharMapping) }
  PKEditCharMapping = ^TKEditCharMapping;

  { Declares options - possible values for the edit control's Options property }
  TKEditOption = (
    { The editor will receive dropped files }
    eoDropFiles,
    { All undo/redo operations of the same kind will be grouped together }
    eoGroupUndo,
    { The editor allows undo/redo operations after the edit control's Modified property
      has been set to False }
    eoUndoAfterSave,
    { TKMemo only: show formatting markers. }
    eoShowFormatting,
    { TKMemo only: acquire TAB characters. }
    eoWantTab
  );

  { Options can be arbitrary combined }
  TKEditOptions = set of TKEditOption;

  { Declares possible values for the Action parameter in the @link(TKEditReplaceTextEvent) event }
  TKEditReplaceAction = (
    { Quit replace sequence }
    eraCancel,
    { Replace this occurence }
    eraYes,
    { Don't replace this occurence }
    eraNo,
    { Replace all following occurences without prompting }
    eraAll
  );

  { @abstract(Declares OnReplaceText event handler)
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

  { Declares possible values for the ErrorReason member of the @link(TKEditSearchData) structure }
  TKEditSearchError = (
    { No error occured }
    eseOk,
    { There is a character in the search string that cannot be interpreted as hexadecimal digits}
    eseNoDigitsFind,
    { There is a character in the replace string that cannot be interpreted as hexadecimal digits}
    eseNoDigitsReplace,
    { No other search string found }
    eseNoMatch
  );

  { Declares search options - possible values for the Options member of the @link(TKEditSearchData) structure }
  TKEditSearchOption = (
    { Replace all occurences }
    esoAll,
    { Search backwards }
    esoBackwards,
    { Search entire scope instead from current caret position }
    esoEntireScope,
    { Include to identify search - this element will be automatically cleared
      to provide the @link(TKEditSearchData) structure for additional search }
    esoFirstSearch,
    { Match case when a binary search should be executed }
    esoMatchCase,
    { Prompt user before a string is about to be replaced. This assumes @link(OnReplaceText)
      is assigned }
    esoPrompt,
    { Search the current selection only }
    esoSelectedOnly,
    { Treat the supplied search and/or replace strings as hexadecimal sequence.
      When the search string contains a character that cannot be interpreted as
      hexadecimal digit, the execution stops and @link(eseNoDigitsFind) error will
      be returned. Similarly, @link(eseNoDigitsReplace) errors will be returned
      on invalid replace string }
    esoTreatAsDigits,
    { Internal option - don't modify }
    esoWereDigits
  );

  { Search options can be arbitrary combined }
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

  { Pointer to @link(TKEditSearchData) }
  PKEditSearchData = ^TKEditSearchData;


const
  { Default value for the @link(TKCustomHexEditor.DisabledDrawStyle) property }
  cEditDisabledDrawStyleDef = eddBright;

{ Returns default key mapping structure }
function CreateDefaultKeyMapping: TKEditKeyMapping;

{ Returns default char mapping structure }
function DefaultCharMapping: TKEditCharMapping;

{ Returns default search data structure }
function DefaultSearchData: TKEditSearchData;

implementation

function CreateDefaultKeyMapping: TKEditKeyMapping;
begin
  Result := TKEditKeyMapping.Create;
end;

function DefaultCharMapping: TKEditCharMapping;
var
  I: Integer;
begin
  SetLength(Result, cCharMappingSize);
  for I := 0 to cCharMappingSize - 1 do
    if (I < $20) or (I >= $80) then
      Result[I] := '.'
    else
      Result[I] := AnsiChar(I);
end;

function DefaultSearchData: TKEditSearchData;
begin
  with Result do
  begin
    ErrorReason := eseOk;
    Options := [esoAll, esoFirstSearch, esoPrompt, esoTreatAsDigits];
    SelStart := 0;
    SelEnd := 0;
    TextToFind := '';
    TextToReplace := '';
  end;
end;

{ TKEditKeyMapping }

constructor TKEditKeyMapping.Create;
begin
  FMap := nil;
  CreateMap;
end;

procedure TKEditKeyMapping.Assign(Source: TKEditKeyMapping);
begin
  FMap := Copy(Source.Map);
end;

procedure TKEditKeyMapping.CreateMap;
begin
  AddKey(ecLeft, VK_LEFT, []);
  AddKey(ecRight, VK_RIGHT, []);
  AddKey(ecInsertNewLine, VK_RETURN, []);
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
  AddKey(ecDeleteLastChar, VK_BACK, []);
  AddKey(ecDeleteLastChar, VK_BACK, [ssShift]);
  AddKey(ecDeleteChar, VK_DELETE, []);
  AddKey(ecDeleteBOL, ord('X'), [ssCtrl,ssShift]);
  AddKey(ecDeleteEOL, ord('Y'), [ssCtrl,ssShift]);
  AddKey(ecDeleteLine, ord('Y'), [ssCtrl]);
  AddKey(ecSelectAll, ord('A'), [ssCtrl]);
  AddKey(ecToggleMode, VK_INSERT, []);
end;

class function TKEditKeyMapping.EmptyMap: TKEditCommandAssignment;
begin
  Result.Command := ecNone;
  Result.Key.Key := 0;
  Result.Key.Shift := [];
end;

procedure TKEditKeyMapping.AddKey(Command: TKEditCommand; Key: Word; Shift: TShiftState);
var
  I: Integer;
begin
  I := Length(FMap);
  SetLength(FMap, I + 1);
  FMap[I].Command := Command;
  FMap[I].Key.Key := Key;
  FMap[I].Key.Shift := Shift;
end;

function TKEditKeyMapping.FindCommand(AKey: Word; AShift: TShiftState): TKEditCommand;
var
  I: Integer;
  Key: TKEditKey;
begin
  Result := ecNone;
  for I := 0 to Length(FMap) - 1 do
  begin
    Key := FMap[I].Key;
    if (Key.Key = AKey) and (Key.Shift = AShift) then
    begin
      Result := FMap[I].Command;
      Exit;
    end;
  end;
end;

function TKEditKeyMapping.GetKey(AIndex: TKEditCommand): TKEditKey;
var
  I: Integer;
begin
  Result.Key := 0;
  Result.Shift := [];
  for I := 0 to Length(FMap) - 1 do
    if FMap[I].Command = AIndex then
    begin
      Result := FMap[I].Key;
      Exit;
    end;
end;

function TKEditKeyMapping.GetAssignment(AIndex: Integer): TKEditCommandAssignment;
begin
  if (AIndex >= 0) and (AIndex < Length(FMap)) then
    Result := FMap[AIndex]
  else
    Result := EmptyMap;
end;

procedure TKEditKeyMapping.SetKey(AIndex: TKEditCommand; const AValue: TKEditKey);
var
  I: Integer;
begin
  for I := 0 to Length(FMap) - 1 do
    if FMap[I].Command = AIndex then
    begin
      FMap[I].Key := AValue;
      Exit;
    end;
end;


end.
