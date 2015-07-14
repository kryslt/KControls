{ @abstract(This unit contains miscellaneous supporting functions)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(20 Oct 2001)
  @lastmod(6 Jul 2014)

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

unit KFunctions;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
 {$IFDEF USE_WINAPI}
  Windows,
 {$ELSE}
  {$IF DEFINED(UNIX) and (FPC_FULLVERSION>=20701)}
    UnixCP,
  {$IFEND}
 {$ENDIF}
 // use the LCL interface support whenever possible
  LCLType, LCLIntf, LMessages, LCLProc, LCLVersion, Interfaces, InterfaceBase,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  Classes, Controls, ComCtrls, Graphics, StdCtrls, SysUtils,
  Forms;

const
{$IFNDEF FPC}
  { @exclude }
  KM_MOUSELEAVE = WM_MOUSELEAVE;
  { @exclude }
  LM_USER = WM_USER;
  { @exclude }
  LM_CANCELMODE = WM_CANCELMODE;
  { @exclude }
  LM_CHAR = WM_CHAR;
  { @exclude }
  LM_CLEAR = WM_CLEAR;
  { @exclude }
  LM_CLOSEQUERY = WM_CLOSE;
  { @exclude }
  LM_COPY = WM_COPY;
  { @exclude }
  LM_CUT = WM_CUT;
  { @exclude }
  LM_DROPFILES = WM_DROPFILES;
  { @exclude }
  LM_ERASEBKGND = WM_ERASEBKGND;
  { @exclude }
  LM_GETDLGCODE = WM_GETDLGCODE;
  { @exclude }
  LM_HSCROLL = WM_HSCROLL;
  { @exclude }
  LM_KEYDOWN = WM_KEYDOWN;
  { @exclude }
  LM_KILLFOCUS = WM_KILLFOCUS;
  { @exclude }
  LM_LBUTTONDOWN = WM_LBUTTONDOWN;
  { @exclude }
  LM_LBUTTONUP = WM_LBUTTONUP;
  { @exclude }
  LM_MOUSEMOVE = WM_MOUSEMOVE;
  { @exclude }
  LM_MOVE = WM_MOVE;
  { @exclude }
  LM_PASTE = WM_PASTE;
  { @exclude }
  LM_SETFOCUS = WM_SETFOCUS;
  { @exclude }
  LM_SIZE = WM_SIZE;
  { @exclude }
  LM_VSCROLL = WM_VSCROLL;
  { @exclude }
  LCL_MAJOR = 0;
  { @exclude }
  LCL_MINOR = 0;
  { @exclude }
  LCL_RELEASE = 0;
{$ELSE}
  // hope this is correct about WM_MOUSELEAVE otherwise adapt it as you wish
 {$IFDEF LCLWin32}
  {$IF ((LCL_MAJOR=0) AND (LCL_MINOR=9) AND (LCL_RELEASE<27))}
  { @exclude }
  KM_MOUSELEAVE = LM_LEAVE; // LCL 0.9.26.2-
  {$ELSE}
  { @exclude }
  KM_MOUSELEAVE = LM_MOUSELEAVE; // LCL 0.9.27+
  {$IFEND}
 {$ELSE}
  {$IFDEF LCLWinCE}
  { @exclude }
  KM_MOUSELEAVE = LM_LEAVE;
  {$ELSE}
  { @exclude }
  KM_MOUSELEAVE = LM_MOUSELEAVE;
  {$ENDIF}
 {$ENDIF}
 { @exclude }
 //WM_CTLCOLORBTN = Messages.WM_CTLCOLORBTN;
 { @exclude }
 //WM_CTLCOLORSTATIC = Messages.WM_CTLCOLORSTATIC;
{$ENDIF}

{$IFDEF USE_WINAPI}
  { @exclude }
  SHFolderDll = 'SHFolder.dll';
{$ENDIF}

  { Base for custom messages used by KControls suite. }
  KM_BASE = LM_USER + 1024;

  { Custom message. }
  KM_LATEUPDATE = KM_BASE + 1;

  { Constant for horizontal resize cursor. }
  crHResize = TCursor(101);
  { Constant for vertical resize cursor. }
  crVResize = TCursor(102);
  { Constant for uncaptured dragging cursor. }
  crDragHandFree = TCursor(103);
  { Constant for captured dragging cursor. }
  crDragHandGrip = TCursor(104);

  { Checkbox frame size in logical screen units. }
  cCheckBoxFrameSize = 13;

  { Carriage return character. }
  cCR = #13;
  { Line feed character. }
  cLF = #10;
  { TAB character. }
  cTAB = #9;
  { SPACE character. }
  cSPACE = #32;
  { String terminator. }
  cNULL = #0;
  { Set of word break characters. }
  cWordBreaks = [cNULL, cTAB, cSPACE];
  { Set of line break characters. }
  cLineBreaks = [cCR, cLF];
  { Text ellipsis string. }
  cEllipsis = '...';
  { Alphabetic letters. }
  cLetters = ['a'..'z', 'A'..'Z'];
  { Number. }
  cNumbers = ['0'..'9'];

{$IFDEF UNIX}
  cEOL = cLF;
  cFirstEOL = cLF;
{$ELSE}
  cEOL = cCR + cLF;
  cFirstEOL = cCR;
{$ENDIF}

type
{$IFNDEF FPC}
  { @exclude }
  TLMessage = TMessage;
  { @exclude }
  TLMCopy = TWMCopy;
  { @exclude }
  TLMMouse = TWMMouse;
  { @exclude }
  TLMNoParams = TWMNoParams;
  { @exclude }
  TLMKey = TWMKey;
  { @exclude }
  TLMChar = TWMChar;
  { @exclude }
  TLMEraseBkGnd = TWMEraseBkGnd;
  { @exclude }
  TLMHScroll = TWMHScroll;
  { @exclude }
  TLMKillFocus = TWMKillFocus;
  { @exclude }
  TLMMove = TWMMove;
  { @exclude }
  TLMPaste = TWMPaste;
  { @exclude }
  TLMSetFocus = TWMSetFocus;
  { @exclude }
  TLMSize = TWMSize;
  { @exclude }
  TLMVScroll = TWMVScroll;

 {$IFNDEF COMPILER17_UP}
  { Support for Win64 messaging. }
  LONG_PTR = Longint;
 {$ENDIF}
{$ENDIF}

  //PInteger = ^Integer; defined by System.pas
  { Static array for Integer. }
  TIntegers = array[0..MaxInt div SizeOf(Integer) - 1] of Integer;
  { Pointer for TIntegers. }
  PIntegers = ^TIntegers;
  { Dynamic array for Integer. }
  TDynIntegers = array of Integer;

  //PCardinal = ^Cardinal; defined by System.pas
  { Static array for Cardinal. }
  TCardinals = array[0..MaxInt div SizeOf(Cardinal) - 1] of Cardinal;
  { Pointer for TCardinals. }
  PCardinals = ^TCardinals;
  { Dynamic array for Cardinal. }
  TDynCardinals = array of Cardinal;

  //PShortInt = ^ShortInt; defined by System.pas
  { Static array for ShortInt. }
  TShortInts = array[0..MaxInt div SizeOf(ShortInt) - 1] of ShortInt;
  { Pointer for TShortInts. }
  PShortInts = ^TShortInts;
  { Dynamic array for ShortInt. }
  TDynShortInts = array of ShortInt;

  //PSmallInt = ^SmallInt; defined by System.pas
  { Static array for SmallInt. }
  TSmallInts = array[0..MaxInt div SizeOf(SmallInt) - 1] of SmallInt;
  { Pointer for TSmallInts. }
  PSmallInts = ^TSmallInts;
  { Dynamic array for SmallInt. }
  TDynSmallInts = array of SmallInt;

  //PLongInt = ^LongInt; defined by System.pas
  { Static array for LongInt. }
  TLongInts = array[0..MaxInt div SizeOf(LongInt) - 1] of LongInt;
  { Pointer for TLongInts. }
  PLongInts = ^TLongInts;
  { Dynamic array for LongInt. }
  TDynLongInts = array of LongInt;

  //PInt64 = ^Int64; defined by System.pas
  { Static array for Int64. }
  TInt64s = array[0..MaxInt div SizeOf(Int64) - 1] of Int64;
  { Pointer for TInt64s. }
  PInt64s = ^TInt64s;
  { Dynamic array for Int64. }
  TDynInt64s = array of Int64;

  //PByte = ^Byte; defined by System.pas
  { Static array for Byte. }
  TBytes = array[0..MaxInt div SizeOf(Byte) - 1] of Byte;
  { Pointer for TBytes. }
  PBytes = ^TBytes;
  { Dynamic array for Byte. }
  TDynBytes = array of Byte;

  //PWord = ^Word; defined by System.pas
  { Static array for Word. }
  TWords = array[0..MaxInt div SizeOf(Word) - 1] of Word;
  { Pointer for TWords. }
  PWords = ^TWords;
  { Dynamic array for Word. }
  TDynWords = array of Word;

  //PLongWord = ^LongWord; defined by System.pas
  { Static array for LongWord. }
  TLongWords = array[0..MaxInt div SizeOf(LongWord) - 1] of LongWord;
  { Pointer for TLongWords. }
  PLongWords = ^TLongWords;
  { Dynamic array for LongWord. }
  TDynLongWords = array of LongWord;

{$IF DEFINED(COMPILER10_UP) OR DEFINED(FPC)}
 {$IFDEF FPC}
  PUInt64 = ^UInt64;
 {$ELSE}
  //PUInt64 = ^UInt64; defined by System.pas
 {$ENDIF}
  { Static array for UInt64. }
  TUInt64s = array[0..MaxInt div SizeOf(UInt64) - 1] of UInt64;
  { Pointer for TUInt64s. }
  PUInt64s = ^TUInt64s;
  { Dynamic array for UInt64. }
  TDynUInt64s = array of UInt64;
{$IFEND}

  //PSingle = ^Single; defined by System.pas
  { Static array for Single. }
  TSingles = array[0..MaxInt div SizeOf(Single) - 1] of Single;
  { Pointer for TSingles. }
  PSingles = ^TSingles;
  { Dynamic array for Single. }
  TDynSingles = array of Single;

  //PDouble = ^Double; defined by System.pas
  { Static array for Double. }
  TDoubles = array[0..MaxInt div SizeOf(Double) - 1] of Double;
  { Pointer for TDoubles. }
  PDoubles = ^TDoubles;
  { Dynamic array for Double. }
  TDynDoubles = array of Double;

{$IFNDEF FPC}
  //PExtended = ^Extended; defined by System.pas
  { Static array for Extended. }
  TExtendeds = array[0..MaxInt div SizeOf(Extended) - 1] of Extended;
  { Pointer for TExtendeds. }
  PExtendeds = ^TExtendeds;
  { Dynamic array for Extended. }
  TDynExtendeds = array of Extended;
{$ENDIF}

  //PChar is special type
  { Static array for Char. }
  TChars = array[0..MaxInt div SizeOf(Char) - 1] of Char;
  { Pointer for TChars. }
  PChars = ^TChars;
  { Dynamic array for Char. }
  TDynChars = array of Char;

  //PAnsiChar is special type
  { Static array for AnsiChar. }
  TAnsiChars = array[0..MaxInt div SizeOf(AnsiChar) - 1] of AnsiChar;
  { Pointer for TChars. }
  PAnsiChars = ^TAnsiChars;
  { Dynamic array for Char. }
  TDynAnsiChars = array of AnsiChar;

  PBoolean = ^Boolean;
  { Static array for Double. }
  TBooleans = array[0..MaxInt div SizeOf(Boolean) - 1] of Boolean;
  { Pointer for TBooleans. }
  PBooleans = ^TBooleans;
  { Dynamic array for Double. }
  TDynBooleans = array of Boolean;

{$IFDEF FPC}
  { TKString is UTF8 string in Lazarus. }
  TKString = string;
  { TKChar is UTF8 character in Lazarus. }
  TKChar = TUTF8Char;
  { PKChar is pointer to UTF8 character in Lazarus. }
  PKChar = ^TUTF8Char;
  { PKText is PChar (null terminated UTF8 string) in Lazarus. }
  PKText = PChar;
{$ELSE}
 {$IFDEF STRING_IS_UNICODE}
  { TKString is UnicodeString in unicode aware Delphi. }
  TKString = string;
  { TKChar is Char in unicode aware Delphi. }
  TKChar = Char;
  { PKChar is pointer to Char in unicode aware Delphi. }
  PKChar = ^Char;
  { PKText is PChar in unicode aware Delphi. }
  PKText = PChar;
 {$ELSE}
  { TKString is WideString in old non-unicode Delphi versions. }
  TKString = WideString;
  { TKChar is WideChar in old non-unicode Delphi versions. }
  TKChar = WideChar;
  { PKChar is pointer to WideChar in old non-unicode Delphi versions. }
  PKChar = ^WideChar;
  { PKText is PWideChar in old non-unicode Delphi versions. }
  PKText = PWideChar;
  {$ENDIF}
 {$ENDIF}

  { Useful structure to handle general data and size as a single item }
  TDataSize = record
    Data: Pointer;
    Size: Integer;
  end;
  { Pointer for TDataSize }
  PDataSize = ^TDataSize;

  { Set type for @link(CharInSetEx). }
  TKSysCharSet = set of AnsiChar;

  { Defines a currency format settings for @link(FormatCurrency). }
  TKCurrencyFormat = record
    CurrencyFormat,
    CurrencyDecimals: Byte;
    CurrencyString: TKString;
    DecimalSep: Char;
    ThousandSep: Char;
    UseThousandSep: Boolean;
  end;

  { Record for LCL context switching (e.g. between app and shared library). }
  TKAppContext = record
    Application: TApplication;
    Screen: TScreen;
    GlobalNameSpace: IReadWriteSync;
    MainThreadID: LongWord;
    IntConstList: TThreadList;
  {$IFDEF FPC}
    WidgetSet: TWidgetSet;
    DragManager: TDragManager;
  {$ENDIF}
  end;

  { Pointer to TKAppContext }
  PKAppContext = ^TKAppContext;

{ Replaces possible decimal separators in S with DecimalSeparator variable.}
function AdjustDecimalSeparator(const S: string): string;

{ Converts an AnsiString into a TKString. If CodePage is not set
  the current system code page for ANSI-UTFx translations will be used. }
function AnsiStringToString(const Text: AnsiString; CodePage: Cardinal = 0): TKString;

type
  { Callback for binary search data item comparison. }
  TBsCompareProc = function(Data: Pointer; Index: Integer; KeyPtr: Pointer): Integer;

{ Performs binary search on previously sorted data given by AData and ACount.
  KeyPtr is the pointer to the key which is passed to the ACompareProc.
  The items are compared by CompareProc callback. Returns the zero based index
  of the matched data or -1 if no match has been found. }
function BinarySearch(AData: Pointer; ACount: Integer; KeyPtr: Pointer;
  ACompareProc: TBSCompareProc; ASortedDown: Boolean): Integer;

{ Under Windows this function calls the WinAPI TrackMouseEvent. Under other OSes
  the implementation is still missing. }
procedure CallTrackMouseEvent(Control: TWinControl; var Status: Boolean);

{ Center window identified by CenteredWnd with regard to another window BoundWnd. }
procedure CenterWindowInWindow(CenteredWnd, BoundWnd: HWnd);

{ Center window identified by CenteredWnd with regard to main screen. }
procedure CenterWindowOnScreen(CenteredWnd: HWnd);

{ Compiler independent Delphi2009-like CharInSet function for ANSI characters. }
function CharInSetEx(AChar: AnsiChar; const ASet: TKSysCharSet): Boolean; overload;

{ Compiler independent Delphi2009-like CharInSet function for Unicode characters. }
function CharInSetEx(AChar: WideChar; const ASet: TKSysCharSet): Boolean; overload;

{ Compares two Integers. Returns 1 if I1 > I2, -1 if I1 < I2 and 0 if I1 = I2. }
function CompareIntegers(I1, I2: Integer): Integer;

{ Compares two PWideChar strings. Returns 1 if W1 > W2, -1 if W1 < W2 and
  0 if W1 = W2. The strings will be compared using the default user locale
  unless another locale has been specified in Locale. }
function CompareWideChars(W1, W2: PWideChar{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal = LOCALE_USER_DEFAULT{$ENDIF}): Integer;

{$IFDEF STRING_IS_UNICODE}
{ Compares two Unicode strings (Lazarus, Delphi 2009 and better). Returns 1 if S1 > S2,
  -1 if S1 < S2 and 0 if S1 = S2. The strings will be compared using the default
  user locale unless another locale has been specified in Locale. }
function CompareChars(S1, S2: PChar{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal = LOCALE_USER_DEFAULT{$ENDIF}): Integer;
{$ENDIF}

{ Compares two WideString strings. Returns 1 if W1 > W2, -1 if W1 < W2 and
  0 if W1 = W2. The strings will be compared using the default user locale
  unless another locale has been specified in Locale. }
function CompareWideStrings(W1, W2: WideString{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal = LOCALE_USER_DEFAULT{$ENDIF}): Integer;

{$IFDEF STRING_IS_UNICODE}
{ Compares two Unicode strings (Lazarus, Delphi 2009 and better). Returns 1 if S1 > S2,
  -1 if S1 < S2 and 0 if S1 = S2. The strings will be compared using the default
  user locale unless another locale has been specified in Locale. }
function CompareStrings(S1, S2: string{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal = LOCALE_USER_DEFAULT{$ENDIF}): Integer;
{$ENDIF}

{ Converts tab characters in a string to space characters. }
procedure ConvertTabsToSpaces(var AText: TKString; ASpacesForTab: Integer);

{ Creates given directory, even if more folders have to be created. }
function CreateMultipleDir(const Dir: string): Boolean;

{ Converts hexadecimal digit to nibble. }
function DigitToNibble(Digit: AnsiChar; var Nibble: Byte): Boolean;

{ Performs integer division. If there is a nonzero remainder,
  the result will be incremented. }
function DivUp(Dividend, Divisor: Integer): Integer;

{ Performs integer division. If there is a nonzero remainder,
  the result will be decremented. }
function DivDown(Dividend, Divisor: Integer): Integer;

{ Returns True if focused window is some text editing window, such as TEdit. }
function EditIsFocused: Boolean;

{ Returns True if some text editing window is focused and contains a text. }
function EditIsTextFocused: Boolean;

{ Returns True if the focused text editing window can perform an undo operation. }
function EditCanUndoFocused: Boolean;

{ Performs an undo operation on the focused text editing window. }
procedure EditUndoFocused;

{ Performs a delete operation on the focused text editing window. }
procedure EditDeleteFocused;

{ Performs a clipboard cut operation on the focused text editing window. }
procedure EditCutFocused;

{ Performs a clipboard copy operation on the focused text editing window. }
procedure EditCopyFocused;

{ Performs a clipboard paste operation on the focused text editing window. }
procedure EditPasteFocused;

{ Performs a select all operation on the focused text editing window. }
procedure EditSelectAllFocused;

{ Enables or disables all children of AParent depending on AEnabled.
  If ARecursive is True then the function applies to whole tree of controls
  owned by AParent. }
procedure EnableControls(AParent: TWinControl; AEnabled, ARecursive: Boolean);

{ Ensures the path given by APath has slash at the end. }
procedure EnsureLastPathSlash(var APath: string);

{ Raises a general exception with associated message Msg. }
procedure Error(const Msg: string);

{ Swaps values of two SmallInt variables. }
procedure Exchange(var Value1, Value2: SmallInt); overload;
{ Swaps values of two ShortInt variables. }
procedure Exchange(var Value1, Value2: ShortInt); overload;
{ Swaps values of two Integer variables. }
procedure Exchange(var Value1, Value2: Integer); overload;
{ Swaps values of two Int64 variables. }
procedure Exchange(var Value1, Value2: Int64); overload;
{ Swaps values of two Byte variables. }
procedure Exchange(var Value1, Value2: Byte); overload;
{ Swaps values of two Word variables. }
procedure Exchange(var Value1, Value2: Word); overload;
{ Swaps values of two Cardinal variables. }
procedure Exchange(var Value1, Value2: Cardinal); overload;
{$IFDEF COMPILER10_UP }
{ Swaps values of two UInt64 variables. }
procedure Exchange(var Value1, Value2: UInt64); overload;
{$ENDIF}
{ Swaps values of two Single variables. }
procedure Exchange(var Value1, Value2: Single); overload;
{ Swaps values of two Double variables. }
procedure Exchange(var Value1, Value2: Double); overload;
{$IFNDEF FPC}
{ Swaps values of two Extended variables. }
procedure Exchange(var Value1, Value2: Extended); overload;
{$ENDIF}
{ Swaps values of two Char variables. }
procedure Exchange(var Value1, Value2: Char); overload;

{ Fills the message record. }
function FillMessage(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): TLMessage;

{ Formats the given currency value with to specified parameters. Not thread safe. }
function FormatCurrency(Value: Currency; const AFormat: TKCurrencyFormat): TKString;

{ Backups application context, e.g. when calling a shared library. }
function GetAppContext(var Ctx: TKAppContext): Boolean;

{ Returns the module version for given module. Works under WinX only. }
function GetAppVersion(const ALibName: string; var MajorVersion, MinorVersion, BuildNumber, RevisionNumber: Word): Boolean;

{ Returns number of a specific character in a string. }
function GetCharCount(const AText: TKString; AChar: TKChar): Integer;

{ Returns the Text property of any TWinControl instance as WideString (up to Delphi 2007)
  or string (Delphi 2009, Lazarus). }
function GetControlText(Value: TWinControl): TKString;

{ Returns the standard locale dependent format settings. }
function GetFormatSettings: TFormatSettings;

{ Returns current status of Shift, Alt and Ctrl keys. }
function GetShiftState: TShiftState;

{ Converts an integer into binary string with custom alignment
  (given by Digits). }
function IntToAscii(Value: Int64; Digits: Integer): string;
{ Converts an integer into binary digit string with custom alignment
  (given by Digits) and suffix. }
function IntToBinStr(Value: Int64; Digits: Byte; const Suffix: string): string;
{ Converts an integer value into BCD number. }
function IntToBCD(Value: Cardinal): Cardinal;
{ Converts an integer into decimal digit string. Equals to IntToStr. }
function IntToDecStr(Value: Int64): string;
{ Converts an integer into hexadecimal digit string with custom alignment
  (given by Digits), prefix and suffix. Digits represented by alphabetical
  characters can be either in lower or upper case. }
function IntToHexStr(Value: Int64; Digits: Byte; const Prefix, Suffix: string;
  UseLowerCase: Boolean): string;
{ Converts an integer into octal digit string. }
function IntToOctStr(Value: Int64): string;

function IntPowerInt(Value: Int64; Exponent: Integer): Int64;

{ Converts a binary string into integer with custom alignment (given by Digits). }
function AsciiToInt(S: string; Digits: Integer): Int64;
{ Converts a BCD number into integer value. }
function BCDToInt(Value: Cardinal): Cardinal;
{ Converts a binary digit string into integer with custom alignment
  (given by Digits) and sign of a value represented by the string (given by Signed).
  Code returns either zero for a successful conversion or the position of
  first bad character. }
function BinStrToInt(S: string; Digits: Byte; Signed: Boolean;
  var Code: Integer): Int64;
{ Converts a decimal digit string into integer. Code returns either zero for
  a successful conversion or the position of first bad character. Equals to Val. }
function DecStrToInt(S: string; var Code: Integer): Int64;
{ Converts a hexadecimal digit string into integer with custom alignment
  (given by Digits) and sign of a value represented by the string (given by Signed).
  Code returns either zero for a successful conversion or the position of
  first bad character. }
function HexStrToInt(S: string; Digits: Byte; Signed: Boolean;
  var Code: Integer): Int64;
{ Converts an octal digit string into integer. Code returns either zero for
  a successful conversion or the position of first bad character. }
function OctStrToInt(S: string; var Code: Integer): Int64;

{ Calls SysUtils.Format. }
function KFormat(const Format: string; const Args: array of const;
  const AFormatSettings: TFormatSettings): string; overload;

{ Calls SysUtils.WideFormat. }
function KFormat(const Format: WideString; const Args: array of const;
  const AFormatSettings: TFormatSettings): WideString; overload;

{ Returns a clipped ShortInt value so that it lies between Min and Max }
function MinMax(Value, Min, Max: ShortInt): ShortInt; overload;
{ Returns a clipped SmallInt value so that it lies between Min and Max }
function MinMax(Value, Min, Max: SmallInt): SmallInt; overload;
{ Returns a clipped Integer value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Integer): Integer; overload;
{ Returns a clipped Int64 value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Int64): Int64; overload;
{ Returns a clipped Single value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Single): Single; overload;
{ Returns a clipped Double value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Double): Double; overload;
{$IFNDEF FPC}
{ Returns a clipped Extended value so that it lies between Min and Max }
function MinMax(Value, Min, Max: Extended): Extended; overload;
{$ENDIF}

{ Converts nibble to hexadecimal digit. }
function NibbleToDigit(Nibble: Byte; UpperCase: Boolean): AnsiChar;

type
  { Callback for quicksort data item comparison. }
  TQsCompareProc = function(Data: Pointer; Index1, Index2: Integer): Integer;
  { Callback for quicksort data item exchange. }
  TQsExchangeProc = procedure(Data: Pointer; Index1, Index2: Integer);

{ Sorts Count number of items by means of a non recursive quicksort algorithm.
  The items are compared by CompareProc callback and sorted by
  ExchangeProc callback. }
procedure QuickSortNR(AData: Pointer; ACount: Integer; ACompareProc: TQsCompareProc;
  AExchangeProc: TQsExchangeProc; ASortedDown: Boolean);

{ Sorts Count number of items by means of a recursive quicksort algorithm.
  The items are compared by CompareProc callback and sorted by
  ExchangeProc callback. }
procedure QuickSort(AData: Pointer; ACount: Integer; ACompareProc: TQsCompareProc;
  AExchangeProc: TQsExchangeProc; ASortedDown: Boolean);

{ Add AX and AY to APoint. }
procedure OffsetPoint(var APoint: TPoint; AX, AY: Integer); overload;

{ Add AOffset to APoint. }
procedure OffsetPoint(var APoint: TPoint; const AOffset: TPoint); overload;

{ Examines if some part of Rect lies within Bounds. }
function RectInRect(Bounds, Rect: TRect): Boolean;

{ Add AX and AY to ARect. }
procedure OffsetRect(var ARect: TRect; AX, AY: Integer); overload;

{ Add AOffset to ARect. }
procedure OffsetRect(var ARect: TRect; const AOffset: TPoint); overload;

{ Restores application context, e.g. when calling a shared library. }
function SetAppContext(const Ctx: TKAppContext): Boolean;

{ Under Windows this function calls the WinAPI SetWindowRgn. Under other OSes
  the implementation is still missing. }
procedure SetControlClipRect(AControl: TWinControl; const ARect: TRect);

{ Modifies the Text property of any TWinControl instance. The value is given as
  WideString (up to Delphi 2007) or string (Delphi 2009, Lazarus). }
procedure SetControlText(Value: TWinControl; const Text: TKString);

{ Ensures the path given by APath has no slash at the end. }
procedure StripLastPathSlash(var APath: string);

{ Returns next character index for given null terminated string and character index.
  Takes MBCS (UTF8 in Lazarus) into account. }
function StrNextCharIndex(const AText: TKString; Index: Integer): Integer;

{ Returns previous character index for given null terminated string and character index.
  Takes MBCS (UTF8 in Lazarus) into account. }
function StrPreviousCharIndex(const AText: TKString; Index: Integer): Integer;

{ Returns the index for given string where character at given index begins.
  Takes MBCS (UTF8 in Lazarus) into account. }
function StringCharBegin(const AText: TKString; Index: Integer): Integer;

{ Returns the number of characters in a string. Under Delphi it equals Length,
  under Lazarus it equals UTF8Length. }
function StringLength(const AText: TKString): Integer;

{ Returns next character index for given string and character index.
  Takes MBCS (UTF8 in Lazarus) into account. }
function StringNextCharIndex(const AText: TKString; Index: Integer): Integer;

{ Performs standard Copy operation. Takes MBCS (UTF8 in Lazarus) into account. }
function StringCopy(const ASource: TKString; At, Count: Integer): TKString;

{ Performs standard Delete operation. Takes MBCS (UTF8 in Lazarus) into account. }
procedure StringDelete(var ASource: TKString; At, Count: Integer);

{ Trims characters specified by ASet from the beginning and end of AText.
  New text length is returned by ALen. }
procedure TrimWhiteSpaces(const AText: TKString; var AStart, ALen: Integer; const ASet: TKSysCharSet); overload;

{ Trims characters specified by ASet from the beginning and end of AText. }
procedure TrimWhiteSpaces(var AText: TKString; const ASet: TKSysCharSet); overload;

{$IFNDEF FPC}
{ Trims characters specified by ASet from the beginning and end of AText. }
procedure TrimWhiteSpaces(var AText: AnsiString; const ASet: TKSysCharSet); overload;
{$ENDIF}

{ Converts a TKString into AnsiString. If CodePage is not set
  the current system code page for ANSI-UTFx translations will be used. }
function StringToAnsiString(const AText: TKString; CodePage: Cardinal = 0): AnsiString;

{$IFDEF USE_WINAPI}
function GetWindowsFolder(CSIDL: Cardinal; var APath: string): Boolean;

function RunExecutable(const AFileName: string; AWaitForIt: Boolean): DWORD;
{$ENDIF}

function SystemCodePage: Integer;

function UnicodeUpperCase(const AText: TKString): TKString;
function UnicodeLowerCase(const AText: TKString): TKString;
function UnicodeToNativeUTF(const AParam: WideChar): TKString;
function UnicodeStringReplace(const AText, AOldPattern, ANewPattern: TKString;
  AFlags: TReplaceFlags): TKString;

implementation

uses
  Math, TypInfo
{$IFDEF USE_WINAPI}
  , ShlObj
{$ELSE}
  , versionresource
{$ENDIF}
{$IFDEF USE_WIDEWINPROCS}
  , KWideWinProcs
{$ENDIF}
{$IFDEF FPC}
  , LConvEncoding
{$ENDIF}
;

function AdjustDecimalSeparator(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(Result) do
    if CharInSetEx(Result[I], [',', '.']) then
      Result[I] := GetFormatSettings.DecimalSeparator;
end;

function AnsiStringToString(const Text: AnsiString; CodePage: Cardinal): TKString;
var
{$IFDEF FPC}
  CP: string;
{$ELSE}
  Len: Integer;
{$ENDIF}
begin
{$IFDEF FPC}
  if CodePage = 0 then
    CP := 'ansi'
  else
    CP := Format('cp%d', [Codepage]);
  Result := LConvEncoding.ConvertEncoding(Text, CP, 'utf8');
{$ELSE}
  Len := MultiByteToWideChar(CodePage, 0, PAnsiChar(Text), -1, nil, 0);
  SetLength(Result, Len shr 1);
  MultiByteToWideChar(CodePage, 0, PAnsiChar(Text), -1, PWideChar(Result), Len);
{$ENDIF}
end;

function BinarySearch(AData: Pointer; ACount: Integer; KeyPtr: Pointer;
  ACompareProc: TBsCompareProc; ASortedDown: Boolean): Integer;
var
  Lo, Hi, Index, Ret: Integer;
begin
  Result := -1;
  Lo := 0;
  Hi := ACount - 1;
  repeat
    Index := (Lo + Hi) div 2;
    Ret := ACompareProc(AData, Index, KeyPtr);
    if ASortedDown and (Ret < 0) or not ASortedDown and (Ret > 0) then
      Hi := Index - 1
    else
      Lo := Index + 1
  until (Lo > Hi) or (Ret = 0);
  if Ret = 0 then
    Result := Index;
end;


procedure CallTrackMouseEvent(Control: TWinControl; var Status: Boolean);
{$IFDEF USE_WINAPI}
var
  TE: TTrackMouseEvent;
begin
  if not Status then
  begin
    TE.cbSize := SizeOf(TE);
    TE.dwFlags := TME_LEAVE;
    TE.hwndTrack := Control.Handle;
    TE.dwHoverTime := HOVER_DEFAULT;
    TrackMouseEvent(TE);
    Status := True;
  end;
end;
{$ELSE}
begin
  // This is a TODO for Lazarus team.
end;
{$ENDIF}

procedure CenterWindowInWindow(CenteredWnd, BoundWnd: HWnd);
var
  R1, R2: TRect;
begin
  GetWindowRect(CenteredWnd, R1);
  GetWindowRect(BoundWnd, R2);
  R1.Left := Max((R2.Right - R2.Left - R1.Right + R1.Left) div 2, 0);
  R1.Top := Max((R2.Bottom - R2.Top - R1.Bottom + R1.Top) div 2, 0);
  SetWindowPos(CenteredWnd, 0, R1.Left, R1.Top, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
end;

procedure CenterWindowOnScreen(CenteredWnd: HWnd);
var
  R: TRect;
begin
  GetWindowRect(CenteredWnd, R);
  R.Left := Max((Screen.Width - R.Right + R.Left) div 2, 0);
  R.Top := Max((Screen.Height - R.Bottom + R.Top) div 2, 0);
  SetWindowPos(CenteredWnd, 0, R.Left, R.Top, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
end;

function CharInSetEx(AChar: AnsiChar; const ASet: TKSysCharSet): Boolean;
begin
  Result := AChar in ASet;
end;

function CharInSetEx(AChar: WideChar; const ASet: TKSysCharSet): Boolean;
begin
  Result := (Ord(AChar) < $100) and
  {$IFDEF COMPILER12_UP}
    CharInSet(AChar, ASet);
  {$ELSE}
    (AnsiChar(AChar) in ASet);
  {$ENDIF}
end;

function CompareIntegers(I1, I2: Integer): Integer;
begin
  if I1 > I2 then Result := 1
  else if I1 < I2 then Result := -1
  else Result := 0;
end;

function CompareWideChars(W1, W2: PWideChar{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal{$ENDIF}): Integer;
begin
  if (W1 = nil) or (W2 = nil) then
  begin
    if W1 <> nil then Result := 1
    else if W2 <> nil then Result := -1
    else Result := 0;
  end else
  begin
  {$IFDEF USE_WIDEWINPROCS}
    Result := WideWinProcs.CompareString(Locale, 0, W1, -1, W2, -1);
    Dec(Result, 2);
  {$ELSE}
    Result := WideCompareStr(WideString(W1), WideString(W2));
  {$ENDIF}
  end;
end;

{$IFDEF STRING_IS_UNICODE}
function CompareChars(S1, S2: PChar{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal{$ENDIF}): Integer;
begin
  if (S1 = nil) or (S2 = nil) then
  begin
    if S1 <> nil then Result := 1
    else if S2 <> nil then Result := -1
    else Result := 0;
  end else
  begin
  {$IFDEF USE_WIDEWINPROCS}
    Result := WideWinProcs.CompareString(Locale, 0, PWideChar(S1), -1, PWideChar(S2), -1);
    Dec(Result, 2);
  {$ELSE}
    Result := CompareStr(string(S1), string(S2));
  {$ENDIF}
  end;
end;
{$ENDIF}

function CompareWideStrings(W1, W2: WideString{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal{$ENDIF}): Integer;
begin
{$IFDEF USE_WIDEWINPROCS}
  Result := WideWinProcs.CompareString(Locale, 0, PWideChar(W1), -1, PWideChar(W2), -1);
  Dec(Result, 2);
{$ELSE}
  Result := WideCompareStr(W1, W2);
{$ENDIF}
end;

{$IFDEF STRING_IS_UNICODE}
function CompareStrings(S1, S2: string{$IFDEF USE_WIDEWINPROCS}; Locale: Cardinal{$ENDIF}): Integer;
begin
{$IFDEF USE_WIDEWINPROCS}
  Result := WideWinProcs.CompareString(Locale, 0, PWideChar(S1), -1, PWideChar(S2), -1);
  Dec(Result, 2);
{$ELSE}
  Result := CompareStr(S1, S2);
{$ENDIF}
end;
{$ENDIF}

procedure ConvertTabsToSpaces(var AText: TKString; ASpacesForTab: Integer);
var
  TabCount: Integer;
  S: TKString;
  I, J, K: Integer;
begin
  if ASpacesForTab >= 0 then
  begin
    TabCount := GetCharCount(AText, cTAB);
    if TabCount > 0 then
    begin
      SetLength(S, Length(AText) + (ASpacesForTab - 1) * TabCount);
      J := 1;
      for I := 1 to Length(AText) do
      begin
        if AText[I] = cTAB then
        begin
          for K := 0 to ASpacesForTab - 1 do
          begin
            S[J] := cSPACE;
            Inc(J);
          end;
        end else
        begin
          S[J] := AText[I];
          Inc(J);
        end;
      end;
      AText := S;
    end;
  end;
end;

function CreateMultipleDir(const Dir: string): Boolean;
var
  I: Integer;
  T: string;
begin
  for I := 1 to Length(Dir) do
  begin
    if CharInSetEx(Dir[I], ['/', '\']) then
    begin
      T := Copy(Dir, 1, I - 1);
      if not (DirectoryExists(T) or CreateDir(T)) then Break;
    end;
  end;
  if not DirectoryExists(Dir) then
    CreateDir(Dir);
  Result := DirectoryExists(Dir);
end;

function DigitToNibble(Digit: AnsiChar; var Nibble: Byte): Boolean;
begin
  Result := True;
  if (Digit >= '0') and (Digit <= '9') then
    Nibble := Ord(Digit) - Ord('0')
  else if (Digit >= 'a') and (Digit <= 'f') then
    Nibble := Ord(Digit) - Ord('a') + 10
  else if (Digit >= 'A') and (Digit <= 'F') then
    Nibble := Ord(Digit) - Ord('A') + 10
  else
    Result := False;
end;

function DivUp(Dividend, Divisor: Integer): Integer;
begin
  if Divisor = 0 then
    Result := 0
  else if Dividend mod Divisor > 0 then
    Result := Dividend div Divisor + 1
  else
    Result := Dividend div Divisor;
end;

function DivDown(Dividend, Divisor: Integer): Integer;
begin
  if Divisor = 0 then
    Result := 0
  else if Dividend mod Divisor < 0 then
    Result := Dividend div Divisor - 1
  else
    Result := Dividend div Divisor;
end;

{$IFDEF USE_WINAPI}
function EditFocusedHandle: THandle;
var
  Len: Integer;
  Wnd: HWND;
  S: string;
  C: TWinControl;
begin
  Result := 0;
  Wnd := GetFocus;
  C := FindControl(Wnd);
  if (C <> nil) and (C is TCustomEdit) or (C is TCustomMemo) or
    (C is TComboBox) and (TComboBox(C).Style in [csSimple, csDropDown])
{$IFnDEF FPC}
   or (C is TRichEdit)
{$ENDIF}
  then
    Result := Wnd
  else
  begin
    SetLength(S, 100);
    Len := GetClassName(Wnd, PChar(S), 100);
    if Len > 0 then
    begin
      SetLength(S, Len);
      S := UpperCase(S);
      if (S = 'EDIT') then
        Result := Wnd;
    end;
  end;
end;
{$ENDIF}

function EditIsFocused: Boolean;
{$IFDEF USE_WINAPI}
begin
  Result := EditFocusedHandle <> 0;
end;
{$ELSE}
begin
  // can this be implemented somehow?
  Result := False;
end;
{$ENDIF}

function EditIsTextFocused: Boolean;
{$IFDEF USE_WINAPI}
var
  A, B: Integer;
  Wnd: THandle;
begin
  Wnd := EditFocusedHandle;
  if Wnd <> 0 then
  begin
    SendMessage(Wnd, EM_GETSEL, WParam(@A), LParam(@B));
    Result := A <> B;
  end else
    Result := False;
end;
{$ELSE}
begin
  // can this be implemented somehow?
  Result := False;
end;
{$ENDIF}

function EditCanUndoFocused: Boolean;
begin
{$IFDEF USE_WINAPI}
  Result := LongBool(SendMessage(GetFocus, EM_CANUNDO, 0, 0));
{$ELSE}
  // can this be implemented somehow?
  Result := False;
{$ENDIF}
end;

procedure EditUndoFocused;
begin
{$IFDEF USE_WINAPI}
  SendMessage(GetFocus, WM_UNDO, 0, 0);
{$ENDIF}
end;

procedure EditDeleteFocused;
begin
  SendMessage(GetFocus, LM_CLEAR, 0, 0);
end;

procedure EditCutFocused;
begin
  SendMessage(GetFocus, LM_CUT, 0, 0);
end;

procedure EditCopyFocused;
begin
  SendMessage(GetFocus, LM_COPY, 0, 0);
end;

procedure EditPasteFocused;
begin
  SendMessage(GetFocus, LM_PASTE, 0, 0);
end;

procedure EditSelectAllFocused;
begin
{$IFDEF USE_WINAPI}
  SendMessage(GetFocus, EM_SETSEL, 0, -1);
{$ENDIF}
end;

procedure EnableControls(AParent: TWinControl; AEnabled, ARecursive: Boolean);

  procedure DoEnable(AParent: TWinControl);
  var
    I: Integer;
  begin
    if AParent <> nil then
      for I := 0 to AParent.ControlCount - 1 do
      begin
        AParent.Controls[I].Enabled := AEnabled;
        if ARecursive and (AParent.Controls[I] is TWinControl) then
          DoEnable(TWinControl(AParent.Controls[I]));
      end;
  end;

begin
  DoEnable(AParent);
end;

procedure EnsureLastPathSlash(var APath: string);
begin
  if APath <> '' then
    if not CharInSetEx(APath[Length(APath)], ['\', '/']) then APath := APath + '/';
end;

procedure Exchange(var Value1, Value2: ShortInt);
var
  Tmp: ShortInt;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: SmallInt);
var
  Tmp: SmallInt;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Integer);
var
  Tmp: Integer;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Int64);
var
  Tmp: Int64;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Byte);
var
  Tmp: Byte;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Word);
var
  Tmp: Word;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Cardinal);
var
  Tmp: Cardinal;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

{$IFDEF COMPILER10_UP }
procedure Exchange(var Value1, Value2: UINT64);
var
  Tmp: UINT64;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;
{$ENDIF}

procedure Exchange(var Value1, Value2: Single);
var
  Tmp: Single;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Exchange(var Value1, Value2: Double);
var
  Tmp: Double;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

{$IFNDEF FPC}
procedure Exchange(var Value1, Value2: Extended);
var
  Tmp: Extended;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;
{$ENDIF}

procedure Exchange(var Value1, Value2: Char);
var
  Tmp: Char;
begin
  Tmp := Value1;
  Value1 := Value2;
  Value2 := Tmp;
end;

procedure Error(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

function FillMessage(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): TLMessage;
begin
  Result.Msg := Msg;
  Result.LParam := LParam;
  Result.WParam := WParam;
  Result.Result := 0;
end;

function FormatCurrency(Value: Currency; const AFormat: TKCurrencyFormat): TKString;
var
  Fmt: string;
  FS: TFormatSettings;
begin
  if AFormat.UseThousandSep then
  begin
    FS.ThousandSeparator := AFormat.ThousandSep;
    Fmt := '%.*n';
  end else
    Fmt := '%.*f';
  FS.DecimalSeparator := AFormat.DecimalSep;
  case AFormat.CurrencyFormat of
    0: Result := KFormat('%s' + Fmt, [AFormat.CurrencyString, AFormat.CurrencyDecimals, Value], FS);
    1: Result := KFormat(Fmt + '%s', [AFormat.CurrencyDecimals, Value, AFormat.CurrencyString], FS);
    2: Result := KFormat('%s ' + Fmt, [AFormat.CurrencyString, AFormat.CurrencyDecimals, Value], FS);
  else
    Result := KFormat(Fmt + ' %s', [AFormat.CurrencyDecimals, Value, AFormat.CurrencyString], FS);
  end;
end;

function GetAppContext(var Ctx: TKAppContext): Boolean;
begin
  Ctx.Application := Forms.Application;
  Ctx.Screen := Forms.Screen;
  Ctx.GlobalNameSpace := Classes.GlobalNameSpace;
//  Ctx.IntConstList := Classes.IntConstList;
{$IFDEF FPC}
  Ctx.MainThreadID := Classes.MainThreadID;
  Ctx.DragManager := Controls.DragManager;
  Ctx.WidgetSet := InterfaceBase.WidgetSet;
{$ENDIF}
  Result := True;
end;

function GetAppVersion(const ALibName: string; var MajorVersion, MinorVersion, BuildNumber, RevisionNumber: Word): Boolean;
var
{$IFDEF USE_WINAPI}
 dwHandle, dwLen: DWORD;
 BufLen: Cardinal;
 lpData: LPTSTR;
 pFileInfo: ^VS_FIXEDFILEINFO;
{$ELSE}
 Info: TVersionResource;
 Stream: TResourceStream;
 ResID: Integer;
 Res: TFPResourceHandle;
{$ENDIF}
begin
  Result := False;
{$IFDEF USE_WINAPI}
  dwLen := GetFileVersionInfoSize(PChar(ALibName), dwHandle);
  if dwLen <> 0 then
  begin
    GetMem(lpData, dwLen);
    try
      if GetFileVersionInfo(PChar(ALibName), dwHandle, dwLen, lpData) then
      begin
        if VerQueryValue(lpData, '\\', Pointer(pFileInfo), BufLen) then
        begin
          MajorVersion := HIWORD(pFileInfo.dwFileVersionMS);
          MinorVersion := LOWORD(pFileInfo.dwFileVersionMS);
          BuildNumber := HIWORD(pFileInfo.dwFileVersionLS);
          RevisionNumber := LOWORD(pFileInfo.dwFileVersionLS);
          Result := True;
        end;
      end;
    finally
      FreeMem(lpData);
    end;
  end;
{$ELSE}
  Info := TVersionResource.Create;
  try
    ResID := 1;
    // Defensive code to prevent failure if no resource available...
    Res := FindResource(HInstance, PChar(PtrInt(ResID)), PChar(RT_VERSION));
    If Res = 0 Then
      Exit;

    Stream := TResourceStream.CreateFromID(HInstance, ResID, PChar(RT_VERSION));
    Try
      Info.SetCustomRawDataStream(Stream);
      MajorVersion := Info.FixedInfo.FileVersion[0];
      MinorVersion := Info.FixedInfo.FileVersion[1];
      BuildNumber := Info.FixedInfo.FileVersion[2];
      RevisionNumber := Info.FixedInfo.FileVersion[3];
      Info.SetCustomRawDataStream(nil);
    Finally
      Stream.Free;
    End;
    Result := True;
  finally
    Info.Free;
  end;
{$ENDIF}
end;

function GetCharCount(const AText: TKString; AChar: TKChar): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(AText) do
    if AText[I] = AChar then
      Inc(Result);
end;

function GetControlText(Value: TWinControl): TKString;

  function GetTextBuffer(Value: TWinControl): string;
  begin
    SetLength(Result, Value.GetTextLen);
    Value.GetTextBuf(PChar(Result), Length(Result) + 1);
  end;

begin
{$IFDEF FPC}
  Result := GetTextBuffer(Value); // conversion from UTF8 forced anyway
{$ELSE}
 {$IFDEF STRING_IS_UNICODE}
  Result := GetTextBuffer(Value);
 {$ELSE}
  if Value.HandleAllocated and (Win32Platform = VER_PLATFORM_WIN32_NT) then // unicode fully supported
  begin
    SetLength(Result, GetWindowTextLengthW(Value.Handle));
    GetWindowTextW(Value.Handle, PWideChar(Result), Length(Result) + 1);
  end else
    Result := GetTextBuffer(Value);
 {$ENDIF}
{$ENDIF}
end;

function GetFormatSettings: TFormatSettings;
begin
{$IFDEF FPC}
  Result := FormatSettings;
{$ELSE}
 {$IFDEF COMPILER12_UP}
  Result := TFormatSettings.Create;
 {$ELSE}
  GetLocaleFormatSettings(GetThreadLocale, Result);
 {$ENDIF}
{$ENDIF}
end;

function GetShiftState: TShiftState;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;

function IntToAscii(Value: Int64; Digits: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := 0;
  while I < Digits do
  begin
    Result := Result + Chr(Value and $FF);
    Value := Value shr 8;
    Inc(I);
  end;
end;

function IntToBCD(Value: Cardinal): Cardinal;
var
  Exp: Cardinal;
begin
  Result := 0;
  Exp := 1;
  while (Value > 0) and (Exp > 0) do
  begin
    Result := Result + Value mod 10 * Exp;
    Value := Value div 10;
    Exp := Exp * 16;
  end;
end;

function IntToBinStr(Value: Int64; Digits: Byte; const Suffix: string): string;
var
  B: Byte;
  C: Char;
begin
  Result := '';
  if Digits <> 0 then
    Digits := MinMax(Digits, 1, 64);
  repeat
    B := Byte(Value and $1);
    Value := Value shr 1;
    C := Chr(Ord('0') + B);
    Result := C + Result;
  until (Value = 0) or ((Digits <> 0) and (Length(Result) = Digits));
  while Length(Result) < Digits do
    Result := '0' + Result;
  Result := Result + Suffix;
end;

function IntToDecStr(Value: Int64): string;
var
  B: Byte;
  C: Char;
  Signum: Boolean;
begin
  if Value < 0 then
  begin
    Signum := True;
    Value := -Value;
  end else
    Signum := False;
  Result := '';
  repeat
    B := Byte(Value mod 10);
    Value := Value div 10;
    C := Chr(Ord('0') + B);
    Result := C + Result;
  until Value = 0;
  Result := '0' + Result;
  if Signum then
    Result := '-' + Result;
end;

function IntToHexStr(Value: Int64; Digits: Byte; const Prefix, Suffix: string; UseLowerCase: Boolean): string;
var
  B: Byte;
begin
  Result := '';
  if Digits <> 0 then
    Digits := MinMax(Digits, 1, 16);
  repeat
    B := Byte(Value and $F);
    Value := Value shr 4;
    Result := Char(NibbleToDigit(B, not UseLowerCase)) + Result;
  until (Value = 0) or ((Digits <> 0) and (Length(Result) = Digits));
  while Length(Result) < Digits do
    Result := '0' + Result;
  Result := Prefix + Result + Suffix;
end;

function IntToOctStr(Value: Int64): string;
var
  B: Byte;
  C: Char;
  Signum: Boolean;
begin
  if Value < 0 then
  begin
    Signum := True;
    Value := -Value;
  end else
    Signum := False;
  Result := '';
  repeat
    B := Byte(Value mod 8);
    Value := Value div 8;
    C := Chr(Ord('0') + B);
    Result := C + Result;
  until Value = 0;
  Result := '0' + Result;
  if Signum then
    Result := '-' + Result;
end;

function IntPowerInt(Value: Int64; Exponent: Integer): Int64;
begin
  Result := Value;
  while Exponent > 1 do
  begin
    Result := Result * Value;
    Dec(Exponent);
  end;
end;

function AsciiToInt(S: string; Digits: Integer): Int64;
var
  I: Integer;
begin
  Result := 0;
  I := Min(Length(S), Digits);
  while I > 0 do
  begin
    Result := Result shl 8;
    Result := Ord(S[I]) + Result;
    Dec(I);
  end;
end;

function BCDToInt(Value: Cardinal): Cardinal;
var
  Exp: Cardinal;
begin
  Result := 0;
  Exp := 1;
  while Value > 0 do
  begin
    Result := Result + Min(Value and 15, 9) * Exp;
    Value := Value shr 4;
    Exp := Exp * 10;
  end;
end;

function BinStrToInt(S: string; Digits: Byte; Signed: Boolean; var Code: Integer): Int64;
var
  I, L, Len: Integer;
  N: Byte;
  C: Char;
  M: Int64;
begin
  Result := 0;
  Code := 0;
  L := 0;
  Len := Length(S);
  if (Digits = 0) or (Digits > 64) then
    Digits := 64;
  if (Len >= 1) and CharInSetEx(S[Len], ['b', 'B']) then
  begin
    Delete(S, Len, 1);
    Dec(Len);
  end;
  I := 1;
  while I <= Len do
  begin
    C := S[I];
    N := 255;
    if (C >= '0') and (C <= '1') then N := Ord(C) - Ord('0');
    if N > 1 then
    begin
      Code := I;
      Break;
    end
    else if (N > 0) or (Result <> 0) then
    begin
      if L >= Digits then
      begin
        Code := I;
        Break;
      end;
      Result := Result shl 1;
      Inc(Result, N);
      Inc(L);
    end;
    Inc(I);
  end;
  if Signed and (Digits < 64) then
  begin
    M := Int64(1) shl Digits;
    if Result >= M shr 1 - 1 then
      Dec(Result, M);
  end;
end;

function DecStrToInt(S: string; var Code: Integer): Int64;
var
  I, Len: Integer;
  N: Byte;
  C: Char;
  Minus: Boolean;
begin
  Result := 0;
  Code := 0;
  Len := Length(S);
  Minus := S[1] = '-';
  if Minus then I := 2 else I := 1;
  while I <= Len do
  begin
    C := S[I];
    N := 255;
    if (C >= '0') and (C <= '9') then N := Ord(C) - Ord('0');
    if N > 9 then
    begin
      Code := I;
      Break;
    end
    else if (N > 0) or (Result <> 0) then
    begin
      Result := Result * 10;
      Inc(Result, N);
    end;
    Inc(I);
  end;
  if Minus then Result := -Result;
end;

function HexStrToInt(S: string; Digits: Byte; Signed: Boolean; var Code: Integer): Int64;
var
  I, L, Len: Integer;
  N: Byte;
  C: AnsiChar;
  M: Int64;
begin
  Result := 0;
  Code := 0;
  L := 0;
  Len := Length(S);
  if (Digits = 0) or (Digits > 16) then
    Digits := 16;
  if (Len >= 2) and (AnsiChar(S[1]) = '0') and CharInSetEx(S[2], ['x', 'X']) then
    I := 3
  else if (Len >= 1) and CharInSetEx(S[1], ['x', 'X', '$']) then
    I := 2
  else
    I := 1;
  while I <= Len do
  begin
    C := AnsiChar(S[I]);
    N := 255;
    DigitToNibble(C, N);
    if N > 15 then
    begin
      if CharInSetEx(C, ['h', 'H']) then
      begin
        if Len > I then Code := I + 1;
      end else
        Code := I;
      Break;
    end
    else if (N > 0) or (Result <> 0) then
    begin
      if L >= Digits then
      begin
        Code := I;
        Break;
      end;
      Result := Result shl 4;
      Inc(Result, N);
      Inc(L);
    end;
    Inc(I);
  end;
  if Signed and (Digits < 16) then
  begin
    M := Int64(1) shl (Digits shl 2);
    if Result >= M shr 1 - 1 then
      Dec(Result, M);
  end;
end;

function OctStrToInt(S: string; var Code: Integer): Int64;
var
  I, Len: Integer;
  N: Byte;
  C: Char;
  Minus: Boolean;
begin
  Result := 0;
  Code := 0;
  Len := Length(S);
  Minus := S[1] = '-';
  if Minus then I := 2 else I := 1;
  while I <= Len do
  begin
    C := S[I];
    N := 255;
    if (C >= '0') and (C <= '7') then N := Ord(C) - Ord('0');
    if N > 7 then
    begin
      Code := I;
      Break;
    end
    else if (N > 0) or (Result <> 0) then
    begin
      Result := Result * 8;
      Inc(Result, N);
    end;
    Inc(I);
  end;
  if Minus then Result := -Result;
end;

function KFormat(const Format: string; const Args: array of const; const AFormatSettings: TFormatSettings): string;
begin
  Result := SysUtils.Format(Format, Args, AFormatSettings);
end;

function KFormat(const Format: WideString; const Args: array of const; const AFormatSettings: TFormatSettings): WideString;
begin
  Result := SysUtils.WideFormat(Format, Args, AFormatSettings);
end;

function MinMax(Value, Min, Max: ShortInt): ShortInt;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: SmallInt): SmallInt;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: Integer): Integer;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: Int64): Int64;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: Single): Single;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

function MinMax(Value, Min, Max: Double): Double;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;

{$IFNDEF FPC}
function MinMax(Value, Min, Max: Extended): Extended;
begin
  if Max < Min then
    Exchange(Min, Max);
  if Value <= Max then
    if Value >= Min then
      Result := Value
    else
      Result := Min
  else
    Result := Max;
end;
{$ENDIF}

function NibbleToDigit(Nibble: Byte; UpperCase: Boolean): AnsiChar;
begin
  if Nibble < 10 then
    Result := AnsiChar(Ord('0') + Nibble)
  else if UpperCase then
    Result := AnsiChar(Ord('A') + Nibble - 10)
  else
    Result := AnsiChar(Ord('a') + Nibble - 10);
end;

procedure QuickSortNR(AData: Pointer; ACount: Integer; ACompareProc: TQsCompareProc;
  AExchangeProc: TQsExchangeProc; ASortedDown: Boolean);
type
  TStackItem = record
    LIndex, RIndex: Integer;
  end;
const
  cStackGrow = 100;
var
  Key, Left, Right, L, R, LBack, RBack, StackLen, StackPtr: Integer;
  Stack: array of TStackItem;
begin
  { this is the non recursive quick sort algorithm to avoid stack overflows.
    Right parts of divided arrays are stored into a stack-like array
    in dynamic memory for later use. }
  Left := 0;
  Right := ACount - 1;
  SetLength(Stack, cStackGrow);
  StackPtr := 0;
  with Stack[StackPtr] do begin LIndex := Left; RIndex := Right end;
  repeat
    with Stack[StackPtr] do begin Left := LIndex; Right := RIndex end;
    Dec(StackPtr);
    repeat
      L := Left;
      R := Right;
      Key := (L + R) div 2;
      LBack := Left - 1;
      RBack := Right;
      repeat
        if ASortedDown then
        begin
          while (L < Right) and (ACompareProc(AData, L, Key) < 0) do Inc(L);
          while (R > Left) and (ACompareProc(AData, R, Key) > 0) do Dec(R);
        end else
        begin
          while (L < Right) and (ACompareProc(AData, L, Key) > 0) do Inc(L);
          while (R > Left) and (ACompareProc(AData, R, Key) < 0) do Dec(R);
        end;
        if L <= R then
        begin
          if L < R then
            if (L = Key) or (R = Key) then
            begin
              // preserve Key, exchange later
              LBack := L;
              RBack := R;
            end else
              AExchangeProc(AData, L, R);
          Dec(R);
          Inc(L);
        end;
      until L >= R;
      // exchange anything with former Key
      if LBack >= Left then
        AExchangeProc(AData, LBack, RBack);
      if L < Right then
      begin
        Inc(StackPtr);
        StackLen := Length(Stack);
        if StackPtr >= StackLen then
          SetLength(Stack, StackLen + cStackGrow);
        with Stack[StackPtr] do begin LIndex := L; RIndex := Right end;
      end;
      Right := R;
    until Left >= Right;
  until StackPtr < 0;
end;

procedure QuickSort(AData: Pointer; ACount: Integer; ACompareProc: TQsCompareProc;
  AExchangeProc: TQsExchangeProc; ASortedDown: Boolean);

  procedure Sort(const Left, Right: Integer);
  var
    Key, L, R, LBack, RBack: Integer;
  begin
    Key := (Left + Right) div 2;
    L := Left;
    R := Right;
    LBack := Left - 1;
    RBack := Right;
    repeat
      if ASortedDown then
      begin
        while (L < Right) and (ACompareProc(AData, L, Key) < 0) do Inc(L);
        while (R > Left) and (ACompareProc(AData, R, Key) > 0) do Dec(R);
      end else
      begin
        while (L < Right) and (ACompareProc(AData, L, Key) > 0) do Inc(L);
        while (R > Left) and (ACompareProc(AData, R, Key) < 0) do Dec(R);
      end;
      if L <= R then
      begin
        if L < R then
          if (L = Key) or (R = Key) then
          begin
            // preserve Key, exchange later
            LBack := L;
            RBack := R;
          end else
            AExchangeProc(AData, L, R);
        Inc(L);
        Dec(R);
      end;
    until L >= R;
    // exchange anything with former Key
    if LBack >= Left then
      AExchangeProc(AData, LBack, RBack);
    if Left < R  then Sort(Left, R);
    if L < Right then Sort(L, Right);
  end;

begin
  if ACount > 1 then
    Sort(0, ACount - 1);
end;

procedure OffsetPoint(var APoint: TPoint; AX, AY: Integer);
begin
  Inc(APoint.X, AX);
  Inc(APoint.Y, AY);
end;

procedure OffsetPoint(var APoint: TPoint; const AOffset: TPoint);
begin
  Inc(APoint.X, AOffset.X);
  Inc(APoint.Y, AOffset.Y);
end;

function RectInRect(Bounds, Rect: TRect): Boolean;
begin
  Result :=
    (Rect.Left < Bounds.Right) and (Rect.Right >= Bounds.Left) and
    (Rect.Top < Bounds.Bottom) and (Rect.Bottom >= Bounds.Top);
end;

procedure OffsetRect(var ARect: TRect; AX, AY: Integer);
begin
  Inc(ARect.Left, AX);
  Inc(ARect.Top, AY);
  Inc(ARect.Right, AX);
  Inc(ARect.Bottom, AY);
end;

procedure OffsetRect(var ARect: TRect; const AOffset: TPoint);
begin
  Inc(ARect.Left, AOffset.X);
  Inc(ARect.Top, AOffset.Y);
  Inc(ARect.Right, AOffset.X);
  Inc(ARect.Bottom, AOffset.Y);
end;

function SetAppContext(const Ctx: TKAppContext): Boolean;
begin
  Forms.Application := Ctx.Application;
  Forms.Screen := Ctx.Screen;
  Classes.GlobalNameSpace := Ctx.GlobalNameSpace;
{$IFDEF FPC}
//  Classes.IntConstList := Ctx.IntConstList;
  Classes.MainThreadID := Ctx.MainThreadID;
  Controls.DragManager := Ctx.DragManager;
  InterfaceBase.WidgetSet := Ctx.WidgetSet;
{$ENDIF}
  Result := True;
end;

procedure SetControlClipRect(AControl: TWinControl; const ARect: TRect);
begin
  if AControl.HandleAllocated then
  begin
  {$IFDEF USE_WINAPI}
    SetWindowRgn(AControl.Handle, CreateRectRgn(0, 0, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top), True);
  {$ELSE}
    //how to do that?
  {$ENDIF}
  end;
end;

procedure SetControlText(Value: TWinControl; const Text: TKString);

  procedure SetTextBuffer(Value: TWinControl; const Text: string);
  begin
    Value.SetTextBuf(PChar(Text));
  end;

begin
{$IFDEF FPC}
  SetTextBuffer(Value, Text); // conversion to UTF8 forced anyway
{$ELSE}
 {$IFDEF STRING_IS_UNICODE}
  SetTextBuffer(Value, Text);
 {$ELSE}
  if Value.HandleAllocated and (Win32Platform = VER_PLATFORM_WIN32_NT) then // unicode fully supported
    SetWindowTextW(Value.Handle, PWideChar(Text))
  else
    SetTextBuffer(Value, Text);
 {$ENDIF}
{$ENDIF}
end;

procedure StripLastPathSlash(var APath: string);
begin
  if APath <> '' then
    if CharInSetEx(APath[Length(APath)], ['\', '/']) then Delete(APath, Length(APath), 1);
end;

function StrNextCharIndex(const AText: TKString; Index: Integer): Integer;
begin
{$IFDEF FPC}
  Result := Index + UTF8CharacterLength(@AText[Index]);
{$ELSE}
  Result := Index + 1; // neglecting surrogate pairs
{$ENDIF}
end;

function StrPreviousCharIndex(const AText: TKString; Index: Integer): Integer;
begin
{$IFDEF FPC}
  Result := Index - UTF8CharacterLength(@AText[StringCharBegin(AText, Index - 1)]);
{$ELSE}
  Result := Index - 1; // neglecting surrogate pairs
{$ENDIF}
end;

function StringCharBegin(const AText: TKString; Index: Integer): Integer;
begin
{$IFDEF FPC}
  Result := UTF8CharToByteIndex(PChar(AText), Length(AText), Index)
{$ELSE}
  Result := Index // neglecting surrogate pairs
{$ENDIF}
end;

function StringLength(const AText: TKString): Integer;
begin
{$IFDEF FPC}
  Result := UTF8Length(AText)
{$ELSE}
  Result := Length(AText) // neglecting surrogate pairs
{$ENDIF}
end;

function StringNextCharIndex(const AText: TKString; Index: Integer): Integer;
begin
{$IFDEF FPC}
  Result := Index + UTF8CharacterLength(@AText[Index]);
{$ELSE}
  Result := Index + 1; // neglecting surrogate pairs
{$ENDIF}
end;

function StringCopy(const ASource: TKString; At, Count: Integer): TKString;
begin
{$IFDEF FPC}
  Result := UTF8Copy(ASource, At, Count);
{$ELSE}
  Result := Copy(ASource, At, Count);
{$ENDIF}
end;

procedure StringDelete(var ASource: TKString; At, Count: Integer);
begin
{$IFDEF FPC}
  UTF8Delete(ASource, At, Count);
{$ELSE}
  Delete(ASource, At, Count);
{$ENDIF}
end;

procedure TrimWhiteSpaces(const AText: TKString; var AStart, ALen: Integer; const ASet: TKSysCharSet);
begin
  while (ALen > 0) and CharInSetEx(AText[AStart], ASet) do
  begin
    Inc(AStart);
    Dec(ALen);
  end;
  while (ALen > 0) and CharInSetEx(AText[AStart + ALen - 1], ASet) do
    Dec(ALen);
end;

procedure TrimWhiteSpaces(var AText: TKString; const ASet: TKSysCharSet);
begin
  while (Length(AText) > 0) and CharInSetEx(AText[1], ASet) do
    Delete(AText, 1, 1);
  while (Length(AText) > 0) and CharInSetEx(AText[Length(AText)], ASet) do
    Delete(AText, Length(AText), 1);
end;

{$IFNDEF FPC}
procedure TrimWhiteSpaces(var AText: AnsiString; const ASet: TKSysCharSet);
begin
  while (Length(AText) > 0) and CharInSetEx(AText[1], ASet) do
    Delete(AText, 1, 1);
  while (Length(AText) > 0) and CharInSetEx(AText[Length(AText)], ASet) do
    Delete(AText, Length(AText), 1);
end;
{$ENDIF}

function StringToAnsiString(const AText: TKString; CodePage: Cardinal): AnsiString;
var
{$IFDEF FPC}
  CP: string;
{$ELSE}
  Len: Integer;
  W: WideString;
  DefaultChar: AnsiChar;
{$ENDIF}
begin
{$IFDEF FPC}
  if CodePage = 0 then
    CP := 'ansi'
  else
    CP := Format('cp%d', [Codepage]);
  Result := LConvEncoding.ConvertEncoding(AText, 'utf8', CP);
{$ELSE}
  DefaultChar := #0;
  W := WideString(AText);
  Len := WideCharToMultiByte(CodePage, 0, PWideChar(W), -1, nil, 0, @DefaultChar, nil);
  SetLength(Result, Len - 1);
  WideCharToMultiByte(CodePage, 0, PWideChar(W), -1, PAnsiChar(Result), Len, @DefaultChar, nil);
{$ENDIF}
end;

{$IFDEF USE_WINAPI}
function GetWindowsFolder(CSIDL: Cardinal; var APath: string): Boolean;
type
  TSHGetFolderPathProc = function(hWnd: HWND; CSIDL: Integer; hToken: THandle;
    dwFlags: DWORD; pszPath: PAnsiChar): HResult; stdcall;
var
  SHFolderHandle: HMODULE;
  SHGetFolderPathProc: TSHGetFolderPathProc;
  Buffer: PAnsiChar;
begin
  Result := False;
  APath := '';
  SHFolderHandle := GetModuleHandle(SHFolderDll);
  if SHFolderHandle <> 0 then
  begin
    SHGetFolderPathProc := GetProcAddress(SHFolderHandle, 'SHGetFolderPathA');
    if Assigned(SHGetFolderPathProc) then
    begin
      GetMem(Buffer, MAX_PATH);
      try
        if Succeeded(SHGetFolderPathProc(0, CSIDL, 0, 0, Buffer)) then
        begin
          APath := string(Buffer);
          Result := True;
        end
      finally
        FreeMem(Buffer);
      end;
    end;
  end;
end;

function RunExecutable(const AFileName: string; AWaitForIt: Boolean): DWORD;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ErrMsg: PChar;
begin
  Result := STILL_ACTIVE;
  GetStartupInfo(StartupInfo);
  if CreateProcess(nil, PChar(AFileName), nil, nil, IsConsole,
    NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
  begin
    try
      if (not AWaitForIt) or (WaitForSingleObject(ProcessInfo.hProcess,INFINITE) = WAIT_OBJECT_0) then
        GetExitCodeProcess(ProcessInfo.hProcess, Result);
    finally
      CloseHandle(ProcessInfo.hThread);
      CloseHandle(ProcessInfo.hProcess);
    end;
  end else
  begin
    if FormatMessage(Format_Message_Allocate_Buffer or Format_Message_From_System, nil,
      GetLastError, 0, @errMsg, 0, nil) <> 0 then
    begin
      try
        raise Exception.Create ('CreateProcess failed with error "' + String (errMsg) + '".');
      finally
        LocalFree (HLOCAL(errMsg));
      end;
    end;
  end;
end;
{$ENDIF}

function SystemCodePage: Integer;
begin
{$IFDEF USE_WINAPI}
  Result := getACP;
{$ELSE}
 {$IF DEFINED(UNIX) and (FPC_FULLVERSION>=20701)}
  Result := GetSystemCodepage;
 {$ELSE}
  Result := 0;
 {$IFEND}
{$ENDIF}
end;

function UnicodeUpperCase(const AText: TKString): TKString;
begin
{$IFDEF FPC}
  Result := LCLProc.UTF8UpperCase(AText);
{$ELSE}
 {$IFDEF STRING_IS_UNICODE}
  Result := AnsiUpperCase(AText);
 {$ELSE}
  Result := WideUpperCase(AText);
 {$ENDIF}
{$ENDIF} 
end;

function UnicodeLowerCase(const AText: TKString): TKString;
begin
{$IFDEF FPC}
  Result := LCLProc.UTF8LowerCase(AText);
{$ELSE}
 {$IFDEF STRING_IS_UNICODE}
  Result := AnsiLowerCase(AText);
 {$ELSE}
  Result := WideLowerCase(AText);
 {$ENDIF}
{$ENDIF}
end;

function UnicodeToNativeUTF(const AParam: WideChar): TKString;
begin
{$IFDEF FPC}
  Result := UnicodeToUTF8(Cardinal(AParam));
{$ELSE}
  Result := AParam;
{$ENDIF}
end;

function UnicodeStringReplace(const AText, AOldPattern, ANewPattern: TKString;
  AFlags: TReplaceFlags): TKString;
var
  SearchStr, Pattern, Candidate: TKString;
  I, NewI, PatternLen, SearchLen: Integer;
  DoInc, Found: Boolean;
begin
  Result := '';
  if rfIgnoreCase in AFlags then
  begin
    SearchStr := UnicodeUpperCase(AText);
    Pattern := UnicodeUpperCase(AOldPattern);
  end else
  begin
    SearchStr := AText;
    Pattern := AOldPattern;
  end;
  PatternLen := Length(Pattern);
  SearchLen := Length(SearchStr);
  Found := False;
  I := 1;
  while (I <= SearchLen) do
  begin
    DoInc := True;
    if (rfReplaceAll in AFlags) or not Found then
    begin
      if SearchStr[I] = Pattern[1] then
      begin
        Candidate := Copy(SearchStr, I, PatternLen);
        if Candidate = Pattern then
        begin
          Result := Result + ANewPattern;
          Inc(I, PatternLen);
          DoInc := False;
          Found := True;
        end;
      end;
    end;
    if DoInc then
    begin
      NewI := StrNextCharIndex(SearchStr, I);
      Result := Result + Copy(SearchStr, I, NewI - I);
      I := NewI;
    end;
  end;
end;

end.
