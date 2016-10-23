unit KExtras;

{$include KControls.inc}
{.$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Classes, Controls, ComCtrls, Graphics, StdCtrls, Forms, Registry, KFunctions, KGraphics, KLog;

resourcestring
  sBadCIdentifier = 'The invalid C language identifier "%s" has been replaced with "%s".';

const
  IdStartCharSet = ['_', 'a'..'z', 'A'..'Z'];
  IdCharSet = ['0'..'9'] + IdStartCharSet;

type
  T_UW_B = record
    case Word of
      1: (B0, B1: Byte);
      2: (W: Word);
  end;

  T_UL_BW = record
    case LongWord of
      1: (B0, B1, B2, B3: Byte);
      2: (W0, W1: Word);
      3: (L: LongWord);
  end;

  TKHexDigits = (hd1, hd2, hd4, hd8, hd16);

  TKColorConversionMode = (ccmCut, ccmDither, ccmAuto);

  PKColorRecArray = ^TKColorRecArray;
  TKColorRecArray = array[0..0] of TKColorRec;

  PKPaletteEntry = ^TKPaletteEntry;
  TKPaletteEntry = packed record
    B, G, R, Alpha: Byte;
  end;

  PKPaletteEntryArray = ^TKPaletteEntryArray;
  TKPaletteEntryArray = array[0..0] of TKPaletteEntry;

  PKPaletteEntries = ^TKPaletteEntries;
  TKPaletteEntries = array of TKPaletteEntry;

  TKDoubleBuffer = class(TObject)
  private
    FBitmap: HBITMAP;
    FClipRect: TRect;
    FClipRgn: HRGN;
    FSavedBitmap: HBITMAP;
    FMemoryDC: HDC;
  public
    { Creates a memory device context for double buffering }
    constructor Create(DC: HDC; Width, Height: Integer);
    { Destroys a memory device context }
    destructor Destroy; override;
    { Copies the content of the memory device context to the given device context }
    procedure PaintTo(DC: HDC);
    { Provides read only acccess to the memory device context }
    property MemoryDC: HDC read FMemoryDC;
  end;

function CheckCIdentifier(const S: string): Boolean;
function CorrectCIdentifier(var S: string; Log: TKLog): Boolean;

{searches S for integral numbers. The Pos-th number is returned in Num.
Returns True if found, False if not. Recognizes decimal and hexadecimal
integer representations}
function FindInteger(const S: string; Pos: Integer; out Num: Int64): Boolean;

procedure ReadListFromSepStr(const SepStr: string; const Separators: array of Char; List: TStrings);
procedure WriteListToSepStr(var SepStr: string; Separator: Char; List: TStrings);

{the same as TWinControl.FindNextControl. WHY declared as protected ???}
function FindNextControl(ParentControl, CurControl: TWinControl;
  GoForward, CheckTabStop, CheckParent: Boolean): TWinControl;
function GetFocusedChild(ParentControl: TWinControl; Recurse: Boolean): TWinControl;

{color list functions}
function GetColors: TStrings;
function GetColorList: TStringList;
procedure SetColors(Value: TStrings);
procedure LoadColorsFromRegistry(Reg: TRegistryIniFile; const Key: string);
procedure SaveColorsToRegistry(Reg: TRegistryIniFile; const Key: string);

{check sum functions}
function CalcCheckSum(var Buffer; Count: Integer): Byte;
function VerifyCheckSum(var Buffer; Count: Integer; Value: Byte): Boolean;

function InstanceIsRunning(const AppName: string): Boolean;

procedure EnableWindows(AExceptWnd: HWND; AExceptAppHandle, AEnable: Boolean);

function RegReadRect(Reg: TRegistryIniFile;
  const Section, Ident: String; const Default: TRect): TRect;
procedure RegWriteRect(Reg: TRegistryIniFile;
  const Section, Ident: String; const Value: TRect);
function RegReadWindowPlacement(Reg: TRegistryIniFile; const Section, Ident: string;
  DefWindowState: TWindowState; DefBoundsRect: TRect): TWindowPlacement;
procedure RegWriteWindowPlacement(Reg: TRegistryIniFile; const Section, Ident: string;
  P: TWindowPlacement);
procedure RegReadFormState(Reg: TRegistryIniFile; F: TForm; const Section, Ident: string);
procedure RegWriteFormState(Reg: TRegistryIniFile; F: TForm; const Section, Ident: string);

{ TTreeView management }
procedure TVMoveSelUp(TV: TTreeView; CrossFolders: Boolean);
procedure TVMoveSelDown(TV: TTreeView; CrossFolders: Boolean);
function AddNode(TV: TTreeView; const Caption: string): TTreeNode;
function AddChildNode(TV: TTreeView; const Caption: string): TTreeNode;

{ TListView management }
procedure ListItemMove(LV: TListView; FromIndex, ToIndex: Integer);

{ TListBox management }
procedure LBSetHorzScroll(LB: TListBox);

function PosChars(const Text: string; Chars: TKSysCharSet): Boolean;

procedure StripDelimiters(var Text: string; const Delimiters: TKSysCharSet);

//search for AFileName in ARoot recursively and if found return full qualified path
function FindPath(const ARoot: string; const AFileName: string): string;

//fill List with all file/directory names found in Path and corresponding to Attr
function BuildFileList(const Path: string; const Attr: Integer; const List: TStrings): Boolean;

//delete all files and subdirectories found in Path recursively
function DelTree(const Path: string): Boolean;
function DelTreeEx(const Path: string; AbortOnFailure: Boolean): Boolean;

{ color count reduction with Floyd Steinberg dithering }
procedure MedianCutQuantize(Bitmap: TBitmap; NewBpp: TPixelFormat; Mode: TKColorConversionMode);

function StreamReadString(Stream: TStream): string;
function StreamReadStringW(Stream: TStream): WideString;

procedure StreamWriteString(Stream: TStream; const S: string);
procedure StreamWriteStringW(Stream: TStream; const S: WideString);

{ date and time routines }
function DateToMinutes(DT: TDateTime): Cardinal;
function MinutesToDate(I: Integer): TDateTime;

{ misc }
procedure DataToString(Buffer: Pointer; Size: Integer; var S: string);
procedure StringToData(const S: string; Buffer: Pointer; Size: Integer);

procedure AddStringsToList(L: TStrings; const Strings: array of string);
procedure PrintLines(S: TStrings);

implementation

uses
  DateUtils, Math, Messages, Printers, SysUtils;

const
  ColorList: TStringList = nil;

  InstMutex: THandle = 0;
  InstExists: Boolean = False;

{ TDoubleBuffer }

constructor TKDoubleBuffer.Create(DC: HDC; Width, Height: Integer);
begin
  FMemoryDC := CreateCompatibleDC(DC);
  FBitmap := CreateCompatibleBitmap(DC, Width, Height);
  FSavedBitmap := SelectObject(FMemoryDC, FBitmap);
  if DC <> 0 then
  begin
    GetClipBox(DC, FClipRect);
    FClipRgn := CreateRectRgnIndirect(FClipRect);
    SelectClipRgn(FMemoryDC, FClipRgn);
  end;
end;

destructor TKDoubleBuffer.Destroy;
begin
  SelectObject(FMemoryDC, FSavedBitmap);
  DeleteObject(FClipRgn);
  DeleteObject(FBitmap);
  DeleteDC(FMemoryDC);
  inherited;
end;

procedure TKDoubleBuffer.PaintTo(DC: HDC);
begin
  with FClipRect do
    BitBlt(DC, Left, Top, Right - Left, Bottom - Top, FMemoryDC, Left, Top, SRCCOPY);
end;

  
function CheckCIdentifier(const S: string): Boolean;
var
  I: Integer;
begin
  if Length(S) > 0 then
  begin
    Result := True;
    if not CharInSetEx(S[1], IdStartCharSet) then Result := False;
    for I := 2 to Length(S) do
      if not CharInSetEx(S[I], IdCharSet) then
      begin
        Result := False;
        Exit;
      end;
  end else
  Result := False;
end;

function CorrectCIdentifier(var S: string; Log: TKLog): Boolean;
var
  I, Len: Integer;
  T: string;
begin
  Result := False;
  T := S;
  if Length(S) > 0 then
  begin
    if not CharInSetEx(S[1], IdStartCharSet) then
    begin
      S[1] := '_';
      Result := True;
    end;
    I := 2;
    Len := Length(S);
    while (I <= Len) do
    begin
      if not CharInSetEx(S[I], IdCharSet) then
      begin
        Delete(S, I, 1);
        Dec(Len);
        Dec(I);
        Result := True;
      end;
      Inc(I);
    end;
  end else
  begin
    S := '_';
    Result := True;
  end;
  if Result and Assigned(Log) then
    Log.Log(lgInputError, Format(sBadCIdentifier, [T, S]));
end;

function FindInteger(const S: string; Pos: Integer; out Num: Int64): Boolean;
var
  I, J, P, Code: Integer;
  T: string;
begin
  P := 0;
  for I := 1 to Length(S) do
  begin
    if CharInSetEx(S[I], ['0'..'9']) then
    begin
      J := I;
      while (J <= Length(S)) and not CharInSetEx(S[J], [#9, #32]) do Inc(J);
      T := Copy(S, I, J - I);
      Val(T, Num, Code);
      if Code = 0 then
      begin
        Inc(P);
        if P = Pos then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
  Result := False;
end;

procedure ReadListFromSepStr(const SepStr: string; const Separators: array of Char; List: TStrings);
  function IsSeparator(C: Char): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to Length(Separators) - 1 do
      if C = Separators[I] then
        begin Result := True; Exit end;
    Result := False;
  end;
var
  S: string;
  I, J: Integer;
begin
  S := SepStr + Separators[0];
  I := 1;
  J := 0;
  while (I <= Length(S)) do
  begin
    if IsSeparator(S[I]) then
    begin
      if J > 0 then
      begin
        List.Add(Copy(S, J, I - J));
        J := 0;
      end;
    end
    else if J = 0 then
      J := I;
    Inc(I);
  end;
end;

procedure WriteListToSepStr(var SepStr: string; Separator: Char; List: TStrings);
var
  I: Integer;
begin
  SepStr := '';
  if (List <> nil) and (List.Count > 0) then
  begin
    for I := 0 to List.Count - 2 do
      SepStr := SepStr + List[I] + Separator;
    SepStr := SepStr + List[List.Count - 1];
  end;
end;

function FindNextControl(ParentControl, CurControl: TWinControl;
  GoForward, CheckTabStop, CheckParent: Boolean): TWinControl;
var
  I, StartIndex: Integer;
  List: TList;
begin
 Result := nil;
 if ParentControl <> nil then with ParentControl do
 begin
  Result := nil;
  List := TList.Create;
  try
    GetTabOrderList(List);
    if List.Count > 0 then
    begin
      StartIndex := List.IndexOf(CurControl);
      if StartIndex = -1 then
        if GoForward then StartIndex := List.Count - 1 else StartIndex := 0;
      I := StartIndex;
      repeat
        if GoForward then
        begin
          Inc(I);
          if I = List.Count then I := 0;
        end else
        begin
          if I = 0 then I := List.Count;
          Dec(I);
        end;
        CurControl := List[I];
        if CurControl.CanFocus and
          (not CheckTabStop or CurControl.TabStop) and
          (not CheckParent or (CurControl.Parent = ParentControl)) then
          Result := CurControl;
      until (Result <> nil) or (I = StartIndex);
    end;
  finally
    List.Free;
  end;
 end;
end;

function GetFocusedChild(ParentControl: TWinControl; Recurse: Boolean): TWinControl;
var
  H: HWnd;

  procedure Get(W: TWinControl);
  var
    C: TControl;
    I: Integer;
  begin
    if W <> nil then
      for I := 0 to W.ControlCount - 1 do
      begin
        C := W.Controls[I];
        if (C is TWinControl) then
          if (TWinControl(C).Handle = H) then
            Result := TWinControl(C)
          else
            if Recurse then
              Get(TWinControl(C));
        if Result <> nil then Exit;
      end;
  end;

begin
  Result := nil;
  H := GetFocus;
  Get(ParentControl);
end;

function GetColors: TStrings;
begin
  if ColorList = nil then
    ColorList := TStringList.Create;
  Result := ColorList;
end;

function GetColorList: TStringList;
begin
  if ColorList = nil then
    ColorList := TStringList.Create;
  Result := ColorList;
end;

procedure SetColors(Value: TStrings);
begin
  if ColorList = nil then
    ColorList := TStringList.Create;
  try
    ColorList.Assign(Value);
  except
  end;
end;

procedure LoadColorsFromRegistry(Reg: TRegistryIniFile; const Key: string);
var
  I: Integer;
  S: string;
begin
  if ColorList = nil then
    ColorList := TStringList.Create;
  if ColorList <> nil then
  begin
    ColorList.Clear;
    for I := 0 to 15 do
    begin
      S := Reg.ReadString(Key, Format('Color%d', [I]), '');
      if S <> '' then ColorList.Add(S);
    end;
  end;
end;

procedure SaveColorsToRegistry(Reg: TRegistryIniFile; const Key: string);
var
  I: Integer;
begin
  if ColorList <> nil then
  begin
    for I := 0 to ColorList.Count - 1 do
      Reg.WriteString(Key, Format('Color%d', [I]), ColorList[I]);
  end;
end;

function RegReadRect(Reg: TRegistryIniFile;
  const Section, Ident: String; const Default: TRect): TRect;
begin
  with Result do
  begin
    Left := Reg.ReadInteger(Section, Ident + '_Left', Default.Left);
    Top := Reg.ReadInteger(Section, Ident + '_Top', Default.Top);
    Right := Reg.ReadInteger(Section, Ident + '_Right', Default.Right);
    Bottom := Reg.ReadInteger(Section, Ident + '_Bottom', Default.Bottom);
  end;
end;

procedure RegWriteRect(Reg: TRegistryIniFile;
  const Section, Ident: String; const Value: TRect);
begin
  with Value do
  begin
    Reg.WriteInteger(Section, Ident + '_Left', Left);
    Reg.WriteInteger(Section, Ident + '_Top', Top);
    Reg.WriteInteger(Section, Ident + '_Right', Right);
    Reg.WriteInteger(Section, Ident + '_Bottom', Bottom);
  end;
end;

function RegReadWindowPlacement(Reg: TRegistryIniFile; const Section, Ident: string;
  DefWindowState: TWindowState; DefBoundsRect: TRect): TWindowPlacement;
const
  ShowCommands: array[TWindowState] of Integer =
    (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED);
begin
  Result.length := SizeOf(TWindowPlacement);
  Result.flags := WPF_SETMINPOSITION;
  Result.showCmd := Reg.ReadInteger(Section, Ident + '_State', ShowCommands[DefWindowState]);
  Result.ptMinPosition.x := Reg.ReadInteger(Section, Ident + '_MinLeft', 0);
  Result.ptMinPosition.y := Reg.ReadInteger(Section, Ident + '_MinTop', Screen.Height - GetSystemMetrics(SM_CYMINIMIZED));
  Result.ptMaxPosition.x := Reg.ReadInteger(Section, Ident + '_MaxLeft', 0);
  Result.ptMaxPosition.y := Reg.ReadInteger(Section, Ident + '_MaxTop', 0);
  Result.rcNormalPosition := RegReadRect(Reg, Section, Ident, DefBoundsRect);
end;

procedure RegWriteWindowPlacement(Reg: TRegistryIniFile; const Section, Ident: string;
  P: TWindowPlacement);
begin
  P.length := SizeOf(TWindowPlacement);
  Reg.WriteInteger(Section, Ident + '_State', P.showCmd);
  Reg.WriteInteger(Section, Ident + '_MinLeft', P.ptMinPosition.x);
  Reg.WriteInteger(Section, Ident + '_MinTop', P.ptMinPosition.y);
  Reg.WriteInteger(Section, Ident + '_MaxLeft', P.ptMaxPosition.x);
  Reg.WriteInteger(Section, Ident + '_MaxTop', P.ptMaxPosition.y);
  RegWriteRect(Reg, Section, Ident, P.rcNormalPosition);
end;

procedure RegReadFormState(Reg: TRegistryIniFile; F: TForm; const Section, Ident: string);
var
  Placement: TWindowPlacement;
begin
  if F <> nil then
  begin
    Placement := RegReadWindowPlacement(Reg, Section, Ident, F.WindowState, F.BoundsRect);
    SetWindowPlacement(F.Handle, @Placement);
  end;
end;

procedure RegWriteFormState(Reg: TRegistryIniFile; F: TForm; const Section, Ident: string);
var
  Placement: TWindowPlacement;
begin
  if F <> nil then
  begin
    GetWindowPlacement(F.Handle, @Placement);
    RegWriteWindowPlacement(Reg, Section, Ident, Placement);
  end;
end;


type
  ByteArray = array[1..2*1024*1024] of Byte;

function CalcCheckSum(var Buffer; Count: Integer): Byte;
var
  I: Integer;
  Sum: Byte;
begin
  Sum := 0;
  for I := 1 to Count do
    Inc(Sum, ByteArray(Buffer)[I]);
  Result := not Sum;
end;

function VerifyCheckSum(var Buffer; Count: Integer; Value: Byte): Boolean;
var
  I: Integer;
  Sum: Byte;
begin
  Sum := 0;
  for I := 1 to Count do
    Inc(Sum, ByteArray(Buffer)[I]);
  Result := Value = not Sum;
end;

function InstanceIsRunning(const AppName: string): Boolean;
begin
  InstMutex := CreateMutex(nil, True, PChar(AppName));
  InstExists := GetLastError = ERROR_ALREADY_EXISTS;
  Result := InstExists;
end;

procedure EnableWindows(AExceptWnd: HWND; AExceptAppHandle, AEnable: Boolean);
type
  TEnumRec = record
    ExceptWnd: HWND;
    AppHandle: HWND;
    Enable: Boolean;
  end;
  PEnumRec = ^TEnumRec;

  function EnumThreadWndProc(Wnd: HWND; Param: LPARAM): Boolean; stdcall;
  begin
    if IsWindowVisible(Wnd) and (Wnd <> PEnumRec(Param)^.ExceptWnd) and (Wnd <> PEnumRec(Param)^.AppHandle) then
      EnableWindow(Wnd, PEnumRec(Param)^.Enable);
    Result := True;
  end;

var
  EnumRec: TEnumRec;
begin
  EnumRec.ExceptWnd := AExceptWnd;
  if AExceptAppHandle then EnumRec.AppHandle := Application.Handle else EnumRec.AppHandle := 0;
  EnumRec.Enable := AEnable;
  EnumThreadWindows(MainThreadID, @EnumThreadWndProc, Integer(@EnumRec));
end;

procedure TVMoveSelUp(TV: TTreeView; CrossFolders: Boolean);
var
  N: TTreeNode;
begin
  N := TV.Selected.GetPrevSibling;
  if N <> nil then
    TV.Selected.MoveTo(N, naInsert)
  else if CrossFolders then
  begin
    N := TV.Selected.Parent.GetPrevSibling;
    if N <> nil then
      if N.Data = nil then
         TV.Selected.MoveTo(N, naAddChild);
  end;
end;

procedure TVMoveSelDown(TV: TTreeView; CrossFolders: Boolean);
var
  N: TTreeNode;
begin
  N := TV.Selected.GetNextSibling;
  if N <> nil then
  begin
    if N.GetNextSibling <> nil then
      TV.Selected.MoveTo(N.GetNextSibling, naInsert)
    else
      TV.Selected.MoveTo(N, naAdd);
  end else if CrossFolders then
  begin
    N := TV.Selected.Parent.GetNextSibling;
    if N <> nil then
      if N.Data = nil then
         TV.Selected.MoveTo(N, naAddChildFirst);
  end;
end;

function AddNode(TV: TTreeView; const Caption: string): TTreeNode;
begin
  Result := nil;
  if TV.Selected = nil then Exit;
  if TV.Selected.GetNextSibling <> nil then
    Result := TV.Items.Insert(TV.Selected.GetNextSibling, Caption)
  else
    Result := TV.Items.Add(TV.Selected, Caption);
end;

function AddChildNode(TV: TTreeView; const Caption: string): TTreeNode;
begin
  Result := nil;
  if TV.Selected = nil then Exit;
  Result := TV.Items.AddChildFirst(TV.Selected, Caption);
end;

procedure ListItemMove(LV: TListView; FromIndex, ToIndex: Integer); overload;
var
  LI1, LI2: TListItem;
begin
  if (LV <> nil) and (FromIndex <> ToIndex) and
    (FromIndex >= 0) and (FromIndex < LV.Items.Count) and
    (ToIndex >= 0) and (ToIndex < LV.Items.Count) then
  begin
    LI1 := LV.Items[FromIndex];
    if ToIndex > FromIndex then Inc(ToIndex) else Inc(FromIndex);
    LI2 := LV.Items.Insert(ToIndex);
    LI2.Assign(LI1);
    LV.Items.Delete(FromIndex);
    LI2.Selected := True;
  end;
end;

procedure LBSetHorzScroll(LB: TListBox);
var
  I, J, K: Integer;
begin
  if LB <> nil then with LB do
  begin
    K := 0;
    for I := 0 to LB.Count - 1 do
    begin
      J := Canvas.TextWidth(LB.Items[I]);
      if J > K then K := J;
    end;
    Perform(LB_SETHORIZONTALEXTENT, K + 10, 0);
  end;
end;

function PosChars(const Text: string; Chars: TKSysCharSet): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(Text) do
    if CharInSetEx(Text[I], Chars) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure StripDelimiters(var Text: string; const Delimiters: TKSysCharSet);
var
  I, L: Integer;
begin
  L := Length(Text);
  if L > 0 then
  begin
    I := 1;
    while (I <= L) and CharInSetEx(Text[I], Delimiters) do Inc(I);
    Delete(Text, 1, I - 1);
    L := Length(Text);
    I := L;
    while (I >= 1) and CharInSetEx(Text[I], Delimiters) do Dec(I);
    SetLength(Text, I);
  end;
end;

{based on some public domain code}
procedure MedianCutQuantize(Bitmap: TBitmap; NewBpp: TPixelFormat;
  Mode: TKColorConversionMode);
type
  PThreeInt = ^TThreeInt;
  TThreeInt = record
    R, G, B:Integer;
  end;

  PLineErrors = ^TLineErrors;
  TLineErrors = array [-1..-1] of TThreeInt;

  PCube = ^TCube;
  TCube = record
    X1, Y1, Z1,
    X2, Y2, Z2: Byte;
  end;

  PCubes = ^TCubes;
  TCubes = array of TCube;

  PColorSpace = ^TColorSpace;
  TColorSpace = array[0..31, 0..31, 0..31] of Byte;

var
  Space: PColorSpace;

  // trims the color cube so that no empty color space
  // is left around that cube
  procedure OptimizeCube(Cb:PCube);
  var
    A, B, C, D, E, F, G, H, I: Integer;
  begin
    A := 32; B := 32; C := 32;
    D := -1; E := -1; F := -1;
    for G := Cb.X1 to Cb.X2 do
      for H := Cb.Y1 to Cb.Y2 do
        for I := Cb.Z1 to Cb.Z2 do
          if Space[G, H, I] > 0 then
          begin
            if G < A then A := G;
            if G > D then D := G;
            if H < B then B := H;
            if H > E then E := H;
            if I < C then C := I;
            if I > F then F := I;
          end;
    Cb.X1 := A; Cb.Y1 := B; Cb.Z1 := C;
    Cb.X2 := D; Cb.Y2 := E; Cb.Z2 := F;
  end;

var
  A, B, C, ClrNum, ClrUsed, D, E, F, I, Ln, Nxt: Integer;
  IsPalette: Boolean;
  Color: TKPaletteEntry;
  PCA: PKColorRecArray;
  PCE: PKColorRec;
  PPE: PKPaletteEntry;
  Cub: PCube;
  PTI: PThreeInt;
  Dir: ShortInt;
  BM: TBitmap;
  R: TRect;
  DC: HDC;
  LogPal: PLogPalette;
  Cubes: TCubes;
  Palette: TKPaletteEntries;
  Lines: array[0..1] of PLineErrors;
begin
  if Bitmap.PixelFormat = NewBpp then
    Exit;
  case NewBpp of
    pf1bit: ClrNum := 2;
    pf4bit: ClrNum := 16;
    pf8bit: ClrNum := 256;
    pf15bit, pf16bit: ClrNum := 32768; // not actually needed
  else
    Exit;
  end;
  IsPalette := ClrNum <= 256;
  if IsPalette then
  begin
    GetMem(Space, SizeOf(TColorSpace));
    SetLength(Cubes, ClrNum);
    SetLength(Palette, ClrNum);
  end;
  BM := TBitmap.Create;
  try
    R := Rect(0, 0, Bitmap.Width, Bitmap.Height);
    BM.Width := Bitmap.Width;
    BM.Height := Bitmap.Height;
    BM.PixelFormat := pf32bit;
    BM.Canvas.CopyRect(R, Bitmap.Canvas, R);
    Bitmap.Width := 0;
    Bitmap.PixelFormat := NewBpp;
    if IsPalette then
    begin
      ZeroMemory(Space, SizeOf(TColorSpace));
      ZeroMemory(@Palette[0], ClrNum * SizeOf(TPaletteEntry));
      PCA := PKColorRecArray(BM.ScanLine[BM.Height - 1]); // always bottom up DIB
      // fill 3D color space
      for I := 0 to BM.Width * BM.Height - 1 do
      begin
        PCE := @PCA[I];
        Space[PCE.r shr 3, PCE.g shr 3, PCE.b shr 3] := 1;
      end;
      // Fill and trim the starting (biggest) cube
      Cubes[0].X1 := 0;
      Cubes[0].Y1 := 0;
      Cubes[0].Z1 := 0;
      Cubes[0].X2 := 31;
      Cubes[0].Y2 := 31;
      Cubes[0].Z2 := 31;
      OptimizeCube(@Cubes[0]);

      // search for biggest cube, divide that cube by largest side
      // until there are 255 cubes
      ClrUsed := 0;
      D := 0;
      F := 0;
      repeat
        A := 0;
        for B := 0 to ClrUsed do
        begin
          Cub := @Cubes[B];
          if Cub.X2 - Cub.X1 > A then
          begin
            A := Cub.X2 - Cub.X1;
            D := 0;
            F := B;
          end;
          if Cub.Y2 - Cub.Y1 > A then
          begin
            A := Cub.Y2 - Cub.Y1;
            D := 1;
            F := B;
          end;
          if Cub.Z2 - Cub.Z1 > A then
          begin
            A := Cub.Z2 - Cub.Z1;
            D := 2;
            F := B;
          end;
        end;
        if (A = 0) then
          Break;
        Inc(ClrUsed);
        Cubes[ClrUsed] := Cubes[F];
        Cub := @Cubes[F];
        case D of
          0:
          begin
            Cub.X2 := (Cub.X1 + Cub.X2) shr 1;
            Cubes[ClrUsed].X1 := Cub.X2 + 1;
          end;
          1:
          begin
            Cub.Y2 := (Cub.Y1 + Cub.Y2) shr 1;
            Cubes[ClrUsed].Y1 := Cub.Y2 + 1;
          end;
          2:
          begin
            Cub.Z2 := (Cub.Z1 + Cub.Z2) shr 1;
            Cubes[ClrUsed].Z1 := Cub^.Z2 + 1;
          end;
        end;
        OptimizeCube(@Cubes[ClrUsed]);
        OptimizeCube(Cub);
      until ClrUsed = ClrNum - 1;

      // handle autoselect mode
      if Mode = ccmAuto then
        if A > 0 then
          Mode := ccmDither;
          
      // for dithered colors that fall out of cubes
      if Mode = ccmDither then
        FillMemory(Space, SizeOf(TColorSpace), 255);

      // fill the cube-splitted color space with palette indexes and the palette
      for D := 0 to ClrUsed do
      begin
        for A := Cubes[D].X1 to Cubes[D].X2 do
          for B := Cubes[D].Y1 to Cubes[D].Y2 do
            for C := Cubes[D].Z1 to Cubes[D].Z2 do
              Space[A, B, C] := D;
        Palette[D].R := (Cubes[D].X1 + Cubes[D].X2) shl 2 + 2;
        Palette[D].G := (Cubes[D].Y1 + Cubes[D].Y2) shl 2 + 2;
        Palette[D].B := (Cubes[D].Z1 + Cubes[D].Z2) shl 2 + 2;
      end;

      // apply the palette to the empty bitmap
      GetMem(LogPal, SizeOf(LogPal) + ClrNum * SizeOf(TPaletteEntry));
      try
        LogPal.palVersion := $300;
        LogPal.palNumEntries := ClrNum;
        Move(Palette[0], LogPal.palPalEntry, ClrNum * SizeOf(TPaletteEntry));
        I := CreatePalette(LogPal^);
        Bitmap.Palette := I;
      finally
        FreeMem(LogPal);
      end;
    end else
    begin
      Bitmap.Palette := 0;
      ClrUsed := -1;
    end;
    // set new bitmap pixels
    Bitmap.Width := BM.Width;
    DC := Bitmap.Canvas.Handle;
    Cardinal(Color) := 0;
    if Mode = ccmDither then
    begin
      // performs Floyd-Steinberg dithering
      GetMem(Lines[0], (BM.Width + 2) * SizeOf(TThreeInt));
      GetMem(Lines[1], (BM.Width + 2) * SizeOf(TThreeInt));
      try
        I := 0;
        PCA := PKColorRecArray(BM.ScanLine[BM.Height - 1]);
        for B := 0 to BM.Width - 1 do
        begin
          PCE := @PCA[I];
          Lines[0, B].r := PCE.r shl 4;
          Lines[0, B].g := PCE.g shl 4;
          Lines[0, B].b := PCE.b shl 4;
          Inc(I);
        end;
        // main cycle
        Dir := 1;
        for A := 1 to BM.Height do
        begin
          Nxt := A and 1;
          Ln := 1 - Nxt;
          if A < BM.Height then
            for B := 0 to BM.Width - 1 do
            begin
              PCE := @PCA[I];
              Lines[Nxt, B].r := PCE.r shl 4;
              Lines[Nxt, B].g := PCE.g shl 4;
              Lines[Nxt, B].b := PCE.b shl 4;
              Inc(I);
            end;
          if Dir = 1 then B := 0 else B := BM.Width - 1;
          PTI := @Lines[Ln, B];
          while (B >= 0) and (B < BM.Width) do
          begin
            PTI.r := PTI.r div 16;
            if PTI.r > 255 then PTI.r := 255
            else if PTI.r < 0 then PTI.r := 0;
            PTI.g := PTI.g div 16;
            if PTI.g > 255 then PTI.g := 255
            else if PTI.g < 0 then PTI.g := 0;
            PTI.b := PTI.b div 16;
            if PTI.b > 255 then PTI.b := 255
            else if PTI.b < 0 then PTI.b := 0;

            if IsPalette then
            begin
              F := Space[PTI.r shr 3, PTI.g shr 3, PTI.b shr 3];
              // for dithered colors that fall out of any cube
              if F = 255 then
              begin
                D := $0FFFFFFF;
                for C := ClrUsed downto 0 do
                begin
                  PPE := @Palette[C];
                  E := Sqr(PTI.R - PPE.R) + Sqr(PTI.G - PPE.G) + Sqr(PTI.B - PPE.B);
                  if E < D then
                  begin
                    D := E;
                    F := C;
                  end;
                end;
                Space[PTI.R shr 3, PTI.G shr 3, PTI.B shr 3] := F;
              end;
              Color := Palette[F];
              C := PTI.R - Palette[F].R;
              D := PTI.G - Palette[F].G;
              E := PTI.B - Palette[F].B;
            end else
            begin
              Color.R := PTI.R;
              Color.G := PTI.G;
              Color.B := PTI.B;
              C := PTI.R and $07 - 4;
              D := PTI.G and $07 - 4;
              E := PTI.B and $07 - 4;
            end;
            Windows.SetPixel(DC, B, A - 1, ColorToRGB(TColor(Color)));
            if C <> 0 then
            begin
              Inc(Lines[Ln, B + Dir].r, C * 7);
              Inc(Lines[Nxt, B - Dir].r, C * 3);
              Inc(Lines[Nxt, B].r, C * 5);
              Inc(Lines[Nxt, B + Dir].r, C);
            end;
            if D <> 0 then
            begin
              Inc(Lines[Ln, B + Dir].g, D * 7);
              Inc(Lines[Nxt, B - Dir].g, D * 3);
              Inc(Lines[Nxt, B].g, D * 5);
              Inc(Lines[Nxt, B + Dir].g, D);
            end;
            if E <> 0 then
            begin
              Inc(Lines[Ln, B + Dir].b, E * 7);
              Inc(Lines[Nxt, B - Dir].b, E * 3);
              Inc(Lines[Nxt, B].b, E * 5);
              Inc(Lines[Nxt, B + Dir].b, E);
            end;
            Inc(PTI, Dir);
            Inc(B, Dir);
          end;
          Dir := -Dir;
        end;
      finally
        Dispose(Lines[0]);
        Dispose(Lines[1]);
      end;
    end else // no dither
    begin
      PCA := PKColorRecArray(BM.ScanLine[BM.Height - 1]);
      for A := 0 to Bitmap.Height - 1 do
      begin
        C := A * Bitmap.Width;
        for B := 0 to Bitmap.Width - 1 do
        begin
          PCE := @PCA[C + B];
          if IsPalette then
          begin
            D := Space[PCE.R shr 3, PCE.G shr 3, PCE.B shr 3];
            Color := Palette[D];
          end else
          begin
            Color.R := PCE.R;
            Color.G := PCE.G;
            Color.B := PCE.B;
          end;
          Windows.SetPixel(DC, B, A, ColorToRGB(TColor(Color)));
        end;
      end;
    end;
  finally
    BM.Free;
    if IsPalette then
    begin
      FreeMem(Space);
      Cubes := nil;
      Palette := nil;
    end;
  end;
end;

function StreamReadString(Stream: TStream): string;
var
  I, Len: Integer;
begin
  Stream.Read(Len, SizeOf(Integer));
  SetLength(Result, Len);
  for I := 1 to Len do
    Stream.Read(Result[I], SizeOf(Char))
end;

function StreamReadStringW(Stream: TStream): WideString;
var
  I, Len: Integer;
begin
  Stream.Read(Len, SizeOf(Integer));
  SetLength(Result, Len);
  for I := 1 to Len do
    Stream.Read(Result[I], SizeOf(WideChar))
end;

procedure StreamWriteString(Stream: TStream; const S: string);
var
  I, Len: Integer;
begin
  Len := Length(S);
  Stream.Write(Len, SizeOf(Integer));
  for I := 1 to Len do
    Stream.Write(S[I], SizeOf(Char));
end;

procedure StreamWriteStringW(Stream: TStream; const S: WideString);
var
  I, Len: Integer;
begin
  Len := Length(S);
  Stream.Write(Len, SizeOf(Integer));
  for I := 1 to Len do
    Stream.Write(S[I], SizeOf(WideChar));
end;

function DateToMinutes(DT: TDateTime): Cardinal;
begin
  Result := Trunc(DT) * 1440 + HourOf(DT) * 60 + MinuteOf(DT);
end;

function MinutesToDate(I: Integer): TDateTime;
begin
  Result := I div 1440 + EncodeTime(I mod 1440 div 24, I mod 60, 0, 0);
end;

function FindPath(const ARoot: string; const AFileName: string): string;
var
  I: Integer;
  SR: TSearchRec;
  S: string;
begin
  if FileExists(AFileName) then
    Result := AFileName
  else
  begin
    S := Format('%s\%s', [ARoot, ExtractFileName(AFileName)]);
    if FileExists(S) then
      Result := S
    else
    begin
      Result := '';
      I := FindFirst(Format('%s\*', [ARoot]), faAnyFile, SR);
      while (I = 0) and (Result = '') do
      begin
        if (SR.Name <> '.') and (SR.Name <> '..') and (SR.Attr and faDirectory <> 0) then
          Result := FindPath(Format('%s\%s', [ARoot, SR.Name]), AFileName);
        I := FindNext(SR);
      end;
      FindClose(SR);
    end;
  end;
end;

function BuildFileList(const Path: string; const Attr: Integer; const List: TStrings): Boolean;
var
  SearchRec: TSearchRec;
  R: Integer;
begin
  Assert(List <> nil);
  R := FindFirst(Path, Attr, SearchRec);
  Result := R = 0;
  if Result then
  begin
    while R = 0 do
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        List.Add(SearchRec.Name);
      R := FindNext(SearchRec);
    end;
    Result := R = ERROR_NO_MORE_FILES;
    FindClose(SearchRec);
  end;
end;

function DelTreeEx(const Path: string; AbortOnFailure: Boolean): Boolean;
var
  Files: TStringList;
  LPath: string; // writable copy of Path
  FileName: string;
  I: Integer;
  PartialResult: Boolean;
  Attr: DWORD;
begin
  Result := True;
  Files := TStringList.Create;
  try
    LPath := Path;
    StripLastPathSlash(LPath);
    BuildFileList(LPath + '\*.*', faAnyFile, Files);
    for I := 0 to Files.Count - 1 do
    begin
      FileName := LPath + '\' + Files[I];
      // If the current file is itself a directory then recursively delete it
      Attr := GetFileAttributes(PChar(FileName));
      if (Attr <> DWORD(-1)) and ((Attr and FILE_ATTRIBUTE_DIRECTORY) <> 0) then
        PartialResult := DelTreeEx(FileName, AbortOnFailure)
      else
      begin
          // Set attributes to normal in case it's a readonly file
          PartialResult := SetFileAttributes(PChar(FileName), FILE_ATTRIBUTE_NORMAL);
          if PartialResult then
            PartialResult := DeleteFile(FileName);
      end;
      if not PartialResult then
      begin
        Result := False;
        if AbortOnFailure then
          Break;
      end;
    end;
  finally
    FreeAndNil(Files);
  end;
end;

function DelTree(const Path: string): Boolean;
begin
  Result := DelTreeEx(Path, False);
end;

procedure DataToString(Buffer: Pointer; Size: Integer; var S: string);
var
  I: Integer;
  T: string;
begin
  SetLength(S, Size * 2);
  for I := 1 to Size do
  begin
    T := Format('%.2x' , [PBytes(Buffer)^[I - 1]]);
    S[I * 2 - 1] := T[1];
    S[I * 2] := T[2];
  end;
end;

procedure StringToData(const S: string; Buffer: Pointer; Size: Integer);
var
  I, Code: Integer;
  T: string;
begin
  T := '$00';
  for I := 1 to Min(Size, Length(S) div 2) do
  begin
    T[2] := S[I * 2 - 1];
    T[3] := S[I * 2];
    Val(T, PBytes(Buffer)^[I - 1], Code);
  end;
end;

procedure AddStringsToList(L: TStrings; const Strings: array of string);
var
  I: Integer;
begin
  if Assigned(L) then
  begin
    for I := 0 to High(Strings) do
      L.Add(Strings[I]);
  end;
end;

procedure PrintLines(S: TStrings);
var
  FPrn: TextFile;
  I: Integer;
begin
  if (S <> nil) and (S.Count > 0) then
  begin
    AssignPrn(FPrn);
    Rewrite(FPrn);
    try
      for I := 0 to S.Count - 1 do
        Writeln(FPrn, S[I]);
    finally
      CloseFile(FPrn);
    end;
  end;
end;

initialization
  ColorList := nil;
finalization
  ColorList.Free;
  if InstExists and (InstMutex <> 0) then
    ReleaseMutex(InstMutex);
end.
