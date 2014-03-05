unit KFunctionsW;

{$include KControls.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Messages, Classes, SysUtils;

function Win32PlatformIsUnicode: Boolean;
function DefaultUserCodePage: Cardinal;

{File name management}
function ChangeFileExtW(const FileName, Extension: WideString): WideString;
function ExtractFilePathW(const FileName: WideString): WideString;
function ExtractFileDirW(const FileName: WideString): WideString;
function ExtractFileDriveW(const FileName: WideString): WideString;
function ExtractFileNameW(const FileName: WideString): WideString;
function ExtractFileRawNameW(const Path: WideString): WideString;
function ExtractFileExtW(const FileName: WideString): WideString;
procedure StripLastPathSlashW(var S: WideString);
procedure EnsureLastPathSlashW(var S: WideString);

{Stream management}
function StringReplaceW(const S, OldPattern, NewPattern: WideString;
  Flags: TReplaceFlags; WholeWord: Boolean = False): WideString;

implementation

function Win32PlatformIsUnicode: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT);
end;

function DefaultUserCodePage: Cardinal;
begin
  Result := CP_ACP;
end;

function StrScanW(const Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

function LastDelimiterW(const Delimiters, S: WideString): Integer;
var
  P: PWideChar;
begin
  Result := Length(S);
  P := PWideChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (StrScanW(P, S[Result]) <> nil) then
      Exit;
    Dec(Result);
  end;
end;

function ChangeFileExtW(const FileName, Extension: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiterW('.\:',Filename);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

function ExtractFilePathW(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiterW('\:', FileName);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDirW(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiterW(DriveDelim + PathDelim,Filename);
  if (I > 1) and (FileName[I] = PathDelim) and
    (not (FileName[I - 1] in [WideChar(PathDelim), WideChar(DriveDelim)])) then Dec(I);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDriveW(const FileName: WideString): WideString;
var
  I, J: Integer;
begin
  if (Length(FileName) >= 2) and (FileName[2] = DriveDelim) then
    Result := Copy(FileName, 1, 2)
  else if (Length(FileName) >= 2) and (FileName[1] = PathDelim) and
    (FileName[2] = PathDelim) then
  begin
    J := 0;
    I := 3;
    While (I < Length(FileName)) and (J < 2) do
    begin
      if FileName[I] = PathDelim then Inc(J);
      if J < 2 then Inc(I);
    end;
    if FileName[I] = PathDelim then Dec(I);
    Result := Copy(FileName, 1, I);
  end else Result := '';
end;

function ExtractFileNameW(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiterW('\:', FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractFileRawNameW(const Path: WideString): WideString;
var
  I: Integer;
begin
  Result := ExtractFileNameW(Path);
  I := Length(Result);
  repeat
    if Result[I] = '.' then
    begin
      SetLength(Result, I - 1);
      Break;
    end;
    Dec(I);
  until I = 1;
end;

function ExtractFileExtW(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := LastDelimiterW('.\:', FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;

procedure StripLastPathSlashW(var S: WideString);
var
  C: WideChar;
begin
  if S <> '' then
  begin
    C := S[Length(S)];
    if (C = '\') or (C = '/') then
      Delete(S, Length(S), 1);
  end;
end;

procedure EnsureLastPathSlashW(var S: WideString); overload;
var
  C: WideChar;
begin
  if S <> '' then
  begin
    C := S[Length(S)];
    if (C = '\') or (C = '/') then
      S := S + '\';
  end;
end;

{procedure ListItemMoveW(LV: TTntListView; FromIndex, ToIndex: Integer);
var
  LI1, LI2: TTntListItem;
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
end;}

function _WStr(lpString: PWideChar; cchCount: Integer): WideString;
begin
  if cchCount = -1 then
    Result := lpString
  else
    Result := Copy(WideString(lpString), 1, cchCount);
end;

function GetStringTypeExW(Locale: LCID; dwInfoType: DWORD;
  lpSrcStr: PWideChar; cchSrc: Integer; var lpCharType): BOOL; 
var
  AStr: AnsiString;
begin
  if Win32PlatformIsUnicode then
    Result := GetStringTypeExW(Locale, dwInfoType, lpSrcStr, cchSrc, lpCharType)
  else begin
    AStr := _WStr(lpSrcStr, cchSrc);
    Result := GetStringTypeExA(Locale, dwInfoType,
      PAnsiChar(AStr), -1, lpCharType);
  end;
end;

function _WideCharType(WC: WideChar; dwInfoType: Cardinal): Word;
begin
  Win32Check(GetStringTypeExW(GetThreadLocale, dwInfoType, PWideChar(@WC), 1, Result))
end;

function IsWideCharSpace(WC: WideChar): Boolean;
begin
  Result := (_WideCharType(WC, CT_CTYPE1) and C1_SPACE) <> 0;
end;

function IsWideCharPunct(WC: WideChar): Boolean;
begin
  Result := (_WideCharType(WC, CT_CTYPE1) and C1_PUNCT) <> 0;
end;

function IsWideCharMappableToAnsi(const WC: WideChar): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultUserCodePage, 0, PWideChar(@WC), 1, nil, 0, nil, @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;

function CharUpperW(lpsz: PWideChar): PWideChar;
var
  AStr: AnsiString;
  WStr: WideString;
begin
  if Win32PlatformIsUnicode then
    Result := CharUpperW(lpsz)
  else begin
    if HiWord(Cardinal(lpsz)) = 0 then begin
      // literal char mode
      Result := lpsz;
      if IsWideCharMappableToAnsi(WideChar(lpsz)) then begin
        AStr := WideChar(lpsz); // single character may be more than one byte
        CharUpperA(PAnsiChar(AStr));
        WStr := AStr; // should always be single wide char
        if Length(WStr) = 1 then
          Result := PWideChar(WStr[1]);
      end
    end else begin
      // null-terminated string mode
      Result := lpsz;
      while lpsz^ <> #0 do begin
        lpsz^ := WideChar(CharUpperW(PWideChar(lpsz^)));
        Inc(lpsz);
      end;
    end;
  end;
end;

function CharUpperBuffW(lpsz: PWideChar; cchLength: DWORD): DWORD;
var
  i: integer;
begin
  if Win32PlatformIsUnicode then
    Result := CharUpperBuffW(lpsz, cchLength)
  else begin
    Result := cchLength;
    for i := 1 to cchLength do begin
      lpsz^ := WideChar(CharUpperW(PWideChar(lpsz^)));
      Inc(lpsz);
    end;
  end;
end;

function UpperCaseW(const S: WideString): WideString;
begin
  { SysUtils.WideUpperCase is broken for Win9x. }
  Result := S;
  if Length(Result) > 0 then
    CharUpperBuffW(PWideChar(Result), Length(Result));
end;

function LastCharW(W: WideString): WideChar;
begin
  if Length(W) = 0 then
    Result := #0
  else
    Result := W[Length(W)];
end;

function StringReplaceW(const S, OldPattern, NewPattern: WideString;
  Flags: TReplaceFlags; WholeWord: Boolean = False): WideString;

  function IsWordSeparator(WC: WideChar): Boolean;
  begin
    Result := (WC = WideChar(#0))
           or IsWideCharSpace(WC)
           or IsWideCharPunct(WC);
  end;

var
  SearchStr, Patt, NewStr: WideString;
  Offset: Integer;
  PrevChar, NextChar: WideChar;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := WideUpperCase(S);
    Patt := WideUpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end; // done

    if (WholeWord) then
    begin
      if (Offset = 1) then
        PrevChar := LastCharW(Result)
      else
        PrevChar := NewStr[Offset - 1];

      if Offset + Length(OldPattern) <= Length(NewStr) then
        NextChar := NewStr[Offset + Length(OldPattern)]
      else
        NextChar := WideChar(#0);

      if (not IsWordSeparator(PrevChar))
      or (not IsWordSeparator(NextChar)) then
      begin
        Result := Result + Copy(NewStr, 1, Offset + Length(OldPattern) - 1);
        NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
        SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
        continue;
      end;
    end;

    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;


end.
