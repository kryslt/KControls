{ @abstract(This unit contains some useful edit controls)
  @author(Tomas Krysl (tk@@tkweb.eu))
  @created(20 Oct 2001)
  @lastmod(6 Jul 2014)

  Copyright © Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. All redistributions
  of the original or modified source code must retain the original copyright
  notice. The Author accepts no liability for any damage that may result
  from using this code. }

unit KEdits;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF FPC}
    LCLType, LCLIntf, LMessages, LCLProc, LResources,
  {$ELSE}
    Windows, Messages,
  {$ENDIF}
    SysUtils, Classes, Controls, Forms, Graphics, StdCtrls, ComCtrls, Dialogs,
    KFunctions, KDialogs, KLog
  {$IFDEF USE_THEMES}
    , Themes
   {$IFNDEF FPC}
    , UxTheme
   {$ENDIF}
  {$ENDIF}
    ;

const
  KM_NE_UPDATEUPDOWN = KM_BASE + 101;

type
  TKLabelPosition = (
    lpAbove,
    lpBelow,
    lpLeft,
    lpRight
  );

  TKNumberEditAcceptedFormat = (
    neafAscii,
    neafBin,
    neafDec,
    neafFloat,
    neafHex,
    neafOct
  );

  TKNumberEditAcceptedFormats = set of TKNumberEditAcceptedFormat;

  TKNumberEditDisplayedFormat = (
    nedfAsInput,
    nedfAscii,
    nedfBin,
    nedfDec,
    nedfFloat,
    nedfHex,
    nedfOct
  );

  TKNumberEditHexPrefix = (
    nehpC,
    nehpPascal
  );

  TKNumberEditOption = (
    neoKeepEmpty,
    neoLowerCase,
    neoUnsigned,
    neoUseLabel,
    neoUsePrefix,
    neoUseUpDown,
    neoWarning
  );

  TKNumberEditOptions = set of TKNumberEditOption;

  { TKNumberEdit }

  TKNumberEdit = class(TCustomEdit)
  private
    FAcceptedFormats: TKNumberEditAcceptedFormats;
    FCustomSuffix: string;
    FDecimalSeparator: Char;
    FDisplayedFormat: TKNumberEditDisplayedFormat;
  {$IFDEF FPC}
    FFlat: Boolean;
  {$ENDIF}
    FFixedWidth: Integer;
    FHexPrefix: TKNumberEditHexPrefix;
    FLabel: TLabel;
    FLabelPosition: TKLabelPosition;
    FLabelSpacing: Cardinal;
    FLastInputFormat: TKNumberEditDisplayedFormat;
    FLog: TKLog;
    FMax: Extended;
    FMin: Extended;
    FOptions: TKNumberEditOptions;
    FPrecision: Integer;
    FRealUpDownStep: Extended;
    FUpdateUpDown: Boolean;
    FUpDown: TUpDown;
    FUpdownChanging: Boolean;
    FUpDownStep: Extended;
    FWarningColor: TColor;
    FOnUpDownChange: TNotifyEvent;
    function GetCaption: TCaption;
    function GetMaxAsInt: Int64;
    function GetMinAsInt: Int64;
    function GetValue: Extended;
    function GetValueAsInt: Int64;
    function GetValueAsText: string;
    function IsCaptionStored: Boolean;
    function IsCustomSuffixStored: Boolean;
    function IsMaxStored: Boolean;
    function IsMinStored: Boolean;
    function IsUpDownStepStored: Boolean;
    function IsValueStored: Boolean;
    procedure KMNEUpdateUpDown(var Msg: TLMessage); message KM_NE_UPDATEUPDOWN;
    procedure SetAcceptedFormats(AValue: TKNumberEditAcceptedFormats);
    procedure SetCaption(const AValue: TCaption);
    procedure SetCustomSuffix(const AValue: string);
    procedure SetDecimalSeparator(Value: Char);
    procedure SetDisplayedFormat(AValue: TKNumberEditDisplayedFormat);
    procedure SetFixedWidth(AValue: Integer);
    procedure SetHexPrefix(AValue: TKNumberEditHexPrefix);
    procedure SetLabelPosition(Value: TKLabelPosition);
    procedure SetLabelSpacing(Value: Cardinal);
    procedure SetMin(AMin: Extended);
    procedure SetMinAsInt(AMin: Int64);
    procedure SetMax(AMax: Extended);
    procedure SetMaxAsInt(AMax: Int64);
    procedure SetOptions(AValue: TKNumberEditOptions);
    procedure SetPrecision(AValue: Integer);
    procedure SetUpDownStep(AValue: Extended);
    procedure SetValue(AValue: Extended);
    procedure SetValueAsInt(AValue: Int64);
    procedure SetValueAsText(const AValue: string);
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMBiDiModeChanged(var Msg: TLMessage); message CM_BIDIMODECHANGED;
    procedure WMPaste(var Msg: TLMPaste); message LM_PASTE;
    procedure WMKillFocus(var Msg: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMMove(var Msg: TLMMove); message LM_MOVE;
    procedure WMSetFocus(var Msg: TLMSetFocus); message LM_SETFOCUS;
    procedure WMSize(var Msg: TLMSize); message LM_SIZE;
  protected
    procedure Change; override;
  {$IFDEF FPC}
    procedure CreateWnd; override;
    procedure DoOnChangeBounds; override;
  {$ENDIF}
    procedure DoWarning(AValue: Extended); dynamic;
    function GetFormat(S: string; var Fmt: TKNumberEditDisplayedFormat): Extended; virtual;
    procedure GetPrefixSuffix(Format: TKNumberEditDisplayedFormat; out Prefix, Suffix: string); dynamic;
    function InspectInputChar(Key: Char): Char; dynamic;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SafeSetFocus; virtual;
  {$IFDEF FPC}
    procedure SetFlat(Value: Boolean);
  {$ENDIF}
    function SetFormat(AValue: Extended): string; virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateFormats; dynamic;
    procedure UpdateLabel; dynamic;
    procedure UpdateMaxMin; dynamic;
    procedure UpdateUpDown(AValue: Extended); dynamic;
    procedure UpdateUpDownPos; dynamic;
    procedure UpDownChange; dynamic;
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: {$IFDEF COMPILER17_UP}Integer{$ELSE}SmallInt{$ENDIF}; Direction: TUpDownDirection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Empty: Boolean; virtual;
    property MaxAsInt: Int64 read GetMaxAsInt write SetMaxAsInt;
    property MinAsInt: Int64 read GetMinAsInt write SetMinAsInt;
    property ValueAsInt: Int64 read GetValueAsInt write SetValueAsInt;
    property ValueAsText: string read GetValueAsText write SetValueAsText;
  published
    property AcceptedFormats: TKNumberEditAcceptedFormats read FAcceptedFormats
      write SetAcceptedFormats default [neafDec];
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property Caption: TCaption read GetCaption write SetCaption stored
      IsCaptionStored;
    property Color;
    property Constraints;
    {$IFDEF FPC}
      { Specifies the same as Ctl3D in Delphi. }
      property Flat: Boolean read FFlat write SetFlat default False;
    {$ELSE}
      { Inherited property - see Delphi help. }
      property Ctl3D;
    {$ENDIF}
    property CustomSuffix: string read FCustomSuffix write SetCustomSuffix
      stored IsCustomSuffixStored;
    property DecimalSeparator: Char read FDecimalSeparator write SetDecimalSeparator;  
    property DisplayedFormat: TKNumberEditDisplayedFormat read FDisplayedFormat
      write SetDisplayedFormat default nedfAsInput;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedWidth: Integer read FFixedWIdth write SetFixedWidth default 0;
    property Font;
    property HexPrefix: TKNumberEditHexPrefix read FHexPrefix write SetHexPrefix
      default nehpC;
    property HideSelection;
    property LabelPosition: TKLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Cardinal read FLabelSpacing write SetLabelSpacing default 3;
    property Log: TKLog read FLog write FLog;
    property MaxLength;
    property Max: Extended read FMax write SetMax stored IsMaxStored;
    property Min: Extended read FMin write SetMin stored IsMinStored;
    property Options: TKNumberEditOptions read FOptions write SetOptions
      default [neoLowerCase, neoUseLabel, neoUsePrefix, neoUseUpDown, neoWarning];
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Precision: Integer read FPrecision write SetPrecision default 2;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UpDownStep: Extended read FUpDownStep write SetUpDownStep stored IsUpDownStepStored;
    property Value: Extended read GetValue write SetValue stored IsValueStored;
    property Visible;
    property WarningColor: TColor read FWarningColor write FWarningColor default clRed;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUpDownChange: TNotifyEvent read FOnUpDownChange write FOnUpDownChange;
  end;

  TKFileNameEditButtonStyle = (fbNone, fbButton, fbBitBtn, fbSpeedBtn, fbUser);

  TKFileNameEditButtonAlign = (fbaRight, fbaLeft, fbaLeftDown, fbaRightDown);

  TKFileNameEditOption = (foFolderOnly, foAlwaysInitialDir, foAddToList,
    foCheckPath, foCorrectPath, foPathMustExist, foAddInitialDir,
    foCheckWithInitialDir, foWarning);

  TKFileNameEditOptions = set of TKFileNameEditOption;

  TKFileNameEditDlgProperties = class(TPersistent)
  private
    FInitialDir: TFolder;
    FDefaultExt: string;
    FFilter: string;
    FFilterIndex: Integer;
    FOpenOptions: TOpenOptions;
    FBrowseOptions: TKBrowseFolderOptions;
    FBrowseDlgLabel: string;
    function IsOpenOptionsStored: Boolean;
    function IsBrowseOptionsStored: Boolean;
  protected
  public
    constructor Create;
  published
    property BrowseDlgLabel: string read FBrowseDlgLabel write FBrowseDlgLabel;
    property BrowseOptions: TKBrowseFolderOptions read FBrowseOptions write FBrowseOptions stored IsBrowseOptionsStored;
    property DefaultExt: string read FDefaultExt write FDefaultExt;
    property Filter: string read FFilter write FFilter stored True;
    property FilterIndex: Integer read FFilterIndex write FFilterIndex default 1;
    property InitialDir: TFolder read FInitialDir write FInitialDir;
    property OpenOptions: TOpenOptions read FOpenOptions write FOpenOptions stored IsOpenOptionsStored;
  end;

  { TKFileNameEdit }

  TKFileNameEdit = class(TCustomComboBox)
  private
    FButton: TControl;
    FButtonAlign: TKFileNameEditButtonAlign;
    FButtonStyle: TKFileNameEditButtonStyle;
    FButtonText: TCaption;
    FButtonWidth: Integer;
    FButtonDist: Integer;
  {$IFDEF FPC}
    FFlat: Boolean;
  {$ENDIF}
    FLog: TKLog;
    FOptions: TKFileNameEditOptions;
    FWarningColor: TColor;
    FBtnOnClick: TNotifyEvent;
    FDlgProperties: TKFileNameEditDlgProperties;
    function GetFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
    procedure SetButton(Value: TControl);
    function IsButtonStored: Boolean;
    procedure SetButtonAlign(Value: TKFileNameEditButtonAlign);
    procedure SetButtonStyle(Value: TKFileNameEditButtonStyle);
    procedure SetButtonText(const Value: TCaption);
    function IsButtonTextStored: Boolean;
    procedure SetButtonWidth(Value: Integer);
    procedure SetButtonDist(Value: Integer);
    function GetWholeWidth: Integer;
    function GetWholeLeft: Integer;
  {$IFDEF FPC}
    procedure SetFlat(Value: Boolean);
  {$ENDIF}
    procedure SetWholeWidth(Value: Integer);
    procedure SetWholeLeft(const Value: Integer);
    procedure SetOptions(Value: TKFileNameEditOptions);
    procedure UpdateButton;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMBiDiModeChanged(var Msg: TLMessage); message CM_BIDIMODECHANGED;
    procedure WMMove(var Msg: TLMMove); message LM_MOVE;
    procedure WMSize(var Msg: TLMSize); message LM_SIZE;
  protected
    procedure ButtonClick(Sender: TObject); dynamic;
    procedure ButtonExit(Sender: TObject); dynamic;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DropDown; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Style; {Must be published before Items}
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Button: TControl read FButton write SetButton stored IsButtonStored;
    property ButtonAlign: TKFileNameEditButtonAlign read FButtonAlign write SetButtonAlign default fbaRight;
    property ButtonDist: Integer read FButtonDist write SetButtonDist default 8;
    property ButtonStyle: TKFileNameEditButtonStyle read FButtonStyle write SetButtonStyle default fbButton;
    property ButtonText: TCaption read FButtonText write SetButtonText stored IsButtonTextStored;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 70;
    property CharCase;
    property Color;
    property Constraints;
    {$IFDEF FPC}
      { Specifies the same as Ctl3D in Delphi. }
      property Flat: Boolean read FFlat write SetFlat default False;
    {$ELSE}
      { Inherited property - see Delphi help. }
      property Ctl3D;
    {$ENDIF}
    property DlgProperties: TKFileNameEditDlgProperties read FDlgProperties;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property FileName: TFileName read GetFileName write SetFileName;
    property Font;
    property ItemHeight;
    property Log: TKLog read FLog write FLog;
    property MaxLength;
    property Options: TKFileNameEditOptions read FOptions write SetOptions default
      [foAddToList, foCheckPath, foWarning];
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property WarningColor: TColor read FWarningColor write FWarningColor default clRed;
    property WholeLeft: Integer read GetWholeLeft write SetWholeLeft stored False;
    property WholeWidth: Integer read GetWholeWidth write SetWholeWidth stored False;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
  end;

function CorrectPath(const S, InitialDir: TFileName; Options: TKFileNameEditOptions;
  out Err: Boolean; Log: TKLog): TFileName;

function CorrectSubDirName(var S: string; out Warn: Boolean; Log: TKLog): Boolean;

implementation

uses
  ClipBrd, Buttons, Math, Types, TypInfo, KMessageBox, KRes;

const
  IdStartCharSet = ['_', 'a'..'z', 'A'..'Z'];
  IdCharSet = ['0'..'9'] + IdStartCharSet;
  SubDirIllegalCharSet = [#0..#31, '<', '>', ':', '"', '/', '\', '|', '*', '?'];

function CorrectPath(const S, InitialDir: TFileName; Options: TKFileNameEditOptions;
  out Err: Boolean; Log: TKLog): TFileName;

  function FindCharFromPos(const S: string; AChar: Char; ALen: Integer; var APos: Integer): Boolean;
  var
    Bk: Integer;
  begin
    {ignore multiple backslashes}
    while (APos < ALen) and (S[APos] = AChar) do Inc(APos);
    Bk := APos;
    while (Bk < ALen) and (S[Bk] <> AChar) do Inc(Bk);
    Result := Bk < ALen;
  end;

var
  I, Len, K: Integer;
  B0, B, B1, B2, Warn, DirAdded: Boolean;
  T, T1: string;
begin
  T := S; Err := False; DirAdded := False;
  Len := Length(T); K := 1;
  B0 := False; B := False; B1 := False; B2 := False;
  for I := 1 to Len do if T[I] = '/' then T[I] := '\';
  if Len > 0 then
  begin
    {check drive}
    if CharInSetEx(UpCase(T[1]), ['A'..'Z']) and (T[2] = ':') then
    begin
      if (T[3] <> '\') then
      begin
        Insert('\', T, 3);
        Inc(Len);
      end;
      K := 4;
    end
    else if (S[1] = '.') and (S[2] = '\') then  //check current dir
      K := 3
    else if (S[1] = '.') and (S[2] = '.') and (S[3] = '\') then //check parent dir
      K := 4
   { else  //check protocol - not enabled
    begin
      I := 0;
      while not (S[I] in [':', '\']) do Inc(I);
      if (I <> 0) and (S[I] = ':') then
      begin
        T1 := LowerCase(Copy(S, 1, I - 1));
        if (T1 = 'http') or (T1 = 'ftp') or (T1 = 'gopher') or (T1 = 'mailto') or
          (T1 = 'nntp') then
          K := Length(T1) + 2;
      end;
    end};
    if K = 1 then
    begin
      if Options * [foCorrectPath, foAddInitialDir] <> [] then
        while CharInSetEx(T[1], SubDirIllegalCharSet) do Delete(T, 1, 1);
      if foAddInitialDir in Options then
      begin
        if InitialDir[Length(InitialDir)] = '\' then
          T := Format('%s%s', [InitialDir, T])
        else
          T := Format('%s\%s', [InitialDir, T]);
        Len := Length(T);
        K := Length(InitialDir) + 2;
        B0 := True;
        if Assigned(Log) then
          Log.Log(lgNote, Format(sEDCurrentDirAdded, [S]));
        DirAdded := True;
      end;
    end;
    {check subdirectories}
    while (K < Len) and (FindCharFromPos(T, '\', Len, K) or (foFolderOnly in Options)) do
    begin
      {check or correct next subdirectory}
      if K < Len then
      begin
        I := K;
        while (I < Len) and (T[I] <> '\') do Inc(I);
        if T[I] <> '\' then Inc(I);
        T1 := Copy(T, K, I - K);
        if CorrectSubDirName(T1, Warn, nil) then
        begin
          if Warn then B := True;
          if (foCorrectPath in Options) or not Warn then
          begin
            Delete(T, K, I - K);
            Insert(T1, T, K);
            Len := Length(T);
            Inc(K, Length(T1) + 1);
          end else
            Inc(K, I - K + 1);
        end else
          Inc(K, Length(T1) + 1);
      end;
    end;
    {check file name}
    if not (foFolderOnly in Options) then
    begin
      if K < Len then
      begin
        T1 := Copy(T, K, Len - K + 1);
        if CorrectSubDirName(T1, Warn, nil) then
        begin
          if Warn then B := True;
          if (foCorrectPath in Options) or not Warn then
          begin
            Delete(T, K, Len - K);
            T := T + T1;
          end;
        end;
      end else
        B1 := True;
    end;
    {check for path presence}
    if foPathMustExist in Options then
      if foFolderOnly in Options then
      begin
        if not DirectoryExists(T) then
          B2 := True;
      end else
        if not B1 then
        begin
          if not (foCheckWithInitialDir in Options) or DirAdded then
            T1 := T
          else
            if (InitialDir = '') or (ExtractFilePath(T) <> '') then
              T1 := T
            else if InitialDir[Length(InitialDir)] = '\' then
              T1 := InitialDir + T
            else
              T1 := Format('%s\%s', [InitialDir, T]);
          if not FileExists(T1) then
            B2 := True;
        end;
    {log errors}
    if Assigned(Log) then
    begin
      if B then
        if foFolderOnly in Options then
        begin
          if foCorrectPath in Options then
            Log.Log(lgInputError, Format(sEDBadDirCorr, [S, T]))
          else if foCheckPath in Options then
            Log.Log(lgWarning, Format(sEDBadDir, [T]));
        end else
          if foCorrectPath in Options then
            Log.Log(lgInputError, Format(sEDBadPathCorr, [S, T]))
          else if foCheckPath in Options then
            Log.Log(lgWarning, Format(sEDBadPath, [T]));
      if B1 and (Options * [foCheckPath, foCorrectPath] <> []) then
        Log.Log(lgWarning, sEDMissingFileName);
      if B2 then
        if foFolderOnly in Options then
          Log.Log(lgWarning, Format(sEDNoExistingDir, [T]))
        else
          Log.Log(lgWarning, Format(sEDNoExistingPath, [T]));
    end;
  end;
  Err := B0 or B or B1 or B2;
  Result := T;
end;

function CorrectSubDirName(var S: string; out Warn: Boolean; Log: TKLog): Boolean;

  function IsSpecialName(const S: string): Boolean;
  var
    T: string;
  begin
    if S[4] = '.' then T := Copy(S, 1, 3) else T := S;
    Result := (T = 'AUX') or (T = 'PRN') or (T = 'CON');
  end;

var
  I, Len: Integer;
  T: string;
begin
  Result := False;
  Warn := True;
  T := S;
  if Length(S) > 0 then
  begin
    if CharInSetEx(S[1], SubDirIllegalCharSet) then
    begin
      S[1] := '_';
      Result := True;
    end;
    I := 2;
    Len := Length(S);
    while (I <= Len) do
    begin
      if CharInSetEx(S[I], SubDirIllegalCharSet) then
      begin
        Delete(S, I, 1);
        Dec(Len);
        Dec(I);
        Result := True;
      end;
      Inc(I);
    end;
    if IsSpecialName(UpperCase(S)) then
    begin
      S := '_' + S;
      Inc(Len);
      Result := True;
    end;
    while S[Len] = '.' do
    begin
      Delete(S, Len, 1);
      Dec(Len);
      Result := True;
      Warn := False;
    end;
    if S = '.' then
    begin
      S[1] := '_';
      Result := True;
    end;
  end else
  begin
    S := '_';
    Result := True;
  end;
  if Result and Warn and Assigned(Log) then
    Log.Log(lgInputError, Format(sEDBadSubDirName, [T, S]));
end;

{ TKNumberEdit }

constructor TKNumberEdit.Create(AOwner: TComponent);
begin
  inherited;
  FMin := 0;
  FMax := 1000;
  Text := '';
  FWarningColor := clRed;
  FOptions := [neoLowerCase, neoUseLabel, neoUsePrefix, neoUseUpDown, neoWarning];
  FAcceptedFormats := [neafDec];
  FDecimalSeparator := GetFormatSettings.DecimalSeparator;
  FDisplayedFormat := nedfAsInput;
  FLastInputFormat := nedfDec;
  FFixedWidth := 0;
  FPrecision := 2;
  FCustomSuffix := '';
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FLog := nil;
  FUpDown := TUpDown.Create(Self);
  FUpDown.TabStop := False;
  FUpDown.OnChangingEx := UpDownChangingEx;
  FUpDownStep := 1;
  FUpdownChanging := False;
  FUpdateUpDown := True;
  FLabel := TLabel.Create(Self);
  FLabel.FocusControl := Self;
  FOnUpDownChange := nil;
end;

destructor TKNumberEdit.Destroy;
begin
  inherited;
end;

function TKNumberEdit.GetFormat(S: string; var Fmt: TKNumberEditDisplayedFormat): Extended;
var
  I: Int64;
  D: Extended;
  Code: Integer;
  W: Byte;
  K: Integer;
begin
  Result := 0;
  if S = '' then Exit;
  if FCustomSuffix <> '' then
  begin
    K := Pos(FCustomSuffix, S);
    if (K > 0) and (K = Length(S) - Length(FCustomSuffix) + 1) then
      Delete(S, K, Length(CustomSuffix));
    while (S <> '') and (S[Length(S)] = ' ') do
      SetLength(S, Length(S) - 1);
  end;
  if S = '' then Exit;
  // decimal integer - most probable
  if neafDec in FAcceptedFormats then
  begin
    I := DecStrToInt(S, Code);
    if (Code = 0) then
    begin
      Fmt := nedfDec;
      Result := I;
      Exit;
    end;
  end;
  // hexadecimal integer
  if neafHex in FAcceptedFormats then
  begin
    if FFixedWidth > 0 then W := FFixedWidth else W := 8;  // 32 bit
    I := HexStrToInt(S, W, not (neoUnsigned in FOptions), Code);
    if (Code = 0) then
    begin
      Fmt := nedfHex;
      Result := I;
      Exit;
    end;
  end;
  // binary integer
  if neafBin in FAcceptedFormats then
  begin
    if FFixedWidth > 0 then W := FFixedWidth else W := 16; // 16 bit
    I := BinStrToInt(S, W, not (neoUnsigned in FOptions), Code);
    if (Code = 0) then
    begin
      Fmt := nedfBin;
      Result := I;
      Exit;
    end;
  end;
  // octal integer
  if neafOct in FAcceptedFormats then
  begin
//    if FFixedWidth > 0 then W := FFixedWidth else W := 8; // 24 bit
    I := OctStrToInt(S, Code);
    if (Code = 0) then
    begin
      Fmt := nedfBin;
      Result := I;
      Exit;
    end;
  end;
  // double - custom suffix only
  if neafFloat in FAcceptedFormats then
  begin
    K := Pos('.', S);
    if K = 0 then K := Pos(',', S);
    if K = 0 then K := Pos(DecimalSeparator, S);
    if K > 0 then S[K] := '.';
    Val(S, D, Code);
    if (Code = 0) then
    begin
      Fmt := nedfFloat;
      Result := D;
      Exit;
    end;
  end;
  // ascii - least probable
  if neafAscii in FAcceptedFormats then
  begin
    if FFixedWidth > 0 then W := FFixedWidth else W := 4;  // 32 bit
    Result := AsciiToInt(S, W);
    Fmt := nedfAscii;
  end;
end;

procedure TKNumberEdit.GetPrefixSuffix(Format: TKNumberEditDisplayedFormat; out Prefix, Suffix: string);
begin
  Prefix := '';
  Suffix := '';
  case Format of
    nedfBin: if neoLowerCase in FOptions then Suffix := 'b' else Suffix := 'B';
    nedfHex:
      if neoUsePrefix in FOptions then
        case FHexPrefix of
          nehpPascal: Prefix := '$';
          nehpC: Prefix := '0x';
        end
      else
        if neoLowerCase in FOptions then Suffix := 'h' else Suffix := 'H';
    nedfOct: if neoLowerCase in FOptions then Suffix := 'o' else Suffix := 'O';
  end;  
end;

function TKNumberEdit.SetFormat(AValue: Extended): string;
var
  S, Prefix, Suffix: string;
  A: ShortString;
  W: Byte;
  J: Integer;
  F, G: Extended;
  Fmt: TKNumberEditDisplayedFormat;
begin
  S := '';
  if FDisplayedFormat = nedfAsInput then
    Fmt := FLastInputFormat
  else
    Fmt := FDisplayedFormat;
  GetPrefixSuffix(Fmt, Prefix, Suffix);
  case Fmt of
    nedfAscii:
    begin
      if FFixedWidth > 0 then W := FFixedWidth else W := 4;
      S := IntToAscii(Round(AValue), W);
    end;
    nedfBin:
    begin
//      if FFixedWidth > 0 then W := FFixedWidth else W := 16;
      S := IntToBinStr(Round(AValue), FFixedWidth, Suffix);
    end;
    nedfDec:
    begin
      Str(Round(AValue):FFixedWidth, A);
      S := string(A);
    end;
    nedfFloat:
      begin
        if FPrecision < 0 then
        begin
          S := FloatToStrF(AValue, ffGeneral, 15, 15);
        end
        else if FPrecision > 0 then
        begin
          Str(AValue:FFixedWidth:FPrecision, A);
          S := string(A);
        end else
        begin
          // determine number of valid decimal digits
          W := 0;
          F := AValue;
          G := Frac(F);
          while not (IsZero(G, 1E-10) or IsZero(1 - G, 1E-10)) do
          begin
            F := F * 10;
            G := Frac(F);
            Inc(W);
          end;
          Str(AValue:FFixedWidth:W, A);
          S := string(A);
        end;
        J := Pos('.', S);
        if J = 0 then J := Pos(',', S);
        if J > 0 then S[J] := FDecimalSeparator;
      end;
    nedfHex:
    begin
      if FFixedWidth > 0 then W := FFixedWidth else W := 8;
      S := IntToHexStr(Round(AValue), W, Prefix, Suffix, neoLowerCase in FOptions);
    end;
    nedfOct:
    begin
      S := IntToOctStr(Round(AValue));
    end;
  end;
{  Sign := #0;
  for J := 1 to Length(S) do
  begin
    if (S[J] in ['+', '-']) and (J > 1) then
    begin
      Sign := S[J];
      S[J] := '0';
    end;
    if S[J] = ' ' then S[J] := '0';
  end;
  if Sign = '+' then
    Delete(S, 1, 1)
  else if Sign = '-' then
    S := '-' + S;}
  if (S <> '') and (FCustomSuffix <> '') then
    S := S + ' ' + FCustomSuffix;
  Result := S;
end;

function TKNumberEdit.GetValue: Extended;
begin
  Result := GetFormat(Text, FLastInputFormat);
  Result := MinMax(Result, FMin, FMax);
end;

procedure TKNumberEdit.SetValue(AValue: Extended);
var
  S: string;
begin
  Font.Color := clWindowText;
  if AValue > FMax then
  begin
    AValue := FMax;
    DoWarning(AValue);
  end
  else if AValue < FMin then
  begin
    AValue := FMin;
    DoWarning(AValue);
  end;
  S := SetFormat(AValue);
  Text := S;
  UpdateUpDown(AValue);
end;

function TKNumberEdit.GetValueAsInt: Int64;
begin
  try
    Result := Round(GetValue);
  except
    Result := 0;
  end;
end;

procedure TKNumberEdit.SetValueAsInt(AValue: Int64);
begin
  SetValue(AValue);
end;

function TKNumberEdit.GetValueAsText: string;
begin
  Result := SetFormat(GetValue);
end;

procedure TKNumberEdit.SetValueAsText(const AValue: string);
var
  Fmt: TKNumberEditDisplayedFormat;
begin
  Fmt := nedfAsInput;
  SetValue(GetFormat(AValue, Fmt));
end;

procedure TKNumberEdit.SetMin(AMin: Extended);
var
  E: Extended;
begin
  if AMin <> FMin then
  begin
    E := GetValue;
    FMin := AMin;
    UpdateMaxMin;
    SetValue(E);
  end;
end;

procedure TKNumberEdit.SetMax(AMax: Extended);
var
  E: Extended;
begin
  if AMax <> FMax then
  begin
    E := GetValue;
    FMax := AMax;
    UpdateMaxMin;
    SetValue(E);
  end;
end;

function TKNumberEdit.GetMinAsInt: Int64;
begin
  try
    Result := Round(FMin);
  except
    Result := 0;
  end;
end;

procedure TKNumberEdit.SetMinAsInt(AMin: Int64);
begin
  SetMin(AMin);
end;

function TKNumberEdit.GetMaxAsInt: Int64;
begin
  try
    Result := Round(FMax);
  except
    Result := 0;
  end;
end;

procedure TKNumberEdit.SetMaxAsInt(AMax: Int64);
begin
  SetMax(AMax);
end;

function TKNumberEdit.IsValueStored: Boolean;
begin
  Result := GetValue <> 0;
end;

function TKNumberEdit.IsMinStored: Boolean;
begin
  Result := FMin <> 0;
end;

function TKNumberEdit.IsMaxStored: Boolean;
begin
  Result := FMax <> 1000;
end;

procedure TKNumberEdit.SetOptions(AValue: TKNumberEditOptions);
var
  E: Extended;
begin
  if FOptions <> AValue then
  begin
    E := GetValue;
    FOptions := AValue;
    UpdateLabel;
    UpdateMaxMin;
    SetValue(E);
  end;
end;

procedure TKNumberEdit.UpdateFormats;
var
  Fmt: TKNumberEditDisplayedFormat;
  Fmts: set of TKNumberEditDisplayedFormat;
begin
  if FAcceptedFormats = [] then
    FAcceptedFormats := [neafDec];
  Fmts := [];
  Fmt := nedfAsInput;
  if (neafAscii in FAcceptedFormats) then begin Include(Fmts, nedfAscii); Fmt := nedfAscii end;
  if (neafBin in FAcceptedFormats) then begin Include(Fmts, nedfBin); Fmt := nedfBin end;
  if (neafOct in FAcceptedFormats) then begin Include(Fmts, nedfOct); Fmt := nedfOct end;
  if (neafFloat in FAcceptedFormats) then begin Include(Fmts, nedfFloat); Fmt := nedfFloat end;
  if (neafHex in FAcceptedFormats) then begin Include(Fmts, nedfHex); Fmt := nedfHex end;
  if (neafDec in FAcceptedFormats) then begin Include(Fmts, nedfDec); Fmt := nedfDec end;
  if not (FDisplayedFormat in Fmts) then
  begin
    FDisplayedFormat := nedfAsInput;
    FLastInputFormat := Fmt;
  end;
end;

procedure TKNumberEdit.SetAcceptedFormats(AValue: TKNumberEditAcceptedFormats);
var
  E: Extended;
begin
  if AValue <> FAcceptedFormats then
  begin
    E := GetValue;
    FAcceptedFormats := AValue;
    UpdateFormats;
    UpdateMaxMin;
    SetValue(E);
  end;
end;

procedure TKNumberEdit.SetDisplayedFormat(AValue: TKNumberEditDisplayedFormat);
var
  E: Extended;
begin
  if FDisplayedFormat <> AValue then
  begin
    E := GetValue;
    FDisplayedFormat := AValue;
    UpdateFormats;
    UpdateMaxMin;
    SetValue(E);
  end;
end;

procedure TKNumberEdit.SetHexPrefix(AValue: TKNumberEditHexPrefix);
var
  E: Extended;
begin
  if FHexPrefix <> AValue then
  begin
    E := GetValue;
    FHexPrefix := AValue;
    SetValue(E);
  end;
end;

procedure TKNumberEdit.SetCustomSuffix(const AValue: string);
var
  E: Extended;
begin
  if AValue <> FCustomSuffix then
  begin
    E := GetValue;
    FCustomSuffix := AValue;
    SetValue(E);
  end;
end;

function TKNumberEdit.IsCustomSuffixStored: Boolean;
begin
  Result := FCustomSuffix <> '';
end;

procedure TKNumberEdit.SetFixedWidth(AValue: Integer);
var
  E: Extended;
begin
  if FFixedWidth <> AValue then
  begin
    E := GetValue;
    FFixedWidth := AValue;
    SetValue(E);
  end;
end;

procedure TKNumberEdit.SetPrecision(AValue: Integer);
var
  E: Extended;
begin
  if FPrecision <> AValue then
  begin
    E := GetValue;
    FPrecision := AValue;
    SetValue(E);
  end;
end;

procedure TKNumberEdit.SetUpDownStep(AValue: Extended);
var
  E: Extended;
begin
  if FUpDownStep <> AValue then
  begin
    E := GetValue;
    FUpDownStep := AValue;
    SetValue(E);
  end;
end;

function TKNumberEdit.IsUpDownStepStored: Boolean;
begin
  Result := FUpDownStep <> 1;
end;

function TKNumberEdit.GetCaption: TCaption;
begin
  Result := FLabel.Caption;
end;

procedure TKNumberEdit.SetCaption(const AValue: TCaption);
begin
  FLabel.SetTextBuf(PChar(AValue));
end;

function TKNumberEdit.IsCaptionStored: Boolean;
begin
  Result := FLabel.Caption <> Name;
end;

procedure TKNumberEdit.SetLabelPosition(Value: TKLabelPosition);
begin
  if Value <> FLabelPosition then
  begin
    FLabelPosition := Value;
    UpdateLabel;
  end;
end;

procedure TKNumberEdit.SetLabelSpacing(Value: Cardinal);
begin
  if Value < 1 then Value := 1;
  if Value <> FLabelSpacing then
  begin
    FLabelSpacing := Value;
    UpdateLabel;
  end;
end;

procedure TKNumberEdit.UpdateMaxMin;
begin
  try
    if (neafHex in FAcceptedFormats) or (FDisplayedFormat = nedfHex) then
    begin
      if neoUnsigned in FOptions then
      begin
        FMin := KFunctions.MinMax(FMin, 0, High(LongWord));
        FMax := KFunctions.MinMax(FMax, 0, High(LongWord));
        if FMax < FMin then
          FMax := FMin;
      end else
      begin
        FMin := KFunctions.MinMax(FMin, Low(Integer), High(Integer));
        FMax := KFunctions.MinMax(FMax, Low(Integer), High(Integer));
        if FMax < FMin then
          FMax := FMin;
      end;
    end;
    if FMax < FMin then
      FMax := FMin;
  except
    FMin := 0;
    FMax := 1000;
  end;
end;

procedure TKNumberEdit.UpdateUpDown(AValue: Extended);
var
  Fmt: TKNumberEditDisplayedFormat;
  AbsMax, D, PP: Extended;
begin
  if FUpdateUpdown and (FUpDown <> nil) then
    if neoUseUpDown in FOptions then
    begin
      AbsMax := Math.Max(Abs(FMax), Abs(FMin));
      if FDisplayedFormat = nedfAsInput then
        Fmt := FLastInputFormat
      else
        Fmt := FDisplayedFormat;
      D := 1;
      case Fmt of
        nedfDec: D := MinMax(FUpDownStep, 1, Math.Max(AbsMax / 10, 1));
        nedfHex: D := MinMax(FUpDownStep, 1, Math.Max(AbsMax / 16, 1));
        nedfOct: D := MinMax(FUpDownStep, 1, Math.Max(AbsMax / 8, 1));
        nedfBin: D := MinMax(FUpDownStep, 1, Math.Max(AbsMax / 2, 1));
        nedfFloat:
        begin
          PP := IntPower(10, FPrecision);
          D := MinMax(FUpDownStep * PP, 1, Math.Max(AbsMax * PP / 10, 1)) / PP;
        end;
      end;
      // UpDown min, max and position are ShortInt! (ough)
      // - must increase the order accordingly if absolute maximum number has more digits
      while AbsMax / D > 30000 do
        case Fmt of
          nedfDec, nedfFloat: D := D * 10;
          nedfHex: D := D * 16;
          nedfOct: D := D * 8;
          nedfBin: D := D * 2;
        end;
      FUpdownChanging := True;
      try
        FUpDown.Min := Trunc(FMin / D);
        FUpDown.Max := Trunc(FMax / D);
        FUpDown.Position := Trunc(AValue / D);
        FUpDown.Parent := Parent;
        FRealUpDownStep := D;
      finally
        FUpdownChanging := False;
      end;
    end else
      FUpDown.Parent := nil;
end;

procedure TKNumberEdit.UpdateUpDownPos;
begin
  if FUpDown <> nil then
    FUpDown.SetBounds(Left + Width, Top, FUpDown.Width, Height);
end;

procedure TKNumberEdit.UpdateLabel;
var
  P: TPoint;
begin
 if FLabel <> nil then
  if neoUseLabel in FOptions then
  begin
    case FLabelPosition of
      lpAbove: P := Point(Left, Top - FLabel.Height - Integer(FLabelSpacing));
      lpBelow: P := Point(Left, Top + Height + Integer(FLabelSpacing));
      lpLeft: P := Point(Left - Math.Max(Integer(FLabelSpacing), FLabel.Width + 3), Top + (Height - FLabel.Height) div 2);
      lpRight: P := Point(Left + Width + Integer(FLabelSpacing), Top + (Height - FLabel.Height) div 2);
    end;
    FLabel.Left := P.X;
    FLabel.Top := P.Y;
    FLabel.Parent := Parent
  end else
    FLabel.Parent := nil;
end;

function TKNumberEdit.Empty: Boolean;
begin
  Result := (Text = '') or (Text = '-');
end;

procedure TKNumberEdit.DoWarning(AValue: Extended);
var
  Fmt: TKNumberEditDisplayedFormat;
begin
  if (ComponentState * [csLoading, csDesigning] = []) and HasParent then
  begin
    if neoWarning in FOptions then Font.Color := FWarningColor;
    if Assigned(FLog) then
    begin
      if FDisplayedFormat = nedfAsInput then
        Fmt := FLastInputFormat
      else
        Fmt := FDisplayedFormat;
      case Fmt of
        nedfDec: FLog.Log(lgInputError, Format(sEDBadIntValueAsStr,
          [SetFormat(FMin), SetFormat(FMax), SetFormat(AValue)]));
        nedfFloat: FLog.Log(lgInputError, Format(sEDBadFloatValueAsStr,
          [SetFormat(FMin), SetFormat(FMax), SetFormat(AValue)]));
        nedfHex: FLog.Log(lgInputError, Format(sEDBadHexValueAsStr,
          [SetFormat(FMin), SetFormat(FMax), SetFormat(AValue)]));
      end;
    end;
  end;
end;

function TKNumberEdit.InspectInputChar(Key: Char): Char;
var
  KeyDec, KeyHex, KeyBin, KeyOct, KeyFLoat, KeySuffix: Char;
begin
  if neafAscii in FAcceptedFormats then
    Result := Key
  else
  begin
    Result := #0;
    if neafDec in FAcceptedFormats then
    begin
      KeyDec := Key;
      if CharInSetEx(KeyDec, ['0'..'9','-',#8]) then
      begin
        if (KeyDec = '-') and (SelStart <> 0) then
          KeyDec := #0;
        if (Pos('0', Text) = 1) and (neafOct in FAcceptedFormats) then
          KeyDec := #0;
      end else
        KeyDec := #0;
    end else
      KeyDec := #0;
    if neafHex in FAcceptedFormats then
    begin
      KeyHex := Key;
      if CharInSetEx(KeyHex, ['0'..'9', 'a'..'f', 'A'..'F', #8, 'x', 'X', 'h', 'H']) then
      begin
        if CharInSetEx(KeyHex, ['x', 'X']) and ((SelStart > 1) or
          (Pos('x', Text) <> 0) or (Pos('X', Text) <> 0)) then
          KeyHex := #0;
        if CharInSetEx(KeyHex, ['h', 'H']) and ((SelStart < Length(Text)) or
          (Pos('h', Text) <> 0) or (Pos('H', Text) <> 0)) then
          KeyHex := #0;
        if neoLowerCase in FOptions then
        begin
          if CharInSetEx(KeyHex, ['A'..'F', 'X']) then Inc(KeyHex, Ord('a') - Ord('A'));
        end else
          if CharInSetEx(KeyHex, ['a'..'f', 'x']) then Inc(KeyHex, Ord('A') - Ord('a'));
      end else
        KeyHex := #0;
    end else
      KeyHex := #0;
    if neafBin in FAcceptedFormats then
    begin
      KeyBin := Key;
      if CharInSetEx(KeyBin, ['0'..'1', 'b', 'B', #8]) then
      begin
        if CharInSetEx(KeyBin, ['b', 'B']) and ((SelStart < Length(Text)) or
          (Pos('b', Text) <> 0) or (Pos('B', Text) <> 0)) then
          KeyBin := #0;
      end else
        KeyBin := #0;
    end else
      KeyBin := #0;
    if neafOct in FAcceptedFormats then
    begin
      KeyOct := Key;
      if CharInSetEx(KeyOct, ['0'..'7', #8]) then
      begin
        if (Pos('0', Text) > 1) then
          KeyOct := #0;
      end else
        KeyOct := #0;
    end else
      KeyOct := #0;
    if neafFloat in FAcceptedFormats then
    begin
      KeyFLoat := Key;
      if CharInSetEx(KeyFLoat, ['0'..'9','-', '.', ',', 'e', 'E', DecimalSeparator, #8]) then
      begin
        if (KeyFLoat = '-') and (SelStart <> 0) then KeyFLoat := #0;
        if CharInSetEx(KeyFLoat, ['.', ',', DecimalSeparator]) and
          ((Pos('.', Text) <> 0) or (Pos(',', Text) <> 0) or
           (Pos(DecimalSeparator, Text) <> 0)) then
          KeyFLoat := #0;
      end else
        KeyFLoat := #0;
    end else
      KeyFLoat := #0;
    if FCustomSuffix <> '' then
    begin
      if (Pos(Key, FCustomSuffix) <> 0) or (Key = ' ') then
        KeySuffix := Key
      else
        KeySuffix := #0;
    end else
      KeySuffix := #0;
    if KeyFloat <> #0 then Result := KeyFLoat;
    if KeyBin <> #0 then Result := KeyBin;
    if KeyHex <> #0 then Result := KeyHex;
    if KeyDec <> #0 then Result := KeyDec;
    if KeyOct <> #0 then Result := KeyOct;
    if KeySuffix <> #0 then Result := KeySuffix;
  end;  
end;

procedure TKNumberEdit.KeyPress(var Key: Char);
begin
  inherited;
  if Key >= #32 then
  begin
    Key := InspectInputChar(Key);
    if Key <> #0 then
      Font.Color := clWindowText;
  end
end;

procedure TKNumberEdit.Change;
begin
  inherited;
  UpdateUpDown(GetValue);
end;

{$IFDEF FPC}
procedure TKNumberEdit.CreateWnd;
begin
  inherited;
  UpdateUpDownPos;
  UpdateLabel;
end;

procedure TKNumberEdit.DoOnChangeBounds;
begin
  inherited;
  UpdateUpDownPos;
  UpdateLabel;
end;
{$ENDIF}

procedure TKNumberEdit.SetParent(AParent: TWinControl);
begin
  inherited;
  UpdateUpDown(GetValue);
  UpdateLabel;
end;

procedure TKNumberEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FUpDown then
      FUpDown := nil
    else if AComponent = FLabel then
      FLabel := nil;
end;

procedure TKNumberEdit.SafeSetFocus;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if (Form <> nil) and Form.Visible and Form.Enabled and Visible and Enabled then
    Form.ActiveControl := Self;
end;

{$IFDEF FPC}
procedure TKNumberEdit.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;
{$ENDIF}

procedure TKNumberEdit.SetName(const Value: TComponentName);
var
  S: string;
begin
  S := Name;
  inherited;
  if (Text = S) or (Text = Value) or (FLabel.Caption = S) then
  begin
    if (Text = S) or (Text = Value) then
      Text := '0';
    if (FLabel <> nil) and (csSetCaption in ControlStyle) then
      FLabel.SetTextBuf(PChar(Name));
  end;
end;

procedure TKNumberEdit.UpDownChange;
begin
  if Assigned(FOnUpDownChange) then FOnUpDownChange(Self);
end;

procedure TKNumberEdit.UpDownChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue:
  {$IFDEF COMPILER17_UP}Integer{$ELSE}SmallInt{$ENDIF}; Direction: TUpDownDirection);
{var
  V, S, D, F: Extended;}
begin
  if (neoUseUpDown in FOptions) and (FUpDown <> nil) and not FUpdownChanging then
  begin
    SafeSetFocus;
    Font.Color := clWindowText;
    FUpdateUpDown := False;
{  V := GetValue;
  S := FRealUpDownStep;
  D := V / S;
  F := Frac(D);
  if F <> 0 then
  asm
  nop
  end;}
    SetValue(NewValue * FRealUpDownStep {+ F});
    UpDownChange;
    FUpdateUpDown := True;
    PostMessage(Handle, KM_NE_UPDATEUPDOWN, 0, 0);
  end;
end;

procedure TKNumberEdit.KMNEUpdateUpDown(var Msg: TLMessage);
begin
  UpdateUpDown(GetValue);
end;    

procedure TKNumberEdit.CMBiDiModeChanged(var Msg: TLMessage);
begin
  inherited;
  if FLabel <> nil then FLabel.BiDiMode := BidiMode;
end;

procedure TKNumberEdit.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited;
  if FLabel <> nil then FLabel.Enabled := Enabled;
  if FUpDown <> nil then FUpDown.Enabled := Enabled;
end;

procedure TKNumberEdit.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited;
  if FLabel <> nil then FLabel.Visible := Visible;
  if FUpDown <> nil then FUpDown.Visible := Visible;
end;

procedure TKNumberEdit.SetDecimalSeparator(Value: Char);
begin
  if Value <> FDecimalSeparator then
  begin
    FDecimalSeparator := Value;
    SetValue(GetValue);
  end;
end;

procedure TKNumberEdit.WMKillFocus(var Msg: TLMKillFocus);
var
  Fmt: TKNumberEditDisplayedFormat;
  AValue: Extended;
begin
  inherited;
  if Empty and (neoKeepEmpty in FOptions) then
    Exit;
  Fmt := nedfAsInput;
  AValue := GetFormat(Text, Fmt);
  if Fmt = nedfAsInput then
    AValue := MinMax(AValue, FMin, FMax)
  else
    FLastInputFormat := Fmt;
  SetValue(AValue);
  if (Fmt = nedfAsInput) and (ComponentState * [csLoading, csDesigning] = []) and HasParent then
  begin
    if neoWarning in FOptions then Font.Color := FWarningColor;
    if Assigned(FLog) then FLog.Log(lgInputError, sEDFormatNotAccepted);
  end;
end;

procedure TKNumberEdit.WMSetFocus(var Msg: TLMSetFocus);
begin
  inherited;
  Font.Color := clWindowText;
end;

procedure TKNumberEdit.WMMove(var Msg: TLMMove);
begin
  inherited;
  UpdateUpDownPos;
  UpdateLabel;
end;

procedure TKNumberEdit.WMPaste(var Msg: TLMPaste);
var
  S: string;
  I: Integer;
begin
  if ClipBoard.HasFormat(CF_TEXT) then
  begin
    S := ClipBoard.AsText;
    for I := 1 to Length(S) do
      if (InspectInputChar(S[I]) = #0) and not (csDesigning in ComponentState) then
      begin
        Font.Color := WarningColor;
        if Assigned(FLog) then FLog.Log(lgError, sEDClipboardFmtNotAccepted);
        SelLength := 0;
        Exit;
      end;
    Font.Color := clWindowText;
    inherited;
  end;
end;

procedure TKNumberEdit.WMSize(var Msg: TLMSize);
begin
  inherited;
  UpdateUpDownPos;
  UpdateLabel;
end;

{ TKFileNameEditDlgProperties }

constructor TKFileNameEditDlgProperties.Create;
begin
  FInitialDir := '';
  FDefaultExt := '';
  FFilter := sEDAllFiles;
  FFilterIndex := 1;
  FOpenOptions := [ofHideReadOnly, ofEnableSizing];
  FBrowseOptions := [bfReturnOnlyFSDirs, bfDontGoBelowDomain];
  FBrowseDlgLabel := '';
end;

function TKFileNameEditDlgProperties.IsBrowseOptionsStored: Boolean;
begin
  Result := FBrowseOptions <> [bfSetFolder, bfReturnOnlyFSDirs, bfDontGoBelowDomain];
end;

function TKFileNameEditDlgProperties.IsOpenOptionsStored: Boolean;
begin
  Result := FOpenOptions <> [ofHideReadOnly, ofEnableSizing];
end;

{ TKFileNameEdit }

constructor TKFileNameEdit.Create(AOwner: TComponent);
begin
  inherited;
  FButton := nil;
  FButtonStyle := fbNone;
  FButtonAlign := fbaRight;
  FButtonText := sEDBrowse;
  FButtonWidth := 75;
  FButtonDist := 8;
  SetButtonStyle(fbButton);
  FOptions := [foAddToList, foCheckPath, foWarning];
  FWarningColor  := clRed;
  ControlStyle := ControlStyle - [csSetCaption];
  FDlgProperties := TKFileNameEditDlgProperties.Create;
  FLog := nil;
end;

destructor TKFileNameEdit.Destroy;
begin
  FDlgProperties.Free;
  inherited;
end;

procedure TKFileNameEdit.ButtonClick(Sender: TObject);
var
  OD: TOpenDialog;
  BF: TKBrowseFolderDialog;
begin
  if foFolderOnly in FOptions then
  begin
    BF := TKBrowseFolderDialog.Create(Self);
    try
      if (Text = '') or (foAlwaysInitialDir in FOptions) then
        BF.Folder := FDlgProperties.InitialDir
      else
        BF.Folder := Text;
      BF.LabelText := FDlgProperties.BrowseDlgLabel;
      BF.Options := FDlgProperties.BrowseOptions;
      if BF.Execute then
      begin
        Text := BF.Folder;
        if foAddToList in FOptions then
          Items.Insert(0, Text);
        FDlgProperties.InitialDir := ExtractFilePath(Text);
        Font.Color := clWindowText;
      end;
    finally
      BF.Free;
    end;
  end else
  begin
    OD := TOpenDialog.Create(Self);
    try
      if (Text = '') or (foAlwaysInitialDir in FOptions) then
        OD.InitialDir := FDlgProperties.InitialDir
      else
        OD.InitialDir := ExtractFilePath(Text);
      OD.DefaultExt := FDlgProperties.DefaultExt;
      OD.Filter := FDlgProperties.Filter;
      OD.FilterIndex := FDlgProperties.FilterIndex;
      OD.Options := FDlgProperties.OpenOptions;
      if OD.Execute then
      begin
        Text := OD.FileName;
        if foAddToList in FOptions then
          Items.Insert(0, Text);
        FDlgProperties.InitialDir := ExtractFilePath(Text);
        Font.Color := clWindowText;
      end;
    finally
      OD.Free;
    end;
  end;
  if Assigned(FBtnOnClick) then FBtnOnClick(Sender);
end;

procedure TKFileNameEdit.ButtonExit(Sender: TObject);
begin
  DoExit;
end;

procedure TKFileNameEdit.CMBiDiModeChanged(var Msg: TLMessage);
begin
  inherited;
  {switch the button position};
  UpdateButton;
end;

procedure TKFileNameEdit.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited;
  if Assigned(FButton) then FButton.Enabled := Enabled;
end;

procedure TKFileNameEdit.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited;
  if Assigned(FButton) then FButton.Visible := Visible;
end;

procedure TKFileNameEdit.DoEnter;
begin
  Font.Color := clWindowText;
  inherited;
end;

procedure TKFileNameEdit.DoExit;
var
  B: Boolean;
  H: HWnd;
begin
  inherited;
  if FButton is TWinControl then H := TWinControl(FButton).Handle else H := 0;
  if (GetFocus <> H) and (ComponentState * [csLoading, csDesigning] = []) then
  begin
    Text := CorrectPath(Text, FDlgProperties.InitialDir, FOptions, B, FLog);
    if B then
    begin
      if foWarning in FOptions then Font.Color := FWarningColor
    end else
      if foAddToList in FOptions then
        if (Items.IndexOf(Text) < 0) and (Text <> '') then
          Items.Insert(0, Text);
  end;
end;

function TKFileNameEdit.GetFileName: TFileName;
begin
  Result := Text;
end;

procedure TKFileNameEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_RETURN then Key := 0;
end;

procedure TKFileNameEdit.DropDown;
var
  I: Integer;
begin
  // clear empty items if > 1
  if Items.Count > 1 then
    for I := 0 to Items.Count - 1 do
       if Items[I] = '' then
         Items.Delete(I);
  inherited;
end;

procedure TKFileNameEdit.SetButton(Value: TControl);
var
  PI: PPropInfo;
  N: TNotifyEvent;
begin
  if (FButtonStyle = fbUser) and (Value <> Self) then
  begin
    FButton.Free;
    FBtnOnClick := nil;
    FButton := Value;
    if Assigned(FButton) then
    begin
      PI := GetPropInfo(FButton, 'OnClick');
      if PI <> nil then
      begin
        FBtnOnClick := TNotifyEvent(GetMethodProp(FButton, PI));
        N := ButtonClick;
        SetMethodProp(FButton, PI, TMethod(N));
        UpdateButton;
        FButton.Parent := Parent;
      end;
      FButton.FreeNotification(Self);
    end;
  end;
end;

procedure TKFileNameEdit.SetButtonAlign(Value: TKFileNameEditButtonAlign);
begin
 if Value <> FButtonAlign then
 begin
   FButtonAlign := Value;
   UpdateButton;
 end;
end;

procedure TKFileNameEdit.SetButtonStyle(Value: TKFileNameEditButtonStyle);
begin
  if FButtonStyle <> Value then
  begin
    if FButtonStyle <> fbUser then FButton.Free;  
    FButtonStyle := Value;
    FButton := nil;
    FBtnOnClick := nil;
    case Value of
      fbButton:
      begin
        FButton := TButton.Create(Self);
        try
          (FButton as TButton).OnClick := ButtonClick;
        except
        end;
      end;
      fbBitBtn:
      begin
        FButton := TBitBtn.Create(Self);
        try
          with FButton as TBitBtn do
          begin
          {$IFDEF FPC}
            Glyph.LoadFromLazarusResource('OPENDIR');
          {$ELSE}
            Glyph.LoadFromResourceName(HInstance, 'OPENDIR');
          {$ENDIF}
            Glyph.Transparent := True;
            OnClick := ButtonClick;
          end;
        except
        end;
      end;
      fbSpeedBtn:
      begin
        FButton := TSpeedButton.Create(Self);
        try
          with FButton as TSpeedButton do
          begin
          {$IFDEF FPC}
            Glyph.LoadFromLazarusResource('OPENDIR');
          {$ELSE}
            Glyph.LoadFromResourceName(HInstance, 'OPENDIR');
          {$ENDIF}
            Glyph.Transparent := True;
            OnClick := ButtonClick;
          end;
        except
        end;
      end;
    end;
    UpdateButton;
    if Assigned(FButton) then
    begin
      FButton.Name := '_internal_';
      FButton.Parent := Parent;
    end;
  end;
end;

procedure TKFileNameEdit.SetButtonText(const Value: TCaption);
begin
  if Value <> FButtonText then
  begin
    FButtonText := Value;
    UpdateButton;
  end;
end;

procedure TKFileNameEdit.SetButtonWidth(Value: Integer);
begin
  if Value <> FButtonWidth then
  begin
    FButtonWidth := Value;
    UpdateButton;
  end;
end;

procedure TKFileNameEdit.SetButtonDist(Value: Integer);
begin
  if Value <> FButtonDist then
  begin
    FButtonDist := Value;
    UpdateButton;
  end;
end;

procedure TKFileNameEdit.SetFileName(const Value: TFileName);
var
  B: Boolean;
begin
  if Value <> Text then
  begin
    if ComponentState * [csLoading, csDesigning] = [] then
    begin
      Text := CorrectPath(Value, FDlgProperties.InitialDir, FOptions, B, FLog);
      if not (csDesigning in ComponentState) then
        if B then
        begin
          if foWarning in FOptions then Font.Color := FWarningColor
        end else
        begin
          if foWarning in FOptions then Font.Color := clWindowText;
          if foAddToList in FOptions then
            if (Items.IndexOf(Text) < 0) and (Text <> '') then
              Items.Insert(0, Text);
        end;
    end else
      Text := Value;        
  end;
end;

procedure TKFileNameEdit.SetOptions(Value: TKFileNameEditOptions);
begin
  if Value <> FOptions then
    FOptions := Value;
end;

procedure TKFileNameEdit.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(FButton) then
  begin
    if Parent <> nil then
      UpdateButton;
    FButton.Parent := AParent;
  end;
end;

procedure TKFileNameEdit.WMMove(var Msg: TLMMove);
begin
  inherited;
  UpdateButton;
end;

procedure TKFileNameEdit.UpdateButton;

  procedure SetButtonPos(ALeft, ADown: Boolean);
  begin
    if ALeft then
      if ADown then
      begin
        FButton.Left := Left;
        FButton.Top := Top + Height + FButtonDist;
      end else
      begin
        FButton.Left := Left - FButton.Width - FButtonDist;
        FButton.Top := Top;
        FButton.Height := Height;
      end
    else
      if ADown then
      begin
        FButton.Left := Left + Width - FButton.Width;
        FButton.Top := Top + Height + FButtonDist;
      end else
      begin
        FButton.Left := Left + Width + FButtonDist;
        FButton.Top := Top;
        FButton.Height := Height;
      end;
 end;

var
  M: TNotifyEvent;
begin
  if Assigned(FButton) then
  begin
    if FButtonText <> '&' then
      FButton.SetTextBuf(PChar(FButtonText))
    else
      FButton.SetTextBuf('');
    FButton.Width := FButtonWidth;
    if IsPublishedProp(FButton, 'OnExit') then
    try
      SetOrdProp(FButton, 'TabStop', Integer(True));
      SetOrdProp(FButton, 'TabOrder', TabOrder + 1);
      M := ButtonExit;
      SetMethodProp(FButton, 'OnExit', TMethod(M));
    except
    end;
    if BiDiMode in [bdLeftToRight, bdRightToLeftReadingOnly] then
      if FButtonAlign in [fbaLeft, fbaLeftDown] then
        SetButtonPos(True, FButtonAlign = fbaLeftDown)
      else
        SetButtonPos(False, FButtonAlign = fbaRightDown)
    else
      if FButtonAlign in [fbaLeft, fbaLeftDown] then
        SetButtonPos(False, FButtonAlign = fbaLeftDown)
      else
        SetButtonPos(False, FButtonAlign = fbaRightDown);
  end;
end;

procedure TKFileNameEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FButton) and (Operation = opRemove) then
    FButton := nil;
end;

procedure TKFileNameEdit.WMSize(var Msg: TLMSize);
begin
  inherited;
  UpdateButton;
end;

function TKFileNameEdit.IsButtonStored: Boolean;
begin
  Result := (FButton <> nil) and (FButton.Name <> '_internal_');
end;

function TKFileNameEdit.IsButtonTextStored: Boolean;
begin
  Result := FButtonText <> sEDBrowse;
end;

function TKFileNameEdit.GetWholeLeft: Integer;
begin
  if Assigned(FButton) then
    Result := Min(Left, FButton.Left)
  else
    Result := Left;
end;

{$IFDEF FPC}
procedure TKFileNameEdit.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;
{$ENDIF}

procedure TKFileNameEdit.SetWholeLeft(const Value: Integer);
var
  L: Integer;
begin
  L := WholeLeft;
  if L <> Value then
  begin
    if Assigned(FButton) then
      FButton.Left := FButton.Left + Value - L;
    Left := Left + Value - L;
  end;
end;

function TKFileNameEdit.GetWholeWidth: Integer;
begin
  if Assigned(FButton) then
    Result := Max(Left + Width, FButton.Left + FButton.Width) - WholeLeft
  else
    Result := Width;
end;

procedure TKFileNameEdit.SetWholeWidth(Value: Integer);
var
  W: Integer;
begin
  W := WholeWidth;
  if W <> Value then
  begin
    if Assigned(FButton) then
    begin
      if BiDiMode in [bdLeftToRight, bdRightToLeftReadingOnly] then
      begin
        case FButtonAlign of
          fbaLeft: Width := Max(Value - Left + FButton.Left, 5);
          fbaRight: Width := Max(Value - (FButton.Left + FButton.Width - Left - Width), 5);
        else
          Width := Max(Value, FButton.Width + 5);
        end;
      end else
      begin
        case FButtonAlign of
          fbaRight: Width := Max(Value - Left + FButton.Left, 5);
          fbaLeft: Width := Max(Value - (FButton.Left + FButton.Width - Left - Width), 5);
        else
          Width := Max(Value, FButton.Width + 5);
        end;
      end;
      UpdateButton;
    end else
      Width := Value;
  end;
end;

{$IFDEF FPC}
initialization
  {$i kedits.lrs}
{$ELSE}
  {$R kedits.res}
{$ENDIF}
end.