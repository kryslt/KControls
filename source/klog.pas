{ @abstract(This unit contains a component for event logging)
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

unit KLog;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  StdCtrls, Classes, ComCtrls, ExtCtrls, KFunctions;

type
  TKLogDirection = (
    ldAddTop,
    ldAddBottom
  );

  TKLogType = (
    lgNone,
    lgError,
    lgWarning,
    lgNote,
    lgHint,
    lgInfo,
    lgInputError,
    lgIOError,
    lgAll
  );

const
  cHoverTimeDef = 1000;
  cLogDirectionDef = ldAddTop;
  cLogMaskDef = [lgAll];
  cStatusPanelDef = -1; // SimpleText property

type
  TKLogMask = set of TKLogType;

  TKLogEvent = procedure(Sender: TObject; Code: TKLogType; const Text: string) of object;

  TKLogProc = procedure(Code: TKLogType; const Text: string);

  TKLog = class(TComponent)
  private
    FHoverTime: Cardinal;
    FListBox: TListBox;
    FLogDirection: TKLogDirection;
    FLogMask: TKLogMask;
    FLogText: string;
    FStatusBar: TStatusBar;
    FStatusCode: TKLogType;
    FStatusPanel: Integer;
    FStatusTimer: TTimer;
    FStatusText: string;
  protected
    procedure ClearStatusBar;
    function LogTypeToText(Code: TKLogType): string;
    procedure SetHoverTime(Value: Cardinal);
    procedure StatusLogTimer(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Log(Code: TKLogType; const Text: string);
    procedure LogStr(const BracketText, Text: string);
    procedure StatusLog(Code: TKLogType; const Text: string);
    procedure StatusLogStr(const BracketText, Text: string);
  published
    property ListBox: TListBox read FListBox write FListBox;
    property LogDirection: TKLogDirection read FLogDirection write FLogDirection default cLogDirectionDef;
    property LogMask: TKLogMask read FLogMask write FLogMask default cLogMaskDef;
    property LogText: string read FLogText;
    property StatusBar: TStatusBar read FStatusBar write FStatusBar;
    property StatusCode: TKLogType read FStatusCode;
    property StatusPanel: Integer read FStatusPanel write FStatusPanel default cStatusPanelDef;
    property StatusText: string read FStatusText;
    property HoverTime: Cardinal read FHoverTime write SetHoverTime default cHoverTimeDef;
  end;

implementation

uses
  SysUtils, KRes;

constructor TKLog.Create(AOwner: TComponent);
begin
  inherited;
  FHoverTime := cHoverTimeDef;
  FListBox := nil;
  FLogDirection := cLogDirectionDef;
  FLogMask := cLogMaskDef;
  FLogText := '';
  FStatusBar := nil;
  FStatusPanel := cStatusPanelDef;
  FStatusTimer := TTimer.Create(Self);
  FStatusTimer.OnTimer := StatusLogTimer;
  FStatusText := '';
  FStatusCode := lgNone;
end;

destructor TKLog.Destroy;
begin
  FListBox := nil;
  FStatusBar := nil;
  FStatusTimer.Free;
  inherited;
end;

procedure TKLog.Clear;
begin
  if FListBox <> nil then
    FListBox.Clear;
  FStatusText := '';
  ClearStatusBar;
end;

procedure TKLog.ClearStatusBar;
begin
  if FStatusBar <> nil then
  begin
    if FStatusPanel < 0 then
      FStatusBar.SimpleText := ''
    else if FStatusPanel < FStatusBar.Panels.Count then
      FStatusBar.Panels[FStatusPanel].Text := '';
  end;
end;

function TKLog.LogTypeToText(Code: TKLogType): string;
begin
  Result := '';
  if [Code, lgAll] * FLogMask <> [] then
    case Code of
      lgError: Result := sLogError;
      lgWarning: Result := sLogWarning;
      lgNote: Result := sLogNote;
      lgHint: Result := sLogHint;
      lgInfo: Result := sLogInfo;
      lgInputError: Result := sLogInputError;
      lgIOError: Result := sLogIOError;
    end;
end;

procedure TKLog.Log(Code: TKLogType; const Text: string);
var
  S: string;
begin
  S := LogTypeToText(Code);
  if (S <> '') or (Code = lgNone) then
    LogStr(S, Text);
end;

procedure TKLog.LogStr(const BracketText, Text: string);
begin
  if BracketText <> '' then
    FLogText := Format('%s: [%s] %s', [FormatDateTime('dd.mm.yy hh:nn:ss', Now), BracketText, Text])
  else
    FLogText:= Format('%s: %s', [FormatDateTime('dd.mm.yy hh:nn:ss', Now), Text]);
  if FListBox <> nil then
  begin
    if FLogDirection = ldAddTop then
    begin
      FListBox.Items.Insert(0, FLogText);
      if not FListBox.MultiSelect then FListBox.ItemIndex := 0;
    end else
    begin
      FListBox.Items.Add(FLogText);
      if not FListBox.MultiSelect then FListBox.ItemIndex := FListBox.Items.Count - 1;
    end
  end;
end;

procedure TKLog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FListBox then
      FListBox := nil
    else if AComponent = FStatusBar then
      FStatusBar := nil;
end;

procedure TKLog.SetHoverTime(Value: Cardinal);
begin
  if Value <> FHoverTime then
  begin
    FHoverTime := Value;
    FStatusTimer.Interval := FHoverTime;
  end;
end;

procedure TKLog.StatusLog(Code: TKLogType; const Text: string);
var
  S: string;
begin
  S := LogTypeToText(Code);
  if (S <> '') or (Code = lgNone) then
  begin
    FStatusCode := Code;
    StatusLogStr(S, Text);
  end;
end;

procedure TKLog.StatusLogStr(const BracketText, Text: string);
begin
  if BracketText <> '' then
    FStatusText := Format('[%s] %s', [BracketText, Text])
  else
    FStatusText := Text;
  if FStatusBar <> nil then
  begin
    if FStatusPanel < 0 then
      FStatusBar.SimpleText := FStatusText
    else if FStatusPanel < FStatusBar.Panels.Count then
      FStatusBar.Panels[FStatusPanel].Text := FStatusText;
  end;
  FStatusTimer.Enabled := False;
  FStatusTimer.Enabled := True;
end;

procedure TKLog.StatusLogTimer(Sender: TObject);
begin
  FStatusTimer.Enabled := False;
  FStatusText := '';
  FStatusCode := lgNone;
  ClearStatusBar;
end;

end.