program KMemoImeDemo;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, Controls, StdCtrls, ExtCtrls, SysUtils, Classes, LCLType,
  LMessages, kmemo;

type
  TLogProc = procedure(const S: string) of object;

  TDebugKMemo = class(TKMemo)
  private
    FLog: TLogProc;
    procedure Log(const S: string);
    procedure LMIMComposition(var Msg: TLMessage); message LM_IM_COMPOSITION;
  protected
  {$IFDEF FPC}
    procedure UTF8KeyPress(var Key: TUTF8Char); override;
  {$ELSE}
    procedure KeyPress(var Key: Char); override;
  {$ENDIF}
  public
    property OnDebugLog: TLogProc read FLog write FLog;
  end;

  TMainForm = class(TForm)
  private
    FMemo: TDebugKMemo;
    FTextView: TMemo;
    FLogView: TMemo;
    procedure AddLog(const S: string);
    procedure MemoChanged(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure FillClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

procedure TDebugKMemo.Log(const S: string);
begin
  if Assigned(FLog) then
    FLog(S);
end;

procedure TDebugKMemo.LMIMComposition(var Msg: TLMessage);
var
  IMText: string;
  Flags: PtrInt;
begin
  Flags := Msg.WParam;
  if Msg.LParam <> 0 then
    IMText := PChar(Msg.LParam)
  else
    IMText := '';
  Log(Format('LM_IM_COMPOSITION before flags=%d text="%s" sel=%d..%d', [
    Flags, IMText, SelStart, SelEnd
  ]));
  inherited LMIMComposition(Msg);
  Log(Format('LM_IM_COMPOSITION after  flags=%d result=%d sel=%d..%d text="%s"', [
    Flags, Msg.Result, SelStart, SelEnd, Text
  ]));
end;

{$IFDEF FPC}
procedure TDebugKMemo.UTF8KeyPress(var Key: TUTF8Char);
begin
  Log(Format('UTF8KeyPress before key="%s" len=%d sel=%d..%d', [
    Key, Length(Key), SelStart, SelEnd
  ]));
  inherited UTF8KeyPress(Key);
  Log(Format('UTF8KeyPress after  key="%s" len=%d sel=%d..%d text="%s"', [
    Key, Length(Key), SelStart, SelEnd, Text
  ]));
end;
{$ELSE}
procedure TDebugKMemo.KeyPress(var Key: Char);
begin
  Log(Format('KeyPress before key="%s" sel=%d..%d', [
    Key, SelStart, SelEnd
  ]));
  inherited KeyPress(Key);
  Log(Format('KeyPress after  key="%s" sel=%d..%d text="%s"', [
    Key, SelStart, SelEnd, Text
  ]));
end;
{$ENDIF}

constructor TMainForm.Create(TheOwner: TComponent);
var
  TopPanel, ButtonPanel: TPanel;
  Info: TLabel;
  ClearButton, FillButton: TButton;
begin
  inherited Create(TheOwner);
  Caption := 'KMemo IME input demo';
  Width := 980;
  Height := 720;
  Position := poScreenCenter;

  TopPanel := TPanel.Create(Self);
  TopPanel.Parent := Self;
  TopPanel.Align := alTop;
  TopPanel.Height := 88;
  TopPanel.BevelOuter := bvNone;

  Info := TLabel.Create(Self);
  Info.Parent := TopPanel;
  Info.Align := alClient;
  Info.BorderSpacing.Around := 8;
  Info.WordWrap := True;
  Info.Caption :=
    'KMemo IME test. Click the KMemo box and type: "한글은 잘되나". ' +
    'Expected text is shown by KMemo.Text below the editor. The event log records ' +
    'LM_IM_COMPOSITION and UTF8KeyPress order.';

  ButtonPanel := TPanel.Create(Self);
  ButtonPanel.Parent := TopPanel;
  ButtonPanel.Align := alRight;
  ButtonPanel.Width := 180;
  ButtonPanel.BevelOuter := bvNone;

  ClearButton := TButton.Create(Self);
  ClearButton.Parent := ButtonPanel;
  ClearButton.Align := alTop;
  ClearButton.Height := 32;
  ClearButton.Caption := 'Clear';
  ClearButton.OnClick := @ClearClick;

  FillButton := TButton.Create(Self);
  FillButton.Parent := ButtonPanel;
  FillButton.Align := alTop;
  FillButton.Height := 32;
  FillButton.Caption := 'Set expected text';
  FillButton.OnClick := @FillClick;

  FMemo := TDebugKMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.Align := alTop;
  FMemo.Height := 180;
  FMemo.TabOrder := 0;
  FMemo.ScrollBars := ssAutoBoth;
  FMemo.OnDebugLog := @AddLog;
  FMemo.OnChange := @MemoChanged;

  FTextView := TMemo.Create(Self);
  FTextView.Parent := Self;
  FTextView.Align := alTop;
  FTextView.Height := 92;
  FTextView.ReadOnly := True;
  FTextView.ScrollBars := ssAutoVertical;
  FTextView.Lines.Add('KMemo.Text will appear here.');

  FLogView := TMemo.Create(Self);
  FLogView.Parent := Self;
  FLogView.Align := alClient;
  FLogView.ReadOnly := True;
  FLogView.ScrollBars := ssAutoBoth;
  FLogView.Lines.Add('Event log');

  ActiveControl := FMemo;
end;

procedure TMainForm.AddLog(const S: string);
begin
  FLogView.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + '  ' + S);
  FLogView.SelStart := Length(FLogView.Text);
end;

procedure TMainForm.MemoChanged(Sender: TObject);
begin
  FTextView.Lines.Text := 'KMemo.Text:' + LineEnding + FMemo.Text;
  AddLog(Format('OnChange text="%s" sel=%d..%d', [FMemo.Text, FMemo.SelStart, FMemo.SelEnd]));
end;

procedure TMainForm.ClearClick(Sender: TObject);
begin
  FMemo.Text := '';
  FTextView.Clear;
  FLogView.Clear;
  FLogView.Lines.Add('Event log');
  FMemo.SetFocus;
end;

procedure TMainForm.FillClick(Sender: TObject);
begin
  FMemo.Text := '한글은 잘되나';
  FMemo.SelStart := Length('한글은 잘되나');
  FMemo.SetFocus;
end;

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
