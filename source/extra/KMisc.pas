unit KMisc;

{$include KControls.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Messages, Classes, Controls, Graphics, ExtCtrls, Forms, StdCtrls,
  KFunctions, KControls;

type
  TKScrollEvent = procedure(Sender: TOBject; Code, Pos: Integer) of object;

  TKPaintPanel = class(TCustomControl)
  private
    FBorderStyle: TBorderStyle;
    FScrollBars: TScrollStyle;
    FOnEraseBkgnd: TNotifyEvent;
    FOnHScroll: TKScrollEvent;
    FOnPaint: TNotifyEvent;
    FOnVScroll: TKScrollEvent;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetScrollBars(Value: TScrollStyle);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure WMEraseBkgnd(var Msg: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
  public
    property Canvas;
  published
    constructor Create(AOwner: TComponent); override;
    property Align;
    property Anchors;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssNone;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnEraseBkgnd: TNotifyEvent read FOnEraseBkgnd write FOnEraseBkgnd;
    property OnExit;
    property OnHScroll: TKScrollEvent read FOnHScroll write FOnHScroll;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnStartDock;
    property OnStartDrag;
    property OnVScroll: TKScrollEvent read FOnVScroll write FOnVScroll;
  end;

  TKHintWindow = class(THintWindow)
  private
    FData: Pointer;
    FOnPaint: TNotifyEvent;
    procedure HintFontChanged(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHintData(Rect: TRect; const AHint: string;
      AData: Pointer); override;
    property Data: Pointer read FData write FData;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TKHintWindowWrapper = class(TComponent)
  private
    FHW: TKHintWindow;
    function GetOnPaint: TNotifyEvent;
    procedure SetOnPaint(AOnPaint: TNotifyEvent);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property HW: TKHintWindow read FHW;
  published
    property OnPaint: TNotifyEvent read GetOnPaint write SetOnPaint;
  end;

  TKBevel = class(TGraphicControl)
  private
    FBevelColor: TColor;
    FBevelWidth: Integer;
    procedure SetBevelColor(AValue: TColor);
    procedure SetBevelWidth(AValue: Integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BevelColor: TColor read FBevelColor write SetBevelColor default clBlack;
    property BevelWidth: Integer read FBevelWidth write SetBevelWidth default 2;
  end;

implementation

uses
  Math, SysUtils, Themes, Types;

{ TKPaintPanel }

constructor TKPaintPanel.Create;
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csOpaque, csDoubleClicks, csReplicatable];
  FBorderStyle := bsSingle;
  FScrollBars := ssNone;
  FOnEraseBkgnd := nil;
  FOnHScroll := nil;
  FOnPaint := nil;
  FOnVScroll := nil;
end;

procedure TKPaintPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
    if FScrollBars in [ssHorizontal, ssBoth] then Style := Style or WS_HSCROLL;
    if FScrollBars in [ssVertical, ssBoth] then Style := Style or WS_VSCROLL;
    WindowClass.style := CS_DBLCLKS;
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

procedure TKPaintPanel.Paint;
begin
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TKPaintPanel.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TKPaintPanel.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
  {$IFDEF FPC}
    UpdateSize;
  {$ELSE}
    RecreateWnd;
  {$ENDIF}
  end;
end;

procedure TKPaintPanel.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if (ComponentState * [csDesigning] <> []) or not Assigned(FOnEraseBkgnd) then
    inherited
  else
  begin
    FOnEraseBkgnd(Self);
    Msg.Result := -1;
  end;
end;

procedure TKPaintPanel.WMHScroll(var Msg: TLMHScroll);
begin
  if Assigned(FOnHScroll) then
    FOnHScroll(Self, Msg.ScrollCode, Msg.Pos);
end;

procedure TKPaintPanel.WMVScroll(var Msg: TLMVScroll);
begin
  if Assigned(FOnVScroll) then
    FOnVScroll(Self, Msg.ScrollCode, Msg.Pos);
end;

{ TKHintWindow }

constructor TKHintWindow.Create(AOwner: TComponent);
begin
  inherited;
  Screen.HintFont.OnChange := HintFontChanged;
  Color := clInfoBk;
  FOnPaint := nil;
  FData := nil;
end;

destructor TKHintWindow.Destroy;
begin
  Screen.HintFont.OnChange := nil;
  inherited;
end;

procedure TKHintWindow.HintFontChanged(Sender: TObject);
begin
  Canvas.Font := Screen.HintFont;
end;

procedure TKHintWindow.Paint;
var
  R: TRect;
begin
  if not Assigned(FOnPaint) then
  begin
    R := ClientRect;
    Inc(R.Left, 2);
    Inc(R.Top, 2);
    DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_LEFT or DT_NOPREFIX or
      DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
  end else
    FOnPaint(Self);
end;

procedure TKHintWindow.ActivateHintData(Rect: TRect; const AHint: string;
  AData: Pointer);
begin
  FData := AData;
  inherited;
end;

{ TKHintWindowWrapper }

constructor TKHintWindowWrapper.Create(AOwner: TComponent);
begin
  inherited;
  FHW := TKHintWindow.Create(Self);
end;

destructor TKHintWindowWrapper.Destroy;
begin
  FHW.Free;
  inherited;
end;

function TKHintWindowWrapper.GetOnPaint: TNotifyEvent;
begin
  Result := FHW.OnPaint;
end;

procedure TKHintWindowWrapper.SetOnPaint(AOnPaint: TNotifyEvent);
begin
  FHW.OnPaint := AOnPaint;
end;

{ TKSplitter }

{ TKBevel }

constructor TKBevel.Create(AOwner: TComponent);
begin
  inherited;
  FBevelColor := clBlack;
  FBevelWidth := 2;
end;

procedure TKBevel.SetBevelColor(AValue: TColor);
begin
  if AValue <> FBevelColor then
  begin
    FBevelColor := AValue;
    Invalidate;
  end;
end;

procedure TKBevel.SetBevelWidth(AValue: Integer);
begin
  if AValue <> FBevelWidth then
  begin
    FBevelWidth := AValue;
    Invalidate;
  end;
end;

procedure TKBevel.Paint;
var
  R, R1: TRect;
begin
  inherited;
  with Canvas do
  begin
    Brush.Color := FBevelColor;
    R := ClientRect;
    R1 := Rect(R.Left, R.Top, R.Right, R.Top + FBevelWidth);
    FillRect(R1);
    R1 := Rect(R.Left, R.Top, R.Left + FBevelWidth, R.Bottom);
    FillRect(R1);
    R1 := Rect(R.Left, R.Bottom - FBevelWidth, R.Right, R.Bottom);
    FillRect(R1);
    R1 := Rect(R.Right - FBevelWidth, R.Top, R.Right, R.Bottom);
    FillRect(R1);
  end;
end;

end.
