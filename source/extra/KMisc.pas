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

  { TKSplitter }

  TKSplitter = class(TGraphicControl)
  private
    FActiveControl: TWinControl;
    FAutoSnap: Boolean;
    FBeveled: Boolean;
    FBrush: TBrush;
    FControl: TControl;
    FDownPos: TPoint;
    FLineDC: HDC;
    FLineVisible: Boolean;
    FMinSize: NaturalNumber;
    FMaxSize: Integer;
    FNewSize: Integer;
    FOldKeyDown: TKeyEvent;
    FOldSize: Integer;
    FPrevBrush: HBrush;
    FResizeStyle: TResizeStyle;
    FSplit: Integer;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    procedure AllocateLineDC;
    procedure CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure DrawLine;
    function FindControl: TControl;
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ReleaseLineDC;
    procedure SetBeveled(Value: Boolean);
    procedure UpdateControlSize;
    procedure UpdateSize(X, Y: Integer);
  protected
    function CanResize(var NewSize: Integer): Boolean; reintroduce; virtual;
    function DoCanResize(var NewSize: Integer): Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure RequestAlign; override;
    procedure StopSizing; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
    property Align default alLeft;
    property AutoSnap: Boolean read FAutoSnap write FAutoSnap default True;
    property Beveled: Boolean read FBeveled write SetBeveled default False;
    property Color;
    property Cursor default crHSplit;
    property Constraints;
    property MinSize: NaturalNumber read FMinSize write FMinSize default 30;
    property ParentColor;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle
      default rsPattern;
    property Visible;
    property Width default 3;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
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


{ TKSplitter }

type
  TWinControlAccess = class(TWinControl);

constructor TKSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSnap := True;
  Height := 100;
  Align := alLeft;
  Width := 3;
  Cursor := crHSplit;
  FMinSize := 30;
  FResizeStyle := rsUpdate;
  FOldSize := -1;
end;

destructor TKSplitter.Destroy;
begin
  FBrush.Free;
  inherited Destroy;
end;

procedure TKSplitter.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
{$IFDEF MSWINDOWS}
      FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
{$ENDIF}
{$IFDEF LINUX}
      FBrush.Color := clGray;
{$ENDIF}
    end;
    FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
end;

procedure TKSplitter.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(Left, Top);
  if Align in [alLeft, alRight] then
    P.X := Left + FSplit else
    P.Y := Top + FSplit;
  with P do PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;

procedure TKSplitter.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;

function TKSplitter.FindControl: TControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  Result := nil;
  P := Point(Left, Top);
  case Align of
    alLeft: Dec(P.X);
    alRight: Inc(P.X, Width);
    alTop: Dec(P.Y);
    alBottom: Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if Result.Visible and Result.Enabled then
    begin
      R := Result.BoundsRect;
      if (R.Right - R.Left) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);
      if PtInRect(R, P) then Exit;
    end;
  end;
  Result := nil;
end;

procedure TKSplitter.RequestAlign;
begin
  inherited RequestAlign;
  if (Cursor <> crVSplit) and (Cursor <> crHSplit) then Exit;
  if Align in [alBottom, alTop] then
    Cursor := crVSplit
  else
    Cursor := crHSplit;
end;

procedure TKSplitter.Paint;
const
  XorColor = $00FFD8CE;
var
  FrameBrush: HBRUSH;
  R: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  if Beveled then
  begin
    if Align in [alLeft, alRight] then
      InflateRect(R, -1, 2) else
      InflateRect(R, 2, -1);
    OffsetRect(R, 1, 1);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
    OffsetRect(R, -2, -2);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
  end;
  if csDesigning in ComponentState then
    { Draw outline }
    with Canvas do
    begin
      Pen.Style := psDot;
      Pen.Mode := pmXor;
      Pen.Color := XorColor;
      Brush.Style := bsClear;
      Rectangle(0, 0, ClientWidth, ClientHeight);
    end;
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

function TKSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := CanResize(NewSize);
  if Result and (NewSize <= MinSize) and FAutoSnap then
    NewSize := 0;
end;

function TKSplitter.CanResize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanResize) then FOnCanResize(Self, NewSize, Result);
end;

procedure TKSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FControl := FindControl;
    FDownPos := Point(X, Y);
    if Assigned(FControl) then
    begin
      if Align in [alLeft, alRight] then
      begin
        FMaxSize := Parent.ClientWidth - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Visible and (Align in [alLeft, alRight]) then Dec(FMaxSize, Width);
        Inc(FMaxSize, FControl.Width);
      end
      else
      begin
        FMaxSize := Parent.ClientHeight - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alTop, alBottom] then Dec(FMaxSize, Height);
        Inc(FMaxSize, FControl.Height);
      end;
      UpdateSize(X, Y);
      AllocateLineDC;
      with ValidParentForm(Self) do
        if ActiveControl <> nil then
        begin
          FActiveControl := ActiveControl;
          FOldKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
          TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
        end;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TKSplitter.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case Align of
      alLeft: FControl.Width := FNewSize;
      alTop: FControl.Height := FNewSize;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - FNewSize);
            FControl.Width := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - FNewSize);
            FControl.Height := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    Update;
    if Assigned(FOnMoved) then FOnMoved(Self);
    FOldSize := FNewSize;
  end;
end;

procedure TKSplitter.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case Align of
    alLeft: S := FControl.Width + Split;
    alRight: S := FControl.Width - Split;
    alTop: S := FControl.Height + Split;
    alBottom: S := FControl.Height - Split;
  end;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if Align in [alRight, alBottom] then
      S := S - NewSize else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

procedure TKSplitter.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

procedure TKSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSize, Split: Integer;
begin
  inherited;
  if (ssLeft in Shift) and Assigned(FControl) then
  begin
    CalcSplitSize(X, Y, NewSize, Split);
    if DoCanResize(NewSize) then
    begin
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      FNewSize := NewSize;
      FSplit := Split;
      if ResizeStyle = rsUpdate then UpdateControlSize;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TKSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Assigned(FControl) then
  begin
    if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    UpdateControlSize;
    StopSizing;
  end;
end;

procedure TKSplitter.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
  else if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TKSplitter.SetBeveled(Value: Boolean);
begin
  FBeveled := Value;
  Repaint;
end;

procedure TKSplitter.StopSizing;
begin
  if Assigned(FControl) then
  begin
    if FLineVisible then DrawLine;
    FControl := nil;
    ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      TWinControlAccess(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

end.
