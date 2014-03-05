unit KButtons;

{$include KControls.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Messages, Classes, Controls, Graphics, ActnList, Dialogs, StdCtrls,
  Themes;

type
  TKColorButtonStateField = (cbsPressed, cbsMouseCapture, cbsFocused, cbsLostFocus, cbsHot);

  TKColorButtonState = set of TKColorButtonStateField;

  TKColorButton = class(TButtonControl)
  private
    FDlgColor: TColor;
    FColorDlgOptions: TColorDialogOptions;
    FCanvas: TCanvas;
    FState: TKColorButtonState;
    procedure SetDlgColor(Value: TColor);
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFocusChanged(var Msg: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    procedure PaintWindow(DC: HDC); override;
    procedure Paint; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Action;
    property Anchors;
    property DlgColor: TColor read FDlgColor write SetDlgColor default clRed;
    property ColorDlgOptions: TColorDialogOptions read FColorDlgOptions write FColorDlgOptions;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
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
  end;

implementation

uses
  Forms, Types,
  KFunctions;

{ TKColorButton }

constructor TKColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  ControlStyle := [csOpaque, csClickEvents, csDoubleClicks, csCaptureMouse];
  Width := 50;
  Height := 25;
  TabStop := True;
  FDlgColor := clRed;
  FColorDlgOptions := [];
end;

destructor TKColorButton.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TKColorButton.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TKColorButton.Paint;
var
  Ofs, ExOfs: Integer;
  R: TRect;
  C: TColorRef;
  Details: TThemedElementDetails;
begin
  with FCanvas do
  begin
    R := ClientRect;
    Ofs := 0;
    ExOfs := 0;
    if ThemeServices.ThemesEnabled then with ThemeServices do
    begin
      if not Enabled then
        Details := GetElementDetails(tbPushButtonDisabled)
      else if cbsPressed in FState then
        Details := GetElementDetails(tbPushButtonPressed)
      else if FState * [cbsHot, cbsMouseCapture] <> [] then
        Details := GetElementDetails(tbPushButtonHot)
      else if cbsFocused in FState then
        Details := GetElementDetails(tbPushButtonDefaulted)
      else
        Details := GetElementDetails(tbPushButtonNormal);
      InflateRect(R, 1, 1);
      DrawElement(Handle, Details, R);
      InflateRect(R, -1, -1);  
    end else
    begin
      if FState * [cbsFocused, cbsLostFocus] <> [] then
      begin
        Brush.Color := clBlack;
        FrameRect(R);
        ExOfs := 1;
        InflateRect(R, -1, -1);
      end;
      if cbsPressed in FState then
      begin
        Ofs := 1;
        Brush.Color := clBtnFace;
        Pen.Color := clBtnShadow;
        Rectangle(R);
      end else
      begin
        DrawFrameControl(FCanvas.Handle, R, DFC_BUTTON, DFCS_BUTTONPUSH);
      end;
    end;
    C := ColorToRGB(FDlgColor);
    if (GetRValue(C) < 128) and (GetGValue(C) < 128) and
      (GetRValue(C) < 128) then
      Pen.Color := clWhite
    else
      Pen.Color := clBlack;
    Brush.Color := FDlgColor;
    InflateRect(R, -10 + ExOfs, -6 + ExOfs);
    OffsetRect(R, Ofs, Ofs);
    Rectangle(R);
    if not Enabled then
    begin
      MoveTo(R.Left, R.Top);
      LineTo(R.Right, R.Bottom - 1);
      MoveTo(R.Right - 1, R.Top);
      LineTo(R.Left, R.Bottom - 1);
    end;
    if cbsFocused in FState then
    begin
      R := ClientRect;
      InflateRect(R, -4, -4);
      Pen.Color := clWindowFrame;
      Brush.Color := clBtnFace;
      Windows.DrawFocusRect(FCanvas.Handle, R);
    end;
  end;
end;

procedure TKColorButton.Click;
var
  Dlg: TColorDialog;
begin
  Exclude(FState, cbsPressed);
  Dlg := TColorDialog.Create(Self);
  try
    Dlg.Color := FDlgColor;
    Dlg.Options := FColorDlgOptions;
   // Dlg.CustomColors := GetColorList;
    if Dlg.Execute then
    begin
      FDlgColor := Dlg.Color;
//      SetColors(Dlg.CustomColors);
      Invalidate;
    end;
  finally
    Dlg.Free;
  end;
  inherited Click;
end;

procedure TKColorButton.SetDlgColor(Value: TColor);
begin
  if Value <> FDlgColor then
  begin
    FDlgColor := Value;
    Invalidate;
  end;
end;

procedure TKColorButton.WMPaint(var Msg: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

procedure TKColorButton.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  Include(FState, cbsFocused);
  Invalidate;
end;

procedure TKColorButton.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  Exclude(FState, cbsFocused);
  Include(FState, cbsLostFocus);  
  Invalidate;
end;

procedure TKColorButton.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TKColorButton.CMFocusChanged(var Msg: TCMFocusChanged);
begin
  inherited;
  Exclude(FState, cbsLostFocus);
end;

procedure TKColorButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_RETURN then Click;
end;

procedure TKColorButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Enabled and (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    Invalidate;
    FState := FState + [cbsMouseCapture, cbsPressed];
  end;
end;

procedure TKColorButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  if (cbsMouseCapture in FState) then
  begin
    P := Point(X, Y);
    if PtInRect(ClientRect, P) then
    begin
      if not (cbsPressed in FState) then
      begin
        Include(FState, cbsPressed);
        Invalidate;
      end;
    end else
      if cbsPressed in FState then
      begin
        Exclude(FState, cbsPressed);
        Invalidate;
      end;
  end;
end;

procedure TKColorButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if cbsPressed in FState then
  begin
    Exclude(FState, cbsPressed);
    Invalidate;
  end;
  Exclude(FState, cbsMouseCapture);
  inherited;
end;

procedure TKColorButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if Enabled then
    Include(FState, cbsHot);
  Invalidate;
end;

procedure TKColorButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if Enabled then
    Exclude(FState, cbsHot);
  Invalidate;
end;

procedure TKColorButton.CMDialogKey(var Msg: TCMDialogKey);
begin
  with Msg do
    if (CharCode = VK_RETURN) and (cbsFocused in FState) then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TKColorButton.CMDialogChar(var Msg: TCMDialogChar);
begin
  with Msg do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

end.
