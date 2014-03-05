unit KVentilator;

{$include KControls.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  SysUtils, Classes, Controls, Windows, Messages, Graphics, ExtCtrls;

type
  TPointArray = array[1..7] of TPoint;

  TKVentilator = class(TCustomControl)
  private
    FAngle: Double;
    FReverse: Boolean;
    FRun: Boolean;
    FBackgroundColor: TColor;
    FBladePoints: TPointArray;
    FBm: TBitmap;
    FTimer: TTimer;
    FU1, FU2, FU3, FU4: Double;
    FAlpha, FAlphaBack, FTime: Double;
    procedure SetAngle(AValue: Double);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetReverse(AValue: Boolean);
    procedure SetRun(AValue: Boolean);
    procedure ApplyRuntime;
    procedure Timer(Sender: TObject);
  protected
    procedure DoPaint;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Angle: Double read FAngle write SetAngle nodefault;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
    property Reverse: Boolean read FReverse write SetReverse default False;
    property Run: Boolean read FRun write SetRun default False;
    property ShowHint;
  end;

function DefaultBladePoints: TPointArray;

implementation

uses
  Math, KFunctions;

const
  Voltage = PI / 3.1;
  TR = 1000;
  TD = 2000;

function DefaultBladePoints: TPointArray;
begin
  Result[1] := Point(0, 0);
  Result[2] := Point(-6, -8);
  Result[3] := Point(-6, -33);
  Result[4] := Point(16, -37);
  Result[5] := Point(24, -35);
  Result[6] := Point(22, -20);
  Result[7] := Point(0, 0);
end;

{ TKVentilator }

constructor TKVentilator.Create(AOwner: TComponent);
begin
  inherited;
  FAngle := 0;
  FBackgroundColor := clBtnFace;
  Width := 90;
  Height := 90;
  Constraints.MinWidth := 20;
  Constraints.MinHeight := 20;
  FBladePoints := DefaultBladePoints;
  FBm := TBitmap.Create;
  FBm.Width := 90;
  FBm.Height := 90;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := True;
  FTimer.OnTimer := Timer;
  FTimer.Interval := 50;
  FReverse := False;
  FRun := False;
  FAlpha := 0;
  FAlphaBack := 0;
  FU1 := 0;
  FU2 := 0;
  FU3 := 0;
  FU4 := 0;
  FTime := 0;
end;

destructor TKVentilator.Destroy;
begin
  inherited;
  FBm.Free;
end;

procedure TKVentilator.SetAngle(AValue: Double);
begin
  if FAngle <> AValue then
  begin
    FAngle := MinMax(AValue, 0, 2 * PI);
    Invalidate;
  end;
end;

procedure TKVentilator.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Invalidate;
  end;
end;

procedure TKVentilator.SetReverse(AValue: Boolean);
begin
  if AValue <> FReverse then
  begin
    FReverse := AValue;
    ApplyRuntime;
  end;
end;

procedure TKVentilator.SetRun(AValue: Boolean);
begin
  if AValue <> FRun then
  begin
    FRun := AValue;
    ApplyRuntime;
  end;
end;

procedure TKVentilator.ApplyRuntime;
begin
  if FRun then
  begin
    if not FReverse then
    begin
      FTime := TR * ln(1 - Min(-FAlpha, Voltage - 0.1) / Voltage) / 5;
      FU1 := Voltage;
      if FAlpha < 0 then FU4 := Voltage;
      FU2 := 0;
      FU3 := 0;
    end else
    begin
      FTime := TR * ln(1 - Min(FAlpha, Voltage - 0.1) / Voltage) / 5;
      FU3 := Voltage;
      if FAlpha > 0 then FU2 := Voltage;
      FU4 := 0;
      FU1 := 0;
    end;
  end else
  begin
{    if not Reverse then
    begin
    end else
    begin
      if (FU3 <> 0) or (FAlpha <= -Voltage) then
      begin
        FU3 := 0;
        if FU2 = 0 then FU4 := Voltage;
      end;
    end;}
    if FAlpha > 0 then begin FU2 := Voltage; FU4 := 0 end;
    if FAlpha < 0 then begin FU4 := Voltage; FU2 := 0 end;
    FU1 := 0;
    FU3 := 0;
  end;
end;

procedure TKVentilator.Timer(Sender: TObject);
var
  A: Double;
begin
  if ((FU1 <> 0) and (FAlpha < Voltage)) or
   ((FU3 <> 0) and (FAlpha > -Voltage)) then
    FTime := FTime + 5
  else
  begin
    FU1 := 0;
    FU3 := 0;
  end;
  FAlpha := FAlpha + FU1 * exp(-FTime / TR) * (1 - exp(-10 / TR)) - FU2 * 5 / TD
                  - (FU3 * exp(-FTime / TR) * (1 - exp(-10 / TR)) - FU4 * 5 / TD);
  if (FU2 <> 0) and (FAlpha < 0) then
  begin
    FU2 := 0;
    if FU3 = 0 then
      FAlpha := 0;
  end;
  if (FU4 <> 0) and (FAlpha > 0) then
  begin
    FU4 := 0;
    if FU1 = 0 then
      FAlpha := 0;
  end;
  if FAlpha <> 0 then
  begin
    FAngle := FAngle + FAlpha;
    A := Trunc(FAngle / (2 * PI));
    FAngle := FAngle - A * 2 * PI;
    Invalidate;
  end;
  FAlphaBack := FAlpha;
end;

procedure TKVentilator.DoPaint;
var
  R: TRect;
  CX, CY, I: Integer;
  Poly1, Poly2, Poly3: TPointArray;
  P1, P2, P3: TPoint;

  procedure Transform(var Poly: TPointArray; var P: TPoint; AAngle: Double);
  var
    I, X, Y: Integer;
    SinA, CosA: Double;
  begin
    SinA := Sin(AAngle);
    CosA := Cos(AAngle);
    for I := 1 to 7 do
    begin
      X := Poly[I].X;
      Y := Poly[I].Y;
      Poly[I].X := Round(X * CosA - Y * SinA);
      Poly[I].Y := Round(X * SinA + Y * CosA);
    end;
    X := P.X;
    Y := P.Y;
    P.X := Round(X * CosA - Y * SinA);
    P.Y := Round(X * SinA + Y * CosA);
  end;

begin
  inherited;
  with FBm.Canvas do
  begin
    R := ClientRect;
    Brush.Color := FBackgroundColor;
    FillRect(R);
    Pen.Width := 3;
    Pen.Color := clBlack;
    InflateRect(R, -1, -1);
    CX := R.Left + ClientWidth div 2 - 1;
    CY := R.Top + ClientHeight div 2 - 1;
    Arc(R.Left, R.Top, R.Right, R.Bottom, R.Left, CY, R.Left, CY);
    Pen.Width := 1;
    for I := 1 to 7 do Poly1[I] := FBladePoints[I];
    for I := 1 to 7 do Poly2[I] := Poly1[I];
    for I := 1 to 7 do Poly3[I] := Poly1[I];
    P1 := Point(0, -5); P2 := P1; P3 := P1;
    Transform(Poly1, P1, FAngle);
    Transform(Poly2, P2, FAngle + 2 * PI / 3);
    Transform(Poly3, P3, FAngle + 4 * PI / 3);
    for I := 1 to 7 do begin Inc(Poly1[I].X, CX); Inc(Poly1[I].Y, CY) end;
    for I := 1 to 7 do begin Inc(Poly2[I].X, CX); Inc(Poly2[I].Y, CY) end;
    for I := 1 to 7 do begin Inc(Poly3[I].X, CX); Inc(Poly3[I].Y, CY) end;
    Inc(P1.X, CX); Inc(P1.Y, CY);
    Inc(P2.X, CX); Inc(P2.Y, CY);
    Inc(P3.X, CX); Inc(P3.Y, CY);
    PolyBezier(Poly1);
    PolyBezier(Poly2);
    PolyBezier(Poly3);
    Brush.Color := clGray;;
    FloodFill(P1.X, P1.Y, clBlack, fsBorder);
    FloodFill(P2.X, P2.Y, clBlack, fsBorder);
    FloodFill(P3.X, P3.Y, clBlack, fsBorder);
    Brush.Color := clBlack;
    Ellipse(CX - 5, CY - 5, CX + 5, CY + 5);
  end;
  Canvas.Draw(0, 0, FBm);
end;

procedure TKVentilator.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  DoPaint;
  Msg.Result := -1;
end;

procedure TKVentilator.Resize;
var
  I: Integer;
  BP: TPointArray;
begin
  inherited;
  ClientHeight := ClientWidth;
  BP := DefaultBladePoints;
  for I := 1 to 7 do
  begin
    FBladePoints[I].X := Round(BP[I].X / 90 * ClientWidth);
    FBladePoints[I].Y := Round(BP[I].Y / 90 * ClientWidth);
  end;
  FBm.Width := ClientWidth;
  FBm.Height := ClientHeight;
  Invalidate;  
end;

end.
