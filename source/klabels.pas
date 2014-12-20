{ @abstract(This unit contains some useful label controls)
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

unit KLabels;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Classes, Controls, Forms, Graphics, StdCtrls, KFunctions, KControls
  {$IFDEF USE_THEMES}
  , Themes
   {$IFNDEF FPC}
  , UxTheme
   {$ENDIF}
  {$ENDIF}
  ;

type
  TKGradientLabel = class(TKCustomControl)
  private
    BM: TBitmap;
    FLeftColor,
    FRightColor,
    FDividerColor: TColor;
    FDividerWidth: Integer;
    FColorStep: Integer;
    FCaptionWidth: Integer;
    procedure SetLeftColor(Value: TColor);
    procedure SetRightColor(Value: TColor);
    procedure SetDividerColor(Value: TColor);
    procedure SetDividerWidth(Value: Integer);
    procedure SetColorStep(Value: Integer);
    procedure SetCaptionWidth(Value: Integer);
    procedure WMEraseBkGnd(var Msg: TLMessage); message LM_ERASEBKGND;
    procedure CMTextChanged(var Msg: TLMessage); message CM_TEXTCHANGED;
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Caption;
    property CaptionWidth: Integer read FCaptionWidth write SetCaptionWidth default 50;
    property ColorStep: Integer read FColorStep write SetColorStep default 50;
    property Constraints;
    property DividerColor: TColor read FDividerColor write SetDividerColor default clBlack;
    property DividerWidth: Integer read FDividerWidth write SetDividerWidth default 2;
    property Font;
    property LeftColor: TColor read FLeftColor write SetLeftColor default clNavy;
    property RightColor: TColor read FRightColor write SetRightColor default clBlue;
  end;

  TKLinkLabel = class(TLabel)
  private
    FHotColor: TColor;
    FLinkColor: TColor;
    FShowURLAsHint: Boolean;
    FURL: string;
    procedure CMMouseEnter(var Message: TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TLMessage); message CM_MOUSELEAVE;
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    procedure SetHotColor(Value: TColor);
    procedure SetLinkColor(const Value: TColor);
  protected
    FActiveColor: TColor;
    FMouseInControl: Boolean;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  published
    property HotColor: TColor read FHotColor write SetHotColor default clRed;
    property LinkColor: TColor read FLinkColor write SetLinkColor default clBlue;
    property ShowURLAsHint: Boolean read FShowURLAsHint write FShowURLAsHint;
    property URL: string read FURL write FURL;
  end;

implementation

uses
  Math, SysUtils, KGraphics
  {$IFDEF USE_WINAPI}
  , ShellApi
  {$ENDIF}
  ;

{ TKGradientLabel }

constructor TKGradientLabel.Create(AOwner: TComponent);
begin
  inherited;
  BM := TBitmap.Create;
{$IFNDEF FPC}
  BM.IgnorePalette := True;
{$ENDIF}
  Caption := '';
  FLeftColor := clNavy;
  FRightColor := clBlue;
  FDividerColor := clBlack;
  FDividerWidth := 2;
  Font.Color := clWhite;
  Font.Name := 'Arial';
  Font.Height := 20;
  Font.Style := [fsBold];
  FColorStep := 50;
  Width := 50;
  Height := 30;
  FCaptionWidth := 50;
end;

destructor TKGradientLabel.Destroy;
begin
  inherited;
  BM.Free;
end;

procedure TKGradientLabel.Resize;
begin
  FCaptionWidth := Width;
  Invalidate;
  inherited;
end;

procedure TKGradientLabel.SetDividerColor(Value: TColor);
begin
  if Value <> FDividerColor then
  begin
    FDividerColor := Value;
    Invalidate;
  end;
end;

procedure TKGradientLabel.SetDividerWidth(Value: Integer);
begin
  if Value <> FDividerWidth then
  begin
    FDividerWidth := Value;
    Invalidate;
  end;
end;

procedure TKGradientLabel.SetLeftColor(Value: TColor);
begin
  if Value <> FLeftColor then
  begin
    FLeftColor := Value;
    Invalidate;
  end;
end;

procedure TKGradientLabel.SetRightColor(Value: TColor);
begin
  if Value <> FRightColor then
  begin
    FRightColor := Value;
    Invalidate;
  end;
end;

procedure TKGradientLabel.SetCaptionWidth(Value: Integer);
begin
  if Value <> FCaptionWidth then
  begin
    FCaptionWidth := Value;
    Invalidate;
  end;
end;

procedure TKGradientLabel.SetColorStep(Value: Integer);
begin
  Value := Max(Value, 1);
  Value := Min(Value, 255);
  if Value <> FColorStep then
  begin
    FColorStep := Value;
    Invalidate;
  end;
end;

procedure TKGradientLabel.WMEraseBkGnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

procedure TKGradientLabel.Paint;
var
  J, OldJ, K, Num: Integer;
  L: Byte;
  C, C1: TKColorRec;
  RCnt, GCnt, BCnt: Longint;
  RInc, GInc, BInc: Longint;
  B: Boolean;

  function NumToRGB(Num: Cardinal): TKColorRec;
  begin
    Result.R := Byte(Num shr 16);
    Result.G := Byte(Num shr 8);
    Result.B := Byte(Num);
  end;

  function RGBToNum(Col: TKColorRec): Cardinal;
  begin
    Result := Cardinal(Col.R) shl 16 + Cardinal(Col.G) shl 8 + Col.B;
  end;

begin
  K := Width + 1;
  if K > 0 then
  begin
    BM.Width := K;
    BM.Height := Max(Height - FDividerWidth, 1);
    Num := Max(K div FColorStep, 1);
    with BM.Canvas do
    begin
      if FLeftColor <> FRightColor then
      begin
        C := NumToRGB(FLeftColor);
        C1 := NumToRGB(FRightColor);
        // colors per pixel
        RInc := (Integer(C1.R - C.R) shl 16) div K;
        GInc := (Integer(C1.G - C.G) shl 16) div K;
        Binc := (Integer(C1.B - C.B) shl 16) div K;
        // start colors
        RCnt := C.R shl 16;
        GCnt := C.G shl 16;
        BCnt := C.B shl 16;
        // drawing bar
        Brush.Color := RGBToNum(C);
        OldJ := 0;
        B := False;
        for J := 0 to K do
        begin
          Inc(RCnt, RInc);
          L := Byte(RCnt shr 16);
          if L <> C.R then
          begin
            C.R := L;
            B := True;
          end;
          Inc(GCnt, GInc);
          L := Byte(GCnt shr 16);
          if L <> C.G then
          begin
            C.G := L;
            B := True;
          end;
          Inc(BCnt, BInc);
          L := Byte(BCnt shr 16);
          if L <> C.B then
          begin
            C.B := L;
            B := True;
          end;
          if B and (J mod Num = 0) then
          begin
            FillRect(Rect(OldJ, 0, J, Height));
            Brush.Color := RGBToNum(C);
            OldJ := J;
            B := False;
          end;
        end;
      end else
      begin
        Brush.Color := FLeftColor;
        FillRect(Rect(0, 0, Width, Height));
      end;
      Font := Self.Font;
      SetBkMode(Handle, TRANSPARENT);
      TextOut(Max((FCaptionWidth - TextWidth(Caption)) div 2, 10),
        (Height - Font.Height) div 2, Caption);
    end;
    with Canvas do
    begin
      Draw(0,0, BM);
      if FDividerWidth > 0 then
      begin
        Pen.Color := FDividerColor;
        Brush.Color := FDividerColor;
        J := Max(Height - FDividerWidth, 0);
        Rectangle(0, J, Width, Height);
      end;
    end;
  end;
end;

procedure TKGradientLabel.CMTextChanged(var Msg: TLMessage);
begin
  inherited;
  Invalidate;
end;

{ TKLinkLabel }

constructor TKLinkLabel.Create(AOwner: TComponent);
begin
  inherited;
  FMouseInControl := False;
  FShowURLAsHint := True;
  ShowHint := True;
  FHotColor := clRed;
  FLinkColor := clBlue;
  FActiveColor := FLinkColor;
  FURL := 'http://www.tkweb.eu';
  Caption := FURL;
  Cursor := crHandPoint;
end;

procedure TKLinkLabel.Paint;
begin
  Font.Color := FActiveColor;
  inherited;
end;

procedure TKLinkLabel.Click;
begin
  inherited;
{$IFDEF USE_WINAPI}
  ShellExecute(Application.MainForm.Handle, 'open', PChar(FURL), nil, nil, SW_SHOWNORMAL);
{$ELSE}
  OpenURL(FURL);
{$ENDIF}
end;

procedure TKLinkLabel.SetHotColor(Value: TColor);
begin
  if Value <> FHotColor then
  begin
    FHotColor := Value;
    if FMouseInControl then
      Invalidate;
  end;
end;

procedure TKLinkLabel.SetLinkColor(const Value: TColor);
begin
  if Value <> FLinkColor then
  begin
    FLinkColor := Value;
    if not FMouseInControl then
      Invalidate;
  end;
end;

procedure TKLinkLabel.CMMouseEnter(var Message: TLMessage);
begin
  inherited;
  { Don't draw a border if DragMode <> dmAutomatic since this button is meant to
    be used as a dock client. }
  if not (csDesigning in ComponentState) and not FMouseInControl
    and Enabled and (DragMode <> dmAutomatic) and (GetCapture = 0) then
  begin
    FMouseInControl := True;
    FActiveColor := FHotColor;
    if FShowURLAsHint then
      Hint := FURL;
    Invalidate;
  end;
end;

procedure TKLinkLabel.CMMouseLeave(var Message: TLMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) and FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    FActiveColor := FLinkColor;
    if FShowURLAsHint then
      Hint := '';
    Invalidate;
  end;
end;

procedure TKLinkLabel.CMFontChanged(var Message: TLMessage);
begin
  Invalidate;
end;

end.