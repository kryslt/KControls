{ @abstract(This unit contains progress bar controls)
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

unit KProgress;

{$include KControls.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  Classes, Controls, Forms, KFunctions, KControls
  {$IFDEF USE_THEMES}
  , Themes
   {$IFNDEF FPC}
  , UxTheme
   {$ENDIF}
  {$ENDIF}
  ;

type
  TKPercentProgressBar = class(TKCustomControl)
  private
    FPosition, FMin, FMax: Integer;
    FDrawPercent: Boolean;
    procedure SetDrawPercent(Value: Boolean);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure WMEraseBkGnd(var Msg: TLMessage); message LM_ERASEBKGND;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property BorderWidth;
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DrawPercent: Boolean read FDrawPercent write SetDrawPercent;
    property Enabled;
    property Font;
    property Hint;
    property Constraints;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property ParentShowHint;
    property PopupMenu;
    property Position: Integer read FPosition write SetPosition default 0;
    property ShowHint;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Graphics, Math, SysUtils, Types;

{ TKPercentProgressBar }

constructor TKPercentProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  FDrawPercent := True;
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  Top := 10;
  Left := 10;
  Width := 150;
  Height := 20;
  Color := clBtnFace;
end;

destructor TKPercentProgressBar.Destroy;
begin
  inherited;
end;

procedure TKPercentProgressBar.SetDrawPercent(Value: Boolean);
begin
  if Value <> FDrawPercent then
  begin
    FDrawPercent := Value;
    Invalidate;
  end;
end;

procedure TKPercentProgressBar.SetMax(Value: Integer);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    FPosition := Math.Min(FMax, FPosition);
    Invalidate;
  end;
end;

procedure TKPercentProgressBar.SetMin(Value: Integer);
begin
  if Value <> FMin then
  begin
    FMin := Value;
    FPosition := Math.Max(FMin, FPosition);
    Invalidate;
  end;
end;

procedure TKPercentProgressBar.SetPosition(Value: Integer);
begin
  if Value < FMin then Value := FMin
  else if Value > FMax then Value := FMax;
  if Value <> FPosition then
  begin
    FPosition := Value;
    Invalidate;
  end;
end;

procedure TKPercentProgressBar.WMEraseBkGnd(var Msg: TLMessage);
begin
  Msg.Result := -1;
end;

procedure TKPercentProgressBar.Paint;
var
  R: TRect;
  S: string;
  I, J, X, Y, Percent: Integer;
begin
  with Canvas do
  begin
    X := 0; Y := 0;
    SetBkMode(Handle, TRANSPARENT);
    R := ClientRect;
    DrawEdge(Handle, R, BDR_SUNKENOUTER, BF_RECT);
    if FMin > FMax then
    begin
      I := FMin;
      FMin := FMax;
      FMax := I;
    end
    else if FMin = FMax then Dec(FMin);
    Percent := (FPosition - FMin) * 100 div (FMax - FMin);
    S := IntToStr(Percent) + ' %';
    Brush.Color := Color;
    InflateRect(R, -1, -1);
    FrameRect(R);
    InflateRect(R, -1, -1);
    I := (R.Right - R.Left) * Percent div 100;
    J := R.Right;
    R.Right := R.Left + I;
    Brush.Color := clNavy;
    if FDrawPercent then
    begin
      Font := Self.Font;
      Font.Color := clWindow;
      X := (ClientWidth - TextWidth(S)) div 2;
      Y := (ClientHeight - TextHeight(S)) div 2;
      TextRect(R, X, Y, S);
    end else
      FillRect(R);
    Brush.Color := Color;
    R.Left := R.Right;
    R.Right := J;
    if FDrawPercent then
    begin
      Font.Color := clWindowText;
      TextRect(R, X, Y, S);
    end else
      FillRect(R);
  end;
end;

end.
