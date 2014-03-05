unit KCharts;

interface

{$include KControls.inc}
{$WEAKPACKAGEUNIT ON}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ExtCtrls, Contnrs;

type
  TKCharLabelPosition = (
    lpNone,
    lpAboveMax,
    lpBelowMin,
    lpCenterHorz,
    lpCenterVert
  );

const
  cMinDist = 1E-10;

  csLine = 0;

  cAxisLabelPositionDef = lpAboveMax;

  cAutosizeDef = False;

  cAxisIndentDef = 20;

  cAxisMajorGridDef = 20;

  cAxisMaxDef = 100;

  cAxisMinDef = 0;

  cAxisMinorGridDef = cAxisMajorGridDef / 5;

  cNoScrollCode = -1;

  cBorderStyleDef = bsSingle;

  cScrollBarsDef = ssBoth;

  { Minimum for the @link(TKCustomGrid.ScrollSpeed) property }
  cScrollSpeedMin = 50;
  { Maximum for the @link(TKCustomGrid.ScrollSpeed) property }
  cScrollSpeedMax = 1000;
  { Default value for the @link(TKCustomGrid.ScrollSpeed) property }
  cScrollSpeedDef = 100;

type
  TKChartPaintSettings = record
    Canvas: TCanvas;
    OffsetX,
    OffsetY: Integer;
    StepX,
    StepY: Single;
    Style: Integer;
  end;

  TKChartSeriesStyle = (
    ssLine,
    ssFill,
    ssLineAndFill,
    ss3D
  );

  TKCustomChart = class;

  TKChartAxis = class(TObject)
  private
    FAxisLabel: WideString;
    FAxisLabelPosition: TKCharLabelPosition;
    FAutosize: Boolean;
    FIndent: Integer;
    FFont: TFont;
    FMax: Double;
    FMajorGrid: Double;
    FMin: Double;
    FMinorGrid: Double;
    FFreeSpace: Double;
    FStep: Double;
    procedure FontChanged(Sender: TObject);
    function IsMajorGridStored: Boolean;
    function IsMaxStored: Boolean;
    function IsMinorGridStored: Boolean;
    function IsMinStored: Boolean;
    procedure SetAutosize(Value: Boolean);
    procedure SetAxisLabel(const Value: WideString);
    procedure SetAxisLabelPosition(Value: TKCharLabelPosition);
    procedure SetIndent(Value: Integer);
    procedure SetFont(Value: TFont);
    procedure SetFreeSpace(Value: Double);
    procedure SetMajorGrid(Value: Double);
    procedure SetMax(Value: Double);
    procedure SetMin(Value: Double);
    procedure SetMinorGrid(Value: Double);
    procedure SetStep(Value: Double);
  protected
    FChart: TKCustomChart;
    procedure Update; virtual;
  public
    constructor Create(AChart: TKCustomChart); virtual;
    destructor Destroy; override;
    procedure Paint(const Settings: TKChartPaintSettings); virtual;
    property AxisLabel: WideString read FAxisLabel write SetAxisLabel;
    property AxisLabelPosition: TKCharLabelPosition read FAxisLabelPosition write SetAxisLabelPosition default cAxisLabelPositionDef;
    property Autosize: Boolean read FAutosize write SetAutosize default cAutosizeDef;
    property Indent: Integer read FIndent write SetIndent default cAxisIndentDef;
    property Font: TFont read FFont write SetFont;
    property FreeSpace: Double read FFreeSpace write SetFreeSpace;
    property MajorGrid: Double read FMajorGrid write SetMajorGrid stored IsMajorGridStored;
    property Max: Double read FMax write SetMax stored IsMaxStored;
    property Min: Double read FMin write SetMin stored IsMinStored;
    property MinorGrid: Double read FMinorGrid write SetMinorGrid stored IsMinorGridStored;
    property Step: Double read FStep write SetStep;
  end;

  TKChartSeries = class(TObject)
  private
    FBrushStyle: TBrushStyle;
    FColor: TColor;
    FData: array of Double;
    FIndex: Integer;
    FLineWidth: Integer;
    FStyle: TKChartSeriesStyle;
    function GetData(Index: Integer): Double;
    function GetDataCount: Integer;
    procedure SetBrushStyle(Value: TBrushStyle);
    procedure SetData(Index: Integer; Value: Double);
    procedure SetColor(Value: TColor);
    procedure SetLineWidth(Value: Integer);
    procedure SetStyle(Value: TKChartSeriesStyle);
  protected
    FChart: TKCustomChart;
  public
    constructor Create(AChart: TKCustomChart); virtual;
    destructor Destroy; override;
    procedure Add(Value: Double);
    procedure Clear;
    procedure Paint(const Settings: TKChartPaintSettings; AllData: Boolean); virtual;
    property BrushStyle: TBrushStyle read FBrushStyle write SetBrushStyle;
    property Color: TColor read FColor write SetColor;
    property Data[Index: Integer]: Double read GetData write SetData;
    property DataCount: Integer read GetDataCount;
    property LineWidth: Integer read FLineWidth write SetLineWidth;
    property Style: TKChartSeriesStyle read FStyle write SetStyle;
  end;

  TKCustomChart = class(TCustomControl)
  private
    FBorderStyle: TBorderStyle;
    FExtent: TPoint;
    FHorzAxis: TKChartAxis;
    FPaintedIndex: Integer;
    FPosition: TPoint;
    FScrollBars: TScrollStyle;
    FScrollSpeed: Cardinal;
    FScrollTimer: TTimer;
    FSeries: TObjectList;
    FUpdateLock: Integer;
    FVertAxis: TKChartAxis;
    function GetSeries(Index: Integer): TKChartSeries;
    function GetSeriesCount: Integer;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetScrollSpeed(Value: Cardinal);
    procedure SetSeriesCount(Value: Integer);
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMNCPaint(var Msg: TWMNCPaint); message WM_NCPAINT;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  {$IFDEF USE_THEMES}
    procedure WMThemeChanged(var Msg: TMessage); message WM_THEMECHANGED;
  {$ENDIF}
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
  protected
    function CalcHorzExtent: Integer; virtual;
    function CalcVertExtent: Integer; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetPaintSettings: TKChartPaintSettings;
    procedure Paint; override;
    procedure SafeSetFocus;
    procedure Scroll(CodeHorz, CodeVert, DeltaHorz, DeltaVert: Integer);
    procedure UpdateScrollRange(Horz, Vert, UpdateNeeded: Boolean); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    function CanUpdate: Boolean;
    procedure Clear;
    procedure EndUpdate;
    procedure ScrollBy(DeltaX, DeltaY: Integer);
    function ScrollNeeded(Point: TPoint; out DeltaX, DeltaY: Integer): Boolean; dynamic;
    property HorzAxis: TKChartAxis read FHorzAxis;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default cBorderStyleDef;
    property PaintSettings: TKChartPaintSettings read GetPaintSettings;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default cScrollBarsDef;
    { Specifies how fast the scrolling by timer should be }
    property ScrollSpeed: Cardinal read FScrollSpeed write SetScrollSpeed default cScrollSpeedDef;
    property Series[Index: Integer]: TKChartSeries read GetSeries;
    property SeriesCount: Integer read GetSeriesCount write SetSeriesCount;
    property VertAxis: TKChartAxis read FVertAxis;
  end;

  TKChart = class(TKCustomChart)
  published
    { Inherited property - see Delphi help }
    property Align;
    { Inherited property - see Delphi help }
    property Anchors;
    { See TKCustomChart.@link(TKCustomChart.BorderStyle) for details }
    property BorderStyle;
    { Inherited property - see Delphi help }
    property BorderWidth;
    property Color;
    { Inherited property - see Delphi help }
    property Constraints;
    { Inherited property - see Delphi help }
    property Ctl3D;
    { Inherited property - see Delphi help }
    property DragCursor;
    { Inherited property - see Delphi help }
    property DragKind;
    { Inherited property - see Delphi help }
    property DragMode;
    { Inherited property - see Delphi help }
    property Enabled;
    { Inherited property - see Delphi help }
    property Font;
    { Inherited property - see Delphi help }
    property ParentColor;
    { Inherited property - see Delphi help }
    property ParentFont;
    { Inherited property - see Delphi help }
    property ParentShowHint;
    { Inherited property - see Delphi help }
    property PopupMenu;
    { See TKCustomChart.@link(TKCustomChart.ScrollBars) for details }
    property ScrollBars;
    { See TKCustomChart.@link(TKCustomChart.ScrollSpeed) for details }
    property ScrollSpeed;
    { Inherited property - see Delphi help }
    property ShowHint;
    { Inherited property - see Delphi help }
    property TabOrder;
    { Inherited property - see Delphi help }
    property TabStop default True;
    { Inherited property - see Delphi help }
    property Visible;
    { Inherited property - see Delphi help }
    property OnClick;
    { Inherited property - see Delphi help }
    property OnContextPopup;
    { Inherited property - see Delphi help }
    property OnDblClick;
    { Inherited property - see Delphi help }
    property OnDockDrop;
    { Inherited property - see Delphi help }
    property OnDockOver;
    { Inherited property - see Delphi help }
    property OnDragDrop;
    { Inherited property - see Delphi help }
    property OnDragOver;
    { Inherited property - see Delphi help }
    property OnEndDock;
    { Inherited property - see Delphi help }
    property OnEndDrag;
    { Inherited property - see Delphi help }
    property OnEnter;
    { Inherited property - see Delphi help }
    property OnExit;
    { Inherited property - see Delphi help }
    property OnKeyDown;
    { Inherited property - see Delphi help }
    property OnKeyPress;
    { Inherited property - see Delphi help }
    property OnKeyUp;
    { Inherited property - see Delphi help }
    property OnMouseDown;
    { Inherited property - see Delphi help }
    property OnMouseMove;
    { Inherited property - see Delphi help }
    property OnMouseUp;
    { Inherited property - see Delphi help }
    property OnResize;
    { Inherited property - see Delphi help }
    property OnStartDock;
    { Inherited property - see Delphi help }
    property OnStartDrag;
    { Inherited property - see Delphi help }
    property OnUnDock;
  end;

implementation

uses
{$IFDEF USE_THEMES}
  Themes,
{$ENDIF}
  Math, KFunctions;

const
  cStdColorCount = 14;

  StdColors: array[0..cStdColorCount - 1] of TColor = (
    clRed, clLime, clYellow, clBlue, clFuchsia,
    clAqua, clMoneyGreen, clSkyBlue, clMaroon, clGreen,
    clOlive, clNavy, clPurple, clTeal);

{ TKChartSeries }

constructor TKChartSeries.Create(AChart: TKCustomChart);
begin
  inherited Create;
  FBrushStyle := bsSolid;
  FColor := clNone;
  FData := nil;
  FIndex := 0;
  FLineWidth := 3;
  FChart := AChart;
  FStyle := ssLine;
end;

destructor TKChartSeries.Destroy;
begin
  FData := nil;
  inherited;
end;

procedure TKChartSeries.Add(Value: Double);
var
  L: Integer;
begin
  L := Length(FData);
  SetLength(FData, L + 1);
  FData[L] := Value;
  if FChart.CanUpdate then
  begin
    Paint(FChart.PaintSettings, False);
    FChart.UpdateScrollRange(True, True, False);
  end;
end;

procedure TKChartSeries.Clear;
begin
  if FData <> nil then
  begin
    FData := nil;
    FIndex := 0;
    FChart.Invalidate;
  end;
end;

function TKChartSeries.GetData(Index: Integer): Double;
begin
  if (Index >= 0) and (Index < Length(FData)) then
    Result := FData[Index]
  else
    Result := 0;
end;

function TKChartSeries.GetDataCount: Integer;
begin
  Result := Length(FData);
end;

procedure TKChartSeries.Paint(const Settings: TKChartPaintSettings; AllData: Boolean);
var
  I, L, M, Y: Integer;
  X: Single;
begin
  L := Length(FData);
  if AllData then M := 0 else M := FIndex;
  with Settings, Settings.Canvas do if L > M then
  begin
    Pen.Color := Self.Color;
    Pen.Width := Self.LineWidth;
    X := OffsetX + Round(M * StepX);
    Y := OffsetY;
    MoveTo(Round(X), Y - Round(FData[M] * StepY));
    for I := M to L - 1 do
    begin
      LineTo(Round(X), Y - Round(FData[I] * StepY));
      X := X + StepX;
    end;
    FIndex := L - 1;
  end;
end;

procedure TKChartSeries.SetBrushStyle(Value: TBrushStyle);
begin
  if Value <> FBrushStyle then
  begin
    FBrushStyle := Value;
    if FStyle in [ssFill, ssLineAndFill, ss3D] then
      FChart.Invalidate;
  end;
end;

procedure TKChartSeries.SetData(Index: Integer; Value: Double);
begin
  if Index >= 0 then
    if Index < Length(FData) then
    begin
      FData[Index] := Value;
      FChart.UpdateScrollRange(False, True, False);
    end else
      Add(Value);
end;

procedure TKChartSeries.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    FChart.Invalidate;
  end;
end;

procedure TKChartSeries.SetLineWidth(Value: Integer);
begin
  if Value <> FLineWidth then
  begin
    FLineWidth := Value;
    if FStyle in [ssLine, ssLineAndFill, ss3D] then
      FChart.Invalidate;
  end;
end;

procedure TKChartSeries.SetStyle(Value: TKChartSeriesStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    FChart.Invalidate;
  end;
end;

{ TKCustomChart }

constructor TKCustomChart.Create;
begin
  inherited;
  FBorderStyle := bsSingle;
  FExtent := Point(0, 0);
  FHorzAxis := TKChartAxis.Create(Self);
  FPaintedIndex := 0;
  FPosition := Point(0, 0);
  FScrollBars := cScrollBarsDef;
  FScrollSpeed := cScrollSpeedDef;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := FScrollSpeed;
  FScrollTimer.OnTimer := ScrollTimerHandler;
  FSeries := TObjectList.Create;
  FUpdateLock := 0;
  FVertAxis := TKChartAxis.Create(Self);
end;

destructor TKCustomChart.Destroy;
begin
  FHorzAxis.Free;
  FSeries.Free;
  FVertAxis.Free;
  inherited Destroy;
end;

procedure TKCustomChart.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TKCustomChart.EndUpdate;
begin
  if FUpdateLock > 0 then
  begin
    Dec(FUpdateLock);
    UpdateScrollRange(True, True, True);
  end;
end;

function TKCustomChart.CalcHorzExtent: Integer;
var
  I, L: Integer;
begin
  Result := 0;
  for I := 0 to FSeries.Count - 1 do
  begin
    L := TKChartSeries(FSeries[I]).DataCount;
    if L > Result then Result := L;
  end;
  Result := Round(Result * FHorzAxis.Step + FHorzAxis.FreeSpace);
end;

function TKCustomChart.CalcVertExtent: Integer;
var
  I, L: Integer;
begin
  Result := 0;
  for I := 0 to FSeries.Count - 1 do
  begin
    L := TKChartSeries(FSeries[I]).DataCount;
    if L > Result then Result := L;
  end;
  Result := Round(Result * FHorzAxis.Step + FHorzAxis.FreeSpace);
end;

function TKCustomChart.CanUpdate: Boolean;
begin
  Result := FUpdateLock = 0;
end;

procedure TKCustomChart.Clear;
var
  I: Integer;
begin
  for I := 0 to FSeries.Count - 1 do
    TKChartSeries(FSeries[I]).Clear;
  FPosition := Point(0, 0);
end;

procedure TKCustomChart.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP or WS_CLIPCHILDREN;
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

function TKCustomChart.GetPaintSettings: TKChartPaintSettings;
begin
  Result.Canvas := Canvas;
  Result.OffsetX := 0 - FPosition.X;
  Result.OffsetY := ClientHeight - FPosition.Y;
  Result.StepX := 1;
  Result.StepY := 1;
  Result.Style := csLine;
end;

function TKCustomChart.GetSeries(Index: Integer): TKChartSeries;
begin
  if (Index >= 0) and (Index < FSeries.Count) then
    Result := TKChartSeries(FSeries[Index])
  else
    Result := nil;
end;

function TKCustomChart.GetSeriesCount: Integer;
begin
  Result := FSeries.Count;
end;

procedure TKCustomChart.Paint;
var
  I: Integer;
  Settings: TKChartPaintSettings;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  Settings := PaintSettings;
  for I := 0 to FSeries.Count - 1 do
    TKChartSeries(FSeries[I]).Paint(Settings, True);
end;

procedure TKCustomChart.SafeSetFocus;
begin
  if CanFocus and not (Focused or (ComponentState * [csDesigning, csLoading] <> [])) then
    SetFocus;
end;

procedure TKCustomChart.Scroll(CodeHorz, CodeVert, DeltaHorz, DeltaVert: Integer);

  function Axis(Code: Cardinal; HasScrollBar: Boolean;
    ScrollCode: Cardinal; Delta, AMin, AMax: Integer;
    var Position: Integer): Boolean;
  var
    OldPos, Pos: Integer;
    SI: TScrollInfo;
  begin
    Result := False;
    if HasScrollBar then
    begin
      SI.cbSize := SizeOf(TScrollInfo);
      SI.fMask := SIF_PAGE or SIF_TRACKPOS;
      GetScrollInfo(Handle, Code, SI);
    end;
    Pos := Position;
    OldPos := Pos;
    if Delta <> 0 then
      Inc(Pos, Delta)
    else if HasScrollBar then
    case ScrollCode of
      SB_TOP: Pos := AMin;
      SB_BOTTOM: Pos := AMax;
      SB_LINEUP: Dec(Pos);
      SB_LINEDOWN: Inc(Pos);
      SB_PAGEUP: Dec(Pos, SI.nPage);
      SB_PAGEDOWN: Inc(Pos, SI.nPage);
      SB_THUMBTRACK: Pos := SI.nTrackPos;
    end;
    Pos := MinMax(Pos, AMin, AMax);
    if Pos <> OldPos then
    begin
      if HasScrollBar then
      begin
        SI.nPos := Pos;
        SI.fMask := SIF_POS;
        SetScrollInfo(Handle, Code, SI, True);
      end;
      Position := Pos;
      Result := True;
    end;
  end;

var
  OldPos: TPoint;
begin
  OldPos := FPosition;
  if Axis(SB_HORZ, FScrollBars in [ssHorizontal, ssBoth],
    CodeHorz, DeltaHorz, 0, FExtent.X, FPosition.X) or
    Axis(SB_VERT, FScrollBars in [ssVertical, ssBoth],
    CodeVert, DeltaVert, 0, FExtent.Y, FPosition.Y) then
    ScrollWindowEx(Handle, (OldPos.X - FPosition.X), (OldPos.Y - FPosition.Y),
      nil, nil, 0, nil, SW_INVALIDATE);
end;

procedure TKCustomChart.ScrollBy(DeltaX, DeltaY: Integer);
begin
  Scroll(cNoScrollCode, cNoScrollCode, DeltaX, DeltaY);
end;

function TKCustomChart.ScrollNeeded(Point: TPoint;
  out DeltaX, DeltaY: Integer): Boolean;
begin
  DeltaX := 0;
  DeltaY := 0;
  Result := (DeltaX <> 0) or (DeltaY <> 0);
end;

procedure TKCustomChart.ScrollTimerHandler(Sender: TObject);
var
  DeltaX, DeltaY: Integer;
begin
  if MouseCapture and not Dragging and
    ScrollNeeded(ScreenToClient(Mouse.CursorPos), DeltaX, DeltaY) then
    ScrollBy(DeltaX, DeltaY)
  else
    FScrollTimer.Enabled := False;
end;

procedure TKCustomChart.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TKCustomChart.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TKCustomChart.SetScrollSpeed(Value: Cardinal);
begin
  Value := MinMax(Integer(Value), cScrollSpeedMin, cScrollSpeedMax);
  if Value <> FScrollSpeed then
  begin
    FScrollSpeed := Value;
    FScrollTimer.Enabled := False;
    FScrollTimer.Interval := FScrollSpeed;
  end;
end;

procedure TKCustomChart.SetSeriesCount(Value: Integer);
var
  I, J: Integer;
  CS: TKChartSeries;
begin
  if Value > FSeries.Count then
  begin
    while FSeries.Count < Value do
    begin
      CS := TKChartSeries.Create(Self);
      CS.Color := StdColors[FSeries.Count mod cStdColorCount];
      FSeries.Add(CS);
    end;
    Invalidate;
  end else if Value < FSeries.Count then
  begin
    while FSeries.Count > Value do
      FSeries.Delete(FSeries.Count - 1);
    Invalidate;
  end;
end;

procedure TKCustomChart.UpdateScrollRange(Horz, Vert, UpdateNeeded: Boolean);

  procedure Axis(Code: Cardinal; HasScrollBar: Boolean;
    DataExtent, ClientExtent, Position: Integer);
  var
    I: Integer;
    SI: TScrollInfo;
  begin
    if HasScrollBar then
    begin
      SI.cbSize := SizeOf(TScrollInfo);
      SI.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
      SI.nMin := 0;
      SI.nMax := DataExtent;
      SI.nPage := ClientExtent;
      SI.nPos := Position;
      SetScrollInfo(Handle, Code, SI, True);
    end;
  end;

begin
  if HandleAllocated then
  begin
    if Horz then
    begin
      FExtent.X := CalcHorzExtent;
      Axis(SB_HORZ, FScrollBars in [ssHorizontal, ssBoth], FExtent.X, ClientWidth, FPosition.X);
    end;
    if Vert then
    begin
      FExtent.Y := CalcVertExtent;
      Axis(SB_VERT, FScrollBars in [ssVertical, ssBoth], FExtent.Y, ClientHeight, FPosition.Y);
    end;
  end;
end;

procedure TKCustomChart.WMNCPaint(var Msg: TWMNCPaint);
{$IFDEF USE_THEMES}
var
  R: TRect;
  ExStyle: Integer;
  TempRgn: HRGN;
  BorderWidth,
  BorderHeight: Integer;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  with ThemeServices do if ThemesEnabled then
  begin
    // If theming is enabled and the client edge border is set for the window then prevent the default window proc
    // from painting the old border to avoid flickering.
    ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if (ExStyle and WS_EX_CLIENTEDGE) <> 0 then
    begin
      GetWindowRect(Handle, R);
      // Determine width of the client edge.
      BorderWidth := GetSystemMetrics(SM_CXEDGE);
      BorderHeight := GetSystemMetrics(SM_CYEDGE);
      InflateRect(R, -BorderWidth, -BorderHeight);
      TempRgn := CreateRectRgnIndirect(R);
      // Exclude the border from the message region if there is one. Otherwise just use the inflated
      // window area region.
      if Msg.Rgn <> 1 then
        CombineRgn(TempRgn, Msg.Rgn, TempRgn, RGN_AND);
      DefWindowProc(Handle, Msg.Msg, Integer(TempRgn), 0);
      DeleteObject(TempRgn);
      PaintBorder(Self, True);
    end else
      inherited;
  end else
{$ENDIF}
    inherited;
end;

procedure TKCustomChart.WMHScroll(var Msg: TWMHScroll);
begin
  if Msg.ScrollBar = 0 then
  begin
    SafeSetFocus;
    Scroll(Msg.ScrollCode, cNoScrollCode, 0, 0);
  end else
    inherited;
end;

procedure TKCustomChart.WMSize(var Msg: TWMSize);
begin
  inherited;
  UpdateScrollRange(True, True, True);
end;

{$IFDEF USE_THEMES}
procedure TKCustomChart.WMThemeChanged(var Msg: TMessage);
begin
  inherited;
  ThemeServices.UpdateThemes;
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_VALIDATE or RDW_FRAME);
end;
{$ENDIF}

procedure TKCustomChart.WMVScroll(var Msg: TWMVScroll);
begin
  if Msg.ScrollBar = 0 then
  begin
    SafeSetFocus;
    Scroll(cNoScrollCode, Msg.ScrollCode, 0, 0);
  end else
    inherited;
end;

{ TKChartAxis }

constructor TKChartAxis.Create(AChart: TKCustomChart);
begin
  FAxisLabel := '';
  FAxisLabelPosition := cAxisLabelPositionDef;
  FAutosize := cAutosizeDef;
  FChart := AChart;
  FIndent := cAxisIndentDef;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FMax := cAxisMaxDef;
  FMajorGrid := cAxisMajorGridDef;
  FMin := cAxisMinDef;
  FMinorGrid := cAxisMinorGridDef;
end;

destructor TKChartAxis.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TKChartAxis.FontChanged(Sender: TObject);
begin
  Update;
end;

function TKChartAxis.IsMajorGridStored: Boolean;
begin
  Result := not SameValue(FMajorGrid, cAxisMajorGridDef, cMinDist);
end;

function TKChartAxis.IsMaxStored: Boolean;
begin
  Result := not SameValue(FMax, cAxisMaxDef, cMinDist);
end;

function TKChartAxis.IsMinorGridStored: Boolean;
begin
  Result := not SameValue(FMinorGrid, cAxisMinorGridDef, cMinDist);
end;

function TKChartAxis.IsMinStored: Boolean;
begin
  Result := not SameValue(FMin, cAxisMinDef, cMinDist);
end;

procedure TKChartAxis.Paint(const Settings: TKChartPaintSettings);
var
  Indent: Integer;
begin
  with Settings, Settings.Canvas do
  begin
    if FChart.HorzAxis = Self then
      Indent := FChart.VertAxis.Indent
    else
      Indent := FChart.HorzAxis.Indent;
  end;
end;

procedure TKChartAxis.SetAutosize(Value: Boolean);
begin
  if Value <> FAutosize then
  begin
    FAutosize := Value;
    Update;
  end;
end;

procedure TKChartAxis.SetAxisLabel(const Value: WideString);
begin
  if Value <> FAxisLabel then
  begin
    FAxisLabel := Value;
    Update;
  end;
end;

procedure TKChartAxis.SetAxisLabelPosition(Value: TKCharLabelPosition);
begin
  if Value <> FAxisLabelPosition then
  begin
    FAxisLabelPosition := Value;
    Update;
  end;
end;

procedure TKChartAxis.SetIndent(Value: Integer);
begin
  if Value <> FIndent then
  begin
    FIndent := Value;
    Update;
  end;
end;

procedure TKChartAxis.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TKChartAxis.SetFreeSpace(Value: Double);
begin
  if not SameValue(Value, FFreeSpace, cMinDist) then
  begin
    FFreeSpace := Value;
    Update;
  end;
end;

procedure TKChartAxis.SetMajorGrid(Value: Double);
begin
  if not SameValue(Value, FMajorGrid, cMinDist) then
  begin
    FMajorGrid := Value;
    Update;
  end;
end;

procedure TKChartAxis.SetMax(Value: Double);
begin
  if not SameValue(Value, FMax, cMinDist) then
  begin
    FMax := Value;
    Update;
  end;
end;

procedure TKChartAxis.SetMin(Value: Double);
begin
  if not SameValue(Value, FMin, cMinDist) then
  begin
    FMin := Value;
    Update;
  end;
end;

procedure TKChartAxis.SetMinorGrid(Value: Double);
begin
  if not SameValue(Value, FMinorGrid, cMinDist) then
  begin
    FMinorGrid := Value;
    Update;
  end;
end;

procedure TKChartAxis.SetStep(Value: Double);
begin
  if not SameValue(Value, FStep, cMinDist) then
  begin
    FStep := Value;
    Update;
  end;
end;

procedure TKChartAxis.Update;
begin
  FChart.UpdateScrollRange(Self = FChart.HorzAxis, Self = FChart.VertAxis, True);
end;

end.
