{ @abstract(This unit contains modified TPageControl component)
  @author(Tomas Krysl (tk@@tkweb.eu))
  @created(20 Oct 2001)
  @lastmod(12 Feb 2014)

  Copyright © 2001-2014 Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. All redistributions
  of the original or modified source code must retain the original copyright
  notice. The Author accepts no liability for any damage that may result
  from using this code. }

unit kpagecontrol;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  Classes, Controls, Graphics, ExtCtrls, Forms, StdCtrls, ComCtrls, Contnrs, ImgList,
  KFunctions, KControls;

const
  cDefaultTabPadding = 2;
  cDefaultTabHeight = 24;
  cDefaultTabPosition = tpTop;
  cDefaultTabWidth = 0;

  cDefaultHotTop = clGradientActiveCaption;
  cDefaultHotBottom = clGradientInactiveCaption;
  cDefaultNormalTop = clBtnFace;
  cDefaultNormalBottom = clBtnHighlight;
  cDefaultNormalText = clBtnText;
  cDefaultSelectedTop = clHighlight;
  cDefaultSelectedBottom = clHotLight;
  cDefaultSelectedText = clHighlightText;

  ciHotTop = TKColorIndex(0);
  ciHotBottom = TKColorIndex(1);
  ciNormalTop = TKColorIndex(2);
  ciNormalBottom = TKColorIndex(3);
  ciNormalText = TKColorIndex(4);
  ciSelectedTop = TKColorIndex(5);
  ciSelectedBottom = TKColorIndex(6);
  ciSelectedText = TKColorIndex(7);

  ciMaxIndex = ciSelectedText;

type
  TKTabColors = class(TKCustomColors)
  protected
    { Returns color specification structure for given index. }
    function GetColorSpec(Index: TKColorIndex): TKColorSpec; override;
    { Returns maximum color index. }
    function GetMaxIndex: Integer; override;
  public
    constructor Create(AControl: TKCustomControl); override;
  published
    property HotTop: TColor index ciHotTop read GetColor write SetColor default cDefaultHotTop;
    property HotBottom: TColor index ciHotBottom read GetColor write SetColor default cDefaultHotBottom;
    property NormalTop: TColor index ciNormalTop read GetColor write SetColor default cDefaultNormalTop;
    property NormalBottom: TColor index ciNormalBottom read GetColor write SetColor default cDefaultNormalBottom;
    property NormalText: TColor index ciNormalText read GetColor write SetColor default cDefaultNormalText;
    property SelectedTop: TColor index ciSelectedTop read GetColor write SetColor default cDefaultSelectedTop;
    property SelectedBottom: TColor index ciSelectedBottom read GetColor write SetColor default cDefaultSelectedBottom;
    property SelectedText: TColor index ciSelectedText read GetColor write SetColor default cDefaultSelectedText;
  end;

  TKTabState = (tsNormal, tsHot, tsSelected);

  TKTabPaintInfo = record
    TabRect: TRect;
    ImageRect: TRect;
    TextRect: TRect;
    CloseRect: TRect;
  end;

  TKCustomPageControl = class;

  TKTabPanel = class(TKCustomControl)
  private
    FCloseButton: TGraphic;
    FCloseButtonIndex: Integer;
    FColors: TKTabColors;
    FPadding: Integer;
    FPageControl: TKCustomPageControl;
    function GetTabs(Index: Integer): string;
    procedure SetCloseButton(const Value: TGraphic);
    procedure SetCloseButtonIndex(const Value: Integer);
    procedure SetColors(const Value: TKTabColors);
    procedure SetPadding(const Value: Integer);
    procedure SetPageControl(const Value: TKCustomPageControl);
  protected
    FMouseIndex: Integer;
    FMouseInCloseButton: Boolean;
    FPageToClose: Integer;
    function GetTabPaintInfo(ACanvas: TCanvas; ATabIndex: Integer; out Info: TKTabPaintInfo): Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseFormLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseOver(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintTab(ACanvas: TCanvas; ATabIndex: Integer);
    procedure PaintTabBackground(ACanvas: TCanvas; const ARect: TRect; AState: TKTabState); virtual;
    procedure PaintTabCloseButton(ACanvas: TCanvas; const ARect: TRect); virtual;
    procedure PaintTabImage(ACanvas: TCanvas; const ARect: TRect; ATabIndex: Integer); virtual;
    procedure PaintTabText(ACanvas: TCanvas; const ARect: TRect; ATabIndex: Integer; AState: TKTabState); virtual;
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    procedure UpdateTabPanel; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CloseButton: TGraphic read FCloseButton write SetCloseButton;
    property CloseButtonIndex: Integer read FCloseButtonIndex write SetCloseButtonIndex;
    property Colors: TKTabColors read FColors write SetColors;
    property Padding: Integer read FPadding write SetPadding default cDefaultTabPadding;
    property PageControl: TKCustomPageControl read FPageControl write SetPageControl;
    property Tabs[Index: Integer]: string read GetTabs;
  end;

  TKTabSheet = class(TWinControl)
  private
    FImageIndex: TImageIndex;
    FPageControl: TKCustomPageControl;
    FTabVisible: Boolean;
    FTabShowing: Boolean;
    FHighlighted: Boolean;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    function GetPageIndex: Integer;
    procedure SetHighlighted(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetPageControl(APageControl: TKCustomPageControl);
    procedure SetPageIndex(Value: Integer);
    procedure SetTabVisible(Value: Boolean);
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure ReadState(Reader: TReader); override;
    procedure UpdateControlOriginalParentSize(AControl: TControl; var AOriginalParentSize: TPoint); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PageControl: TKCustomPageControl read FPageControl write SetPageControl;
  published
    property BorderWidth;
    property Caption;
    property DoubleBuffered;
    property DragMode;
    property Enabled;
    property Font;
    property Height stored False;
    property Highlighted: Boolean read FHighlighted write SetHighlighted default False;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default 0;
    property Left stored False;
    property Constraints;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabVisible: Boolean read FTabVisible write SetTabVisible default True;
    property Top stored False;
    property Touch;
    property Visible stored False;
    property Width stored False;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnStartDrag;
  end;

  TKTabSheets = class(TObjectList)
  private
    function GetItem(Index: Integer): TKTabSheet;
    procedure SetItem(Index: Integer; const Value: TKTabSheet);
  published
  public
    function Add(AItem: TKTabSheet): Integer;
    function IndexOf(AItem: TKTabSheet): Integer;
    property Items[Index: Integer]: TKTabSheet read GetItem write SetItem; default;
  end;

  TKCustomPageControl = class(TKCustomControl)
  private
    FActivePageIndex: Integer;
    FHotTrack: Boolean;
    FImages: TImageList;
    FPages: TKTabSheets;
    FTabHeight: Integer;
    FTabPanel: TKTabPanel;
    FTabPosition: TTabPosition;
    FTabWidth: Integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TTabChangingEvent;
    FOnGetImageIndex: TTabGetImageEvent;
    FDisabledImages: TImageList;
    function GetActivePage: TKTabSheet;
    function GetPage(Index: Integer): TKTabSheet;
    function GetPageCount: Integer;
    procedure SetActivePageIndex(const Value: Integer);
    procedure SetImages(const Value: TImageList);
    procedure SetTabPosition(const Value: TTabPosition);
    procedure SetTabHeight(const Value: Integer);
    procedure SetTabPanel(const Value: TKTabPanel);
    procedure SetTabWidth(const Value: Integer);
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDockNotification(var Message: TCMDockNotification); message CM_DOCKNOTIFICATION;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
    procedure WMEraseBkgnd(var Msg: TLMessage); message LM_ERASEBKGND;
    procedure SetHotTrack(const Value: Boolean);
    procedure SetDisabledImages(const Value: TImageList);
  protected
    FDeletingPage: Boolean;
    FNewDockSheet: TKTabSheet;
    FUndockingPage: TKTabSheet;
    function CanChange: Boolean; virtual;
    procedure Change; virtual;
    procedure ChangeActivePage(Page: TKTabSheet); virtual;
    procedure InsertPage(Page: TKTabSheet); virtual;
    procedure DeletePage(Index: Integer); virtual;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    function GetImageIndex(TabIndex: Integer): Integer; virtual;
    function GetPageFromDockClient(Client: TControl): TKTabSheet;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure MovePage(CurIndex, NewIndex: Integer);
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    procedure RemovePage(Page: TKTabSheet); virtual;
    procedure SetActivePage(Page: TKTabSheet);
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure ShowControl(AControl: TControl); override;
    procedure UpdateTabPanel; virtual;
    procedure UpdateTabPanelPosition; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindNextPage(CurPage: TKTabSheet; GoForward, CheckTabVisible: Boolean): TKTabSheet;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean = True);
    property ActivePage: TKTabSheet read GetActivePage write SetActivePage;
    property ActivePageIndex: Integer read FActivePageIndex write SetActivePageIndex;
    property DisabledImages: TImageList read FDisabledImages write SetDisabledImages;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default True;
    property Images: TImageList read FImages write SetImages;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TKTabSheet read GetPage;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition default cDefaultTabPosition;
    property TabHeight: Integer read FTabHeight write SetTabHeight default cDefaultTabHeight;
    property TabPanel: TKTabPanel read FTabPanel write SetTabPanel;
    property TabWidth: Integer read FTabWidth write SetTabWidth default cDefaultTabWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnGetImageIndex: TTabGetImageEvent read FOnGetImageIndex write FOnGetImageIndex;
  end;

  TKPageControl = class(TKCustomPageControl)
  published
    property ActivePage;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property Images;
//    property MultiLine;
//    property OwnerDraw;
    property ParentBiDiMode;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
//    property RaggedRight;
//    property ScrollOpposite;
    property ShowHint;
//    property Style;
    property TabHeight;
//    property TabIndex stored False;
    property TabOrder;
    property TabPosition;
    property TabStop;
    property TabWidth;
    property Touch;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
//    property OnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses
  SysUtils
{$IFDEF USE_THEMES}
  , Themes
 {$IFNDEF FPC}
  , UxTheme
 {$ENDIF}
{$ENDIF}
  , KGraphics;

{ TKTabColors }

constructor TKTabColors.Create(AControl: TKCustomControl);
begin
  inherited;

end;

function TKTabColors.GetColorSpec(Index: TKColorIndex): TKColorSpec;
begin
  case Index of
    ciNormalTop: begin Result.Def := cDefaultNormalTop; Result.Name := ''; end;
    ciNormalBottom: begin Result.Def := cDefaultNormalBottom; Result.Name := ''; end;
    ciNormalText: begin Result.Def := cDefaultNormalText; Result.Name := ''; end;
    ciSelectedTop: begin Result.Def := cDefaultSelectedTop; Result.Name := ''; end;
    ciSelectedBottom: begin Result.Def := cDefaultSelectedBottom; Result.Name := ''; end;
    ciSelectedText: begin Result.Def := cDefaultSelectedText; Result.Name := ''; end;
    ciHotTop: begin Result.Def := cDefaultHotTop; Result.Name := ''; end;
    ciHotBottom: begin Result.Def := cDefaultHotBottom; Result.Name := ''; end;
  else
    Result := inherited GetColorSpec(Index);
  end;
end;

function TKTabColors.GetMaxIndex: Integer;
begin
  Result := ciMaxIndex;
end;


{ TKTabPanel }

constructor TKTabPanel.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := bsNone;
  DoubleBuffered := True;
  FColors := TKTabColors.Create(Self);
  FCloseButton := nil;
  FCloseButtonIndex := -1;
  FMouseIndex := -1;
  FMouseInCloseButton := False;
  FPadding := cDefaultTabPadding;
  FPageControl := nil;
end;

destructor TKTabPanel.Destroy;
begin
  FColors.Free;
  inherited;
end;

function TKTabPanel.GetTabPaintInfo(ACanvas: TCanvas; ATabIndex: Integer; out Info: TKTabPaintInfo): Boolean;
var
  I, W, AllW, LeftPadding, RightPadding,
  CloseH, CloseW, ImageH, ImageW, ImagePadding, TextW, TextPadding: Integer;
  Images: TImageList;
  Text: string;
  Page: TKTabSheet;
  R: TRect;
begin
  Result := False;
  if (FPageControl <> nil) and (ATabIndex >= 0) and (ATabIndex < FPageControl.PageCount) then
  begin
    W := 0;
    I := 0;
    while (I <= ATabIndex) and (W < Width) do
    begin
      Page := FPageControl.Pages[I];
      if (Page.ImageIndex >= 0) and (Page.ImageIndex < FPageControl.Images.Count) then
      begin
        ImageW := FPageControl.Images.Width;
        ImageH := FPageControl.Images.Height;
      end else
      begin
        ImageW := 0;
        ImageH := 0;
      end;
      if ImageW <> 0 then
        ImagePadding := FPadding
      else
        ImagePadding := 0;
      if FPageControl.TabWidth <> 0 then
        TextW := FPageControl.TabWidth
      else
        TextW := ACanvas.TextWidth(Page.Caption);
      if TextW <> 0 then
        TextPadding := FPadding
      else
        TextPadding := 0;
      if FCloseButton <> nil then
      begin
        CloseW := FCloseButton.Width;
        CloseH := FCloseButton.Height;
      end
      else if (FCloseButtonIndex >= 0) and (FCloseButtonIndex < FPageControl.Images.Count) then
      begin
        CloseW := FPageControl.Images.Width;
        CloseH := FPageControl.Images.Height;
      end else
      begin
        CloseH := 0;
        CloseW := 0;
      end;
      LeftPadding := 2 * FPadding;
      RightPadding := 2 * FPadding;
      AllW := LeftPadding + ImageW + ImagePadding + TextW + TextPadding + CloseW + RightPadding;
      if I = ATabIndex then
      begin
        Info.TabRect := Rect(W, 0, W + AllW, Height);
        Info.ImageRect.TopLeft := Point(W + LeftPadding, (Height - ImageH) div 2);
        Info.ImageRect.BottomRight := Point(Info.ImageRect.Left + ImageW, Info.ImageRect.Top + ImageH);
        Info.TextRect.TopLeft := Point(W + LeftPadding + ImageW + ImagePadding, 0);
        Info.TextRect.BottomRight := Point(Info.TextRect.Left + TextW, Info.TextRect.Top + Height);
        Info.CloseRect.TopLeft := Point(W + LeftPadding + ImageW + ImagePadding + TextW + TextPadding, (height - CloseH) div 2);
        Info.CloseRect.BottomRight := Point(Info.CloseRect.Left + CloseW, Info.CloseRect.Top + CloseH);
        Result := True;
      end else
        Inc(W, AllW);
      Inc(I);
    end;
  end;
end;

function TKTabPanel.GetTabs(Index: Integer): string;
begin
  if (FPageControl <> nil) and (Index >= 0) and (Index < FPageControl.PageCount) then
    Result := FPageControl.Pages[Index].Caption
  else
    Result := '';
end;

procedure TKTabPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  Info: TKTabPaintInfo;
begin
  inherited;
  FPageToClose := -1;
  if (FPageControl <> nil) and (Button = mbLeft) then
  begin
    for I := 0 to FPageControl.PageCount - 1 do
      if getTabPaintInfo(FPageControl.Canvas, I, Info) then
      begin
        if PtInRect(Info.CloseRect, Point(X, Y)) then
        begin
          FPageToClose := I;
          Break;
        end
        else if PtInRect(Info.TabRect, Point(X, Y)) then
        begin
          FpageControl.ActivePageIndex := I;
          Break;
        end;
      end;
  end;
end;

procedure TKTabPanel.MouseFormLeave;
var
  P: TPoint;
begin
  inherited;
  P := ScreenToClient(Mouse.CursorPos);
  MouseOver([], P.X, P.Y);
end;

procedure TKTabPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  MouseOver(Shift, X, Y);
end;

procedure TKTabPanel.MouseOver;
var
  I, NewMouseIndex: Integer;
  NewMouseInCloseButton: Boolean;
  Info, OldInfo: TKTabPaintInfo;
begin
  if FPageControl <> nil then
  begin
    NewMouseIndex := -1;
    for I := 0 to FPageControl.PageCount - 1 do
      if getTabPaintInfo(FPageControl.Canvas, I, Info) then
      begin
        if PtInRect(Info.TabRect, Point(X, Y)) then
        begin
          NewMouseIndex := I;
          Break;
        end
      end;
    if NewMouseIndex <> FMouseIndex then
    begin
      if FMouseIndex >= 0 then
        if getTabPaintInfo(FPageControl.Canvas, FMouseIndex, OldInfo) then
          InvalidateRect(Handle, OldInfo.TabRect, False);
      FMouseIndex := NewMouseIndex;
      if FMouseIndex >= 0 then
        InvalidateRect(Handle, Info.TabRect, False);
    end
    else if NewMouseIndex >= 0 then
    begin
      NewMouseInCloseButton := PtInRect(Info.CloseRect, Point(X, Y));
      if NewMouseInCloseButton <> FMouseInCloseButton then
      begin
        FMouseInCloseButton := NewMouseInCloseButton;
        InvalidateRect(Handle, Info.CloseRect, False);
      end;
    end;
  end;
end;

procedure TKTabPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  Info: TKTabPaintInfo;
begin
  inherited;
  if (FPageControl <> nil) and (Button = mbLeft) then
  begin
    for I := 0 to FPageControl.PageCount - 1 do
      if getTabPaintInfo(FPageControl.Canvas, I, Info) then
      begin
        if (FPageToClose = I) and PtInRect(Info.CloseRect, Point(X, Y)) then
        begin
          FpageControl.DeletePage(I);
          Break;
        end
      end;
  end;
end;

procedure TKTabPanel.PaintTab(ACanvas: TCanvas; ATabIndex: Integer);
var
  Info: TKTabPaintInfo;
  State: TKTabState;
  R: TRect;
  MousePt: TPoint;
begin
  if getTabPaintInfo(ACanvas, ATabIndex, Info) then
  begin
    MousePt := ScreenToClient(Mouse.CursorPos);
    if FPageControl.ActivePageIndex = ATabIndex then
      State := tsSelected
    else if FPageControl.HotTrack and PtInRect(Info.TabRect, MousePt) then
      State := tsHot
    else
      State := tsNormal;
    if not IsRectEmpty(Info.TabRect) then with ACanvas do
    begin
      PaintTabBackground(ACanvas, Info.TabRect, State);
      if not IsRectEmpty(Info.ImageRect) then
        PaintTabImage(ACanvas, Info.ImageRect, ATabIndex);
      if not IsRectEmpty(Info.TextRect) then
        PaintTabText(ACanvas, Info.TextRect, ATabIndex, State);
      if not IsRectEmpty(Info.CloseRect) then
        PaintTabCloseButton(ACanvas, Info.CloseRect);
    end;
  end;
end;

procedure TKTabPanel.PaintTabBackground(ACanvas: TCanvas; const ARect: TRect; AState: TKTabState);
var
  R: TRect;
  Descent: Integer;
  StartColor, EndColor: TColor;
begin
  with ACanvas do if not IsRectEmpty(ARect) then
  begin
    Pen.Color := clBtnShadow;
    R := ARect;
    if AState = tsSelected then
      Descent := 1
    else
      Descent := 2;
    MoveTo(R.Left, R.Bottom - 1);
    LineTo(R.Left, R.Top + 2 + Descent);
    LineTo(R.Left + 2, R.Top + Descent);
    LineTo(R.Right - 2, R.Top + Descent);
    LineTo(R.Right, R.Top + 2 + Descent);
    LineTo(R.Right, R.Bottom - 1);
    if AState <> tsSelected then
      LineTo(R.Left, R.Bottom - 1);
    R := Rect(R.Left + 2, R.Top + 2 + Descent, R.Right - 1, R.Bottom);
    case AState of
      tsHot:
      begin
        StartColor := FColors.HotTop;
        EndColor := FColors.HotBottom;
      end;
      tsSelected:
      begin
        StartColor := FColors.SelectedTop;
        EndColor := FColors.SelectedBottom;
      end
    else
      StartColor := FColors.NormalTop;
      EndColor := FColors.NormalBottom;
    end;
    DrawGradientRect(ACanvas, R, ColorToRGB(StartColor), ColorToRGB(EndColor), 10, False);
  end;
end;

procedure TKTabPanel.PaintTabCloseButton(ACanvas: TCanvas; const ARect: TRect);
var
  Images: TImageList;
  MousePt: TPoint;
begin
  if (FPageControl <> nil) and not IsRectEmpty(ARect) then with ACanvas do
  begin
    if FCloseButton <> nil then
      Draw(ARect.Left, ARect.Top, FCloseButton)
    else
    begin
      MousePt := ScreenToClient(Mouse.CursorPos);
      if PtInRect(ARect, MousePt) or (FPageControl.DisabledImages = nil) then
        Images := FPageControl.Images
      else
        Images := FPageControl.DisabledImages;
      Images.Draw(ACanvas, ARect.Left, ARect.Top, FCloseButtonIndex);
    end;
  end;
end;

procedure TKTabPanel.PaintTabText(ACanvas: TCanvas; const ARect: TRect; ATabIndex: Integer; AState: TKTabState);
var
  TextBox: TKTextBox;
begin
  if (FPageControl <> nil) and (ATabIndex >= 0) and (ATabIndex < FPageControl.PageCount) and not IsRectEmpty(ARect) then with ACanvas do
  begin
    TextBox := TKTextBox.Create;
    try
      if AState = tsSelected then
        Font.Color := FColors.SelectedText
      else
        Font.Color := FColors.NormalText;
      TextBox.HAlign := halLeft;
      TextBox.VAlign := valCenter;
      TextBox.Text := FPageControl.Pages[ATabIndex].Caption;
      TextBox.Draw(ACanvas, ARect);
    finally
      TextBox.Free;
    end;
  end
end;

procedure TKTabPanel.PaintTabImage(ACanvas: TCanvas; const ARect: TRect; ATabIndex: Integer);
begin
  if (FPageControl <> nil) and (ATabIndex >= 0) and (ATabIndex < FPageControl.PageCount) and not IsRectEmpty(ARect) then
  begin
    FPageControl.Images.Draw(ACanvas, ARect.Left, ARect.Top, FPageControl.Pages[ATabIndex].ImageIndex);
  end
end;

procedure TKTabPanel.PaintToCanvas(ACanvas: TCanvas);
var
  I: Integer;
begin
  inherited;
  if FPageControl <> nil then with ACanvas do
  begin
    for I := 0 to FPageControl.PageCount - 1 do
      PaintTab(ACanvas, I);
  end;
end;

procedure TKTabPanel.SetCloseButton(const Value: TGraphic);
begin
  if Value <> FCloseButton then
  begin
    FCloseButton := Value;
    UpdateTabPanel;
  end;
end;

procedure TKTabPanel.SetCloseButtonIndex(const Value: Integer);
begin
  if Value <> FCloseButtonIndex then
  begin
    FCloseButtonIndex := Value;
    UpdateTabPanel;
  end;
end;

procedure TKTabPanel.SetColors(const Value: TKTabColors);
begin
  FColors.Assign(Value);
end;

procedure TKTabPanel.SetPadding(const Value: Integer);
begin
  if Value <> Fpadding then
  begin
    FPadding := Value;
    UpdateTabPanel;
  end;
end;

procedure TKTabPanel.SetPageControl(const Value: TKCustomPageControl);
begin
  if Value <> FPageControl then
  begin
    FPageControl := Value;
    UpdateTabPanel;
  end;
end;

procedure TKTabPanel.UpdateTabPanel;
begin
  FMouseIndex := -1;
  FMouseInCloseButton := False;
  Invalidate;
end;

{ TKTabSheet }

constructor TKTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible, csPannable];
  Visible := False;
  FTabVisible := True;
  FHighlighted := False;
end;

destructor TKTabSheet.Destroy;
begin
  if FPageControl <> nil then
  begin
    if FPageControl.FUndockingPage = Self then
      FPageControl.FUndockingPage := nil;
    FPageControl.RemovePage(Self);
  end;
  inherited Destroy;
end;

procedure TKTabSheet.DoHide;
begin
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

procedure TKTabSheet.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

function TKTabSheet.GetPageIndex: Integer;
begin
  if FPageControl <> nil then
    Result := FPageControl.FPages.IndexOf(Self)
  else
    Result := -1;
end;

procedure TKTabSheet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not ThemeServices.ThemesAvailable then
    with Params.WindowClass do
      style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TKTabSheet.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TKCustomPageControl then
    PageControl := TKCustomPageControl(Reader.Parent);
end;

procedure TKTabSheet.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    FPageControl.UpdateTabPanel;
  end;
end;

procedure TKTabSheet.SetPageControl(APageControl: TKCustomPageControl);
begin
  if FPageControl <> APageControl then
  begin
    if FPageControl <> nil then
      FPageControl.RemovePage(Self);
    FPageControl := APageControl;
    if FPageControl <> nil then
      FPageControl.InsertPage(Self);
  end;
end;

procedure TKTabSheet.SetPageIndex(Value: Integer);
begin
  if FPageControl <> nil then
  begin
    Value := MinMax(Value, 0, FPageControl.PageCount - 1);
    FPageControl.MovePage(PageIndex, Value);
  end;
end;

procedure TKTabSheet.SetTabVisible(Value: Boolean);
begin
  if FTabVisible <> Value then
  begin
    FTabVisible := Value;
    if FPageControl <> nil then
      FPageControl.UpdateTabPanel;
  end;
end;

procedure TKTabSheet.UpdateControlOriginalParentSize(AControl: TControl;
  var AOriginalParentSize: TPoint);
begin
  inherited;
  if not (csReading in ComponentState) and not HandleAllocated then
    Dec(AOriginalParentSize.X, BorderWidth * 2);
end;

procedure TKTabSheet.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then
  begin
    try
      DoShow
    except
      Application.HandleException(Self);
    end;
  end
  else
  if not Showing then
  begin
    try
      DoHide;
    except
      Application.HandleException(Self);
    end;
  end;
end;

procedure TKTabSheet.SetHighlighted(Value: Boolean);
begin
  if Value <> FHighLighted then
  begin
    FHighlighted := Value;
    if FPageControl <> nil then
      FPageControl.UpdateTabPanel;
  end;
end;

{ TKTabSheets }

function TKTabSheets.Add(AItem: TKTabSheet): Integer;
begin
  Result := inherited Add(AItem);
end;

function TKTabSheets.GetItem(Index: Integer): TKTabSheet;
begin
  Result := TKTabSheet(inherited GetItem(Index));
end;

function TKTabSheets.IndexOf(AItem: TKTabSheet): Integer;
begin
  Result := inherited IndexOf(AItem);
end;

procedure TKTabSheets.SetItem(Index: Integer; const Value: TKTabSheet);
begin
  inherited SetItem(Index, Value);
end;

{ TKCustomPageControl }

constructor TKCustomPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csDoubleClicks, csPannable, csGestures] - [csParentBackground];
  FActivePageIndex := -1;
  FDeletingPage := False;
  FDisabledImages := nil;
  FHotTrack := True;
  FImages := nil;
  FPages := TKTabSheets.Create;
  FTabHeight := cDefaultTabHeight;
  FTabPosition := cDefaultTabPosition;
  FTabWidth := cDefaultTabWidth;
  FOnChange := nil;
  FOnChanging := nil;
  TabPanel := TKTabPanel.Create(nil);
end;

destructor TKCustomPageControl.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
    FPages[I].PageControl := nil;
  FPages.Free;
  FTabPanel.Free;
  inherited Destroy;
end;

function TKCustomPageControl.CanChange: Boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then FOnChanging(Self, Result);
end;

procedure TKCustomPageControl.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TKCustomPageControl.ChangeActivePage(Page: TKTabSheet);
var
  ParentForm: TCustomForm;
  CurPage: TKTabSheet;
  Index: Integer;
begin
  Index := FPages.IndexOf(Page);
  if (Index >= 0) and (Index <> FActivePageIndex) then
  begin
    CurPage := ActivePage;
    if CurPage <> nil then
      CurPage.Visible := False;
    FActivePageIndex := Index;
    CurPage := ActivePage;
    if CurPage <> nil then
    begin
      CurPage.BringToFront;
      CurPage.Visible := True;
      ParentForm := GetParentForm(Self);
      if (ParentForm <> nil) and (CurPage <> nil) and (ParentForm.ActiveControl = CurPage) then
      begin
        CurPage.SelectFirst;
        if Page.CanFocus then
          ParentForm.ActiveControl := Page
        else
          ParentForm.ActiveControl := Self;
      end;
    end;
    UpdateTabPanel;
  end;
end;

procedure TKCustomPageControl.CMDialogKey(var Message: TCMDialogKey);
begin
  if (Focused or Windows.IsChild(Handle, Windows.GetFocus)) and
    (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
    Message.Result := 1;
  end else
    inherited;
end;

procedure TKCustomPageControl.CMDockClient(var Message: TCMDockClient);
var
  IsVisible: Boolean;
  DockCtl: TControl;
  I: Integer;
begin
  with Message do
  begin
    Result := 0;
    DockCtl := DockSource.Control;
    { First, look and see if the page is already docked. If it is,
      then simply move the page index to the end }
    for I := 0 to PageCount - 1 do
    begin
      if DockCtl.Parent = Pages[I] then
      begin
        { We did find it; just move the page to the end }
        Pages[I].PageIndex := PageCount - 1;
        Exit;
      end;
    end;

    FNewDockSheet := TKTabSheet.Create(Self);
    try
      try
        if DockCtl is TCustomForm then
          FNewDockSheet.Caption := TCustomForm(DockCtl).Caption;
        FNewDockSheet.PageControl := Self;
        DockCtl.Dock(Self, DockSource.DockRect);
      except
        FNewDockSheet.Free;
        raise;
      end;
      IsVisible := DockCtl.Visible;
      FNewDockSheet.TabVisible := IsVisible;
      if IsVisible then
        ActivePage := FNewDockSheet;
      DockCtl.Align := alClient;
    finally
      FNewDockSheet := nil;
    end;
  end;
end;

procedure TKCustomPageControl.CMDockNotification(var Message: TCMDockNotification);
var
  I: Integer;
  S: string;
  Page: TKTabSheet;
begin
  Page := GetPageFromDockClient(Message.Client);
  if Page <> nil then
    case Message.NotifyRec.ClientMsg of
      WM_SETTEXT:
        begin
          S := PChar(Message.NotifyRec.MsgLParam);
          // Search for first CR/LF and end string there
          for I := 1 to Length(S) do
            if CharInSet(S[I], [#13, #10]) then
            begin
              SetLength(S, I - 1);
              Break;
            end;
          Page.Caption := S;
        end;
      CM_VISIBLECHANGED:
        Page.TabVisible := Boolean(Message.NotifyRec.MsgWParam);
    end;
  inherited;
end;

procedure TKCustomPageControl.CMUnDockClient(var Message: TCMUnDockClient);
var
  Page: TKTabSheet;
begin
  with Message do
  begin
    Result := 0;
    Page := GetPageFromDockClient(Client);
    if Page <> nil then
    begin
      FUndockingPage := Page;
      Client.Align := alNone;
    end;
  end;
end;

procedure TKCustomPageControl.DeletePage(Index: Integer);
var
  Page, NextPage: TKTabSheet;
begin
  if not FDeletingPage and (Index >= 0) and (Index < FPages.Count) then
  begin
    FDeletingPage := True;
    try
      Page := FPages[Index];
      NextPage := FindNextPage(Page, True, not (csDesigning in ComponentState));
      if NextPage = Page then
        NextPage := nil;
      Page.Parent := nil;
      Page.PageControl := nil;
      FPages.Delete(Index);
      if (Index = FActivePageIndex) or (FActivePageIndex >= FPages.Count) then
        FActivePageIndex := -1;
      SetActivePage(NextPage);
    finally
      FDeletingPage := False;
    end;
  end;
end;

procedure TKCustomPageControl.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  if FNewDockSheet <> nil then
    Client.Parent := FNewDockSheet;
end;

procedure TKCustomPageControl.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Source.DockRect := R;
  DoDockOver(Source, X, Y, State, Accept);
end;

procedure TKCustomPageControl.DoRemoveDockClient(Client: TControl);
begin
  if (FUndockingPage <> nil) and not (csDestroying in ComponentState) then
  begin
    RemovePage(FUndockingPage);
    FUndockingPage := nil;
  end;
end;

function TKCustomPageControl.FindNextPage(CurPage: TKTabSheet;
  GoForward, CheckTabVisible: Boolean): TKTabSheet;
var
  I, StartIndex: Integer;
begin
  if FPages.Count <> 0 then
  begin
    StartIndex := FPages.IndexOf(CurPage);
    if StartIndex = -1 then
      if GoForward then
        StartIndex := FPages.Count - 1
      else
        StartIndex := 0;
    I := StartIndex;
    repeat
      if GoForward then
      begin
        Inc(I);
        if I = FPages.Count then
          I := 0;
      end else
      begin
        if I = 0 then
          I := FPages.Count;
        Dec(I);
      end;
      Result := TKTabSheet(FPages[I]);
      if not CheckTabVisible or Result.TabVisible then
        Exit;
    until I = StartIndex;
  end;
  Result := nil;
end;

function TKCustomPageControl.GetActivePage: TKTabSheet;
begin
  if FActivePageIndex >= 0 then
    Result := FPages[FActivePageIndex]
  else
    Result := nil;
end;

procedure TKCustomPageControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
    Proc(TComponent(FPages[I]));
end;

function TKCustomPageControl.GetImageIndex(TabIndex: Integer): Integer;
begin
  Result := -1;
  if Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, TabIndex, Result);
end;

function TKCustomPageControl.GetPageFromDockClient(Client: TControl): TKTabSheet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to PageCount - 1 do
  begin
    if (Client.Parent = Pages[I]) and (Client.HostDockSite = Self) then
    begin
      Result := Pages[I];
      Exit;
    end;
  end;
end;

function TKCustomPageControl.GetPage(Index: Integer): TKTabSheet;
begin
  Result := TKTabSheet(FPages[Index]);
end;

function TKCustomPageControl.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TKCustomPageControl.GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
  MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := GetPageFromDockClient(Client) = nil;
  inherited GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
end;

procedure TKCustomPageControl.InsertPage(Page: TKTabSheet);
begin
  FPages.Add(Page);
  Page.PageControl := Self;
  Page.Parent := Self;
end;

procedure TKCustomPageControl.MovePage(CurIndex, NewIndex: Integer);
begin
  FPages.Move(CurIndex, NewIndex);
end;

procedure TKCustomPageControl.PaintToCanvas(ACanvas: TCanvas);
begin
end;

procedure TKCustomPageControl.RemovePage(Page: TKTabSheet);
begin
  DeletePage(FPages.IndexOf(Page));
end;

procedure TKCustomPageControl.SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean = True);
var
  Page: TKTabSheet;
begin
  Page := FindNextPage(ActivePage, GoForward, CheckTabVisible);
  if (Page <> nil) and (Page <> ActivePage) and CanChange then
  begin
    SetActivePage(Page);
    Change;
  end;
end;

procedure TKCustomPageControl.SetActivePage(Page: TKTabSheet);
begin
  if (Page <> nil) and (Page.PageControl <> Self) then
    Exit;
  ChangeActivePage(Page);
end;

procedure TKCustomPageControl.SetActivePageIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value < PageCount) then
    ActivePage := Pages[Value]
  else
    ActivePage := nil;
end;

procedure TKCustomPageControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  TKTabSheet(Child).PageIndex := Order;
end;

procedure TKCustomPageControl.SetDisabledImages(const Value: TImageList);
begin
  if FDisabledImages <> Value then
  begin
    FDisabledImages := Value;
    UpdateTabPanel;
  end;
end;

procedure TKCustomPageControl.SetHotTrack(const Value: Boolean);
begin
  if Value <> FHotTrack then
  begin
    FHotTrack := Value;
    UpdateTabPanel;
  end;
end;

procedure TKCustomPageControl.SetImages(const Value: TImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    UpdateTabPanel;
  end;
end;

procedure TKCustomPageControl.SetTabHeight(const Value: Integer);
begin
  if Value <> FTabHeight then
  begin
    FTabHeight := Value;
    UpdateTabPanelPosition;
  end;
end;

procedure TKCustomPageControl.SetTabPanel(const Value: TKTabPanel);
begin
  if Value <> FTabPanel then
  begin
    FreeAndNil(FTabPanel);
    FTabPanel := Value;
    UpdateTabPanelPosition;
  end;
end;

procedure TKCustomPageControl.SetTabPosition(const Value: TTabPosition);
begin
  if Value <> FTabPosition then
  begin
    FTabPosition := Value;
    UpdateTabPanelPosition;
  end;
end;

procedure TKCustomPageControl.SetTabWidth(const Value: Integer);
begin
  if Value <> FTabWidth then
  begin
    FTabWidth := Value;
    UpdateTabPanel;
  end;
end;

procedure TKCustomPageControl.ShowControl(AControl: TControl);
begin
  if (AControl is TKTabSheet) and (TKTabSheet(AControl).PageControl = Self) then
    SetActivePage(TKTabSheet(AControl));
  inherited ShowControl(AControl);
end;

procedure TKCustomPageControl.UpdateTabPanel;
begin
  if FTabPanel <> nil then
    FTabPanel.Invalidate;
end;

procedure TKCustomPageControl.UpdateTabPanelPosition;
begin
  if FTabPanel <> nil then
  begin
    case FTabPosition of
      tpBottom:
      begin
        FTabPanel.Align := alBottom;
        FTabPanel.Height := FTabHeight;
      end;
      tpTop:
      begin
        FTabPanel.Align := alTop;
        FTabPanel.Height := FTabHeight;
      end;
      tpLeft:
      begin
        FTabPanel.Align := alLeft;
        FTabPanel.Width := FTabHeight;
      end;
      tpRight:
      begin
        FTabPanel.Align := alRight;
        FTabPanel.Width := FTabHeight;
      end;
    end;
    FTabPanel.PageControl := Self;
    FTabPanel.Parent := Self;
  end;
end;

procedure TKCustomPageControl.WMEraseBkgnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

end.
