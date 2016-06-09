{ @abstract(This unit contains a dialog for editing an image)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(28 Apr 2009)
  @lastmod(30 July 2015)

  Copyright © Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. All redistributions
  of the original or modified source code must retain the original copyright
  notice. The Author accepts no liability for any damage that may result
  from using this code.
}
unit kmemodlgimage; // lowercase name because of Lazarus/Linux

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, KControls, KMemo, ExtCtrls, ExtDlgs, kbuttons, kedits,
  ComCtrls;

type
  TKMemoImageForm = class(TForm)
    BUOk: TButton;
    BUCancel: TButton;
    BUBrowse: TButton;
    ODMain: TOpenPictureDialog;
    PCMain: TPageControl;
    TSBasic: TTabSheet;
    TSAdvanced: TTabSheet;
    GBPreview: TGroupBox;
    IMPreview: TImage;
    GBShading: TGroupBox;
    LBBorderLeft: TLabel;
    LBBorderRight: TLabel;
    LBBorderTop: TLabel;
    LBBorderBottom: TLabel;
    LBBorderColor: TLabel;
    LBShading: TLabel;
    LBBorderRadius: TLabel;
    EDBorderLeft: TKNumberEdit;
    EDBorderRight: TKNumberEdit;
    EDBorderTop: TKNumberEdit;
    EDBorderBottom: TKNumberEdit;
    CLBBorder: TKColorButton;
    CLBShading: TKColorButton;
    EDBorderRadius: TKNumberEdit;
    GBCrop: TGroupBox;
    LBCropLeft: TLabel;
    LBCropRight: TLabel;
    LBCropTop: TLabel;
    LBCropBottom: TLabel;
    EDCropLeft: TKNumberEdit;
    EDCropRight: TKNumberEdit;
    EDCropTop: TKNumberEdit;
    EDCropBottom: TKNumberEdit;
    GBPosition: TGroupBox;
    RBPositionText: TRadioButton;
    RBPositionRelative: TRadioButton;
    RBPositionAbsolute: TRadioButton;
    EDOffsetX: TKNumberEdit;
    EDOffsetY: TKNumberEdit;
    GBSize: TGroupBox;
    EDScaleX: TKNumberEdit;
    EDScaleY: TKNumberEdit;
    CBProportional: TCheckBox;
    GBWrap: TGroupBox;
    RBWrapAround: TRadioButton;
    RBWrapAroundLeft: TRadioButton;
    RBWrapAroundRight: TRadioButton;
    RBWrapTopBottom: TRadioButton;
    procedure BUBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EDScaleXChange(Sender: TObject);
    procedure EDScaleYChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Clear;
    procedure Load(AItem: TKMemoImageBlock);
    procedure Save(AItem: TKMemoImageBlock);
  end;

implementation

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  KGraphics;

{ TKMemoHyperlinkForm }

procedure TKMemoImageForm.BUBrowseClick(Sender: TObject);
begin
  if ODMain.Execute then
    IMPreview.Picture.LoadFromFile(ODMain.FileName);
end;

procedure TKMemoImageForm.Clear;
begin
end;

procedure TKMemoImageForm.EDScaleXChange(Sender: TObject);
begin
  if CBProportional.Checked then
    EDScaleY.ValueAsInt := EDScaleX.ValueAsInt
end;

procedure TKMemoImageForm.EDScaleYChange(Sender: TObject);
begin
  if CBProportional.Checked then
    EDScaleX.ValueAsInt := EDScaleY.ValueAsInt
end;

procedure TKMemoImageForm.FormCreate(Sender: TObject);
begin
  PCMain.ActivePageIndex := 0;
end;

procedure TKMemoImageForm.Load(AItem: TKMemoImageBlock);
begin
  if AItem <> nil then
  begin
    IMPreview.Picture := AItem.Image;
    case AItem.Position of
      mbpText: RBPositionText.Checked := True;
      mbpRelative: RBPositionRelative.Checked := True;
      mbpAbsolute: RBPositionAbsolute.Checked := True;
    end;
    EDOffsetX.ValueAsInt := AItem.LeftOffset;
    EDOffsetY.ValueAsInt := AItem.TopOffset;
    EDScaleX.ValueAsInt := AItem.ScaleX;
    EDScaleY.ValueAsInt := AItem.ScaleY;
    CBProportional.Checked := AItem.ScaleX = AItem.ScaleY;
    case AItem.ImageStyle.WrapMode of
      wrAround, wrTight: RBWrapAround.Checked := True;
      wrAroundLeft, wrTightLeft: RBWrapAroundLeft.Checked := True;
      wrAroundRight, wrTightRight: RBWrapAroundRight.Checked := True;
    else
      RBWrapTopBottom.Checked := True;
    end;
    EDBorderBottom.ValueAsInt := AItem.ImageStyle.BorderWidths.Bottom;
    EDBorderLeft.ValueAsInt := AItem.ImageStyle.BorderWidths.Left;
    EDBorderRight.ValueAsInt := AItem.ImageStyle.BorderWidths.Right;
    EDBorderTop.ValueAsInt := AItem.ImageStyle.BorderWidths.Top;
    EDBorderRadius.ValueAsInt := AItem.ImageStyle.BorderRadius;
    CLBBorder.DlgColor := AItem.ImageStyle.BorderColor;
    if AItem.ImageStyle.Brush.Style <> bsClear then
      CLBShading.DlgColor := AItem.ImageStyle.Brush.Color
    else
      CLBShading.DlgColor := clNone;
    EDCropLeft.ValueAsInt := AItem.Crop.Left;
    EDCropRight.ValueAsInt := AItem.Crop.Right;
    EDCropTop.ValueAsInt := AItem.Crop.Top;
    EDCropBottom.ValueAsInt := AItem.Crop.Bottom;
  end;
end;

procedure TKMemoImageForm.Save(AItem: TKMemoImageBlock);
begin
  if AItem <> nil then
  begin
    AItem.Image := IMPreview.Picture;
    if RBPositionText.Checked then
      AItem.Position := mbpText
    else if RBPositionRelative.Checked then
      AItem.Position := mbpRelative
    else
      AItem.Position := mbpAbsolute;
    if AItem.Position = mbpText then
    begin
      AItem.LeftOffset := 0;
      AItem.TopOffset := 0;
    end else
    begin
      AItem.LeftOffset := EDOffsetX.ValueAsInt;
      AItem.TopOffset := EDOffsetY.ValueAsInt;
    end;
    AItem.ScaleX := EDScaleX.ValueAsInt;
    AItem.ScaleY := EDScaleY.ValueAsInt;
    if RBWrapAround.Checked then
      AItem.ImageStyle.WrapMode := wrAround
    else if RBWrapAroundLeft.Checked then
      AItem.ImageStyle.WrapMode := wrAroundLeft
    else if RBWrapAroundRight.Checked then
      AItem.ImageStyle.WrapMode := wrAroundRight
    else
      AItem.ImageStyle.WrapMode := wrTopBottom;
    AItem.ImageStyle.BorderWidths.Bottom := EDBorderBottom.ValueAsInt;
    AItem.ImageStyle.BorderWidths.Left := EDBorderLeft.ValueAsInt;
    AItem.ImageStyle.BorderWidths.Right := EDBorderRight.ValueAsInt;
    AItem.ImageStyle.BorderWidths.Top := EDBorderTop.ValueAsInt;
    AItem.ImageStyle.BorderRadius := EDBorderRadius.ValueAsInt;
    AItem.ImageStyle.BorderColor := CLBBorder.DlgColor;
    if CLBShading.DlgColor <> clNone then
      AItem.ImageStyle.Brush.Color := CLBShading.DlgColor;
    AItem.Crop.Left := EDCropLeft.ValueAsInt;
    AItem.Crop.Right := EDCropRight.ValueAsInt;
    AItem.Crop.Top := EDCropTop.ValueAsInt;
    AItem.Crop.Bottom := EDCropBottom.ValueAsInt;
  end;
end;

end.