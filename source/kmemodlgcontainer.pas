{ @abstract(This unit contains a dialog for image editing)
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
unit kmemodlgcontainer; // lowercase name because of Lazarus/Linux

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, KControls, KMemo, ExtCtrls, ExtDlgs, KButtons, KEdits,
  ComCtrls;

type

  { TKMemoImageForm }

  TKMemoContainerForm = class(TForm)
    BUOk: TButton;
    BUCancel: TButton;
    GBPreview: TGroupBox;
    GBPosition: TGroupBox;
    RBPositionRelative: TRadioButton;
    RBPositionAbsolute: TRadioButton;
    EDOffsetX: TKNumberEdit;
    EDOffsetY: TKNumberEdit;
    GBSize: TGroupBox;
    EDWidth: TKNumberEdit;
    EDHeight: TKNumberEdit;
    CBAutoWidth: TCheckBox;
    GBWrap: TGroupBox;
    RBWrapAround: TRadioButton;
    RBWrapAroundLeft: TRadioButton;
    RBWrapAroundRight: TRadioButton;
    RBWrapTopBottom: TRadioButton;
    GBShading: TGroupBox;
    LBBorderWidth: TLabel;
    LBBorderColor: TLabel;
    LBShading: TLabel;
    EDBorderWidth: TKNumberEdit;
    CLBBorder: TKColorButton;
    CLBShading: TKColorButton;
    MEPreview: TKMemo;
    CBAutoHeight: TCheckBox;
    procedure EDWidthExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FLockUpdate: Boolean;
    FPreviewContainer: TKMemoContainer;
    procedure UpdateFields;
  public
    { Public declarations }
    procedure Clear;
    procedure Load(AItem: TKMemoContainer);
    procedure Save(AItem: TKMemoContainer);
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

procedure TKMemoContainerForm.Clear;
begin
end;

procedure TKMemoContainerForm.EDWidthExit(Sender: TObject);
begin
  if not FLockUpdate then
    Save(FPreviewContainer);
end;

procedure TKMemoContainerForm.FormCreate(Sender: TObject);
begin
  FLockUpdate := False;
end;

procedure TKMemoContainerForm.Load(AItem: TKMemoContainer);
begin
  if AItem <> nil then
  begin
    MEPreview.Clear;
    FPreviewContainer := MEPreview.Blocks.AddContainer;
    FPreviewContainer.Assign(AItem);
    FPreviewContainer.LeftOffset := 0;
    FPreviewContainer.TopOffset := 0;
    FLockUpdate := True;
    try
      case AItem.Position of
        mbpRelative: RBPositionRelative.Checked := True;
        mbpAbsolute: RBPositionAbsolute.Checked := True;
      end;
      EDOffsetX.ValueAsInt := AItem.LeftOffset;
      EDOffsetY.ValueAsInt := AItem.TopOffset;
      EDWidth.ValueAsInt := AItem.Width;
      EDHeight.ValueAsInt := AItem.Height;
      CBAutoWidth.Checked := not AItem.FixedWidth;
      CBAutoHeight.Checked := not AItem.FixedHeight;
      case AItem.BlockStyle.WrapMode of
        wrAround, wrTight: RBWrapAround.Checked := True;
        wrAroundLeft, wrTightLeft: RBWrapAroundLeft.Checked := True;
        wrAroundRight, wrTightRight: RBWrapAroundRight.Checked := True;
      else
        RBWrapTopBottom.Checked := True;
      end;
      EDBorderWidth.ValueAsInt := AItem.BlockStyle.BorderWidth;
      CLBBorder.DlgColor := AItem.BlockStyle.BorderColor;
      if AItem.BlockStyle.Brush.Style <> bsClear then
        CLBShading.DlgColor := AItem.BlockStyle.Brush.Color
      else
        CLBShading.DlgColor := clNone;
    finally
      FLockUpdate := False;
    end;
    UpdateFields;
  end;
end;

procedure TKMemoContainerForm.Save(AItem: TKMemoContainer);
begin
  if AItem <> nil then
  begin
    AItem.LockUpdate;
    try
      if AItem <> FPreviewContainer then
      begin
        if RBPositionRelative.Checked then
          AItem.Position := mbpRelative
        else
          AItem.Position := mbpAbsolute;
        AItem.LeftOffset := EDOffsetX.ValueAsInt;
        AItem.TopOffset := EDOffsetY.ValueAsInt;
      end;
      AItem.RequiredWidth := EDWidth.ValueAsInt;
      AItem.RequiredHeight := EDHeight.ValueAsInt;
      AItem.FixedWidth := not CBAutoWidth.Checked;
      AItem.FixedHeight := not CBAutoHeight.Checked;
      if RBWrapAround.Checked then
        AItem.BlockStyle.WrapMode := wrAround
      else if RBWrapAroundLeft.Checked then
        AItem.BlockStyle.WrapMode := wrAroundLeft
      else if RBWrapAroundRight.Checked then
        AItem.BlockStyle.WrapMode := wrAroundRight
      else
        AItem.BlockStyle.WrapMode := wrTopBottom;
      AItem.BlockStyle.BorderWidth := EDBorderWidth.ValueAsInt;
      AItem.BlockStyle.BorderColor := CLBBorder.DlgColor;
      if CLBShading.DlgColor <> clNone then
        AItem.BlockStyle.Brush.Color := CLBShading.DlgColor;
    finally
      AItem.UnLockUpdate;
    end;
  end;
end;

procedure TKMemoContainerForm.UpdateFields;
begin
  // nothing to do yet
end;

end.
