{ @abstract(This unit contains a dialog for paragraph style editing)
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
unit kmemodlgparastyle; // lowercase name because of Lazarus/Linux

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, KEdits, KControls, KButtons, KMemo;

type
  TKMemoParaStyleForm = class(TForm)
    BUOk: TButton;
    BUCancel: TButton;
    GBCommon: TGroupBox;
    CoBAlign: TComboBox;
    LBalignment: TLabel;
    GBIndent: TGroupBox;
    LBFirstIndent: TLabel;
    CoBFIrstIndent: TComboBox;
    EDFirstIndent: TKNumberEdit;
    EDLeftIndent: TKNumberEdit;
    LBLeftIndent: TLabel;
    EDRightIndent: TKNumberEdit;
    LBRightIndent: TLabel;
    GBSpacing: TGroupBox;
    LBSpaceAbove: TLabel;
    LBSpaceBelow: TLabel;
    EDSpaceAbove: TKNumberEdit;
    EDSpaceBelow: TKNumberEdit;
    GBShading: TGroupBox;
    LBBorderLeft: TLabel;
    LBBorderRight: TLabel;
    EDBorderLeft: TKNumberEdit;
    EDBorderRight: TKNumberEdit;
    LBBorderTop: TLabel;
    LBBorderBottom: TLabel;
    EDBorderTop: TKNumberEdit;
    EDBorderBottom: TKNumberEdit;
    CLBBorder: TKColorButton;
    CLBShading: TKColorButton;
    LBBorderColor: TLabel;
    LBShading: TLabel;
    LBBorderRadius: TLabel;
    EDBorderRadius: TKNumberEdit;
    CBWordWrap: TCheckBox;
    LBLineSpacing: TLabel;
    CoBLineSpacing: TComboBox;
    LBLineSpacingValue: TLabel;
    EDLineSpacingValue: TKNumberEdit;
    procedure CoBLineSpacingClick(Sender: TObject);
  private
    { Private declarations }
    FMemo: TKmemo;
    FStyle: TKMemoParaStyle;
    procedure UpdateLineSpacingValue(AValue: Double);
  public
    { Public declarations }
    procedure Load(AMemo: TKMemo; AStyle: TKMemoParaStyle);
    procedure Save(AStyle: TKMemoParaStyle);
  end;

implementation

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  Math, KFunctions, KGraphics;

{ TKMemoParaStyleForm }

procedure TKMemoParaStyleForm.CoBLineSpacingClick(Sender: TObject);
begin
  case CoBLineSpacing.ItemIndex of
    3,4: UpdateLineSpacingValue(Abs(FStyle.LineSpacingValue))
  else
    UpdateLineSpacingValue(FStyle.LineSpacingFactor);
  end;
end;

procedure TKMemoParaStyleForm.Load(AMemo: TKMemo; AStyle: TKMemoParaStyle);
begin
  Assert(AMemo <> nil);
  Assert(AStyle <> nil);
  FMemo := AMemo;
  FStyle := AStyle;
  CoBAlign.ItemIndex := Integer(AStyle.HAlign);
  EDLeftIndent.Value := FMemo.Px2PtX(AStyle.LeftPadding);
  EDRightIndent.Value := FMemo.Px2PtX(AStyle.RightPadding);
  EDFirstIndent.Value := FMemo.Px2PtX(Abs(AStyle.FirstIndent));
  if AStyle.FirstIndent = 0 then
    CoBFirstIndent.ItemIndex := 0
  else if AStyle.FirstIndent > 0 then
    CoBFirstIndent.ItemIndex := 1
  else
    CoBFirstIndent.ItemIndex := 2;
  EDSpaceAbove.Value := FMemo.Px2PtY(AStyle.TopPadding);
  EDSpaceBelow.Value := FMemo.Px2PtY(AStyle.BottomPadding);
  EDBorderBottom.Value := FMemo.Px2PtY(AStyle.BorderWidths.Bottom);
  EDBorderLeft.Value := FMemo.Px2PtX(AStyle.BorderWidths.Left);
  EDBorderRight.Value := FMemo.Px2PtX(AStyle.BorderWidths.Right);
  EDBorderTop.Value := FMemo.Px2PtY(AStyle.BorderWidths.Top);
  EDBorderRadius.Value := FMemo.Px2PtX(AStyle.BorderRadius);
  CLBBorder.DlgColor := AStyle.BorderColor;
  if AStyle.Brush.Style <> bsClear then
    CLBShading.DlgColor := AStyle.Brush.Color
  else
    CLBShading.DlgColor := clNone;
  CBWordWrap.Checked := AStyle.WordWrap;
  case AStyle.LineSpacingMode of
    lsmFactor:
    begin
      if SameValue(AStyle.LineSpacingFactor, 1) then
        CoBLineSpacing.ItemIndex := 0
      else if SameValue(AStyle.LineSpacingFactor, 1.5) then
        CoBLineSpacing.ItemIndex := 1
      else if SameValue(AStyle.LineSpacingFactor, 2) then
        CoBLineSpacing.ItemIndex := 2
      else
        CoBLineSpacing.ItemIndex := 5;
      UpdateLineSpacingValue(AStyle.LineSpacingFactor);
    end;
    lsmValue:
    begin
      if AStyle.LineSpacingValue >= 0 then
        CoBLineSpacing.ItemIndex := 3
      else
        CoBLineSpacing.ItemIndex := 4;
      UpdateLineSpacingValue(Abs(AStyle.LineSpacingValue));
    end;
  end;
end;

procedure TKMemoParaStyleForm.Save(AStyle: TKMemoParaStyle);
begin
  if AStyle <> nil then
  begin
    AStyle.HAlign := TKHAlign(CoBAlign.ItemIndex);
    AStyle.LeftPadding := FMemo.Pt2PxX(EDLeftIndent.Value);
    AStyle.RightPadding := FMemo.Pt2PxX(EDRightIndent.Value);
    case CoBFirstIndent.ItemIndex of
      1: AStyle.FirstIndent := FMemo.Pt2PxX(EDFirstIndent.Value);
      2: AStyle.FirstIndent := FMemo.Pt2PxX(-EDFirstIndent.Value);
    else
      AStyle.FirstIndent := 0;
    end;
    AStyle.TopPadding := FMemo.Pt2PxY(EDSpaceAbove.Value);
    AStyle.BottomPadding := FMemo.Pt2PxY(EDSpaceBelow.Value);
    AStyle.BorderWidths.Bottom := FMemo.Pt2PxY(EDBorderBottom.Value);
    AStyle.BorderWidths.Left := FMemo.Pt2PxX(EDBorderLeft.Value);
    AStyle.BorderWidths.Right := FMemo.Pt2PxX(EDBorderRight.Value);
    AStyle.BorderWidths.Top := FMemo.Pt2PxY(EDBorderTop.Value);
    AStyle.BorderRadius := FMemo.Pt2PxX(EDBorderRadius.Value);
    AStyle.BorderColor := CLBBorder.DlgColor;
    if CLBShading.DlgColor <> clNone then
      AStyle.Brush.Color := CLBShading.DlgColor;
    AStyle.WordWrap := CBWordWrap.Checked;
    case CoBLineSpacing.ItemIndex of
      1: begin AStyle.LineSpacingMode := lsmFactor; AStyle.LineSpacingFactor := 1.5; end;
      2: begin AStyle.LineSpacingMode := lsmFactor; AStyle.LineSpacingFactor := 2; end;
      3: begin AStyle.LineSpacingMode := lsmValue; AStyle.LineSpacingValue := FMemo.Pt2PxY(-MinMax(EDLineSpacingValue.Value, 5, 100)); end;
      4: begin AStyle.LineSpacingMode := lsmValue; AStyle.LineSpacingValue := FMemo.Pt2PxY(MinMax(EDLineSpacingValue.Value, 5, 100)); end;
      5: begin AStyle.LineSpacingMode := lsmFactor; AStyle.LineSpacingFactor := MinMax(EDLineSpacingValue.Value, 0.1, 10); end;
    else
      AStyle.LineSpacingMode := lsmFactor; AStyle.LineSpacingFactor := 1;
    end;
  end;
end;

procedure TKMemoParaStyleForm.UpdateLineSpacingValue(AValue: Double);
begin
  case CoBLineSpacing.ItemIndex of
    1: begin EDLineSpacingValue.Enabled := False; EDLineSpacingValue.Value := 1.5; end;
    2: begin EDLineSpacingValue.Enabled := False; EDLineSpacingValue.Value := 2; end;
    3: begin EDLineSpacingValue.Enabled := True; EDLineSpacingValue.Value := FMemo.Px2PtY(Round(AValue)); end;
    4: begin EDLineSpacingValue.Enabled := True; EDLineSpacingValue.Value := FMemo.Px2PtY(Round(AValue)); end;
    5: begin EDLineSpacingValue.Enabled := True; EDLineSpacingValue.Value := AValue; end;
  else
    EDLineSpacingValue.Enabled := False; EDLineSpacingValue.Value := 1;
  end;
end;

end.