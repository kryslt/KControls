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
    procedure UpdateLineSpacingValue(AValue: Double);
  public
    { Public declarations }
    procedure Load(AStyle: TKMemoParaStyle);
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
  UpdateLineSpacingValue(EDLineSpacingValue.Value);
end;

procedure TKMemoParaStyleForm.Load(AStyle: TKMemoParaStyle);
begin
  if AStyle <> nil then
  begin
    CoBAlign.ItemIndex := Integer(AStyle.HAlign);
    EDLeftIndent.ValueAsInt := AStyle.LeftPadding;
    EDRightIndent.ValueAsInt := AStyle.RightPadding;
    EDFirstIndent.ValueAsInt := Abs(AStyle.FirstIndent);
    if AStyle.FirstIndent = 0 then
      CoBFirstIndent.ItemIndex := 0
    else if AStyle.FirstIndent > 0 then
      CoBFirstIndent.ItemIndex := 1
    else
      CoBFirstIndent.ItemIndex := 2;
    EDSpaceAbove.ValueAsInt := AStyle.TopPadding;
    EDSpaceBelow.ValueAsInt := AStyle.BottomPadding;
    EDBorderBottom.ValueAsInt := AStyle.BorderWidths.Bottom;
    EDBorderLeft.ValueAsInt := AStyle.BorderWidths.Left;
    EDBorderRight.ValueAsInt := AStyle.BorderWidths.Right;
    EDBorderTop.ValueAsInt := AStyle.BorderWidths.Top;
    EDBorderRadius.ValueAsInt := AStyle.BorderRadius;
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
end;

procedure TKMemoParaStyleForm.Save(AStyle: TKMemoParaStyle);
begin
  if AStyle <> nil then
  begin
    AStyle.HAlign := TKHAlign(CoBAlign.ItemIndex);
    AStyle.LeftPadding := EDLeftIndent.ValueAsInt;
    AStyle.RightPadding := EDRightIndent.ValueAsInt;
    case CoBFirstIndent.ItemIndex of
      1: AStyle.FirstIndent := EDFirstIndent.ValueAsInt;
      2: AStyle.FirstIndent := -EDFirstIndent.ValueAsInt;
    else
      AStyle.FirstIndent := 0;
    end;
    AStyle.TopPadding := EDSpaceAbove.ValueAsInt;
    AStyle.BottomPadding := EDSpaceBelow.ValueAsInt;
    AStyle.BorderWidths.Bottom := EDBorderBottom.ValueAsInt;
    AStyle.BorderWidths.Left := EDBorderLeft.ValueAsInt;
    AStyle.BorderWidths.Right := EDBorderRight.ValueAsInt;
    AStyle.BorderWidths.Top := EDBorderTop.ValueAsInt;
    AStyle.BorderRadius := EDBorderRadius.ValueAsInt;
    AStyle.BorderColor := CLBBorder.DlgColor;
    if CLBShading.DlgColor <> clNone then
      AStyle.Brush.Color := CLBShading.DlgColor;
    AStyle.WordWrap := CBWordWrap.Checked;
    case CoBLineSpacing.ItemIndex of
      1: begin AStyle.LineSpacingMode := lsmFactor; AStyle.LineSpacingFactor := 1.5; end;
      2: begin AStyle.LineSpacingMode := lsmFactor; AStyle.LineSpacingFactor := 2; end;
      3: begin AStyle.LineSpacingMode := lsmValue; AStyle.LineSpacingValue := -MinMax(EDLineSpacingValue.ValueAsInt, 5, 100); end;
      4: begin AStyle.LineSpacingMode := lsmValue; AStyle.LineSpacingValue := MinMax(EDLineSpacingValue.ValueAsInt, 5, 100); end;
      5: begin AStyle.LineSpacingMode := lsmFactor; AStyle.LineSpacingFactor := MinMax(EDLineSpacingValue.Value, 0.1, 10); end;
    else
      AStyle.LineSpacingMode := lsmFactor; AStyle.LineSpacingFactor := 1;
    end;
  end;
end;

procedure TKMemoParaStyleForm.UpdateLineSpacingValue(AValue: Double);
begin
  case CoBLineSpacing.ItemIndex of
    1: begin EDLineSpacingValue.LastInputFormat := nedfFloat; EDLineSpacingValue.Enabled := False; EDLineSpacingValue.Value := 1.5; end;
    2: begin EDLineSpacingValue.LastInputFormat := nedfFloat; EDLineSpacingValue.Enabled := False; EDLineSpacingValue.Value := 2; end;
    3: begin EDLineSpacingValue.LastInputFormat := nedfDec; EDLineSpacingValue.Enabled := True; EDLineSpacingValue.Value := AValue; end;
    4: begin EDLineSpacingValue.LastInputFormat := nedfDec; EDLineSpacingValue.Enabled := True; EDLineSpacingValue.Value := AValue; end;
    5: begin EDLineSpacingValue.LastInputFormat := nedfFloat; EDLineSpacingValue.Enabled := True; EDLineSpacingValue.Value := AValue; end;
  else
    EDLineSpacingValue.LastInputFormat := nedfFloat; EDLineSpacingValue.Enabled := False; EDLineSpacingValue.Value := 1;
  end;
end;

end.