unit KMemoDlgParaStyle;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
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
  private
    { Private declarations }
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
  KGraphics;

{ TKMemoParaStyleForm }

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
  end;
end;

end.
