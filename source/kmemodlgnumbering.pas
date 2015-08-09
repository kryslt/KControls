unit KMemoDlgNumbering;

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, KEdits, KControls, KButtons, KMemo, ExtCtrls;

type
  TKMemoNumberingForm = class(TForm)
    BUOk: TButton;
    BUCancel: TButton;
    RGNumbering: TRadioGroup;
    GBOptions: TGroupBox;
    LBStartAt: TLabel;
    EDStartAt: TKNumberEdit;
    CoBListLevel: TComboBox;
    LBListLevel: TLabel;
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

{ TKMemoNumberingForm }

procedure TKMemoNumberingForm.Load(AStyle: TKMemoParaStyle);
begin
  if AStyle <> nil then
  begin
    RGNumbering.ItemIndex := Integer(AStyle.Numbering);
    EDStartAt.ValueAsInt := AStyle.NumberStartAt;
  end;
end;

procedure TKMemoNumberingForm.Save(AStyle: TKMemoParaStyle);
begin
  if AStyle <> nil then
  begin
    AStyle.Numbering := TKMemoParaNumbering(RGNumbering.ItemIndex);
    AStyle.NumberStartAt := EDStartAt.ValueAsInt;
  end;
end;

end.