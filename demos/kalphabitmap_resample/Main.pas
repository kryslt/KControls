unit Main;

interface

{$include kcontrols.inc}

uses
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,

  KFunctions, KGraphics, KLog, KEdits;

type
  TForm1 = class(TForm)
    BUResample: TButton;
    EDW: TEdit;
    EDH: TEdit;
    KLog1: TKLog;
    LBLog: TListBox;
    EDWindow: TEdit;
    BULoadFromFile: TButton;
    BULoadTestBitmap: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CoBType: TComboBox;
    Label4: TLabel;
    ScBIM1: TScrollBox;
    IM1: TImage;
    Label5: TLabel;
    Label6: TLabel;
    ScBIM2: TScrollBox;
    IM2: TImage;
    EDFile: TKFileNameEdit;
    BUHalf: TButton;
    BUDouble: TButton;
    BUTriple: TButton;
    BUXdYh: TButton;
    CBAspectRatio: TCheckBox;
    procedure BUResampleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BULoadFromFileClick(Sender: TObject);
    procedure BULoadTestBitmapClick(Sender: TObject);
    procedure BUHalfClick(Sender: TObject);
    procedure BUDoubleClick(Sender: TObject);
    procedure BUTripleClick(Sender: TObject);
    procedure BUXdYhClick(Sender: TObject);
    procedure EDFileChange(Sender: TObject);
    procedure EDWExit(Sender: TObject);
    procedure CBAspectRatioClick(Sender: TObject);
  private
    { Private declarations }
    FStartup: Boolean;
    FB1, FB2: TKAlphaBitmap;
    procedure LoadBitmap(const AFileName: string);
    procedure LoadTestBitmap;
    procedure Resample;
    procedure UpdateAspectRatio;
    procedure UpdateImages;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FStartup := True;
  FB1 := TKAlphaBitmap.Create;
  FB2 := TKAlphaBitmap.Create;
  EDFile.FileName := '../../penguins.jpg';
  CBAspectRatio.Checked;
  CoBType.ItemIndex := Integer(rkLanczos);
  EDWindow.Text := '2';
  LoadTestBitmap;
  FStartup := False;
end;

procedure TForm1.LoadBitmap(const AFileName: string);
begin
  FB1.LoadFromFile(AFileName);
  UpdateImages;
end;

procedure TForm1.LoadTestBitmap;
const
  cBlockSize = 20;
  cBlockCount = 10;
var
  I, J: Integer;
  SkipX, SkipY: Boolean;
begin
  SkipX := False;
  SkipY := False;
  FB1.SetSize(cBlockSize * cBlockCount, cBlockSize * cBlockCount);
  for I := 0 to cBlockCount - 1 do
  begin
    SkipX := not SkipX;
    for J := 0 to cBlockCount - 1 do
    begin
      SkipY := not SkipY;
      if SkipX xor SkipY then
        FB1.Canvas.Brush.Color := clRed
      else
        FB1.Canvas.Brush.Color := clWhite;
      FB1.Canvas.FillRect(Rect(I * cBlockSize, J * cBlockSize, I * cBlockSize + cBlockSize, J * cBlockSize + cBlockSize));
    end;
  end;
  FB1.UpdatePixels;
  FB1.AlphaFill(255);
  FB1.SaveToFile('../../test.bmp');
  UpdateImages;
end;

procedure TForm1.Resample;
begin
  FB1.ResamplingKernel := TKResamplingKernel(CoBType.ItemIndex);
  FB1.ResamplingWindow := StrToIntDef(EDWindow.Text, 3);
{$IFDEF DEBUG_ALPHABITMAP}
  KLog1.Clear;
  FB1.Log := KLog1;
{$ENDIF}
  UpdateAspectRatio;
  FB1.Resample(FB2, StrToIntDef(EDW.Text, FB1.Width), StrToIntDef(EDH.Text, FB1.Height));
  IM2.Picture.Assign(FB2);
end;

procedure TForm1.UpdateAspectRatio;
begin
  if CBAspectRatio.Checked then
    EDH.Text := IntToStr(FB1.Height * StrToIntDef(EDW.Text, FB1.Width) div FB1.Width);
end;

procedure TForm1.UpdateImages;
begin
  IM1.Picture.Assign(FB1);
  IM2.Picture.Assign(FB1);
  EDW.Text := IntToStr(FB1.Width);
  EDH.Text := IntToStr(FB1.Height);
end;

procedure TForm1.BUDoubleClick(Sender: TObject);
begin
  EDW.Text := IntToStr(FB1.Width * 2);
  EDH.Text := IntToStr(FB1.Height * 2);
  Resample;
end;

procedure TForm1.BUHalfClick(Sender: TObject);
begin
  EDW.Text := IntToStr(FB1.Width div 2);
  EDH.Text := IntToStr(FB1.Height div 2);
  Resample;
end;

procedure TForm1.BUResampleClick(Sender: TObject);
begin
  Resample;
end;

procedure TForm1.BUTripleClick(Sender: TObject);
begin
  EDW.Text := IntToStr(FB1.Width * 3);
  EDH.Text := IntToStr(FB1.Height * 3);
  Resample;
end;

procedure TForm1.BUXdYhClick(Sender: TObject);
begin
  CBAspectRatio.Checked := False;
  EDW.Text := IntToStr(FB1.Width * 2);
  EDH.Text := IntToStr(FB1.Height div 2);
  Resample;
end;

procedure TForm1.CBAspectRatioClick(Sender: TObject);
begin
  UpdateAspectRatio;
end;

procedure TForm1.EDFileChange(Sender: TObject);
begin
  if not FStartup then
    BULoadFromFileClick(nil);
end;

procedure TForm1.EDWExit(Sender: TObject);
begin
  UpdateAspectRatio;
end;

procedure TForm1.BULoadFromFileClick(Sender: TObject);
begin
  if FileExists(EDFile.FileName) then
    LoadBitmap(EDFile.FileName)
  else
    KLog1.Log(lgError, 'File does not exist!');
end;

procedure TForm1.BULoadTestBitmapClick(Sender: TObject);
begin
  LoadTestBitmap;
end;

end.
