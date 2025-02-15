unit Main;

interface

{$include kcontrols.inc}

uses
 {$IFDEF MSWINDOWS}
  Windows,
 {$ELSE}
  LCLType,
 {$ENDIF}
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,

  KFunctions, KGraphics, KLog, KEdits;

type
  TMainForm = class(TForm)
    BUResample: TButton;
    EDW: TEdit;
    EDH: TEdit;
    Log: TKLog;
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
    Timer: TTimer;
    EDAnimCnt: TEdit;
    Label7: TLabel;
    BUAnimDown: TButton;
    BUAnimUp: TButton;
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
    procedure TimerTimer(Sender: TObject);
    procedure BUAnimDownClick(Sender: TObject);
    procedure BUAnimUpClick(Sender: TObject);
  private
    { Private declarations }
    FAnimation, FAnimationDirection: Boolean;
    FAnimationCount, FAnimatedWidth, FAnimatedHeight: Integer;
    FInsideTimer, FStartup: Boolean;
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
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FStartup := True;
  FAnimation := False;
  FInsideTimer := False;
  FB1 := TKAlphaBitmap.Create;
{$IFDEF DEBUG_ALPHABITMAP}
  FB1.Log := Log;
{$ENDIF}
  FB2 := TKAlphaBitmap.Create;
  EDFile.FileName := '../../penguins.jpg';
  CoBType.ItemIndex := Integer(rkLanczos);
  EDWindow.Text := '2';
  LoadTestBitmap;
  CBAspectRatio.Checked := True;
  FStartup := False;
end;

procedure TMainForm.LoadBitmap(const AFileName: string);
begin
  FB1.LoadFromFile(AFileName);
  UpdateImages;
end;

procedure TMainForm.LoadTestBitmap;
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
  try
    FB1.SaveToFile('../../test.bmp');
  except
  end;
  UpdateImages;
end;

procedure TMainForm.Resample;
begin
  FAnimation := False;
  FB1.ResamplingKernel := TKResamplingKernel(CoBType.ItemIndex);
  FB1.ResamplingWindow := StrToIntDef(EDWindow.Text, 3);
{$IFDEF DEBUG_ALPHABITMAP}
  Log.Clear;
{$ENDIF}
  UpdateAspectRatio;
  FB1.Resample(FB2, StrToIntDef(EDW.Text, FB1.Width), StrToIntDef(EDH.Text, FB1.Height));
  IM2.Picture.Assign(FB2);
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  if FInsideTimer then Exit;
  if FAnimation then
  begin
    FInsideTimer := True;
    try
    {$IFDEF DEBUG_ALPHABITMAP}
      Log.Clear;
    {$ENDIF}
      FB1.Resample(FB2, FAnimatedWidth, FAnimatedHeight);
      IM2.Picture.Assign(FB2);
      if FAnimationDirection then
      begin
        if FB1.Width > FB1.Height then
        begin
          Inc(FAnimatedHeight);
          FAnimatedWidth := MulDiv(FAnimatedHeight, FB1.Width, FB1.Height);
        end else
        begin
          Inc(FAnimatedWidth);
          FAnimatedHeight := MulDiv(FAnimatedWidth, FB1.Height, FB1.Width);
        end;
      end else
      begin
        if FB1.Width > FB1.Height then
        begin
          Dec(FAnimatedHeight);
          FAnimatedWidth := MulDiv(FAnimatedHeight, FB1.Width, FB1.Height);
        end else
        begin
          Dec(FAnimatedWidth);
          FAnimatedHeight := MulDiv(FAnimatedWidth, FB1.Height, FB1.Width);
        end;
        if (FAnimatedWidth = 1) or (FAnimatedheight = 1) then
          FAnimation := False;
      end;
      Dec(FAnimationCount);
      if FAnimationCount = 0 then
        FAnimation := False;
      EDW.Text := IntToStr(FAnimatedWidth);
      EDH.Text := IntToStr(FAnimatedHeight);
    finally
      FInsideTimer := False;
    end;
  end;
end;

procedure TMainForm.UpdateAspectRatio;
begin
  if CBAspectRatio.Checked then
    EDH.Text := IntToStr(MulDiv(FB1.Height, StrToIntDef(EDW.Text, FB1.Width), FB1.Width));
end;

procedure TMainForm.UpdateImages;
begin
  IM1.Picture.Assign(FB1);
  IM2.Picture.Assign(FB1);
  EDW.Text := IntToStr(FB1.Width);
  EDH.Text := IntToStr(FB1.Height);
end;

procedure TMainForm.BUDoubleClick(Sender: TObject);
begin
  EDW.Text := IntToStr(FB1.Width * 2);
  EDH.Text := IntToStr(FB1.Height * 2);
  Resample;
end;

procedure TMainForm.BUHalfClick(Sender: TObject);
begin
  EDW.Text := IntToStr(FB1.Width div 2);
  EDH.Text := IntToStr(FB1.Height div 2);
  Resample;
end;

procedure TMainForm.BUResampleClick(Sender: TObject);
begin
  Resample;
end;

procedure TMainForm.BUTripleClick(Sender: TObject);
begin
  EDW.Text := IntToStr(FB1.Width * 3);
  EDH.Text := IntToStr(FB1.Height * 3);
  Resample;
end;

procedure TMainForm.BUAnimDownClick(Sender: TObject);
begin
  FAnimation := True;
  FAnimationDirection := False;
  FAnimatedWidth := FB1.Width;
  FAnimatedHeight := FB1.Height;
  FAnimationCount := StrToIntDef(EDAnimCnt.Text, 10);
end;

procedure TMainForm.BUAnimUpClick(Sender: TObject);
begin
  FAnimation := True;
  FAnimationDirection := True;
  FAnimatedWidth := FB1.Width;
  FAnimatedHeight := FB1.Height;
  FAnimationCount := StrToIntDef(EDAnimCnt.Text, 10);
end;

procedure TMainForm.BUXdYhClick(Sender: TObject);
begin
  CBAspectRatio.Checked := False;
  EDW.Text := IntToStr(FB1.Width * 2);
  EDH.Text := IntToStr(FB1.Height div 2);
  Resample;
end;

procedure TMainForm.CBAspectRatioClick(Sender: TObject);
begin
  UpdateAspectRatio;
end;

procedure TMainForm.EDFileChange(Sender: TObject);
begin
  if not FStartup then
    BULoadFromFileClick(nil);
end;

procedure TMainForm.EDWExit(Sender: TObject);
begin
  UpdateAspectRatio;
end;

procedure TMainForm.BULoadFromFileClick(Sender: TObject);
begin
  if FileExists(EDFile.FileName) then
    LoadBitmap(EDFile.FileName)
  else
    Log.Log(lgError, 'File does not exist!');
end;

procedure TMainForm.BULoadTestBitmapClick(Sender: TObject);
begin
  LoadTestBitmap;
end;

end.
