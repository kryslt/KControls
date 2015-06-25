unit Main;

interface

{$include KControls.inc}

uses
  {$IFDEF FPC}
    LCLIntf, LResources,
  {$ELSE}
    Windows, Messages,
  {$ENDIF}
    SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, KGrids, KMemo, KGraphics, KFunctions, ExtCtrls, Grids, KEditCommon;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Memo: TKMemo;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF USE_PNG_SUPPORT}
uses
 {$IFDEF FPC}
  fpImage, IntfGraphics
 {$ELSE}
  PngImage
 {$ENDIF}
  ;
{$ENDIF}

procedure TMainForm.FormCreate(Sender: TObject);
var
  TB: TKTextMemoBlock;
  IB: TKImageMemoBlock;
  PA: TKMemoParagraph;
  Index: Integer;
  MS: TMemoryStream;
  S: AnsiString;
  W: TKString;
  I: Integer;
begin
  Memo := TKMemo.Create(Self);
  Memo.ContentPadding.Top := 20;
  Memo.ContentPadding.Left := 20;
  Memo.ContentPadding.Right := 20;
  Memo.Align := alClient;
  Memo.Options := Memo.Options + [eoShowFormatting];
  Memo.Colors.BkGnd := clWhite;
  Memo.Font.Name := 'Arial';
  Memo.Font.Size := 20;
  Memo.ParaStyle.HAlign := halCenter;
  Memo.Blocks.LockUpdate;
  try
    Memo.Blocks.Clear;
{    Memo.Blocks.AddTextBlock('This is a test text 1.');
    Memo.Blocks.AddImageBlock('../../resource_src/kmessagebox_info.png');
    IB := Memo.Blocks.AddImageBlock('label_wre.png');
    IB.LeftOffset := 20;
    IB.TopOffset := 20;
    IB.Position := mbpRelative;
    TB := Memo.Blocks.AddTextBlock('This is a test text 2.');
    TB.TextStyle.Font.Size := 40;
    TB.TextStyle.Brush.Color := clLime;
    TB.TextStyle.Font.Color := clRed;
    Memo.Blocks.AddImageBlock('../../resource_src/kmessagebox_stop.png');
    TB := Memo.Blocks.AddTextBlock('This is a test text 3.');
    TB.TextStyle.Font.Size := 30;
    TB.TextStyle.Font.Name := 'Times New Roman';
    TB.TextStyle.Font.Style := [fsUnderline];
    TB.TextStyle.Font.Color := clBlue;
    TB.TextStyle.Brush.Color := clYellow;
    PA := Memo.Blocks.AddParagraph;
    PA.ParaStyle.LeftPadding := 30;
    PA.ParaStyle.RightPadding := 30;
    PA.ParaStyle.TopPadding := 20;
    PA.ParaStyle.BottomPadding := 20;
  //  PA.ParaStyle.HAlign := halCenter;
    PA.ParaStyle.HAlign := halRight;
    PA.ParaStyle.Brush.Color := clInfoBk;
  //  PA.ParaStyle.Brush.Style := bsCross;
    PA.ParaStyle.BorderRadius := 5;
  //  PA.ParaStyle.BorderWidth := 1;
  //  PA.ParaStyle.WordWrap := False;
    Memo.Blocks.AddTextBlock('This is a test text 4.');
    Memo.Blocks.AddParagraph;
    IB := Memo.Blocks.AddImageBlock('label_wre.png');
    IB.ImageStyle.BorderWidth := 1;
    IB.ImageStyle.BorderRadius := 10;
    IB.ImageStyle.Brush.Color := clLime;
    IB.LeftOffset := 50;
    IB.TopOffset := 0;
    IB.Position := mbpRelative; }

  {  Memo.Blocks.AddTextBlock('testtext1');
    Memo.Blocks.AddNewLineBlock;
    Memo.Blocks.AddTextBlock('testtext2');
    Memo.Blocks.AddNewLineBlock;
    Memo.Blocks.AddTextBlock('testtext3');
    Memo.Blocks.AddNewLineBlock;
    Memo.Blocks.AddTextBlock('testtext4');
    Memo.Blocks.AddNewLineBlock;
    TB := Memo.Blocks.AddTextBlock('This is big bold text.');
    TB.Font.Style := [fsBold];
    TB.Font.Size := 15;
    TB := Memo.Blocks.AddTextBlock(' This is small bold text.');
    TB.Font.Style := [fsBold];
    Memo.Blocks.AddNewLineBlock;
    TB := Memo.Blocks.AddTextBlock('This is text on gray.');
    TB.Brush.Color := clGray;
    TB.Font.Size := 15;
    TB := Memo.Blocks.AddTextBlock(' This is red text on gray.');
    TB.Brush.Color := clGray;
    TB.Font.Size := 15;
    TB.Font.Color := clRed;
    Memo.Blocks.AddNewLineBlock;
    Memo.Blocks.AddImageBlock('../../resource_src/kmessagebox_info.png');
    Memo.Blocks.AddImageBlock('../../resource_src/kmessagebox_stop.png');
    Memo.Blocks.AddNewLineBlock;
    TB := Memo.Blocks.AddTextBlock('This is vertically aligned text.');
    TB.Font.Size := 12;
    TB.VAlign := valCenter;
    Memo.Blocks.AddImageBlock('../../resource_src/kmessagebox_warning.png');
    Memo.Blocks.AddTextBlock(' another text');
    Memo.Blocks.AddNewLineBlock;
    TB := Memo.Blocks.AddTextBlock('This is text on gray.');
    TB.Brush.Color := clGray;
    TB.Font.Size := 15;
    TB := Memo.Blocks.AddTextBlock(' This is red text on gray.');
    TB.Brush.Color := clGray;
    TB.Font.Size := 15;
    TB.Font.Color := clRed;
    Memo.Blocks.AddNewLineBlock;}
    MS := TMemoryStream.Create;
    try
      MS.LoadFromFile('../../kgrid_readme.txt');
      SetString(S, PAnsiChar(MS.Memory), MS.Size);
      Memo.Blocks.Text := S;
    finally
      MS.Free;
    end;

{    IB := Memo.Blocks.AddImageBlock('label_wre.png', 15);
    IB.LeftOffset := 350;
    IB.TopOffset := 260;
    IB.Position := mbpRelative;}

    Memo.BackgroundImage.LoadFromFile('../../resource_src/clouds.jpg');
  finally
    //Memo.UnlockUpdate;
    Memo.Blocks.UnlockUpdate;
  end;
{
  Memo.ContentPadding.Left := 50;
  Memo.ContentPadding.Top := 30;
  Memo.ContentPadding.Right := 60;
  Memo.Blocks.Lines[2] := 'This is replacement.';
  Memo.Select(0, 5);}
{  W := Memo.Text;
  Memo.Text := W;
  W := Memo.Text;
  Memo.Text := W;}
  Memo.Parent := Self;
  Memo.Select(2, 10);
end;

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
end.
