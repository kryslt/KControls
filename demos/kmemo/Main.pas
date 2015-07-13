unit Main;

interface

{$include KControls.inc}

uses
  {$IFDEF FPC}
    LCLIntf, LResources, LCLProc,
  {$ELSE}
    Windows, Messages,
  {$ENDIF}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, KGrids,
    KMemo, KGraphics, KFunctions, ExtCtrls, Grids, StdCtrls, KEditCommon;

type

  { TMainForm }

  TMainForm = class(TForm)
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Memo: TKMemo;
//    MF: TKMetafile;
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

  procedure AddTextField(CO: TKMemoContainer; Text1: Boolean);
  var
    TB: TKMemoTextBlock;
    PA: TKMemoParagraph;
  begin
    CO.Blocks.LockUpdate;
    try
      if Text1 then
      begin
        TB := CO.Blocks.AddTextBlock('This is special text 1');
        TB.TextStyle.Font.Color := clRed;
        PA := CO.Blocks.AddParagraph;
        PA.ParaStyle.Brush.Color := clInfoBk;
        PA.ParaStyle.BorderRadius := 5;
        TB := CO.Blocks.AddTextBlock('This is test text 2');
        TB.TextStyle.Brush.Color := clYellow;
        TB.TextStyle.Font.Style := [fsBold];
        CO.Blocks.AddImageBlock('../../resource_src/kmessagebox_stop.png');
        CO.Blocks.AddParagraph;
        CO.Blocks.AddTextBlock('This is test text 3');
        CO.Blocks.AddParagraph;
        CO.Blocks.AddTextBlock('This is test text 4');
        CO.Blocks.AddParagraph;
      end else
      begin
        TB := CO.Blocks.AddTextBlock('This is other text 1');
        CO.Blocks.AddParagraph;
      end;
    finally
      CO.Blocks.UnlockUpdate;
    end;
  end;

var
  TB: TKMemoTextBlock;
  IB: TKMemoImageBlock;
  PA: TKMemoParagraph;
  TBL: TKMemoTable;
  Index: Integer;
  MS: TMemoryStream;
  S: AnsiString;
  W: TKString;
  I: Integer;
begin
//  MF := TKMetafile.Create;
//  MF.LoadFromFile('test.wmf');
//  MF.Width := 4320;
//  MF.Height := 992;

  Memo := TKMemo.Create(Self);
  Memo.ContentPadding.Top := 20;
  Memo.ContentPadding.Left := 20;
  Memo.ContentPadding.Right := 20;
  Memo.Align := alClient;
  Memo.Options := Memo.Options + [eoShowFormatting];
  Memo.Colors.BkGnd := clWhite;
  Memo.Font.Name := 'Arial';
  Memo.Font.Size := 20;
  Memo.ParaStyle.FirstIndent := 30;
//  Memo.ParaStyle.HAlign := halCenter;
  Memo.Blocks.LockUpdate;
  try
    Memo.Blocks.Clear;
{    Memo.Blocks.AddTextBlock('This is test text 1.');
    PA := Memo.Blocks.AddParagraph;
    IB := Memo.Blocks.AddImageBlock('label_wre.png');}
//    IB.ImageStyle.BorderWidth := 1;
//    IB.ImageStyle.BorderRadius := 10;
//    IB.ImageStyle.Brush.Color := clLime;
//    IB.LeftOffset := 0;
//    IB.TopOffset := 0;
//    IB.Position := mbpRelative;
{    TB := Memo.Blocks.AddTextBlock('This is a test text 2.');
    PA := Memo.Blocks.AddParagraph;}
   {    Memo.Blocks.AddImageBlock('../../resource_src/kmessagebox_info.png');
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
    Memo.Blocks.AddParagraph;}
    //AddTextField;
    //AddTextField;

{    TBL := Memo.Blocks.AddTable;
    TBL.BlockStyle.TopPadding := 20;
    TBL.BlockStyle.BottomPadding := 30;
    TBL.CellStyle.BorderWidth := 2;
    TBL.CellStyle.ContentPadding.AssignFromValues(5,5,5,5);
    TBL.CellStyle.Brush.Color := clWhite;
    TBL.ColCount := 2;
    TBL.RowCount := 2;
//    TBL.Rows[0].Cells[0].FixedWidth := True;
//    TBL.Rows[1].Cells[1].FixedWidth := True;
    TBL.Rows[0].RequiredHeight := 200;
    TBL.ColWidths[0] := 500;
    TBl.ColWidths[1] := 400;
    AddTextField(TBL.Rows[0].Cells[0], True);
    AddTextField(TBL.Rows[0].Cells[1], True);
    AddTextField(TBL.Rows[1].Cells[0], False);
    AddTextField(TBL.Rows[1].Cells[1], False);
    TBL.RequiredWidth := 600;
    TBL.FixedWidth := True;
    TBL.ApplyDefaultCellStyle;

    PA := Memo.Blocks.AddParagraph;
    PA.ParaStyle.FirstIndent := 0;
    PA.ParaStyle.HAlign := halCenter;
    PA.ParaStyle.CancelFloat := True;}

//    TB := Memo.Blocks.AddTextBlock('This is big bold text.');

//    IB := Memo.Blocks.AddImageBlock('label_wre.png');
//    IB.ImageStyle.BorderWidth := 1;
//    IB.ImageStyle.BorderRadius := 10;
//    IB.ImageStyle.Brush.Color := clLime;
//    IB.LeftOffset := 0;
//    IB.TopOffset := 0;
//    IB.Position := mbpRelative;

{    TB := Memo.Blocks.AddTextBlock('This is big bold text.');
    TB.TextStyle.Font.Style := [fsBold];
    TB.TextStyle.Font.Size := 15;}
{    TB := Memo.Blocks.AddTextBlock(' This is small bold text.');
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
    TB := Memo.Blocks4.AddTextBlock('This is vertically aligned text.');
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

    {
    Memo.Clear;
    MS := TMemoryStream.Create;
    try
      MS.LoadFromFile('../../kgrid_readme.txt');
      SetString(S, PAnsiChar(MS.Memory), MS.Size);
      Memo.Blocks.Text := S;
    finally
      MS.Free;
    end;}

  {  Memo.Blocks[15].Position := mbpRelative;}
    //Memo.Font.Size := 15;
//    Memo.Blocks[15].TopOffset := 50;

//    IB := Memo.Blocks.AddImageBlock('label_wre.png', 15);
    //    IB.LeftOffset := 350;
    //    IB.TopOffset := 260;
    //    IB.Position := mbpRelative;
//    IB.ScaleWidth := 600;
//    IB.ScaleHeight := 400;
//    PA := Memo.Blocks.AddParagraph;

    Memo.BackgroundImage.LoadFromFile('../../resource_src/clouds.jpg');
  finally
    //Memo.UnlockUpdate;
    Memo.Blocks.UnlockUpdate;
  end;

//  Memo.LoadFromRTF('test_no_img.rtf');
  Memo.LoadFromRTF('test.rtf');
  Memo.SaveToRTF('test_save.rtf');
//  Memo.LoadFromRTF('test1.rtf');
//  Memo.LoadFromRTF('kgrid_manual.rtf');
//  Memo.LoadFromRTF('Word2007RTFSpec9.rtf');
//  Memo.LoadFromRTF('../../../../_SC/wattrouter_eco/docu/manual_CZ/WATTrouterECO_CZ.rtf');

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
//  Memo.Select(2, 10);
//  Mainform.Canvas.StretchDraw(ClientRect, MF);
end;

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
procedure TMainForm.FormDestroy(Sender: TObject);
var
  S: string;
begin
//  MF.Free;
end;

procedure TMainForm.FormPaint(Sender: TObject);
const
  cTabChar = #$AE; // right arrow but valid only for Symbol font!
//  cTabChar = #$2192; // right arrow in unicode!
var
  S, TabAsUTF8: string;
begin
  S := 'A'#9'B'#9'C';
  TabAsUTF8 := UnicodeToNativeUTF(cTabChar);
  Canvas.Font.Color := clBlack;
  Canvas.Font.Name := 'Symbol';
  S := StringReplace(S, #9, TabAsUTF8, [rfReplaceAll]);
  Canvas.TextOut(10, 10, S);
end;

end.