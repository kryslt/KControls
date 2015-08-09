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
    KMemo, KGraphics, KFunctions, ExtCtrls, Grids, StdCtrls, KEditCommon,
    KSplitter, KControls, KLabels, KDialogs;

type

  { TMainForm }

  TMainForm = class(TForm)
    BUPreview: TButton;
    BUPrint: TButton;
    PNMain: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    BULoad: TButton;
    Splitter1: TSplitter;
    KPrintPreviewDialog1: TKPrintPreviewDialog;
    KPrintSetupDialog1: TKPrintSetupDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure KMemo1DropFiles(Sender: TObject; X, Y: Integer; Files: TStrings);
    procedure BULoadClick(Sender: TObject);
    procedure BUPreviewClick(Sender: TObject);
    procedure BUPrintClick(Sender: TObject);
  private
    { Private declarations }
    Memo: TKMemo;
    MemoCopy: TKMemo;
//    MF: TKMetafile;
    procedure LoadFiles;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

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
        CO.Blocks.AddHyperlink('www.solarcontrols.cz', 'www.solarcontrols.cz');
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
  Memo.ContentPadding.Bottom := 20;
  Memo.Align := alClient;
  Memo.Options := Memo.Options + [eoDropFiles, eoShowFormatting, eoWantTab];
  Memo.OnDropFiles := KMemo1DropFiles;
  Memo.Parent := Panel1;
  Memo.PageSetup.Title := 'test_document';
  Memo.Clear;
//  Memo.Colors.BkGnd := clWhite;
//  Memo.Font.Name := 'Arial';
//  Memo.Font.Size := 20;
//  Memo.ParaStyle.FirstIndent := 30;
//  Memo.ParaStyle.HAlign := halCenter;
  Memo.Blocks.LockUpdate;
  try
    Memo.Blocks.Clear;
    Memo.Blocks.AddTextBlock('This is test text 1. This is test text 1. This is test text 1. This is test text 1. This is test text 1. This is test text 1.');
    PA := Memo.Blocks.AddParagraph;
    PA.ParaStyle.Numbering := pnuLetterHi;
    PA.ParaStyle.FirstIndent := -20;
    PA.ParaStyle.LeftPadding := 20;
//    PA.TextStyle.Font.Color := clRed;
//    PA.TextStyle.Font.Size := 40;
    TB := Memo.Blocks.AddTextBlock('This is a test text 2.');
    PA := Memo.Blocks.AddParagraph;
    PA.ParaStyle.Numbering := pnuLetterHi;
    PA.ParaStyle.FirstIndent := -20;
    PA.ParaStyle.LeftPadding := 20;
    TB := Memo.Blocks.AddTextBlock('This is a level 2 test text 1.');
    PA := Memo.Blocks.AddParagraph;
    PA.ParaStyle.NumberingList := 2;
    PA.ParaStyle.Numbering := pnuRomanLo;
    PA.ParaStyle.FirstIndent := -20;
    PA.ParaStyle.LeftPadding := 60;
    TB := Memo.Blocks.AddTextBlock('This is a level 2 test text 2.');
    PA := Memo.Blocks.AddParagraph;
    PA.ParaStyle.NumberingList := 2;
    PA.ParaStyle.Numbering := pnuRomanLo;
    PA.ParaStyle.FirstIndent := -20;
    PA.ParaStyle.LeftPadding := 60;
    TB := Memo.Blocks.AddTextBlock('This is a level 1 test text 1.');
    PA := Memo.Blocks.AddParagraph;
    PA.ParaStyle.NumberingList := 1;
    PA.ParaStyle.Numbering := pnuArabic;
    PA.ParaStyle.FirstIndent := -20;
    PA.ParaStyle.LeftPadding := 40;
    TB := Memo.Blocks.AddTextBlock('This is a level 1 test text 2.');
    PA := Memo.Blocks.AddParagraph;
    PA.ParaStyle.NumberingList := 1;
    PA.ParaStyle.Numbering := pnuArabic;
    PA.ParaStyle.FirstIndent := -20;
    PA.ParaStyle.LeftPadding := 40;
    TB := Memo.Blocks.AddTextBlock('This is a level 2 test text 3.');
    PA := Memo.Blocks.AddParagraph;
    PA.ParaStyle.NumberingList := 2;
    PA.ParaStyle.Numbering := pnuRomanLo;
    PA.ParaStyle.FirstIndent := -20;
    PA.ParaStyle.LeftPadding := 60;
    TB := Memo.Blocks.AddTextBlock('This is a test text 3.');
    PA := Memo.Blocks.AddParagraph;
    PA.ParaStyle.Numbering := pnuLetterHi;
    PA.ParaStyle.NumberStartAt := 1;
    PA.ParaStyle.FirstIndent := -20;
    PA.ParaStyle.LeftPadding := 20;
    TB := Memo.Blocks.AddTextBlock('This is a bullet text.');
    PA := Memo.Blocks.AddParagraph;
    PA.ParaStyle.Numbering := pnuBullets;
    PA.ParaStyle.FirstIndent := -20;
    PA.ParaStyle.LeftPadding := 20;
//    IB := Memo.Blocks.AddImageBlock('label_wre.png');
//    IB.ImageStyle.BorderWidth := 1;
//    IB.ImageStyle.BorderRadius := 10;
//    IB.ImageStyle.Brush.Color := clLime;
//    IB.LeftOffset := 0;
//    IB.TopOffset := 0;
//    IB.Position := mbpRelative;
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
    TBL.ColCount := 3;
    TBL.RowCount := 3;
//    TBL.Rows[0].Cells[0].FixedWidth := True;
//    TBL.Rows[1].Cells[1].FixedWidth := True;
    TBL.Rows[0].RequiredHeight := 200;
//    TBL.ColWidths[0] := 300;
//    TBl.ColWidths[1] := 300;
//    TBL.Rows[1].Cells[0].ColSpan := 2;
//    TBL.Rows[1].Cells[2].RowSpan := 2;
    TBL.Rows[1].Cells[1].ColSpan := 2;
    TBL.Rows[1].Cells[0].RowSpan := 2;
    AddTextField(TBL.Rows[0].Cells[0], True);
    AddTextField(TBL.Rows[0].Cells[1], True);
    AddTextField(TBL.Rows[0].Cells[2], True);
    AddTextField(TBL.Rows[1].Cells[0], True);
    AddTextField(TBL.Rows[1].Cells[1], False);
    AddTextField(TBL.Rows[1].Cells[2], False);
    AddTextField(TBL.Rows[2].Cells[0], True);
    AddTextField(TBL.Rows[2].Cells[1], True);
    AddTextField(TBL.Rows[2].Cells[2], True);
//    TBL.RequiredWidth := 600;
//    TBL.FixedWidth := True;
    TBL.ApplyDefaultCellStyle;}

{    PA := Memo.Blocks.AddParagraph;
    PA.ParaStyle.FirstIndent := 0;
    PA.ParaStyle.HAlign := halCenter;}


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

//    Memo.BackgroundImage.LoadFromFile('../../resource_src/clouds.jpg');
//    Memo.Colors.BkGnd := clGreen;
  finally
    //Memo.UnlockUpdate;
    Memo.Blocks.UnlockUpdate;
  end;
  MemoCopy := TKMemo.Create(Self);
  MemoCopy.ContentPadding.Top := 20;
  MemoCopy.ContentPadding.Left := 20;
  MemoCopy.ContentPadding.Right := 20;
  MemoCopy.ContentPadding.Bottom := 20;
  MemoCopy.Align := alClient;
  MemoCopy.Options := MemoCopy.Options + [eoShowFormatting, eoWantTab];
  MemoCopy.Parent := Panel2;
  MemoCopy.Blocks.AddTextBlock('This is test text 1.');
  MemoCopy.Blocks.AddParagraph;
  MemoCopy.Blocks.AddTextBlock('This is a test text 2.');
  MemoCopy.Blocks.AddParagraph;
end;

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
  S := UnicodeStringReplace(S, #9, TabAsUTF8, [rfReplaceAll]);
  Canvas.TextOut(10, 10, S);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  Panel1.Width := ClientWidth div 2;
end;

procedure TMainForm.BUPreviewClick(Sender: TObject);
begin
  KPrintPreviewDialog1.Control := Memo;
  KPrintPreviewDialog1.Execute;
end;

procedure TMainForm.BUPrintClick(Sender: TObject);
begin
  KPrintSetupDialog1.Control := Memo;
  KPrintSetupDialog1.Execute;
end;

procedure TMainForm.BULoadClick(Sender: TObject);
begin
  LoadFiles;
end;

procedure TMainForm.LoadFiles;
begin
  Memo.LoadFromRTF('test.rtf');
//  Memo.LoadFromRTF('test1.rtf');
//  Memo.LoadFromRTF('test_no_img.rtf');
//  Memo.LoadFromRTF('test_simple.rtf');
//  Memo.LoadFromRTF('kgrid_manual.rtf');
//  Memo.LoadFromRTF('../../../../_SC/wattrouter_eco/docu/manual_CZ/WATTrouterECO_CZ.rtf');
//  Memo.LoadFromRTF('simpletable.rtf');
//  Memo.LoadFromRTF('advancedtable.rtf');
//  Memo.Select(10, 510);
  Memo.SaveToRTF('test_save.rtf');

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
//  Mainform.Canvas.StretchDraw(ClientRect, MF);
  MemoCopy.LoadFromRTF('test_save.rtf');
//  MemoCopy.LoadFromRTF('test_save.rtf');
  MemoCopy.SaveToRTF('test_copy_save.rtf');
end;

procedure TMainForm.KMemo1DropFiles(Sender: TObject; X, Y: Integer;  Files: TStrings);
begin
  Memo.LoadFromFile(Files[0]);
end;

end.
