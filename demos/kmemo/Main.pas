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
    BUtest: TButton;
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
    procedure FormResize(Sender: TObject);
    procedure KMemo1DropFiles(Sender: TObject; X, Y: Integer; Files: TStrings);
    procedure BULoadClick(Sender: TObject);
    procedure BUPreviewClick(Sender: TObject);
    procedure BUPrintClick(Sender: TObject);
    procedure BUTestClick(Sender: TObject);
  private
    { Private declarations }
    KMemo1: TKMemo;
    KMemo2: TKMemo;
    procedure LoadFiles;
    procedure Test1;
    procedure Test2;
    procedure Test3;
    procedure Test4;
    procedure Test5;
    procedure Test6;
    procedure Test7;
    procedure Test8;
    procedure Test9;
    procedure Test10;
    procedure Test11;
    procedure Test12;
    procedure Test13;
    procedure Test14;
    procedure Test15;
    procedure Test16;
    procedure Test17;
    procedure Test18;
    procedure Test19;
    procedure Test20;
    procedure Test21;
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

procedure TMainForm.FormCreate(Sender: TObject);
begin
  KMemo1 := TKMemo.Create(Self);
  KMemo1.ContentPadding.Top := 20;
  KMemo1.ContentPadding.Left := 20;
  KMemo1.ContentPadding.Right := 20;
  KMemo1.ContentPadding.Bottom := 20;
  KMemo1.Align := alClient;
  KMemo1.Options := KMemo1.Options + [eoDropFiles, eoShowFormatting, eoWantTab];
  KMemo1.OnDropFiles := KMemo1DropFiles;
  KMemo1.Parent := Panel1;
  KMemo1.PageSetup.Title := 'test_document';
  KMemo1.Clear;

  KMemo2 := TKMemo.Create(Self);
  KMemo2.ContentPadding.Top := 20;
  KMemo2.ContentPadding.Left := 20;
  KMemo2.ContentPadding.Right := 20;
  KMemo2.ContentPadding.Bottom := 20;
  KMemo2.Align := alClient;
  KMemo2.Options := KMemo2.Options + [eoShowFormatting, eoWantTab];
  KMemo2.Parent := Panel2;
  KMemo2.Clear;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  Panel1.Width := ClientWidth div 2;
end;

procedure TMainForm.BUPreviewClick(Sender: TObject);
begin
  Test20;
end;

procedure TMainForm.BUPrintClick(Sender: TObject);
begin
  Test21;
end;

procedure TMainForm.BULoadClick(Sender: TObject);
begin
  LoadFiles;
end;

procedure TMainForm.LoadFiles;
begin
//  KMemo1.LoadFromRTF('test.rtf');
//  KMemo1.LoadFromRTF('test1.rtf');
//  KMemo1.LoadFromRTF('test_no_img.rtf');
//  KMemo1.LoadFromRTF('test_simple.rtf');
  KMemo1.LoadFromRTF('kmemo_manual.rtf');
//  KMemo1.LoadFromRTF('simpletable.rtf');
//  KMemo1.LoadFromRTF('advancedtable.rtf');
//  KMemo1.Select(10, 510);
  KMemo1.SaveToRTF('test_save.rtf');

  KMemo2.LoadFromRTF('test_save.rtf');
  KMemo2.SaveToRTF('test_copy_save.rtf');
end;

procedure TMainForm.KMemo1DropFiles(Sender: TObject; X, Y: Integer;  Files: TStrings);
begin
  KMemo1.LoadFromFile(Files[0]);
end;

procedure TMainForm.BUTestClick(Sender: TObject);
begin
  Test18;
end;

procedure TMainForm.Test1;
begin
  KMemo1.Blocks.AddTextBlock('Hello world!');
end;

procedure TMainForm.Test2;
begin
  KMemo1.Blocks.Clear;
  KMemo1.Blocks.AddTextBlock('Hello world!');
end;

procedure TMainForm.Test3;
begin
  with KMemo1.Blocks do
  begin
    LockUpdate;
    try
      Clear;
      AddTextBlock('Hello world!');
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TMainForm.Test4;
begin
  with KMemo1.Blocks do
  begin
    LockUpdate;
    try
      Clear;
      AddTextBlock('First paragraph text!');
      AddParagraph;
      AddTextBlock('Second paragraph text!');
      AddParagraph;
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TMainForm.Test5;
var
  TB: TKMemoTextBlock;
begin
  TB := KMemo1.Blocks.AddTextBlock('Hello world!');
  TB.TextStyle.Font.Name := 'Arial';
  TB.TextStyle.Font.Color := clRed;
  TB.TextStyle.Font.Style := [fsBold];
end;

procedure TMainForm.Test6;
var
  TB: TKMemoTextBlock;
  PA: TKMemoParagraph;
begin
  TB := KMemo1.Blocks.AddTextBlock('Hello world!');
  PA := KMemo1.Blocks.AddParagraph;
  PA.ParaStyle.HAlign := halCenter;
  PA.ParaStyle.BottomPadding := 20;
end;

procedure TMainForm.Test7;
var
  TB: TKMemoTextBlock;
  PA: TKMemoParagraph;
begin
  TB := KMemo1.Blocks.AddTextBlock('Hello world!');
  PA := KMemo1.Blocks.AddParagraph;
  PA.Numbering := pnuArabic;
end;

procedure TMainForm.Test8;
var
  TB: TKMemoTextBlock;
  PA: TKMemoParagraph;
begin
  KMemo1.Blocks.LockUpdate;
  try
    KMemo1.Blocks.Clear;
    KMemo1.Blocks.AddTextBlock('This is test text 1. This is test text 1. This is test text 1. This is test text 1. This is test text 1. This is test text 1.');
    PA := KMemo1.Blocks.AddParagraph;
    PA.Numbering := pnuLetterHi;
    PA.NumberingListLevel.FirstIndent := -20;
    PA.NumberingListLevel.LeftIndent := 20;
    TB := KMemo1.Blocks.AddTextBlock('This is a test text 2.');
    PA := KMemo1.Blocks.AddParagraph;
    PA.Numbering := pnuLetterHi;
    TB := KMemo1.Blocks.AddTextBlock('This is a level 2 test text 1.');
    PA := KMemo1.Blocks.AddParagraph;
    PA.Numbering := pnuRomanLo;
    PA.NumberingListLevel.FirstIndent := -20;
    PA.NumberingListLevel.LeftIndent := 60;
    TB := KMemo1.Blocks.AddTextBlock('This is a level 2 test text 2.');
    PA := KMemo1.Blocks.AddParagraph;
    PA.Numbering := pnuRomanLo;
    TB := KMemo1.Blocks.AddTextBlock('This is a level 1 test text 1.');
    PA := KMemo1.Blocks.AddParagraph;
    PA.Numbering := pnuArabic;
    PA.NumberingListLevel.FirstIndent := -20;
    PA.NumberingListLevel.LeftIndent := 40;
    TB := KMemo1.Blocks.AddTextBlock('This is a level 1 test text 2.');
    PA := KMemo1.Blocks.AddParagraph;
    PA.Numbering := pnuArabic;
    TB := KMemo1.Blocks.AddTextBlock('This is a level 2 test text 3.');
    PA := KMemo1.Blocks.AddParagraph;
    PA.Numbering := pnuRomanLo;
    TB := KMemo1.Blocks.AddTextBlock('This is a test text 3.');
    PA := KMemo1.Blocks.AddParagraph;
    PA.Numbering := pnuLetterHi;
    PA.ParaStyle.NumberStartAt := 1;
    TB := KMemo1.Blocks.AddTextBlock('This is a bullet text.');
    PA := KMemo1.Blocks.AddParagraph;
    PA.Numbering := pnuBullets;
  finally
    KMemo1.Blocks.UnlockUpdate;
  end;
end;

procedure TMainForm.Test9;
begin
  KMemo1.Blocks.AddImageBlock('penguins.jpg');
end;

procedure TMainForm.Test10;
var
  IB: TKMemoImageBlock;
begin
  IB := KMemo1.Blocks.AddImageBlock('penguins.jpg');
  IB.Position := mbpRelative;
  IB.LeftOffset := 50;
end;

procedure TMainForm.Test11;
var
  CO: TKMemoContainer;
begin
  CO := KMemo1.Blocks.AddContainer;
  CO.Position := mbpRelative;
  CO.LeftOffset := 50;
  CO.TopOffset := 20;
  CO.FixedWidth := True;
  CO.RequiredWidth := 300;
  CO.BlockStyle.Brush.Color := clLime;
  CO.Blocks.AddTextBlock('Text in a container!');
  CO.Blocks.AddImageBlock('penguins.jpg');
end;

procedure TMainForm.Test12;
var
  TBL: TKMemoTable;
begin
  TBL := KMemo1.Blocks.AddTable;
  TBL.ColCount := 2;
  TBL.RowCount := 2;
  TBL.Cells[0, 0].Blocks.AddTextBlock('Table text 1');
  TBL.Cells[0, 1].Blocks.AddTextBlock('Table text 2');
  TBL.Cells[1, 0].Blocks.AddTextBlock('Table text 3');
  TBL.Cells[1, 1].Blocks.AddTextBlock('Table text 4');
  TBL.CellStyle.BorderWidth := 1;
  TBL.ApplyDefaultCellStyle;
end;

procedure TMainForm.Test13;

  procedure AddTextField(CO: TKMemoContainer; Text1: Boolean);
  var
    TB: TKMemoTextBlock;
    PA: TKMemoParagraph;
  begin
    CO.Blocks.LockUpdate;
    try
      if Text1 then
      begin
        TB := CO.Blocks.AddTextBlock('This is test text 1');
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
        CO.Blocks.AddHyperlink('www.google.com', 'www.google.com');
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
  TBL: TKMemoTable;
begin
  KMemo1.Blocks.LockUpdate;
  try
    TBL := KMemo1.Blocks.AddTable;
    TBL.BlockStyle.TopPadding := 20;
    TBL.BlockStyle.BottomPadding := 30;
    TBL.CellStyle.BorderWidth := 2;
    TBL.CellStyle.ContentPadding.AssignFromValues(5,5,5,5);
    TBL.CellStyle.Brush.Color := clWhite;
    TBL.ColCount := 3;
    TBL.RowCount := 3;
    TBL.Rows[0].RequiredHeight := 200;
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
  //    TBL.FixedWidth := True;
  //    TBL.RequiredWidth := 600;
    TBL.ApplyDefaultCellStyle;
  finally
    KMemo1.Blocks.UnLockUpdate;
  end;
end;

procedure TMainForm.Test14;
begin
  KMemo1.Blocks.AddHyperlink('www.google.com', 'www.google.com');
end;

procedure TMainForm.Test15;
begin
  KMemo1.Colors.BkGnd := clYellow;
  KMemo1.BackgroundImage.LoadFromFile('../../resource_src/clouds.jpg');
end;

procedure TMainForm.Test16;
begin
  KMemo1.TextStyle.Font.Name := 'Arial';
  KMemo1.TextStyle.Font.Size := 20;
  KMemo1.ParaStyle.HAlign := halCenter;
end;

procedure TMainForm.Test17;
begin
  KMemo1.LoadFromRTF('kmemo_manual.rtf');
  KMemo1.SaveToRTF('kmemo_manual_copy.rtf');
end;

procedure TMainForm.Test18;
begin
  KMemo1.ExecuteCommand(ecSelectAll);
  KMemo1.ExecuteCommand(ecCopy);
end;

procedure TMainForm.Test19;
var
  TextStyle: TKMemoTextStyle;
  ParaStyle: TKMemoParaStyle;
begin
  KMemo1.ExecuteCommand(ecSelectAll);
  ParaStyle := TKMemoParaStyle.Create;
  TextStyle := TKMemoTextStyle.Create;
  try
    TextStyle.Font.Style := [fsBold];
    ParaStyle.FirstIndent := 20;
    KMemo1.SelectionParaStyle := ParaStyle;
    KMemo1.SelectionTextStyle := TextStyle;
  finally
    ParaStyle.Free;
    TextStyle.Free;
  end;
end;

procedure TMainForm.Test20;
begin
  KPrintPreviewDialog1.Control := KMemo1;
  KPrintPreviewDialog1.Execute;
end;

procedure TMainForm.Test21;
begin
  KPrintSetupDialog1.Control := KMemo1;
  KPrintSetupDialog1.Execute;
end;

end.
