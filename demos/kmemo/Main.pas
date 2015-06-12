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
  Index: Integer;
  MS: TMemoryStream;
  S: AnsiString;
  W: TKString;
  I: Integer;
begin
  Memo := TKMemo.Create(Self);
  Memo.ContentPadding.Left := 20;
  Memo.ContentPadding.Right := 20;
  Memo.Align := alClient;
  Memo.Options := Memo.Options + [eoShowFormatting];
  Memo.Colors.BkGnd := clWhite;
  Memo.Font.Name := 'Arial';
  Memo.Font.Size := 36;
  Memo.Blocks.LockUpdate;
  Memo.Blocks.Clear(False);
  Memo.Blocks.AddTextBlock('This is a test text 1.');
  TB := Memo.Blocks.AddTextBlock(' This is a test text 2.');
  TB.Font.Size := 40;
  TB.Font.Color := clRed;
  TB := Memo.Blocks.AddTextBlock(' This is a test text 3.');
  TB.Font.Size := 30;
  TB.Font.Color := clBlue;
  TB.Brush.Color := clYellow;
//  Width := 50;
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
  Memo.Blocks.AddNewLineBlock;
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile('../../kgrid_readme.txt');
    SetString(S, PAnsiChar(MS.Memory), MS.Size);
    for I := 0 to 0 do
      Memo.Blocks.Text := S;
  finally
    MS.Free;
  end;
  Memo.BackgroundImage.LoadFromFile('../../resource_src/clouds.jpg');
  }
  Memo.Blocks.UnlockUpdate;
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
