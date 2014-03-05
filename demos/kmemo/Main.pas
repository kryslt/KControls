unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, KGrids, KStdCtrls, StdCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Memo: TKMemo;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  Memo := TKMemo.Create(Self);
  Memo.BoundsRect := Rect(20, 20, 400, 300);
  Memo.Parent := Self;
end;

end.
