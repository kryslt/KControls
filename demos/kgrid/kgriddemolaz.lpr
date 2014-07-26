program kgriddemolaz;

uses
  Interfaces, Printer4Lazarus,
  Forms,
  Main in 'main.pas',
  Input in 'input.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TInputForm, InputForm);
  Application.Run;
end.
