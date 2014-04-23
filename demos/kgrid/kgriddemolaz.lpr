program kgriddemolaz;

uses
  Interfaces, Printer4Lazarus,
  Forms, kcontrolslaz,
  Main in 'main.pas',
  Input in 'input.pas';

{$IFDEF WINDOWS}{$R kgriddemolaz.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TInputForm, InputForm);
  Application.Run;
end.
