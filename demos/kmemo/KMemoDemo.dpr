program KMemoDemo;

{$include kcontrols.inc}

uses
  Forms,
  Main in 'Main.pas' {Form2};

{$R *.res}

begin
{$IFDEF COMPILER10_UP}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
