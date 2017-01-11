program KMemoDemoXE;

{$include kcontrols.inc}

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.res}
{$R xpman.res}

begin
{$IFDEF COMPILER10_UP}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
