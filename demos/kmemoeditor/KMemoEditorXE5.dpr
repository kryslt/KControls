program KMemoEditorXE5;

{$include kcontrols.inc}

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.res}
{$IFDEF USE_THEMES}
  {$R xpman.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
