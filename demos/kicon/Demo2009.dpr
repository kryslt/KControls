program Demo2009;

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.res}
{$R XPman.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
