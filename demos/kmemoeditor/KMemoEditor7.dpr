program KMemoEditor7;

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.res}
{$R xpman.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
