program HexEditor2009;

uses
  Forms,
  Res in 'Res.pas',
  Basic in 'Basic.pas',
  About in 'About.pas' {AboutForm},
  Search in 'Search.pas' {SearchForm},
  Replace in 'Replace.pas' {ReplaceForm},
  Options in 'Options.pas' {OptionsForm},
  PrintSetup in 'PrintSetup.pas' {PrintSetupForm},
  ReplacePrompt in 'ReplacePrompt.pas' {ReplacePromptForm},
  Main in 'Main.pas' {MainForm},
  PrintStatus in 'PrintStatus.pas' {PrintStatusForm},
  Preview in 'Preview.pas' {PreviewForm};

{$R *.RES}
{$R XPman.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSearchForm, SearchForm);
  Application.CreateForm(TReplaceForm, ReplaceForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TPrintSetupForm, PrintSetupForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TReplacePromptForm, ReplacePromptForm);
  Application.CreateForm(TPrintStatusForm, PrintStatusForm);
  Application.CreateForm(TPreviewForm, PreviewForm);
  Application.Run;
end.
