unit KHelp;

{$include KControls.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Forms, ShellApi, Classes, SysUtils,
  HelpIntfs;

resourcestring
  sHelpErrorCaption = 'Help system error';
  sHelpError = 'Cannot find help file "%s"!';

const
  HH_DISPLAY_TOC   = $0001;
  HH_DISPLAY_INDEX = $0002;
  HH_DISPLAY_TOPIC = $0000;
  HH_CLOSE_ALL     = $0012;

procedure KHtmlHelp(uCommand: UINT; dwData: PDWORD; HelpFile: string = '');

implementation

uses
  KFunctions, KMessageBox;

function HtmlHelp(hwndCaller: HWND; pszFile: PChar; uCommand: UINT;
  dwData: PDWORD): HWND; stdcall; external 'hhctrl.ocx' Name 'HtmlHelpA';

procedure KHtmlHelp(uCommand: UINT; dwData: PDWORD; HelpFile: string = '');
begin
  if HelpFile = '' then
    HelpFile := Application.HelpFile;
  if HtmlHelp(0, PChar(HelpFile), uCommand, dwData) = 0 then
    AppMsgBox(sHelpErrorCaption, Format(sHelpError, [Application.HelpFile]), MB_OK + MB_ICONEXCLAMATION);
end;

end.
