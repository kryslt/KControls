unit KIniFiles;

{$include KControls.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Messages, Classes, SysUtils, IniFiles, Forms;

type
  TKIniFile = class(TIniFile)
  private
  protected
  public
    function ReadRect(const Section, Ident: String;
      const Default: TRect): TRect; virtual;
    procedure WriteRect(const Section, Ident: String;
      const Value: TRect); virtual;
    function ReadWindowPlacement(const Section, Ident: string;
      DefWindowState: TWindowState; DefBoundsRect: TRect): TWindowPlacement;
    procedure WriteWindowPlacement(const Section, Ident: string;
      P: TWindowPlacement);
    procedure ReadFormState(F: TForm; const Section: string);
    procedure WriteFormState(F: TForm; const Section: string);
  end;

implementation

{ TKIniFile }

function TKIniFile.ReadRect(const Section, Ident: String;
  const Default: TRect): TRect;
begin
  with Result do
  begin
    Left := ReadInteger(Section, Ident + 'Left', Default.Left);
    Top := ReadInteger(Section, Ident + 'Top', Default.Top);
    Right := ReadInteger(Section, Ident + 'Right', Default.Right);
    Bottom := ReadInteger(Section, Ident + 'Bottom', Default.Bottom);
  end;
end;

procedure TKIniFile.WriteRect(const Section, Ident: String;
  const Value: TRect);
begin
  with Value do
  begin
    WriteInteger(Section, Ident + 'Left', Left);
    WriteInteger(Section, Ident + 'Top', Top);
    WriteInteger(Section, Ident + 'Right', Right);
    WriteInteger(Section, Ident + 'Bottom', Bottom);
  end;
end;

function TKIniFile.ReadWindowPlacement(const Section, Ident: string;
  DefWindowState: TWindowState; DefBoundsRect: TRect): TWindowPlacement;
const
  ShowCommands: array[TWindowState] of Integer =
    (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED);
begin
  Result.length := SizeOf(TWindowPlacement);
  Result.flags := WPF_SETMINPOSITION;
  Result.showCmd := ReadInteger(Section, Ident + 'State', ShowCommands[DefWindowState]);
  Result.ptMinPosition.x := ReadInteger(Section, Ident + 'MinLeft', 0);
  Result.ptMinPosition.y := ReadInteger(Section, Ident + 'MinTop', Screen.Height - GetSystemMetrics(SM_CYMINIMIZED));
  Result.ptMaxPosition.x := ReadInteger(Section, Ident + 'MaxLeft', 0);
  Result.ptMaxPosition.y := ReadInteger(Section, Ident + 'MaxTop', 0);
  Result.rcNormalPosition := ReadRect(Section, Ident, DefBoundsRect);
end;

procedure TKIniFile.WriteWindowPlacement(const Section, Ident: string;
  P: TWindowPlacement);
begin
  P.length := SizeOf(TWindowPlacement);
  WriteInteger(Section, Ident + 'State', P.showCmd);
  WriteInteger(Section, Ident + 'MinLeft', P.ptMinPosition.x);
  WriteInteger(Section, Ident + 'MinTop', P.ptMinPosition.y);
  WriteInteger(Section, Ident + 'MaxLeft', P.ptMaxPosition.x);
  WriteInteger(Section, Ident + 'MaxTop', P.ptMaxPosition.y);
  WriteRect(Section, Ident, P.rcNormalPosition);
end;

procedure TKIniFile.ReadFormState(F: TForm; const Section: string);
var
  Placement: TWindowPlacement;
begin
  if F <> nil then
  begin
    Placement := ReadWindowPlacement(Section, F.Name, F.WindowState, F.BoundsRect);
    SetWindowPlacement(F.Handle, @Placement);
  end;
end;

procedure TKIniFile.WriteFormState(F: TForm; const Section: string);
var
  Placement: TWindowPlacement;
begin
  if F <> nil then
  begin
    GetWindowPlacement(F.Handle, @Placement);
    WriteWindowPlacement(Section, F.Name, Placement);
  end;
end;

end.
