program PWIG;

{$mode delphi}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  pwiggen in 'pwiggen.pas';

var
  PWIGMain: TPWIG;

{$R *.res}

begin
  try
    PWIGMain := TPWIG.Create;
    try
      PWIGMain.PrintCopyright;
      if PWIGMain.ReadParams then
        PWIGMain.Generate
      else
        PWIGMain.PrintHelp;
    finally
      PWIGMain.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
