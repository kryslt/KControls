program PWIG;

{$MODE Delphi}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  pwiggen in 'pwiggen.pas';

var
  PWIGMain: TPWIG;
begin
  try
    PWIGMain := TPWIG.Create;
    try
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
