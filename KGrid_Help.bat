if not exist .\help\kgrid\nul mkdir .\help\kgrid
del .\help\kgrid\*.chm
del .\help\kgrid\*.hhc
del .\help\kgrid\*.hhk
del .\help\kgrid\*.hhp
del .\help\kgrid\*.htm*
del .\help\kgrid\*.gif
del .\help\kgrid\*.log
DIPasDoc_Console.exe -OHtmlHelp -E.\Help\KGrid -Le -Nkgrid "-TKGrid help" -Z..\KComponents\style.css .\source\kdbgrids.pas .\source\kgrids.pas .\source\kfunctions.pas .\source\kdialogs.pas .\source\kgraphics.pas .\source\kcontrols.pas .\source\kwidewinprocs.pas
