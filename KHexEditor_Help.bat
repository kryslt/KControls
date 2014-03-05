if not exist .\Help\KHexEditor\nul mkdir .\Help\KHexEditor
del .\Help\KHexEditor\*.chm
del .\Help\KHexEditor\*.hhc
del .\Help\KHexEditor\*.hhk
del .\Help\KHexEditor\*.hhp
del .\Help\KHexEditor\*.htm*
del .\Help\KHexEditor\*.gif
del .\Help\KHexEditor\*.log
DIPasDoc_Console.exe -OHtmlHelp -E.\Help\KHexEditor -Le -NKHexEditor "-TKHexEditor help" -Z.\style.css .\source\kfunctions.pas .\source\kdialogs.pas .\source\kgraphics.pas .\source\kcontrols.pas .\source\kwidewinprocs.pas .\Source\keditcommon.pas .\Source\khexeditor.pas
