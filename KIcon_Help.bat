if not exist .\Help\nul mkdir .\Help
del .\Help\*.chm
del .\Help\*.hhc
del .\Help\*.hhk
del .\Help\*.hhp
del .\Help\*.htm*
del .\Help\*.gif
del .\Help\*.log
DIPasDoc_Console.exe -DUSE_WINAPI -OHtmlHelp -E.\Help\KIcon -Le -NKIcon "-TKIcon help" -Z..\KComponents\style.css .\source\kfunctions.pas .\source\kgraphics.pas .\source\kicon.pas
