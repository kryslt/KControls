if not exist .\Help\KControls\nul mkdir .\Help\KControls
del .\Help\KControls\*.chm
del .\Help\KControls\*.hhc
del .\Help\KControls\*.hhk
del .\Help\KControls\*.hhp
del .\Help\KControls\*.htm*
del .\Help\KControls\*.gif
del .\Help\KControls\*.log
DIPasDoc_Console.exe -DUSE_WINAPI -OHtmlHelp -E.\Help\KControls -Le -NKControls "-TKControls help" -Z..\KComponents\style.css .\source\kcontrols.pas .\source\kdialogs.pas .\source\keditcommon.pas .\source\kedits.pas .\source\kfunctions.pas .\source\kgraphics.pas .\source\kgrids.pas .\source\kdbgrids.pas .\source\khexeditor.pas .\Source\kicon.pas .\source\klabels.pas .\source\klog.pas .\source\kmessagebox.pas .\source\kprogress.pas .\source\kmemo.pas .\source\kmemortf.pas .\source\kbuttons.pas
