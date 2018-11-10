Software: KControls component suite for Delphi and Lazarus
Original authorship: Tomas Krysl
-------------------


LICENSE:
-------------------
License information for each source file can be found in it's header.
If there is none, the code is public domain.


SYSTEM REQUIREMENTS:
-------------------
- platforms: Win32, Win64, GTK, GTK2, QT, Carbon(untested), WinCE(partially tested)
- works under Delphi 7 and higher (tested on Delphi 7, DelphiXE+) 
  and Lazarus 1.2.2 and higher
- should work under Delphi 6
- some more problems might be experienced for older Lazarus versions
- see other readme files for additional informations about individual components


INSTALLATION:
-------------------
1. Compile and install package. It might be needed to specify search path to Source directory in Delphi/RAD Studio.
   For Rad Studio XE2 and later add VCL and VCL.Imaging namespaces to Unit Scope Names.
2. When compiling an application or demo, it might be needed to specify the search path to KControls sources 
   or JCL sources (if JCL is configured via kcontrols.inc).



BUG REPORTING:
-------------------
In case you find a bug or have ideas to improve please create an issue here:
https://bitbucket.org/tomkrysl/kcontrols/issues


INFO FOR CONTRIBUTORS:
-------------------
You may contribute to this project, in such case:
1) fork the repository on https://bitbucket.org/tomkrysl/kcontrols
2) implement and test your changes, if possible in all these IDEs:
  a) Delphi 7 and latest Delphi XE version under Windows 7 or higher
  b) Lazarus on Windows 7 or higher
  c) Lazarus on some version of Linux, preferrably Ubuntu or Kubuntu.
  d) Lazarus on MAC
  Please (if possible) keep in mind the differences between Delphi and Lazarus (mainly string encoding UTF16 in Delphi vs. UTF8 in Lazarus).
  When working with texts, always test with different Unicode characters.
  Testing of BiDi features is not required, because they are not supported by KControls.
3) create a pull request on https://bitbucket.org/tomkrysl/kcontrols.

If you plan to contribute regularly to this project, you can be granted write access to the repository.


VERSION HISTORY:
-------------------
Version 1.7 (August 2015):
  Added:
    -TKMemo major improvements 
    -TKPageControl, TKSpeedButton, TKSplitter components    
    -packages up to Delphi XE8
  

Version 1.6 (July 2014):
  Added:
    -TKDBGrid improvements
    -packages for Delphi XE6

Version 1.5 (July 2014):
  Added:
    -new components TKBitBtn, TKColorButton, TKMemo (early alpha state!)
    -TKGrid improvements

Version 1.4 (February 2014): 
  Added:
    -new components TKNumberEdit, TKFileNameEdit, TKLog, TKBrowseFolderDialog,
       TKLinkLabel, TKGradientLabel, TKPercentProgressBar
    -packages up to Delphi XE5

Version 1.3 (April 2012): 
  Added:
    -packages for Delphi XE2
  Modified:
    -no separate install packages KGrid, KHexEditor, KIcon

Version 1.2 (Oktober 2010): 
  Update - based on KGrid 1.7, KHexEditor 1.5, KIcon 2.2

Version 1.1 (Oktober 2010): 
  Update - based on KGrid 1.6, KHexEditor 1.4, KIcon 2.1

Version 1.0 (October 2009): 
  Initial release - based on KGrid 1.5, KHexEditor 1.4, KIcon 1.8