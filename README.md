# KControls component suite for Delphi and Lazarus

Original authorship: Tomas Krysl

## REPOSITORY LOCATION:
The repository was originally located at https://bitbucket.org/tomkrysl/kcontrols. 
Since January 2020 it is on Github, moved from Bitbucket because Atlassian discontinued Mercurial VCS in 2020.
The original repository should be deleted on 1th June 2020.
More info at https://bitbucket.org/blog/sunsetting-mercurial-support-in-bitbucket.
Bitbucket continues with GIT support but IMO GitHub is better for GIT.

## LICENSE:
License information for each source file can be found in it's header.
If there is none, the code is public domain.

## SYSTEM REQUIREMENTS:
- platforms: Win32, Win64, GTK, GTK2, QT, Carbon, Cocoa, WinCE(partially tested)
- IDEs: 
  - Delphi 7 and higher (tested on Delphi 7, DelphiXE+) 
  - Lazarus 1.2.2 and higher
- maybe will work with older Delphi with minor changes
- some more problems might be experienced for older Lazarus versions
- see other readme files for additional informations about individual components

## INSTALLATION:
1. Compile and install package. It might be needed to specify search path to Source directory in Delphi/RAD Studio.
   For Rad Studio XE2 and later add VCL and VCL.Imaging namespaces to Unit Scope Names.
2. When compiling an application or demo, it might be needed to specify the search path to KControls sources 
   or JCL sources (if JCL is configured via kcontrols.inc).

## BUG REPORTING:
In case you find a bug or have ideas to improve please create an issue or pull request here:

https://github.com/kryslt/KControls/issues

## IMPORTANT INFO FOR CONTRIBUTORS:
You may contribute to this project, in such case:
1. clone or fork the repository on https://github.com/kryslt/KControls/
2. implement and test your changes, if possible in all these IDEs:
   - Delphi 7 and latest Delphi version under Windows 7 or higher
   - Lazarus on Windows 7 or higher
   - Lazarus on some version of Linux, preferrably Ubuntu or Kubuntu.
   - Lazarus on MAC
3. create a pull request on https://github.com/kryslt/KControls/ or include a patch into the issue.

Please KEEP IN MIND THE DIFFERENCES between Delphi and Lazarus (mainly string encoding UTF16 in Delphi vs. UTF8 in Lazarus).

When working with texts, always test with different Unicode characters.

Testing of bidirectional text or aligning features is not required, because right to left mode is not supported by KControls.  

DON'T REMOVE ANY PROTECTED OR PUBLIC STUFF, the package is already heavily used by me and others!

This main repository must stay compilable in both IDEs!

I will not merge any changes that violate these rules!

If you need specific features please use your clone or fork.

If you plan to regularly contribute to this project and have enough experiences with this kind of programming, you can be granted write access to the repository.

## VERSION HISTORY:
Version 1.7 (August 2015):
- Added:
  - TKMemo major improvements 
  - TKPageControl, TKSpeedButton, TKSplitter components    
  - packages up to Delphi XE8
  
Version 1.6 (July 2014):
- Added:
  - TKDBGrid improvements
  - packages for Delphi XE6

Version 1.5 (July 2014):
- Added:
  - new components TKBitBtn, TKColorButton, TKMemo (early alpha state!)
  - TKGrid improvements

Version 1.4 (February 2014): 
- Added:
  - new components TKNumberEdit, TKFileNameEdit, TKLog, TKBrowseFolderDialog, TKLinkLabel, TKGradientLabel, TKPercentProgressBar
  - packages up to Delphi XE5

Version 1.3 (April 2012): 
- Added:
  - packages for Delphi XE2
- Modified:
  - no separate install packages KGrid, KHexEditor, KIcon

Version 1.2 (Oktober 2010): 
- Update - based on KGrid 1.7, KHexEditor 1.5, KIcon 2.2

Version 1.1 (Oktober 2010): 
- Update - based on KGrid 1.6, KHexEditor 1.4, KIcon 2.1

Version 1.0 (October 2009): 
- Initial release - based on KGrid 1.5, KHexEditor 1.4, KIcon 1.8
