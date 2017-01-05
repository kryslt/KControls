Software: PWIG (Pascal Wrapper and Interface Generator)
Original authorship: Tomas Krysl (tk@tkweb.eu)
-------------------

NOTE:
-------------------
This readme only covers the PWIG tool. 
Refer to kcontrols_readme.txt for installation requirements and other information.

DESCRIPTION:
-------------------
PWIG is a software development tool that connects programs/libraries written in one programming language
with a variety of another programming languages. It reads the interface definitions
from a single XML configuration file and generates wrapper code for the caller (main program)
and the callee (shared library).

PWIG never parses any of your code.
This means you have to implement the wrapper classes on the callee side yourself.
The workflow is similar to eg. implementing the COM server in Delphi.

PWIG has been written in Free Pascal/Lazarus, hence its name (Pascal Wrapper and Interface Generator).
However, it can be used not only by Pascal programmers but for any other programming languages as well,
as long as its wrapper generators exist.

Syntax: PWIG [input file]

Example: PWIG text.xml

Entire configuration must be present in the input file.
You can create the input file with the PWIG GUI or write it by hand.
See additional documentation for the input file structure.


PLANNED:
-------------------
- PWIG GUI
- other wrapper generators (Python, Java, ...)

KNOWN PROBLEMS:
-------------------
General:
 -Only few basic data types are supported
 -Currency and Date&Time types not tested at all, use only at own risk

C# wrapper:
 -C# lacks support for indexed properties (what a shame, first I thought C# was modern language, but it is clearly not!!!)
 -C# output cannot be used in Xamarin PCL if you plan to build apps for iOS. You have to use shared project instead.
  This is because of the DllImport clause (and other attributes) which requires constant expressions.
  Again, DLL import stuff very badly designed in .NET!!!

C++ wrapper:
 -C++ properties supported only via __declspec(property) in Visual Studio
 -C++ lacks native support for events (you must override the caller wrapper class to use them)
 -indexed properties not supported at all

CONTRIBUTORS:
-------------------


VERSION HISTORY
-------------------
Version 0.9.2 (January 2017): Added wrapper generators for C++ (unmanaged code) and C# (managed code).
Version 0.9.1 (October 2016): Initial release.
