Software: KMemo component for Delphi and Lazarus
Original authorship: Tomas Krysl (tk@tkweb.eu)
-------------------

NOTE:
-------------------
This readme only covers the KMemo component.
Refer to kcontrols_readme.txt for installation requirements and other information.

PLANNED:
-------------------
- other paragraph styles (bullets and numbering, tab stops, line spacing)
- undo/redo commands
- search/replace commands
- readers/writers from/into other formats (HTML, ODT?)

KNOWN PROBLEMS:
-------------------
- Loading/Saving RTF documents created in different text processors may result in substantial differences in produced documents,
  as only a subset of RTF tags from the RTF specification v 1.9 is supported. RTF compatibility has been primarilly tested 
  with Microsoft Word which seems to comply with this RTF specification the most. Other text processors (such as LibreOffice Writer)
  may show completely different results for RTF documents created by KMemo.


CONTRIBUTORS:
-------------------


VERSION HISTORY
-------------------
Version 1.7 (July 2015): 
  Added:
    -TKMemoContainer class to support nested blocks, embedded objects and floating shapes
    -Advanced table support through TKMemoTable, TKMemoTableRow and TKMemoTableCell classes
    -many new paragraph and text formatting styles
    -Hyperlinks through TKMemoHyperlink class
    -advanced RTF reader and writer (KMemoRTF.pas)
    -clipboard operations (copy, cut, paste), also with full Rich Text Format support
    -print/preview support 
  Modified:
    -Reworked text and paragraph formatting through TKMemoTextStyle, TKMemoParaStyle classes
    -Reworked TKMemoBlocks, TKMemoTextBlock and TKMemoImageBlock

Version 1.5 (July 2014): 
  Added:
    -TKMemo component first introduced (early alpha state)
