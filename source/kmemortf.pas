{ @abstract(This unit contains RTF reader and writer for TKMemo control)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(28 Apr 2009)
  @lastmod(30 July 2015)

  Copyright © Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. All redistributions
  of the original or modified source code must retain the original copyright
  notice. The Author accepts no liability for any damage that may result
  from using this code.
}

unit KMemoRTF;

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  Classes, Contnrs, Graphics, Controls,
  KControls, KFunctions, KGraphics, KMemo;

type
  TKMemoRTFGroup = (rgNone, rgUnknown, rgColorTable, rgFontTable, rgFooter, rgHeader, rgInfo, rgPicture, rgShape, rgShapeInst, rgShapeResult, rgShapePict, rgStyleSheet, rgText);

  TKMemoRTFColor = class(TObject)
  private
    FColorRec: TKColorRec;
  public
    constructor Create;
    property ColorRec: TKColorRec read FColorRec write FColorRec;
    property Red: Byte read FColorRec.R write FColorRec.R;
    property Green: Byte read FColorRec.G write FColorRec.G;
    property Blue: Byte read FColorRec.B write FColorRec.B;
  end;

  TKMemoRTFColorTable = class(TObjectList)
  private
    function GetItem(Index: Integer): TKMemoRTFColor;
    procedure SetItem(Index: Integer; const Value: TKMemoRTFColor);
  public
    procedure AddColor(AColor: TColor); virtual;
    function GetColor(AIndex: Integer): TColor; virtual;
    function GetIndex(AColor: TColor): Integer; virtual;
    property Items[Index: Integer]: TKMemoRTFColor read GetItem write SetItem; default;
  end;

  TKMemoRTFFont = class(TObject)
  private
    FFont: TFont;
    FFontIndex: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Font: TFont read FFont;
    property FontIndex: Integer read FFontIndex write FFontIndex;
  end;

  TKMemoRTFFontTable = class(TObjectList)
  private
    function GetItem(Index: Integer): TKMemoRTFFont;
    procedure SetItem(Index: Integer; const Value: TKMemoRTFFont);
  public
    procedure AddFont(AFont: TFont); virtual;
    function GetFont(AFontIndex: Integer): TFont; virtual;
    function GetIndex(AFont: TFont): Integer; virtual;
    property Items[Index: Integer]: TKMemoRTFFont read GetItem write SetItem; default;
  end;

  TKMemoRTFShapeContentType = (sctUnknown, sctContainer, sctImage, sctRectangle, sctText);

  TKMemoRTFShape = class(TObject)
  private
    FContentPosition: TKRect;
    FContentType: TKMemoRTFShapeContentType;
    FCtrlName: AnsiString;
    FCtrlValue: AnsiString;
    FStyle: TKMemoBlockStyle;
    FWrap: Integer;
    FWrapSide: Integer;
    FHorzPosCode: Integer;
    FVertPosCode: Integer;
    procedure SetWrap(const Value: Integer);
    procedure SetWrapSide(const Value: Integer);
    function GetWrap: Integer;
    function GetWrapSide: Integer;
  protected
    procedure RTFWrapToWrapMode; virtual;
    procedure WrapModeToRTFWrap; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property ContentPosition: TKRect read FContentPosition;
    property ContentType: TKMemoRTFShapeContentType read FContentType write FContentType;
    property CtrlName: AnsiString read FCtrlName write FCtrlName;
    property CtrlValue: AnsiString read FCtrlValue write FCtrlValue;
    property HorzPosCode: Integer read FHorzPosCode write FHorzPosCode;
    property Style: TKMemoBlockStyle read FStyle;
    property VertPosCode: Integer read FVertPosCode write FVertPosCode;
    property Wrap: Integer read GetWrap write SetWrap;
    property WrapSide: Integer read GetWrapSide write SetWrapSide;
  end;

  TKMemoRTFReader = class(TObject)
  private
    function GetActiveFont: TKMemoRTFFont;
    function GetActiveColor: TKMemoRTFColor;
    function GetActiveImage: TKMemoImageBlock;
    function GetActiveShape: TKMemoRTFShape;
    function GetActiveContainer: TKMemoContainer;
    function GetActiveTable: TKMemoTable;
  protected
    FActiveColor: TKMemoRTFColor;
    FActiveContainer: TKMemoContainer;
    FActiveFont: TKMemoRTFFont;
    FActiveImage: TKMemoImageBlock;
    FActiveShape: TKMemoRTFShape;
    FActiveString: TKString;
    FActiveTable: TKMemoTable;
    FActiveText: TKMemoTextBlock;
    FBackgroundImage: Boolean;
    FBlocks: TKMemoBlocks;
    FColorTable: TKMemoRTFColorTable;
    FDefFontIndex: Integer;
    FFontTable: TKMemoRTFFontTable;
    FHeaderRead: Boolean;
    FImageClass: TGraphicClass;
    FImageEnhMetafile: Boolean;
    FImageOriginalHeight: Integer;
    FImageOriginalWidth: Integer;
    FIgnoreChars: Integer;
    FGraphicClass: TGraphicClass;
    FMemo: TKCustomMemo;
    FParaBorder: TAlign;
    FStream: TMemoryStream;
    FTableBorder: TAlign;
    FTableCell: TKMemoTableCell;
    FTableCellXPos: Integer;
    FTableCol: Integer;
    FTableColCount: Integer;
    FTableRow: TKMemoTableRow;
    FTableLastRow: Boolean;
    FTextStyle: TKMemoTextStyle;
    procedure AddText(const APart: TKString; ATextStyle: TKMemoTextStyle); virtual;
    procedure ApplyFont(ATextStyle: TKMemoTextStyle; AFontIndex: Integer; var CodePage: Integer); virtual;
    procedure ApplyHighlight(ATextStyle: TKMemoTextStyle; AHighlightCode: Integer); virtual;
    function ParamToBool(const AValue: AnsiString): Boolean; virtual;
    function ParamToColor(const AValue: AnsiString): TColor; virtual;
    function ParamToInt(const AValue: AnsiString): Integer; virtual;
    function ParamToEMU(const AValue: AnsiString): Integer; virtual;
    function EMUToPoints(AValue: Integer): Integer;
    procedure FlushColor; virtual;
    procedure FlushContainer; virtual;
    procedure FlushFont; virtual;
    procedure FlushImage; virtual;
    procedure FlushParagraph(ATextStyle: TKMemoTextStyle; AParaStyle: TKMemoParaStyle); virtual;
    procedure FlushShape; virtual;
    procedure FlushTable; virtual;
    procedure FlushText; virtual;
    function HighlightCodeToColor(AValue: Integer): TColor; virtual;
    function ReadNext(out ACtrl, AText: AnsiString; out AParam: Int64): Boolean; virtual;
    procedure ReadColorGroup(const ACtrl, AText: AnsiString; AParam: Integer); virtual;
    procedure ReadFontGroup(const ACtrl, AText: AnsiString; AParam: Integer); virtual;
    function ReadHeaderGroup(const ACtrl, AText: AnsiString; AParam: Integer; var AGroup: TKMemoRTFGroup; var ACodePage: Integer): Boolean; virtual;
    procedure ReadGroup(GroupIndex: Integer; Group: TKMemoRTFGroup; ATextStyle: TKMemoTextStyle; AParaStyle: TKMemoParaStyle; CodePage, IgnoreUnicodeChars: Integer); virtual;
    procedure ReadPictureGroup(const ACtrl, AText: AnsiString; AParam: Integer); virtual;
    function ReadShapeGroup(const ACtrl, AText: AnsiString; AParam: Integer): Boolean; virtual;
    function ReadParaFormatting(const ACtrl: AnsiString; AParam: Integer; AParaStyle: TKMemoParaStyle): Boolean; virtual;
    function ReadSpecialCharacter(const ACtrl: AnsiString; AParam: Integer; ATextStyle: TKMemoTextStyle; ACodePage, AIgnoreUnicodeChars: Integer): Boolean; virtual;
    function ReadTableFormatting(const ACtrl: AnsiString; AParam: Integer; ATextStyle: TKMemoTextStyle; AParaStyle: TKMemoParaStyle): Boolean; virtual;
    function ReadTextFormatting(const ACtrl: AnsiString; AParam: Integer; ATextStyle: TKMemoTextStyle; var ACodePage: Integer): Boolean; virtual;
    function TwipsToPoints(AValue: Integer): Integer; virtual;
    property ActiveColor: TKMemoRTFColor read GetActiveColor;
    property ActiveContainer: TKMemoContainer read GetActiveContainer;
    property ActiveFont: TKMemoRTFFont read GetActiveFont;
    property ActiveImage: TKMemoImageBlock read GetActiveImage;
    property ActiveShape: TKMemoRTFShape read GetActiveShape;
    property ActiveTable: TKMemoTable read GetActiveTable;
  public
    constructor Create(AMemo: TKCustomMemo); virtual;
    destructor Destroy; override;
    procedure Load(const AFileName: TKString); virtual;
  end;

  { TKMemoRTFWriter }

  TKMemoRTFWriter = class(TObject)
  private
  protected
    FCodePage: Integer;
    FColorTable: TKMemoRTFColorTable;
    FFontTable: TKMemoRTFFontTable;
    FMemo: TKCustomMemo;
    FStream: TMemoryStream;
    function ColorToHighlightCode(AValue: TColor): Integer; virtual;
    procedure FillColorTable(ABlocks: TKMemoBlocks); virtual;
    procedure FillFontTable(ABlocks: TKMemoBlocks); virtual;
    function PointsToTwips(AValue: Integer): Integer; virtual;
    procedure WriteBody(ABlocks: TKMemoBlocks; var AGroup: Integer; AInsideTable: Boolean); virtual;
    procedure WriteColorTable(var AGroup: Integer); virtual;
    procedure WriteCtrl(const ACtrl: AnsiString);
    procedure WriteCtrlParam(const ACtrl: AnsiString; AParam: Integer);
    procedure WriteFontTable(var AGroup: Integer); virtual;
    procedure WriteGroupBegin(var AGroup: Integer);
    procedure WriteGroupEnd(var AGroup: Integer);
    procedure WriteHeader(AGroup: Integer); virtual;
    procedure WriteParagraph(AItem: TKMemoParagraph; var AGroup: Integer; AInsideTable: Boolean); virtual;
    procedure WriteParaStyle(AParaStyle: TKMemoParaStyle); virtual;
    procedure WriteSemiColon;
    procedure WriteSpace;
    procedure WriteString(const AText: AnsiString);
    procedure WriteTable(AItem: TKMemoTable; var AGroup: Integer); virtual;
    procedure WriteTableRowProperties(ATable: TKmemoTable; ARowIndex: Integer); virtual;
    procedure WriteTextBlock(AItem: TKMemoTextBlock; var AGroup: Integer); virtual;
    procedure WriteTextStyle(ATextStyle: TKMemoTextStyle); virtual;
    procedure WriteUnicodeString(const AText: TKString); virtual;
  public
    constructor Create(AMemo: TKCustomMemo); virtual;
    destructor Destroy; override;
    procedure Save(const AFileName: TKString); virtual;
  end;

implementation

uses
  Math, SysUtils, KHexEditor, KRes
{$IFDEF FPC}
  , LCLProc, LConvEncoding
{$ELSE}
  , JPeg, Windows
{$ENDIF}
  ;

function CharSetToCP(ACharSet: TFontCharSet): Integer;
begin
  case ACharset of
    1: Result := 0; //Default
    2: Result := 42; //Symbol
    77: Result := 10000; //Mac Roman
    78: Result := 10001; //Mac Shift Jis
    79: Result := 10003; //Mac Hangul
    80: Result := 10008; //Mac GB2312
    81: Result := 10002; //Mac Big5
    83: Result := 10005; //Mac Hebrew
    84: Result := 10004; //Mac Arabic
    85: Result := 10006; //Mac Greek
    86: Result := 10081; //Mac Turkish
    87: Result := 10021; //Mac Thai
    88: Result := 10029; //Mac East Europe
    89: Result := 10007; //Mac Russian
    128: Result := 932; //Shift JIS
    129: Result := 949; //Hangul
    130: Result := 1361; //Johab
    134: Result := 936; //GB2312
    136: Result := 950; //Big5
    161: Result := 1253; //Greek
    162: Result := 1254; //Turkish
    163: Result := 1258; //Vietnamese
    177: Result := 1255; //Hebrew
    178: Result := 1256; //Arabic
    186: Result := 1257; //Baltic
    204: Result := 1251; //Russian
    222: Result := 874; //Thai
    238: Result := 1250; //Eastern European
    254: Result := 437; //PC 437
    255: Result := 850; //OEM
  else
    Result := 1252; //ANSI
  end;
end;

function CPToCharSet(ACodePage: Integer): TFontCharSet;
begin
  case ACodePage of
    0: Result := 1; //Default
    42: Result := 2; //Symbol
    10000: Result := 77; //Mac Roman
    10001: Result := 78; //Mac Shift Jis
    10003: Result := 79; //Mac Hangul
    10008: Result := 80; //Mac GB2312
    10002: Result := 81; //Mac Big5
    10005: Result := 83; //Mac Hebrew
    10004: Result := 84; //Mac Arabic
    10006: Result := 85; //Mac Greek
    10081: Result := 86; //Mac Turkish
    10021: Result := 87; //Mac Thai
    10029: Result := 88; //Mac East Europe
    10007: Result := 89; //Mac Russian
    932: Result := 128; //Shift JIS
    949: Result := 129; //Hangul
    1361: Result := 130; //Johab
    936: Result := 134; //GB2312
    950: Result := 136; //Big5
    1253: Result := 161; //Greek
    1254: Result := 162; //Turkish
    1258: Result := 163; //Vietnamese
    1255: Result := 177; //Hebrew
    1256: Result := 178; //Arabic
    1257: Result := 186; //Baltic
    1251: Result := 204; //Russian
    874: Result := 222; //Thai
    1250: Result := 238; //Eastern European
    437: Result := 254; //PC 437
    850: Result := 255; //OEM
  else
    Result := 0; //ANSI
  end;
end;

function AdobeSymbolToUTF16(AValue: Integer): Integer;
begin
  case AValue of
    $20: Result := $0020;  //SPACE //space
    {/$20: Result := $00A0;  //NO-BREAK SPACE	//space }
    $21: Result := $0021;  //EXCLAMATION MARK //exclam
    $22: Result := $2200;  //FOR ALL //universal
    $23: Result := $0023;  //NUMBER SIGN //numbersign
    $24: Result := $2203;  //THERE EXISTS //existential
    $25: Result := $0025;  //PERCENT SIGN //percent
    $26: Result := $0026;  //AMPERSAND //ampersand
    $27: Result := $220B;  //CONTAINS AS MEMBER //suchthat
    $28: Result := $0028;  //LEFT PARENTHESIS //parenleft
    $29: Result := $0029;  //RIGHT PARENTHESIS //parenright
    $2A: Result := $2217;  //ASTERISK OPERATOR //asteriskmath
    $2B: Result := $002B;  //PLUS SIGN //plus
    $2C: Result := $002C; //COMMA //comma
    $2D: Result := $2212; //MINUS SIGN //minus
    $2E: Result := $002E; //FULL STOP //period
    $2F: Result := $002F; //SOLIDUS //slash
    $30: Result := $0030; //DIGIT ZERO //zero
    $31: Result := $0031; //DIGIT ONE //one
    $32: Result := $0032; //DIGIT TWO //two
    $33: Result := $0033; //DIGIT THREE //three
    $34: Result := $0034; //DIGIT FOUR //four
    $35: Result := $0035; //DIGIT FIVE //five
    $36: Result := $0036; //DIGIT SIX //six
    $37: Result := $0037; //DIGIT SEVEN //seven
    $38: Result := $0038; //DIGIT EIGHT //eight
    $39: Result := $0039; //DIGIT NINE //nine
    $3A: Result := $003A; //COLON //colon
    $3B: Result := $003B; //SEMICOLON //semicolon
    $3C: Result := $003C; //LESS-THAN SIGN //less
    $3D: Result := $003D; //EQUALS SIGN //equal
    $3E: Result := $003E; //GREATER-THAN SIGN //greater
    $3F: Result := $003F; //QUESTION MARK //question
    $40: Result := $2245; //APPROXIMATELY EQUAL TO //congruent
    $41: Result := $0391; //GREEK CAPITAL LETTER ALPHA //Alpha
    $42: Result := $0392; //GREEK CAPITAL LETTER BETA //Beta
    $43: Result := $03A7; //GREEK CAPITAL LETTER CHI //Chi
    $44: Result := $0394; //GREEK CAPITAL LETTER DELTA //Delta
    {/$44: Result := $2206; //INCREMENT //Delta}
    $45: Result := $0395; //GREEK CAPITAL LETTER EPSILON //Epsilon
    $46: Result := $03A6; //GREEK CAPITAL LETTER PHI //Phi
    $47: Result := $0393; //GREEK CAPITAL LETTER GAMMA //Gamma
    $48: Result := $0397; //GREEK CAPITAL LETTER ETA //Eta
    $49: Result := $0399; //GREEK CAPITAL LETTER IOTA //Iota
    $4A: Result := $03D1; //GREEK THETA SYMBOL //theta1
    $4B: Result := $039A; //GREEK CAPITAL LETTER KAPPA //Kappa
    $4C: Result := $039B; //GREEK CAPITAL LETTER LAMDA //Lambda
    $4D: Result := $039C; //GREEK CAPITAL LETTER MU //Mu
    $4E: Result := $039D; //GREEK CAPITAL LETTER NU //Nu
    $4F: Result := $039F; //GREEK CAPITAL LETTER OMICRON //Omicron
    $50: Result := $03A0; //GREEK CAPITAL LETTER PI //Pi
    $51: Result := $0398; //GREEK CAPITAL LETTER THETA //Theta
    $52: Result := $03A1; //GREEK CAPITAL LETTER RHO //Rho
    $53: Result := $03A3; //GREEK CAPITAL LETTER SIGMA //Sigma
    $54: Result := $03A4; //GREEK CAPITAL LETTER TAU //Tau
    $55: Result := $03A5; //GREEK CAPITAL LETTER UPSILON //Upsilon
    $56: Result := $03C2; //GREEK SMALL LETTER FINAL SIGMA //sigma1
    $57: Result := $03A9; //GREEK CAPITAL LETTER OMEGA //Omega
    {/$57: Result := $2126; //OHM SIGN //Omega}
    $58: Result := $039E; //GREEK CAPITAL LETTER XI //Xi
    $59: Result := $03A8; //GREEK CAPITAL LETTER PSI //Psi
    $5A: Result := $0396; //GREEK CAPITAL LETTER ZETA //Zeta
    $5B: Result := $005B; //LEFT SQUARE BRACKET //bracketleft
    $5C: Result := $2234; //THEREFORE //therefore
    $5D: Result := $005D; //RIGHT SQUARE BRACKET //bracketright
    $5E: Result := $22A5; //UP TACK //perpendicular
    $5F: Result := $005F; //LOW LINE //underscore
    $60: Result := $F8E5; //RADICAL EXTENDER //radicalex (CUS)
    $61: Result := $03B1; //GREEK SMALL LETTER ALPHA //alpha
    $62: Result := $03B2; //GREEK SMALL LETTER BETA //beta
    $63: Result := $03C7; //GREEK SMALL LETTER CHI //chi
    $64: Result := $03B4; //GREEK SMALL LETTER DELTA //delta
    $65: Result := $03B5; //GREEK SMALL LETTER EPSILON //epsilon
    $66: Result := $03C6; //GREEK SMALL LETTER PHI //phi
    $67: Result := $03B3; //GREEK SMALL LETTER GAMMA //gamma
    $68: Result := $03B7; //GREEK SMALL LETTER ETA //eta
    $69: Result := $03B9; //GREEK SMALL LETTER IOTA //iota
    $6A: Result := $03D5; //GREEK PHI SYMBOL //phi1
    $6B: Result := $03BA; //GREEK SMALL LETTER KAPPA //kappa
    $6C: Result := $03BB; //GREEK SMALL LETTER LAMDA //lambda
    $6D: Result := $00B5; //MICRO SIGN //mu
    {/$6D: Result := $03BC; //GREEK SMALL LETTER MU //mu}
    $6E: Result := $03BD; //GREEK SMALL LETTER NU //nu
    $6F: Result := $03BF; //GREEK SMALL LETTER OMICRON //omicron
    $70: Result := $03C0; //GREEK SMALL LETTER PI //pi
    $71: Result := $03B8; //GREEK SMALL LETTER THETA //theta
    $72: Result := $03C1; //GREEK SMALL LETTER RHO //rho
    $73: Result := $03C3; //GREEK SMALL LETTER SIGMA //sigma
    $74: Result := $03C4; //GREEK SMALL LETTER TAU //tau
    $75: Result := $03C5; //GREEK SMALL LETTER UPSILON //upsilon
    $76: Result := $03D6; //GREEK PI SYMBOL //omega1
    $77: Result := $03C9; //GREEK SMALL LETTER OMEGA //omega
    $78: Result := $03BE; //GREEK SMALL LETTER XI //xi
    $79: Result := $03C8; //GREEK SMALL LETTER PSI //psi
    $7A: Result := $03B6; //GREEK SMALL LETTER ZETA //zeta
    $7B: Result := $007B; //LEFT CURLY BRACKET //braceleft
    $7C: Result := $007C; //VERTICAL LINE //bar
    $7D: Result := $007D; //RIGHT CURLY BRACKET //braceright
    $7E: Result := $223C; //TILDE OPERATOR //similar
    $A0: Result := $20AC; //EURO SIGN //Euro
    $A1: Result := $03D2; //GREEK UPSILON WITH HOOK SYMBOL //Upsilon1
    $A2: Result := $2032; //PRIME //minute
    $A3: Result := $2264; //LESS-THAN OR EQUAL TO //lessequal
    $A4: Result := $2044; //FRACTION SLASH //fraction
    {/$A4: Result := $2215; //DIVISION SLASH //fraction}
    $A5: Result := $221E; //INFINITY //infinity
    $A6: Result := $0192; //LATIN SMALL LETTER F WITH HOOK //florin
    $A7: Result := $2663; //BLACK CLUB SUIT //club
    $A8: Result := $2666; //BLACK DIAMOND SUIT //diamond
    $A9: Result := $2665; //BLACK HEART SUIT //heart
    $AA: Result := $2660; //BLACK SPADE SUIT //spade
    $AB: Result := $2194; //LEFT RIGHT ARROW //arrowboth
    $AC: Result := $2190; //LEFTWARDS ARROW //arrowleft
    $AD: Result := $2191; //UPWARDS ARROW //arrowup
    $AE: Result := $2192; //RIGHTWARDS ARROW //arrowright
    $AF: Result := $2193; //DOWNWARDS ARROW //arrowdown
    $B0: Result := $00B0; //DEGREE SIGN //degree
    $B1: Result := $00B1; //PLUS-MINUS SIGN //plusminus
    $B2: Result := $2033; //DOUBLE PRIME //second
    $B3: Result := $2265; //GREATER-THAN OR EQUAL TO //greaterequal
    $B4: Result := $00D7; //MULTIPLICATION SIGN //multiply
    $B5: Result := $221D; //PROPORTIONAL TO //proportional
    $B6: Result := $2202; //PARTIAL DIFFERENTIAL //partialdiff
    $B7: Result := $2022; //BULLET //bullet
    $B8: Result := $00F7; //DIVISION SIGN //divide
    $B9: Result := $2260; //NOT EQUAL TO //notequal
    $BA: Result := $2261; //IDENTICAL TO //equivalence
    $BB: Result := $2248; //ALMOST EQUAL TO //approxequal
    $BC: Result := $2026; //HORIZONTAL ELLIPSIS //ellipsis
    $BD: Result := $F8E6; //VERTICAL ARROW EXTENDER //arrowvertex (CUS)
    $BE: Result := $F8E7; //HORIZONTAL ARROW EXTENDER //arrowhorizex (CUS)
    $BF: Result := $21B5; //DOWNWARDS ARROW WITH CORNER LEFTWARDS //carriagereturn
    $C0: Result := $2135; //ALEF SYMBOL //aleph
    $C1: Result := $2111; //BLACK-LETTER CAPITAL I //Ifraktur
    $C2: Result := $211C; //BLACK-LETTER CAPITAL R //Rfraktur
    $C3: Result := $2118; //SCRIPT CAPITAL P //weierstrass
    $C4: Result := $2297; //CIRCLED TIMES //circlemultiply
    $C5: Result := $2295; //CIRCLED PLUS //circleplus
    $C6: Result := $2205; //EMPTY SET //emptyset
    $C7: Result := $2229; //INTERSECTION //intersection
    $C8: Result := $222A; //UNION //union
    $C9: Result := $2283; //SUPERSET OF //propersuperset
    $CA: Result := $2287; //SUPERSET OF OR EQUAL TO //reflexsuperset
    $CB: Result := $2284; //NOT A SUBSET OF //notsubset
    $CC: Result := $2282; //SUBSET OF //propersubset
    $CD: Result := $2286; //SUBSET OF OR EQUAL TO //reflexsubset
    $CE: Result := $2208; //ELEMENT OF //element
    $CF: Result := $2209; //NOT AN ELEMENT OF //notelement
    $D0: Result := $2220; //ANGLE //angle
    $D1: Result := $2207; //NABLA //gradient
    $D2: Result := $F6DA; //REGISTERED SIGN SERIF //registerserif (CUS)
    $D3: Result := $F6D9; //COPYRIGHT SIGN SERIF //copyrightserif (CUS)
    $D4: Result := $F6DB; //TRADE MARK SIGN SERIF //trademarkserif (CUS)
    $D5: Result := $220F; //N-ARY PRODUCT //product
    $D6: Result := $221A; //SQUARE ROOT //radical
    $D7: Result := $22C5; //DOT OPERATOR //dotmath
    $D8: Result := $00AC; //NOT SIGN //logicalnot
    $D9: Result := $2227; //LOGICAL AND //logicaland
    $DA: Result := $2228; //LOGICAL OR //logicalor
    $DB: Result := $21D4; //LEFT RIGHT DOUBLE ARROW //arrowdblboth
    $DC: Result := $21D0; //LEFTWARDS DOUBLE ARROW //arrowdblleft
    $DD: Result := $21D1; //UPWARDS DOUBLE ARROW //arrowdblup
    $DE: Result := $21D2; //RIGHTWARDS DOUBLE ARROW //arrowdblright
    $DF: Result := $21D3; //DOWNWARDS DOUBLE ARROW //arrowdbldown
    $E0: Result := $25CA; //LOZENGE //lozenge
    $E1: Result := $2329; //LEFT-POINTING ANGLE BRACKET //angleleft
    $E2: Result := $F8E8; //REGISTERED SIGN SANS SERIF //registersans (CUS)
    $E3: Result := $F8E9; //COPYRIGHT SIGN SANS SERIF //copyrightsans (CUS)
    $E4: Result := $F8EA; //TRADE MARK SIGN SANS SERIF //trademarksans (CUS)
    $E5: Result := $2211; //N-ARY SUMMATION //summation
    $E6: Result := $F8EB; //LEFT PAREN TOP //parenlefttp (CUS)
    $E7: Result := $F8EC; //LEFT PAREN EXTENDER //parenleftex (CUS)
    $E8: Result := $F8ED; //LEFT PAREN BOTTOM //parenleftbt (CUS)
    $E9: Result := $F8EE; //LEFT SQUARE BRACKET TOP //bracketlefttp (CUS)
    $EA: Result := $F8EF; //LEFT SQUARE BRACKET EXTENDER //bracketleftex (CUS)
    $EB: Result := $F8F0; //LEFT SQUARE BRACKET BOTTOM //bracketleftbt (CUS)
    $EC: Result := $F8F1; //LEFT CURLY BRACKET TOP //bracelefttp (CUS)
    $ED: Result := $F8F2; //LEFT CURLY BRACKET MID //braceleftmid (CUS)
    $EE: Result := $F8F3; //LEFT CURLY BRACKET BOTTOM //braceleftbt (CUS)
    $EF: Result := $F8F4; //CURLY BRACKET EXTENDER //braceex (CUS)
    $F1: Result := $232A; //RIGHT-POINTING ANGLE BRACKET //angleright
    $F2: Result := $222B; //INTEGRAL //integral
    $F3: Result := $2320; //TOP HALF INTEGRAL //integraltp
    $F4: Result := $F8F5; //INTEGRAL EXTENDER //integralex (CUS)
    $F5: Result := $2321; //BOTTOM HALF INTEGRAL //integralbt
    $F6: Result := $F8F6; //RIGHT PAREN TOP //parenrighttp (CUS)
    $F7: Result := $F8F7; //RIGHT PAREN EXTENDER //parenrightex (CUS)
    $F8: Result := $F8F8; //RIGHT PAREN BOTTOM //parenrightbt (CUS)
    $F9: Result := $F8F9; //RIGHT SQUARE BRACKET TOP //bracketrighttp (CUS)
    $FA: Result := $F8FA; //RIGHT SQUARE BRACKET EXTENDER //bracketrightex (CUS)
    $FB: Result := $F8FB; //RIGHT SQUARE BRACKET BOTTOM //bracketrightbt (CUS)
    $FC: Result := $F8FC; //RIGHT CURLY BRACKET TOP //bracerighttp (CUS)
    $FD: Result := $F8FD; //RIGHT CURLY BRACKET MID //bracerightmid (CUS)
    $FE: Result := $F8FE; //RIGHT CURLY BRACKET BOTTOM //bracerightbt (CUS)
  else
    Result := AValue;
  end;
end;

{ TKMemoRTFColor }

constructor TKMemoRTFColor.Create;
begin
  FColorRec.Value := 0;
end;

{ TKMemoRTFColorTable }

procedure TKMemoRTFColorTable.AddColor(AColor: TColor);
var
  RTFColor: TKMemoRTFColor;
begin
  if AColor <> clNone then
  begin
    if GetIndex(AColor) < 0 then
    begin
      RTFColor := TKMemoRTFColor.Create;
      RTFColor.ColorRec := ColorToColorRec(AColor);
      Add(RTFColor);
    end;
  end;
end;

function TKMemoRTFColorTable.getColor(AIndex: Integer): TColor;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := ColorRecToColor(Items[AIndex].ColorRec)
  else
    Result := clNone;
end;

function TKMemoRTFColorTable.GetIndex(AColor: TColor): Integer;
var
  I: Integer;
  Color, ColorRec: TKColorRec;
begin
  Color := ColorToColorRec(AColor);
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    ColorRec := Items[I].ColorRec;
    if ColorRec.Value = Color.Value then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TKMemoRTFColorTable.GetItem(Index: Integer): TKMemoRTFColor;
begin
  Result := TKMemoRTFColor(inherited GetItem(Index));
end;

procedure TKMemoRTFColorTable.SetItem(Index: Integer; const Value: TKMemoRTFColor);
begin
  inherited SetItem(Index, Value);
end;

{ TKMemoRTFFont }

constructor TKMemoRTFFont.Create;
begin
  FFont := TFont.Create;
end;

destructor TKMemoRTFFont.Destroy;
begin
  FFont.Free;
  inherited;
end;

{ TKMemoRTFFontTable }

procedure TKMemoRTFFontTable.AddFont(AFont: TFont);
var
  RTFFont: TKMemoRTFFont;
begin
  if GetIndex(AFont) < 0 then
  begin
    RTFFont := TKmemoRTFFont.Create;
    RTFFont.Font.Assign(AFont);
    RTFFont.FontIndex := Count;
    Add(RTFFont);
  end;
end;

function TKMemoRTFFontTable.GetFont(AFontIndex: Integer): TFont;
var
  I: Integer;
  Item: TKMemoRTFFont;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if Item.FontIndex = AFontIndex then
    begin
      Result := Item.Font;
      Exit;
    end;
  end;
end;

function TKMemoRTFFontTable.GetIndex(AFont: TFont): Integer;
var
  I: Integer;
  Font: TFont;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    Font := Items[I].Font;
    if (Font.Name = AFont.Name) and (Font.Charset = AFont.Charset) and (Font.Pitch = AFont.Pitch) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TKMemoRTFFontTable.GetItem(Index: Integer): TKMemoRTFFont;
begin
  Result := TKMemoRTFFont(inherited GetItem(Index));
end;

procedure TKMemoRTFFontTable.SetItem(Index: Integer;
  const Value: TKMemoRTFFont);
begin
  inherited SetItem(Index, Value);
end;

{ TKMemoRTFShape }

constructor TKMemoRTFShape.Create;
begin
  FContentPosition := TKRect.Create;
  FContentType := sctUnknown;
  FHorzPosCode := 0;
  FStyle := TKMemoBlockStyle.Create;
  FStyle.Brush.Color := clWhite;
  FStyle.ContentPadding.AssignFromValues(2, 2, 2, 2);
  FVertPosCode := 0;
  FWrap := 0;
  FWrapSide := 0;
end;

destructor TKMemoRTFShape.Destroy;
begin
  FContentPosition.Free;
  FStyle.Free;
  inherited;
end;

function TKMemoRTFShape.GetWrap: Integer;
begin
  WrapModeToRTFWrap;
  Result := FWrap;
end;

function TKMemoRTFShape.GetWrapSide: Integer;
begin
  WrapModeToRTFWrap;
  Result := FWrap;
end;

procedure TKMemoRTFShape.SetWrap(const Value: Integer);
begin
  FWrap := Value;
  RTFWrapToWrapMode;
end;

procedure TKMemoRTFShape.SetWrapSide(const Value: Integer);
begin
  FWrapSide := Value;
  RTFWrapToWrapMode;
end;

procedure TKMemoRTFShape.WrapModeToRTFWrap;
begin
  FWrapSide := 0;
  FWrap := 0;
  case FStyle.WrapMode of
    wrAround: FWrap := 2;
    wrAroundLeft: begin FWrap := 2; FWrapSide := 1; end;
    wrAroundRight: begin FWrap := 2; FWrapSide := 2; end;
    wrTight: FWrap := 4;
    wrTightLeft: begin FWrap := 4; FWrapSide := 1; end;
    wrTightRight: begin FWrap := 4; FWrapSide := 2; end;
    wrTopBottom: FWrap := 1;
    wrNone: FWrap := 3;
  end;
end;

procedure TKMemoRTFShape.RTFWrapToWrapMode;
begin
  case FWrap of
    1: FStyle.WrapMode := wrTopBottom;
    2: case FWrapSide of
         1: FStyle.WrapMode := wrAroundLeft;
         2: FStyle.WrapMode := wrAroundRight;
       else
         FStyle.WrapMode := wrAround;
       end;
    3: FStyle.WrapMode := wrNone;
    4: case FWrapSide of
         1: FStyle.WrapMode := wrTightLeft;
         2: FStyle.WrapMode := wrTightRight;
       else
         FStyle.WrapMode := wrTight;
       end;
  else
    FStyle.WrapMode := wrAround;
  end;
end;

{ TKMemoRTFReader }

constructor TKMemoRTFReader.Create(AMemo: TKCustomMemo);
begin
  FBlocks := AMemo.Blocks;
  FColorTable := TKMemoRTFColorTable.Create;
  FFontTable := TKMemoRTFFontTable.Create;
  FMemo := AMemo;
  FStream := TMemoryStream.Create;
  FTextStyle := TKMemoTextStyle.Create;
end;

destructor TKMemoRTFReader.Destroy;
begin
  FColorTable.Free;
  FFontTable.Free;
  FStream.Free;
  FTextStyle.Free;
  inherited;
end;

function TKMemoRTFReader.ParamToBool(const AValue: AnsiString): Boolean;
begin
  Result := Boolean(StrToIntDef(string(AValue), 0));
end;

function TKMemoRTFReader.ParamToColor(const AValue: AnsiString): TColor;
begin
  Result := ColorRecToColor(MakeColorRec(StrToIntDef(string(AValue), 0)));
end;

function TKMemoRTFReader.ParamToEMU(const AValue: AnsiString): Integer;
begin
  Result := EMUToPoints(StrToIntDef(string(AValue), 0));
end;

function TKMemoRTFReader.ParamToInt(const AValue: AnsiString): Integer;
begin
  Result := StrToIntDef(string(AValue), 0);
end;

function TKMemoRTFReader.EMUToPoints(AValue: Integer): Integer;
begin
  Result := DivUp(AValue, 12700);
end;

procedure TKMemoRTFReader.AddText(const APart: TKString; ATextStyle: TKMemoTextStyle);
var
  S: TKString;
begin
  S := APart;
  while (FIgnoreChars > 0) and (S <> '') do
  begin
    Delete(S, 1, 1);
    Dec(FIgnoreChars);
  end;
  if S <> '' then
  begin
    FTextStyle.Assign(ATextStyle);
    if FTextStyle.StyleChanged then
    begin
      FlushText;
      FTextStyle.StyleChanged := False;
    end;
    if FActiveText = nil then
    begin
      FActiveText := TKMemoTextBlock.Create(nil);
      FActiveText.TextStyle.Assign(FTextStyle);
    end;
    FActiveString := FActiveString + S;
  end;
end;

procedure TKMemoRTFReader.ApplyFont(ATextStyle: TKMemoTextStyle; AFontIndex: Integer; var CodePage: Integer);
var
  Font: TFont;
begin
  Font := FFontTable.GetFont(AFontIndex);
  if Font <> nil then
  begin
    ATextStyle.Font.Name := Font.Name;
    ATextStyle.Font.Charset := Font.Charset;
    ATextStyle.Font.Pitch := Font.Pitch;
    CodePage := CharSetToCP(Font.Charset);
  end else
  asm
    nop
  end;
end;

procedure TKMemoRTFReader.ApplyHighlight(ATextStyle: TKMemoTextStyle; AHighlightCode: Integer);
var
  Color: TColor;
begin
  Color := HighlightCodeToColor(AHighlightCode);
  if Color <> clNone then
    ATextStyle.Brush.Color := Color
  else
    ATextStyle.Brush.Style := bsClear;
end;

procedure TKMemoRTFReader.FlushColor;
begin
  if FActiveColor <> nil then
  begin
    FColorTable.Add(FActiveColor);
    FActiveColor := nil;
  end;
end;

procedure TKMemoRTFReader.FlushContainer;
begin
  if FActiveContainer <> nil then
  begin
    FBlocks := FActiveContainer.Parent;
    FBlocks.AddAt(FActiveContainer);
    FActiveContainer := nil;
  end;
end;

procedure TKMemoRTFReader.FlushFont;
begin
  if FActiveFont <> nil then
  begin
    FFontTable.Add(FActiveFont);
    FActiveFont := nil;
  end;
end;

procedure TKMemoRTFReader.FlushImage;
begin
  if FActiveImage <> nil then
  begin
    FBlocks.AddAt(FActiveImage);
    FActiveImage := nil;
  end;
end;

procedure TKMemoRTFReader.FlushParagraph(ATextStyle: TKMemoTextStyle; AParaStyle: TKMemoParaStyle);
var
  PA: TKMemoParagraph;
begin
  PA := FBlocks.AddParagraph;
  PA.TextStyle.Assign(ATextStyle);
  PA.ParaStyle.Assign(AParaStyle);
  FParaBorder := alNone;
end;

procedure TKMemoRTFReader.FlushShape;
begin
  if FActiveShape <> nil then
  begin
    case FActiveShape.ContentType of
      sctImage:
      begin
        // image was inside shape
        if FActiveImage <> nil then
        begin
          ActiveImage.Position := mbpRelative;
          if FActiveShape.HorzPosCode = 2 then
            ActiveImage.LeftOffset := FActiveShape.ContentPosition.Left;
          if FActiveShape.VertPosCode = 2 then
            ActiveImage.TopOffset := FActiveShape.ContentPosition.Top;
          ActiveImage.ImageStyle.Assign(FActiveShape.Style);
          FlushImage;
        end;
      end;
      sctRectangle:
      begin
        // currently only document background supported
        if FBackgroundImage then
        begin
          if FActiveImage <> nil then
          begin
            FMemo.BackgroundImage.Assign(ActiveImage.Image);
            FreeAndNil(FActiveImage);
          end else
            FMemo.Colors.BkGnd := FActiveShape.Style.Brush.Color;
          FBackgroundImage := False;
        end;
      end;
      sctContainer:
      begin
        if FActiveContainer <> nil then
        begin
          // container was inside shape
          ActiveContainer.Position := mbpRelative;
          ActiveContainer.FixedWidth := True;
          if FActiveShape.HorzPosCode = 2 then
            ActiveContainer.LeftOffset := FActiveShape.ContentPosition.Left;
          if FActiveShape.VertPosCode = 2 then
            ActiveContainer.TopOffset := FActiveShape.ContentPosition.Top;
          ActiveContainer.RequiredWidth := FActiveShape.ContentPosition.Right - FActiveShape.ContentPosition.Left;
          ActiveContainer.RequiredHeight := FActiveShape.ContentPosition.Bottom - FActiveShape.ContentPosition.Top;
          ActiveContainer.BlockStyle.Assign(FActiveShape.Style);
          FlushContainer;
        end;
      end;
      sctText:
      begin
        // unformatted text was inside shape
        FlushText;
      end;
    end;
    FreeAndNil(FActiveShape);
  end;
end;

procedure TKMemoRTFReader.FlushTable;
begin
  if FActiveTable <> nil then
  begin
    //FTable.Position := mbpRelative;
    FBlocks := FActiveTable.Parent;
    FBlocks.AddAt(FActiveTable);
    FActiveTable := nil;
  end;
end;

procedure TKMemoRTFReader.FlushText;
begin
  if FActiveText <> nil then
  begin
    if FActiveString <> '' then
    begin
      FActiveText.InsertString(FActiveString);
      FActiveString := '';
    end;
    if FActiveText.TextStyle.Font.Name = 'Symbol' then
    begin
      FActiveText.TextStyle.Font.Name := 'Arial';
      FActiveText.TextStyle.Font.Charset := 0;
    end;
    if FBlocks <> nil then
      FBlocks.AddAt(FActiveText);
    FActiveText := nil;
  end;
end;

function TKMemoRTFReader.GetActiveColor: TKMemoRTFColor;
begin
  if FActiveColor = nil then
    FActiveColor := TKMemoRTFColor.Create;
  Result := FActiveColor;
end;

function TKMemoRTFReader.GetActiveContainer: TKMemoContainer;
begin
  if FActiveContainer = nil then
    FActiveContainer := TKMemoContainer.Create(FBlocks);
  Result := FActiveContainer;
end;

function TKMemoRTFReader.GetActiveFont: TKMemoRTFFont;
begin
  if FActiveFont = nil then
    FActiveFont := TKMemoRTFFont.Create;
  Result := FActiveFont;
end;

function TKMemoRTFReader.GetActiveImage: TKMemoImageBlock;
begin
  if FActiveImage = nil then
  begin
    FActiveImage := TKMemoImageBlock.Create(nil);
    FActiveImage.ImageStyle.ContentPadding.AssignFromValues(0, 0, 0, 0);
  end;
  Result := FActiveImage;
end;

function TKMemoRTFReader.GetActiveShape: TKMemoRTFShape;
begin
  if FActiveShape = nil then
    FActiveShape := TKMemoRTFShape.Create;
  Result := FActiveShape;
end;

function TKMemoRTFReader.GetActiveTable: TKMemoTable;
begin
  if FActiveTable = nil then
    FActiveTable := TKMemoTable.Create(FBlocks);
  Result := FActiveTable;
end;

function TKMemoRTFReader.HighlightCodeToColor(AValue: Integer): TColor;
begin
  case AValue of
    1: Result := clBlack;
    2: Result := clBlue;
    3: Result := clAqua; // cyan
    4: Result := clLime; // green
    5: Result := clFuchsia; // magenta
    6: Result := clRed;
    7: Result := clYellow;
    9: Result := clNavy;
    10: Result := clTeal; // dark cyan
    11: Result := clGreen; // dark green
    12: Result := clPurple; // dark magenta
    13: Result := clMaroon; // dark red
    14: Result := clOlive; // dark yellow
    15: Result := clGray; // dark gray
    16: Result := clSilver; // light gray
  else
    Result := clNone;
  end;
end;

procedure TKMemoRTFReader.Load(const AFileName: TKString);
begin
  if FileExists(AFileName) then
  try
    FStream.LoadFromFile(AFileName);
    FMemo.TextStyle.Defaults;
    FMemo.ParaStyle.Defaults;
    FMemo.Colors.BkGnd := cBkGndDef;
    FMemo.BackgroundImage.Graphic := nil;
    FActiveColor := nil;
    FActiveContainer := nil;
    FActiveFont := nil;
    FActiveImage := nil;
    FActiveShape := nil;
    FActiveTable := nil;
    FActiveString := '';
    FActiveText := nil;
    FBackgroundImage := False;
    FColorTable.Clear;
    FDefFontIndex := 0;
    FHeaderRead := False;
    FImageClass := nil;
    FIgnoreChars := 0;
    FParaBorder := alNone;
    FTableBorder := alNone;
    FTableCell := nil;
    FTableCol := -1;
    FTableColCount := 0;
    FTableRow := nil;
    FBlocks.LockUpdate;
    try
      FBlocks.Clear;
      ReadGroup(0, rgNone, FMemo.TextStyle, FMemo.ParaStyle, 0, 0);
    finally
      FlushColor;
      FlushFont;
      FlushText;
      FlushShape;
      FlushImage;
      FBlocks.UnlockUpdate;
    end;
  except
    KFunctions.Error(sErrMemoLoadFromRTF);
  end;
end;

function TKMemoRTFReader.ReadNext(out ACtrl, AText: AnsiString; out AParam: Int64): Boolean;

  procedure ReadText(var AText: AnsiString; AChar: AnsiChar);
  begin
    repeat
      if (AChar <> cCR) and (AChar <> cLF) then
        AText := AText + AChar;
      Result := FStream.Read(AChar, 1) > 0;
    until CharInSetEx(AChar, ['{', '}', '\']) or not Result;
    FStream.Seek(-1, soFromCurrent);
  end;

var
  C: AnsiChar;
  ParamStr: AnsiString;
  Code: Integer;
begin
  AParam := MaxInt;
  ACtrl := '';
  AText := '';
  Result := FStream.Read(C, 1) > 0;
  if C = '\' then
  begin
    FStream.Read(C, 1);
    if CharInSetEx(C, cLetters) then
    begin
      // control word
      repeat
        ACtrl := ACtrl + C;
        Result := FStream.Read(C, 1) > 0;
      until not (Result and CharInSetEx(C, cLetters));
      if (C = '-') or CharInSetEx(C, cNumbers) then
      begin
        // control word parameter
        ParamStr := '';
        repeat
          ParamStr := ParamStr + C;
          Result := FStream.Read(C, 1) > 0;
        until not (Result and CharInSetEx(C, cNumbers));
        AParam := StrToIntDef(TKString(ParamStr), 0);
        if C <> ' ' then
          FStream.Seek(-1, soFromCurrent);
      end
      else if C <> ' ' then
        FStream.Seek(-1, soFromCurrent);
    end else
    begin
      ACtrl := C; //control symbol
      if C = '''' then
      begin
        //hexadecimal value - special symbol
        SetLength(ParamStr, 2);
        Result := FStream.Read(ParamStr[1], 2) = 2;
        if Result then
          AParam := HexStrToInt(string(ParamStr), Length(ParamStr), False, Code);
      end
      else if CharInSetEx(C, ['{', '}', '\'])  then
      begin
        AText := C; // control symbol is printable character
        ACtrl := '';
      end;
    end;
    if FStream.Read(C, 1) > 0 then
    begin
      if CharInSetEx(C, ['{', '}', '\']) then
        FStream.Seek(-1, soFromCurrent)
      else
        ReadText(AText, C);
    end;
  end
  else if CharInSetEx(C, ['{', '}', ';']) then
    ACtrl := C // group
  else
    ReadText(AText, C);
end;

procedure TKMemoRTFReader.ReadColorGroup(const ACtrl, AText: AnsiString; AParam: Integer);
begin
  if ACtrl = 'red' then
    ActiveColor.Red := Byte(AParam)
  else if ACtrl = 'green' then
    ActiveColor.Green := Byte(AParam)
  else if ACtrl = 'blue' then
    ActiveColor.Blue := Byte(AParam);
  if AText = ';' then
    FlushColor;
end;

procedure TKMemoRTFReader.ReadFontGroup(const ACtrl, AText: AnsiString; AParam: Integer);
var
  I: Integer;
  S: TKString;
begin
  if ACtrl = 'f' then
    ActiveFont.FFontIndex := AParam
  else if ACtrl = 'fcharset' then
    ActiveFont.Font.Charset := AParam
  else if ACtrl = 'fprq' then
  begin
    case AParam of
      1: ActiveFont.Font.Pitch := fpFixed;
      2: ActiveFont.Font.Pitch := fpVariable;
    else
      ActiveFont.Font.Pitch := fpDefault;
    end;
  end;
  if AText <> '' then
  begin
    S := TKString(AText);
    I := Pos(';', S);
    if I > 0 then
      Delete(S, I, 1);
    ActiveFont.Font.Name := S;
    FlushFont;
  end
end;

procedure TKMemoRTFReader.ReadGroup(GroupIndex: Integer; Group: TKMemoRTFGroup; ATextStyle: TKMemoTextStyle; AParaStyle: TKMemoParaStyle; CodePage, IgnoreUnicodeChars: Integer);
var
  Ctrl: AnsiString;
  Text: AnsiString;
  Param: Int64;
  LocalTextStyle: TKMemoTextStyle;
  LocalParaStyle: TKMemoParaStyle;
  IsPicture, IsShapeInst: Boolean;
begin
  IsPicture := False;
  IsShapeInst := False;
  LocalParaStyle := TKMemoParaStyle.Create;
  LocalTextStyle := TKMemoTextStyle.Create;
  try
    LocalParaStyle.Assign(AParaStyle);
    LocalTextStyle.Assign(ATextStyle);
    Ctrl := '';
    while (FStream.Position < FStream.Size) and (Ctrl <> '}') do
    begin
      ReadNext(Ctrl, Text, Param);
      if (Ctrl <> '') or (Text <> '') then
      begin
        if Ctrl = '{' then
        begin
          ReadGroup(GroupIndex + 1, Group, LocalTextStyle, LocalParaStyle, CodePage, IgnoreUnicodeChars)
        end
        else if Ctrl = '*' then
        begin
          if Group <> rgShape then
            Group := rgUnknown;
        end
        else if Ctrl <> '}' then
        begin
          case Group of
            rgColorTable: ReadColorGroup(Ctrl, Text, Param);
            rgFontTable: ReadFontGroup(Ctrl, Text, Param);
            rgFooter:;
            rgHeader:;
            rgInfo:;
            rgPicture: ReadPictureGroup(Ctrl, Text, Param);
            rgShape:
            begin
              if Ctrl = 'shpinst' then
              begin
                IsShapeInst := True;
                Group := rgShapeInst;
              end;
            end;
            rgShapeInst: if not ReadShapeGroup(Ctrl, Text, Param) then
            begin
              if Ctrl = 'pict' then
              begin
                Group := rgPicture // picture in this shape
              end
              else if Ctrl = 'shptxt' then
              begin
                FlushText;
                ActiveShape.ContentType := sctContainer;
                FBlocks := ActiveContainer.Blocks;
                Group := rgText // text in this shape
              end;
            end;
            rgShapePict:
            begin
              if Ctrl = 'pict' then
              begin
                Group := rgPicture;
                IsPicture := True;
              end;
            end;
            rgStyleSheet:;
            rgUnknown:
            begin
              if Ctrl = 'shppict' then
              begin
                FlushText;
                FlushShape;
                Group := rgShapePict; // picture inside text
              end
              else if Ctrl = 'background' then
              begin
                FBackgroundImage := True;
                Group := rgNone;
              end;
            end;
          else
            if not FHeaderRead then
            begin
              if Ctrl = 'rtf' then
                FHeaderRead := True;
            end else
            begin
              if Ctrl <> '' then
              begin
                if Ctrl = 'par' then
                begin
                  FlushText;
                  FlushParagraph(LocalTextStyle, LocalParaStyle);
                end
                // supported text formatting
                else if ReadTextFormatting(Ctrl, Param, LocalTextStyle, CodePage) then
                begin
                end
                // supported paragraph formatting
                else if ReadParaFormatting(Ctrl, Param, LocalParaStyle) then
                begin
                end
                // supported special characters
                else if ReadSpecialCharacter(Ctrl, Param, LocalTextStyle, CodePage, IgnoreUnicodeChars) then
                begin
                end
                // supported table commands
                else if ReadTableFormatting(Ctrl, Param, LocalTextStyle, LocalParaStyle) then
                begin
                end
                // shape control words
                else if Ctrl = 'shp' then
                begin
                  Group := rgShape;
                end
                else if Ctrl = 'nonshppict' then
                begin
                  Group := rgUnknown;
                end
                else if Ctrl = 'pict' then
                begin
                  FlushText;
                  FlushShape;
                  Group := rgPicture;
                  IsPicture := True; // picture inside text
                end
                else if Ctrl = 'uc' then
                begin
                  IgnoreUnicodeChars := Param;
                end
                // file header control words (unfortunately header has not its own group)
                else if ReadHeaderGroup(Ctrl, Text, Param, Group, CodePage) then
                begin
                  Text := '';
                end
                // document headers
                else if (Ctrl = 'header') or (Ctrl = 'headerl') or (Ctrl = 'headerr') then
                begin
                  Group := rgHeader;
                end
                // document footers
                else if (Ctrl = 'footer') or (Ctrl = 'footerl') or (Ctrl = 'footerr') then
                begin
                  Group := rgHeader;
                end
                // document info
                else if Ctrl = 'info' then
                begin
                  Group := rgInfo;
                end
              end;
              if Text <> '' then
              begin
                AddText(TKString(Text), LocalTextStyle);
              end
            end;
          end;
        end;
      end;
    end;
    if IsPicture then
      FlushImage;
    if IsShapeInst then
      FlushShape;
  finally
    LocalParaStyle.Free;
    LocalTextStyle.Free;
  end;
end;

function TKMemoRTFReader.ReadHeaderGroup(const ACtrl, AText: AnsiString; AParam: Integer; var AGroup: TKMemoRTFGroup; var ACodePage: Integer): Boolean;
begin
  Result := True;
  if ACtrl = 'ansicpg' then
  begin
    ACodePage := AParam;
  end
  else if ACtrl = 'deff' then
  begin
    FDefFontIndex := AParam;
  end
  else if ACtrl = 'fonttbl' then
  begin
    AGroup := rgFontTable;
  end
  else if ACtrl = 'colortbl' then
  begin
    AGroup := rgColorTable;
  end
  else if ACtrl = 'stylesheet' then
  begin
    AGroup := rgStyleSheet;
  end else
    Result := False;
end;

function TKMemoRTFReader.ReadParaFormatting(const ACtrl: AnsiString;
  AParam: Integer; AParaStyle: TKMemoParaStyle): Boolean;
begin
  Result := True;
  if ACtrl = 'pard' then
  begin
    AParaStyle.Assign(FMemo.ParaStyle);
  end
  else if ACtrl = 'fi' then
  begin
    AParaStyle.FirstIndent := TwipsToPoints(AParam);
  end
  else if ACtrl = 'li' then
  begin
    AParaStyle.LeftPadding := TwipsToPoints(AParam);
  end
  else if ACtrl = 'ri' then
  begin
    AParaStyle.RightPadding := TwipsToPoints(AParam);
  end
  else if ACtrl = 'sb' then
  begin
    AParaStyle.TopPadding := TwipsToPoints(AParam);
  end
  else if ACtrl = 'sa' then
  begin
    AParaStyle.BottomPadding := TwipsToPoints(AParam);
  end
  else if ACtrl = 'ql' then
  begin
    AParaStyle.HAlign := halLeft;
  end
  else if ACtrl = 'qr' then
  begin
    AParaStyle.HAlign := halRight;
  end
  else if ACtrl = 'qc' then
  begin
    AParaStyle.HAlign := halCenter;
  end
  else if ACtrl = 'qj' then
  begin
    AParaStyle.HAlign := halJustify;
  end
  else if ACtrl = 'cbpat' then
  begin
    AParaStyle.Brush.Color := FColorTable.GetColor(AParam - 1);
  end
  else if ACtrl = 'nowwrap' then
  begin
    AParaStyle.WordWrap := False;
  end
  else if ACtrl = 'brdrb' then
    FParaBorder := alBottom
  else if ACtrl = 'brdrl' then
    FParaBorder := alLeft
  else if ACtrl = 'brdrr' then
    FParaBorder := alRight
  else if ACtrl = 'brdrt' then
    FParaBorder := alTop
  else if ACtrl = 'box' then
    FParaBorder := alClient
  else if ACtrl = 'brdrw' then
  begin
    case FParaBorder of
      alBottom: AParaStyle.BorderWidths.Bottom := TwipsToPoints(AParam);
      alLeft: AParaStyle.BorderWidths.Left := TwipsToPoints(AParam);
      alRight: AParaStyle.BorderWidths.Right := TwipsToPoints(AParam);
      alTop: AParaStyle.BorderWidths.Top := TwipsToPoints(AParam);
      alClient: AParaStyle.BorderWidth := TwipsToPoints(AParam);
    else
      Result := False;
    end
  end
  else if ACtrl = 'brdrradius' then
  begin
    if FParaBorder <> alNone then
      AParaStyle.BorderRadius := TwipsToPoints(AParam)
    else
      Result := False;
  end
  else if ACtrl = 'brdrcf' then
  begin
    if FParaBorder <> alNone then
      AParaStyle.BorderColor := FColorTable.GetColor(AParam - 1)
    else
      Result := False;
  end
  else
    Result := False;
end;

procedure TKMemoRTFReader.ReadPictureGroup(const ACtrl, AText: AnsiString; AParam: Integer);
var
  S: AnsiString;
  MS: TMemoryStream;
  Image: TGraphic;
begin
  if ACtrl = 'jpegblip' then
    FImageClass := TJpegImage
{$IFDEF USE_PNG_SUPPORT}
  else if ACtrl = 'pngblip' then
    FImageClass := TKPngImage
{$ENDIF}    
{$IFDEF USE_WINAPI}
  else if ACtrl = 'emfblip' then
  begin
    FImageClass := TKMetafile;
    FImageEnhMetafile := True;
  end
  else if ACtrl = 'wmetafile' then
  begin
    FImageClass := TKMetafile;
    FImageEnhMetafile := False;
  end
{$ELSE}
  // how do we load metafiles under other OSes?
{$ENDIF}
  else if ACtrl = 'picw' then
    FImageOriginalWidth := AParam
  else if ACtrl = 'pich' then
    FImageOriginalHeight := AParam
  else if ACtrl = 'piccropb' then
    ActiveImage.Crop.Bottom := TwipsToPoints(AParam)
  else if ACtrl = 'piccropl' then
    ActiveImage.Crop.Left := TwipsToPoints(AParam)
  else if ACtrl = 'piccropr' then
    ActiveImage.Crop.Right := TwipsToPoints(AParam)
  else if ACtrl = 'piccropt' then
    ActiveImage.Crop.Top := TwipsToPoints(AParam)
  else if ACtrl = 'picwgoal' then
    ActiveImage.ScaleWidth := TwipsToPoints(AParam)
  else if ACtrl = 'pichgoal' then
    ActiveImage.ScaleHeight := TwipsToPoints(AParam)
  else if (ACtrl = '') and (AText <> '') then
  begin
    if FImageClass <> nil then
    begin
      S := AText;
      if DigitsToBinStr(S) then
      begin
        S := BinStrToBinary(S);
        try
          MS := TMemoryStream.Create;
          try
            MS.Write(S[1], Length(S));
            MS.Seek(0, soFromBeginning);
            Image := FImageClass.Create;
            try
            {$IFDEF USE_WINAPI}
              if Image is TKMetafile then
              begin
                //if not FImageEnhMetafile then MS.SaveToFile('test.wmf');
                TKMetafile(Image).CopyOnAssign := False; // we will destroy this instance anyway...
                TKMetafile(Image).Enhanced := FImageEnhMetafile;
                TKmetafile(Image).LoadFromStream(MS);
                if not FImageEnhMetafile then
                begin
                  // WMF extent could be incorrect here, so use RTF info
                  TKMetafile(Image).Width := FImageOriginalWidth;
                  TKMetafile(Image).Height := FImageOriginalHeight;
                end;
              end else
            {$ENDIF}
                Image.LoadFromStream(MS);
              ActiveImage.Image.Graphic := Image;
            finally
              Image.Free;
            end;
          finally
            MS.Free;
          end;
        except
          KFunctions.Error(sErrMemoLoadImageFromRTF);
        end;
      end;
      FImageClass := nil;
    end else
    asm
      nop;
    end;
  end;
end;

function TKMemoRTFReader.ReadShapeGroup(const ACtrl, AText: AnsiString; AParam: Integer): Boolean;
begin
  Result := True;
  if ACtrl = 'shpleft' then
    ActiveShape.ContentPosition.Left := TwipsToPoints(AParam)
  else if ACtrl = 'shpright' then
    ActiveShape.ContentPosition.Right := TwipsToPoints(AParam)
  else if ACtrl = 'shptop' then
    ActiveShape.ContentPosition.Top := TwipsToPoints(AParam)
  else if ACtrl = 'shpbottom' then
    ActiveShape.ContentPosition.Bottom := TwipsToPoints(AParam)
  else if ACtrl = 'shpbxcolumn' then
    ActiveShape.HorzPosCode := 2 // we silently assume posrelh always comes later
  else if ACtrl = 'shpbypara' then
    ActiveShape.VertPosCode := 2 // we silently assume posrelv always comes later
  else if ACtrl = 'shpwr' then
    ActiveShape.Wrap := AParam
  else if ACtrl = 'shpwrk' then
    ActiveShape.WrapSide := AParam
  else if ACtrl = 'sn' then
    ActiveShape.CtrlName := AText
  else if ACtrl = 'sv' then
  begin
    ActiveShape.CtrlValue := AText;
    if ActiveShape.CtrlName = 'posrelh' then
    begin
      ActiveShape.HorzPosCode := ParamToInt(AText);
    end
    else if ActiveShape.CtrlName = 'posrelv' then
    begin
      ActiveShape.VertPosCode := ParamToInt(AText);
    end
    else if ActiveShape.CtrlName = 'fFilled' then
    begin
      if not ParamToBool(AText) then
        ActiveShape.Style.Brush.Style := bsClear;
    end
    else if ActiveShape.CtrlName = 'fillColor' then
      ActiveShape.Style.Brush.Color := ParamToColor(AText)
    else if ActiveShape.CtrlName = 'fLine' then
    begin
      if not ParamToBool(AText) then
        ActiveShape.Style.BorderWidth := 0;
    end
    else if ActiveShape.CtrlName = 'lineColor' then
      ActiveShape.Style.BorderColor := ParamToColor(AText)
    else if ActiveShape.CtrlName = 'lineWidth' then
      ActiveShape.Style.BorderWidth := ParamToEMU(AText)
    else if ActiveShape.CtrlName = 'shapeType' then
    begin
      // supported shape types
      case StrToIntDef(string(ActiveShape.CtrlValue), 0) of
        1: ActiveShape.ContentType := sctRectangle;
        75: ActiveShape.ContentType := sctImage;
      end;
    end;
  end
  else
    Result := False;
end;

function TKMemoRTFReader.ReadSpecialCharacter(const ACtrl: AnsiString; AParam: Integer; ATextStyle: TKMemoTextStyle; ACodePage, AIgnoreUnicodeChars: Integer): Boolean;
var
  S: TKString;
begin
  Result := True;
  if ACtrl = 'tab' then
  begin
    // tab is rendered with an arrow symbol
    AddText(#9, ATextStyle);
  end
  else if ACtrl = 'lquote' then
  begin
    // we must suppose selected font supports this
    AddText(UnicodeToNativeUTF(#$2018), ATextStyle);
  end
  else if ACtrl = 'rquote' then
  begin
    // we must suppose selected font supports this
    AddText(UnicodeToNativeUTF(#$2019), ATextStyle);
  end
  else if ACtrl = 'ldblquote' then
  begin
    // we must suppose selected font supports this
    AddText(UnicodeToNativeUTF(#$201C), ATextStyle);
  end
  else if ACtrl = 'rdblquote' then
  begin
    // we must suppose selected font supports this
    AddText(UnicodeToNativeUTF(#$201D), ATextStyle);
  end
  else if ACtrl = 'endash' then
  begin
    AddText(UnicodeToNativeUTF(#$2013), ATextStyle);
  end
  else if ACtrl = 'emdash' then
  begin
    AddText(UnicodeToNativeUTF(#$2014), ATextStyle);
  end
  else if ACtrl = 'bullet' then
  begin
    AddText(UnicodeToNativeUTF(#$2022), ATextStyle);
  end
  else if (ACtrl = '~') or (ACtrl = 'emspace') or (ACtrl = 'enspace') then
  begin
    AddText(' ', ATextStyle); // nonbreaking spaces not supported
  end
  else if ACtrl = '''' then
  begin
    if ATextStyle.Font.Name = 'Symbol' then
      S := UnicodeToNativeUTF(WideChar(AdobeSymbolToUTF16(AParam)))
    else
      S := AnsiStringToString(AnsiChar(AParam), ACodePage);
    AddText(S, ATextStyle);
  end
  else if ACtrl = 'u' then
  begin
    if ATextStyle.Font.Name = 'Symbol' then
      AParam := AdobeSymbolToUTF16(AParam);
    S := UnicodeToNativeUTF(WideChar(AParam));
    AddText(S, ATextStyle);
    FIgnoreChars := AIgnoreUnicodeChars;
  end else
    Result := False;
end;

function TKMemoRTFReader.ReadTableFormatting(const ACtrl: AnsiString;
  AParam: Integer; ATextStyle: TKMemoTextStyle; AParaStyle: TKMemoParaStyle): Boolean;
var
  I, Value: Integer;
  Cell: TKMemoTableCell;
begin
  Result := True;
  if FTableColCount = 0 then
  begin
    if ACtrl = 'trowd' then // start of a row
    begin
      FlushText;
      if FActiveTable = nil then
      begin
        ActiveTable.RowCount := 1;
        ActiveTable.ColCount := 1;
        FTableColCount := 1;
        FTableRow := ActiveTable.Rows[ActiveTable.RowCount - 1];
        FBlocks := FTableRow.Cells[FTableColCount - 1].Blocks; // starting new cell
        FTableCellXPos := 0;
      end;
    end else
      Result := False;
  end
  else if FTableRow <> nil then
  begin
    if ACtrl = 'cell' then // end of a cell
    begin
      FlushText;
      Cell := FTableRow.Cells[FTableColCount - 1];
      Cell.ParaStyle.Assign(AParaStyle);
      Inc(FTableColCount);
      FTableRow.CellCount := FTableColCount;
      FBlocks := FTableRow.Cells[FTableColCount - 1].Blocks; // starting new cell
    end
    else if ACtrl = 'row' then // end of a row
    begin
      if FTableRow.Cells[FTableColCount - 1].Blocks.Count = 0 then
        FTableRow.CellCount := FTableRow.CellCount - 1;
      if FTableLastRow then
      begin
        FlushTable;
        FTableLastRow := False;
        FTableColCount := 0;
      end else
      begin
        ActiveTable.RowCount := ActiveTable.RowCount + 1;
        ActiveTable.ColCount := Max(ActiveTable.ColCount, FTableRow.CellCount);
        FTableRow := ActiveTable.Rows[ActiveTable.RowCount - 1];
        FTableColCount := 1;
        FTableRow.CellCount := FTableColCount;
        FBlocks := FTableRow.Cells[FTableColCount - 1].Blocks; // starting new cell
        FTableCellXPos := 0;
      end;
      FTableCol := -1;
      FTableCell := nil;
      FTableBorder := alNone;
    end
    else if ACtrl = 'lastrow' then
    begin
      FTableLastRow := True;
    end
    else if (ACtrl = 'trowd') and (FTableRow.CellCount > 1) then
    begin
      // this block comes again after definition of the row and is used to read row/cell properties
      // we don't support reading cell properties before entire row is defined
      FTableCol := 0;
      FTableCell := FTableRow.Cells[FTableCol];
    end
    else if (FTableCol >= 0) and (FTableCell <> nil) then
    begin
      // read row properties
      if ACtrl = 'trpaddb' then
      begin
        // we silently assume trgaph will come before trpaddx
        for I := 0 to FTableRow.CellCount - 1 do
          FTableRow.Cells[I].BlockStyle.BottomPadding := TwipsToPoints(AParam);
      end
      else if ACtrl = 'trpaddl' then
      begin
        // we silently assume trgaph will come before trpaddx
        for I := 0 to FTableRow.CellCount - 1 do
          FTableRow.Cells[I].BlockStyle.LeftPadding := TwipsToPoints(AParam);
      end
      else if ACtrl = 'trpaddr' then
      begin
        // we silently assume trgaph will come before trpaddx
        for I := 0 to FTableRow.CellCount - 1 do
          FTableRow.Cells[I].BlockStyle.RightPadding := TwipsToPoints(AParam);
      end
      else if ACtrl = 'trpaddt' then
      begin
        // we silently assume trgaph will come before trpaddx
        for I := 0 to FTableRow.CellCount - 1 do
          FTableRow.Cells[I].BlockStyle.TopPadding := TwipsToPoints(AParam);
      end
      else if ACtrl = 'trgaph' then
      begin
        Value := TwipsToPoints(AParam);
        for I := 0 to FTableRow.CellCount - 1 do
          FTableRow.Cells[I].BlockStyle.ContentPadding.AssignFromValues(Value, Value, Value, Value);
      end
      // read cell properties
      else if ACtrl = 'clbrdrb' then
        FTableBorder := alBottom
      else if ACtrl = 'clbrdrl' then
        FTableBorder := alLeft
      else if ACtrl = 'clbrdrr' then
        FTableBorder := alRight
      else if ACtrl = 'clbrdrt' then
        FTableBorder := alTop
      else if ACtrl = 'brdrw' then
      begin
        case FTableBorder of
          alBottom: FTableCell.RequiredBorderWidths.Bottom := TwipsToPoints(AParam);
          alLeft: FTableCell.RequiredBorderWidths.Left := TwipsToPoints(AParam);
          alRight: FTableCell.RequiredBorderWidths.Right := TwipsToPoints(AParam);
          alTop: FTableCell.RequiredBorderWidths.Top := TwipsToPoints(AParam);
        end;
      end
      else if ACtrl = 'brdrnone' then
      begin
        case FTableBorder of
          alBottom: FTableCell.RequiredBorderWidths.Bottom := 0;
          alLeft: FTableCell.RequiredBorderWidths.Left := 0;
          alRight: FTableCell.RequiredBorderWidths.Right := 0;
          alTop: FTableCell.RequiredBorderWidths.Top := 0;
        end;
      end
      else if ACtrl = 'brdrcf' then
      begin
        if FTableBorder <> alNone then
        begin
          // we currently support only one color for a cell, get the last...
          FTableCell.BlockStyle.BorderColor := FColorTable.GetColor(AParam - 1);
        end;
      end
      else if ACtrl = 'clcbpat' then
      begin
        FTableCell.BlockStyle.Brush.Color := FColorTable.GetColor(AParam - 1);
      end
      else if ACtrl = 'cellx' then
      begin
        // this command comes as the last for current cell
        Value := FTableCellXPos;
        FTableCellXPos := TwipsToPoints(AParam);
        FTableCell.RequiredWidth := FTableCellXPos - Value;
        Inc(FTableCol);
        if FTableCol < FTableRow.CellCount then
          FTableCell := FTableRow.Cells[FTableCol]
        else
          FTableCell := nil; // error in RTF, ignore next properties
      end;
    end else
      Result := False;
  end else
    Result := False;
end;

function TKMemoRTFReader.ReadTextFormatting(const ACtrl: AnsiString;
  AParam: Integer; ATextStyle: TKMemoTextStyle; var ACodePage: Integer): Boolean;
begin
  Result := True;
  if ACtrl = 'plain' then
  begin
    ATextStyle.Assign(FMemo.TextStyle);
  end
  else if ACtrl = 'f' then
  begin
    ApplyFont(ATextStyle, AParam, ACodePage);
  end
  else if ACtrl = 'b' then
  begin
    if AParam = 0 then
      ATextStyle.Font.Style := ATextStyle.Font.Style - [fsBold]
    else
      ATextStyle.Font.Style := ATextStyle.Font.Style + [fsBold];
  end
  else if ACtrl = 'i' then
  begin
    if AParam = 0 then
      ATextStyle.Font.Style := ATextStyle.Font.Style - [fsItalic]
    else
      ATextStyle.Font.Style := ATextStyle.Font.Style + [fsItalic];
  end
  else if ACtrl = 'ul' then
  begin
    if AParam = 0 then
      ATextStyle.Font.Style := ATextStyle.Font.Style - [fsUnderline]
    else
      ATextStyle.Font.Style := ATextStyle.Font.Style + [fsUnderline];
  end
  else if ACtrl = 'strike' then
  begin
    if AParam = 0 then
      ATextStyle.Font.Style := ATextStyle.Font.Style - [fsStrikeout]
    else
      ATextStyle.Font.Style := ATextStyle.Font.Style + [fsStrikeout];
  end
  else if ACtrl = 'caps' then
  begin
    ATextStyle.Capitals := tcaNormal;
  end
  else if ACtrl = 'scaps' then
  begin
    ATextStyle.Capitals := tcaSmall;
  end
  else if ACtrl = 'fs' then
  begin
    ATextStyle.Font.Size := DivUp(AParam, 2);
  end
  else if ACtrl = 'cf' then
  begin
    ATextStyle.Font.Color := FColorTable.GetColor(AParam - 1);
  end
  else if ACtrl = 'cb' then
  begin
    ATextStyle.Brush.Color := FColorTable.GetColor(AParam - 1);
  end
  else if ACtrl = 'highlight' then
  begin
    ATextStyle.Brush.Color := FColorTable.GetColor(AParam - 1);
//    ApplyHighlight(ATextStyle, AParam);
  end
  else
    Result := False;
end;

function TKMemoRTFReader.TwipsToPoints(AValue: Integer): Integer;
begin
  Result := DivUp(AValue, 20);
end;

{ TKMemoRTFWriter }

constructor TKMemoRTFWriter.Create(AMemo: TKCustomMemo);
begin
  FMemo := AMemo;
  FColorTable := TKMemoRTFColorTable.Create;
  FFontTable := TKMemoRTFFontTable.Create;
  FStream := TMemoryStream.Create;
end;

destructor TKMemoRTFWriter.Destroy;
begin
  FColorTable.Free;
  FFontTable.Free;
  FStream.Free;
  inherited;
end;

function TKMemoRTFWriter.ColorToHighlightCode(AValue: TColor): Integer;
begin
  case AValue of
    clBlack: Result := 1;
    clBlue: Result := 2;
    clAqua: Result := 3; // cyan
    clLime: Result := 4; // green
    clFuchsia: Result := 5; // magenta
    clRed: Result := 6;
    clYellow: Result := 7;
    clNavy: Result := 9;
    clTeal: Result := 10; // dark cyan
    clGreen: Result := 11; // dark green
    clPurple: Result := 12; // dark magenta
    clMaroon: Result := 13; // dark red
    clOlive: Result := 14; // dark yellow
    clGray: Result := 15; // dark gray
    clSilver: Result := 16; // light gray
  else
    Result := 0;
  end;
end;

procedure TKMemoRTFWriter.FillColorTable(ABlocks: TKMemoBlocks);
var
  I: Integer;
  Item: TKmemoBlock;
begin
  if ABlocks <> nil then
  begin
    for I := 0 to ABlocks.Count - 1 do
    begin
      Item := Ablocks[I];
      if Item is TKMemoTextBlock then
      begin
        FColorTable.AddColor(TKmemoTextBlock(Item).TextStyle.Brush.Color);
        FColorTable.AddColor(TKmemoTextBlock(Item).TextStyle.Font.Color);
        if Item is TKMemoParagraph then
        begin
          FColorTable.AddColor(TKMemoParagraph(Item).ParaStyle.Brush.Color);
          FColorTable.AddColor(TKMemoParagraph(Item).ParaStyle.BorderColor);
        end;
      end
      else if Item is TKMemoContainer then
      begin
        FColorTable.AddColor(TKmemoContainer(Item).BlockStyle.Brush.Color);
        FColorTable.AddColor(TKmemoContainer(Item).BlockStyle.BorderColor);
        if Item is TKMemoTable then
        begin
          FColorTable.AddColor(TKmemoTable(Item).CellStyle.Brush.Color);
          FColorTable.AddColor(TKmemoTable(Item).CellStyle.BorderColor);
        end;
        FillColorTable(TKmemoContainer(Item).Blocks);
      end;
    end;
  end;
end;

procedure TKMemoRTFWriter.FillFontTable(ABlocks: TKMemoBlocks);
var
  I: Integer;
  Item: TKmemoBlock;
begin
  if ABlocks <> nil then
  begin
    for I := 0 to ABlocks.Count - 1 do
    begin
      Item := Ablocks[I];
      if Item is TKMemoTextBlock then
        FFontTable.AddFont(TKmemoTextBlock(Item).TextStyle.Font)
      else if Item is TKmemoContainer then
        FillFontTable(TKmemoContainer(Item).Blocks);
    end;
  end;
end;

function TKMemoRTFWriter.PointsToTwips(AValue: Integer): Integer;
begin
  Result := AValue * 20;
end;

procedure TKMemoRTFWriter.Save(const AFileName: TKString);
var
  Group: Integer;
begin
  try
    FCodePage := SystemCodepage;
    Group := 0;
    WriteGroupBegin(Group);
    try
      WriteHeader(Group);
      WriteBody(FMemo.Blocks, Group, False);
    finally
      WriteGroupEnd(Group);
    end;
    FStream.SaveToFile(AFileName);
  except
    KFunctions.Error(sErrMemoSaveToRTF);
  end;
end;

procedure TKMemoRTFWriter.WriteBody(ABlocks: TKMemoBlocks; var AGroup: Integer; AInsideTable: Boolean);
var
  I: Integer;
  Item: TKMemoBlock;
begin
  if ABlocks <> nil then
  begin
    for I := 0 to ABlocks.Count - 1 do
    begin
      Item := ABlocks[I];
      if Item is TKMemoParagraph then
        WriteParagraph(TKMemoParagraph(Item), AGroup, AInsideTable)
      else if Item is TKMemoTextBlock then
        WriteTextBlock(TKMemoTextBlock(Item), AGroup)
      else if Item is TKMemoContainer then
      begin
        if Item is TKMemoTable then
          WriteTable(TKMemoTable(Item), AGroup)
        else
          WriteBody(TKMemoContainer(Item).Blocks, AGroup, AInsideTable);
      end;
    end;
  end;
end;

procedure TKMemoRTFWriter.WriteColorTable(var AGroup: Integer);
var
  I: Integer;
  ColorRec: TKColorRec;
begin
  WriteGroupBegin(AGroup);
  try
    WriteCtrl('colortbl');
    WriteSemicolon;
    for I := 0 to FColorTable.Count - 1 do
    begin
      ColorRec := FColorTable[I].ColorRec;
      WriteCtrlParam('red', ColorRec.R);
      WriteCtrlParam('green', ColorRec.G);
      WriteCtrlParam('blue', ColorRec.B);
      WriteSemiColon;
    end;
  finally
    WriteGroupEnd(AGroup);
  end;
end;

procedure TKMemoRTFWriter.WriteCtrl(const ACtrl: AnsiString);
begin
  WriteString('\' + ACtrl);
end;

procedure TKMemoRTFWriter.WriteCtrlParam(const ACtrl: AnsiString;
  AParam: Integer);
begin
  WriteString(AnsiString(Format('\%s%d', [ACtrl, AParam])));
end;

procedure TKMemoRTFWriter.WriteFontTable(var AGroup: Integer);
var
  I, Pitch, Charset: Integer;
begin
  WriteGroupBegin(AGroup);
  try
    WriteCtrl('fonttbl');
    for I := 0 to FFontTable.Count - 1 do
    begin
      WriteGroupBegin(AGroup);
      try
        WriteCtrlParam('f', I);
        Charset := FFontTable[I].Font.Charset;
       {if Charset = 0 then
          Charset := CPToCharset(FCodePage); // don't override charset, it is still important for certain fonts!}
        WriteCtrlParam('fcharset', Charset);
        case FFontTable[I].Font.Pitch of
          fpFixed: Pitch := 1;
          fpVariable: Pitch := 2;
        else
          Pitch := 0;
        end;
        WriteCtrlParam('fprq', Pitch);
        WriteSpace;
        WriteString(AnsiString(FFontTable[I].Font.Name));
        WriteSemiColon;
      finally
        WriteGroupEnd(AGroup);
      end;
    end;
  finally
    WriteGroupEnd(AGroup);
  end;
end;

procedure TKMemoRTFWriter.WriteGroupBegin(var AGroup: Integer);
begin
  WriteString('{');
  Inc(AGroup);
end;

procedure TKMemoRTFWriter.WriteGroupEnd(var AGroup: Integer);
begin
  WriteString('}');
  Dec(AGroup);
end;

procedure TKMemoRTFWriter.WriteHeader(AGroup: Integer);
begin
  FFontTable.AddFont(FMemo.TextStyle.Font);
  FillFontTable(FMemo.Blocks);
  FillColorTable(FMemo.Blocks);
  WriteCtrl('rtf1');
  WriteCtrl('ansi');
  WriteCtrlParam('ansicpg', FCodePage);
  if FFontTable.Count > 0 then
    WriteCtrlParam('deff', 0);
  WriteCtrlParam('uc', 1);
  WriteFontTable(AGroup);
  WriteColorTable(AGroup);
end;

procedure TKMemoRTFWriter.WriteParagraph(AItem: TKMemoParagraph;
  var AGroup: Integer; AInsideTable: Boolean);
begin
  WriteGroupBegin(AGroup);
  try
    WriteParaStyle(AItem.ParaStyle);
    WriteTextStyle(AItem.TextStyle);
    if AinsideTable then
      WriteCtrl('intbl');
    WriteCtrl('par');
  finally
    WriteGroupEnd(AGroup);
  end;
end;

procedure TKMemoRTFWriter.WriteParaStyle(AParaStyle: TKMemoParaStyle);
begin
  WriteCtrl('pard'); // always store complete paragraph properties
  if AParaStyle.FirstIndent <> 0 then
    WriteCtrlParam('fi', PointsToTwips(AParaStyle.FirstIndent));
  if AParaStyle.LeftPadding <> 0 then
    WriteCtrlParam('li', PointsToTwips(AParaStyle.LeftPadding));
  if AParaStyle.RightPadding <> 0 then
    WriteCtrlParam('ri', PointsToTwips(AParaStyle.RightPadding));
  if AParaStyle.TopPadding <> 0 then
    WriteCtrlParam('sb', PointsToTwips(AParaStyle.TopPadding));
  if AParaStyle.BottomPadding <> 0 then
    WriteCtrlParam('sa', PointsToTwips(AParaStyle.BottomPadding));
  case AParaStyle.HAlign of
    halLeft: WriteCtrl('ql');
    halCenter: WriteCtrl('qc');
    halRight: WriteCtrl('qr');
    halJustify: WriteCtrl('qj');
  end;
  if AParaStyle.Brush.Style <> bsClear then
    WriteCtrlParam('cbpat', FColorTable.GetIndex(AParaStyle.Brush.Color) + 1);
  if not AParaStyle.WordWrap then
    WriteCtrl('nowwrap');
  if AParaStyle.BorderWidths.NonZero then
  begin
    if AParaStyle.BorderWidths.Bottom > 0 then
    begin
      WriteCtrl('brdrb');
      WriteCtrlParam('brdrw', PointsToTwips(AParaStyle.BorderWidths.Bottom))
    end;
    if AParaStyle.BorderWidths.Left > 0 then
    begin
      WriteCtrl('brdrl');
      WriteCtrlParam('brdrw', PointsToTwips(AParaStyle.BorderWidths.Left))
    end;
    if AParaStyle.BorderWidths.Right > 0 then
    begin
      WriteCtrl('brdrr');
      WriteCtrlParam('brdrw', PointsToTwips(AParaStyle.BorderWidths.Right))
    end;
    if AParaStyle.BorderWidths.Top > 0 then
    begin
      WriteCtrl('brdrt');
      WriteCtrlParam('brdrw', PointsToTwips(AParaStyle.BorderWidths.Top))
    end;
  end else
  begin
    if AParaStyle.BorderWidth > 0 then
    begin
      WriteCtrl('box');
      WriteCtrlParam('brdrw', PointsToTwips(AParaStyle.BorderWidth))
    end;
    if AParaStyle.BorderRadius > 0 then
      WriteCtrlParam('brdrradius', PointsToTwips(AParaStyle.BorderRadius))
  end;
  if AParaStyle.BorderColor <> clNone then
    WriteCtrlParam('brdrcf', FColorTable.GetIndex(AParaStyle.BorderColor) + 1)
end;

procedure TKMemoRTFWriter.WriteSemiColon;
begin
  WriteString(';');
end;

procedure TKMemoRTFWriter.WriteSpace;
begin
  WriteString(' ');
end;

procedure TKMemoRTFWriter.WriteString(const AText: AnsiString);
begin
  FStream.Write(AText[1], Length(AText));
end;

procedure TKMemoRTFWriter.WriteTable(AItem: TKMemoTable; var AGroup: Integer);
var
  I, J: Integer;
  Row: TKMemoTableRow;
  Cell: TKMemoTableCell;
begin
  for I := 0 to AItem.RowCount - 1 do
  begin
    Row := AItem.Rows[I];
    WriteCtrl('trowd');
    WriteTableRowProperties(AItem, I);
    for J := 0 to Row.CellCount - 1 do
    begin
      Cell := Row.Cells[J];
      WriteParaStyle(Cell.ParaStyle);
      WriteGroupBegin(AGroup);
      try
        WriteBody(Cell.Blocks, AGroup, True);
      finally
        WriteGroupEnd(AGroup);
      end;
      WriteCtrl('cell');
    end;
    WriteGroupBegin(AGroup);
    try
      WriteCtrl('trowd');
      WriteTableRowProperties(AItem, I);
      if I = AItem.RowCount - 1 then
        WriteCtrl('lastrow');
      WriteCtrl('row');
    finally
      WriteGroupEnd(AGroup);
    end;
  end;
end;

procedure TKMemoRTFWriter.WriteTableRowProperties(ATable: TKmemoTable; ARowIndex: Integer);

  procedure WriteBorderWidth(AWidth: Integer);
  begin
    if AWidth <> 0 then
    begin
      WriteCtrl('brdrs');
      WriteCtrlParam('brdrw', PointsToTwips(AWidth))
    end else
      WriteCtrl('brdrnone');
  end;

var
  Row: TKmemoTableRow;
  Cell: TKMemoTableCell;
  I, XPos: Integer;
begin
  WriteCtrlParam('irow', ARowIndex);
  Row := ATable.Rows[ARowIndex];
  Xpos := 0;
  for I := 0 to Row.CellCount - 1 do
  begin
    Cell := Row.Cells[I];
    if I = 0 then
    begin
      WriteCtrlParam('trpaddb', PointsToTwips(Cell.BlockStyle.BottomPadding));
      WriteCtrlParam('trpaddl', PointsToTwips(Cell.BlockStyle.LeftPadding));
      WriteCtrlParam('trpaddr', PointsToTwips(Cell.BlockStyle.RightPadding));
      WriteCtrlParam('trpaddt', PointsToTwips(Cell.BlockStyle.TopPadding));
    end;
    WriteCtrl('clbrdrb');
    WriteBorderWidth(Cell.RequiredBorderWidths.Bottom);
    WriteCtrlParam('brdrcf', FColorTable.GetIndex(Cell.BlockStyle.BorderColor) + 1);
    WriteCtrl('clbrdrl');
    WriteBorderWidth(Cell.RequiredBorderWidths.Left);
    WriteCtrlParam('brdrcf', FColorTable.GetIndex(Cell.BlockStyle.BorderColor) + 1);
    WriteCtrl('clbrdrr');
    WriteBorderWidth(Cell.RequiredBorderWidths.Right);
    WriteCtrlParam('brdrcf', FColorTable.GetIndex(Cell.BlockStyle.BorderColor) + 1);
    WriteCtrl('clbrdrt');
    WriteBorderWidth(Cell.RequiredBorderWidths.Top);
    WriteCtrlParam('brdrcf', FColorTable.GetIndex(Cell.BlockStyle.BorderColor) + 1);
    if Cell.BlockStyle.Brush.Style <> bsClear then
      WriteCtrlParam('clcbpat', FColorTable.GetIndex(Cell.BlockStyle.Brush.Color) + 1);
    if Cell.Width = 0 then
      Inc(Xpos, Max(Cell.RequiredWidth, 5)) // was table extent not calculated, use required width
    else
      Inc(Xpos, Cell.Width);
    WriteCtrlParam('cellx', PointsToTwips(Xpos));
  end;
end;

procedure TKMemoRTFWriter.WriteTextBlock(AItem: TKMemoTextBlock;
  var AGroup: Integer);
begin
  WriteGroupBegin(AGroup);
  try
    WriteTextStyle(AItem.TextStyle);
    WriteSpace;
    WriteUnicodeString(AItem.Text);
  finally
    WriteGroupEnd(AGroup);
  end;
end;

procedure TKMemoRTFWriter.WriteTextStyle(ATextStyle: TKMemoTextStyle);
begin
  WriteCtrlParam('f', FFontTable.GetIndex(ATextStyle.Font));
  if fsBold in ATextStyle.Font.Style then
    WriteCtrl('b');
  if fsItalic in ATextStyle.Font.Style then
    WriteCtrl('i');
  if fsUnderline in ATextStyle.Font.Style then
    WriteCtrl('ul');
  if fsStrikeout in ATextStyle.Font.Style then
    WriteCtrl('strike');
  case ATextStyle.Capitals of
    tcaNormal: WriteCtrl('caps');
    tcaSmall: WriteCtrl('scaps');
  end;
  WriteCtrlParam('fs', ATextStyle.Font.Size * 2);
  if ATextStyle.Font.Color <> clNone then
    WriteCtrlParam('cf', FColorTable.GetIndex(ATextStyle.Font.Color) + 1);
  if ATextStyle.Brush.Style <> bsClear then
    WriteCtrlParam('highlight', FColorTable.GetIndex(ATextStyle.Brush.Color) + 1);
end;

procedure TKMemoRTFWriter.WriteUnicodeString(const AText: TKString);
var
  I: Integer;
  UnicodeValue: Cardinal;
  WasAnsi: Boolean;
  S, Ansi: AnsiString;
  C: TKChar;
{$IFDEF FPC}
  CharLen: Integer;
{$ENDIF}
begin
  S := '';
  for I := 1 to StringLength(AText) do
  begin
  {$IFDEF FPC}
    C := UTF8Copy(AText, I, 1);
    if Length(C) = 1 then
  {$ELSE}
    C := AText[I];
    if Ord(C) < $80 then
  {$ENDIF}
    begin
      if C = #9 then
        S := AnsiString(Format('%s\tab ', [S]))
      else if (C = '\') or (C = '{') or (C = '}') then
        S := AnsiString(Format('%s\%s', [S, TKString(C)]))
      else
        S := S + AnsiString(C)
    end else
    begin
      WasAnsi := False;
      if FCodePage <> 0 then
      begin
        // first try Ansi codepage conversion for better backward compatibility
        Ansi := StringToAnsiString(C, FCodePage);
        if (Ansi <> '') and (Ansi <> #0) then
        begin
          S := AnsiString(Format('%s\''%x', [S, Ord(Ansi[1])]));
          WasAnsi := True;
        end;
      end;
      if not WasAnsi then
      begin
        // next store as Unicode character
      {$IFDEF FPC}
        UnicodeValue := UTF8CharacterToUnicode(@C[1], CharLen);
      {$ELSE}
        UnicodeValue := Ord(C);
      {$ENDIF}
        S := AnsiString(Format('%s\u%d\''3F', [S, UnicodeValue]));
      end;
    end;
  end;
  WriteString(S);
end;

end.

