unit KMemoRTF;

interface

uses
  Classes, Contnrs, Graphics,
  KControls, KFunctions, KGraphics, KMemo;

type
  TKMemoRTFGroup = (rgNone, rgUnknown, rgColorTable, rgFontTable, rgHeader, rgInfo, rgPicture, rgShape, rgShapeInst, rgShapeResult, rgShapePict, rgStyleSheet, rgText);

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
    procedure AddColor(const AColor: TKColorRec);
    function GetColor(AIndex: Integer): TColor; virtual;
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
    function GetFont(AFontIndex: Integer): TFont;
    property Items[Index: Integer]: TKMemoRTFFont read GetItem write SetItem; default;
  end;

  TKMemoRTFShapeContentType = (sctUnknown, sctContainer, sctImage, sctText);

  TKMemoRTFShape = class(TObject)
  private
    FContentPosition: TKRect;
    FContentType: TKMemoRTFShapeContentType;
    FCtrlName: AnsiString;
    FCtrlValue: AnsiString;
    FStyle: TKMemoBlockStyle;
    FWrap: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property ContentPosition: TKRect read FContentPosition;
    property ContentType: TKMemoRTFShapeContentType read FContentType write FContentType;
    property CtrlName: AnsiString read FCtrlName write FCtrlName;
    property CtrlValue: AnsiString read FCtrlValue write FCtrlValue;
    property Style: TKMemoBlockStyle read FStyle;
    property Wrap: Integer read FWrap write FWrap;
  end;

  TKMemoRTFReader = class(TObject)
  private
    function GetFont: TKMemoRTFFont;
    function GetColor: TKMemoRTFColor;
    function GetImageBlock: TKMemoImageBlock;
    function GetShape: TKMemoRTFShape;
    function GetContainer: TKMemoContainer;
  protected
    FBlocks: TKMemoBlocks;
    FColorTable: TKMemoRTFColorTable;
    FColor: TKMemoRTFColor;
    FContainer: TKMemoContainer;
    FDefCodePage: Integer;
    FDefFontIndex: Integer;
    FFontTable: TKMemoRTFFontTable;
    FFont: TKMemoRTFFont;
    FHeaderRead: Boolean;
    FImageBlock: TKMemoImageBlock;
    FImageClass: TGraphicClass;
    FGraphicClass: TGraphicClass;
    FMemo: TKCustomMemo;
    FShape: TKMemoRTFShape;
    FStream: TMemoryStream;
    FTextBlock: TKMemoTextBlock;
    FTextStyle: TKMemoTextStyle;
    procedure AddText(const APart: TKString; ATextStyle: TKMemoTextStyle); virtual;
    procedure ApplyFont(ATextStyle: TKMemoTextStyle; AFontIndex: Integer); virtual;
    procedure ApplyHighlight(ATextStyle: TKMemoTextStyle; AHighlightCode: Integer); virtual;
    function DirectBool(const AValue: AnsiString): Boolean; virtual;
    function DirectColor(const AValue: AnsiString): TColor; virtual;
    function DirectEMU(const AValue: AnsiString): Integer; virtual;
    function EMUToPoints(AValue: Integer): Integer;
    procedure FlushColor; virtual;
    procedure FlushContainer; virtual;
    procedure FlushFont; virtual;
    procedure FlushImage; virtual;
    procedure FlushShape; virtual;
    procedure FlushText; virtual;
    function HighlightCodeToColor(AValue: Integer): TColor; virtual;
    function ReadNext(out ACtrl, AText: AnsiString; out AParam: Int64): Boolean; virtual;
    procedure ReadColorGroup(const ACtrl, AText: AnsiString; AParam: Integer); virtual;
    procedure ReadFontGroup(const ACtrl, AText: AnsiString; AParam: Integer); virtual;
    function ReadHeaderGroup(const ACtrl: AnsiString; AParam: Integer; var Group: TKMemoRTFGroup): Boolean; virtual;
    procedure ReadGroup(GroupIndex: Integer; Group: TKMemoRTFGroup; ATextStyle: TKMemoTextStyle; AParaStyle: TKMemoParaStyle); virtual;
    procedure ReadPictureGroup(const ACtrl, AText: AnsiString; AParam: Integer); virtual;
    function ReadShapeGroup(const ACtrl, AText: AnsiString; AParam: Integer): Boolean; virtual;
    function ReadParaFormatting(const ACtrl: AnsiString; AParam: Integer; AParaStyle: TKMemoParaStyle): Boolean; virtual;
    function ReadTableFormatting(const ACtrl: AnsiString; AParam: Integer): Boolean; virtual;
    function ReadTextFormatting(const ACtrl: AnsiString; AParam: Integer; ATextStyle: TKMemoTextStyle): Boolean; virtual;
    function TwipsToPoints(AValue: Integer): Integer;
    property Color: TKMemoRTFColor read GetColor;
    property Container: TKMemoContainer read GetContainer;
    property Font: TKMemoRTFFont read GetFont;
    property ImageBlock: TKMemoImageBlock read GetImageBlock;
    property Shape: TKMemoRTFShape read GetShape;
  public
    constructor Create(AMemo: TKCustomMemo); virtual;
    destructor Destroy; override;
    procedure Load(const AFileName: TKString); virtual;
  end;

implementation

uses
  SysUtils, KHexEditor, JPeg;

{ TKMemoRTFColor }

constructor TKMemoRTFColor.Create;
begin
  FColorRec.Value := 0;
end;

{ TKMemoRTFColorTable }

procedure TKMemoRTFColorTable.AddColor(const AColor: TKColorRec);
var
  Value: TKMemoRTFColor;
begin
  Value := TKMemoRTFColor.Create;
  Value.ColorRec := AColor;
  Add(Value);
end;

function TKMemoRTFColorTable.getColor(AIndex: Integer): TColor;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := ColorRecToColor(Items[AIndex].ColorRec)
  else
    Result := clNone;
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
  FStyle := TKMemoBlockStyle.Create;
  FStyle.Brush.Color := clWhite;
  FStyle.BorderWidth := 1;
  FStyle.ContentPadding.AssignFromValues(2, 2, 2, 2);
  FWrap := 0;
end;

destructor TKMemoRTFShape.Destroy;
begin
  FContentPosition.Free;
  FStyle.Free;
  inherited;
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

function TKMemoRTFReader.DirectBool(const AValue: AnsiString): Boolean;
begin
  Result := Boolean(StrToIntDef(string(AValue), 0));
end;

function TKMemoRTFReader.DirectColor(const AValue: AnsiString): TColor;
begin
  Result := ColorRecToColor(MakeColorRec(StrToIntDef(string(AValue), 0)));
end;

function TKMemoRTFReader.DirectEMU(const AValue: AnsiString): Integer;
begin
  Result := EMUToPoints(StrToIntDef(string(AValue), 0));
end;

function TKMemoRTFReader.EMUToPoints(AValue: Integer): Integer;
begin
  Result := DivUp(AValue, 12700);
end;

procedure TKMemoRTFReader.AddText(const APart: TKString; ATextStyle: TKMemoTextStyle);
begin
  FTextStyle.Assign(ATextStyle);
  if FTextStyle.StyleChanged then
  begin
    FlushText;
    FTextStyle.StyleChanged := False;
  end;
  if FTextBlock = nil then
  begin
    FTextBlock := TKMemoTextBlock.Create(nil);
    FTextBlock.TextStyle.Assign(FTextStyle);
  end;
  FTextBlock.InsertString(APart);
end;

procedure TKMemoRTFReader.ApplyFont(ATextStyle: TKMemoTextStyle; AFontIndex: Integer);
var
  Font: TFont;
begin
  Font := FFontTable.GetFont(AFontIndex);
  if Font <> nil then
  begin
    ATextStyle.Font.Name := Font.Name;
    ATextStyle.Font.Charset := Font.Charset;
    ATextStyle.Font.Pitch := Font.Pitch;
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
  if FColor <> nil then
  begin
    FColorTable.Add(FColor);
    FColor := nil;
  end;
end;

procedure TKMemoRTFReader.FlushContainer;
begin
  if FContainer <> nil then
  begin
    FBlocks := FContainer.Parent;
    FBlocks.AddAt(FContainer);
    FContainer := nil;
  end;
end;

procedure TKMemoRTFReader.FlushFont;
begin
  if FFont <> nil then
  begin
    FFontTable.Add(FFont);
    FFont := nil;
  end;
end;

procedure TKMemoRTFReader.FlushImage;
begin
  if FImageBlock <> nil then
  begin
    FBlocks.AddAt(FImageBlock);
    FImageBlock := nil;
  end;
end;

procedure TKMemoRTFReader.FlushShape;
begin
  if FShape <> nil then
  begin
    case FSHape.ContentType of
      sctImage:
      begin
        // image was inside shape
        ImageBlock.Position := mbpRelative;
        ImageBlock.LeftOffset := FShape.ContentPosition.Left;
        ImageBlock.TopOffset := FShape.ContentPosition.Top;
        ImageBlock.ImageStyle.Assign(FShape.Style);
        FlushImage;
      end;
      sctContainer:
      begin
        // container was inside shape
        Container.Position := mbpRelative;
        Container.FixedWidth := True;
        Container.LeftOffset := FShape.ContentPosition.Left;
        Container.TopOffset := FShape.ContentPosition.Top;
        Container.RequiredWidth := FShape.ContentPosition.Right - FShape.ContentPosition.Left;
        Container.RequiredHeight := FShape.ContentPosition.Bottom - FShape.ContentPosition.Top;
        Container.BlockStyle.Assign(FShape.Style);
        FlushContainer;
      end;
      sctText:
      begin
        // unformatted text was inside shape
        FlushText;
      end;
    end;
    FreeAndNil(FShape);
  end;
end;

procedure TKMemoRTFReader.FlushText;
begin
  if FTextBlock <> nil then
  begin
    FBlocks.AddAt(FTextBlock);
    FTextBlock := nil;
  end;
end;

function TKMemoRTFReader.GetColor: TKMemoRTFColor;
begin
  if FColor = nil then
    FColor := TKMemoRTFColor.Create;
  Result := FColor;
end;

function TKMemoRTFReader.GetContainer: TKMemoContainer;
begin
  if FContainer = nil then
    FContainer := TKMemoContainer.Create(FBlocks);
  Result := FContainer;
end;

function TKMemoRTFReader.GetFont: TKMemoRTFFont;
begin
  if FFont = nil then
    FFont := TKMemoRTFFont.Create;
  Result := FFont;
end;

function TKMemoRTFReader.GetImageBlock: TKMemoImageBlock;
begin
  if FImageBlock = nil then
    FImageBlock := TKMemoImageBlock.Create(nil);
  Result := FImageBlock;
end;

function TKMemoRTFReader.GetShape: TKMemoRTFShape;
begin
  if FShape = nil then
    FShape := TKMemoRTFShape.Create;
  Result := FShape;
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
    FColor := nil;
    FColorTable.Clear;
    FContainer := nil;
    FDefCodePage := 0;
    FDefFontIndex := 0;
    FFont := nil;
    FHeaderRead := False;
    FImageBlock := nil;
    FImageClass := nil;
    FShape := nil;
    FTextBlock := nil;
    FBlocks.LockUpdate;
    try
      FBlocks.Clear;
      ReadGroup(0, rgNone, FMemo.TextStyle, FMemo.ParaStyle);
    finally
      FlushColor;
      FlushFont;
      FlushText;
      FlushShape;
      FlushImage;
      FBlocks.UnlockUpdate;
    end;
  except
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
    Color.Red := Byte(AParam)
  else if ACtrl = 'green' then
    Color.Green := Byte(AParam)
  else if ACtrl = 'blue' then
    Color.Blue := Byte(AParam);
  if AText = ';' then
    FlushColor;
end;

procedure TKMemoRTFReader.ReadFontGroup(const ACtrl, AText: AnsiString; AParam: Integer);
var
  I: Integer;
  S: string;
begin
  if ACtrl = 'f' then
    Font.FFontIndex := AParam
  else if ACtrl = 'fcharset' then
    Font.Font.Charset := AParam
  else if ACtrl = 'fprq' then
  begin
    case AParam of
      1: Font.Font.Pitch := fpFixed;
      2: Font.Font.Pitch := fpVariable;
    else
      Font.Font.Pitch := fpDefault;
    end;
    Font.Font.Pitch := TFontPitch(AParam)
  end
  else if (ACtrl = '') and (AText <> '') then
  begin
    S := string(AText);
    I := Pos(';', S);
    if I > 0 then
      Delete(S, I, 1);
    Font.Font.Name := S;
    FlushFont;
  end
end;

procedure TKMemoRTFReader.ReadGroup(GroupIndex: Integer; Group: TKMemoRTFGroup; ATextStyle: TKMemoTextStyle; AParaStyle: TKMemoParaStyle);
var
  Ctrl: AnsiString;
  Text: AnsiString;
  Param: Int64;
  LocalTextStyle: TKMemoTextStyle;
  LocalParaStyle: TKMemoParaStyle;
  PA: TKMemoParagraph;
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
          ReadGroup(GroupIndex + 1, Group, LocalTextStyle, LocalParaStyle)
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
                Shape.ContentType := sctContainer;
                FBlocks := Container.Blocks;
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
                FlushShape;
                Group := rgShapePict; // picture inside text
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
                // supported header control words
                if ReadHeaderGroup(Ctrl, Param, Group) then
                begin
                end
                // body control words
                else if Ctrl = 'info' then
                begin
                  Group := rgInfo;
                end
                else if Ctrl = 'shp' then
                begin
                  Group := rgShape;
                end
                else if Ctrl = 'nonshppict' then
                begin
                  Group := rgUnknown;
                end
                else if Ctrl = 'par' then
                begin
                  FlushText;
                  PA := FBlocks.AddParagraph;
                  PA.TextStyle.Assign(LocalTextStyle);
                  PA.ParaStyle.Assign(LocalParaStyle);
                end
                // supported table commands
                else if ReadTableFormatting(Ctrl, Param) then
                begin
                end
                // supported text formatting
                else if ReadTextFormatting(Ctrl, Param, LocalTextStyle) then
                begin
                end
                // supported paragraph formatting
                else if ReadParaFormatting(Ctrl, Param, LocalParaStyle) then
                begin
                end
                // supported special characters
                else if (Ctrl = '''') then
                begin
                  AddText(AnsiStringToWideString(AnsiChar(Param), FDefCodePage), LocalTextStyle);
                end
                else if Ctrl = 'tab' then
                begin
                  AddText(#9, LocalTextStyle);
                end
                else if Ctrl = '~' then
                begin
                  AddText(' ', LocalTextStyle); // nonbreaking spaces not supported yet
                end
                else if (Ctrl = '\') or (Ctrl = '{') or (Ctrl = '}') then
                begin
                  AddText(TKString(Ctrl), LocalTextStyle)
                end;
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

function TKMemoRTFReader.ReadHeaderGroup(const ACtrl: AnsiString; AParam: Integer; var Group: TKMemoRTFGroup): Boolean;
begin
  Result := False;
  // header control words (unfortunately header has not its own group)
  if ACtrl = 'ansicpg' then
  begin
    FDefCodePage := AParam;
  end
  else if ACtrl = 'deff' then
  begin
    FDefFontIndex := AParam;
  end
  else if ACtrl = 'fonttbl' then
  begin
    Group := rgFontTable;
  end
  else if ACtrl = 'colortbl' then
  begin
    Group := rgColorTable;
  end
  else if ACtrl = 'stylesheet' then
  begin
    Group := rgStyleSheet;
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
  else
    Result := False;
end;

procedure TKMemoRTFReader.ReadPictureGroup(const ACtrl, AText: AnsiString; AParam: Integer);
var
  S: AnsiString;
  MS: TMemoryStream;
  Image: TGraphic;
begin
  if ACtrl = 'pngblip' then
    FImageClass := TKPngImage
  else if ACtrl = 'wmetafile' then
    FImageClass := TMetafile
  else if ACtrl = 'jpegblip' then
    FImageClass := TJpegImage
  else if ACtrl = 'picwgoal' then
    ImageBlock.ScaleWidth := TwipsToPoints(AParam)
  else if ACtrl = 'pichgoal' then
    ImageBlock.ScaleHeight := TwipsToPoints(AParam)
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
              Image.LoadFromStream(MS);
              ImageBlock.Image.Graphic := Image;
            finally
              Image.Free;
            end;
          finally
            MS.Free;
          end;
        except
        end;
      end;
      FImageClass := nil;
    end;
  end;
end;

function TKMemoRTFReader.ReadShapeGroup(const ACtrl, AText: AnsiString; AParam: Integer): Boolean;
begin
  Result := True;
  if ACtrl = 'shpleft' then
    Shape.ContentPosition.Left := TwipsToPoints(AParam)
  else if ACtrl = 'shpright' then
    Shape.ContentPosition.Right := TwipsToPoints(AParam)
  else if ACtrl = 'shptop' then
    Shape.ContentPosition.Top := TwipsToPoints(AParam)
  else if ACtrl = 'shpbottom' then
    Shape.ContentPosition.Bottom := TwipsToPoints(AParam)
  else if ACtrl = 'shpwr' then
    Shape.Wrap := AParam
  else if ACtrl = 'sn' then
    Shape.CtrlName := AText
  else if ACtrl = 'sv' then
  begin
    Shape.CtrlValue := AText;
    if Shape.CtrlName = 'fFilled' then
    begin
      if not DirectBool(AText) then
        Shape.Style.Brush.Style := bsClear;
    end
    else if Shape.CtrlName = 'fillColor' then
      Shape.Style.Brush.Color := DirectColor(AText)
    else if Shape.CtrlName = 'fLine' then
    begin
      if not DirectBool(AText) then
        Shape.Style.BorderWidth := 0;
    end
    else if Shape.CtrlName = 'lineColor' then
      Shape.Style.BorderColor := DirectColor(AText)
    else if Shape.CtrlName = 'lineWidth' then
      Shape.Style.BorderWidth := DirectEMU(AText)
    else if Shape.CtrlName = 'pib' then
      Shape.ContentType := sctImage;
  end
  else
    Result := False;
end;

function TKMemoRTFReader.ReadTableFormatting(const ACtrl: AnsiString;
  AParam: Integer): Boolean;
begin
  Result := True;
  if ACtrl = 'trowd' then
  else
    Result := False;
end;

function TKMemoRTFReader.ReadTextFormatting(const ACtrl: AnsiString;
  AParam: Integer; ATextStyle: TKMemoTextStyle): Boolean;
begin
  Result := True;
  if ACtrl = 'plain' then
  begin
    ATextStyle.Assign(FMemo.TextStyle);
  end
  else if ACtrl = 'f' then
  begin
    ApplyFont(ATextStyle, AParam);
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
    ATextStyle.Font.Size := AParam div 2;
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
    ApplyHighlight(ATextStyle, AParam);
  end
  else
    Result := False;
end;

function TKMemoRTFReader.TwipsToPoints(AValue: Integer): Integer;
begin
  Result := DivUp(AValue, 20);
end;

end.
