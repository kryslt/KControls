unit KMemoRTF;

interface

uses
  Classes, Contnrs, Graphics,
  KFunctions, KGraphics, KMemo;

type
  TKMemoRTFGroup = (rgNone, rgUnknown, rgColorTable, rgFontTable, rgStyleSheet, rgInfo, rgShape, rgShapeInst, rgShapeResult, rgPicture);

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

  TKMemoRTFReader = class(TObject)
  private
    function GetFont: TKMemoRTFFont;
    function GetColor: TKMemoRTFColor;
    function GetImageBlock: TKMemoImageBlock;
  protected
    FBlocks: TKMemoBlocks;
    FCodePage: Integer;
    FColorTable: TKMemoRTFColorTable;
    FColor: TKMemoRTFColor;
    FDefFontIndex: Integer;
    FFontTable: TKMemoRTFFontTable;
    FFont: TKMemoRTFFont;
    FImageBlock: TKMemoImageBlock;
    FImageClass: TGraphicClass;
    FGraphicClass: TGraphicClass;
    FMemo: TKCustomMemo;
    FStream: TMemoryStream;
    FTextBlock: TKMemoTextBlock;
    FTextStyle: TKMemoTextStyle;
    procedure AddImage(AImage: TGraphic); virtual;
    procedure AddText(const APart: TKString; ATextStyle: TKMemoTextStyle); virtual;
    procedure ApplyFont(ATextStyle: TKMemoTextStyle; AFontIndex: Integer); virtual;
    procedure ApplyHighlight(ATextStyle: TKMemoTextStyle; AHighlightCode: Integer); virtual;
    procedure FlushColor; virtual;
    procedure FlushFont; virtual;
    procedure FlushImage; virtual;
    procedure FlushText; virtual;
    function ReadNext(out ACtrl, AText: AnsiString; out AParam: Int64): Boolean; virtual;
    procedure ReadColorGroup(const ACtrl: AnsiString; AParam: Integer); virtual;
    procedure ReadFontGroup(const ACtrl, AText: AnsiString; AParam: Integer); virtual;
    procedure ReadGroup(GroupIndex: Integer; Group: TKMemoRTFGroup; ATextStyle: TKMemoTextStyle; AParaStyle: TKMemoParaStyle); virtual;
    procedure ReadPictureGroup(const ACtrl, AText: AnsiString; AParam: Integer); virtual;
    function ReadParaFormatting(const ACtrl: AnsiString; AParam: Integer; AParaStyle: TKMemoParaStyle): Boolean; virtual;
    function ReadTextFormatting(const ACtrl: AnsiString; AParam: Integer; ATextStyle: TKMemoTextStyle): Boolean; virtual;
    procedure SkipSemiColon;
    function TwipsToPoints(AValue: Integer): Integer;
    property Color: TKMemoRTFColor read GetColor;
    property Font: TKMemoRTFFont read GetFont;
    property ImageBlock: TKMemoImageBlock read GetImageBlock;
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

procedure TKMemoRTFReader.AddImage(AImage: TGraphic);
begin
  ImageBlock.Image.Graphic := AImage;
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
  case AHighlightCode of
    1: Color := clBlack;
    2: Color := clBlue;
    3: Color := clAqua; // cyan
    4: Color := clLime; // green
    5: Color := clFuchsia; // magenta
    6: Color := clRed;
    7: Color := clYellow;
    9: Color := clNavy;
    10: Color := clTeal; // dark cyan
    11: Color := clGreen; // dark green
    12: Color := clPurple; // dark magenta
    13: Color := clMaroon; // dark red
    14: Color := clOlive; // dark yellow
    15: Color := clGray; // dark gray
    16: Color := clSilver; // light gray
  else
    Color := clNone;
  end;
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

procedure TKMemoRTFReader.Load(const AFileName: TKString);
var
  Ctrl, Text: AnsiString;
  Param: Int64;
begin
  if FileExists(AFileName) then
  try
    FStream.LoadFromFile(AFileName);
    FCodePage := 0;
    FColorTable.Clear;
    FMemo.TextStyle.Defaults;
    FMemo.ParaStyle.Defaults;
    FColor := nil;
    FFont := nil;
    FImageBlock := nil;
    FImageClass := nil;
    FTextBlock := nil;
    ReadNext(Ctrl, Text, Param);
    if Ctrl = '{' then
    begin
      FBlocks.LockUpdate;
      try
        FBlocks.Clear;
        ReadGroup(0, rgNone, FMemo.TextStyle, FMemo.ParaStyle);
      finally
        FBlocks.UnlockUpdate;
      end;
    end;
  except
  end;
end;

function TKMemoRTFReader.ReadNext(out ACtrl, AText: AnsiString; out AParam: Int64): Boolean;
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
  end
  else if (C = '{') or (C = '}') or (C = ';') then
    ACtrl := C // group
  else
  begin
    repeat
      if (C <> cCR) and (C <> cLF) then
        AText := AText + C;
      Result := FStream.Read(C, 1) > 0;
    until (C = '\') or (C = '{') or (C = '}') or not Result;
    FStream.Seek(-1, soFromCurrent);
  end;
end;

procedure TKMemoRTFReader.ReadColorGroup(const ACtrl: AnsiString; AParam: Integer);
begin
  if ACtrl = 'red' then
    Color.Red := Byte(AParam)
  else if ACtrl = 'green' then
    Color.Green := Byte(AParam)
  else if ACtrl = 'blue' then
    Color.Blue := Byte(AParam)
  else if ACtrl = ';' then
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
begin
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
            rgColorTable: ReadColorGroup(Ctrl, Param);
            rgFontTable: ReadFontGroup(Ctrl, Text, Param);
            rgInfo:;
            rgPicture: ReadPictureGroup(Ctrl, Text, Param);
            rgShape:
            begin
              if Ctrl = 'shpinst' then
                Group := rgShapeInst;
            end;
            rgShapeInst:
            begin
              if Ctrl = 'pict' then
                Group := rgPicture;
            end;
            rgStyleSheet:;
            rgUnknown:
            begin
              if Ctrl = 'shppict' then
                Group := rgPicture;
            end;
          else
            if Ctrl <> '' then
            begin
              if Ctrl = 'fonttbl' then
              begin
                Group := rgFontTable;
              end
              else if Ctrl = 'colortbl' then
              begin
                Group := rgColorTable;
                SkipSemiColon;
              end
              else if Ctrl = 'stylesheet' then
              begin
                Group := rgStyleSheet;
              end
              else if Ctrl = 'info' then
              begin
                Group := rgInfo;
              end
              else if Ctrl = 'shp' then
              begin
                Group := rgShape;
              end
              else if Ctrl = 'ansicpg' then
              begin
                FCodePage := Param;
              end
              else if Ctrl = 'deff' then
              begin
                FDefFontIndex := Param;
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
                AddText(AnsiStringToWideString(AnsiChar(Param), FCodePage), LocalTextStyle);
              end
              else if Ctrl = 'tab' then
              begin
                AddText(#9, LocalTextStyle);
              end
              else if Ctrl = '~' then
              begin
                AddText(' ', LocalTextStyle); // nonbreaking spaces not supported
              end
              else if (Ctrl = '\') or (Ctrl = '{') or (Ctrl = '}') then
              begin
                AddText(TKString(Ctrl), LocalTextStyle)
              end;
            end
            else if Text <> '' then
            begin
              AddText(TKString(Text), LocalTextStyle);
            end
          end;
        end;
      end;
    end;
  finally
    LocalParaStyle.Free;
    LocalTextStyle.Free;
  end;
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
              AddImage(Image);
            finally
              Image.Free;
            end;
          finally
            MS.Free;
          end;
        except
        end;
      end;
      FlushImage;
      FImageClass := nil;
    end else
    asm
      nop
    end;
  end;
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

procedure TKMemoRTFReader.SkipSemiColon;
var
  C: AnsiChar;
begin
  FStream.Read(C, 1);
  if C <> ';' then
    FStream.Seek(-1, soFromCurrent);
end;

function TKMemoRTFReader.TwipsToPoints(AValue: Integer): Integer;
begin
  Result := AValue div 20;
end;

end.
