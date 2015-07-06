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
  end;

  TKMemoRTFColorTable = class(TObjectList)
  private
    function GetItem(Index: Integer): TKMemoRTFColor;
    procedure SetItem(Index: Integer; const Value: TKMemoRTFColor);
  public
    procedure AddColor(const AColor: TKColorRec);
    function GetColor(AIndex: Integer): TColor;
    property Items[Index: Integer]: TKMemoRTFColor read GetItem write SetItem; default;
  end;

  TKMemoRTFReader = class(TObject)
  private
    function GetImageBlock: TKMemoImageBlock;
    function GetTextBlock: TKMemoTextBlock;
  protected
    FBlocks: TKMemoBlocks;
    FCodePage: Integer;
    FColorTable: TKMemoRTFColorTable;
    FColorRec: TKColorRec;
    FImageBlock: TKMemoImageBlock;
    FImageClass: TGraphicClass;
    FGraphicClass: TGraphicClass;
    FMemo: TKCustomMemo;
    FStream: TMemoryStream;
    FTextBlock: TKMemoTextBlock;
    procedure AddText(const APart: TKString); virtual;
    procedure ApplyHighlight(ATextStyle: TKMemoTextStyle; AHighlightCode: Integer); virtual;
    procedure FlushImage; virtual;
    procedure FlushText(ATextStyle: TKMemoTextStyle); virtual;
    function ReadNext(out ACtrl, AText: AnsiString; out AParam: Int64): Boolean; virtual;
    procedure ReadColorGroup(const ACtrl: AnsiString; AParam: Integer); virtual;
    procedure ReadGroup(GroupIndex: Integer; Group: TKMemoRTFGroup; ATextStyle: TKMemoTextStyle; AParaStyle: TKMemoParaStyle); virtual;
    procedure ReadPictureGroup(const ACtrl, AText: AnsiString; AParam: Integer); virtual;
    procedure SkipSemiColon;
    function TwipsToPoints(AValue: Integer): Integer;
    property ImageBlock: TKMemoImageBlock read GetImageBlock;
    property TextBlock: TKMemoTextBlock read GetTextBlock;
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

{ TKMemoRTFReader }

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

constructor TKMemoRTFReader.Create(AMemo: TKCustomMemo);
begin
  FBlocks := AMemo.Blocks;
  FColorTable := TKMemoRTFColorTable.Create;
  FMemo := AMemo;
  FStream := TMemoryStream.Create;
end;

destructor TKMemoRTFReader.Destroy;
begin
  FColorTable.Free;
  FStream.Free;
  inherited;
end;

procedure TKMemoRTFReader.AddText(const APart: TKString);
begin
  TextBlock.InsertString(APart);
end;

procedure TKMemoRTFReader.FlushImage;
begin
  if FImageBlock <> nil then
  begin
    FBlocks.AddAt(FImageBlock);
    FImageBlock := nil;
  end;
end;

procedure TKMemoRTFReader.FlushText(ATextStyle: TKMemoTextStyle);
begin
  if FTextBlock <> nil then
  begin
    FTextBlock.TextStyle.Assign(ATextStyle);
    FBlocks.AddAt(FTextBlock);
    FTextBlock := nil;
  end;
end;

function TKMemoRTFReader.GetImageBlock: TKMemoImageBlock;
begin
  if FImageBlock = nil then
    FImageBlock := TKMemoImageBlock.Create(nil);
  Result := FImageBlock;
end;

function TKMemoRTFReader.GetTextBlock: TKMemoTextBlock;
begin
  if FTextBlock = nil then
    FTextBlock := TKMemoTextBlock.Create(nil);
  Result := FTextBlock;
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
    FColorRec.Value := 0;
    FColorTable.Clear;
    FMemo.TextStyle.Defaults;
    FMemo.ParaStyle.Defaults;
    FTextBlock := nil;
    FImageBlock := nil;
    FImageClass := nil;
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
          AParam := HexStrToInt(ParamStr, Length(ParamStr), False, Code);
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
    FColorRec.R := Byte(AParam)
  else if ACtrl = 'green' then
    FColorRec.G := Byte(AParam)
  else if ACtrl = 'blue' then
    FColorRec.B := Byte(AParam)
  else if ACtrl = ';' then
  begin
    FColorTable.AddColor(FColorRec);
    FColorRec.Value := 0;
  end;
end;

procedure TKMemoRTFReader.ReadGroup(GroupIndex: Integer; Group: TKMemoRTFGroup; ATextStyle: TKMemoTextStyle; AParaStyle: TKMemoParaStyle);
var
  Ctrl: AnsiString;
  Text: AnsiString;
  Param: Int64;
  Code, Value: Integer;
  UText: TKString;
  LocalTextStyle: TKMemoTextStyle;
  LocalParaStyle: TKMemoParaStyle;
  NewDestination: TKMemoRTFGroup;
  PA: TKMemoParagraph;
begin
  LocalParaStyle := TKMemoParaStyle.Create;
  LocalTextStyle := TKMemoTextStyle.Create;
  try
    LocalParaStyle.Assign(AParaStyle);
    LocalTextStyle.Assign(ATextStyle);
    Ctrl := '';
    NewDestination := Group;
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
            rgFontTable:;
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
              else if Ctrl = 'plain' then
              begin
                FlushText(LocalTextStyle);
                LocalTextStyle.Assign(ATextStyle);
              end
              else if Ctrl = 'par' then
              begin
                FlushText(LocalTextStyle);
                PA := FBlocks.AddParagraph;
                PA.ParaStyle.Assign(LocalParaStyle);
              end
              else if Ctrl = 'pard' then
              begin
                LocalParaStyle.Assign(FMemo.ParaStyle);
              end
              // supported text formatting
              else if Ctrl = 'b' then
              begin
                FlushText(LocalTextStyle);
                if Param = 0 then
                  LocalTextStyle.Font.Style := LocalTextStyle.Font.Style - [fsBold]
                else
                  LocalTextStyle.Font.Style := LocalTextStyle.Font.Style + [fsBold];
              end
              else if Ctrl = 'i' then
              begin
                FlushText(LocalTextStyle);
                if Param = 0 then
                  LocalTextStyle.Font.Style := LocalTextStyle.Font.Style - [fsItalic]
                else
                  LocalTextStyle.Font.Style := LocalTextStyle.Font.Style + [fsItalic];
              end
              else if Ctrl = 'ul' then
              begin
                FlushText(LocalTextStyle);
                if Param = 0 then
                  LocalTextStyle.Font.Style := LocalTextStyle.Font.Style - [fsUnderline]
                else
                  LocalTextStyle.Font.Style := LocalTextStyle.Font.Style + [fsUnderline];
              end
              else if Ctrl = 'strike' then
              begin
                FlushText(LocalTextStyle);
                if Param = 0 then
                  LocalTextStyle.Font.Style := LocalTextStyle.Font.Style - [fsStrikeout]
                else
                  LocalTextStyle.Font.Style := LocalTextStyle.Font.Style + [fsStrikeout];
              end
              else if Ctrl = 'caps' then
              begin
                FlushText(LocalTextStyle);
                LocalTextStyle.Capitals := tcaNormal;
              end
              else if Ctrl = 'scaps' then
              begin
                FlushText(LocalTextStyle);
                LocalTextStyle.Capitals := tcaSmall;
              end
              else if Ctrl = 'fs' then
              begin
                FlushText(LocalTextStyle);
                LocalTextStyle.Font.Size := Param div 2;
              end
              else if Ctrl = 'cf' then
              begin
                FlushText(LocalTextStyle);
                LocalTextStyle.Font.Color := FColorTable.GetColor(Param - 1);
              end
              else if Ctrl = 'cb' then
              begin
                FlushText(LocalTextStyle);
                LocalTextStyle.Brush.Color := FColorTable.GetColor(Param - 1);
              end
              else if Ctrl = 'highlight' then
              begin
                FlushText(LocalTextStyle);
                ApplyHighlight(LocalTextStyle, Param);
              end
              // supported paragraph formatting
              else if Ctrl = 'fi' then
              begin
                LocalParaStyle.FirstIndent := TwipsToPoints(Param);
              end
              else if Ctrl = 'li' then
              begin
                LocalParaStyle.LeftPadding := TwipsToPoints(Param);
              end
              else if Ctrl = 'ri' then
              begin
                LocalParaStyle.RightPadding := TwipsToPoints(Param);
              end
              else if Ctrl = 'sb' then
              begin
                LocalParaStyle.TopPadding := TwipsToPoints(Param);
              end
              else if Ctrl = 'sa' then
              begin
                LocalParaStyle.BottomPadding := TwipsToPoints(Param);
              end
              else if Ctrl = 'ql' then
              begin
                LocalParaStyle.HAlign := halLeft;
              end
              else if Ctrl = 'qr' then
              begin
                LocalParaStyle.HAlign := halRight;
              end
              else if Ctrl = 'qc' then
              begin
                LocalParaStyle.HAlign := halCenter;
              end
              else if Ctrl = 'qj' then
              begin
                //LocalParaStyle.HAlign := halJustify; // TODO
              end
              else if Ctrl = 'cbpat' then
              begin
                LocalParaStyle.Brush.Color := FColorTable.GetColor(Param - 1);
              end
              // supported special characters
              else if Group = rgNone then
              begin
                if (Ctrl = '''') then
                begin
                  Text := AnsiStringToWideString(AnsiChar(Param), FCodePage);
                  AddText(Text);
                end
                else if Ctrl = 'tab' then
                begin
                  AddText(#9);
                end
                else if Ctrl = '~' then
                begin
                  AddText(' '); // nonbreaking spaces not supported
                end
                else if (Ctrl = '\') or (Ctrl = '{') or (Ctrl = '}') then
                begin
                  AddText(TKString(Ctrl))
                end
              end;
            end
            else if (Text <> '') and (Group = rgNone) then
            begin
              AddText(TKString(Text));
            end
          end;
        end;
      end;
    end;
    if LocalTextStyle.StyleChanged then
      FlushText(LocalTextStyle);
  finally
    LocalParaStyle.Free;
    LocalTextStyle.Free;
  end;
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
      FlushImage;
      FImageClass := nil;
    end else
    asm
      nop
    end;
  end;
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
