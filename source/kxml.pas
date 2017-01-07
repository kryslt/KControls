{ @abstract(This unit contains simple XML parser and writer.)
  @author(Graham Murt, portions Tomas Krysl)

  Copyright (c) 2003 Graham Murt  - www.murtsoft.co.uk
  Portions Copyright (c) 2015 Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }

// Original copyright notice: 

{******************************************************************************}
{                                                                              }
{   Lightweight native pascal XML implementation (originally GmXml.pas)        }
{                                                                              }
{           Copyright (c) 2003 Graham Murt  - www.murtsoft.co.uk               }
{                                                                              }
{   Feel free to e-mail me with any comments, suggestions, bugs or help at:    }
{                                                                              }
{                           graham@murtsoft.co.uk                              }
{                                                                              }
{******************************************************************************}

unit kxml;

{$include kcontrols.inc}

interface

uses Classes, Contnrs, SysUtils;

resourcestring
  sParseNoOpenTag = 'XML parsing error: Open tag not found.';
  sParseValueOutSideTag = 'XML parsing error: Value outside tag.';
  sParseNumericConvertError = 'XML parsing error: Cannot convert value %s to number.';

const
  COMP_VERSION = 0.13;
  XML_SPECIFICATION = '<?xml version="1.0"%s?>';

type
  TXmlNode = class;
  TXmlNodeList = class;

  TXmlEnumNodeEvent = procedure(Sender: TObject; ANode: TXmlNode) of object;

  // *** TXmlNodeElement ***

  TXmlNodeAttribute = class
  private
    FName: string;
    FValue: string;
    procedure SetName(AValue: string);
    procedure SetValue(AValue: string);
  public
    procedure Assign(Source: TXmlNodeAttribute);
    property Name: string read FName write SetName;
    property Value: string read FValue write SetValue;
  end;

  TXmlNodeAttributes = class(TObjectList) // without generics here for backward compatibility...
  private
    function GetItem(Index: Integer): TXmlNodeAttribute;
    procedure SetItem(Index: Integer; const Value: TXmlNodeAttribute);
  public
    procedure Assign(Source: TXmlNodeAttributes);
    property Items[Index: Integer]: TXmlNodeAttribute read GetItem write SetItem; default;
    procedure AddPair(const AName, AValue: string);
    function Find(const AName: string; out AValue: string): Boolean;
  end;

  // *** TXmlNode ***

  { TXmlNode }

  TXmlNode = class(TPersistent)
  private
    FChildren: TXmlNodeList;
    FAttributes: TXmlNodeAttributes;
    FName: string;
    FParent: TXmlNode;
    FValue: string;
    // events...
    function GetAsDisplayString: string;
    function GetAsHexInt: integer;
    function GetIsLeafNode: Boolean;
    function GetAsBoolean: Boolean;
    function GetAsFloat: Extended;
    function GetAsInteger: integer;
    function GetAsString: string;
    function GetAttribute: TXmlNodeAttribute;
    function GetLevel: integer;
    function CloseTag: string;
    function OpenTag: string;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsHexInt(AValue: integer);
    procedure SetAsInteger(const Value: integer);
    procedure SetAsString(const Value: string);
    procedure SetName(Value: string); virtual;
  public
    constructor Create(AParentNode: TXmlNode); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure EnumerateNodes(ACallback: TXmlEnumNodeEvent);

    procedure Clear;

    function ChildAsBoolean(const ChildNodeName: string; DefValue: Boolean): Boolean;
    function ChildAsDateTime(const ChildNodeName: string; DefValue: TDateTime): TDateTime;
    function ChildAsFloat(const ChildNodeName: string; DefValue: Double): Double;
    function ChildAsInteger(const ChildNodeName: string; DefValue: Int64): Int64;
    function ChildAsInteger(const ChildNodeName: string; DefValue: Integer): Integer;
    function ChildAsString(const ChildNodeName: string; const DefValue: string): string;

    property AsDisplayString: string read GetAsDisplayString;
    property AsString: string read GetAsString write SetAsString;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsHexInt: integer read GetAsHexInt write SetAsHexInt;
    property Attribute: TXmlNodeAttribute read GetAttribute;
    property Attributes: TXmlNodeAttributes read FAttributes;
    property Children: TXmlNodeList read FChildren;
    property IsLeafNode: Boolean read GetIsLeafNode;
    property Level: integer read GetLevel;
    property Name: string read FName write SetName;
    property Parent: TXmlNode read FParent;
  end;

  // *** TXmlNodeList ***

  TXmlNodeList = class
  private
    FParent: TXmlNode;
    FList: TList;
    function GetCount: integer;
    function GetNode(index: integer): TXmlNode;
    function GetNodeByName(AName: string): TXmlNode;
    procedure SetNodeByName(AName: string; ANode: TXmlNode);
    function GetRoot: TXmlNode;
  protected
  public
    constructor Create(AParent: TXmlNode);
    destructor Destroy; override;
    procedure Assign(Source: TXmlNodeList);
    function Add(AName: string): TXmlNode;
    procedure AddNode(ANode: TXmlNode);
    procedure DeleteNode(ANode: TXmlNode);
    procedure SetNode(index: integer; const Value: TXmlNode);
    //procedure NextNode;
    procedure Clear;
    procedure PeekNode(ANode: TXmlNode);
    procedure PokeNode(ANode: TXmlNode);
    property Count: integer read GetCount;
    property Node[index: integer]: TXmlNode read GetNode write SetNode; default;
    property NodeByName[AName: string]: TXmlNode read GetNodeByName write SetNodeByName;
    property Root: TXmlNode read GetRoot;
  end;

  // *** TXml ***

  TXml = class(TComponent)
  private
    FAutoIndent: Boolean;
    FEncoding: string;
    FIncludeHeader: Boolean;
    FKeepLineBreaksInValues: Boolean;
    FLineBreaks: Boolean;
    FNodes: TXmlNodeList;
    FStrings: TSTringList;
    function GetAbout: string;
    function GetDisplayText: string;
    function GetEncodingStr: string;
    function GetIndent(ALevel: integer): string;
    function GetText(ReplaceEscapeChars: Boolean): string;
    function GetXmlText: string;
    procedure SetAsText(Value: string);
    procedure SetAbout(Value: string);
    procedure SetAutoIndent(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function IsXML(const AText: string; AIncludeHeader: Boolean): Boolean;
    procedure LoadFromFile(AFileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(AFilename: string);
    procedure SaveToStream(Stream: TStream);
    property DisplayText: string read GetDisplayText;
    property Nodes: TXmlNodeList read FNodes;
    property Text: string read GetXmlText write SetAsText;
    property LineBreaks: Boolean read FLineBreaks write FLineBreaks;
    property KeepLineBreaksInValues: Boolean read FKeepLineBreaksInValues write FKeepLineBreaksInValues;
  published
    property About: string read GetAbout write SetAbout;
    property AutoIndent: Boolean read FAutoIndent write SetAutoIndent default True;
    property Encoding: string read FEncoding write FEncoding;
    property IncludeHeader: Boolean read FIncludeHeader write FIncludeHeader default True;
  end;

function DecodeText(const AText: AnsiString): AnsiString;
function EncodeText(const AText: AnsiString): AnsiString;

function StrToFloatDefLoc(const AValue: string; ADefault: Double; const AFormatSettings: TFormatSettings): Double; overload;
function StrToFloatDefLoc(const AValue: string; ADefault: Double): Double; overload;

implementation

uses
  KFunctions;

//------------------------------------------------------------------------------

// *** Unit Functions ***

function StrPos(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

procedure ReplaceText(var AText: string; AFind, AReplace: string);
var
  Index: integer;
begin
  Index := 1;
  while StrPos(AFind, AText, Index) <> 0 do
  begin
    Index := StrPos(AFind, AText, Index);
    Delete(AText, Index, Length(AFind));
    Insert(AReplace, AText, Index);
    Inc(Index, Length(AReplace));
  end;
end;

procedure XmlError(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

function DecodeText(const AText: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := Length(AText);
  SetLength(Result, Length(AText));
  while I > 0 do
  begin
    if I < Length(AText) then
      Result[I] := AnsiChar(Ord(AText[I]) - Ord(Result[I + 1]))
    else
      Result[I] := AnsiChar(Ord(AText[I]) - $40);
    Result[I] := AnsiChar(Ord(Result[I]) + $80);
    Dec(I);
  end;
end;

function EncodeText(const AText: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := 1;
  SetLength(Result, Length(AText));
  while I <= Length(AText) do
  begin
    Result[I] := AnsiChar(Ord(AText[I]) - $80);
    if I < Length(AText) then
      Result[I] := AnsiChar(Ord(Result[I]) + Ord(AText[I + 1]))
    else
      Result[I] := AnsiChar(Ord(Result[I]) + $40);
    Inc(I);
  end;
end;

function StrToFloatDefLoc(const AValue: string; ADefault: Double; const AFormatSettings: TFormatSettings): Double;
var
  S: string;
begin
  S := AValue;
  if AFormatSettings.DecimalSeparator <> '.' then
    S := StringReplace(S, '.', AFormatSettings.DecimalSeparator, [rfReplaceAll]);
  if AFormatSettings.DecimalSeparator <> ',' then
    S := StringReplace(S, ',', AFormatSettings.DecimalSeparator, [rfReplaceAll]);
  Result := StrToFloatDef(S, ADefault);
end;

function StrToFloatDefLoc(const AValue: string; ADefault: Double): Double;
begin
  Result := StrToFloatDefLoc(AValue, ADefault, GetFormatSettings);
end;

//------------------------------------------------------------------------------

// *** TXmlNodeElement ***

procedure TXmlNodeAttribute.Assign(Source: TXmlNodeAttribute);
begin
  if Source <> nil then
  begin
    FName := Source.FName;
    FValue := Source.FValue;
  end;
end;

procedure TXmlNodeAttribute.SetName(AValue: string);
begin
  TrimWhiteSpaces(AValue, cLineBreaks + cWordBreaks);
  FName := AValue;
end;

procedure TXmlNodeAttribute.SetValue(AValue: string);
begin
  TrimWhiteSpaces(AValue, cLineBreaks + cWordBreaks);
  FValue := AValue;
  ReplaceText(FValue, '"', '');
end;

//------------------------------------------------------------------------------

{ TXmlNodeAttributes }

procedure TXmlNodeAttributes.AddPair(const AName, AValue: string);
var
  Attr: TXmlNodeAttribute;
begin
  Attr := TXmlNodeAttribute.Create;
  Attr.Name := AName;
  Attr.Value := AValue;
  Add(Attr);
end;

procedure TXmlNodeAttributes.Assign(Source: TXmlNodeAttributes);
var
  I: Integer;
begin
  Clear;
  if Source <> nil then
  begin
    for I := 0 to Source.Count - 1 do
    begin
      AddPair(Source[I].Name, Source[I].Value);
    end;
  end;
end;

function TXmlNodeAttributes.Find(const AName: string;
  out AValue: string): Boolean;
var
  I: Integer;
  Attr: TXmlNodeAttribute;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Attr := TXmlNodeAttribute(Get(I));
    if SameText(AName, Attr.Name) then
    begin
      AValue := Attr.Value;
      Exit(True);
    end;
  end;
end;

function TXmlNodeAttributes.GetItem(Index: Integer): TXmlNodeAttribute;
begin
  Result := TXmlNodeAttribute(inherited GetItem(Index));
end;

procedure TXmlNodeAttributes.SetItem(Index: Integer;
  const Value: TXmlNodeAttribute);
begin
  inherited SetItem(Index, Value);
end;

//------------------------------------------------------------------------------

// *** TXmlNode ***

constructor TXmlNode.Create(AParentNode: TXmlNode);
begin
  inherited Create;
  FChildren := TXmlNodeList.Create(Self);
  FAttributes := TXmlNodeAttributes.Create;
  FParent := AParentNode;
end;

destructor TXmlNode.Destroy;
begin
  FAttributes.Free;
  FChildren.Free;
  inherited Destroy;
end;

procedure TXmlNode.Assign(Source: TPersistent);
begin
  if Source is TXmlNode then
  begin
    FChildren.FParent := Self;
    FAttributes.Assign(TXmlNode(Source).FAttributes);
    FName := TXmlNode(Source).FName;
    FValue := TXmlNode(Source).FValue;
    FChildren.Assign(TXmlNode(Source).FChildren);
  end;
end;

procedure TXmlNode.Clear;
begin
  FAttributes.Clear;
  FChildren.Clear;
end;

function TXmlNode.ChildAsBoolean(const ChildNodeName: string;
  DefValue: Boolean): Boolean;
var
  Child: TXmlNode;
begin
  Result := DefValue;
  Child := Children.NodeByName[ChildNodeName];
  if Child <> nil then
    Result := StrToBoolDef(Child.AsDisplayString, DefValue);
end;

function TXmlNode.ChildAsDateTime(const ChildNodeName: string; DefValue: TDateTime): TDateTime;
var
  Child: TXmlNode;
begin
  Result := DefValue;
  Child := Children.NodeByName[ChildNodeName];
  if Child <> nil then
    Result := StrToDateTimeDef(Child.AsDisplayString, DefValue);
end;

function TXmlNode.ChildAsFloat(const ChildNodeName: string;
  DefValue: Double): Double;
var
  Child: TXmlNode;
begin
  Result := DefValue;
  Child := Children.NodeByName[ChildNodeName];
  if Child <> nil then
    Result := StrToFloatDefLoc(Child.AsDisplayString, DefValue);
end;

function TXmlNode.ChildAsInteger(const ChildNodeName: string;
  DefValue: Integer): Integer;
var
  Child: TXmlNode;
  Code: Integer;
  S: string;
begin
  Result := DefValue;
  Child := Children.NodeByName[ChildNodeName];
  if Child <> nil then
  begin
    S := Child.AsDisplayString;
    if not TryStrToInt(S, Result) then
    begin
      Result := HexStrToInt(S, 8, True, Code);
      if Code <> 0 then
        Result := DefValue;
    end;
  end;
end;

function TXmlNode.ChildAsInteger(const ChildNodeName: string;
  DefValue: Int64): Int64;
var
  Child: TXmlNode;
  Code: Integer;
  S: string;
begin
  Result := DefValue;
  Child := Children.NodeByName[ChildNodeName];
  if Child <> nil then
  begin
    S := Child.AsDisplayString;
    if not TryStrToInt64(S, Result) then
    begin
      Result := HexStrToInt(S, 16, True, Code);
      if Code <> 0 then
        Result := DefValue;
    end;
  end;
end;

function TXmlNode.ChildAsString(const ChildNodeName: string;
  const DefValue: string): string;
var
  Child: TXmlNode;
begin
  Result := DefValue;
  Child := Children.NodeByName[ChildNodeName];
  if Child <> nil then
    Result := Child.AsDisplayString;
end;

function TXmlNode.CloseTag: string;
begin
  Result := '</'+FName+'>';
end;

function TXmlNode.OpenTag: string;
var
  I: Integer;
begin
  if FAttributes.Count = 0 then
    Result := '<' + Name + '>'
  else
  begin
    Result := '<' + Name;
    for I := 0 to FAttributes.Count - 1 do
      Result := Format('%s %s="%s"',[Result, FAttributes[I].Name, FAttributes[I].Value]);
    Result := Result + '>';
  end;
end;

procedure TXmlNode.EnumerateNodes(ACallback: TXmlEnumNodeEvent);
var
  ICount: integer;
begin
  for ICount := 0 to FChildren.Count-1 do
  begin
    if Assigned(ACallback) then ACallback(Self, FChildren[ICount]);
  end;
end;

function TXmlNode.GetAsBoolean: Boolean;
begin
  Result := Boolean(StrToInt(FValue));
end;

function TXmlNode.GetAsFloat: Extended;
begin
  Result := StrToFloat(FValue);
end;

function TXmlNode.GetAsInteger: integer;
begin
  Result := StrToInt(FValue);
end;

function TXmlNode.GetAsHexInt: integer;
var
  Code: Integer;
begin
  Result := HexStrToInt(FValue, 8, True, Code);
  if Code <> 0 then
    Error(Format(sParseNumericConvertError, [FValue]));
end;

function TXmlNode.GetAsString: string;
begin
  Result := FValue;
end;

function TXmlNode.GetAttribute: TXmlNodeAttribute;
begin
  if FAttributes.Count = 0 then
    FAttributes.Add(TXmlNodeAttribute.Create);
  Result := FAttributes[0];
end;

function TXmlNode.GetLevel: integer;
var
  AParent: TXmlNode;
begin
  AParent := Parent;
  Result := 0;
  while AParent <> nil do
  begin
    AParent := AParent.Parent;
    Inc(Result);
  end;
end;

procedure TXmlNode.SetAsBoolean(const Value: Boolean);
begin
  FValue := IntToStr(Ord(Value));
end;

procedure TXmlNode.SetAsFloat(const Value: Extended);
begin
  FValue := FloatToStr(Value);
end;

procedure TXmlNode.SetAsHexInt(AValue: integer);
begin
  FValue := IntToHexStr(AValue, 8, '0x', '', False);
end;

procedure TXmlNode.SetAsInteger(const Value: integer);
begin
  FValue := IntToStr(Value);
end;

procedure TXmlNode.SetAsString(const Value: string);
begin
  FValue := Value;
  // replace any illegal characters...
  ReplaceText(FValue, '&', '&amp;');
  ReplaceText(FValue, '<', '&lt;');
  ReplaceText(FValue, '>', '&gt;');
  ReplaceText(FValue, '''', '&apos;');
  ReplaceText(FValue, '"', '&quot;');
end;

function TXmlNode.GetAsDisplayString: string;
begin
  Result := FValue;
  // replace any illegal characters...
  ReplaceText(Result, '&amp;', '&');
  ReplaceText(Result, '&lt;', '<');
  ReplaceText(Result, '&gt;', '>');
  ReplaceText(Result, '&apos;', '''');
  ReplaceText(Result, '&quot;', '"');
  ReplaceText(Result, '&pos;', ''''); // backward compatibility
end;

function TXmlNode.GetIsLeafNode: Boolean;
begin
  Result := FChildren.Count = 0;
end;

procedure TXmlNode.SetName(Value: string);
var
  AElement, AValue: string;
  CharPos: Integer;
  Attr: TXmlNodeAttribute;
begin
  FAttributes.Clear;
  FName := Value;
  if FName <> '' then
  begin
    if FName[1] = '<' then Delete(FName, 1, 1);
    if FName[Length(FName)] = '>' then Delete(FName, Length(FName), 1);
    TrimWhiteSpaces(FName, cLineBreaks + cWordBreaks);

    // extract attribute if one exists...
    if Pos('=', FName) <> 0 then
    begin
      AElement := FName;
      CharPos := Pos(' ', FName);
      FName := Copy(FName, 1, CharPos-1);
      Delete(AElement, 1, CharPos);
      TrimWhiteSpaces(AElement, cWordBreaks);
      while AElement <> '' do
      begin
        Attr := TXmlNodeAttribute.Create;
        CharPos := Pos('=', AElement);
        AValue := Copy(AElement, 0, CharPos - 1);
        TrimWhiteSpaces(AValue, cWordBreaks);
        Attr.Name := AValue;
        Delete(AElement, 1, CharPos);
        TrimWhiteSpaces(AElement, cWordBreaks);
        if AElement <> '' then
        begin
          if AElement[1] = '"' then
            Delete(AElement, 1, 1);
          CharPos := Pos('"', AElement);
          AValue := Copy(AElement, 1, CharPos - 1);
          Attr.Value := AValue;
          Delete(AElement, 1, CharPos);
        end;
        FAttributes.Add(Attr);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

// *** TXmlNodeList ***

constructor TXmlNodeList.Create(AParent: TXmlNode);
begin
  inherited Create;
  FList := TList.Create;
  FParent := AParent;
end;

destructor TXmlNodeList.Destroy;
var
  ICount: integer;
begin
  for ICount := Count-1 downto 0 do
    Node[ICount].Free;
  FList.Free;
  inherited Destroy;
end;

function TXmlNodeList.Add(AName: string): TXmlNode;
begin
  Result := TXmlNode.Create(FParent);
  Result.Name := AName;
  AddNode(Result);
end;

procedure TXmlNodeList.AddNode(ANode: TXmlNode);
begin
  FList.Add(ANode);
end;

procedure TXmlNodeList.Assign(Source: TXmlNodeList);
var
  I: Integer;
  N: TXmlNode;
begin
  if Source <> nil then
  begin
    for I := 0 to Source.Count - 1 do
    begin
      N := TXmlNode.Create(FParent);
      N.Assign(Source.Node[I]);
      AddNode(N);
    end;
  end;
end;

procedure TXmlNodeList.Clear;
var
  ICount: integer;
begin
  for ICount := 0 to FList.Count-1 do
  begin
    Node[ICount].Free;
    Node[ICount] := nil;
  end;
  FList.Clear;
end;

procedure TXmlNodeList.DeleteNode(ANode: TXmlNode);
var
  I: Integer;
begin
  I := FList.IndexOf(ANode);
  if I >= 0 then
  begin
    Node[I].Free;
    FList.Delete(I);
  end;
end;

function TXmlNodeList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TXmlNodeList.GetNode(index: integer): TXmlNode;
begin
  Result := TXmlNode(FList[index]);
end;

function TXmlNodeList.GetNodeByName(AName: string): TXmlNode;
var
  ICount: integer;
begin
  Result := nil;
  for ICount := 0 to Count-1 do
  begin
    if LowerCase(Node[ICount].Name) = LowerCase(AName) then
    begin
      Result := Node[ICount];
      Exit;
    end;
  end;
end;

procedure TXmlNodeList.SetNodeByName(AName: string; ANode: TXmlNode);
var
  ICount: integer;
begin
  for ICount := 0 to Count-1 do
  begin
    if LowerCase(Node[ICount].Name) = LowerCase(AName) then
    begin
      Node[ICount] := ANode;
      Exit;
    end;
  end;
end;

function TXmlNodeList.GetRoot: TXmlNode;
begin
  Result := nil;
  if Count > 0 then Result := Node[0];
end;

procedure TXmlNodeList.PeekNode(ANode: TXmlNode);
var
  I: Integer;
begin
  I := FList.IndexOf(ANode);
  if I >= 0 then
    FList.Delete(I);
end;

procedure TXmlNodeList.PokeNode(ANode: TXmlNode);
begin
  FList.Add(ANode);
end;

procedure TXmlNodeList.SetNode(index: integer; const Value: TXmlNode);
begin
  FList[index] := Value;
end;

//------------------------------------------------------------------------------

// *** TXml ***

constructor TXml.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrings := TStringList.Create;
  FNodes := TXmlNodeList.Create(nil);
  FIncludeHeader := True;
  FAutoIndent := True;
  FEncoding := '';
  FLineBreaks := True;
  FKeepLineBreaksInValues := True;
end;

destructor TXml.Destroy;
begin
  FNodes.Free;
  FStrings.Free;
  inherited Destroy;
end;

function TXml.GetAbout: string;
begin
  Result := Format('%s v%f', [ClassName, COMP_VERSION]);
end;

function TXml.GetDisplayText: string;
begin
  Result := GetText(True);
end;

function TXml.GetEncodingStr: string;
begin
  Result := '';
  if FEncoding <> '' then Result := Format(' encoding="%s"', [FEncoding]);
end;

function TXml.GetIndent(ALevel: integer): string;
begin
  Result := '';
  if FAutoIndent then Result := StringOfChar(' ', ALevel*2);

end;

function TXml.GetText(ReplaceEscapeChars: Boolean): string;

  procedure NodeToStringList(var AXml: TStringList; ANode: TXmlNode; AReplaceChars: Boolean);
  var
    ICount: integer;
    AValue: string;
  begin
    if ANode.IsLeafNode then
    begin
      if AReplaceChars then AValue := ANode.AsDisplayString else AValue := ANode.AsString;
      AXml.Add(GetIndent(ANode.Level) + ANode.OpenTag + AValue + ANode.CloseTag);
    end else
    begin
      AXml.Add(GetIndent(ANode.Level)+ANode.OpenTag);
      for ICount := 0 to ANode.FChildren.Count-1 do
        NodeToStringList(AXml, ANode.Children.Node[ICount], AReplaceChars);
      AXml.Add(GetIndent(ANode.Level)+ANode.CloseTag);
    end;
  end;

var
  ICount: integer;
begin
  FStrings.Clear;
  if FNodes.Count = 0 then Exit;
  if FIncludeHeader then
    FStrings.Add(Format(XML_SPECIFICATION, [GetEncodingStr]));
  for ICount := 0 to FNodes.Count-1 do
    NodeToStringList(FStrings, FNodes.Node[ICount], ReplaceEscapeChars);
  Result := FStrings.Text;
  if not FLineBreaks then
    Result := StringReplace(Result, cEOL, '', [rfReplaceAll]);
end;

function TXml.GetXmlText: string;
begin
  Result := GetText(False);
end;

class function TXml.IsXML(const AText: string; AIncludeHeader: Boolean): Boolean;
const
  cMaxBOMSize = 4;
var
  ACursor: Integer;
begin
  Result := False;
  ACursor := 1;
  // skip BOM if present
  while (ACursor < Length(AText)) and (AText[ACursor] <> '<') and ((ACursor <= cMaxBOMSize) or CharInSetEx(AText[ACursor], cWordBreaks + cLineBreaks)) do
    Inc(ACursor);
  if (ACursor < Length(AText)) and (AText[ACursor] = '<') then
  begin
    if AIncludeHeader then
      Result := Pos(Copy(XML_SPECIFICATION, 1, 5), AText) = 1
    else
    begin
      // check last non white space character, must be '>'
      ACursor := Length(AText);
      while (ACursor > 1) and CharInSetEx(AText[ACursor], cWordBreaks + cLineBreaks) do
        Dec(ACursor);
      Result := AText[ACursor] = '>';
    end;
  end;
end;

procedure TXml.SetAsText(Value: string);

  function NonEmptyValue(const AValue: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to Length(AValue) do
      if not CharInSetEx(AValue[I], cLineBreaks + cWordBreaks) then
      begin
        Result := True;
        Exit;
      end;
  end;

var
  ACursor: integer;
  AText: string;
  ATag: string;
  AValue: string;
  ATags: string;
  N: TXmlNode;
  SingleTag: Boolean;
begin
  AText := Value;
  ACursor := 1;
  ATags := '';
  N := nil;
  // skip BOM if present
  while (ACursor < Length(Value)) and (Value[ACursor] <> '<') do
    Inc(ACursor);
  while ACursor < Length(Value) do
  begin
    AValue := '';
    if Value[ACursor] = '<' then
    begin
      // reading a tag
      ATag := '<';
      while (Value[ACursor] <> '>') and (ACursor < Length(Value)) do
      begin
        Inc(ACursor);
        ATag := ATag + Value[ACursor];
      end;
      if ATag[2] = '/' then
      begin
        if N <> nil then
          N := N.Parent
        else
          XmlError(sParseNoOpenTag);
      end else
      if ATag[2] <> '?' then
      begin
        SingleTag := ATag[Length(ATag) - 1] = '/';
        if SingleTag then
        begin
          Delete(ATag, Length(ATag) - 1, 1);
          if N = nil then
            Exit; // XML is empty
        end;
        if N = nil then
          N := Nodes.Add(ATag)
        else if SingleTag then
          N.Children.Add(ATag)
        else
          N := N.Children.Add(ATag);
      end;
    end
    else
    begin
      // reading a value...
      while (Value[ACursor] <> '<') and (ACursor < Length(Value)) do
      begin
        if ((Value[ACursor] <> #13) and (Value[ACursor] <> #10)) or FKeepLineBreaksInValues then
          AValue := AValue + Value[ACursor];
        Inc(ACursor);
      end;
      if N <> nil then
        N.FValue := AValue
      else if NonEmptyValue(AValue) then
        XmlError(sParseValueOutSideTag);
      Dec(ACursor);
    end;
    Inc(ACursor);
  end;
end;

procedure TXml.SetAbout(Value: string);
begin
  // does nothing... (only needed to display property in Object Inspector)
end;

procedure TXml.SetAutoIndent(const Value: Boolean);
begin
  FAutoIndent := Value;
end;

procedure TXml.LoadFromFile(AFileName: string);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(AFileName);
    LoadFromStream(MS);
  finally
    MS.Free;
  end;
end;

procedure TXml.LoadFromStream(Stream: TStream);
var
  S: AnsiString;
begin
  SetLength(S, Stream.Size);
  Stream.Read(S[1], Stream.Size);
  Text := UTF8ToString(S);
end;

procedure TXml.SaveToFile(AFilename: string);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    SaveToStream(MS);
    MS.SaveToFile(AFileName);
  finally
    MS.Free;
  end;
end;

procedure TXml.SaveToStream(Stream: TStream);
var
  S: AnsiString;
begin
  S := StringToUTF8(Text);
  Stream.Write(S[1], Length(S));
end;

end.
