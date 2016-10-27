{ @abstract(This unit contains main PWIG executive)
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(20 Oct 2016)

  Copyright © Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }

unit pwiggen;

{$MODE Delphi}

interface

uses
  Generics.Collections, KXml;

type
  // contains a subset of types supported by COM
  TPWIGBaseType = (
    btLongInt, // COM long
    btLongWord, // COM unsigned long
    btSmallInt, // COM short
    btWord, // COM unsigned short
    btInt64, // COM int64
    btUInt64, // COM unsigned int64
    btSingle, // COM single precision float
    btDouble, // COM double precision float
    btUnicodeString, //COM BSTR, otherwise UTF8 encoded string
    btRawByteString, // array of bytes, memory management not supported by COM!
    btCurrency, // COM currency
    btDateTime, //COM DATE
    btEnum, // custom enumerated type, TPWIGEnum
    btAlias, // custom alias type, TPWIGAlias
    btInterface // custom interface type TPWIGInterface
  );

  { TPWIGType }

  TPWIGType = class
  private
    FBaseType: TPWIGBaseType;
    FCustomTypeGUID: string;
    FCustomTypeName: string;
  public
    constructor Create; virtual;
    procedure Load(ANode: TXmlNode); virtual;
    procedure Save(ANode: TXmlNode); virtual;
    property BaseType: TPWIGBaseType read FBaseType write FBaseType;
    property CustomTypeGUID: string read FCustomTypeGUID write FCustomTypeGUID;
    property CustomTypeName: string read FCustomTypeName write FCustomTypeName;
  end;

  { TPWIGElement }

  TPWIGElement = class
  private
    FDescription: string;
    FGUID: string;
    FName: string;
    FVersion: string;
  public
    constructor Create; virtual;
    procedure Load(ANode: TXmlNode); virtual;
    procedure Save(ANode: TXmlNode); virtual;
    property Description: string read FDescription write FDescription;
    property GUID: string read FGUID write FGUID;
    property Name: string read FName write FName;
    property Version: string read FVersion write FVersion;
  end;

  { TPWIGElementList }

  TPWIGElementList<T: TPWIGElement> = class(TObjectList<T>)
  public
    procedure Load(ANode: TXmlNode; const ASubnodeName: string); virtual;
    procedure Save(ANode: TXmlNode; const ASubnodeName: string); virtual;
    function Find(const AGUID: string; const AName: string = ''): T;
    function FindName(const AGUID: string; const AName: string = ''): string;
  end;

  { TPWIGDictionary }

  TPWIGDictionary<TKey, TValue> = class(TDictionary<TKey, TValue>)
  public
    procedure Load(ANode: TXmlNode; const ASubnodeName, AKeyNode, AValueNode: string); virtual;
    procedure Save(ANode: TXmlNode; const ASubnodeName, AKeyNode, AValueNode: string); virtual;
  end;

  TPWIGParamDirection = (
    pdIn,
    pdOut,
    pdInOut
  );

  { TPWIGParam }

  TPWIGParam = class(TPWIGElement)
  private
    FParamType: TPWIGType;
    FFlagOutput: Boolean;
    FFlagInput: Boolean;
    FFlagRetVal: Boolean;
    function GetParamDirection: TPWIGParamDirection;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Load(ANode: TXmlNode); override;
    procedure Save(ANode: TXmlNode); override;
    property FlagInput: Boolean read FFlagInput write FFlagInput;
    property FlagOutput: Boolean read FFlagOutput write FFlagOutput;
    property ParamDirection: TPWIGParamDirection read GetParamDirection;
    property ParamType: TPWIGType read FParamType;
    property FlagRetVal: Boolean read FFlagRetVal write FFlagRetVal;
  end;

  { TPWIGParamList }

  TPWIGParamList = class(TPWIGElementList<TPWIGParam>)
    function FindRetVal: TPWIGParam; virtual;
  end;

  TPWIGCallingConv = (
    ccStdCall,
    ccCDecl
  );

  { TPWIGMethod }

  TPWIGMethod = class(TPWIGElement)
  private
    FParams: TPWIGParamList;
    FCallingConv: TPWIGCallingConv;
    FId: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Load(ANode: TXmlNode); override;
    procedure Save(ANode: TXmlNode); override;
    property CallingConv: TPWIGCallingConv read FCallingConv write FCallingConv;
    property Id: Integer read FId write FId;
    property Params: TPWIGParamList read FParams;
  end;

  TPWIGMethodList = TPWIGElementList<TPWIGMethod>;

  TPWIGPropertyType = (
    ptReadWrite,
    ptReadOnly,
    ptWriteOnly
  );

  { TPWIGProperty }

  TPWIGProperty = class(TPWIGMethod)
  private
    FPropertyType: TPWIGPropertyType;
  public
    constructor Create; override;
    procedure Load(ANode: TXmlNode); override;
    procedure Save(ANode: TXmlNode); override;
    property PropertyType: TPWIGPropertyType read FPropertyType write FPropertyType;
  end;

  TPWIGPropertyList = TPWIGElementList<TPWIGProperty>;

  { TPWIGEnumElement }

  TPWIGEnumElement = class(TPWIGElement)
  private
    FValue: Integer;
  public
    constructor Create; override;
    procedure Load(ANode: TXmlNode); override;
    procedure Save(ANode: TXmlNode); override;
    property Value: Integer read FValue write FValue;
  end;

  TPWIGEnumElements = TPWIGElementList<TPWIGEnumElement>;

  { TPWIGEnum }

  TPWIGEnum = class(TPWIGElement)
  private
    FElements: TPWIGEnumElements;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Load(ANode: TXmlNode); override;
    procedure Save(ANode: TXmlNode); override;
    property Elements: TPWIGEnumElements read FElements;
  end;

  TPWIGEnumList = TPWIGElementList<TPWIGEnum>;

  { TPWIGAlias }

  TPWIGAlias = class(TPWIGElement)
  private
    FAliasedType: TPWIGType;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Load(ANode: TXmlNode); override;
    procedure Save(ANode: TXmlNode); override;
    property AliasedType: TPWIGType read FAliasedType write FAliasedType;
  end;

  TPWIGAliasList = TPWIGElementList<TPWIGAlias>;

  { TPWIGInterface }

  TPWIGInterface = class(TPWIGElement)
  private
    FBaseInterface: string;
    FFlagDual: Boolean;
    FFlagOleAutomation: Boolean;
    FFlagDispEvents: Boolean; // marks dispinterface in COM
    FMethods: TPWIGMethodList;
    FProperties: TPWIGPropertyList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Load(ANode: TXmlNode); override;
    procedure Save(ANode: TXmlNode); override;
    property BaseInterface: string read FBaseInterface write FBaseInterface;
    property FlagDual: Boolean read FFlagDual write FFlagDual;
    property FlagOleAutomation: Boolean read FFlagOleAutomation write FFlagOleAutomation;
    property FlagDispEvents: Boolean read FFlagDispEvents write FFlagDispEvents;
    property Methods: TPWIGMethodList read FMethods;
    property Properties: TPWIGPropertyList read FProperties;
  end;

  TPWIGInterfaceList = TPWIGElementList<TPWIGInterface>;

  { TPWIGInterfaceRef }

  TPWIGInterfaceRef = class(TPWIGElement)
  private
    FFlagDefault: Boolean;
    FFlagSource: Boolean;
    FRefGUID: string;
    FRefName: string;
  public
    constructor Create; override;
    procedure Load(ANode: TXmlNode); override;
    procedure Save(ANode: TXmlNode); override;
    property FlagDefault: Boolean read FFlagDefault write FFlagDefault;
    property FlagSource: Boolean read FFlagSource write FFlagSource;
    property RefGUID: string read FRefGUID write FRefGUID;
    property RefName: string read FRefName write FRefName;
  end;

  TPWIGInterfaceRefList = TPWIGElementList<TPWIGInterfaceRef>;

  { TPWIGClass }

  TPWIGClass = class(TPWIGElement)
  private
    FInterfaceRefs: TPWIGInterfaceRefList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Load(ANode: TXmlNode); override;
    procedure Save(ANode: TXmlNode); override;
    property InterfaceRefs: TPWIGInterfaceRefList read FInterfaceRefs;
  end;

  TPWIGClassList = TPWIGElementList<TPWIGClass>;

  TPWIG = class;

  { TPWIGGenerator }

  TPWIGGenerator = class
  protected
    FPWIG: TPWIG;
  public
    constructor Create(AOwner: TPWIG); virtual;
    procedure SaveToFile(const AFileName: string); virtual; abstract;
    procedure SaveToFiles(const ACalleeFileName, ACallerFileName: string); virtual; abstract;
  end;

  { TPWIG }

  TPWIG = class(TPWIGElement)
  private
    FAliases: TPWIGAliasList;
    FClasses: TPWIGClassList;
    FEnums: TPWIGEnumList;
    FInputFile: string;
    FInterfaces: TPWIGInterfaceList;
  protected
    property InputFile: string read FInputFile;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ReadParams: Boolean;
    function LoadFromFile(const AFileName: string): Boolean;
    function SaveToFile(const AFileName: string): Boolean;
    procedure Generate;
    procedure GeneratePascalWrappers(const ACalleeFileName, ACallerFileName: string);
    procedure GenerateRIDL(const AFileName: string);
    procedure PrintHelp;
    property Aliases: TPWIGAliasList read FAliases;
    property Classes: TPWIGClassList read FClasses;
    property Enums: TPWIGEnumList read FEnums;
    property Interfaces: TPWIGInterfaceList read FInterfaces;
  end;

function GUIDToXMLGUID(const AID: string): string;
function XMLGUIDToGUID(const AID: string): string;
function XMLGUIDToGUIDNoCurlyBraces(const AID: string): string;

implementation

uses
  SysUtils, TypInfo,
  PWIG_RIDL, PWIG_Pascal;

const
  nnPWIGConfig = 'pwig_configuration';

  nnPWIGType = 'pwig_datatype';
  nnPWIGTypeBaseType = 'pwig_datatype_basetype';
  nnPWIGTypeCustomGUID = 'pwig_datatype_customguid';
  nnPWIGTypeCustomName = 'pwig_datatype_customname';

  nnPWIGElementGUID = 'pwig_element_guid';
  nnPWIGElementName = 'pwig_element_name';
  nnPWIGElementDesc = 'pwig_element_description';
  nnPWIGElementVersion = 'pwig_element_version';

  nnPWIGParamInput = 'pwig_param_input';
  nnPWIGParamOutput = 'pwig_param_output';
  nnPWIGParamRetval = 'pwig_param_retval';

  nnPWIGAlias = 'pwig_alias';

  nnPWIGEnum = 'pwig_enumeration';
  nnPWIGEnumElement = 'pwig_enum_element';
  nnPWIGEnumElementValue = 'pwig_enum_element_value';

  nnPWIGInterface = 'pwig_interface';
  nnPWIGInterfaceBaseIntf = 'pwig_interface_baseintf';
  nnPWIGInterfaceDual = 'pwig_interface_dual';
  nnPWIGInterfaceOleAutomation = 'pwig_interface_oleautomation';
  nnPWIGInterfaceDispEvents = 'pwig_interface_dispevents';

  nnPWIGMethod = 'pwig_method';
  nnPWIGMethodCallingConv= 'pwig_method_calling_conv';
  nnPWIGMethodId = 'pwig_method_id';
  nnPWIGMethodParam = 'pwig_method_param';

  nnPWIGProperty = 'pwig_property';
  nnPWIGPropertyType = 'pwig_property_type';

  nnPWIGInterfaceRef = 'pwig_interfaceref';
  nnPWIGInterfaceRefDefault = 'pwig_interfaceref_default';
  nnPWIGInterfaceRefSource = 'pwig_interfaceref_source';
  nnPWIGInterfaceRefGUID = 'pwig_interfaceref_guid';
  nnPWIGInterfaceRefName = 'pwig_interfaceref_name';

  nnPWIGClass = 'pwig_class';

function GUIDToXMLGUID(const AID: string): string;
begin
  if AID <> '' then
  begin
    Result := UpperCase(AID);
    if Result[1] = '{' then
      Delete(Result, 1, 1);
    if Result[Length(Result)] = '}' then
      Delete(Result, Length(Result), 1);
  end else
    Result := '';
end;

function XMLGUIDToGUID(const AID: string): string;
begin
  if AID <> '' then
  begin
    Result := UpperCase(AID);
    if Result[1] = '_' then
      Result[1] := '{'
    else
      Result := '{' + Result;
    if Result[Length(Result)] = '_' then
      Result[Length(Result)] := '}'
    else
      Result := Result + '}';
  end;
end;

function XMLGUIDToGUIDNoCurlyBraces(const AID: string): string;
begin
  if AID <> '' then
  begin
    Result := UpperCase(AID);
    if Result[1] = '_' then
      Delete(Result, 1, 1);
    if Result[Length(Result)] = '_' then
      Delete(Result, Length(Result), 1);
  end;
end;

function UniqID: string;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToXMLGUID(GUIDToString(GUID));
end;

// following conversion routines could be generic as well,
// but at the moment of writing some bugs in FPC didn't allow that

function BaseTypeToString(AType: TPWIGBaseType): string;
var
  S: string;
begin
  S := GetEnumName(TypeInfo(TPWIGBaseType), Ord(AType));
  Delete(S, 1, 2); // delete 'bt' prefix
  Result := S;
end;

function StringToBaseType(const AText: string): TPWIGBaseType;
var
  Value: TPWIGBaseType;
begin
  Value := TPWIGBaseType(GetEnumValue(TypeInfo(TPWIGBaseType), 'bt' + AText));
  if (Value >= Low(TPWIGBaseType)) and (Value <= High(TPWIGBaseType)) then
    Result := Value
  else
    Result := btLongInt;
end;

function CallingConvToString(AType: TPWIGCallingConv): string;
var
  S: string;
begin
  S := GetEnumName(TypeInfo(TPWIGCallingConv), Ord(AType));
  Delete(S, 1, 2); // delete 'cc' prefix
  Result := S;
end;

function StringToCallingConv(const AText: string): TPWIGCallingConv;
var
  Value: TPWIGCallingConv;
begin
  Value := TPWIGCallingConv(GetEnumValue(TypeInfo(TPWIGCallingConv), 'cc' + AText));
  if (Value >= Low(TPWIGCallingConv)) and (Value <= High(TPWIGCallingConv)) then
    Result := Value
  else
    Result := ccStdCall;
end;

function PropertyTypeToString(AType: TPWIGPropertyType): string;
var
  S: string;
begin
  S := GetEnumName(TypeInfo(TPWIGPropertyType), Ord(AType));
  Delete(S, 1, 2); // delete 'pt' prefix
  Result := S;
end;

function StringToPropertyType(const AText: string): TPWIGPropertyType;
var
  Value: TPWIGPropertyType;
begin
  Value := TPWIGPropertyType(GetEnumValue(TypeInfo(TPWIGPropertyType), 'pt' + AText));
  if (Value >= Low(TPWIGPropertyType)) and (Value <= High(TPWIGPropertyType)) then
    Result := Value
  else
    Result := ptReadWrite;
end;

{ TPWIGGenerator }

constructor TPWIGGenerator.Create(AOwner: TPWIG);
begin
  FPWIG := AOwner;
end;

{ TPWIGType }

constructor TPWIGType.Create;
begin
  FBaseType := btLongInt;
  FCustomTypeGUID := '';
  FCUstomTypeName := '';
end;

procedure TPWIGType.Load(ANode: TXmlNode);
var
  N: TXmlNode;
begin
  if ANode <> nil then
  begin
    N := ANode.Children.NodeByName[nnPWIGType];
    if N <> nil then
    begin
      FBaseType := StringToBaseType(N.ChildAsString(nnPWIGTypeBaseType, ''));
      FCustomTypeGUID := N.ChildAsString(nnPWIGTypeCustomGUID, FCustomTypeGUID);
      FCUstomTypeName := N.ChildAsString(nnPWIGTypeCustomName, FCUstomTypeName);
    end;
  end;
end;

procedure TPWIGType.Save(ANode: TXmlNode);
var
  N: TXmlNode;
begin
  if ANode <> nil then
  begin
    N := ANode.Children.Add(nnPWIGType);
    if N <> nil then
    begin
      N.Children.Add(nnPWIGTypeBaseType).AsString := BaseTypeToString(FBaseType);
      N.Children.Add(nnPWIGTypeCustomGUID).AsString := FCustomTypeGUID;
      N.Children.Add(nnPWIGTypeCustomName).AsString := FCUstomTypeName;
    end;
  end;
end;

{ TPWIGElement }

constructor TPWIGElement.Create;
begin
  FGUID := UniqID;
  FDescription := '';
  FName := '';
  FVersion := '';
end;

procedure TPWIGElement.Load(ANode: TXmlNode);
begin
  if ANode <> nil then
  begin
    FDescription := ANode.ChildAsString(nnPWIGElementDesc, FDescription);
    FGUID := ANode.ChildAsString(nnPWIGElementGUID, FGUID);
    FName := ANode.ChildAsString(nnPWIGElementName, FName);
    FVersion := ANode.ChildAsString(nnPWIGElementVersion, FVersion);
  end;
end;

procedure TPWIGElement.Save(ANode: TXmlNode);
begin
  if ANode <> nil then
  begin
    ANode.Children.Add(nnPWIGElementDesc).AsString := FDescription;
    ANode.Children.Add(nnPWIGElementGUID).AsString := FGUID;
    ANode.Children.Add(nnPWIGElementName).AsString := FName;
    ANode.Children.Add(nnPWIGElementVersion).AsString := FVersion;
  end;
end;

{ TPWIGElementList<T> }

procedure TPWIGElementList<T>.Load(ANode: TXmlNode; const ASubnodeName: string);
var
  I: Integer;
  N, NList: TXmlNode;
  Elem: T;
begin
  if ANode <> nil then
  begin
    NList := ANode.Children.NodeByName[ASubnodeName + '_list'];
    if NList <> nil then
    begin
      Clear;
      for I := 0 to NList.Children.Count - 1 do
      begin
        N := NList.Children[I];
        if N.Name = ASubnodeName then
        begin
          Elem := T.Create;
          Elem.Load(N);
          Add(Elem);
        end;
      end;
    end;
  end;
end;

procedure TPWIGElementList<T>.Save(ANode: TXmlNode; const ASubnodeName: string);
var
  N, NList: TXmlNode;
  Elem: T;
begin
  if ANode <> nil then
  begin
    NList := ANode.Children.Add(ASubnodeName + '_list');
    if NList <> nil then
      for Elem in Self do
      begin
        N := NList.Children.Add(ASubnodeName);
        if N <> nil then
          Elem.Save(N);
      end;
  end;
end;

function TPWIGElementList<T>.Find(const AGUID: string; const AName: string = ''): T;
var
  Elem: T;
begin
  Result := nil;
  for Elem in Self do
    if ((AGUID = '') or (Elem.GUID = AGUID)) and ((AName = '') or (Elem.Name = AName)) then
      Exit(Elem);
end;

function TPWIGElementList<T>.FindName(const AGUID: string; const AName: string = ''): string;
var
  Elem: T;
begin
  Elem := Find(AGUID, AName);
  if Elem <> nil then
    Result := Elem.Name
  else
    Result := '/*unknown_type*/';
end;


{ TPWIGDictionary }

procedure TPWIGDictionary<TKey, TValue>.Load(ANode: TXmlNode; const ASubnodeName, AKeyNode, AValueNode: string);
var
  I: Integer;
  N, NList: TXmlNode;
  EKey: TKey;
  EName: TValue;
begin
  if ANode <> nil then
  begin
    NList := ANode.Children.NodeByName[ASubnodeName + '_list'];
    if NList <> nil then
    begin
      Clear;
      for I := 0 to NList.Children.Count - 1 do
      begin
        N := NList.Children[I];
        if N.Name = ASubnodeName then
        begin
          EKey := TKey(N.ChildAsInteger(AKeyNode, 0));
          EName := TValue(N.ChildAsString(AValueNode, ''));
          Add(EKey, EName);
        end;
      end;
    end;
  end;
end;

procedure TPWIGDictionary<TKey, TValue>.Save(ANode: TXmlNode; const ASubnodeName, AKeyNode, AValueNode: string);
var
  N, NList: TXmlNode;
  Pair: TPair<TKey, TValue>;
begin
  inherited;
  if ANode <> nil then
  begin
    NList := ANode.Children.Add(ASubnodeName + '_list');
    if NList <> nil then
      for Pair in Self do
      begin
        N := NList.Children.Add(ASubnodeName);
        if N <> nil then
        begin
          N.Children.Add(AKeyNode).AsInteger := Integer(Pair.Key);
          N.Children.Add(AValueNode).AsString := string(Pair.Value);
        end;
      end;
  end;
end;

{ TPWIGParam }

function TPWIGParam.GetParamDirection: TPWIGParamDirection;
begin
  if FlagInput and FlagOutput then
    Result := pdInOut
  else if FlagOutput then
    Result := pdOut
  else
    Result := pdIn;
end;

constructor TPWIGParam.Create;
begin
  inherited;
  FFlagInput := False;
  FFlagOutput := False;
  FParamType := TPWIGType.Create;
  FFlagRetVal := False;
end;

destructor TPWIGParam.Destroy;
begin
  FParamType.Free;
  inherited;
end;

procedure TPWIGParam.Load(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    FFlagInput := ANode.ChildAsBoolean(nnPWIGParamInput, FFlagInput);
    FFlagOutput := ANode.ChildAsBoolean(nnPWIGParamOutput, FFlagOutput);
    FFlagRetVal := ANode.ChildAsBoolean(nnPWIGParamRetVal, FFlagRetVal);
    FParamType.Load(ANode);
  end;
end;

procedure TPWIGParam.Save(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    ANode.Children.Add(nnPWIGParamInput).AsBoolean := FFlagInput;
    ANode.Children.Add(nnPWIGParamOutput).AsBoolean := FFlagOutput;
    ANode.Children.Add(nnPWIGParamRetval).AsBoolean := FFlagRetVal;
    FParamType.Save(ANode);
  end;
end;

{ TPWIGParamList }

function TPWIGParamList.FindRetVal: TPWIGParam;
var
  Param: TPWIGParam;
begin
  Result := nil;
  for Param in Self do
    if Param.FFlagRetVal then
      Exit(Param);
end;

{ TPWIGMethod }

constructor TPWIGMethod.Create;
var
  GUID: TGUID;
begin
  inherited;
  GUID := StringToGUID(XMLGuidToGuid(Self.GUID));
  FCallingConv := ccStdCall;
  FId := GUID.D1; // start with some kind of randomization ;
  FParams := TPWIGParamList.Create;
end;

destructor TPWIGMethod.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TPWIGMethod.Load(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    FCallingConv := StringToCallingConv(ANode.ChildAsString(nnPWIGMethodCallingConv, ''));
    FId := ANode.ChildAsInteger(nnPWIGMethodId, FId);
    FParams.Load(ANode, nnPWIGMethodParam);
  end;
end;

procedure TPWIGMethod.Save(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    ANode.Children.Add(nnPWIGMethodCallingConv).AsString := CallingConvToString(FCallingConv);
    ANode.Children.Add(nnPWIGMethodId).AsHexInt := FId;
    FParams.Save(ANode, nnPWIGMethodParam);
  end;
end;

{ TPWIGProperty }

constructor TPWIGProperty.Create;
begin
  inherited;
  FPropertyType := ptReadWrite;
end;

procedure TPWIGProperty.Load(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    FPropertyType := StringToPropertyType(ANode.ChildAsString(nnPWIGPropertyType, ''));
  end;
end;

procedure TPWIGProperty.Save(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    ANode.Children.Add(nnPWIGPropertyType).AsString := PropertyTypeToString(FPropertyType);
  end;
end;

{ TPWIGEnumElement }

constructor TPWIGEnumElement.Create;
begin
  inherited;
  FValue := 0;
end;

procedure TPWIGEnumElement.Load(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    FValue := ANode.ChildAsInteger(nnPWIGEnumElementValue, FValue);
  end;
end;

procedure TPWIGEnumElement.Save(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    ANode.Children.Add(nnPWIGEnumElementValue).AsInteger := FValue;
  end;
end;

{ TPWIGEnum }

constructor TPWIGEnum.Create;
begin
  inherited;
  FElements := TPWIGEnumElements.Create;
end;

destructor TPWIGEnum.Destroy;
begin
  FElements.Free;
  inherited;
end;

procedure TPWIGEnum.Load(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    FElements.Load(ANode, nnPWIGEnumElement);
  end;
end;

procedure TPWIGEnum.Save(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    FElements.Save(ANode, nnPWIGEnumElement);
  end;
end;

{ TPWIGAlias }

constructor TPWIGAlias.Create;
begin
  inherited;
  FAliasedType := TPWIGType.Create;
end;

destructor TPWIGAlias.Destroy;
begin
  FAliasedType.Free;
  inherited;
end;

procedure TPWIGAlias.Load(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    FAliasedType.Load(ANode);
  end;
end;

procedure TPWIGAlias.Save(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    FAliasedType.Save(ANode);
  end;
end;

{ TPWIGInterface }

constructor TPWIGInterface.Create;
begin
  inherited;
  FBaseInterface := 'IDispatch';
  FFlagDual := False;
  FFlagOleAutomation := False;
  FFlagDispEvents := False;
  FMethods := TPWIGMethodList.Create;
  FProperties := TPWIGPropertyList.Create;
end;

destructor TPWIGInterface.Destroy;
begin
  FMethods.Free;
  FProperties.Free;
  inherited;
end;

procedure TPWIGInterface.Load(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    FBaseInterface := ANode.ChildAsString(nnPWIGInterfaceBaseIntf, FBaseInterface);
    FFlagDual := ANode.ChildAsBoolean(nnPWIGInterfaceDual, FFlagDual);
    FFlagOleAutomation := ANode.ChildAsBoolean(nnPWIGInterfaceOleAutomation, FFlagOleAutomation);
    FFlagDispEvents := ANode.ChildAsBoolean(nnPWIGInterfaceDispEvents, FFlagDispEvents);
    FMethods.Load(ANode, nnPWIGMethod);
    FProperties.Load(ANode, nnPWIGProperty);
  end;
end;

procedure TPWIGInterface.Save(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    ANode.Children.Add(nnPWIGInterfaceBaseIntf).AsString := FBaseInterface;
    ANode.Children.Add(nnPWIGInterfaceDual).AsBoolean := FFlagDual;
    ANode.Children.Add(nnPWIGInterfaceOleAutomation).AsBoolean := FFlagOleAutomation;
    ANode.Children.Add(nnPWIGInterfaceDispEvents).AsBoolean := FFlagDispEvents;
    FMethods.Save(ANode, nnPWIGMethod);
    FProperties.Save(ANode, nnPWIGProperty);
  end;
end;

{ TPWIGInterfaceRef }

constructor TPWIGInterfaceRef.Create;
begin
  inherited;
  FlagDefault := False;
  FlagSource := False;
  FRefGUID := '';
  FRefName := '';
end;

procedure TPWIGInterfaceRef.Load(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    FFlagDefault := ANode.ChildAsBoolean(nnPWIGInterfaceRefDefault, FFlagDefault);
    FFlagSource := ANode.ChildAsBoolean(nnPWIGInterfaceRefSource, FFlagSource);
    FRefGUID := ANode.ChildAsString(nnPWIGInterfaceRefGUID, FRefGUID);
    FRefName := ANode.ChildAsString(nnPWIGInterfaceRefName, FRefName);
  end;
end;

procedure TPWIGInterfaceRef.Save(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    ANode.Children.Add(nnPWIGInterfaceRefDefault).AsBoolean := FFlagDefault;
    ANode.Children.Add(nnPWIGInterfaceRefSource).AsBoolean := FFlagSource;
    ANode.Children.Add(nnPWIGInterfaceRefGUID).AsString := FRefGUID;
    ANode.Children.Add(nnPWIGInterfaceRefName).AsString := FRefName;
  end;
end;

{ TPWIGClass }

constructor TPWIGClass.Create;
begin
  inherited;
  FInterfaceRefs := TPWIGInterfaceRefList.Create;
end;

destructor TPWIGClass.Destroy;
begin
  FInterfaceRefs.Free;
  inherited;
end;

procedure TPWIGClass.Load(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    FInterfaceRefs.Load(ANode, nnPWIGInterfaceRef);
  end;
end;

procedure TPWIGClass.Save(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    FInterfaceRefs.Save(ANode, nnPWIGInterfaceRef);
  end;
end;

{ TPWIG }

constructor TPWIG.Create;
begin
  inherited;
  FAliases := TPWIGAliasList.Create;
  FClasses := TPWIGClassList.Create;
  FEnums := TPWIGEnumList.Create;
  FInputFile := '';
  FInterfaces := TPWIGInterfaceList.Create;
end;

destructor TPWIG.Destroy;
begin
  FAliases.Free;
  FClasses.Free;
  FEnums.Free;
  FInterfaces.Free;
  inherited;
end;

procedure TPWIG.Generate;
begin
end;

procedure TPWIG.GenerateRIDL(const AFileName: string);
var
  Gen: TPWIGGenRIDL;
begin
  Gen := TPWIGGenRIDL.Create(Self);
  try
    Gen.SaveToFile(AFileName);
  finally
    Gen.Free;
  end;
end;

procedure TPWIG.GeneratePascalWrappers(const ACalleeFileName, ACallerFileName: string);
var
  Gen: TPWIGGenPascal;
begin
  Gen := TPWIGGenPascal.Create(Self);
  try
    Gen.SaveToFiles(ACalleeFileName, ACallerFileName);
  finally
    Gen.Free;
  end;
end;

function TPWIG.LoadFromFile(const AFileName: string): Boolean;
var
  Xml: TXml;
  N: TXmlNode;
begin
  Result := False;
  if FileExists(AFileName) then
  begin
    try
      Xml := TXml.Create(nil);
      try
        Xml.LoadFromFile(AFileName);
        N := Xml.Nodes.NodeByName[nnPWIGConfig];
        if N <> nil then
        begin
          inherited Load(N);
          FAliases.Load(N, nnPWIGAlias);
          FEnums.Load(N, nnPWIGEnum);
          FInterfaces.Load(N, nnPWIGInterface);
          FClasses.Load(N, nnPWIGClass);
        end;
        Result := True;
      finally
        Xml.Free;
      end;
    except
    end;
  end;
end;

procedure TPWIG.PrintHelp;
begin

end;

function TPWIG.ReadParams: Boolean;
begin
  Result := True;
end;


function TPWIG.SaveToFile(const AFileName: string): Boolean;
var
  Xml: TXml;
  N: TXmlNode;
begin
  Result := False;
  try
    Xml := TXml.Create(nil);
    try
      N := Xml.Nodes.Add(nnPWIGConfig);
      if N <> nil then
      begin
        inherited Save(N);
        FAliases.Save(N, nnPWIGAlias);
        FEnums.Save(N, nnPWIGEnum);
        FInterfaces.Save(N, nnPWIGInterface);
        FClasses.Save(N, nnPWIGClass);
      end;
      Xml.SaveToFile(AFileName);
      Result := True;
    finally
      Xml.Free;
    end;
  except
  end;
end;

end.
