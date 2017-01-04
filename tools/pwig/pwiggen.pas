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

{$mode delphi}

interface

uses
  Generics.Collections, KXml;

type
  // forward declarations
  TPWIG = class;

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

  // available calling conventions for library calls
  TPWIGCallingConv = (
    ccNone,
    ccCDecl,
    ccSysCall,
    ccPascal,
    ccStdCall,
    ccFastCall,
    ccSafeCall
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


  // supported generators
  TPWIGGeneratorType = (
    gtCpp, // C++ (Visual Studio etc.)
    gtCSharp, // CSharp (Visual Studio etc.)
    gtPascal, // Delphi or Lazarus
    gtRIDL // Delphi RIDL
  );

  { TPWIGGenerator }

  TPWIGGenerator = class
  protected
    FPWIG: TPWIG;
  public
    constructor Create(AOwner: TPWIG); virtual;
    procedure SaveCallerFiles(const AFileName: string); virtual; abstract;
    procedure SaveCalleeFiles(const AFileName: string); virtual; abstract;
  end;

  { TPWIGGeneratorConfig }

  TPWIGGeneratorConfig = class(TPWIGElement)
  private
    FGenType: TPWIGGeneratorType;
    FGenPath: string;
  public
    constructor Create; override;
    procedure Load(ANode: TXmlNode); override;
    procedure Save(ANode: TXmlNode); override;
    property GenType: TPWIGGeneratorType read FGenType write FGenType;
    property GenPath: string read FGenPath write FGenPath;
  end;

  TPWIGGeneratorConfigList = TPWIGElementList<TPWIGGeneratorConfig>;

  { TPWIG }

  TPWIG = class(TPWIGElement)
  private
    FAliases: TPWIGAliasList;
    FClasses: TPWIGClassList;
    FEnums: TPWIGEnumList;
    FGlobalCallingConv: TPWIGCallingConv;
    FInputFile: string;
    FInterfaces: TPWIGInterfaceList;
    FCalleeConfigs: TPWIGGeneratorConfigList;
    FCallerConfigs: TPWIGGeneratorConfigList;
    FStaticLibraryName: string;
  protected
    property InputFile: string read FInputFile;
  public
    constructor Create; override;
    destructor Destroy; override;
    function FindDefaultIntf(AClass: TPWIGClass; AEvents: Boolean): TPWIGInterface;
    procedure FixReferences;
    function ReadParams: Boolean;
    function LoadFromFile(const AFileName: string): Boolean;
    function SaveToFile(const AFileName: string): Boolean;
    procedure Generate;
    procedure PrintHelp;
    procedure PrintCopyright;
    property Aliases: TPWIGAliasList read FAliases;
    property Classes: TPWIGClassList read FClasses;
    property Enums: TPWIGEnumList read FEnums;
    property Interfaces: TPWIGInterfaceList read FInterfaces;
    property GlobalCallingConv: TPWIGCallingConv read FGlobalCallingConv write FGlobalCallingConv;
    property CalleeConfigs: TPWIGGeneratorConfigList read FCalleeConfigs;
    property CallerConfigs: TPWIGGeneratorConfigList read FCallerConfigs;
    property StaticLibraryName: string read FStaticLibraryName;
  end;

function GUIDToXMLGUID(const AID: string): string;
function XMLGUIDToGUID(const AID: string): string;
function XMLGUIDToGUIDNoCurlyBraces(const AID: string): string;

implementation

uses
  Math, SysUtils, TypInfo,
  KFunctions,
  PWIG_Cpp, PWIG_CSharp, PWIG_Pascal, PWIG_RIDL;

const
  nnPWIGConfig = 'pwig_configuration';
  nnPWIGGlobalCallingConv = 'pwig_global_calling_conv';
  nnPWIGStaticLibName = 'pwig_static_library_name';

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

  nnPWIGGenCalleeConfig = 'pwig_callee_configuration';
  nnPWIGGenCallerConfig = 'pwig_caller_configuration';
  nnPWIGGenConfigType = 'pwig_generator_type';
  nnPWIGGenConfigPath = 'pwig_generator_path';

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
    Result := ccNone;
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

function GenTypeToString(AType: TPWIGGeneratorType): string;
var
  S: string;
begin
  S := GetEnumName(TypeInfo(TPWIGGeneratorType), Ord(AType));
  Delete(S, 1, 2); // delete 'gt' prefix
  Result := S;
end;

function StringToGenType(const AText: string): TPWIGGeneratorType;
var
  Value: TPWIGGeneratorType;
begin
  Value := TPWIGGeneratorType(GetEnumValue(TypeInfo(TPWIGGeneratorType), 'gt' + AText));
  if (Value >= Low(TPWIGGeneratorType)) and (Value <= High(TPWIGGeneratorType)) then
    Result := Value
  else
    Result := gtPascal;
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
  Result := T(nil);
  for Elem in Self do
    if ((AGUID = '') or (Elem.GUID = AGUID)) and ((AName = '') or (Elem.Name = AName)) then
      Exit(Elem);
end;

function TPWIGElementList<T>.FindName(const AGUID: string; const AName: string = ''): string;
var
  Elem: T;
begin
  Elem := Find(AGUID, AName);
  if Elem <> T(nil) then
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
  FCallingConv := ccNone; // use global settings by default
  FId := GUID.D1; // start with some kind of randomization
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

{ TPWIGGenerator }

constructor TPWIGGenerator.Create(AOwner: TPWIG);
begin
  FPWIG := AOwner;
end;


{ TPWIGGeneratorConfig }

constructor TPWIGGeneratorConfig.Create;
begin
  inherited;
  FGenType := gtPascal;
  FGenPath := '';
end;

procedure TPWIGGeneratorConfig.Load(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    FGenType := StringToGenType(ANode.ChildAsString(nnPWIGGenConfigType, ''));
    FGenPath := ANode.ChildAsString(nnPWIGGenConfigPath, FGenPath);
  end;
end;

procedure TPWIGGeneratorConfig.Save(ANode: TXmlNode);
begin
  inherited;
  if ANode <> nil then
  begin
    ANode.Children.Add(nnPWIGGenConfigType).AsString := GenTypeToString(FGenType);
    ANode.Children.Add(nnPWIGGenConfigPath).AsString := FGenPath;
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
  FCalleeConfigs := TPWIGGeneratorConfigList.Create;
  FCallerConfigs := TPWIGGeneratorConfigList.Create;
end;

destructor TPWIG.Destroy;
begin
  FAliases.Free;
  FClasses.Free;
  FEnums.Free;
  FInterfaces.Free;
  FCalleeConfigs.Free;
  FCallerConfigs.Free;
  inherited;
end;

function TPWIG.FindDefaultIntf(AClass: TPWIGClass; AEvents: Boolean): TPWIGInterface;
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
begin
  Result := nil;
  Assert(AClass <> nil);
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FInterfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and Ref.FlagDefault and not (LIntf.FlagDispEvents xor AEvents) then
      Exit(LIntf);
  end;
end;

procedure TPWIG.FixReferences;

  procedure FixInterfaceMethodIds;

    function FindID(AIntf: TPWIGInterface; AMethod: TPWIGMethod): Boolean;
    var
      Method: TPWIGMethod;
    begin
      Result := False;
      for Method in AIntf.Methods do
      begin
        if (Method <> AMethod) and (Method.Id = AMethod.Id) then
          Exit(True);
      end;
      for Method in AIntf.Properties do
      begin
        if (Method <> AMethod) and (Method.Id = AMethod.Id) then
          Exit(True);
      end;
    end;

    function MaxId(AIntf: TPWIGInterface): LongInt;
    var
      Method: TPWIGMethod;
    begin
      Result := -MaxInt;
      for Method in AIntf.Methods do
      begin
        Result := Max(Result, Method.Id);
      end;
      for Method in AIntf.Properties do
      begin
        Result := Max(Result, Method.Id);
      end;
      if Result = -MaxInt then
        Result := 0;
    end;

    procedure WriteWarning(const AIntfName, AMethodName: string; AOld, ANew: LongInt);
    begin
      System.Writeln('Warning: Duplicate ID found for method ', AIntfName, '.', AMethodName, ', replacing old ID: ', AOld, ' with new ID: ', ANew);
    end;

  var
    LIntf: TPWIGInterface;
    Method: TPWIGMethod;
    NewID: LongInt;
  begin
    // find duplicate Ids and replace them with new, unique Ids
    for LIntf in FInterfaces do
    begin
      for Method in LIntf.Methods do
      begin
        if FindID(LIntf, Method) then
        begin
          NewID := MaxID(LIntf) + 1;
          WriteWarning(LIntf.Name, Method.Name, Method.Id, NewID);
          Method.Id := NewID;
        end;
      end;
      for Method in LIntf.Properties do
      begin
        if FindID(LIntf, Method) then
        begin
          NewID := MaxID(LIntf) + 1;
          WriteWarning(LIntf.Name, Method.Name, Method.Id, NewID);
          Method.Id := NewID;
        end;
      end;
    end;
  end;

  procedure FixDefaultInterfaces(AEvents: Boolean);
  var
    LCls: TPWIGClass;
    LIntf: TPWIGInterface;
    Ref: TPWIGInterfaceRef;
  begin
    // set first valid referenced interface as default if there is none
    for LCls in FClasses do
    begin
      LIntf := FindDefaultIntf(LCls, AEvents);
      if LIntf = nil then
      begin
        for Ref in LCLs.InterfaceRefs do
        begin
          LIntf := FInterfaces.Find(Ref.RefGUID, Ref.RefName);
          if (LIntf <> nil) and (LIntf.FlagDispEvents = AEvents) then
          begin
            Writeln('Warning: No default interface found for class ', LCls.Name, ', using: ', LIntf.Name);
            Ref.FlagDefault := True;
            Break;
          end;
        end;
      end;
    end;
  end;

begin
  // fix interface method IDs
  FixInterfaceMethodIds;

  // fix class default interfaces
  FixDefaultInterfaces(False); // for normal interfaces
  FixDefaultInterfaces(True); // for event interfaces (dispinterfaces)
end;

procedure TPWIG.Generate;

  function CreateGenerator(AGenType: TPWIGGeneratorType): TPWIGGenerator;
  begin
    Result := nil;
    case AGenType of
      gtCpp: Result := TPWIGGenCpp.Create(Self);
      gtCSharp: Result := TPWIGGenCSharp.Create(Self);
      gtPascal: Result := TPWIGGenPascal.Create(Self);
      gtRIDL: Result := TPWIGGenRIDL.Create(Self);
    end;
  end;

var
  Gen: TPWIGGenerator;
  Config: TPWIGGeneratorConfig;
begin
  Writeln('Processing callee configurations:');
  Writeln('');
  for Config in FCalleeConfigs do
  begin
    Gen := CreateGenerator(Config.GenType);
    try
      Gen.SaveCalleeFiles(Config.GenPath);
    finally
      Gen.Free;
    end;
  end;
  Writeln('');
  Writeln('Processing caller configurations:');
  Writeln('');
  for Config in FCallerConfigs do
  begin
    Gen := CreateGenerator(Config.GenType);
    try
      Gen.SaveCallerFiles(Config.GenPath);
    finally
      Gen.Free;
    end;
  end;
  Writeln('');
  Writeln('Processing completed!');
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
          FGlobalCallingConv := StringToCallingConv(N.ChildAsString(nnPWIGGlobalCallingConv, ''));
          FStaticLibraryName := N.ChildAsString(nnPWIGStaticLibName, '');
          FCalleeConfigs.Load(N, nnPWIGGenCalleeConfig);
          FCallerConfigs.Load(N, nnPWIGGenCallerConfig);
          FAliases.Load(N, nnPWIGAlias);
          FEnums.Load(N, nnPWIGEnum);
          FInterfaces.Load(N, nnPWIGInterface);
          FClasses.Load(N, nnPWIGClass);
          FixReferences;
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
  Writeln('PWIG is a software development tool that connects programs written in one programming language with a variety of another programming languages.');
  Writeln('It reads the interface definitions from a single XML configuration file and generates wrapper code for the caller (main program) and the callee (shared library).');
  Writeln('PWIG has been written in Free Pascal Compiler/Lazarus, hence its name (Pascal Wrapper and Interface Generator).');
  Writeln('However, it can be used not only by Pascal programmers but for any other programming languages as well, as long as its wrapper generators exist.');
  Writeln('');
  Writeln('Syntax: PWIG [input file]');
  Writeln('');
  Writeln('Example: PWIG text.xml');
  Writeln('');
  Writeln('Entire configuration must be present in the input file.');
  Writeln('You can create the input file with the PWIG GUI or write it by hand.');
  Writeln('See additional documentation for the input file structure.');
  Writeln('');
  Writeln('Currently supported generators:');
  Writeln('-Pascal (Delphi, FPC/Lazarus)');
  Writeln('-RIDL (Delphi version of COM IDL)');
  Writeln('');
end;

procedure TPWIG.PrintCopyright;
var
  VMajor, VMinor, VBuild, VRev: Word;
  AppVersion: string;
begin
  if GetAppVersion(ParamStr(0), VMajor, VMinor, VBuild, VRev) then
    AppVersion := Format('%d.%d.%d.%d', [VMajor, VMinor, VBuild, VRev])
  else
    AppVersion := '<unknown version>';
  Writeln('');
  Writeln('PWIG ', AppVersion, ', Copyright (C) 2016 Tomas Krysl (tk@tkweb.eu)');
  Writeln('');
end;

function TPWIG.ReadParams: Boolean;
begin
  Result := False;
  if ParamCount = 1 then
  begin
    FInputFile := ParamStr(1);
    if FileExists(FInputFile) then
    begin
      Result := LoadFromFile(InputFile);
      if Result then
        Writeln('Input file has been read: ', FInputFile)
      else
        Writeln('Bad format of input file: ', FInputFile,  '!');
    end
    else
      Writeln('Input file does not exist: ', FInputFile,  '!');
  end else
    Writeln('Missing input file!');
  Writeln('');
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
        N.Children.Add(nnPWIGGlobalCallingConv).AsString := CallingConvToString(FGlobalCallingConv);
        N.Children.Add(nnPWIGStaticLibName).AsString := FStaticLibraryName;
        FCalleeConfigs.Save(N, nnPWIGGenCalleeConfig);
        FCallerConfigs.Save(N, nnPWIGGenCallerConfig);
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
