{ @abstract(This unit contains PWIG Pascal wrapper class generator (usable by Lazarus or Delphi))
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(23 Oct 2016)

  Copyright © Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  Generated outputs tested in:
  -Lazarus 1.6 + FPC 3.0.0, Win32 (callee + caller)
  -Delphi XE, Win32 (callee + caller)

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }

unit pwig_pascal;

{$mode delphi}

interface

uses
  Classes, SysUtils, PWIGGen;

type

  TPWIGGenPascalMethodStyle = (
    msIntfDeclaration,
    msLibCallGetLength,
    msLibCall,
    msLibExport,
    msClassDeclaration,
    msClassImplementation,
    msClassCall,
    msPropertyDeclaration,
    msEventDeclaration,
    msEventSinkCallGetLength,
    msEventSinkCall
  );

  { TPWIGGenPascal }

  TPWIGGenPascal = class(TPWIGGenerator)
  private
    FFlags: string;
    FIndent: string;
    FFirstDashSep: Boolean;
    procedure IncIndent;
    procedure DecIndent;
    procedure ClearFlags;
    procedure AddFlag(AFlag: Boolean; const AFlagText: string);
    procedure InitDashSep;
    function ParamDirection(AMethod: TPWIGMethod; AParam: TPWIGParam; AGetter: Boolean): TPWIGParamDirection;
    function ParamDirectionToString(ADirection: TPWIGParamDirection): string;
    procedure WriteDashSep;
    procedure WriteSpace;
    procedure WritePascalBegin;
    procedure WritePascalEnd;
    procedure WritePascalEndElse;
    procedure WriteTry;
    procedure WriteExcept(const AMessage: string = '');
    procedure WriteExceptLibCall;
    property Flags: string read FFlags;
    property Indent: string read FIndent;
  protected
    F: TextFile;
    function GetDescription: string; override;
    function GetName: string; override;
    function CallingConvToString(AMethod: TPWIGMethod): string; virtual;
    function TypeToString(AType: TPWIGType): string; virtual;
    function ReplaceNotAllowedParamName(AName: string): string; virtual;
    procedure WriteIntfElementProps(AElement: TPWIGElement); virtual;
    procedure WriteIntfAliasProps(AAlias: TPWIGAlias); virtual;
    procedure WriteIntfEnumProps(AEnum: TPWIGEnum); virtual;
    procedure WriteIntfInterfaceProps(AIntf: TPWIGInterface); virtual;
    procedure WriteIntfClassProps(AClass: TPWIGClass); virtual;
    procedure WriteMethod(const AClassName: string; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      AStyle: TPWIGGenPascalMethodStyle; ADefaultInterface, AGetter: Boolean; const APrefix: string); virtual;

    procedure WriteCalleeConstructor(AClass: TPWIGClass; AIntf: TPWIGInterface; ABody: Boolean); virtual;
    procedure WriteCalleeDestructor(AClass: TPWIGClass; AIntf: TPWIGInterface; ABody: Boolean); virtual;
    procedure WriteCalleeMethod(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      ABody, ADefaultInterface, AGetter: Boolean; const APrefix: string); virtual;
    procedure WriteCalleeEventSetter(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      ABody, ADefaultInterface: Boolean); virtual;
    procedure WriteCalleeDeclarations(AClass: TPWIGClass); virtual;
    procedure WriteCalleeEventSinkDeclarations(AIntf: TPWIGInterface); virtual;
    procedure WriteCalleeEventSinkImplementations(AIntf: TPWIGInterface); virtual;
    procedure WriteCalleeExportedFuncs(AClass: TPWIGClass; ABody: Boolean); virtual;
    procedure WriteCalleeExports(AClass: TPWIGClass); virtual;
    function WriteCalleeUnicodeStringDeclarations(AClass: TPWIGClass; AIntf: TPWIGInterface;
       AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;
    function WriteCalleeUnicodeStringManagement1(AClass: TPWIGClass; AIntf: TPWIGInterface;
       AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;
    function WriteCalleeUnicodeStringManagement2(AClass: TPWIGClass; AIntf: TPWIGInterface;
       AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;

    procedure WriteCallerConstructor(AClass: TPWIGClass; AIntf: TPWIGInterface); virtual;
    procedure WriteCallerDestructor(AClass: TPWIGClass; AIntf: TPWIGInterface); virtual;
    procedure WriteCallerMethod(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      ADefaultInterface, AGetter: Boolean; const APrefix: string); virtual;
    procedure WriteCallerEventCallback(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      ADefaultInterface: Boolean); virtual;
    procedure WriteCallerDeclarations(AClass: TPWIGClass); virtual;
    procedure WriteCallerPointers(AClass: TPWIGClass; AUseType: Boolean); virtual;
    procedure WriteCallerLibLoads(AClass: TPWIGClass); virtual;
    procedure WriteCallerImplementations(AClass: TPWIGClass); virtual;
    function WriteCallerUnicodeStringDeclarations(AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;
    function WriteCallerUnicodeStringManagement1(AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;
    function WriteCallerUnicodeStringManagement2(AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;
    function WriteCallerUnicodeStringManagement3(AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;

    procedure WriteInterfaceFile(const AFileName: string);
    procedure WriteCalleeFile(const AFileName: string);
    procedure WriteCallerFile(const AFileName: string);
  public
    constructor Create(AOwner: TPWIG); override;
    procedure SaveCalleeFiles(const AFileName: string); override;
    procedure SaveCallerFiles(const AFileName: string); override;
  end;

implementation

uses
  KFunctions;

{ TPWIGGenPascal }

constructor TPWIGGenPascal.Create(AOwner: TPWIG);
begin
  inherited Create(AOwner);
  FIndent := '';
end;

procedure TPWIGGenPascal.IncIndent;
begin
  FIndent := FIndent + '  ';
end;

procedure TPWIGGenPascal.DecIndent;
begin
  Delete(FIndent, 1, 2);
end;

procedure TPWIGGenPascal.ClearFlags;
begin
  FFlags := '';
end;

procedure TPWIGGenPascal.AddFlag(AFlag: Boolean; const AFlagText: string);
begin
  if AFlag then
  begin
    if FFlags <> '' then
      FFlags := FFlags + ', ';
    FFlags := FFlags + AFlagText;
  end;
end;

procedure TPWIGGenPascal.InitDashSep;
begin
  FFirstDashSep := True;
end;

function TPWIGGenPascal.ParamDirection(AMethod: TPWIGMethod;
  AParam: TPWIGParam; AGetter: Boolean): TPWIGParamDirection;
begin
  Result := AParam.ParamDirection;
  if AMethod is TPWIGProperty then
  begin
    // all params are [in] except last one which is [out, retval] for a getter
    if not AGetter or (AParam <> AMethod.Params.Last) then
      Result := pdIn
    else
      Result := pdOut;
  end;
end;

function TPWIGGenPascal.ParamDirectionToString(ADirection: TPWIGParamDirection
  ): string;
begin
  Result := '';
  case ADirection of
    pdIn: Result := 'const';
    pdOut: Result := 'out';
    pdInOut: Result := 'var';
  end;
end;

function TPWIGGenPascal.CallingConvToString(AMethod: TPWIGMethod): string;
var
  Conv: TPWIGCallingConv;
begin
  if (AMethod <> nil) and (AMethod.CallingConv <> ccNone) then
    Conv := AMethod.CallingConv
  else
    Conv := FPWIG.GlobalCallingConv;
  Result := '';
  case Conv of
    ccCDecl: Result := 'cdecl';
    ccPascal: Result := 'pascal';
    ccStdCall: Result := 'stdcall';
    ccSafeCall: Result := 'safecall';
  end;
end;

function TPWIGGenPascal.TypeToString(AType: TPWIGType): string;
begin
  Result := '';
  case AType.BaseType of
    btLongInt: Result := 'LongInt';
    btLongWord: Result := 'LongWord';
    btSmallInt: Result := 'SmallInt';
    btWord: Result := 'Word';
    btInt64: Result := 'Int64';
    btUInt64: Result := 'UInt64';
    btSingle: Result := 'Single';
    btDouble: Result := 'Double';
    btUnicodeString: Result := 'string'; // Unicode string
    btRawByteString: Result := 'PByteArray';
    btCurrency: Result := 'Currency';
    btDateTime: Result := 'TDateTime';
    btEnum: Result := FPWIG.Enums.FindName(AType.CustomTypeGUID, AType.CustomTypeName);
    btAlias: Result := FPWIG.Aliases.FindName(AType.CustomTypeGUID, AType.CustomTypeName);
    btInterface: Result := FPWIG.Interfaces.FindName(AType.CustomTypeGUID, AType.CustomTypeName);
  end;
end;

function TPWIGGenPascal.ReplaceNotAllowedParamName(AName: string): string;
begin
  Result := AName;
  // naive check here, should be sufficient
  if LowerCase(AName) = 'result' then
    Result := AName + 'Param';
end;

procedure TPWIGGenPascal.WriteDashSep;
begin
  if not FFirstDashSep then
    Writeln(F, ',');
  FFirstDashSep := False;
end;

procedure TPWIGGenPascal.WriteSpace;
begin
  Writeln(F);
end;

procedure TPWIGGenPascal.WritePascalBegin;
begin
  Writeln(F, Indent, 'begin');
  IncIndent;
end;

procedure TPWIGGenPascal.WritePascalEnd;
begin
  DecIndent;
  Writeln(F, Indent, 'end;');
end;

procedure TPWIGGenPascal.WritePascalEndElse;
begin
  DecIndent;
  Writeln(F, Indent, 'end else');
end;

procedure TPWIGGenPascal.WriteTry;
begin
  Writeln(F, Indent, 'try');
  IncIndent;
end;

procedure TPWIGGenPascal.WriteExcept(const AMessage: string);
begin
  DecIndent;
  Writeln(F, Indent, 'except');
  if AMessage <> '' then
  begin
    IncIndent;
    Writeln(F, Indent, AMessage);
    DecIndent;
  end;
  Writeln(F, Indent, 'end;');
end;

procedure TPWIGGenPascal.WriteExceptLibCall;
begin
  WriteExcept('on E : Exception do LibCallError(E.Message);');
end;

function TPWIGGenPascal.GetDescription: string;
begin
  Result := 'Pascal (unmanaged code, callee + caller). Usable in Delphi, Lazarus/Free Pascal.';
end;

function TPWIGGenPascal.GetName: string;
begin
  Result := 'Pascal';
end;

procedure TPWIGGenPascal.SaveCalleeFiles(const AFileName: string);
begin
  WriteInterfaceFile(AFileName);
  WriteCalleeFile(AFileName);
end;

procedure TPWIGGenPascal.SaveCallerFiles(const AFileName: string);
begin
  WriteInterfaceFile(AFileName);
  WriteCallerFile(AFileName);
end;

procedure TPWIGGenPascal.WriteIntfElementProps(AElement: TPWIGElement);
begin
  if AElement.Name <> '' then
    Writeln(F, Indent, '// Name: ', AElement.Name);
  if AElement.Version <> '' then
    Writeln(F, Indent, '// Version: ', AElement.Version);
  if AElement.GUID <> '' then
    Writeln(F, Indent, '// GUID: ', AElement.GUID);
  if AElement.Description <> '' then
    Writeln(F, Indent, '// Description: ', AElement.Description);
  if (AElement is TPWIGInterface) and TPWIGInterface(AElement).FlagDual then
    Writeln(F, Indent, '// Dual interface (COM only)');
  if (AElement is TPWIGInterface) and TPWIGInterface(AElement).FlagOleAutomation then
    Writeln(F, Indent, '// OLE automation interface (COM only)');
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteIntfAliasProps(AAlias: TPWIGAlias);
begin
  Writeln(F, Indent, '// Alias type properties:');
  WriteIntfElementProps(AAlias);
  Writeln(F, Indent, AAlias.Name, ' = type ', TypeToString(AAlias.AliasedType), ';');
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteIntfEnumProps(AEnum: TPWIGEnum);
var
  I: Integer;
  Elem: TPWIGEnumElement;
begin
  Writeln(F, Indent, '// Enumerated type properties:');
  WriteIntfElementProps(AEnum);
  Writeln(F, Indent, AEnum.Name, ' = (');
  IncIndent;
  for I := 0 to AEnum.Elements.Count - 1 do
  begin
    Elem := AEnum.Elements[I];
    Write(F, Indent, Elem.Name, ' = ', Elem.Value);
    if I < AEnum.Elements.Count - 1 then
      Writeln(F, ',')
    else
      Writeln(F);
  end;
  DecIndent;
  Writeln(F, Indent, ');');
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteIntfInterfaceProps(AIntf: TPWIGInterface);
var
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
begin
  Writeln(F, Indent, '// Interface type properties:');
  WriteIntfElementProps(AIntf);
  Writeln(F, Indent, '// Methods:');
  for Method in AIntf.Methods do
  begin
    WriteMethod('', AIntf, Method, msIntfDeclaration, False, False, '');
  end;
  Writeln(F, Indent, '// Properties:');
  for Prop in AIntf.Properties do
  begin
    if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
    begin
      // write getter
      WriteMethod('', AIntf, Prop, msIntfDeclaration, False, True, 'Get');
    end;
    if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
    begin
      // write setter
      WriteMethod('', AIntf, Prop, msIntfDeclaration, False, False, 'Set');
    end;
  end;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteIntfClassProps(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf, LIntfDef: TPWIGInterface;
  Method: TPWIGMethod;
begin
  Writeln(F, Indent, '// Class properties:');
  WriteIntfElementProps(AClass);
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if LIntf <> nil then
      if LIntf.FlagDispEvents then
      begin
        // output prototypes for event handler setup functions
        LIntfDef := FPWIG.FindDefaultIntf(AClass, False);
        if LIntfDef <> nil then
          // we suppose default interface always exists!
          for Method in LIntf.Methods do
            Writeln(F, Indent, 'TSet', AClass.Name, LIntf.Name, Method.Name, ' = function(const ItemHandle: ', LIntfDef.Name, '; const EventSink: ', LIntf.Name, '; const EventHandler: T', LIntf.Name, Method.Name, '): Boolean; ', CallingConvToString(nil),';');
      end else
      begin
        // use constructor and destructor only for default interface
        if Ref.FlagDefault then
        begin
          Writeln(F, Indent, 'T', AClass.Name, 'Create = function(out ItemHandle: ', LIntf.Name, '): Boolean; ', CallingConvToString(nil),';');
          Writeln(F, Indent, 'T', AClass.Name, 'Destroy = function(const ItemHandle: ', LIntf.Name, '): Boolean; ', CallingConvToString(nil),';');
        end;
      end;
  end;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteMethod(const AClassName: string; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
  AStyle: TPWIGGenPascalMethodStyle; ADefaultInterface, AGetter: Boolean; const APrefix: string);

  procedure WriteLibCallUnicodeStringParam(AParamDir: TPWIGParamDirection; const AParamName: string);
  begin
    if AParamDir = pdIn then
      Write(F, 'PAnsiChar(String2LibUtf8String(', AParamName, '))')
    else if AStyle in [msLibCallGetLength, msEventSinkCallGetLength] then
      Write(F, 'nil, Length__', AParamName)
    else
      Write(F, 'PAnsiChar(AnsiString__', AParamName, '), Length__', AParamName);
  end;

var
  Param, RetVal: TPWIGParam;
  ParamDir: TPWIGParamDirection;
  CallingConv, HandleParam, IntfName, ParamName, ParamSpecifier, FuncKeyword, RetValAssignment: string;
  FirstParam: Boolean;
begin
  if AStyle in [msIntfDeclaration, msLibExport] then
  begin
    CallingConv := '; ' + CallingConvToString(AMethod);
    HandleParam :=  'const ItemHandle: ' + AIntf.Name;
    RetVal := nil;
    FuncKeyword := '';
    IntfName := AIntf.name;
  end
  else if AStyle in [msLibCall, msLibCallGetLength] then
  begin
    CallingConv := '';
    HandleParam := 'FItemHandle';
    if AGetter then
      RetVal := AMethod.Params.Last
    else
      RetVal := AMethod.Params.FindRetVal;
    FuncKeyword := '';
    IntfName := AIntf.Name;
  end
  else if AStyle in [msEventSinkCall, msEventSinkCallGetLength] then
  begin
    CallingConv := '';
    HandleParam := 'F' + AMethod.Name + 'EventSink';
    RetVal := nil;
    FuncKeyWord := '';
    RetValAssignment := '';
    IntfName := '';
  end else
  begin
    CallingConv := '';
    HandleParam := '';
    if AGetter or (AStyle = msPropertyDeclaration) then
      RetVal := AMethod.Params.Last
    else
      RetVal := AMethod.Params.FindRetVal;
    if RetVal <> nil then
    begin
      FuncKeyword := 'function';
      ParamName := ReplaceNotAllowedParamName(RetVal.Name);
      if RetVal.ParamType.BaseType = btUnicodeString then
        ParamName := 'String__' + AClassName + AIntf.Name + AMethod.Name + ParamName;
      RetValAssignment := ParamName + ' := ';
    end else
    begin
      FuncKeyword := 'procedure';
      RetValAssignment :=  '';
    end;
    if ADefaultInterface then
      IntfName := ''
    else
      IntfName := AIntf.Name;
  end;
  case AStyle of
    msIntfDeclaration:
      Write(F, Indent, 'T', APrefix, IntfName, AMethod.Name, ' = function(', HandleParam);
    msLibCall, msLibCallGetLength:
      Write(F, Indent, 'Func', AClassName, APrefix, IntfName, AMethod.Name, '(', HandleParam);
    msLibExport:
      Write(F, Indent, 'function ', AClassName, APrefix, IntfName, AMethod.Name, '(', HandleParam);
    msClassDeclaration:
      Write(F, Indent, FuncKeyword, ' ', APrefix, IntfName, AMethod.Name);
    msClassImplementation:
      Write(F, Indent, FuncKeyword, ' T', AClassName, '.', APrefix, IntfName, AMethod.Name);
    msClassCall:
      Write(F, Indent, RetValAssignment, 'T', AClassName, '(ItemHandle).', APrefix, IntfName, AMethod.Name);
    msPropertyDeclaration:
      Write(F, Indent, 'property ', IntfName, AMethod.Name);
    msEventDeclaration:
      Write(F, Indent, 'T', AClassName, IntfName, AMethod.Name, APrefix, ' = ', FuncKeyword);
    msEventSinkCall, msEventSinkCallGetLength:
      Write(F, Indent, 'F', AMethod.Name, 'EventHandler(', HandleParam);
  end;
  if (AMethod.Params.Count = 0) or (AMethod.Params.Count = 1) and (RetVal <> nil) then
  begin
    if (RetVal <> nil) and (AStyle in [msLibCall, msLibCallGetLength]) then
    begin
      Write(F, ', ');
      if RetVal.ParamType.BaseType = btUnicodeString then
      begin
        ParamDir := ParamDirection(AMethod, RetVal, AGetter);
        ParamName := ReplaceNotAllowedParamName(RetVal.Name);
        WriteLibCallUnicodeStringParam(ParamDir, ParamName);
      end else
        Write(F, 'Result');
    end;
    if HandleParam <> '' then
      Write(F, ')');
  end
  else
  begin
    if HandleParam <> '' then
    begin
      if AStyle in [msClassCall, msLibCall, msLibCallGetLength, msEventSinkCall, msEventSinkCallGetLength] then
        Write(F, ', ')
      else
        Write(F, '; ');
    end
    else if AStyle = msPropertyDeclaration then
      Write(F, '[')
    else
      Write(F, '(');
    FirstParam := True;
    for Param in AMethod.Params do
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      ParamDir := ParamDirection(AMethod, Param, AGetter);
      if Param <> RetVal then
      begin
        if not FirstParam then
        begin
          if AStyle in [msClassCall, msLibCall, msLibCallGetLength, msEventSinkCall, msEventSinkCallGetLength] then
            Write(F, ', ')
          else
            Write(F, '; ');
        end;
        FirstParam := False;
        ParamSpecifier := ParamDirectionToString(ParamDir) + ' ';
        if (AStyle in [msClassDeclaration, msClassImplementation]) and (AMethod is TPWIGProperty) then
        begin
          // Delphi does not support const in property getter/setter
          if (ParamDir = pdIn) and (Param <> AMethod.Params.Last) then
            ParamSpecifier := '';
        end;
        if (Param.ParamType.BaseType = btUnicodeString) and (AStyle in [msIntfDeclaration, msLibExport]) then
        begin
          // automated string management for UnicodeString
          if ParamDir = pdIn then
            Write(F, ParamSpecifier, ParamName, ': PAnsiChar')
          else
            Write(F, 'const ', ParamName, ': PAnsiChar; var Length__', ParamName, ': LongInt');
        end
        else if (Param.ParamType.BaseType = btUnicodeString) and (AStyle in [msClassCall]) then
        begin
          if ParamDir = pdIn then
            Write(F, 'LibUtf8String2String(', ParamName, ')')
          else
            Write(F, 'String__' + AClassName + AIntf.Name + AMethod.Name + ParamName);
        end
        else if (Param.ParamType.BaseType = btUnicodeString) and (AStyle in [msLibCall, msLibCallGetLength, msEventSinkCall, msEventSinkCallGetlength]) then
        begin
          WriteLibCallUnicodeStringParam(ParamDir, ParamName);
        end
        else if AStyle in [msClassCall, msLibCall, msLibCallGetLength, msEventSinkCall, msEventSinkCallGetLength] then
          Write(F, ParamName)
        else if AStyle = msPropertyDeclaration then
          Write(F, ParamName, ': ', TypeToString(Param.ParamType))
        else
          Write(F, ParamSpecifier, ParamName, ': ', TypeToString(Param.ParamType));
      end
      else if AStyle in [msLibCall, msLibCallGetLength] then
      begin
        Write(F, ', ');
        if Param.ParamType.BaseType = btUnicodeString then
        begin
          WriteLibCallUnicodeStringParam(ParamDir, ParamName);
        end else
          Write(F, 'Result');
      end;
    end;
    if AStyle = msPropertyDeclaration then
      Write(F, ']')
    else
      Write(F, ')');
  end;
  if AStyle in [msIntfDeclaration, msLibExport] then
    Write(F, ': Boolean', CallingConv)
  else if (AStyle in [msClassDeclaration, msClassImplementation]) and (RetVal <> nil) then
    Write(F, ': ', TypeToString(RetVal.ParamType))
  else if (AStyle = msPropertyDeclaration) and (AMethod is TPWIGProperty) then
  begin
    if RetVal <> nil then
      Write(F, ': ', TypeToString(RetVal.ParamType));
    if TPWIGProperty(AMethod).PropertyType in [ptReadOnly, ptReadWrite] then
    begin
      // write getter
      Write(F, ' read Get', IntfName, AMethod.Name);
    end;
    if TPWIGProperty(AMethod).PropertyType in [ptWriteOnly, ptReadWrite] then
    begin
      // write setter
      Write(F, ' write Set', IntfName, AMethod.Name);
    end;
  end;
  if AStyle in [msLibCall, msLibCallGetLength] then
    Writeln(F)
  else if AStyle = msEventDeclaration then
    Writeln(F, ' of object;')
  else
    Writeln(F, ';');
end;

procedure TPWIGGenPascal.WriteCalleeMethod(AClass: TPWIGClass; AIntf: TPWIGInterface;
 AMethod: TPWIGMethod; ABody, ADefaultInterface, AGetter: Boolean; const APrefix: string);
var
  UnicodeStringsExist: Boolean;
begin
  UnicodeStringsExist := False;
  if ABody then
    UnicodeStringsExist := WriteCalleeUnicodeStringDeclarations(AClass, AIntf, AMethod, AGetter);
  WriteMethod(AClass.Name, AIntf, AMethod, msLibExport, False, AGetter, APrefix);
  if ABody then
  begin
    // begin to write function body
    WritePascalBegin;
    Writeln(F, Indent, 'Result := False;');
    WriteTry;
    Writeln(F, Indent, 'if TObject(ItemHandle) is T', AClass.Name, ' then');
    WritePascalBegin;
    if UnicodeStringsExist then
      WriteCalleeUnicodeStringManagement1(AClass, AIntf, AMethod, AGetter);
    WriteMethod(AClass.Name, AIntf, AMethod, msClassCall, ADefaultInterface, AGetter, APrefix);
    if UnicodeStringsExist then
      WriteCalleeUnicodeStringManagement2(AClass, AIntf, AMethod, AGetter);
    // end of function body
    Writeln(F, Indent, 'Result := True;');
    WritePascalEnd;
    WriteExcept;
    WritePascalEnd;
    WriteSpace;
  end;
end;

procedure TPWIGGenPascal.WriteCalleeEventSetter(AClass: TPWIGClass;
  AIntf: TPWIGInterface; AMethod: TPWIGMethod; ABody, ADefaultInterface: Boolean);
var
  LIntfDef: TPWIGInterface;
  IntfName: string;
begin
  LIntfDef := FPWIG.FindDefaultIntf(AClass, False);
  if LIntfDef = nil then Exit; // we suppose default interface always exists!
  if ADefaultInterface then
    IntfName := ''
  else
    IntfName := AIntf.Name;
  Writeln(F, Indent, 'function Set', AClass.Name, AIntf.Name, AMethod.Name, '(const ItemHandle:', LIntfDef.Name, '; const EventSink: ', AIntf.Name, '; const EventHandler: T', AIntf.Name, AMethod.Name, '): Boolean; ', CallingConvToString(nil),';');
  if not ABody then Exit;
  WritePascalBegin;
  Writeln(F, Indent, 'Result := False;');
  WriteTry;
  Writeln(F, Indent, 'if TObject(ItemHandle) is T', AClass.Name, ' then');
  WritePascalBegin;
  Writeln(F, Indent, 'T', AClass.Name, '(ItemHandle).Setup', IntfName, AMethod.Name, '(EventSink, EventHandler);');
  Writeln(F, Indent, 'Result := True;');
  WritePascalEnd;
  WriteExcept;
  WritePascalEnd;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCalleeConstructor(AClass: TPWIGClass; AIntf: TPWIGInterface; ABody: Boolean);
begin
  Writeln(F, Indent, 'function ', AClass.Name, 'Create(out ItemHandle:', AIntf.Name, '): Boolean; ', CallingConvToString(nil),';');
  if not ABody then Exit;
  WritePascalBegin;
  WriteTry;
  Writeln(F, Indent, 'ItemHandle := ', AIntf.Name, '(T', AClass.Name, '.Create);');
  Writeln(F, Indent, 'Result := True;');
  WriteExcept('Result := False;');
  WritePascalEnd;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCalleeDestructor(AClass: TPWIGClass; AIntf: TPWIGInterface; ABody: Boolean);
begin
  Writeln(F, Indent, 'function ', AClass.Name, 'Destroy(ItemHandle: ', AIntf.Name, '): Boolean; ', CallingConvToString(nil),';');
  if not ABody then Exit;
  WritePascalBegin;
  Writeln(F, Indent, 'Result := False;');
  WriteTry;
  Writeln(F, Indent, 'if TObject(ItemHandle) is T', AClass.Name, ' then');
  WritePascalBegin;
  Writeln(F, Indent, 'T', AClass.Name, '(ItemHandle).Free;');
  Writeln(F, Indent, 'Result := True;');
  WritePascalEnd;
  WriteExcept;
  WritePascalEnd;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCalleeDeclarations(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
  IntfName: string;
begin
  WriteIntfElementProps(AClass);
  Writeln(F, Indent, 'T', AClass.Name, ' = class(TObject)');
  Writeln(F, Indent, 'private');
  IncIndent;
  // write event handler declarations, private section
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and LIntf.FlagDispEvents then
    begin
      Writeln(F, Indent, 'FEvents: T', LIntf.Name, ';');
    end;
  end;
  DecIndent;
  Writeln(F, Indent, 'public');
  IncIndent;
  // write interface declarations, public section
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
    begin
      // use constructor and destructor only for default interface
      if Ref.FlagDefault then
      begin
        Writeln(F, Indent, 'constructor Create;');
        Writeln(F, Indent, 'destructor Destroy; override;');
      end;
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        WriteMethod(AClass.Name, LIntf, Method, msClassDeclaration, Ref.FlagDefault, False, '');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteMethod(AClass.Name, LIntf, Prop, msClassDeclaration, Ref.FlagDefault, True, 'Get');
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteMethod(AClass.Name, LIntf, Prop, msClassDeclaration, Ref.FlagDefault, False, 'Set');
        end;
      end;
    end;
  end;
  // write event handler declarations, public section
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and LIntf.FlagDispEvents then
    begin
      Writeln(F, Indent, '// Setup event handlers:');
      if Ref.FlagDefault then
        IntfName := ''
      else
        IntfName := LIntf.Name;
      for Method in LIntf.Methods do
      begin
        Writeln(F, Indent, 'procedure Setup', IntfName, Method.Name, '(const EventSink: ', LIntf.Name, '; const EventHandler: T', LIntf.Name, Method.Name, ');');
      end;
    end;
  end;
  WritePascalEnd;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCalleeEventSinkDeclarations(AIntf: TPWIGInterface);
var
  Method: TPWIGMethod;
begin
  WriteIntfElementProps(AIntf);
  Writeln(F, Indent, '// Wrapper class for ', AIntf.Name, ' interface event sinks:');
  WriteSpace;
  Writeln(F, Indent, 'type');
  IncIndent;
  Writeln(F, Indent, 'T', AIntf.Name, ' = class(TObject)');
  Writeln(F, Indent, 'private');
  IncIndent;
  for Method in AIntf.Methods do
  begin
    Writeln(F, Indent, 'F', Method.Name, 'EventSink: ', AIntf.Name, ';');
    Writeln(F, Indent, 'F', Method.Name, 'EventHandler: T', AIntf.Name, Method.Name, ';');
  end;
  DecIndent;
  Writeln(F, Indent, 'public');
  IncIndent;
  Writeln(F, Indent, 'constructor Create;');
  Writeln(F, Indent, '// Setup event handlers:');
  for Method in AIntf.Methods do
  begin
    Writeln(F, Indent, 'procedure Setup', Method.Name, '(const EventSink: ', AIntf.Name, '; const EventHandler: T', AIntf.Name, Method.Name, ');');
  end;
  Writeln(F, Indent, '// Call event handlers:');
  for Method in AIntf.Methods do
  begin
    WriteMethod('', AIntf, Method, msClassDeclaration, True, False, '');
  end;
  WritePascalEnd;
  DecIndent;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCalleeEventSinkImplementations(
  AIntf: TPWIGInterface);
var
  Method: TPWIGMethod;
  UnicodeStringsExist: Boolean;
begin
  WriteIntfElementProps(AIntf);
  WriteSpace;
  Writeln(F, Indent, 'constructor T', AIntf.Name, '.Create;');
  WritePascalBegin;
  for Method in AIntf.Methods do
  begin
    Writeln(F, Indent, 'F', Method.Name, 'EventSink := 0;');
    Writeln(F, Indent, 'F', Method.Name, 'EventHandler := nil;');
  end;
  WritePascalEnd;
  WriteSpace;
  Writeln(F, Indent, '// Setup event handlers:');
  WriteSpace;
  for Method in AIntf.Methods do
  begin
    Writeln(F, Indent, 'procedure T', AIntf.Name, '.Setup', Method.Name, '(const EventSink: ', AIntf.Name, '; const EventHandler: T', AIntf.Name, Method.Name, ');');
    WritePascalBegin;
    Writeln(F, Indent, 'F', Method.Name, 'EventSink := EventSink;');
    Writeln(F, Indent, 'F', Method.Name, 'EventHandler := EventHandler;');
    WritePascalEnd;
    WriteSpace;
  end;
  Writeln(F, Indent, '// Call event handlers:');
  WriteSpace;
  for Method in AIntf.Methods do
  begin
    WriteMethod(AIntf.Name, AIntf, Method, msClassImplementation, True, False, '');
    UnicodeStringsExist := WriteCallerUnicodeStringDeclarations(Method, False);
    WritePascalBegin;
    WriteTry;
    if UnicodeStringsExist then
      WriteCallerUnicodeStringManagement1(Method, False);
    Writeln(F, Indent, 'if Assigned(F', Method.Name, 'EventHandler) then');
    WritePascalBegin;
    if UnicodeStringsExist then
    begin
      WriteMethod('', AIntf, Method, msEventSinkCallGetLength, True, False, '');
      WriteCallerUnicodeStringManagement2(Method, False);
    end;
    WriteMethod('', AIntf, Method, msEventSinkCall, False, False, '');
    WritePascalEnd;
    if UnicodeStringsExist then
      WriteCallerUnicodeStringManagement3(Method, False);
    WriteExcept;
    WritePascalEnd;
    WriteSpace;
  end;
end;

procedure TPWIGGenPascal.WriteCalleeExportedFuncs(AClass: TPWIGClass; ABody: Boolean);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
begin
  WriteIntfElementProps(AClass);
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if LIntf <> nil then
      if LIntf.FlagDispEvents then
      begin
        Writeln(F, Indent, '// Event handler setters:');
        for Method in LIntf.Methods do
        begin
          WriteCalleeEventSetter(AClass, LIntf, Method, ABody, Ref.FlagDefault);
        end;
      end else
      begin
        // use constructor and destructor only for default interface
        if Ref.FlagDefault then
        begin
          Writeln(F, Indent, '// Constructor:');
          WriteCalleeConstructor(AClass, LIntf, ABody);
          Writeln(F, Indent, '// Destructor:');
          WriteCalleeDestructor(AClass, LIntf, ABody);
        end;
        Writeln(F, Indent, '// Methods:');
        for Method in LIntf.Methods do
        begin
          WriteCalleeMethod(AClass, LIntf, Method, ABody, Ref.FlagDefault, False, '');
        end;
        Writeln(F, Indent, '// Properties:');
        for Prop in LIntf.Properties do
        begin
          if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
          begin
            // write getter
            WriteCalleeMethod(AClass, LIntf, Prop, ABody, Ref.FlagDefault, True, 'Get');
          end;
          if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
          begin
            // write setter
            WriteCalleeMethod(AClass, LIntf, Prop, ABody, Ref.FlagDefault, False, 'Set');
          end;
        end;
      end;
  end;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCalleeExports(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
begin
  Writeln(F, Indent, '// ', AClass.Name);
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if LIntf <> nil then
      if LIntf.FlagDispEvents then
      begin
        for Method in LIntf.Methods do
        begin
          WriteDashSep;
          Write(F, Indent, 'Set', AClass.Name, LIntf.Name, Method.Name);
        end;
      end else
      begin
        // use constructor and destructor only for default interface
        if Ref.FlagDefault then
        begin
          WriteDashSep;
          Write(F, Indent, AClass.Name, 'Create');
          WriteDashSep;
          Write(F, Indent, AClass.Name, 'Destroy');
        end;
        for Method in LIntf.Methods do
        begin
          WriteDashSep;
          Write(F, Indent, AClass.Name, LIntf.Name, Method.Name);
        end;
        for Prop in LIntf.Properties do
        begin
          if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
          begin
            // write getter
            WriteDashSep;
            Write(F, Indent, AClass.Name, 'Get', LIntf.Name, Prop.Name);
          end;
          if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
          begin
            // write setter
            WriteDashSep;
            Write(F, Indent, AClass.Name, 'Set', LIntf.Name, Prop.Name);
          end;
        end;
      end;
  end;
  Writeln(F);
end;

function TPWIGGenPascal.WriteCalleeUnicodeStringDeclarations(
  AClass: TPWIGClass; AIntf: TPWIGInterface;
  AMethod: TPWIGMethod; AGetter: Boolean): Boolean;
var
  Param: TPWIGParam;
  ParamName: string;
begin
  // automated string management for UnicodeString, callee side
  Result := False;
  for Param in AMethod.Params do
    if (Param.ParamType.BaseType = btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn)  then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      if not Result then
      begin
        Writeln(F, Indent, 'var');
        IncIndent;
      end;
      Result := True;
      Writeln(F, Indent, 'String__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ': string;');
      Writeln(F, Indent, 'AnsiString__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ': AnsiString;');
    end;
  if Result then
  begin
    // because the function is called twice, var or out parameters are not set on the second call
    // we must store them in a temporary variable as well
    for Param in AMethod.Params do
      if (Param.ParamType.BaseType <> btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn)  then
      begin
        ParamName := ReplaceNotAllowedParamName(Param.Name);
        Writeln(F, Indent, 'Tmp__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ': ', TypeToString(Param.ParamType), ';');
      end;
    DecIndent;
  end;
  WriteSpace;
end;

function TPWIGGenPascal.WriteCalleeUnicodeStringManagement1(
  AClass: TPWIGClass; AIntf: TPWIGInterface;
  AMethod: TPWIGMethod; AGetter: Boolean): Boolean;
var
  Param: TPWIGParam;
  ParamName: string;
begin
  // automated string management for UnicodeString, callee side
  Result := True;
  for Param in AMethod.Params do
    if (Param.ParamType.BaseType = btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn) then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      Writeln(F, Indent, 'if ', ParamName, ' = nil then');
      WritePascalBegin;
      Exit;
    end;
end;

function TPWIGGenPascal.WriteCalleeUnicodeStringManagement2(AClass: TPWIGClass;
  AIntf: TPWIGInterface; AMethod: TPWIGMethod; AGetter: Boolean): Boolean;
var
  Param: TPWIGParam;
  ParamName: string;
begin
  // automated string management for UnicodeString, callee side
  for Param in AMethod.Params do
    if ParamDirection(AMethod, Param, AGetter) <> pdIn then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      if Param.ParamType.BaseType = btUnicodeString then
      begin
        Writeln(F, Indent, 'AnsiString__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ' := String2LibUtf8String(String__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ');');
        Writeln(F, Indent, 'Length__', ParamName, ' := Length(AnsiString__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ');')
      end else
        Writeln(F, Indent, 'Tmp__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ' := ', ParamName, ';');
    end;
  WritePascalEndElse;
  WritePascalBegin;
  for Param in AMethod.Params do
    if ParamDirection(AMethod, Param, AGetter) <> pdIn then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      if Param.ParamType.BaseType = btUnicodeString then
      begin
        Writeln(F, Indent, 'if Length(AnsiString__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ') > 0 then');
        IncIndent;
        Writeln(F, Indent, 'System.Move(AnsiString__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, '[1], ', ParamName, '^, Min(Length__', ParamName, ', Length(AnsiString__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ')));');
        DecIndent;
      end
      else
        Writeln(F, Indent, ParamName, ' := Tmp__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ';');
    end;
  WritePascalEnd;
  Result := True;
end;

procedure TPWIGGenPascal.WriteCallerConstructor(AClass: TPWIGClass;
  AIntf: TPWIGInterface);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  IntfName: string;
begin
  Writeln(F, Indent, 'constructor T', AClass.Name, '.Create(AInterfaceHandle: ', AIntf.Name, ');');
  WritePascalBegin;
  WriteTry;
  Writeln(F, Indent, 'FItemHandle := AInterfaceHandle;');
  Writeln(F, Indent, 'if (FItemHandle = 0) and Assigned(Func', AClass.Name, 'Create) then');
  IncIndent;
  Writeln(F, Indent, 'Func', AClass.Name, 'Create(FItemHandle);');
  DecIndent;
  // call event handler setters
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and LIntf.FlagDispEvents then
    begin
      for Method in LIntf.Methods do
      begin
        if Ref.FlagDefault then
          IntfName := ''
        else
          IntfName := LIntf.Name;
        Writeln(F, Indent, 'F', IntfName, Method.Name, ' := nil;');
        Writeln(F, Indent, 'if Assigned(FuncSet', AClass.name, LIntf.Name, Method.Name, ') then');
        WritePascalBegin;
        Writeln(F, Indent, 'if not (');
        IncIndent;
        Writeln(F, Indent, 'FuncSet', AClass.name, LIntf.Name, Method.Name, '(FItemHandle, ', LIntf.Name, '(Self), ', AClass.Name, LIntf.Name, Method.Name, ')');
        DecIndent;
        Writeln(F, Indent, ') then LibError(''FuncSet', AClass.name, LIntf.Name, Method.Name, ''');');
        WritePascalEnd;
      end;
    end;
  end;
  WriteExceptLibCall;
  WritePascalEnd;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCallerDestructor(AClass: TPWIGClass;
  AIntf: TPWIGInterface);
begin
  Writeln(F, Indent, 'destructor T', AClass.Name, '.Destroy;');
  WritePascalBegin;
  WriteTry;
  Writeln(F, Indent, 'if Assigned(Func', AClass.Name, 'Destroy) then');
  IncIndent;
  Writeln(F, Indent, 'Func', AClass.Name, 'Destroy(FItemHandle);');
  DecIndent;
  Writeln(F, Indent, 'inherited;');
  WriteExceptLibCall;
  WritePascalEnd;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCallerMethod(AClass: TPWIGClass;
  AIntf: TPWIGInterface; AMethod: TPWIGMethod;
  ADefaultInterface, AGetter: Boolean; const APrefix: string);
var
  FuncName: string;
  UnicodeStringsExist: Boolean;
begin
  FuncName := AClass.Name + APrefix + AIntf.Name + AMethod.Name;
  WriteMethod(AClass.Name, AIntf, AMethod, msClassImplementation, ADefaultInterface, AGetter, APrefix);
  UnicodeStringsExist := WriteCallerUnicodeStringDeclarations(AMethod, AGetter);
  // write method body
  WritePascalBegin;
  WriteTry;
  Writeln(F, Indent, 'if Assigned(Func', FuncName, ') then');
  WritePascalBegin;
  if UnicodeStringsExist then
  begin
    WriteCallerUnicodeStringManagement1(AMethod, AGetter);
    Writeln(F, Indent, 'if not (');
    IncIndent;
    WriteMethod(AClass.Name, AIntf, AMethod, msLibCallGetLength, ADefaultInterface, AGetter, APrefix);
    DecIndent;
    Writeln(F, Indent, ') then LibError(''Func', FuncName, ''');');
    WriteCallerUnicodeStringManagement2(AMethod, AGetter);
  end;
  Writeln(F, Indent, 'if not (');
  IncIndent;
  WriteMethod(AClass.Name, AIntf, AMethod, msLibCall, ADefaultInterface, AGetter, APrefix);
  DecIndent;
  Writeln(F, Indent, ') then LibError(''Func', FuncName, ''');');
  WritePascalEnd;
  if UnicodeStringsExist then
    WriteCallerUnicodeStringManagement3(AMethod, AGetter);
  WriteExceptLibCall;
  WritePascalEnd;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCallerEventCallback(AClass: TPWIGClass;
  AIntf: TPWIGInterface; AMethod: TPWIGMethod; ADefaultInterface: Boolean);
var
  IntfName: string;
  UnicodeStringsExist: Boolean;
begin
  if ADefaultinterface then
    IntfName := ''
  else
    IntfName := AIntf.Name;
  UnicodeStringsExist := WriteCalleeUnicodeStringDeclarations(AClass, AIntf, AMethod, False);
  WriteMethod(AClass.Name, AIntf, AMethod, msLibExport, ADefaultInterface, False, '');
  // write method body
  WritePascalBegin;
  Writeln(F, Indent, 'Result := False;');
  WriteTry;
  Writeln(F, Indent, 'if TObject(ItemHandle) is T', AClass.Name, ' then');
  WritePascalBegin;
  Writeln(F, Indent, 'if Assigned(T', AClass.Name, '(ItemHandle).', IntfName, AMethod.Name, ') then');
  WritePascalBegin;
  if UnicodeStringsExist then
    WriteCalleeUnicodeStringManagement1(AClass, AIntf, AMethod, False);
  WriteMethod(AClass.Name, AIntf, AMethod, msClassCall, ADefaultInterface, False, '');
  if UnicodeStringsExist then
    WriteCalleeUnicodeStringManagement2(AClass, AIntf, AMethod, False);
  WritePascalEnd;
  Writeln(F, Indent, 'Result := True;');
  WritePascalEnd;
  WriteExcept;
  WritePascalEnd;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCallerDeclarations(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
  IntfName: string;
begin
  WriteIntfElementProps(AClass);
  // write event handler typedefs
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and LIntf.FlagDispEvents then
    begin
      for Method in LIntf.Methods do
      begin
        WriteMethod(AClass.Name, LIntf, Method, msEventDeclaration, False, False, 'Event');
      end;
    end;
  end;
  WriteSpace;
  Writeln(F, Indent, 'T', AClass.Name, ' = class(TObject)');
  Writeln(F, Indent, 'private');
  IncIndent;
  // write event handler declarations, private section
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and LIntf.FlagDispEvents then
    begin
      if Ref.FlagDefault then
        IntfName := ''
      else
        IntfName := LIntf.Name;
      for Method in LIntf.Methods do
      begin
        Writeln(F, Indent, 'F', IntfName, Method.Name, ': T', AClass.Name, LIntf.Name, Method.Name, 'Event;');
      end;
    end;
  end;
  // write interface declarations, private section
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
    begin
      if Ref.FlagDefault then
        Writeln(F, Indent, 'FItemHandle: ', LIntf.Name, ';');
      Writeln(F, Indent, '// Property getters and setters:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteMethod(AClass.Name, LIntf, Prop, msClassDeclaration, Ref.FlagDefault, True, 'Get');
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteMethod(AClass.Name, LIntf, Prop, msClassDeclaration, Ref.FlagDefault, False, 'Set');
        end;
      end;
    end;
  end;
  DecIndent;
  Writeln(F, Indent, 'public');
  IncIndent;
  // write interface declarations, public section
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
    begin
      if Ref.FlagDefault then
      begin
        Writeln(F, Indent, 'constructor Create(AInterfaceHandle: ', LIntf.Name, ' = 0);');
        Writeln(F, Indent, 'destructor Destroy; override;');
      end;
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        WriteMethod(AClass.Name, LIntf, Method, msClassDeclaration, Ref.FlagDefault, False, '');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        WriteMethod(AClass.Name, LIntf, Prop, msPropertyDeclaration, Ref.FlagDefault, False, '');
      end;
    end;
  end;
  // write event handler declarations, public section
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and LIntf.FlagDispEvents then
    begin
      if Ref.FlagDefault then
        IntfName := ''
      else
        IntfName := LIntf.Name;
      Writeln(F, Indent, '// Events:');
      for Method in LIntf.Methods do
      begin
        Writeln(F, Indent, 'property ', IntfName, Method.Name, ': T', AClass.Name, LIntf.Name, Method.Name, 'Event read F', IntfName, Method.Name, ' write F', IntfName, Method.Name, ';');
      end;
    end;
  end;
  // write Handle property
  Writeln(F, Indent, '// Default interface handle:');
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents and Ref.FlagDefault then
      Writeln(F, Indent, 'property Handle: ', LIntf.Name, ' read FItemHandle;');
  end;
  WritePascalEnd;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCallerPointers(AClass: TPWIGClass; AUseType: Boolean);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
begin
  WriteIntfElementProps(AClass);
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
    begin
      // use constructor and destructor only for default interface
      if Ref.FlagDefault then
      begin
        Writeln(F, Indent, '// Constructor:');
        if AUseType then
          Writeln(F, Indent, 'Func', AClass.Name, 'Create: T', AClass.Name, 'Create = nil;')
        else
          Writeln(F, Indent, 'Func', AClass.Name, 'Create := nil;');
        Writeln(F, Indent, '// Destructor:');
        if AUseType then
          Writeln(F, Indent, 'Func', AClass.Name, 'Destroy: T', AClass.Name, 'Destroy = nil;')
        else
          Writeln(F, Indent, 'Func', AClass.Name, 'Destroy := nil;');
      end;
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        if AUseType then
          Writeln(F, Indent, 'Func', AClass.Name, LIntf.Name, Method.Name, ': T', LIntf.Name, Method.Name, ' = nil;')
        else
          Writeln(F, Indent, 'Func', AClass.Name, LIntf.Name, Method.Name, ' := nil;');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          if AUseType then
            Writeln(F, Indent, 'Func', AClass.Name, 'Get', LIntf.Name, Prop.Name, ': TGet', LIntf.Name, Prop.Name, ' = nil;')
          else
            Writeln(F, Indent, 'Func', AClass.Name, 'Get', LIntf.Name, Prop.Name, ' := nil;');
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          if AUseType then
            Writeln(F, Indent, 'Func', AClass.Name, 'Set', LIntf.Name, Prop.Name, ': TSet', LIntf.Name, Prop.Name, ' = nil;')
          else
            Writeln(F, Indent, 'Func', AClass.Name, 'Set', LIntf.Name, Prop.Name, ' := nil;');
        end;
      end;
    end;
  end;
  // write event handler setters
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and LIntf.FlagDispEvents then
    begin
      Writeln(F, Indent, '// Event handler setters:');
      for Method in LIntf.Methods do
      begin
        if AUseType then
          Writeln(F, Indent, 'FuncSet', AClass.Name, LIntf.Name, Method.Name, ': TSet', AClass.Name, LIntf.Name, Method.Name, ' = nil;')
        else
          Writeln(F, Indent, 'FuncSet', AClass.Name, LIntf.Name, Method.Name, ' := nil;')
      end;
    end;
  end;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCallerLibLoads(AClass: TPWIGClass);

  procedure WriteLibLoad(const AFuncName: string);
  begin
    Writeln(F, Indent, 'Func', AFuncName, ' := GetProcAddress(LibModule, ''', AFuncName, ''');');
    Writeln(F, Indent, 'if not Assigned(Func', AFuncName, ') then LibLoadError(''', AFuncName, ''');');
  end;

var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
begin
  WriteIntfElementProps(AClass);
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
    begin
      // use constructor and destructor only for default interface
      if Ref.FlagDefault then
      begin
        Writeln(F, Indent, '// Constructor:');
        WriteLibLoad(AClass.Name + 'Create');
        Writeln(F, Indent, '// Destructor:');
        WriteLibLoad(AClass.Name + 'Destroy');
      end;
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        WriteLibLoad(AClass.Name + LIntf.Name + Method.Name);
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteLibLoad(AClass.Name + 'Get' + LIntf.Name + Prop.Name);
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteLibLoad(AClass.Name + 'Set' + LIntf.Name + Prop.Name);
        end;
      end;
    end;
  end;
  // write event handler setters
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and LIntf.FlagDispEvents then
    begin
      Writeln(F, Indent, '// Event handler setters:');
      for Method in LIntf.Methods do
      begin
        WriteLibLoad('Set' + AClass.Name + LIntf.Name + Method.Name);
      end;
    end;
  end;
end;

procedure TPWIGGenPascal.WriteCallerImplementations(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
begin
  WriteIntfElementProps(AClass);
  // write event handler callbacks
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and LIntf.FlagDispEvents then
    begin
      Writeln(F, Indent, '// Event handler callbacks:');
      for Method in LIntf.Methods do
      begin
        WriteCallerEventCallback(AClass, LIntf, Method, Ref.FlagDefault);
      end;
    end;
  end;

  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
    begin
      Writeln(F, Indent, '// Constructor:');
      WriteCallerConstructor(AClass, LIntf);
      Writeln(F, Indent, '// Destructor:');
      WriteCallerDestructor(AClass, LIntf);
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        WriteCallerMethod(AClass, LIntf, Method, Ref.FlagDefault, False, '');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteCallerMethod(AClass, LIntf, Prop, Ref.FlagDefault, True, 'Get');
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteCallerMethod(AClass, LIntf, Prop, Ref.FlagDefault, False, 'Set');
        end;
      end;
    end;
  end;
  WriteSpace;
end;

function TPWIGGenPascal.WriteCallerUnicodeStringDeclarations(
  AMethod: TPWIGMethod; AGetter: Boolean): Boolean;
var
  Param: TPWIGParam;
  ParamName: string;
begin
  // automated string management for UnicodeString, caller side
  Result := False;
  for Param in AMethod.Params do
    if (Param.ParamType.BaseType = btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn)  then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      if not Result then
      begin
        Writeln(F, Indent, 'var');
        IncIndent;
      end;
      Result := True;
      Writeln(F, Indent, 'AnsiString__', ParamName, ': AnsiString; Length__', ParamName, ': LongInt;');
    end;
  if Result then
  begin
    DecIndent;
  end;
end;

function TPWIGGenPascal.WriteCallerUnicodeStringManagement1(
  AMethod: TPWIGMethod; AGetter: Boolean): Boolean;
var
  Param: TPWIGParam;
  ParamName: string;
begin
  // automated string management for UnicodeString, caller side
  for Param in AMethod.Params do
    if (Param.ParamType.BaseType = btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn)  then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      Writeln(F, Indent, 'Length__', ParamName, ' := 0;');
    end;
  Result := True;
end;

function TPWIGGenPascal.WriteCallerUnicodeStringManagement2(
  AMethod: TPWIGMethod; AGetter: Boolean): Boolean;
var
  Param: TPWIGParam;
  ParamName: string;
begin
  // automated string management for UnicodeString
  for Param in AMethod.Params do
    if (Param.ParamType.BaseType = btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn)  then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      Writeln(F, Indent, 'SetLength(AnsiString__', ParamName, ', Max(Length__', ParamName, ', 1));');
    end;
  Result := True;
end;

function TPWIGGenPascal.WriteCallerUnicodeStringManagement3(
  AMethod: TPWIGMethod; AGetter: Boolean): Boolean;
var
  Param, RetVal: TPWIGParam;
  ParamName, S: string;
begin
  // automated string management for UnicodeString
  if AGetter then
    RetVal := AMethod.Params.Last
  else
    RetVal := AMethod.Params.FindRetVal;
  for Param in AMethod.Params do
    if (Param.ParamType.BaseType = btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn)  then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      if Param <> RetVal then
        S := ParamName
      else
        S := 'Result';
      Writeln(F, Indent, 'if Length__', ParamName, ' > 0 then ', S, ' := LibUtf8String2String(AnsiString__', ParamName, ') else ', S, ' := '''';')
    end;
  Result := True;
end;

procedure TPWIGGenPascal.WriteInterfaceFile(const AFileName: string);
var
  LIntf: TPWIGInterface;
  LCls: TPWIGClass;
  LAlias: TPWIGAlias;
  LEnum: TPWIGEnum;
  Name, Path, Ext, GeneratedFile, IntfName: string;
begin
  Path := ExtractFilePath(AFileName);
  Name := ExtractFileRawName(AFileName);
  Ext := ExtractFileExt(AFileName);

  // write interface file (usable both for callee and for caller)
  IntfName := Name + '_intf';

  GeneratedFile := Path + IntfName + Ext;

  AssignFile(F, GeneratedFile);
  try
    try
      Rewrite(F);

      // write file header (warning etc.)
      Writeln(F, Indent, '// ************************************************************************');
      Writeln(F, Indent, '// This file contains common interface for both the caller and the callee.');
      Writeln(F, Indent, '// -------');
      Writeln(F, Indent, '// WARNING');
      Writeln(F, Indent, '// -------');
      Writeln(F, Indent, '// This file was generated by PWIG. Do not edit.');
      Writeln(F, Indent, '// File generated on ', DateTimeToStr(Now));
      WriteSpace;

      // begin to write library
      Writeln(F, Indent, 'unit ', IntfName, ';');
      WriteSpace;
      Writeln(F, Indent, '{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}');
      Writeln(F, Indent, '{$MINENUMSIZE 4} // needed for correct interop with .NET');
      WriteSpace;
      Writeln(F, Indent, '// Library properties:');
      WriteIntfElementProps(FPWIG);

      // interface
      Writeln(F, Indent, 'interface');
      WriteSpace;

      // write uses clause
      // now hardcoded, later might be automated when needed
      Writeln(F, Indent, 'uses');
      IncIndent;
      Writeln(F, Indent, 'SysUtils;');
      DecIndent;
      WriteSpace;

      // write library ID
      Writeln(F, Indent, 'const');
      IncIndent;
      Writeln(F, Indent, 'cLibGUID = ''', FPWIG.GUID, ''';');
      DecIndent;
      WriteSpace;

      Writeln(F, Indent, 'type');
      WriteSpace;
      IncIndent;

      // write forward declarations
      Writeln(F, Indent, '// Forward declarations:');
      for LIntf in FPWIG.Interfaces do
        Writeln(F, Indent, LIntf.Name, ' = type Int64;');
      WriteSpace;

      // write enums
      for LEnum in FPWIG.Enums do
        WriteIntfEnumProps(LEnum);

      // write aliases
      for LAlias in FPWIG.Aliases do
        WriteIntfAliasProps(LAlias);

      // write interfaces
      for LIntf in FPWIG.Interfaces do
        WriteIntfInterfaceProps(LIntf);

      // write classes
      for LCls in FPWIG.Classes do
        WriteIntfClassProps(LCls);

      Writeln(F, Indent, '// Library helper functions');
      Writeln(F, Indent, 'function String2LibUtf8String(const AText: string): AnsiString;');
      Writeln(F, Indent, 'function LibUtf8String2String(const AText: AnsiString): string;');
      WriteSpace;

      // end of interface
      DecIndent;

      // implementation
      Writeln(F, Indent, 'implementation');
      WriteSpace;

      Writeln(F, Indent, 'function String2LibUtf8String(const AText: string): AnsiString;');
      WritePascalBegin;
      Writeln(F, Indent, '{$IFDEF FPC}');
      Writeln(F, Indent, 'Result := AText; // FPC string is UTF8 by default');
      Writeln(F, Indent, '{$ELSE}');
      Writeln(F, Indent, 'Result := UTF8Encode(AText);');
      Writeln(F, Indent, '{$ENDIF}');
      WritePascalEnd;
      WriteSpace;

      Writeln(F, Indent, 'function LibUtf8String2String(const AText: AnsiString): string;');
      WritePascalBegin;
      Writeln(F, Indent, '{$IFDEF FPC}');
      Writeln(F, Indent, 'Result := AText; // FPC string is UTF8 by default');
      Writeln(F, Indent, '{$ELSE}');
      Writeln(F, Indent, 'Result := UTF8ToString(AText);');
      Writeln(F, Indent, '{$ENDIF}');
      WritePascalEnd;
      WriteSpace;

      // finish
      Writeln(F, Indent, 'end.');

      Writeln('Pascal common interface file generated: ', GeneratedFile);
    except
      Writeln('Could not generate Pascal common interface file: ', GeneratedFile);
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TPWIGGenPascal.WriteCalleeFile(const AFileName: string);
var
  LCls: TPWIGClass;
  LIntf: TPWIGInterface;
  Name, Path, Ext, GeneratedFile, IntfName, ImplName: string;
begin
  Path := ExtractFilePath(AFileName);
  Name := ExtractFileRawName(AFileName);
  Ext := ExtractFileExt(AFileName);

  // write wrapper file for the callee
  IntfName := Name + '_intf';
  ImplName := Name + '_callee';

  GeneratedFile := Path + ImplName + Ext;

  AssignFile(F, GeneratedFile);
  try
    try
      Rewrite(F);

      // write file header (warning etc.)
      Writeln(F, Indent, '// ************************************************************************');
      Writeln(F, Indent, '// This file implements library exports for the callee.');
      Writeln(F, Indent, '// -------');
      Writeln(F, Indent, '// WARNING');
      Writeln(F, Indent, '// -------');
      Writeln(F, Indent, '// This file was generated by PWIG. Do not edit.');
      Writeln(F, Indent, '// File generated on ', DateTimeToStr(Now));
      WriteSpace;

      // begin to write library
      Writeln(F, Indent, 'unit ', ImplName, ';');
      WriteSpace;
      Writeln(F, Indent, '{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}');
      WriteSpace;
      Writeln(F, Indent, '// Library properties:');
      WriteIntfElementProps(FPWIG);

      // interface
      Writeln(F, Indent, 'interface');
      WriteSpace;

      // write uses clause
      // now hardcoded, later might be automated when needed
      Writeln(F, Indent, 'uses');
      IncIndent;
      Writeln(F, Indent, 'Classes, ', IntfName, ';');
      DecIndent;
      WriteSpace;

      ImplName := ImplName + '_impl';

      // entire declaration part is commented out because the classes must be implemented in other file
      Writeln(F, Indent, '// Copy these class declarations into ', ImplName, '.pas and implement them there.');
      Writeln(F, Indent, '// Note: ', ImplName, '.pas is not maintained by PWIG and must be implemented by library author!');
      Writeln(F, Indent, '(*');

      Writeln(F, Indent, 'type');
      WriteSpace;
      IncIndent;

      // write forward declarations
      Writeln(F, Indent, '// Forward declarations:');
      for LCls in FPWIG.Classes do
        Writeln(F, Indent, 'T', LCls.Name, ' = class;');
      WriteSpace;

      // write class interfaces (implementation must be done by library author)
      for LCls in FPWIG.Classes do
        WriteCalleeDeclarations(LCls);

      // end of commented out section
      DecIndent;
      Writeln(F, Indent, '// End of declarations to be copied to ', ImplName, '.pas file.');
      Writeln(F, Indent, '*)');
      WriteSpace;

      // write event sink wrapper classes
      for LIntf in FPWIG.Interfaces do
        if LIntf.FlagDispEvents then
          WriteCalleeEventSinkDeclarations(LIntf);

      // write library identification code - interface part
      Writeln(F, Indent, '// Library identification code');
      Writeln(F, Indent, 'function GetLibGUID: PAnsiChar; ', CallingConvToString(nil),';');
      WriteSpace;

      // write exported function declarations
      for LCls in FPWIG.Classes do
        WriteCalleeExportedFuncs(LCls, False);

      WriteSpace;

      // implementation
      Writeln(F, Indent, 'implementation');
      WriteSpace;

      // write uses clause
      // now hardcoded, later might be automated when needed
      Writeln(F, Indent, 'uses');
      IncIndent;
      Writeln(F, Indent, 'Math, SysUtils', ', ', ImplName, ';');
      DecIndent;
      WriteSpace;

      // write library identification code - implementation part
      Writeln(F, Indent, '// Library identification code');
      Writeln(F, Indent, 'function GetLibGUID: PAnsiChar; ', CallingConvToString(nil),';');
      WritePascalBegin;
      Writeln(F, Indent, 'Result := cLibGUID;');
      WritePascalEnd;
      WriteSpace;

      // write exported function bodies
      for LCls in FPWIG.Classes do
        WriteCalleeExportedFuncs(LCls, True);

      // write event sink wrapper classes
      for LIntf in FPWIG.Interfaces do
        if LIntf.FlagDispEvents then
          WriteCalleeEventSinkImplementations(LIntf);

      // write library exports
      Writeln(F, Indent, '// Copy these exports into your main library file.');
      Writeln(F, Indent, '// This is needed because of FPC bug #tbd.');
      Writeln(F, Indent, '(*');
      Writeln(F, Indent, 'exports');
      IncIndent;
      InitDashSep;
      WriteDashSep;
      Writeln(F, Indent, 'GetLibGUID');
      for LCls in FPWIG.Classes do
        WriteCalleeExports(LCls);
      Writeln(F, Indent, ';');
      DecIndent;
      Writeln(F, Indent, '*)');
      WriteSpace;

      // finish
      Writeln(F, Indent, 'end.');

      Writeln('Pascal callee interface file generated: ', GeneratedFile);
    except
      Writeln('Could not generate Pascal callee interface file: ', GeneratedFile);
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TPWIGGenPascal.WriteCallerFile(const AFileName: string);
var
  LCls: TPWIGClass;
  Name, Path, Ext, GeneratedFile, IntfName, ImplName: string;
begin
  Path := ExtractFilePath(AFileName);
  Name := ExtractFileRawName(AFileName);
  Ext := ExtractFileExt(AFileName);

  // write wrapper file for the caller
  IntfName := Name + '_intf';
  ImplName := Name + '_caller';

  GeneratedFile := Path + ImplName + Ext;

  AssignFile(F, GeneratedFile);
  try
    try
      Rewrite(F);

      // write file header (warning etc.)
      Writeln(F, Indent, '// ************************************************************************');
      Writeln(F, Indent, '// This file implements library imports for the caller.');
      Writeln(F, Indent, '// -------');
      Writeln(F, Indent, '// WARNING');
      Writeln(F, Indent, '// -------');
      Writeln(F, Indent, '// This file was generated by PWIG. Do not edit.');
      Writeln(F, Indent, '// File generated on ', DateTimeToStr(Now));
      WriteSpace;

      // begin to write library
      Writeln(F, Indent, 'unit ', ImplName, ';');
      WriteSpace;
      Writeln(F, Indent, '{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}');
      WriteSpace;
      Writeln(F, Indent, '// Library properties:');
      WriteIntfElementProps(FPWIG);

      // interface
      Writeln(F, Indent, 'interface');
      WriteSpace;

      // write uses clause
      // now hardcoded, later might be automated when needed
      Writeln(F, Indent, 'uses');
      IncIndent;
      Writeln(F, Indent, '{$IFDEF FPC}');
      Writeln(F, Indent, 'DynLibs,');
      Writeln(F, Indent, '{$ELSE}');
      Writeln(F, Indent, '{$IFDEF MSWINDOWS}');
      Writeln(F, Indent, 'Windows,');
      Writeln(F, Indent, '{$ENDIF}');
      Writeln(F, Indent, '{$ENDIF}');
      Writeln(F, Indent, IntfName, ';');
      DecIndent;
      WriteSpace;


      Writeln(F, Indent, 'type');
      WriteSpace;
      IncIndent;

      // write forward declarations
      Writeln(F, Indent, '// Forward declarations:');
      for LCls in FPWIG.Classes do
        Writeln(F, Indent, 'T', LCls.Name, ' = class;');
      WriteSpace;

      // write class interfaces
      for LCls in FPWIG.Classes do
        WriteCallerDeclarations(LCls);

      // library loader
      Writeln(F, Indent, 'function ', FPWIG.Name, 'LibLoad(const FileName: string): Boolean;');
      // library unloader
      Writeln(F, Indent, 'procedure ', FPWIG.Name, 'LibUnload;');
      // library load error string (FPC only)
      Writeln(F, Indent, 'var LibLoadErrorMsg: string;');

      // end of interface
      DecIndent;

      // implementation
      Writeln(F, Indent, 'implementation');
      WriteSpace;

      // write uses clause
      // now hardcoded, later might be automated when needed
      Writeln(F, Indent, 'uses');
      IncIndent;
      Writeln(F, Indent, 'Math, SysUtils;');
      DecIndent;
      WriteSpace;

      // write library imports
      Writeln(F, Indent, 'const');
      IncIndent;
      Writeln(F, Indent, 'LibModule: HMODULE = 0;');
      for LCls in FPWIG.Classes do
        WriteCallerPointers(LCls, True);
      DecIndent;

      // write library generous error function
      Writeln(F, Indent, 'procedure LibError(const AMessage: string);');
      WritePascalBegin;
      Writeln(F, Indent, 'raise Exception.Create(AMessage);');
      WritePascalEnd;
      WriteSpace;

      // write library calling error function
      Writeln(F, Indent, 'procedure LibCallError(const AFuncName: string);');
      WritePascalBegin;
      Writeln(F, Indent, 'LibError(Format(''Error while calling library function %s!'', [AFuncName]));');
      WritePascalEnd;
      WriteSpace;

      // write library loading error function
      Writeln(F, Indent, 'procedure LibLoadError(const AFuncName: string);');
      WritePascalBegin;
      Writeln(F, Indent, 'LibError(Format(''Requested function %s does not exist in the library!'', [AFuncName]));');
      WritePascalEnd;
      WriteSpace;

      // write library loader implementation
      Writeln(F, Indent, 'function ', FPWIG.Name, 'LibLoad(const FileName: string): Boolean;');
      Writeln(F, Indent, 'type');
      IncIndent;
      Writeln(F, Indent, 'T_FuncLibID = function: PAnsichar; ' + CallingConvToString(nil) + ';');
      DecIndent;
      Writeln(F, Indent, 'var');
      IncIndent;
      Writeln(F, Indent, '_FuncLibID: T_FuncLibID;');
      DecIndent;
      WritePascalBegin;
      Writeln(F, Indent, 'Result := False;');
      WriteTry;
      Writeln(F, Indent, 'if LibModule = 0 then');
      WritePascalBegin;
      Writeln(F, Indent, 'LibModule := LoadLibrary({$IFnDEF FPC}PChar{$ENDIF}(FileName));');
      Writeln(F, Indent, '{$IFDEF FPC}');
      Writeln(F, Indent, 'LibLoadErrorMsg := GetLoadErrorStr;');
      Writeln(F, Indent, '{$ENDIF}');
      WritePascalEnd;
      Writeln(F, Indent, 'if LibModule <> 0 then');
      WritePascalBegin;
      Writeln(F, Indent, '// Call library identification code first');
      Writeln(F, Indent, '_FuncLibID := GetProcAddress(LibModule, ''GetLibGUID'');');
      Writeln(F, Indent, 'if not Assigned(_FuncLibID) then LibLoadError(''GetLibGUID'');');
      Writeln(F, Indent, 'if _FuncLibID <> cLibGUID then LibError(''Incompatible library interface!'');');
      WriteSpace;
      for LCls in FPWIG.Classes do
        WriteCallerLibLoads(LCls);
      Writeln(F, Indent, 'Result := True;');
      WritePascalEnd;
      WriteExcept;
      WritePascalEnd;
      WriteSpace;

      // write library unloader implementation
      Writeln(F, Indent, 'procedure ', FPWIG.Name, 'LibUnload;');
      WritePascalBegin;
      Writeln(F, Indent, 'if LibModule <> 0 then');
      IncIndent;
      Writeln(F, Indent, 'FreeLibrary(LibModule);');
      DecIndent;
      Writeln(F, Indent, 'LibModule := 0;');
      for LCls in FPWIG.Classes do
        WriteCallerPointers(LCls, False);
      WritePascalEnd;
      WriteSpace;

      // write class method bodies
      for LCls in FPWIG.Classes do
        WriteCallerImplementations(LCls);

      // finish
      Writeln(F, Indent, 'end.');

      Writeln('Pascal caller interface file generated: ', GeneratedFile);
    except
      Writeln('Could not generate Pascal caller interface file: ', GeneratedFile);
    end;
  finally
    CloseFile(F);
  end;

end;

end.

