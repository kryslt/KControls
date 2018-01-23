{ @abstract(This unit contains PWIG C++ wrapper class generator (usable by C++ IDEs))
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(2 Jan 2017)

  Copyright © Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  Generated outputs tested in:
  -Visual Studio Community 2015 (unmanaged code, callee + caller)

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }

unit pwig_cpp;

{$mode delphi}

interface

uses
  Classes, SysUtils, PWIGGen;

type

  TPWIGGenCppMethodStyle = (
    msIntfDeclaration,
    msLibCallGetLength,
    msLibCall,
    msLibExport,
    msClassDeclaration,
    msClassImplementation,
    msClassCall,
    msEventCall,
    msEventDeclaration,
    msEventSinkCallGetLength,
    msEventSinkCall,
    msEventCallback
  );

  { TPWIGGenCpp }

  TPWIGGenCpp = class(TPWIGGenerator)
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
    procedure WriteCurlyBegin;
    procedure WriteCurlyEnd(ANewLine: Boolean = True);
    procedure WriteCurlyEndElse;
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
    function TypeToNullValue(AType: TPWIGType): string;
    function ReplaceNotAllowedParamName(AName: string): string; virtual;
    function ReturnParamName(AMethod: TPWIGMethod; AGetter: Boolean): string;
    procedure WriteIntfElementProps(AElement: TPWIGElement); virtual;
    procedure WriteIntfAliasProps(AAlias: TPWIGAlias); virtual;
    procedure WriteIntfEnumProps(AEnum: TPWIGEnum); virtual;
    procedure WriteIntfInterfaceProps(AIntf: TPWIGInterface); virtual;
    procedure WriteIntfClassProps(AClass: TPWIGClass); virtual;
    procedure WriteMethod(const AClassName: string; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      AStyle: TPWIGGenCppMethodStyle; ADefaultInterface, AGetter: Boolean; const APrefix: string;
      const ASpecifier: string = ''); virtual;

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
    procedure WriteCalleeImplementations(AClass: TPWIGClass); virtual;
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
    procedure WriteCallerCallEventMethod(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      ADefaultInterface, AGetter: Boolean; const APrefix: string); virtual;
    procedure WriteCallerEventCallback(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      ADefaultInterface: Boolean); virtual;
    procedure WriteCallerDeclarations(AClass: TPWIGClass); virtual;
    procedure WriteCallerPointers(AClass: TPWIGClass; AUseType: Boolean); virtual;
    procedure WriteCallerLibLoads(AClass: TPWIGClass); virtual;
    procedure WriteCallerImplementations(AClass: TPWIGClass); virtual;
    function WriteCallerOutputParamInits(AMethod: TPWIGMethod; AGetter: Boolean): Boolean;
    function WriteCallerUnicodeStringDeclarations(AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;
    function WriteCallerUnicodeStringManagement1(AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;
    function WriteCallerUnicodeStringManagement2(AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;
    function WriteCallerUnicodeStringManagement3(AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;

    procedure WriteInterfaceHeaderFile(const AFileName: string);
    procedure WriteInterfaceSourceFile(const AFileName: string);
    procedure WriteCalleeHeaderFile(const AFileName: string);
    procedure WriteCalleeSourceFile(const AFileName: string);
    procedure WriteCallerHeaderFile(const AFileName: string);
    procedure WriteCallerSourceFile(const AFileName: string);
  public
    constructor Create(AOwner: TPWIG); override;
    procedure SaveCalleeFiles(const AFileName: string); override;
    procedure SaveCallerFiles(const AFileName: string); override;
  end;

implementation

uses
  KFunctions;

{ TPWIGGenCpp }

constructor TPWIGGenCpp.Create(AOwner: TPWIG);
begin
  inherited Create(AOwner);
  FIndent := '';
end;

procedure TPWIGGenCpp.IncIndent;
begin
  FIndent := FIndent + '  ';
end;

procedure TPWIGGenCpp.DecIndent;
begin
  Delete(FIndent, 1, 2);
end;

procedure TPWIGGenCpp.ClearFlags;
begin
  FFlags := '';
end;

procedure TPWIGGenCpp.AddFlag(AFlag: Boolean; const AFlagText: string);
begin
  if AFlag then
  begin
    if FFlags <> '' then
      FFlags := FFlags + ', ';
    FFlags := FFlags + AFlagText;
  end;
end;

procedure TPWIGGenCpp.InitDashSep;
begin
  FFirstDashSep := True;
end;

function TPWIGGenCpp.ParamDirection(AMethod: TPWIGMethod;
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

function TPWIGGenCpp.ParamDirectionToString(ADirection: TPWIGParamDirection
  ): string;
begin
  Result := '';
  case ADirection of
    pdIn: Result := '';
    pdOut: Result := '&';
    pdInOut: Result := '&';
  end;
end;

function TPWIGGenCpp.CallingConvToString(AMethod: TPWIGMethod): string;
var
  Conv: TPWIGCallingConv;
begin
  if (AMethod <> nil) and (AMethod.CallingConv <> ccNone) then
    Conv := AMethod.CallingConv
  else
    Conv := FPWIG.GlobalCallingConv;
  Result := '';
  case Conv of
    ccCDecl: Result := '__cdecl';
    ccPascal: Result := '__pascal';
    ccStdCall: Result := '__stdcall';
    ccSafeCall: Result := '__safecall';
  end;
end;

function TPWIGGenCpp.TypeToString(AType: TPWIGType): string;
begin
  Result := 'void';
  case AType.BaseType of
    btLongInt: Result := 'int32_t';
    btLongWord: Result := 'uint32_t';
    btSmallInt: Result := 'int16_t';
    btWord: Result := 'uint16_t';
    btInt64: Result := 'int64_t';
    btUInt64: Result := 'uint64_t';
    btSingle: Result := 'single';
    btDouble: Result := 'double';
    btUnicodeString: Result := 'std::wstring'; // Unicode string
    btRawByteString: Result := 'uint8_t *';
    btCurrency: Result := 'double';
    btDateTime: Result := 'time_t';
    btEnum: Result := FPWIG.Enums.FindName(AType.CustomTypeGUID, AType.CustomTypeName);
    btAlias: Result := FPWIG.Aliases.FindName(AType.CustomTypeGUID, AType.CustomTypeName);
    btInterface: Result := FPWIG.Interfaces.FindName(AType.CustomTypeGUID, AType.CustomTypeName);
  end;
end;

function TPWIGGenCpp.TypeToNullValue(AType: TPWIGType): string;
var
  LEnum: TPWIGEnum;
  LAlias: TPWIGAlias;
begin
  Result := '';
  case AType.BaseType of
    btLongInt: Result := '0';
    btLongWord: Result := '0';
    btSmallInt: Result := '0';
    btWord: Result := '0';
    btInt64: Result := '0';
    btUInt64: Result := '0';
    btSingle: Result := '0';
    btDouble: Result := '0';
    btUnicodeString: Result := 'L""';
    btRawByteString: Result := 'NULL';
    btCurrency: Result := '0';
    btDateTime: Result := '0';
    btEnum:
      begin
        LEnum := FPWIG.Enums.Find(AType.CustomTypeGUID, AType.CustomTypeName);
        Result := '(' + LEnum.Name + ')' + LEnum.Elements[0].Name;
      end;
    btAlias:
      begin
        LAlias := FPWIG.Aliases.Find(AType.CustomTypeGUID, AType.CustomTypeName);
        Result := TypeToNullValue(LAlias.AliasedType);
      end;
    btInterface: Result := '0';
  end;
end;

function TPWIGGenCpp.ReplaceNotAllowedParamName(AName: string): string;
begin
  Result := AName;
end;

function TPWIGGenCpp.ReturnParamName(AMethod: TPWIGMethod; AGetter: Boolean): string;
var
  RetVal: TPWIGParam;
begin
  if AGetter then
    RetVal := AMethod.Params.Last
  else
    RetVal := AMethod.Params.FindRetVal;
  if RetVal <> nil then
    Result := RetVal.Name
  else
    Result := '';
end;

procedure TPWIGGenCpp.WriteDashSep;
begin
  if not FFirstDashSep then
    Writeln(F, ',');
  FFirstDashSep := False;
end;

procedure TPWIGGenCpp.WriteSpace;
begin
  Writeln(F);
end;

procedure TPWIGGenCpp.WriteCurlyBegin;
begin
  Writeln(F, Indent, '{');
  IncIndent;
end;

procedure TPWIGGenCpp.WriteCurlyEnd(ANewLine: Boolean);
begin
  DecIndent;
  Write(F, Indent, '}');
  if ANewLine then
    Writeln(F);
end;

procedure TPWIGGenCpp.WriteCurlyEndElse;
begin
  DecIndent;
  Writeln(F, Indent, '}');
  Writeln(F, Indent, 'else');
end;

procedure TPWIGGenCpp.WriteTry;
begin
  Writeln(F, Indent, 'try');
  WriteCurlyBegin;
end;

procedure TPWIGGenCpp.WriteExcept(const AMessage: string);
begin
  WriteCurlyEnd;
  Writeln(F, Indent, 'catch (std::exception& e)');
  WriteCurlyBegin;
  if AMessage <> '' then
  begin
    IncIndent;
    Writeln(F, Indent, AMessage);
    DecIndent;
  end;
  WriteCurlyEnd;
end;

procedure TPWIGGenCpp.WriteExceptLibCall;
begin
  WriteExcept('LibCallError(LibUtf8String2String((char*)e.what()));');
end;

function TPWIGGenCpp.GetDescription: string;
begin
  Result := 'C++ (unmanaged code, callee + caller). Usable in Visual Studio, maybe other IDEs.';
end;

function TPWIGGenCpp.GetName: string;
begin
  Result := 'Cpp';
end;

procedure TPWIGGenCpp.SaveCalleeFiles(const AFileName: string);
begin
  WriteInterfaceHeaderFile(AFileName);
  WriteInterfaceSourceFile(AFileName);
  WriteCalleeHeaderFile(AFileName);
  WriteCalleeSourceFile(AFileName);
end;

procedure TPWIGGenCpp.SaveCallerFiles(const AFileName: string);
begin
  WriteInterfaceHeaderFile(AFileName);
  WriteInterfaceSourceFile(AFileName);
  WriteCallerHeaderFile(AFileName);
  WriteCallerSourceFile(AFileName);
end;

procedure TPWIGGenCpp.WriteIntfElementProps(AElement: TPWIGElement);
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

procedure TPWIGGenCpp.WriteIntfAliasProps(AAlias: TPWIGAlias);
begin
  Writeln(F, Indent, '// Alias type properties:');
  WriteIntfElementProps(AAlias);
  Writeln(F, Indent, 'typedef ', TypeToString(AAlias.AliasedType), ' ', AAlias.Name, ';');
  WriteSpace;
end;

procedure TPWIGGenCpp.WriteIntfEnumProps(AEnum: TPWIGEnum);
var
  I: Integer;
  Elem: TPWIGEnumElement;
begin
  Writeln(F, Indent, '// Enumerated type properties:');
  WriteIntfElementProps(AEnum);
  Writeln(F, Indent, 'typedef enum');
  WriteCurlyBegin;
  for I := 0 to AEnum.Elements.Count - 1 do
  begin
    Elem := AEnum.Elements[I];
    Write(F, Indent, Elem.Name, ' = ', Elem.Value);
    if I < AEnum.Elements.Count - 1 then
      Writeln(F, ',')
    else
      Writeln(F);
  end;
  WriteCurlyEnd(False);
  Writeln(F, ' ', AEnum.Name, ';');
  WriteSpace;
end;

procedure TPWIGGenCpp.WriteIntfInterfaceProps(AIntf: TPWIGInterface);
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

procedure TPWIGGenCpp.WriteIntfClassProps(AClass: TPWIGClass);
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
            Writeln(F, Indent, 'typedef bool (', CallingConvToString(nil), ' *TSet', AClass.Name, LIntf.Name, Method.Name, ')(',
              LIntfDef.Name, ' ItemHandle, ', LIntf.Name, ' EventSink, T', LIntf.Name, Method.Name, ' EventHandler);');
      end else
      begin
        // use constructor and destructor only for default interface
        if Ref.FlagDefault then
        begin
          Writeln(F, Indent, 'typedef bool (', CallingConvToString(nil), ' *T', AClass.Name, 'Create)(', LIntf.Name, ' &ItemHandle);');
          Writeln(F, Indent, 'typedef bool (', CallingConvToString(nil), ' *T', AClass.Name, 'Destroy)(', LIntf.Name, ' ItemHandle);');
        end;
      end;
  end;
  WriteSpace;
end;

procedure TPWIGGenCpp.WriteMethod(const AClassName: string;
  AIntf: TPWIGInterface; AMethod: TPWIGMethod; AStyle: TPWIGGenCppMethodStyle;
  ADefaultInterface, AGetter: Boolean; const APrefix: string;
  const ASpecifier: string);

  procedure WriteLibCallUnicodeStringParam(AParamDir: TPWIGParamDirection; const AParamName: string);
  begin
    if AParamDir = pdIn then
      Write(F, '(char *)String2LibUtf8String(', AParamName, ').c_str()')
    else if AStyle in [msLibCallGetLength, msEventSinkCallGetLength] then
      Write(F, 'NULL, Length__', AParamName)
    else
      Write(F, 'AnsiString__', AParamName, ', Length__', AParamName);
  end;

var
  Param, RetVal: TPWIGParam;
  ParamDir: TPWIGParamDirection;
  CallingConv, HandleParam, IntfName, ParamName, ParamSpecifier, RetValType, RetValAssignment: string;
  FirstParam: Boolean;
begin
  RetValType := 'void';
  if AStyle in [msIntfDeclaration, msLibExport, msEventCallback] then
  begin
    CallingConv := CallingConvToString(AMethod);
    HandleParam :=  AIntf.Name + ' ItemHandle';
    RetVal := nil;
    IntfName := AIntf.name;
  end
  else if AStyle in [msLibCall, msLibCallGetLength] then
  begin
    CallingConv := '';
    HandleParam := 'm_ItemHandle';
    if AGetter then
      RetVal := AMethod.Params.Last
    else
      RetVal := AMethod.Params.FindRetVal;
    IntfName := AIntf.Name;
  end
  else if AStyle in [msEventSinkCall, msEventSinkCallGetLength] then
  begin
    CallingConv := '';
    HandleParam := 'm_' + AMethod.Name + 'EventSink';
    RetVal := nil;
    RetValAssignment := '';
    IntfName := '';
  end else
  begin
    CallingConv := '';
    HandleParam := '';
    if AGetter then
      RetVal := AMethod.Params.Last
    else
      RetVal := AMethod.Params.FindRetVal;
    if RetVal <> nil then
    begin
      ParamName := ReplaceNotAllowedParamName(RetVal.Name);
      if RetVal.ParamType.BaseType = btUnicodeString then
        ParamName := 'String__' + AClassName + AIntf.Name + AMethod.Name + ParamName
      else
        ParamName := '' + ParamName;
      RetValAssignment := ParamName + ' = ';
      RetValType := TypeToString(RetVal.ParamType);
    end else
    begin
      RetValAssignment :=  '';
    end;
    if ADefaultInterface then
      IntfName := ''
    else
      IntfName := AIntf.Name;
  end;
  case AStyle of
    msIntfDeclaration:
      Write(F, Indent, 'typedef bool (', CallingConv, ' *T', APrefix, IntfName, AMethod.Name, ')(', HandleParam);
    msLibCall, msLibCallGetLength:
      Write(F, Indent, 'Func', AClassName, APrefix, IntfName, AMethod.Name, '(', HandleParam);
    msLibExport:
      Write(F, Indent, 'LIB_EXPORT bool ', CallingConv, ' ', AClassName, APrefix, IntfName, AMethod.Name, '(', HandleParam);
    msEventCallback:
      Write(F, Indent, 'bool ', CallingConv, ' ', AClassName, APrefix, IntfName, AMethod.Name, '(', HandleParam);
    msClassDeclaration:
      Write(F, Indent, ASpecifier, RetValType, ' ', APrefix, IntfName, AMethod.Name, '(');
    msClassImplementation:
      Write(F, Indent, ASpecifier, RetValType, ' ', AClassName, '::', APrefix, IntfName, AMethod.Name, '(');
    msClassCall:
      Write(F, Indent, RetValAssignment, '__Cls->', APrefix, IntfName, AMethod.Name, '(');
    msEventCall:
      Write(F, Indent, RetValAssignment, '(this->*', APrefix, IntfName, AMethod.Name, ')(');
    msEventDeclaration:
      Write(F, Indent, 'typedef bool (', AClassName, '::*T', IntfName, AMethod.Name, APrefix, ')(');
    msEventSinkCall, msEventSinkCallGetLength:
      Write(F, Indent, 'm_', AMethod.Name, 'EventHandler(', HandleParam);
  end;
  if (AMethod.Params.Count = 0) or (AMethod.Params.Count = 1) and (RetVal <> nil) then
  begin
    if (RetVal <> nil) and (AStyle in [msLibCall, msLibCallGetLength]) then
    begin
      Write(F, ', ');
      ParamName := ReplaceNotAllowedParamName(RetVal.Name);
      if RetVal.ParamType.BaseType = btUnicodeString then
      begin
        ParamDir := ParamDirection(AMethod, RetVal, AGetter);
        WriteLibCallUnicodeStringParam(ParamDir, ParamName);
      end else
        Write(F, '', ParamName);
    end;
  end else
  begin
    if HandleParam <> '' then
    begin
      Write(F, ', ');
    end;
    FirstParam := True;
    for Param in AMethod.Params do
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      ParamDir := ParamDirection(AMethod, Param, AGetter);
      if Param <> RetVal then
      begin
        if not FirstParam then
        begin
          Write(F, ', ')
        end;
        FirstParam := False;
        ParamSpecifier := ParamDirectionToString(ParamDir);
        if (Param.ParamType.BaseType = btUnicodeString) and (AStyle in [msIntfDeclaration, msLibExport, msEventCallback]) then
        begin
          // automated string management for UnicodeString
          if ParamDir = pdIn then
            Write(F, 'char * ', ParamName)
          else
            Write(F, 'char * ', ParamName, ', int32_t &Length__', ParamName);
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
        else if AStyle in [msClassCall, msEventCall, msLibCall, msLibCallGetLength, msEventSinkCall, msEventSinkCallGetLength] then
          Write(F, ParamName)
        else
        begin
          if ParamSpecifier <> '' then
            ParamSpecifier := ParamSpecifier + ' ';
          Write(F, TypeToString(Param.ParamType), ' ', ParamSpecifier, ParamName);
        end;
      end
      else if AStyle in [msLibCall, msLibCallGetLength] then
      begin
        Write(F, ', ');
        if Param.ParamType.BaseType = btUnicodeString then
        begin
          WriteLibCallUnicodeStringParam(ParamDir, ParamName);
        end else
          Write(F, '', ParamName);
      end;
    end;
  end;
  Write(F, ')');
  if AStyle in [msClassImplementation, msLibCall, msLibCallGetLength] then
    Writeln(F)
  else if AStyle in [msLibExport, msEventCallback] then
    Writeln(F, ASpecifier)
  else
    Writeln(F, ';');
end;

procedure TPWIGGenCpp.WriteCalleeMethod(AClass: TPWIGClass; AIntf: TPWIGInterface;
 AMethod: TPWIGMethod; ABody, ADefaultInterface, AGetter: Boolean; const APrefix: string);
var
  UnicodeStringsExist: Boolean;
  Specifier: string;
begin
  UnicodeStringsExist := False;
  if ABody then
  begin
    UnicodeStringsExist := WriteCalleeUnicodeStringDeclarations(AClass, AIntf, AMethod, AGetter);
    Specifier := '';
  end else
    Specifier := ';';
  WriteMethod(AClass.Name, AIntf, AMethod, msLibExport, False, AGetter, APrefix, Specifier);
  if ABody then
  begin
    // begin to write function body
    WriteCurlyBegin;
    Writeln(F, Indent, 'bool __Result = false;');
    WriteTry;
    Writeln(F, Indent, AClass.Name, ' * __Cls = dynamic_cast<', AClass.Name, '*>((', AClass.Name,'*)ItemHandle);');
    if UnicodeStringsExist then
      WriteCalleeUnicodeStringManagement1(AClass, AIntf, AMethod, AGetter);
    WriteMethod(AClass.Name, AIntf, AMethod, msClassCall, ADefaultInterface, AGetter, APrefix);
    if UnicodeStringsExist then
      WriteCalleeUnicodeStringManagement2(AClass, AIntf, AMethod, AGetter);
    // end of function body
    Writeln(F, Indent, '__Result = true;');
    WriteExcept;
    Writeln(F, Indent, 'return __Result;');
    WriteCurlyEnd;
    WriteSpace;
  end;
end;

procedure TPWIGGenCpp.WriteCalleeEventSetter(AClass: TPWIGClass;
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
  Write(F, Indent, 'LIB_EXPORT bool ', CallingConvToString(nil), ' Set', AClass.Name, AIntf.Name, AMethod.Name, '(', LIntfDef.Name, ' ItemHandle, ', AIntf.Name, ' EventSink, T', AIntf.Name, AMethod.Name, ' EventHandler)');
  if ABody then
  begin
    Writeln(F);
    WriteCurlyBegin;
    Writeln(F, Indent, 'bool __Result = false;');
    WriteTry;
    Writeln(F, Indent, AClass.Name, ' * __Cls = dynamic_cast<', AClass.Name, '*>((', AClass.Name,'*)ItemHandle);');
    Writeln(F, Indent, '__Cls->Setup', IntfName, AMethod.Name, '(EventSink, EventHandler);');
    Writeln(F, Indent, '__Result = true;');
    WriteExcept;
    Writeln(F, Indent, 'return __Result;');
    WriteCurlyEnd;
    WriteSpace;
  end else
    Writeln(F, ';');
end;

procedure TPWIGGenCpp.WriteCalleeConstructor(AClass: TPWIGClass; AIntf: TPWIGInterface; ABody: Boolean);
begin
  Write(F, Indent, 'LIB_EXPORT bool ', CallingConvToString(nil), ' ', AClass.Name, 'Create(', AIntf.Name, ' &ItemHandle)');
  if ABody then
  begin
    Writeln(F);
    WriteCurlyBegin;
    Writeln(F, Indent, 'bool __Result = false;');
    WriteTry;
    Writeln(F, Indent, 'ItemHandle = (', AIntf.Name, ') new ', AClass.Name, '();');
    Writeln(F, Indent, '__Result = true;');
    WriteExcept;
    Writeln(F, Indent, 'return __Result;');
    WriteCurlyEnd;
    WriteSpace;
  end else
    Writeln(F, ';');
end;

procedure TPWIGGenCpp.WriteCalleeDestructor(AClass: TPWIGClass; AIntf: TPWIGInterface; ABody: Boolean);
begin
  Write(F, Indent, 'LIB_EXPORT bool ', CallingConvToString(nil), ' ', AClass.Name, 'Destroy(', AIntf.Name, ' ItemHandle)');
  if ABody then
  begin
    Writeln(F);
    WriteCurlyBegin;
    Writeln(F, Indent, 'bool __Result = false;');
    WriteTry;
    Writeln(F, Indent, AClass.Name, ' * __Cls = dynamic_cast<', AClass.Name, '*>((', AClass.Name,'*)ItemHandle);');
    Writeln(F, Indent, 'delete __Cls;');
    Writeln(F, Indent, '__Result = true;');
    WriteExcept;
    Writeln(F, Indent, 'return __Result;');
    WriteCurlyEnd;
    WriteSpace;
  end else
    Writeln(F, ';');
end;

procedure TPWIGGenCpp.WriteCalleeDeclarations(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
  IntfName: string;
begin
  WriteIntfElementProps(AClass);
  Writeln(F, Indent, 'class ', AClass.Name, ' : public Base', FPWIG.Name);
  WriteCurlyBegin;
  Writeln(F, Indent, 'private:');
  IncIndent;
  // write event handler declarations, private section
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and LIntf.FlagDispEvents then
    begin
      Writeln(F, Indent, 'Aux', LIntf.Name, ' * m_Events;');
    end;
  end;
  DecIndent;
  Writeln(F, Indent, 'public:');
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
        Writeln(F, Indent, AClass.Name, '();');
        Writeln(F, Indent, '~', AClass.Name, '();');
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
        Writeln(F, Indent, 'void Setup', IntfName, Method.Name, '(', LIntf.Name, ' EventSink, T', LIntf.Name, Method.Name, ' EventHandler);');
      end;
    end;
  end;
  DecIndent;
  WriteCurlyEnd(False);
  Writeln(F, ';');
  WriteSpace;
end;

procedure TPWIGGenCpp.WriteCalleeEventSinkDeclarations(AIntf: TPWIGInterface);
var
  Method: TPWIGMethod;
begin
  WriteIntfElementProps(AIntf);
  Writeln(F, Indent, '// Wrapper class for ', AIntf.Name, ' interface event sinks:');
  WriteSpace;
  Writeln(F, Indent, 'class Aux', AIntf.Name, ' : public Base', FPWIG.Name);
  WriteCurlyBegin;
  Writeln(F, Indent, 'private:');
  IncIndent;
  for Method in AIntf.Methods do
  begin
    Writeln(F, Indent, AIntf.Name, ' m_', Method.Name, 'EventSink;');
    Writeln(F, Indent, 'T', AIntf.Name, Method.Name, ' m_', Method.Name, 'EventHandler;');
  end;
  DecIndent;
  Writeln(F, Indent, 'public:');
  IncIndent;
  Writeln(F, Indent, 'Aux', AIntf.Name, '();');
  Writeln(F, Indent, '// Setup event handlers:');
  for Method in AIntf.Methods do
  begin
    Writeln(F, Indent, 'void Setup', Method.Name, '(', AIntf.Name, ' EventSink, T', AIntf.Name, Method.Name, ' EventHandler);');
  end;
  Writeln(F, Indent, '// Call event handlers:');
  for Method in AIntf.Methods do
  begin
    WriteMethod('', AIntf, Method, msClassDeclaration, True, False, '');
  end;
  DecIndent;
  WriteCurlyEnd(False);
  Writeln(F, ';');
  WriteSpace;
end;

procedure TPWIGGenCpp.WriteCalleeEventSinkImplementations(
  AIntf: TPWIGInterface);
var
  Method: TPWIGMethod;
  UnicodeStringsExist: Boolean;
begin
  WriteIntfElementProps(AIntf);
  WriteSpace;
  Writeln(F, Indent, 'Aux', AIntf.Name, '::Aux', AIntf.Name, '()');
  WriteCurlyBegin;
  for Method in AIntf.Methods do
  begin
    Writeln(F, Indent, 'm_', Method.Name, 'EventSink = 0;');
    Writeln(F, Indent, 'm_', Method.Name, 'EventHandler = NULL;');
  end;
  WriteCurlyEnd;
  WriteSpace;
  Writeln(F, Indent, '// Setup event handlers:');
  WriteSpace;
  for Method in AIntf.Methods do
  begin
    Writeln(F, Indent, 'void Aux', AIntf.Name, '::Setup', Method.Name, '(', AIntf.Name, ' EventSink, T', AIntf.Name, Method.Name, ' EventHandler)');
    WriteCurlyBegin;
    Writeln(F, Indent, 'm_', Method.Name, 'EventSink = EventSink;');
    Writeln(F, Indent, 'm_', Method.Name, 'EventHandler = EventHandler;');
    WriteCurlyEnd;
    WriteSpace;
  end;
  Writeln(F, Indent, '// Call event handlers:');
  WriteSpace;
  for Method in AIntf.Methods do
  begin
    WriteMethod('Aux' + AIntf.Name, AIntf, Method, msClassImplementation, True, False, '');
    UnicodeStringsExist := WriteCallerUnicodeStringDeclarations(Method, False);
    WriteCurlyBegin;
    WriteTry;
    if UnicodeStringsExist then
      WriteCallerUnicodeStringManagement1(Method, False);
    Writeln(F, Indent, 'if (m_', Method.Name, 'EventHandler != NULL)');
    WriteCurlyBegin;
    if UnicodeStringsExist then
    begin
      WriteMethod('', AIntf, Method, msEventSinkCallGetLength, True, False, '');
      WriteCallerUnicodeStringManagement2(Method, False);
    end;
    WriteMethod('', AIntf, Method, msEventSinkCall, False, False, '');
    WriteCurlyEnd;
    if UnicodeStringsExist then
      WriteCallerUnicodeStringManagement3(Method, False);
    WriteExcept;
    WriteCurlyEnd;
    WriteSpace;
  end;
end;

procedure TPWIGGenCpp.WriteCalleeExportedFuncs(AClass: TPWIGClass; ABody: Boolean);
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

procedure TPWIGGenCpp.WriteCalleeImplementations(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
  IntfName: string;
begin
  WriteIntfElementProps(AClass);
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
    begin
      Writeln(F, Indent, '// Constructor:');
      Writeln(F, Indent, AClass.Name, '::', AClass.Name, '()');
      WriteCurlyBegin;
      WriteCurlyEnd;
      WriteSpace;
      Writeln(F, Indent, '// Destructor:');
      Writeln(F, Indent, AClass.Name, '::~', AClass.Name, '()');
      WriteCurlyBegin;
      WriteCurlyEnd;
      WriteSpace;
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        WriteMethod(AClass.Name, LIntf, Method, msClassImplementation, Ref.FlagDefault, False, '');
        WriteCurlyBegin;
        WriteCurlyEnd;
        WriteSpace;
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteMethod(AClass.Name, LIntf, Prop, msClassImplementation, Ref.FlagDefault, True, 'Get');
          WriteCurlyBegin;
          WriteCurlyEnd;
          WriteSpace;
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteMethod(AClass.Name, LIntf, Prop, msClassImplementation, Ref.FlagDefault, True, 'Set');
          WriteCurlyBegin;
          WriteCurlyEnd;
          WriteSpace;
        end;
      end;
      WriteSpace;
    end;
  end;
  // write event handler setup functions
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
        Writeln(F, Indent, 'void ', AClass.Name, '::Setup', IntfName, Method.Name, '(', LIntf.Name, ' EventSink, T', LIntf.Name, Method.Name, ' EventHandler)');
        WriteCurlyBegin;
        WriteCurlyEnd;
        WriteSpace;
      end;
    end;
  end;
  WriteSpace;
end;

function TPWIGGenCpp.WriteCalleeUnicodeStringDeclarations(
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
      Writeln(F, Indent, 'static std::wstring String__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ';');
      Writeln(F, Indent, 'static std::string AnsiString__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ';');
      Result := True;
    end;
  if Result then
  begin
    // because the function is called twice, var or out parameters are not set on the second call
    // we must store them in a temporary variable as well
    for Param in AMethod.Params do
      if (Param.ParamType.BaseType <> btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn)  then
      begin
        ParamName := ReplaceNotAllowedParamName(Param.Name);
        Writeln(F, Indent, 'static ', TypeToString(Param.ParamType), ' Tmp__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ';');
      end;
    DecIndent;
  end;
  WriteSpace;
end;

function TPWIGGenCpp.WriteCalleeUnicodeStringManagement1(
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
      Writeln(F, Indent, 'if (', ParamName, ' == NULL)');
      WriteCurlyBegin;
      Exit;
    end;
end;

function TPWIGGenCpp.WriteCalleeUnicodeStringManagement2(AClass: TPWIGClass;
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
        Writeln(F, Indent, 'AnsiString__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ' = String2LibUtf8String(String__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ');');
        Writeln(F, Indent, 'Length__', ParamName, ' = (int32_t)(AnsiString__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, '.length());')
      end else
        Writeln(F, Indent, 'Tmp__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ' = ', ParamName, ';');
    end;
  WriteCurlyEndElse;
  WriteCurlyBegin;
  for Param in AMethod.Params do
    if ParamDirection(AMethod, Param, AGetter) <> pdIn then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      if Param.ParamType.BaseType = btUnicodeString then
      begin
        Writeln(F, Indent, 'if ((AnsiString__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, '.length()) > 0)');
        IncIndent;
        Writeln(F, Indent, 'memcpy(', ParamName, ', AnsiString__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, '.c_str(), LibMin(Length__', ParamName, ', (int32_t)AnsiString__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, '.length()));');
        DecIndent;
      end
      else
        Writeln(F, Indent, '', ParamName, ' = Tmp__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ';');
    end;
  WriteCurlyEnd;
  Result := True;
end;

procedure TPWIGGenCpp.WriteCallerConstructor(AClass: TPWIGClass;
  AIntf: TPWIGInterface);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  IntfName: string;
begin
  Writeln(F, Indent, AClass.Name, '::', AClass.Name, '(', AIntf.Name, ' AInterfaceHandle)');
  WriteCurlyBegin;
  WriteTry;
  Writeln(F, Indent, 'm_ItemHandle = AInterfaceHandle;');
  Writeln(F, Indent, 'if ((m_ItemHandle == 0) && (*Func', AClass.Name, 'Create != NULL))');
  IncIndent;
  Writeln(F, Indent, 'Func', AClass.Name, 'Create(m_ItemHandle);');
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
        //Writeln(F, Indent, IntfName, Method.Name, ' = NULL;');
        Writeln(F, Indent, 'if (*FuncSet', AClass.name, LIntf.Name, Method.Name, ' != NULL)');
        WriteCurlyBegin;
        Writeln(F, Indent, 'if (!');
        IncIndent;
        Writeln(F, Indent, 'FuncSet', AClass.name, LIntf.Name, Method.Name, '(m_ItemHandle, (', LIntf.Name, ')this, ', AClass.Name, LIntf.Name, Method.Name, ')');
        DecIndent;
        Writeln(F, Indent, ') LibError(L"FuncSet', AClass.name, LIntf.Name, Method.Name, '");');
        WriteCurlyEnd;
      end;
    end;
  end;
  WriteExceptLibCall;
  WriteCurlyEnd;
  WriteSpace;
end;

procedure TPWIGGenCpp.WriteCallerDestructor(AClass: TPWIGClass;
  AIntf: TPWIGInterface);
begin
  Writeln(F, Indent, AClass.Name, '::~', AClass.Name, '()');
  WriteCurlyBegin;
  WriteTry;
  Writeln(F, Indent, 'if (*Func', AClass.Name, 'Destroy != NULL)');
  IncIndent;
  Writeln(F, Indent, 'Func', AClass.Name, 'Destroy(m_ItemHandle);');
  DecIndent;
  WriteExceptLibCall;
  WriteCurlyEnd;
  WriteSpace;
end;

procedure TPWIGGenCpp.WriteCallerMethod(AClass: TPWIGClass;
  AIntf: TPWIGInterface; AMethod: TPWIGMethod;
  ADefaultInterface, AGetter: Boolean; const APrefix: string);
var
  FuncName, ParamName: string;
  UnicodeStringsExist: Boolean;
begin
  FuncName := AClass.Name + APrefix + AIntf.Name + AMethod.Name;
  WriteMethod(AClass.Name, AIntf, AMethod, msClassImplementation, ADefaultInterface, AGetter, APrefix);
  // write method body
  WriteCurlyBegin;
  WriteCallerOutputParamInits(AMethod, AGetter);
  UnicodeStringsExist := WriteCallerUnicodeStringDeclarations(AMethod, AGetter);
  WriteTry;

  Writeln(F, Indent, 'if (*Func', FuncName, ' != NULL)');
  WriteCurlyBegin;

  if UnicodeStringsExist then
  begin
    WriteCallerUnicodeStringManagement1(AMethod, AGetter);
    Writeln(F, Indent, 'if (!');
    IncIndent;
    WriteMethod(AClass.Name, AIntf, AMethod, msLibCallGetLength, ADefaultInterface, AGetter, APrefix);
    DecIndent;
    Writeln(F, Indent, ') LibError(L"Func', FuncName, '");');
    WriteCallerUnicodeStringManagement2(AMethod, AGetter);
  end;
  Writeln(F, Indent, 'if (!');
  IncIndent;
  WriteMethod(AClass.Name, AIntf, AMethod, msLibCall, ADefaultInterface, AGetter, APrefix);
  DecIndent;
  Writeln(F, Indent, ') LibError(L"Func', FuncName, '");');
  if UnicodeStringsExist then
    WriteCallerUnicodeStringManagement3(AMethod, AGetter);

  WriteCurlyEnd;
  WriteExceptLibCall;
  ParamName := ReturnParamName(AMethod, AGetter);
  if ParamName <> '' then
    Writeln(F, Indent, 'return ', ParamName,';');
  WriteCurlyEnd;
  WriteSpace;
end;

procedure TPWIGGenCpp.WriteCallerCallEventMethod(AClass: TPWIGClass;
  AIntf: TPWIGInterface; AMethod: TPWIGMethod; ADefaultInterface,
  AGetter: Boolean; const APrefix: string);
var
  IntfName: string;
begin
  if ADefaultinterface then
    IntfName := ''
  else
    IntfName := AIntf.Name;
  WriteMethod(AClass.Name, AIntf, AMethod, msClassImplementation, ADefaultInterface, AGetter, APrefix);
  // write method body
  WriteCurlyBegin;
{  Writeln(F, Indent, 'if (', IntfName, AMethod.Name, ' != NULL)');
  WriteCurlyBegin;
  WriteMethod(AClass.Name, AIntf, AMethod, msEventCall, ADefaultInterface, False, '');
  WriteCurlyEnd;}
  WriteCurlyEnd;
  WriteSpace;
end;

procedure TPWIGGenCpp.WriteCallerEventCallback(AClass: TPWIGClass;
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
  WriteMethod(AClass.Name, AIntf, AMethod, msEventCallback, ADefaultInterface, False, '');
  // write method body
  WriteCurlyBegin;
  Writeln(F, Indent, 'bool __Result = false;');
  WriteTry;
  Writeln(F, Indent, AClass.Name, ' * __Cls = dynamic_cast<', AClass.Name, '*>((', AClass.Name,'*)ItemHandle);');
//  Writeln(F, Indent, 'if (__Cls->', IntfName, AMethod.Name, ' != NULL)');
//  WriteCurlyBegin;
  if UnicodeStringsExist then
    WriteCalleeUnicodeStringManagement1(AClass, AIntf, AMethod, False);
  WriteMethod(AClass.Name, AIntf, AMethod, msClassCall, ADefaultInterface, False, 'CallEvent');
  if UnicodeStringsExist then
    WriteCalleeUnicodeStringManagement2(AClass, AIntf, AMethod, False);
//  WriteCurlyEnd;
  Writeln(F, Indent, '__Result = true;');
  WriteExcept;
  Writeln(F, Indent, 'return __Result;');
  WriteCurlyEnd;
  WriteSpace;
end;

procedure TPWIGGenCpp.WriteCallerDeclarations(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
  IntfName: string;
begin
  WriteIntfElementProps(AClass);
  // write event handler typedefs
  {for Ref in AClass.InterfaceRefs do
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
  WriteSpace;}
  Writeln(F, Indent, 'class ', AClass.Name, ' : public Base', FPWIG.Name);
  WriteCurlyBegin;
  Writeln(F, Indent, 'private:');
  IncIndent;
  // write interface declarations, private section
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
    begin
      if Ref.FlagDefault then
      begin
        Writeln(F, Indent, LIntf.Name, ' m_ItemHandle;');
      end;
    end
  end;
  DecIndent;
  Writeln(F, Indent, 'public:');
  IncIndent;
  // write interface declarations, public section
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
    begin
      if Ref.FlagDefault then
      begin
        Writeln(F, Indent, AClass.Name, '(', LIntf.Name, ' AInterfaceHandle = 0);');
        Writeln(F, Indent, '~', AClass.Name, '();');
        IntfName := '';
      end else
        IntfName := LIntf.Name;
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        WriteMethod(AClass.Name, LIntf, Method, msClassDeclaration, Ref.FlagDefault, False, '');
      end;
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
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.Params.Count = 1 then
        begin
          Write(F, Indent, '__declspec( property (');
          if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
          begin
            // write getter
            Write(F, 'get=Get', IntfName, Prop.Name);
          end;
          if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
          begin
            if Prop.PropertyType in [ptReadWrite] then
              Write(F, ', ');
            // write setter
            Write(F, 'put=Set', IntfName, Prop.Name);
          end;
          Writeln(F, ')) ', TypeToString(Prop.Params.Last.ParamType), ' ', IntfName, Prop.Name, ';');
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
      if Ref.FlagDefault then
        IntfName := ''
      else
        IntfName := LIntf.Name;
      Writeln(F, Indent, '// Events:');
      for Method in LIntf.Methods do
      begin
        WriteMethod(AClass.Name, LIntf, Method, msClassDeclaration, Ref.FlagDefault, False, 'CallEvent', 'virtual ');
        //Writeln(F, Indent, 'T', LIntf.Name, Method.Name, 'Event ', IntfName, Method.Name, ';');
      end;
    end;
  end;
  // write Handle property
  Writeln(F, Indent, '// Default interface handle:');
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents and Ref.FlagDefault then
    begin
      Writeln(F, Indent, LIntf.Name, ' __GetHandle(void);');
      Writeln(F, Indent, '__declspec( property (get=__GetHandle)) ', LIntf.Name, ' Handle;');
    end;
  end;
  DecIndent;
  WriteCurlyEnd(False);
  Writeln(F, ';');
  WriteSpace;
end;

procedure TPWIGGenCpp.WriteCallerPointers(AClass: TPWIGClass; AUseType: Boolean);
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
          Writeln(F, Indent, 'static T', AClass.Name, 'Create Func', AClass.Name, 'Create = NULL;')
        else
          Writeln(F, Indent, 'Func', AClass.Name, 'Create = NULL;');
        Writeln(F, Indent, '// Destructor:');
        if AUseType then
          Writeln(F, Indent, 'static T', AClass.Name, 'Destroy Func', AClass.Name, 'Destroy = NULL;')
        else
          Writeln(F, Indent, 'Func', AClass.Name, 'Destroy = NULL;');
      end;
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        if AUseType then
          Writeln(F, Indent, 'static T', LIntf.Name, Method.Name, ' Func', AClass.Name, LIntf.Name, Method.Name, ' = NULL;')
        else
          Writeln(F, Indent, 'Func', AClass.Name, LIntf.Name, Method.Name, ' = NULL;');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          if AUseType then
            Writeln(F, Indent, 'static TGet', LIntf.Name, Prop.Name, ' Func', AClass.Name, 'Get', LIntf.Name, Prop.Name, ' = NULL;')
          else
            Writeln(F, Indent, 'Func', AClass.Name, 'Get', LIntf.Name, Prop.Name, ' = NULL;');
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          if AUseType then
            Writeln(F, Indent, 'static TSet', LIntf.Name, Prop.Name, ' Func', AClass.Name, 'Set', LIntf.Name, Prop.Name, ' = NULL;')
          else
            Writeln(F, Indent, 'Func', AClass.Name, 'Set', LIntf.Name, Prop.Name, ' = NULL;');
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
          Writeln(F, Indent, 'static TSet', AClass.Name, LIntf.Name, Method.Name, ' FuncSet', AClass.Name, LIntf.Name, Method.Name, ' = NULL;')
        else
          Writeln(F, Indent, 'FuncSet', AClass.Name, LIntf.Name, Method.Name, ' = NULL;')
      end;
    end;
  end;
  WriteSpace;
end;

procedure TPWIGGenCpp.WriteCallerLibLoads(AClass: TPWIGClass);

  procedure WriteLibLoad(const AFuncName, ATypeName: string);
  begin
    Writeln(F, Indent, 'Func', AFuncName, ' = (T', ATypeName, ')GetProcAddress(LibModule, "', AFuncName, '");');
    Writeln(F, Indent, 'if (Func', AFuncName, ' == NULL) LibLoadError(L"', AFuncName, '");');
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
        WriteLibLoad(AClass.Name + 'Create', AClass.Name + 'Create');
        Writeln(F, Indent, '// Destructor:');
        WriteLibLoad(AClass.Name + 'Destroy', AClass.Name + 'Destroy');
      end;
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        WriteLibLoad(AClass.Name + LIntf.Name + Method.Name, LIntf.Name + Method.Name);
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteLibLoad(AClass.Name + 'Get' + LIntf.Name + Prop.Name, 'Get' + LIntf.Name + Prop.Name);
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteLibLoad(AClass.Name + 'Set' + LIntf.Name + Prop.Name, 'Set' + LIntf.Name + Prop.Name);
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
        WriteLibLoad('Set' + AClass.Name + LIntf.Name + Method.Name, 'Set' + AClass.Name + LIntf.Name + Method.Name);
      end;
    end;
  end;
end;

procedure TPWIGGenCpp.WriteCallerImplementations(AClass: TPWIGClass);
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
    if LIntf <> nil then
      if LIntf.FlagDispEvents then
      begin
        for Method in LIntf.Methods do
        begin
          WriteCallerCallEventMethod(AClass, LIntf, Method, Ref.FlagDefault, False, 'CallEvent');
        end;
      end else
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
        // write handle getter
        Writeln(F, Indent, LIntf.Name, ' ', AClass.Name, '::__GetHandle(void)');
        WriteCurlyBegin;
        Writeln(F, Indent, 'return m_ItemHandle;');
        WriteCurlyEnd;
        WriteSpace;
      end;
  end;
  WriteSpace;
end;

function TPWIGGenCpp.WriteCallerOutputParamInits(AMethod: TPWIGMethod; AGetter: Boolean): Boolean;
var
  Param, RetVal: TPWIGParam;
  ParamName: string;
begin
  if AGetter then
    RetVal := AMethod.Params.Last
  else
    RetVal := AMethod.Params.FindRetVal;
  for Param in AMethod.Params do
    if ParamDirection(AMethod, Param, AGetter) <> pdIn then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      if RetVal = Param then
        Writeln(F, Indent, TypeToString(Param.ParamType), ' ', ParamName, ' = ', TypeToNullValue(Param.ParamType),';')
      else
        Writeln(F, Indent, '', ParamName, ' = ', TypeToNullValue(Param.ParamType),';')
    end;
  Result := True;
end;

function TPWIGGenCpp.WriteCallerUnicodeStringDeclarations(
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
      Writeln(F, Indent, 'char * AnsiString__', ParamName, ';');
      Writeln(F, Indent, 'int32_t Length__', ParamName, ';');
      Result := True;
    end;
end;

function TPWIGGenCpp.WriteCallerUnicodeStringManagement1(
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
      Writeln(F, Indent, 'Length__', ParamName, ' = 0;');
    end;
  Result := True;
end;

function TPWIGGenCpp.WriteCallerUnicodeStringManagement2(
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
      Writeln(F, Indent, 'AnsiString__', ParamName, ' = new char[LibMax(Length__', ParamName, ' + 1, 1)];');
    end;
  Result := True;
end;

function TPWIGGenCpp.WriteCallerUnicodeStringManagement3(
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
        S := ''
      else
        S := '';
      Writeln(F, Indent, 'AnsiString__', ParamName, '[Length__', ParamName, '] = 0;');
      Writeln(F, Indent, 'if (Length__', ParamName, ' > 0) ', S, ParamName, ' = LibUtf8String2String(AnsiString__', ParamName, ');');
      Writeln(F, Indent, 'delete AnsiString__', ParamName, ';');
    end;
  Result := True;
end;

procedure TPWIGGenCpp.WriteInterfaceHeaderFile(const AFileName: string);
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

  GeneratedFile := Path + IntfName + '.h';

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
      Writeln(F, Indent, '#ifndef ', UpperCase(IntfName), '_H');
      Writeln(F, Indent, '#define ', UpperCase(IntfName), '_H');
      WriteSpace;
      Writeln(F, Indent, '// Library properties:');
      WriteIntfElementProps(FPWIG);

      // write includes
      // now hardcoded, later might be automated when needed
      Writeln(F, Indent, '#include <string>');
      Writeln(F, Indent, '#include "stdafx.h"');
      Writeln(F, Indent, '#include "stdint.h"');
      WriteSpace;

      // write library ID
      Writeln(F, Indent, '#define LIB_GUID "', FPWIG.GUID, '"');
      DecIndent;
      WriteSpace;

      // write generic class
      Writeln(F, Indent, '// Base class for all wrapper classes:');
      Writeln(F, Indent, 'class Base', FPWIG.Name);
      WriteCurlyBegin;
      Writeln(F, Indent, 'public:');
      Writeln(F, Indent, 'virtual ~Base', FPWIG.Name, '() {};');
      WriteCurlyEnd(False);
      Writeln(F, ';');
      WriteSpace;

      // write forward declarations
      Writeln(F, Indent, '// Forward declarations:');
      for LIntf in FPWIG.Interfaces do
        Writeln(F, Indent, 'typedef int64_t ', LIntf.Name, ';');
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
      Writeln(F, Indent, 'template <typename T>');
      Writeln(F, Indent, 'inline T const& LibMax (T const& a, T const& b)  { return a < b ? b:a; }');
      WriteSpace;
      Writeln(F, Indent, 'template <typename T>');
      Writeln(F, Indent, 'inline T const& LibMin (T const& a, T const& b)  { return a < b ? a:b; }');
      WriteSpace;
      Writeln(F, Indent, 'std::string String2LibUtf8String(std::wstring AText);');
      Writeln(F, Indent, 'std::wstring LibUtf8String2String(std::string);');
      WriteSpace;

      // finish
      Writeln(F, Indent, '#endif ', UpperCase(IntfName), '_H');

      Writeln('C++ common interface file generated: ', GeneratedFile);
    except
      Writeln('Could not generate C++ common interface file: ', GeneratedFile);
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TPWIGGenCpp.WriteInterfaceSourceFile(const AFileName: string);
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
      Writeln(F, Indent, '// Library properties:');
      WriteIntfElementProps(FPWIG);

      // write includes
      // now hardcoded, later might be automated when needed
      Writeln(F, Indent, '#include <string>');
      Writeln(F, Indent, '#include "stdafx.h"');
      Writeln(F, Indent, '#include "stdint.h"');
      Writeln(F, Indent, '#include "', IntfName, '.h"');
      WriteSpace;

      Writeln(F, Indent, '// Library helper functions');

      Writeln(F, Indent, 'std::string String2LibUtf8String(std::wstring AText)');
      WriteCurlyBegin;
      Writeln(F, Indent, 'size_t length = WideCharToMultiByte(CP_UTF8, 0, AText.c_str(), -1, NULL, 0, NULL, NULL) + 1;');
      Writeln(F, Indent, 'char * buff = new char[length];');
      Writeln(F, Indent, 'WideCharToMultiByte(CP_UTF8, 0, AText.c_str(), -1, buff, (int)length, NULL, NULL);');
      Writeln(F, Indent, 'std::string Result(buff);');
      Writeln(F, Indent, 'delete buff;');
      Writeln(F, Indent, 'return Result;');
      WriteCurlyEnd;
      WriteSpace;

      Writeln(F, Indent, 'std::wstring LibUtf8String2String(std::string AText)');
      WriteCurlyBegin;
      Writeln(F, Indent, 'size_t length = MultiByteToWideChar(CP_UTF8, 0, AText.c_str(), -1, NULL, 0) + 1;');
      Writeln(F, Indent, 'wchar_t * buff = new wchar_t[length];');
      Writeln(F, Indent, 'MultiByteToWideChar(CP_UTF8, 0, AText.c_str(), -1, buff, (int)length);');
      Writeln(F, Indent, 'std::wstring Result(buff);');
      Writeln(F, Indent, 'delete buff;');
      Writeln(F, Indent, 'return Result;');
      WriteCurlyEnd;
      WriteSpace;

      // finish
      WriteSpace;

      Writeln('C++ common interface file generated: ', GeneratedFile);
    except
      Writeln('Could not generate C++ common interface file: ', GeneratedFile);
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TPWIGGenCpp.WriteCalleeHeaderFile(const AFileName: string);
var
  LCls: TPWIGClass;
  LIntf: TPWIGInterface;
  Name, Path, Ext, GeneratedFile, IntfName, ImplName, ImplNameAuth: string;
begin
  Path := ExtractFilePath(AFileName);
  Name := ExtractFileRawName(AFileName);
  Ext := ExtractFileExt(AFileName);

  // write wrapper file for the callee
  IntfName := Name + '_intf';
  ImplName := Name + '_callee';
  ImplNameAuth := ImplName + '_impl';

  GeneratedFile := Path + ImplName + '.h';

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
      Writeln(F, Indent, '#ifndef ', UpperCase(ImplName), '_H');
      Writeln(F, Indent, '#define ', UpperCase(ImplName), '_H');
      WriteSpace;
      Writeln(F, Indent, '// Library properties:');
      WriteIntfElementProps(FPWIG);

      // write includes
      // now hardcoded, later might be automated when needed
      Writeln(F, Indent, '#include <string>');
      Writeln(F, Indent, '#include "stdafx.h"');
      Writeln(F, Indent, '#include "stdint.h"');
      Writeln(F, Indent, '#include "', IntfName, '.h"');
      WriteSpace;

      // write library export clause
      Writeln(F, Indent, '#define LIB_EXPORT extern "C" __declspec(dllexport)');
      WriteSpace;

      // write forward declarations
      Writeln(F, Indent, '// Forward declarations:');
      for LCls in FPWIG.Classes do
        Writeln(F, Indent, 'class ', LCls.Name, ';');
      for LIntf in FPWIG.Interfaces do
        if LIntf.FlagDispEvents then
          Writeln(F, Indent, 'class Aux', LIntf.Name, ';');
      WriteSpace;

      // entire declaration part is commented out because the classes must be implemented in other file
      Writeln(F, Indent, '// Copy these class declarations into ', ImplNameAuth, '.h to allow custom members to be added.');
      Writeln(F, Indent, '// Note: ', ImplNameAuth, '.h is not maintained by PWIG and must be implemented by library author!');
      Writeln(F, Indent, '/*');

      // write class interfaces (implementation must be done by library author)
      for LCls in FPWIG.Classes do
        WriteCalleeDeclarations(LCls);

      // end of commented out section
      Writeln(F, Indent, '// End of declarations to be copied to ', ImplNameAuth, '.h file.');
      Writeln(F, Indent, '*/');
      WriteSpace;

      // write event sink wrapper classes
      for LIntf in FPWIG.Interfaces do
        if LIntf.FlagDispEvents then
          WriteCalleeEventSinkDeclarations(LIntf);

      // write library identification code - interface part
      Writeln(F, Indent, '// Library identification code');
      Writeln(F, Indent, 'LIB_EXPORT char * ', CallingConvToString(nil),' GetLibGUID(void);');
      WriteSpace;

      // write exported function declarations
      for LCls in FPWIG.Classes do
        WriteCalleeExportedFuncs(LCls, False);

      // finish
      Writeln(F, Indent, '#endif ', UpperCase(ImplName), '_H');
      WriteSpace;

      Writeln('C++ callee interface header file generated: ', GeneratedFile);
    except
      Writeln('Could not generate C++ callee interface header file: ', GeneratedFile);
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TPWIGGenCpp.WriteCalleeSourceFile(const AFileName: string);
var
  LCls: TPWIGClass;
  LIntf: TPWIGInterface;
  Name, Path, Ext, GeneratedFile, IntfName, ImplName, ImplNameAuth: string;
begin
  Path := ExtractFilePath(AFileName);
  Name := ExtractFileRawName(AFileName);
  Ext := ExtractFileExt(AFileName);

  // write wrapper file for the callee
  IntfName := Name + '_intf';
  ImplName := Name + '_callee';
  ImplNameAuth := ImplName + '_impl';

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
      Writeln(F, Indent, '// Library properties:');
      WriteIntfElementProps(FPWIG);

      // write includes
      // now hardcoded, later might be automated when needed
      Writeln(F, Indent, '#include <algorithm>');
      Writeln(F, Indent, '#include <exception>');
      Writeln(F, Indent, '#include <string>');
      Writeln(F, Indent, '#include "stdafx.h"');
      Writeln(F, Indent, '#include "stdint.h"');
      Writeln(F, Indent, '#include "', IntfName, '.h"');
      Writeln(F, Indent, '#include "', ImplName, '.h"');
      Writeln(F, Indent, '#include "', ImplNameAuth, '.h"');
      WriteSpace;

      // write library identification code - implementation part
      Writeln(F, Indent, '// Library identification code');
      Writeln(F, Indent, 'LIB_EXPORT char * ', CallingConvToString(nil), ' GetLibGUID(void)');
      WriteCurlyBegin;
      Writeln(F, Indent, 'return LIB_GUID;');
      WriteCurlyEnd;
      WriteSpace;

      // write exported function bodies
      for LCls in FPWIG.Classes do
        WriteCalleeExportedFuncs(LCls, True);

      // write event sink wrapper classes
      for LIntf in FPWIG.Interfaces do
        if LIntf.FlagDispEvents then
          WriteCalleeEventSinkImplementations(LIntf);

      // class implementation part is commented out because the classes must be implemented in other file
      Writeln(F, Indent, '// Copy these class declarations into your custom *.cpp file and implement them there.');
      Writeln(F, Indent, '/*');

      // write exported function bodies
      for LCls in FPWIG.Classes do
        WriteCalleeImplementations(LCls);

      // end of commented out section
      DecIndent;
      Writeln(F, Indent, '// End of declarations to be implemented in other *.cpp file.');
      Writeln(F, Indent, '*/');
      WriteSpace;

      // finish
      WriteSpace;

      Writeln('C++ callee interface source file generated: ', GeneratedFile);
    except
      Writeln('Could not generate C++ callee interface source file: ', GeneratedFile);
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TPWIGGenCpp.WriteCallerHeaderFile(const AFileName: string);
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

  GeneratedFile := Path + ImplName + '.h';

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
      Writeln(F, Indent, '#ifndef ', UpperCase(ImplName), '_H');
      Writeln(F, Indent, '#define ', UpperCase(ImplName), '_H');
      WriteSpace;
      Writeln(F, Indent, '// Library properties:');
      WriteIntfElementProps(FPWIG);

      // write includes
      // now hardcoded, later might be automated when needed
      Writeln(F, Indent, '#include <string>');
      Writeln(F, Indent, '#include "stdafx.h"');
      Writeln(F, Indent, '#include "stdint.h"');
      Writeln(F, Indent, '#include "', IntfName, '.h"');
      WriteSpace;

      // write forward declarations
      Writeln(F, Indent, '// Forward declarations:');
      for LCls in FPWIG.Classes do
        Writeln(F, Indent, 'class ', LCls.Name, ';');
      WriteSpace;

      // write class interfaces
      for LCls in FPWIG.Classes do
        WriteCallerDeclarations(LCls);

      // library loader
      Writeln(F, Indent, 'bool ', FPWIG.Name, 'LibLoad(std::wstring FileName);');
      // library unloader
      Writeln(F, Indent, 'void ', FPWIG.Name, 'LibUnload();');
      WriteSpace;

      // finish
      Writeln(F, Indent, '#endif ', UpperCase(ImplName), '_H');
      WriteSpace;

      Writeln('C++ caller interface header file generated: ', GeneratedFile);
    except
      Writeln('Could not generate C++ caller interface header file: ', GeneratedFile);
    end;
  finally
    CloseFile(F);
  end;

end;

procedure TPWIGGenCpp.WriteCallerSourceFile(const AFileName: string);
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
      Writeln(F, Indent, '// Library properties:');
      WriteIntfElementProps(FPWIG);

      // write includes
      // now hardcoded, later might be automated when needed
      Writeln(F, Indent, '#include <algorithm>');
      Writeln(F, Indent, '#include <exception>');
      Writeln(F, Indent, '#include <string>');
      Writeln(F, Indent, '#include "stdafx.h"');
      Writeln(F, Indent, '#include "stdint.h"');
      Writeln(F, Indent, '#include "', IntfName, '.h"');
      Writeln(F, Indent, '#include "', ImplName, '.h"');
      WriteSpace;

      // write library imports
      Writeln(F, '//Local variables:');
      Writeln(F, Indent, 'static HMODULE LibModule = 0;');
      WriteSpace;
      for LCls in FPWIG.Classes do
        WriteCallerPointers(LCls, True);

      // write library generous error function
      Writeln(F, Indent, 'static void LibError(std::wstring AMessage)');
      WriteCurlyBegin;
      Writeln(F, Indent, 'throw std::runtime_error(String2LibUtf8String(AMessage));');
      WriteCurlyEnd;
      WriteSpace;

      // write library calling error function
      Writeln(F, Indent, 'static void LibCallError(std::wstring AFuncName)');
      WriteCurlyBegin;
      Writeln(F, Indent, 'LibError(L"Error while calling library function " + AFuncName + L"!");');
      WriteCurlyEnd;
      WriteSpace;

      // write library loading error function
      Writeln(F, Indent, 'static void LibLoadError(std::wstring AFuncName)');
      WriteCurlyBegin;
      Writeln(F, Indent, 'LibError(L"Requested function " + AFuncName + L" does not exist in the library!");');
      WriteCurlyEnd;
      WriteSpace;

      // write library loader implementation
      Writeln(F, Indent, 'bool ', FPWIG.Name, 'LibLoad(std::wstring FileName)');
      WriteCurlyBegin;
      Writeln(F, Indent, 'typedef char * (', CallingConvToString(nil), ' *T_FuncLibID)(void);');
      Writeln(F, Indent, 'T_FuncLibID _FuncLibID;');
      Writeln(F, Indent, 'bool __Result = false;');
      WriteTry;
      Writeln(F, Indent, 'if (LibModule == 0)');
      WriteCurlyBegin;
      Writeln(F, Indent, 'LibModule = LoadLibrary(FileName.c_str());');
      WriteCurlyEnd;
      Writeln(F, Indent, 'if (LibModule != 0)');
      WriteCurlyBegin;
      Writeln(F, Indent, '// Call library identification code first');
      Writeln(F, Indent, '_FuncLibID = (T_FuncLibID)GetProcAddress(LibModule, "GetLibGUID");');
      Writeln(F, Indent, 'if (_FuncLibID == NULL) LibLoadError(L"GetLibGUID");');
      Writeln(F, Indent, 'char * id = _FuncLibID();');
      Writeln(F, Indent, 'if (strcmp(id, LIB_GUID) != 0) LibError(L"Incompatible library interface!");');
      WriteSpace;
      for LCls in FPWIG.Classes do
        WriteCallerLibLoads(LCls);
      Writeln(F, Indent, '__Result = true;');
      WriteCurlyEnd;
      WriteExcept;
      Writeln(F, Indent, 'return __Result;');
      WriteCurlyEnd;
      WriteSpace;

      // write library unloader implementation
      Writeln(F, Indent, 'void ', FPWIG.Name, 'LibUnload()');
      WriteCurlyBegin;
      Writeln(F, Indent, 'if (LibModule != 0)');
      IncIndent;
      Writeln(F, Indent, 'FreeLibrary(LibModule);');
      DecIndent;
      Writeln(F, Indent, 'LibModule = 0;');
      WriteSpace;
      for LCls in FPWIG.Classes do
        WriteCallerPointers(LCls, False);
      WriteCurlyEnd;
      WriteSpace;

      // write class method bodies
      for LCls in FPWIG.Classes do
        WriteCallerImplementations(LCls);

      // finish
      WriteSpace;

      Writeln('C++ caller interface file generated: ', GeneratedFile);
    except
      Writeln('Could not generate C++ caller interface file: ', GeneratedFile);
    end;
  finally
    CloseFile(F);
  end;
end;

end.

