{ @abstract(This unit contains PWIG Pascal wrapper class generator (usable by Lazarus or Delphi))
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(23 Oct 2016)

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

unit pwig_pascal;

{$mode delphi}

interface

uses
  Classes, SysUtils, PWIGGen;

type

  { TPWIGGenPascal }

  TPWIGGenPascalMethodStyle = (
    msIntfDeclaration,
    msLibCallGetLength,
    msLibCall,
    msLibExport,
    msClassDeclaration,
    msClassImplementation,
    msClassCall,
    msPropertyDeclaration
  );

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
    property Flags: string read FFlags;
    property Indent: string read FIndent;
  protected
    F: TextFile;
    function CallingConvToString(AConv: TPWIGCallingConv): string; virtual;
    function TypeToString(AType: TPWIGType): string; virtual;
    function ReplaceNotAllowedParamName(AName: string): string; virtual;
    procedure WriteIntfElementProps(AElement: TPWIGElement); virtual;
    procedure WriteIntfAliasProps(AAlias: TPWIGAlias); virtual;
    procedure WriteIntfEnumProps(AEnum: TPWIGEnum); virtual;
    procedure WriteIntfInterfaceProps(AIntf: TPWIGInterface); virtual;
    procedure WriteIntfClassProps(AClass: TPWIGClass); virtual;
    procedure WriteMethod(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      AStyle: TPWIGGenPascalMethodStyle; ADefaultInterface, AGetter: Boolean; const APrefix: string); virtual;

    procedure WriteCalleeConstructor(AClass: TPWIGClass; AIntf: TPWIGInterface; ABody: Boolean); virtual;
    procedure WriteCalleeDestructor(AClass: TPWIGClass; AIntf: TPWIGInterface; ABody: Boolean); virtual;
    procedure WriteCalleeMethod(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      ABody, ADefaultInterface, AGetter: Boolean; const APrefix: string); virtual;
    procedure WriteCalleeDeclarations(AClass: TPWIGClass); virtual;
    procedure WriteCalleeExportedFuncs(AClass: TPWIGClass; ABody: Boolean); virtual;
    procedure WriteCalleeExports(AClass: TPWIGClass); virtual;

    procedure WriteCallerConstructor(AClass: TPWIGClass; AIntf: TPWIGInterface); virtual;
    procedure WriteCallerDestructor(AClass: TPWIGClass; AIntf: TPWIGInterface); virtual;
    procedure WriteCallerMethod(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      ADefaultInterface, AGetter: Boolean; const APrefix: string); virtual;
    procedure WriteCallerDeclarations(AClass: TPWIGClass); virtual;
    procedure WriteCallerPointers(AClass: TPWIGClass; AUseType: Boolean); virtual;
    procedure WriteCallerLibLoads(AClass: TPWIGClass); virtual;
    procedure WriteCallerImplementations(AClass: TPWIGClass); virtual;

    procedure WriteInterfaceFile(const AFileName: string);
    procedure WriteCalleeFile(const AFileName: string);
    procedure WriteCallerFile(const AFileName: string);
  public
    constructor Create(AOwner: TPWIG); override;
    procedure SaveToFile(const AFileName: string); override;
    procedure SaveToFiles(const ACalleeFileName, ACallerFileName: string); override;
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

function TPWIGGenPascal.CallingConvToString(AConv: TPWIGCallingConv): string;
begin
  Result := '';
  case AConv of
    ccStdCall: Result := 'stdcall';
    ccCDecl: Result := 'cdecl';
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
    btUnicodeString: Result := 'AnsiString'; // UTF8 encoded Unicode string
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
    Result := AName + '1';
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

procedure TPWIGGenPascal.SaveToFile(const AFileName: string);
begin
  SaveToFiles(AFileName, AFileName);
end;

procedure TPWIGGenPascal.SaveToFiles(const ACalleeFileName,
  ACallerFileName: string);
begin
  WriteInterfaceFile(ACalleeFileName);
  WriteCalleeFile(ACalleeFileName);
  WriteInterfaceFile(ACallerFileName);
  WriteCallerFile(ACallerFileName);
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
    WriteMethod(nil, AIntf, Method, msIntfDeclaration, False, False, '');
  end;
  Writeln(F, Indent, '// Properties:');
  for Prop in AIntf.Properties do
  begin
    if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
    begin
      // write getter
      WriteMethod(nil, AIntf, Prop, msIntfDeclaration, False, True, 'Get');
    end;
    if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
    begin
      // write setter
      WriteMethod(nil, AIntf, Prop, msIntfDeclaration, False, False, 'Set');
    end;
  end;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteIntfClassProps(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
begin
  Writeln(F, Indent, '// Class properties:');
  WriteIntfElementProps(AClass);
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
    begin
      // use constructor and destructor only for default interface
      if Ref.FlagDefault then
      begin
        Writeln(F, Indent, 'T', AClass.Name, 'Create = function(out ItemHandle:', LIntf.Name, '): Boolean; stdcall;');
        Writeln(F, Indent, 'T', AClass.Name, 'Destroy = function(const ItemHandle:', LIntf.Name, '): Boolean; stdcall;');
      end;
    end;
  end;
end;

procedure TPWIGGenPascal.WriteMethod(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
  AStyle: TPWIGGenPascalMethodStyle; ADefaultInterface, AGetter: Boolean; const APrefix: string);

  procedure WriteLibCallUnicodeStringParam(AParamDir: TPWIGParamDirection; const AParamName: string);
  begin
    if AParamDir = pdIn then
      Write(F, 'PAnsiChar(', AParamName, ')')
    else if AStyle = msLibCallGetLength then
      Write(F, 'nil, Length__', AParamName)
    else
      Write(F, '@String__', AParamName, '[1], Length__', AParamName);
  end;

var
  Param, RetVal: TPWIGParam;
  ParamDir: TPWIGParamDirection;
  CallingConv, HandleParam, IntfName, ParamName, ParamSpecifier, FuncKeyword, RetValAssignment: string;
  FirstParam: Boolean;
begin
  if AStyle in [msIntfDeclaration, msLibExport] then
  begin
    CallingConv := '; ' + CallingConvToString(AMethod.CallingConv);
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
        ParamName := 'String__'+ ParamName;
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
      Write(F, Indent, 'Func', AClass.Name, APrefix, IntfName, AMethod.Name, '(', HandleParam);
    msLibExport:
      Write(F, Indent, 'function ', AClass.Name, APrefix, IntfName, AMethod.Name, '(', HandleParam);
    msClassDeclaration:
      Write(F, Indent, FuncKeyword, ' ', APrefix, IntfName, AMethod.Name);
    msClassImplementation:
      Write(F, Indent, FuncKeyword, ' T', AClass.Name, '.', APrefix, IntfName, AMethod.Name);
    msClassCall:
      Write(F, Indent, RetValAssignment, 'T', AClass.Name, '(ItemHandle).', APrefix, IntfName, AMethod.Name);
    msPropertyDeclaration:
      Write(F, Indent, 'property ', IntfName, AMethod.Name);
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
      if AStyle in [msClassCall, msLibCall, msLibCallGetLength] then
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
          if AStyle in [msClassCall, msLibCall, msLibCallGetLength] then
            Write(F, ', ')
          else
            Write(F, '; ');
        end;
        FirstParam := False;
        ParamSpecifier := ParamDirectionToString(ParamDir);
        if (Param.ParamType.BaseType = btUnicodeString) and (AStyle in [msIntfDeclaration, msLibExport]) then
        begin
          // automated string management for UnicodeString
          if ParamDir = pdIn then
            Write(F, ParamSpecifier, ' ', ParamName, ': PAnsiChar')
          else
            Write(F, 'const ', ParamName, ': PAnsiChar; var Length__', ParamName, ': LongInt');
        end
        else if (Param.ParamType.BaseType = btUnicodeString) and (AStyle in [msClassCall]) then
        begin
          if ParamDir = pdIn then
            Write(F, ParamName)
          else
            Write(F, 'String__' + ParamName);
        end
        else if (Param.ParamType.BaseType = btUnicodeString) and (AStyle in [msLibCall, msLibCallGetLength]) then
        begin
          WriteLibCallUnicodeStringParam(ParamDir, ParamName);
        end
        else if AStyle in [msClassCall, msLibCall, msLibCallGetLength] then
          Write(F, ParamName)
        else if AStyle = msPropertyDeclaration then
          Write(F, ParamName, ': ', TypeToString(Param.ParamType))
        else
          Write(F, ParamSpecifier, ' ', ParamName, ': ', TypeToString(Param.ParamType));
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
  else
    Writeln(F, ';');
end;

procedure TPWIGGenPascal.WriteCalleeMethod(AClass: TPWIGClass; AIntf: TPWIGInterface;
 AMethod: TPWIGMethod; ABody, ADefaultInterface, AGetter: Boolean; const APrefix: string);
var
  Param: TPWIGParam;
  ParamName: string;
  FirstParam: Boolean;
begin
  WriteMethod(AClass, AIntf, AMethod, msLibExport, False, AGetter, APrefix);
  if not ABody then Exit;
  // automated string management for UnicodeString
  FirstParam := True;
  for Param in AMethod.Params do
    if (Param.ParamType.BaseType = btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn)  then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      if FirstParam then
      begin
        Writeln(F, Indent, 'var ');
        IncIndent;
        Write(F, Indent);
      end else
        Write(F, ', ');
      FirstParam := False;
      Write(F, 'String__', ParamName);
    end;
  if not FirstParam then
  begin
    Writeln(F, ': AnsiString;');
    DecIndent;
  end;
  // begin to write function body
  Writeln(F, Indent, 'begin');
  IncIndent;
  Writeln(F, Indent, 'Result := False;');
  Writeln(F, Indent, 'try');
  IncIndent;
  Writeln(F, Indent, 'if TObject(ItemHandle) is T', AClass.Name, ' then');
  Writeln(F, Indent, 'begin');
  IncIndent;
  WriteMethod(AClass, AIntf, AMethod, msClassCall, ADefaultInterface, AGetter, APrefix);
  // automated string management for UnicodeString
  for Param in AMethod.Params do
    if (Param.ParamType.BaseType = btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn) then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      Writeln(F, Indent, 'if ', ParamName, ' = nil then');
      IncIndent;
      Writeln(F, Indent, 'Length__', ParamName, ' := Length(String__', ParamName, ')');
      DecIndent;
      Writeln(F, Indent, 'else');
      IncIndent;
      Writeln(F, Indent, 'System.Move(String__', ParamName, '[1], ', ParamName, '^, Min(Length__', ParamName, ', Length(String__', ParamName, ')));');
      DecIndent;
    end;
  // end of function body
  Writeln(F, Indent, 'Result := True;');
  DecIndent;
  Writeln(F, Indent, 'end;');
  DecIndent;
  Writeln(F, Indent, 'except');
  Writeln(F, Indent, 'end;');
  DecIndent;
  Writeln(F, Indent, 'end;');
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCalleeConstructor(AClass: TPWIGClass; AIntf: TPWIGInterface; ABody: Boolean);
begin
  Writeln(F, Indent, 'function ', AClass.Name, 'Create(out ItemHandle:', AIntf.Name, '): Boolean; stdcall;');
  if not ABody then Exit;
  Writeln(F, Indent, 'begin');
  IncIndent;
  Writeln(F, Indent, 'try');
  IncIndent;
  Writeln(F, Indent, 'ItemHandle := ', AIntf.Name, '(T', AClass.Name, '.Create);');
  Writeln(F, Indent, 'Result := True;');
  DecIndent;
  Writeln(F, Indent, 'except');
  IncIndent;
  Writeln(F, Indent, 'Result := False;');
  DecIndent;
  Writeln(F, Indent, 'end;');
  DecIndent;
  Writeln(F, Indent, 'end;');
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCalleeDestructor(AClass: TPWIGClass; AIntf: TPWIGInterface; ABody: Boolean);
begin
  Writeln(F, Indent, 'function ', AClass.Name, 'Destroy(ItemHandle: ', AIntf.Name, '): Boolean; stdcall;');
  if not ABody then Exit;
  Writeln(F, Indent, 'begin');
  IncIndent;
  Writeln(F, Indent, 'Result := False;');
  Writeln(F, Indent, 'try');
  IncIndent;
  Writeln(F, Indent, 'if TObject(ItemHandle) is T', AClass.Name, ' then');
  Writeln(F, Indent, 'begin');
  IncIndent;
  Writeln(F, Indent, 'T', AClass.Name, '(ItemHandle).Free;');
  Writeln(F, Indent, 'Result := True;');
  DecIndent;
  Writeln(F, Indent, 'end;');
  DecIndent;
  Writeln(F, Indent, 'except');
  Writeln(F, Indent, 'end;');
  DecIndent;
  Writeln(F, Indent, 'end;');
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCalleeDeclarations(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
begin
  WriteIntfElementProps(AClass);
  Writeln(F, Indent, 'T', AClass.Name, ' = class(TObject)');
  Writeln(F, Indent, 'public');
  IncIndent;

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
        WriteMethod(AClass, LIntf, Method, msClassDeclaration, Ref.FlagDefault, False, '');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteMethod(AClass, LIntf, Prop, msClassDeclaration, Ref.FlagDefault, True, 'Get');
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteMethod(AClass, LIntf, Prop, msClassDeclaration, Ref.FlagDefault, False, 'Set');
        end;
      end;
    end;
  end;
  DecIndent;
  Writeln(F, Indent, 'end;');
  WriteSpace;
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
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
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
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
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

procedure TPWIGGenPascal.WriteCallerConstructor(AClass: TPWIGClass;
  AIntf: TPWIGInterface);
begin
  Writeln(F, Indent, 'constructor T', AClass.Name, '.Create(AInterfaceHandle: ', AIntf.Name, ');');
  Writeln(F, Indent, 'begin');
  IncIndent;
  Writeln(F, Indent, 'FItemHandle := AInterfaceHandle;');
  Writeln(F, Indent, 'if (FItemHandle = 0) and Assigned(Func', AClass.Name, 'Create) then');
  IncIndent;
  Writeln(F, Indent, 'Func', AClass.Name, 'Create(FItemHandle);');
  DecIndent;
  DecIndent;
  Writeln(F, Indent, 'end;');
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCallerDestructor(AClass: TPWIGClass;
  AIntf: TPWIGInterface);
begin
  Writeln(F, Indent, 'destructor T', AClass.Name, '.Destroy;');
  Writeln(F, Indent, 'begin');
  IncIndent;
  Writeln(F, Indent, 'if Assigned(Func', AClass.Name, 'Destroy) then');
  IncIndent;
  Writeln(F, Indent, 'Func', AClass.Name, 'Destroy(FItemHandle);');
  DecIndent;
  Writeln(F, Indent, 'inherited;');
  DecIndent;
  Writeln(F, Indent, 'end;');
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCallerMethod(AClass: TPWIGClass;
  AIntf: TPWIGInterface; AMethod: TPWIGMethod;
  ADefaultInterface, AGetter: Boolean; const APrefix: string);
var
  FuncName, ParamName, S: string;
  Param, RetVal: TPWIGParam;
  FirstParam: Boolean;
begin
  FuncName := AClass.Name + APrefix + AIntf.Name + AMethod.Name;
  WriteMethod(AClass, AIntf, AMethod, msClassImplementation, ADefaultInterface, AGetter, APrefix);
  // automated string management for UnicodeString
  FirstParam := True;
  for Param in AMethod.Params do
    if (Param.ParamType.BaseType = btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn)  then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      if FirstParam then
      begin
        Writeln(F, Indent, 'var ');
        IncIndent;
        Write(F, Indent);
      end else
        Write(F, '; ');
      FirstParam := False;
      Write(F, 'String__', ParamName, ': AnsiString; Length__', ParamName, ': LongInt');
    end;
  if not FirstParam then
  begin
    Writeln(F, ';');
    DecIndent;
  end;
  // write method body
  Writeln(F, Indent, 'begin');
  IncIndent;
  Writeln(F, Indent, 'if Assigned(Func', FuncName, ') then');
  Writeln(F, Indent, 'begin');
  IncIndent;
  // automated string management for UnicodeString
  for Param in AMethod.Params do
    if (Param.ParamType.BaseType = btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn)  then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      Writeln(F, Indent, 'Length__', ParamName, ' := 0;');
    end;
  if not FirstParam then
  begin
    Writeln(F, Indent, 'if not (');
    IncIndent;
    WriteMethod(AClass, AIntf, AMethod, msLibCallGetLength, ADefaultInterface, AGetter, APrefix);
    DecIndent;
    Writeln(F, Indent, ') then LibCallError(''Func', FuncName, ''');');
  end;
  // automated string management for UnicodeString
  for Param in AMethod.Params do
    if (Param.ParamType.BaseType = btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn)  then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      Writeln(F, Indent, 'SetLength(String__', ParamName, ', Max(Length__', ParamName, ', 1));');
    end;
  Writeln(F, Indent, 'if not (');
  IncIndent;
  WriteMethod(AClass, AIntf, AMethod, msLibCall, ADefaultInterface, AGetter, APrefix);
  DecIndent;
  Writeln(F, Indent, ') then LibCallError(''Func', FuncName, ''');');
  DecIndent;
  Writeln(F, Indent, 'end;');
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
      Writeln(F, Indent, 'if Length__', ParamName, ' > 0 then ', S, ' := String__', ParamName, ' else ', S, ' := '''';')
    end;
  DecIndent;
  Writeln(F, Indent, 'end;');
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteCallerDeclarations(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
begin
  WriteIntfElementProps(AClass);
  Writeln(F, Indent, 'T', AClass.Name, ' = class(TObject)');
  Writeln(F, Indent, 'private');
  IncIndent;
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
          WriteMethod(AClass, LIntf, Prop, msClassDeclaration, Ref.FlagDefault, True, 'Get');
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteMethod(AClass, LIntf, Prop, msClassDeclaration, Ref.FlagDefault, False, 'Set');
        end;
      end;
    end;
  end;
  DecIndent;
  Writeln(F, Indent, 'public');
  IncIndent;
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
        WriteMethod(AClass, LIntf, Method, msClassDeclaration, Ref.FlagDefault, False, '');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        WriteMethod(AClass, LIntf, Prop, msPropertyDeclaration, Ref.FlagDefault, False, '');
      end;
    end;
  end;
  DecIndent;
  Writeln(F, Indent, 'end;');
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
end;

procedure TPWIGGenPascal.WriteCallerImplementations(AClass: TPWIGClass);
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

      // end of interface
      DecIndent;

      // implementation
      Writeln(F, Indent, 'implementation');
      WriteSpace;

      // finish
      Writeln(F, Indent, 'end.');

      Writeln('Pascal common interface file generated:', AFileName);
    except
      Writeln('Could not generate Pascal common interface file :', AFileName);
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TPWIGGenPascal.WriteCalleeFile(const AFileName: string);
var
  LCls: TPWIGClass;
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
      Writeln(F, Indent, '*)');

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

      // write exported function bodies
      for LCls in FPWIG.Classes do
        WriteCalleeExportedFuncs(LCls, True);

      // write library exports
//      Writeln(F, Indent, '// Copy these exports into your main library file.');
      Writeln(F, Indent, '(*');
      Writeln(F, Indent, 'exports');
      IncIndent;
      InitDashSep;
      for LCls in FPWIG.Classes do
        WriteCalleeExports(LCls);
      Writeln(F, Indent, ';');
      DecIndent;
      Writeln(F, Indent, '*)');
      WriteSpace;

      // finish
      Writeln(F, Indent, 'end.');

      Writeln('Pascal callee interface file generated:', AFileName);
    except
      Writeln('Could not generate Pascal callee interface file :', AFileName);
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
      //Writeln(F, Indent, '{$IFDEF FPC}');
      //Writeln(F, Indent, 'LCLIntf, LCLType,');
      //Writeln(F, Indent, '{$ELSE}');
      //Writeln(F, Indent, 'Windows,');
      //Writeln(F, Indent, '{$ENDIF}');
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

      // write library error stub
      Writeln(F, Indent, 'procedure LibCallError(const AFuncName: string);');
      Writeln(F, Indent, 'begin');
      IncIndent;
        Writeln(F, Indent, 'raise Exception.Create(Format(''Error while calling library function %s!'', [AFuncName]));');
      DecIndent;
      Writeln(F, Indent, 'end;');
      WriteSpace;

      // write library error stub
      Writeln(F, Indent, 'procedure LibLoadError(const AFuncName: string);');
      Writeln(F, Indent, 'begin');
      IncIndent;
        Writeln(F, Indent, 'raise Exception.Create(Format(''Requested function %s does not exist in the library!'', [AFuncName]));');
      DecIndent;
      Writeln(F, Indent, 'end;');
      WriteSpace;

      // write library loader implementation
      Writeln(F, Indent, 'function ', FPWIG.Name, 'LibLoad(const FileName: string): Boolean;');
      Writeln(F, Indent, 'begin');
      IncIndent;
      Writeln(F, Indent, 'if LibModule = 0 then');
      IncIndent;
      Writeln(F, Indent, 'LibModule := LoadLibrary(FileName);');
      DecIndent;
      Writeln(F, Indent, 'if LibModule <> 0 then');
      Writeln(F, Indent, 'begin');
      IncIndent;
      for LCls in FPWIG.Classes do
        WriteCallerLibLoads(LCls);
      DecIndent;
      Writeln(F, Indent, 'end;');
      Writeln(F, Indent, 'Result := LibModule <> 0;');
      DecIndent;
      Writeln(F, Indent, 'end;');
      WriteSpace;

      // write library unloader implementation
      Writeln(F, Indent, 'procedure ', FPWIG.Name, 'LibUnload;');
      Writeln(F, Indent, 'begin');
      IncIndent;
      Writeln(F, Indent, 'if LibModule <> 0 then');
      IncIndent;
      Writeln(F, Indent, 'FreeLibrary(LibModule);');
      DecIndent;
      Writeln(F, Indent, 'LibModule := 0;');
      for LCls in FPWIG.Classes do
        WriteCallerPointers(LCls, False);
      DecIndent;
      Writeln(F, Indent, 'end;');
      WriteSpace;

      // write class method bodies
      for LCls in FPWIG.Classes do
        WriteCallerImplementations(LCls);

      // finish
      Writeln(F, Indent, 'end.');

      Writeln('Pascal caller interface file generated:', AFileName);
    except
      Writeln('Could not generate Pascal caller interface file :', AFileName);
    end;
  finally
    CloseFile(F);
  end;

end;

end.

