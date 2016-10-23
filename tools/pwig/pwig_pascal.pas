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

  TPWIGGenPascal = class(TPWIGGenerator)
  private
    FFlags: string;
    FIndent: string;
    procedure IncIndent;
    procedure DecIndent;
    procedure ClearFlags;
    procedure AddFlag(AFlag: Boolean; const AFlagText: string);
    procedure WriteBracketBegin;
    procedure WriteBracketEnd;
    procedure WriteCurlyBegin;
    procedure WriteCurlyEnd;
    procedure WriteSpace;
    property Flags: string read FFlags;
    property Indent: string read FIndent;
  protected
    F: TextFile;
    function CallingConvToString(AConv: TPWIGCallingConv): string; virtual;
    function InterfaceToString(AIntf: TPWIGInterface): string; virtual;
    function TypeToString(AType: TPWIGType): string; virtual;
    function ReplaceNotAllowedParamName(AName: string): string; virtual;
    procedure WriteElementProps(AElement: TPWIGElement); virtual;
    procedure WriteAliasProps(AAlias: TPWIGAlias); virtual;
    procedure WriteEnumProps(AEnum: TPWIGEnum); virtual;
    procedure WriteMethod(AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      ADeclaration, AGetter, ACallingConv, AHandleParam: Boolean; const APrefix: string); virtual;
    procedure WriteMethodBody(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      AGetter, ACallingConv: Boolean; const APrefix: string); virtual;
    procedure WriteConstructor(AClass: TPWIGClass; AIntf: TPWIGInterface); virtual;
    procedure WriteDestructor(AClass: TPWIGClass; AIntf: TPWIGInterface); virtual;
    procedure WriteInterfaceProps(AIntf: TPWIGInterface); virtual;
    procedure WriteClassDeclarations(AClass: TPWIGClass); virtual;
    procedure WriteClassExportedFuncs(AClass: TPWIGClass); virtual;
    procedure WriteClassExports(AClass: TPWIGClass); virtual;
    procedure WritePluginDeclarations(AClass: TPWIGClass); virtual;
    procedure WritePluginImplementations(AClass: TPWIGClass); virtual;
  public
    constructor Create(AOwner: TPWIG); override;
    procedure SaveToFile(const AFileName: string); override;
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

function TPWIGGenPascal.CallingConvToString(AConv: TPWIGCallingConv): string;
begin
  Result := '';
  case AConv of
    ccStdCall: Result := 'stdcall';
    ccCDecl: Result := 'cdecl';
  end;
end;

function TPWIGGenPascal.InterfaceToString(AIntf: TPWIGInterface): string;
begin
  if AIntf.FlagDispEvents then
    Result := 'dispinterface'
  else
    Result := 'interface';
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
    btWideString: Result := 'PAnsiChar'; // UTF8 encoded Unicode string (in Delphi PUTF8Char)
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

procedure TPWIGGenPascal.WriteBracketBegin;
begin
  Writeln(F, Indent, '[');
  IncIndent;
end;

procedure TPWIGGenPascal.WriteBracketEnd;
begin
  DecIndent;
  Writeln(F, Indent, ']');
end;

procedure TPWIGGenPascal.WriteCurlyBegin;
begin
  Writeln(F, Indent, '{');
  IncIndent;
end;

procedure TPWIGGenPascal.WriteCurlyEnd;
begin
  DecIndent;
  Writeln(F, Indent, '};');
end;

procedure TPWIGGenPascal.WriteSpace;
begin
  Writeln(F);
end;

procedure TPWIGGenPascal.SaveToFile(const AFileName: string);
var
  LIntf: TPWIGInterface;
  LCls: TPWIGClass;
  LAlias: TPWIGAlias;
  LEnum: TPWIGEnum;
  Name, Path, Ext, GeneratedFile, IntfName, ImplName: string;
  First: Boolean;
begin
  Path := ExtractFilePath(AFileName);
  Name := ExtractFileRawName(AFileName);
  Ext := ExtractFileExt(AFileName);

  // Note: Keep these 3 parts visually distinguished (don't introduce common functions etc.).
  // It is better for understanding.

  // 1. write interface file (usable both for callee and for caller)
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
      Writeln(F, Indent, '// Library properties:');
      WriteElementProps(FPWIG);

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
        WriteEnumProps(LEnum);

      // write aliases
      for LAlias in FPWIG.Aliases do
        WriteAliasProps(LAlias);

      // write interfaces
      for LIntf in FPWIG.Interfaces do
        WriteInterfaceProps(LIntf);

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

  // 2. write wrapper file for the callee
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
      Writeln(F, Indent, '// Library properties:');
      WriteElementProps(FPWIG);

      // interface
      Writeln(F, Indent, 'interface');
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
        WriteClassDeclarations(LCls);

      // end of interface
      DecIndent;
      Writeln(F, Indent, '*)');
      WriteSpace;

      // implementation
      Writeln(F, Indent, 'implementation');
      WriteSpace;

      // write uses clause
      // now hardcoded, later might be automated when needed
      Writeln(F, Indent, 'uses');
      IncIndent;
      Writeln(F, Indent, 'SysUtils', ', ', IntfName, ', ', ImplName, ';');
      DecIndent;
      WriteSpace;

      // write exported function bodies
      for LCls in FPWIG.Classes do
        WriteClassExportedFuncs(LCls);

      // write library exports
      Writeln(F, Indent, 'exports');
      IncIndent;
      First := True;
      for LCls in FPWIG.Classes do
      begin
        if not First then
          Writeln(F, ',');
        WriteClassExports(LCls);
        First := False;
      end;
      Writeln(F, ';');
      DecIndent;

      // finish
      Writeln(F, Indent, 'end.');

      Writeln('Pascal callee interface file generated:', AFileName);
    except
      Writeln('Could not generate Pascal callee interface file :', AFileName);
    end;
  finally
    CloseFile(F);
  end;

  // 3. write wrapper file for the caller
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
      Writeln(F, Indent, '// Library properties:');
      WriteElementProps(FPWIG);

      // interface
      Writeln(F, Indent, 'interface');
      WriteSpace;

      // write uses clause
      // now hardcoded, later might be automated when needed
      Writeln(F, Indent, 'uses');
      IncIndent;
      Writeln(F, Indent, 'SysUtils', ', ', IntfName, ';');
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

      // write class interfaces (implementation must be done by library author)
      for LCls in FPWIG.Classes do
        WritePluginDeclarations(LCls);

      // end of interface
      DecIndent;

      // implementation
      Writeln(F, Indent, 'implementation');
      WriteSpace;

      // write exported function bodies
      for LCls in FPWIG.Classes do
        WritePluginImplementations(LCls);

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

procedure TPWIGGenPascal.WriteElementProps(AElement: TPWIGElement);
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

procedure TPWIGGenPascal.WriteAliasProps(AAlias: TPWIGAlias);
begin
  Writeln(F, Indent, '// Alias type properties:');
  WriteElementProps(AAlias);
  Writeln(F, Indent, AAlias.Name, ' = type ', TypeToString(AAlias.AliasedType), ';');
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteEnumProps(AEnum: TPWIGEnum);
var
  I: Integer;
  Elem: TPWIGEnumElement;
begin
  Writeln(F, Indent, '// Enumerated type properties:');
  WriteElementProps(AEnum);
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

procedure TPWIGGenPascal.WriteMethod(AIntf: TPWIGInterface; AMethod: TPWIGMethod;
  ADeclaration, AGetter, ACallingConv, AHandleParam: Boolean; const APrefix: string);
var
  I: Integer;
  Elem: TPWIGParam;
  CallingConv, HandleParam, ParamName: string;
begin
  if ACallingConv then
    CallingConv := ' ' + CallingConvToString(AMethod.CallingConv) + ';'
  else
    CallingConv := '';
  if AHandleParam then
    HandleParam :=  'const ItemHandle: ' + AIntf.Name
  else
    HandleParam := '';
  if ADeclaration then
    Write(F, Indent, 'T', APrefix, AIntf.Name, AMethod.Name, ' = function(', HandleParam)
  else
    Write(F, Indent, 'function ', APrefix, AIntf.Name, AMethod.Name, '(', HandleParam);
  if AMethod.Params.Count = 0 then
    Write(F, ')')
  else
  begin
    if AHandleParam then
      Write(F, '; ');
    for I := 0 to AMethod.Params.Count - 1 do
    begin
      Elem := AMethod.Params[I];
      ParamName := ReplaceNotAllowedParamName(Elem.Name);
      if AMethod is TPWIGProperty then
      begin
        // all params are [in] except last one which is [out, retval] for a getter
        if not AGetter or (I < AMethod.Params.Count - 1) then
          Write(F, 'const ', ParamName, ': ', TypeToString(Elem.ParamType))
        else
          Write(F, 'out ', ParamName, ': ', TypeToString(Elem.ParamType));
      end else
      begin
        // write param flags as specified
        if Elem.FlagInput and Elem.FlagOutput then
          Write(F, 'var ', ParamName, ': ', TypeToString(Elem.ParamType))
        else if Elem.FlagOutput then
          Write(F, 'out ', ParamName, ': ', TypeToString(Elem.ParamType))
        else
          Write(F, 'const ', ParamName, ': ', TypeToString(Elem.ParamType))
      end;
      if I < AMethod.Params.Count - 1 then
        Write(F, '; ')
      else
        Write(F, ')');
    end;
  end;
  Writeln(F, ':Boolean;', CallingConv);
end;

procedure TPWIGGenPascal.WriteMethodBody(AClass: TPWIGClass; AIntf: TPWIGInterface;
  AMethod: TPWIGMethod; AGetter, ACallingConv: Boolean; const APrefix: string);
var
  I: Integer;
  Elem: TPWIGParam;
  ParamName: string;
begin
  WriteMethod(AIntf, AMethod, False, AGetter, ACallingConv, True, APrefix);
  Writeln(F, Indent, 'begin');
  IncIndent;
  Writeln(F, Indent, 'Result := False;');
  Writeln(F, Indent, 'try');
  IncIndent;
  Writeln(F, Indent, 'if TObject(ItemHandle) is T', AClass.Name, ' then');
  IncIndent;
  Write(F, Indent, 'Result := T', AClass.Name, '(ItemHandle).', APrefix, AIntf.Name, AMethod.Name, '(');
  for I := 0 to AMethod.Params.Count - 1 do
  begin
    Elem := AMethod.Params[I];
    ParamName := ReplaceNotAllowedParamName(Elem.Name);
    Write(F, ParamName);
    if I < AMethod.Params.Count - 1 then
      Write(F, ', ');
  end;
  Writeln(F, ');');
  DecIndent;
  DecIndent;
  Writeln(F, Indent, 'except');
  Writeln(F, Indent, 'end;');
  DecIndent;
  Writeln(F, Indent, 'end;');
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteConstructor(AClass: TPWIGClass; AIntf: TPWIGInterface);
begin
  Writeln(F, Indent, 'function ', AClass.Name, 'Create: ', AIntf.Name, '; cdecl;');
  Writeln(F, Indent, 'begin');
  IncIndent;
  Writeln(F, Indent, 'try');
  IncIndent;
  Writeln(F, Indent, 'Result := ', AIntf.Name, '(T', AClass.Name, '.Create);');
  DecIndent;
  Writeln(F, Indent, 'except');
  IncIndent;
  Writeln(F, Indent, 'Result := 0;');
  DecIndent;
  Writeln(F, Indent, 'end;');
  DecIndent;
  Writeln(F, Indent, 'end;');
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteDestructor(AClass: TPWIGClass; AIntf: TPWIGInterface);
begin
  Writeln(F, Indent, 'function ', AClass.Name, 'Destroy(ItemHandle: ', AIntf.Name, '): Boolean; cdecl;');
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

procedure TPWIGGenPascal.WriteInterfaceProps(AIntf: TPWIGInterface);
var
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
begin
  Writeln(F, Indent, '// Interface type properties:');
  WriteElementProps(AIntf);
  Writeln(F, Indent, '// Methods:');
  for Method in AIntf.Methods do
  begin
    WriteMethod(AIntf, Method, True, False, True, True, '');
  end;
  Writeln(F, Indent, '// Properties:');
  for Prop in AIntf.Properties do
  begin
    if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
    begin
      // write getter
      WriteMethod(AIntf, Prop, True, True, True, True, 'Get');
    end;
    if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
    begin
      // write setter
      WriteMethod(AIntf, Prop, True, False, True, True, 'Set');
    end;
  end;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteClassDeclarations(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
begin
  WriteElementProps(AClass);
  Writeln(F, Indent, 'T', AClass.Name, ' = class(TObject)');
  Writeln(F, Indent, 'public');
  IncIndent;
  Writeln(F, Indent, 'constructor Create;');
  Writeln(F, Indent, 'destructor Destroy; override;');

  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if LIntf <> nil then
    begin
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        WriteMethod(LIntf, Method, False, False, False, False, '');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteMethod(LIntf, Prop, False, True, False, False, 'Get');
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteMethod(LIntf, Prop, False, False, False, False, 'Set');
        end;
      end;
    end;
  end;
  DecIndent;
  Writeln(F, Indent, 'end;');
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteClassExportedFuncs(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
begin
  WriteElementProps(AClass);
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
    begin
      Writeln(F, Indent, '// Constructor:');
      WriteConstructor(AClass, LIntf);
      Writeln(F, Indent, '// Destructor:');
      WriteDestructor(AClass, LIntf);
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        WriteMethodBody(AClass, LIntf, Method, False, True, '');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteMethodBody(AClass, LIntf, Prop, True, True, 'Get');
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteMethodBody(AClass, LIntf, Prop, False, True, 'Set');
        end;
      end;
    end;
  end;
  WriteSpace;
end;

procedure TPWIGGenPascal.WriteClassExports(AClass: TPWIGClass);
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
      Write(F, Indent, AClass.Name, 'Create');
      Writeln(F, ',');
      Write(F, Indent, AClass.Name, 'Destroy');
      for Method in LIntf.Methods do
      begin
        Writeln(F, ',');
        Write(F, Indent, LIntf.Name, Method.Name);
      end;
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          Writeln(F, ',');
          Write(F, Indent, 'Get', LIntf.Name, Prop.Name);
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          Writeln(F, ',');
          Write(F, Indent, 'Set', LIntf.Name, Prop.Name);
        end;
      end;
    end;
  end;
end;

procedure TPWIGGenPascal.WritePluginDeclarations(AClass: TPWIGClass);
begin
  //todo
end;

procedure TPWIGGenPascal.WritePluginImplementations(AClass: TPWIGClass);
begin
  //todo
end;

end.

