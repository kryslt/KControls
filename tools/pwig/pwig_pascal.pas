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
    msLibExport,
    msClassDeclaration,
    msClassCall
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
    procedure WriteBracketBegin;
    procedure WriteBracketEnd;
    procedure WriteCurlyBegin;
    procedure WriteCurlyEnd;
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
    procedure WriteMethod(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      AStyle: TPWIGGenPascalMethodStyle; ADefaultInterface, AGetter: Boolean; const APrefix: string); virtual;

    procedure WriteCalleeConstructor(AClass: TPWIGClass; AIntf: TPWIGInterface); virtual;
    procedure WriteCalleeDestructor(AClass: TPWIGClass; AIntf: TPWIGInterface); virtual;
    procedure WriteCalleeMethodBody(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      ADefaultInterface, AGetter: Boolean; const APrefix: string); virtual;
    procedure WriteCalleeDeclarations(AClass: TPWIGClass); virtual;
    procedure WriteCalleeExportedFuncs(AClass: TPWIGClass); virtual;
    procedure WriteCalleeExports(AClass: TPWIGClass); virtual;

    procedure WriteCallerConstructor(AClass: TPWIGClass; AIntf: TPWIGInterface); virtual;
    procedure WriteCallerDestructor(AClass: TPWIGClass; AIntf: TPWIGInterface); virtual;
    procedure WriteCallerMethodBody(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      ADefaultInterface, AGetter: Boolean; const APrefix: string); virtual;
    procedure WriteCallerDeclarations(AClass: TPWIGClass); virtual;
    procedure WriteCallerImplementations(AClass: TPWIGClass); virtual;
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

procedure TPWIGGenPascal.InitDashSep;
begin
  FFirstDashSep := True;
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
      WriteIntfElementProps(FPWIG);

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
        WriteCalleeDeclarations(LCls);

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
        WriteCalleeExportedFuncs(LCls);

      // write library exports
      Writeln(F, Indent, 'exports');
      IncIndent;
      InitDashSep;
      for LCls in FPWIG.Classes do
        WriteCalleeExports(LCls);
      Writeln(F, Indent, ';');
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
      WriteIntfElementProps(FPWIG);

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
        WriteCallerDeclarations(LCls);

      // end of interface
      DecIndent;

      // implementation
      Writeln(F, Indent, 'implementation');
      WriteSpace;

      // write exported function bodies
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

procedure TPWIGGenPascal.WriteMethod(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
  AStyle: TPWIGGenPascalMethodStyle; ADefaultInterface, AGetter: Boolean; const APrefix: string);
var
  Param, RetVal, PropRetVal: TPWIGParam;
  CallingConv, HandleParam, IntfName, ParamName, ParamSpecifier, FuncKeyword, RetValAssignment: string;
  FirstParam: Boolean;
begin
  if AStyle in [msIntfDeclaration, msLibExport] then
  begin
    CallingConv := '; ' + CallingConvToString(AMethod.CallingConv);
    HandleParam :=  'const ItemHandle: ' + AIntf.Name;
    RetVal := nil;
    FuncKeyword := '';
    if AGetter then
      PropRetVal := AMethod.Params.Last
    else
      PropRetVal := nil;
    IntfName := AIntf.name;
  end
  else
  begin
    CallingConv := '';
    HandleParam := '';
    if AGetter then
      RetVal := AMethod.Params.Last
    else
      RetVal := AMethod.Params.FindRetVal;
    if RetVal <> nil then
    begin
      FuncKeyword := 'function';
      RetValAssignment :=  ReplaceNotAllowedParamName(RetVal.Name) + ' := ';
    end else
    begin
      FuncKeyword := 'procedure';
      RetValAssignment :=  '';
    end;
    PropRetVal := nil;
    if ADefaultInterface then
      IntfName := ''
    else
      IntfName := AIntf.Name;
  end;
  case AStyle of
    msIntfDeclaration:
      Write(F, Indent, 'T', APrefix, IntfName, AMethod.Name, ' = function(', HandleParam);
    msLibExport:
      Write(F, Indent, 'function ', APrefix, IntfName, AMethod.Name, '(', HandleParam);
    msClassDeclaration:
      Write(F, Indent, FuncKeyword, ' ', APrefix, IntfName, AMethod.Name);
    msClassCall:
      Write(F, Indent, RetValAssignment, 'T', AClass.Name, '(ItemHandle).', APrefix, IntfName, AMethod.Name);
  end;
  if (AMethod.Params.Count = 0) or (AMethod.Params.Count = 1) and (RetVal <> nil) then
  begin
    if HandleParam <> '' then
      Write(F, ')');
  end
  else
  begin
    if HandleParam <> '' then
      Write(F, '; ')
    else
      Write(F, '(');
    FirstParam := True;
    for Param in AMethod.Params do
    begin
      if Param <> RetVal then
      begin
        if not FirstParam then
        begin
          if AStyle = msClassCall then
            Write(F, ', ')
          else
            Write(F, '; ');
        end;
        FirstParam := False;
        ParamName := ReplaceNotAllowedParamName(Param.Name);
        if AMethod is TPWIGProperty then
        begin
          // all params are [in] except last one which is [out, retval] for a getter
          if not AGetter or (Param <> PropRetVal) then
            ParamSpecifier := 'const'
          else
            ParamSpecifier := 'out';
        end else
        begin
          // write param flags as specified
          if Param.FlagInput and Param.FlagOutput then
            ParamSpecifier := 'var'
          else if Param.FlagOutput then
            ParamSpecifier := 'out'
          else
            ParamSpecifier := 'const';
        end;
        if AStyle = msClassCall then
          Write(F, ParamName)
        else
          Write(F, ParamSpecifier, ' ', ParamName, ': ', TypeToString(Param.ParamType));
      end;
    end;
    Write(F, ')');
  end;
  if AStyle in [msIntfDeclaration, msLibExport] then
    Write(F, ': Boolean', CallingConv)
  else if (AStyle = msClassDeclaration) and (RetVal <> nil) then
    Write(F, ': ', TypeToString(RetVal.ParamType));
  Writeln(F, ';');
end;

procedure TPWIGGenPascal.WriteCalleeMethodBody(AClass: TPWIGClass; AIntf: TPWIGInterface;
 AMethod: TPWIGMethod;  ADefaultInterface, AGetter: Boolean; const APrefix: string);
begin
  WriteMethod(AClass, AIntf, AMethod, msLibExport, False, AGetter, APrefix);
  Writeln(F, Indent, 'begin');
  IncIndent;
  Writeln(F, Indent, 'Result := False;');
  Writeln(F, Indent, 'try');
  IncIndent;
  Writeln(F, Indent, 'if TObject(ItemHandle) is T', AClass.Name, ' then');
  Writeln(F, Indent, 'begin');
  IncIndent;
  WriteMethod(AClass, AIntf, AMethod, msClassCall, ADefaultInterface, AGetter, APrefix);
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

procedure TPWIGGenPascal.WriteCalleeConstructor(AClass: TPWIGClass; AIntf: TPWIGInterface);
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

procedure TPWIGGenPascal.WriteCalleeDestructor(AClass: TPWIGClass; AIntf: TPWIGInterface);
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

procedure TPWIGGenPascal.WriteCalleeExportedFuncs(AClass: TPWIGClass);
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
        WriteCalleeConstructor(AClass, LIntf);
        Writeln(F, Indent, '// Destructor:');
        WriteCalleeDestructor(AClass, LIntf);
      end;
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        WriteCalleeMethodBody(AClass, LIntf, Method, Ref.FlagDefault, False, '');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteCalleeMethodBody(AClass, LIntf, Prop, Ref.FlagDefault, True, 'Get');
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteCalleeMethodBody(AClass, LIntf, Prop, Ref.FlagDefault, False, 'Set');
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
        Write(F, Indent, LIntf.Name, Method.Name);
      end;
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteDashSep;
          Write(F, Indent, 'Get', LIntf.Name, Prop.Name);
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteDashSep;
          Write(F, Indent, 'Set', LIntf.Name, Prop.Name);
        end;
      end;
    end;
  end;
  Writeln(F);
end;

procedure TPWIGGenPascal.WriteCallerConstructor(AClass: TPWIGClass;
  AIntf: TPWIGInterface);
begin

end;

procedure TPWIGGenPascal.WriteCallerDestructor(AClass: TPWIGClass;
  AIntf: TPWIGInterface);
begin

end;

procedure TPWIGGenPascal.WriteCallerMethodBody(AClass: TPWIGClass;
  AIntf: TPWIGInterface; AMethod: TPWIGMethod;
  ADefaultInterface, AGetter: Boolean; const APrefix: string);
begin

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
  Writeln(F, Indent, 'constructor Create;');
  Writeln(F, Indent, 'destructor Destroy; override;');

  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
    begin
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        WriteMethod(AClass, LIntf, Method, msClassDeclaration, Ref.FlagDefault, False, '');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        Write(F, Indent, 'property ', Prop.Name, ': ', TypeToString(Prop.Params.Last.ParamType));
        if Ref.FlagDefault then
          IntfName := ''
        else
          IntfName := LIntf.Name;
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          Write(F, ' read Get', IntfName, Prop.Name);
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          Write(F, ' write Set', IntfName, Prop.Name);
        end;
        Writeln(F, ';');
      end;
    end;
  end;
  DecIndent;
  Writeln(F, Indent, 'end;');
  WriteSpace;
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
        WriteCallerMethodBody(AClass, LIntf, Method, False, True, '');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteCallerMethodBody(AClass, LIntf, Prop, True, True, 'Get');
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteCallerMethodBody(AClass, LIntf, Prop, False, True, 'Set');
        end;
      end;
    end;
  end;
  WriteSpace;
end;

end.

