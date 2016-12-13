{ @abstract(This unit contains PWIG C# wrapper class generator (usable by C# IDEs))
  @author(Tomas Krysl (tk@tkweb.eu))
  @created(23 Oct 2016)

  Copyright © Tomas Krysl (tk@@tkweb.eu)<BR><BR>

  Generated outputs tested in:
  -Visual Studio Community 2015 + C# 6.0 (caller)
  -Xamarin for Visual Studio 4.2, Android + iOS targets (caller)

  <B>License:</B><BR>
  This code is distributed as a freeware. You are free to use it as part
  of your application for any purpose including freeware, commercial and
  shareware applications. The origin of this source code must not be
  misrepresented; you must not claim your authorship. You may modify this code
  solely for your own purpose. Please feel free to contact the author if you
  think your changes might be useful for other users. You may distribute only
  the original package. The author accepts no liability for any damage
  that may result from using this code. }

unit pwig_csharp;

{$mode delphi}

interface

uses
  Classes, SysUtils, PWIGGen;

type

  TPWIGGenCSharpMethodStyle = (
    msLibCallGetLength,
    msLibCall,
    msLibExport,
    msClassImplementation,
    msClassCall,
    msPropertyDeclaration,
    msEventHandler,
    msEventDeclaration,
    msEventSinkCallGetLength,
    msEventSinkCall
  );

  { TPWIGGenCSharp }

  TPWIGGenCSharp = class(TPWIGGenerator)
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
    procedure WriteCurlyBegin;
    procedure WriteCurlyEnd;
    procedure WriteDashSep;
    procedure WriteSpace;
    property Flags: string read FFlags;
    property Indent: string read FIndent;
  protected
    F: TextFile;
    function CallingConvToString(AMethod: TPWIGMethod): string; virtual;
    function TypeToString(AType: TPWIGType): string; virtual;
    function TypeToNullValue(AType: TPWIGType): string; virtual;
    function ReplaceNotAllowedParamName(AName: string): string; virtual;
    function ReplaceNotAllowedMethodName(AName: string): string; virtual;
    function ReturnParamName(AMethod: TPWIGMethod; AGetter: Boolean): string; virtual;
    procedure WriteIntfElementProps(AElement: TPWIGElement); virtual;
    procedure WriteIntfAliasProps(AAlias: TPWIGAlias); virtual;
    procedure WriteIntfEnumProps(AEnum: TPWIGEnum); virtual;
    procedure WriteMethod(const AClassName: string; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      AStyle: TPWIGGenCSharpMethodStyle; ADefaultInterface, AGetter: Boolean;
      const APrefix, AVisibility: string); virtual;

    function WriteCalleeUnicodeStringDeclarations(AClass: TPWIGClass; AIntf: TPWIGInterface;
       AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;
    function WriteCalleeUnicodeStringManagement1(AClass: TPWIGClass; AIntf: TPWIGInterface;
       AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;
    function WriteCalleeUnicodeStringManagement2(AClass: TPWIGClass; AIntf: TPWIGInterface;
       AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;

    procedure WriteCallerConstructor(AClass: TPWIGClass; AIntf: TPWIGInterface); virtual;
    procedure WriteCallerDestructor(AClass: TPWIGClass; AIntf: TPWIGInterface); virtual;
    procedure WriteCallerMethod(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      ADefaultInterface, AGetter: Boolean; const APrefix, AVisibility: string); virtual;
    procedure WriteCallerEventCallback(AClass: TPWIGClass; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
      ADefaultInterface: Boolean); virtual;
    procedure WriteCallerLibLoads(AClass: TPWIGClass); virtual;
    procedure WriteCallerImplementations(AClass: TPWIGClass); virtual;
    function WriteCallerOutputParamInits(AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;
    function WriteCallerUnicodeStringDeclarations(AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;
    function WriteCallerUnicodeStringManagement1(AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;
    function WriteCallerUnicodeStringManagement2(AMethod: TPWIGMethod; AGetter: Boolean): Boolean; virtual;

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

{ TPWIGGenCSharp }

constructor TPWIGGenCSharp.Create(AOwner: TPWIG);
begin
  inherited Create(AOwner);
  FIndent := '';
end;

procedure TPWIGGenCSharp.IncIndent;
begin
  FIndent := FIndent + '  ';
end;

procedure TPWIGGenCSharp.DecIndent;
begin
  Delete(FIndent, 1, 2);
end;

procedure TPWIGGenCSharp.ClearFlags;
begin
  FFlags := '';
end;

procedure TPWIGGenCSharp.AddFlag(AFlag: Boolean; const AFlagText: string);
begin
  if AFlag then
  begin
    if FFlags <> '' then
      FFlags := FFlags + ', ';
    FFlags := FFlags + AFlagText;
  end;
end;

procedure TPWIGGenCSharp.InitDashSep;
begin
  FFirstDashSep := True;
end;

function TPWIGGenCSharp.ParamDirection(AMethod: TPWIGMethod;
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

function TPWIGGenCSharp.ParamDirectionToString(ADirection: TPWIGParamDirection
  ): string;
begin
  Result := '';
  case ADirection of
    pdIn: Result := '';
    pdOut: Result := 'out';
    pdInOut: Result := 'ref';
  end;
end;

function TPWIGGenCSharp.CallingConvToString(AMethod: TPWIGMethod): string;
var
  Conv: TPWIGCallingConv;
begin
  if (AMethod <> nil) and (AMethod.CallingConv <> ccNone) then
    Conv := AMethod.CallingConv
  else
    Conv := FPWIG.GlobalCallingConv;
  Result := '';
  case Conv of
    ccCDecl: Result := 'Cdecl';
    ccPascal: Result := 'Pascal';
    ccStdCall: Result := 'Stdcall';
    ccSafeCall: Result := 'Safecall';
  end;
  if Result <> '' then
    Result := 'System.Runtime.InteropServices.CallingConvention.'+Result;
end;

function TPWIGGenCSharp.TypeToString(AType: TPWIGType): string;
begin
  Result := '';
  case AType.BaseType of
    btLongInt: Result := 'System.Int32';
    btLongWord: Result := 'System.UInt32';
    btSmallInt: Result := 'System.Int16';
    btWord: Result := 'System.UInt16';
    btInt64: Result := 'System.Int64';
    btUInt64: Result := 'System.UInt64';
    btSingle: Result := 'System.Single';
    btDouble: Result := 'System.Double';
    btUnicodeString: Result := 'System.String'; // managed Unicode string
    btRawByteString: Result := 'System.IntPtr'; // needs to be unmarshalled to byte buffer
    btCurrency: Result := 'System.Decimal'; // hopefully the same format
    btDateTime: Result := 'System.DateTime'; // hopefully the same format
    btEnum: Result := FPWIG.Enums.FindName(AType.CustomTypeGUID, AType.CustomTypeName);
    btAlias: Result := FPWIG.Aliases.FindName(AType.CustomTypeGUID, AType.CustomTypeName);
    btInterface: Result := FPWIG.Interfaces.FindName(AType.CustomTypeGUID, AType.CustomTypeName);
  end;
end;

function TPWIGGenCSharp.TypeToNullValue(AType: TPWIGType): string;
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
    btUnicodeString: Result := '""';
    btRawByteString: Result := 'System.IntPtr.Zero';
    btCurrency: Result := '0';
    btDateTime: Result := '0';
    btEnum:
      begin
        LEnum := FPWIG.Enums.Find(AType.CustomTypeGUID, AType.CustomTypeName);
        Result := LEnum.Name + '.' + LEnum.Elements[0].Name;
      end;
    btAlias:
      begin
        LAlias := FPWIG.Aliases.Find(AType.CustomTypeGUID, AType.CustomTypeName);
        Result := TypeToNullValue(LAlias.AliasedType);
      end;
    btInterface: Result := '0';
  end;
end;

function TPWIGGenCSharp.ReplaceNotAllowedParamName(AName: string): string;
begin
  Result := AName;
  // no known cases
end;

function TPWIGGenCSharp.ReplaceNotAllowedMethodName(AName: string): string;
begin
  Result := AName;
  if AName = 'Finalize' then
    Result := Result + 'Ex';
end;

function TPWIGGenCSharp.ReturnParamName(AMethod: TPWIGMethod; AGetter: Boolean): string;
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

procedure TPWIGGenCSharp.WriteDashSep;
begin
  if not FFirstDashSep then
    Writeln(F, ',');
  FFirstDashSep := False;
end;

procedure TPWIGGenCSharp.WriteSpace;
begin
  Writeln(F);
end;

procedure TPWIGGenCSharp.WriteCurlyBegin;
begin
  Writeln(F, Indent, '{');
  IncIndent;
end;

procedure TPWIGGenCSharp.WriteCurlyEnd;
begin
  DecIndent;
  Writeln(F, Indent, '}');
end;

procedure TPWIGGenCSharp.SaveCalleeFiles(const AFileName: string);
begin
  WriteInterfaceFile(AFileName);
  WriteCalleeFile(AFileName);
end;

procedure TPWIGGenCSharp.SaveCallerFiles(const AFileName: string);
begin
  WriteInterfaceFile(AFileName);
  WriteCallerFile(AFileName);
end;

procedure TPWIGGenCSharp.WriteIntfElementProps(AElement: TPWIGElement);
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

procedure TPWIGGenCSharp.WriteIntfAliasProps(AAlias: TPWIGAlias);
begin
  Writeln(F, Indent, '// Alias type properties:');
  WriteIntfElementProps(AAlias);
  Writeln(F, Indent, 'using ', AAlias.Name, ' = ', TypeToString(AAlias.AliasedType), ';');
  WriteSpace;
end;

procedure TPWIGGenCSharp.WriteIntfEnumProps(AEnum: TPWIGEnum);
var
  I: Integer;
  Elem: TPWIGEnumElement;
begin
  Writeln(F, Indent, '// Enumerated type properties:');
  WriteIntfElementProps(AEnum);
  Writeln(F, Indent, 'public enum ', AEnum.Name, ' {');
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
  Writeln(F, Indent, '};');
  WriteSpace;
end;

procedure TPWIGGenCSharp.WriteMethod(const AClassName: string; AIntf: TPWIGInterface; AMethod: TPWIGMethod;
  AStyle: TPWIGGenCSharpMethodStyle; ADefaultInterface, AGetter: Boolean; const APrefix, AVisibility: string);

  procedure WriteLibCallUnicodeStringParam(AParamDir: TPWIGParamDirection; const AParamName: string);
  begin
    if AParamDir = pdIn then
      Write(F, 'LibInvoke.NativeUtf8FromString(', AParamName, ')')
    else if AStyle in [msLibCallGetLength, msEventSinkCallGetLength] then
      Write(F, 'System.IntPtr.Zero, ref Length__', AParamName)
    else
      Write(F, 'Array__', AParamName, ', ref Length__', AParamName);
  end;

var
  Param, RetVal: TPWIGParam;
  ParamDir: TPWIGParamDirection;
  HandleParam, IntfName, ParamName, ParamSpecifier, RetValAssignment: string;
  FirstParam: Boolean;
begin
  if AStyle in [msLibExport, msEventHandler] then
  begin
    HandleParam := AIntf.Name + ' ItemHandle';
    RetVal := nil;
    IntfName := AIntf.name;
  end
  else if AStyle in [msLibCall, msLibCallGetLength] then
  begin
    HandleParam := 'FItemHandle';
    if AGetter then
      RetVal := AMethod.Params.Last
    else
      RetVal := AMethod.Params.FindRetVal;
    IntfName := AIntf.Name;
  end
  else if AStyle in [msEventSinkCall, msEventSinkCallGetLength] then
  begin
    HandleParam := 'F' + AMethod.Name + 'EventSink';
    RetVal := nil;
    RetValAssignment := '';
    IntfName := '';
  end
  else if AStyle in [msClassCall] then
  begin
    HandleParam := '';
    RetVal := nil;
    if AGetter then
    begin
      RetVal := AMethod.Params.Last;
      RetValAssignment := 'return '
    end else
    begin
      RetVal := AMethod.Params.FindRetVal;
      RetValAssignment := '';
    end;
    if ADefaultInterface then
      IntfName := ''
    else
      IntfName := AIntf.Name;
  end else
  begin
    HandleParam := '';
    if AGetter then
      RetVal := AMethod.Params.Last
    else
      RetVal := AMethod.Params.FindRetVal;
    if RetVal <> nil then
    begin
      ParamName := ReplaceNotAllowedParamName(RetVal.Name);
      if RetVal.ParamType.BaseType = btUnicodeString then
        ParamName := 'String__' + AClassName + AIntf.Name + AMethod.Name + ParamName;
      RetValAssignment := ParamName + ' = ';
    end else
      RetValAssignment :=  '';
    if ADefaultInterface then
      IntfName := ''
    else
      IntfName := AIntf.Name;
  end;
  case AStyle of
    msLibCall, msLibCallGetLength:
      Write(F, Indent, 'LibInvoke.', AClassName, APrefix, IntfName, AMethod.Name, '(', HandleParam);
    msLibExport, msEventHandler:
      Write(F, Indent, AVisibility, ' System.Boolean ', AClassName, APrefix, IntfName, AMethod.Name, '(', HandleParam);
    msClassImplementation:
      begin
        if RetVal <> nil then
          Write(F, Indent, AVisibility, ' ', TypeToString(RetVal.ParamType))
        else
          Write(F, Indent, AVisibility, ' void');
        Write(F, ' ', APrefix, IntfName, ReplaceNotAllowedMethodName(AMethod.Name));
      end;
    msClassCall:
      Write(F, Indent, RetValAssignment, APrefix, IntfName, AMethod.Name);
    msPropertyDeclaration:
      begin
        if RetVal <> nil then
          Write(F, Indent, AVisibility, ' ', TypeToString(RetVal.ParamType))
        else
          Write(F, Indent, AVisibility, ' void');
        Write(F, ' ', IntfName, ReplaceNotAllowedMethodName(AMethod.Name));
      end;
    msEventDeclaration:
      Write(F, Indent, AVisibility, ' delegate void ', AIntf.Name, AMethod.Name, APrefix);
    msEventSinkCall, msEventSinkCallGetLength:
      Write(F, Indent, 'F', AMethod.Name, 'EventHandler(', HandleParam);
  end;
  if (AMethod.Params.Count = 0) or (AMethod.Params.Count = 1) and (RetVal <> nil) then
  begin
    if AStyle <> msPropertyDeclaration then
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
          Write(F, 'out ', ParamName);
      end
      else if HandleParam = '' then
        Write(F, '(');
      Write(F, ')');
    end;
  end else
  begin
    if HandleParam <> '' then
    begin
      if AStyle in [msClassCall, msLibCall, msLibCallGetLength, msEventSinkCall, msEventSinkCallGetLength] then
        Write(F, ', ')
      else
        Write(F, ', ');
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
          Write(F, ', ')
        end;
        FirstParam := False;
        ParamSpecifier := ParamDirectionToString(ParamDir) + ' ';
        if (Param.ParamType.BaseType = btUnicodeString) and (AStyle in [msLibExport, msEventHandler]) then
        begin
          // automated string management for UnicodeString
          if ParamDir = pdIn then
            Write(F, ParamSpecifier, 'System.IntPtr ', ParamName)
          else
            Write(F, 'System.IntPtr ', ParamName, ', ref System.Int32 Length__', ParamName);
        end
        else if (Param.ParamType.BaseType = btUnicodeString) and (AStyle in [msClassCall]) then
        begin
          if ParamDir = pdIn then
            Write(F, 'LibInvoke.StringFromNativeUtf8(', ParamName, ')')
          else
            Write(F, ParamSpecifier, 'String__' + AClassName + AIntf.Name + AMethod.Name + ParamName);
        end
        else if (Param.ParamType.BaseType = btUnicodeString) and (AStyle in [msLibCall, msLibCallGetLength, msEventSinkCall, msEventSinkCallGetlength]) then
        begin
          WriteLibCallUnicodeStringParam(ParamDir, ParamName);
        end
        else if AStyle in [msClassCall, msLibCall, msLibCallGetLength, msEventSinkCall, msEventSinkCallGetLength] then
          Write(F, ParamSpecifier, ParamName)
        else if AStyle = msPropertyDeclaration then
          Write(F, TypeToString(Param.ParamType), ' ', ParamName)
        else
          Write(F, ParamSpecifier, TypeToString(Param.ParamType), ' ', ParamName);
      end
      else if AStyle in [msLibCall, msLibCallGetLength] then
      begin
        Write(F, ', ');
        if Param.ParamType.BaseType = btUnicodeString then
        begin
          WriteLibCallUnicodeStringParam(ParamDir, ParamName);
        end else
          Write(F, 'out ', ParamName);
      end;
    end;
    if AStyle = msPropertyDeclaration then
      Write(F, ']')
    else
      Write(F, ')');
  end;
  if AStyle in [msLibExport, msClassCall, msEventDeclaration] then
    Writeln(F, ';')
  else
    Writeln(F);
end;

function TPWIGGenCsharp.WriteCalleeUnicodeStringDeclarations(
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
      Result := True;
      Writeln(F, Indent, 'private static System.String String__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ';');
    end;
  if Result then
  begin
    // because the function is called twice, var or out parameters are not set on the second call
    // we must store them in a temporary variable as well
    for Param in AMethod.Params do
      if (Param.ParamType.BaseType <> btUnicodeString) and (ParamDirection(AMethod, Param, AGetter) <> pdIn)  then
      begin
        ParamName := ReplaceNotAllowedParamName(Param.Name);
        Writeln(F, Indent, 'private static ', TypeToString(Param.ParamType), ' Tmp__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ';');
      end;
  end;
  WriteSpace;
end;

function TPWIGGenCsharp.WriteCalleeUnicodeStringManagement1(
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
      Writeln(F, Indent, 'if (', ParamName, ' != System.IntPtr.Zero)');
      WriteCurlyBegin;
      Exit;
    end;
end;

function TPWIGGenCsharp.WriteCalleeUnicodeStringManagement2(AClass: TPWIGClass;
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
        Writeln(F, Indent, 'Length__', ParamName, ' = System.Text.Encoding.UTF8.GetByteCount(String__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ');')
      else
        Writeln(F, Indent, 'Tmp__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ' = ', ParamName, ';');
    end;
  WriteCurlyEnd;
  Writeln(F, Indent, 'else');
  WriteCurlyBegin;
  for Param in AMethod.Params do
    if ParamDirection(AMethod, Param, AGetter) <> pdIn then
    begin
      ParamName := ReplaceNotAllowedParamName(Param.Name);
      if Param.ParamType.BaseType = btUnicodeString then
        Writeln(F, Indent, ParamName, ' = LibInvoke.NativeUtf8FromString(String__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ');')
      else
        Writeln(F, Indent, ParamName, ' = Tmp__', AClass.Name, AIntf.Name, AMethod.Name, ParamName, ';');
    end;
  WriteCurlyEnd;
  Result := True;
end;

procedure TPWIGGenCSharp.WriteCallerConstructor(AClass: TPWIGClass;
  AIntf: TPWIGInterface);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  IntfName: string;
begin
  Writeln(F, Indent, 'public ', AClass.Name, '(', AIntf.Name, ' AInterfaceHandle = 0)');
  WriteCurlyBegin;
  Writeln(F, Indent, 'try');
  WriteCurlyBegin;
  Writeln(F, Indent, 'FItemHandle = AInterfaceHandle;');
  Writeln(F, Indent, 'if (FItemHandle == 0)');
  WriteCurlyBegin;
  Writeln(F, Indent, 'if(!');
  IncIndent;
  Writeln(F, Indent, 'LibInvoke.', AClass.Name, 'Create(out FItemHandle)');
  DecIndent;
  Writeln(F, Indent, ') throw new System.Exception("', AClass.name, 'Create");');
  WriteCurlyEnd;
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
        Writeln(F, Indent, IntfName, Method.Name, ' = null;');
        Writeln(F, Indent, 'if (!');
        IncIndent;
        Writeln(F, Indent, 'LibInvoke.Set', AClass.name, LIntf.Name, Method.Name, '(FItemHandle, ((System.IntPtr)System.Runtime.InteropServices.GCHandle.Alloc(this)).ToInt64(), F', AClass.Name, LIntf.Name, Method.Name, ')');
        DecIndent;
        Writeln(F, Indent, ') throw new System.Exception("Set', AClass.name, LIntf.Name, Method.Name, '");');
      end;
    end;
  end;
  WriteCurlyEnd;
  Writeln(F, Indent, 'catch (System.Exception e)');
  WriteCurlyBegin;
  Writeln(F, Indent, 'LibInvoke.LibCallError(e.Message);');
  WriteCurlyEnd;
  WriteCurlyEnd;
  WriteSpace;
end;

procedure TPWIGGenCSharp.WriteCallerDestructor(AClass: TPWIGClass;
  AIntf: TPWIGInterface);
begin
  Writeln(F, Indent, '~', AClass.Name, '()');
  WriteCurlyBegin;
  Writeln(F, Indent, 'Dispose();');
  WriteCurlyEnd;
  WriteSpace;
  Writeln(F, Indent, 'public virtual void Dispose()');
  WriteCurlyBegin;
  Writeln(F, Indent, 'try');
  WriteCurlyBegin;
  Writeln(F, Indent, 'if (FItemHandle != 0)');
  WriteCurlyBegin;
  Writeln(F, Indent, 'if(!');
  IncIndent;
  Writeln(F, Indent, 'LibInvoke.', AClass.Name, 'Destroy(FItemHandle)');
  DecIndent;
  Writeln(F, Indent, ') throw new System.Exception("', AClass.name, 'Destroy");');
  Writeln(F, Indent, 'else');
  IncIndent;
  Writeln(F, Indent, 'FItemHandle = 0;');
  DecIndent;
  WriteCurlyEnd;
  WriteCurlyEnd;
  Writeln(F, Indent, 'catch (System.Exception e)');
  WriteCurlyBegin;
  Writeln(F, Indent, 'LibInvoke.LibCallError(e.Message);');
  WriteCurlyEnd;
  WriteCurlyEnd;
  WriteSpace;
end;

procedure TPWIGGenCSharp.WriteCallerMethod(AClass: TPWIGClass;
  AIntf: TPWIGInterface; AMethod: TPWIGMethod;
  ADefaultInterface, AGetter: Boolean; const APrefix, AVisibility: string);
var
  FuncName, ParamName: string;
  UnicodeStringsExist: Boolean;
begin
  FuncName := AClass.Name + APrefix + AIntf.Name + AMethod.Name;
  WriteMethod(AClass.Name, AIntf, AMethod, msClassImplementation, ADefaultInterface, AGetter, APrefix, AVisibility);
  // write method body
  WriteCurlyBegin;
  WriteCallerOutputParamInits(AMethod, AGetter);

  Writeln(F, Indent, 'try');
  WriteCurlyBegin;

  UnicodeStringsExist := WriteCallerUnicodeStringDeclarations(AMethod, AGetter);
  if UnicodeStringsExist then
  begin
    Writeln(F, Indent, 'if(!');
    IncIndent;
    WriteMethod(AClass.Name, AIntf, AMethod, msLibCallGetLength, ADefaultInterface, AGetter, APrefix, AVisibility);
    DecIndent;
    Writeln(F, Indent, ') throw new System.Exception("', FuncName, '");');
    WriteCallerUnicodeStringManagement1(AMethod, AGetter);
  end;
  Writeln(F, Indent, 'if(!');
  IncIndent;
  WriteMethod(AClass.Name, AIntf, AMethod, msLibCall, ADefaultInterface, AGetter, APrefix, AVisibility);
  DecIndent;
  Writeln(F, Indent, ') throw new System.Exception("', FuncName, '");');

  if UnicodeStringsExist then
    WriteCallerUnicodeStringManagement2(AMethod, AGetter);

  WriteCurlyEnd;
  Writeln(F, Indent, 'catch (System.Exception e)');
  WriteCurlyBegin;
  Writeln(F, Indent, 'LibInvoke.LibCallError(e.Message);');
  WriteCurlyEnd;

  ParamName := ReturnParamName(AMethod, AGetter);
  if ParamName <> '' then
    Writeln(F, Indent, 'return ', ParamName,';');
  WriteCurlyEnd;
  WriteSpace;
end;

procedure TPWIGGenCSharp.WriteCallerEventCallback(AClass: TPWIGClass;
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
  Writeln(F, Indent, '#if __IOS__');
  Writeln(F, Indent, '[MonoPInvokeCallback(typeof(LibInvoke.', AClass.Name, AIntf.Name, AMethod.name, '))]');
  Writeln(F, Indent, '#endif');
  WriteMethod(AClass.Name, AIntf, AMethod, msEventHandler, ADefaultInterface, False, '', 'private static');
  // write method body
  WriteCurlyBegin;
  Writeln(F, Indent, 'System.Boolean Result = false;');
  Writeln(F, Indent, 'try');
  WriteCurlyBegin;
  Writeln(F, Indent, 'System.Object cls = ((System.Runtime.InteropServices.GCHandle)(System.IntPtr)ItemHandle).Target;');
  Writeln(F, Indent, 'if ((cls is ', AClass.Name, ') && (((', AClass.Name, ')cls).', IntfName, AMethod.Name, ' != null))');
  WriteCurlyBegin;
  if UnicodeStringsExist then
    WriteCalleeUnicodeStringManagement1(AClass, AIntf, AMethod, False);
  WriteMethod(AClass.Name, AIntf, AMethod, msClassCall, ADefaultInterface, False, '((' + AClass.Name + ')cls).', '');
  if UnicodeStringsExist then
    WriteCalleeUnicodeStringManagement2(AClass, AIntf, AMethod, False);
  WriteCurlyEnd;
  Writeln(F, Indent, 'Result = true;');
  WriteCurlyEnd;
  Writeln(F, Indent, 'catch (System.Exception e)');
  WriteCurlyBegin;
  Writeln(F, Indent, 'LibInvoke.LibCallError(e.Message);');
  WriteCurlyEnd;
  Writeln(F, Indent, 'return Result;');
  WriteCurlyEnd;
  WriteSpace;
end;

procedure TPWIGGenCSharp.WriteCallerLibLoads(AClass: TPWIGClass);

  procedure WriteLibLoadConstructor(AIntf: TPWIGInterface);
  begin
    Writeln(F, Indent, '[System.Runtime.InteropServices.DllImport(cLibName, EntryPoint="', AClass.Name, 'Create", CallingConvention=', CallingConvToString(nil),')]');
    Writeln(F, Indent, 'public static extern System.Boolean ', AClass.Name, 'Create(out ', AIntf.Name, ' ItemHandle);');
    WriteSpace;
  end;

  procedure WriteLibLoadDestructor(AIntf: TPWIGInterface);
  begin
    Writeln(F, Indent, '[System.Runtime.InteropServices.DllImport(cLibName, EntryPoint="', AClass.Name, 'Destroy", CallingConvention=', CallingConvToString(nil),')]');
    Writeln(F, Indent, 'public static extern System.Boolean ', AClass.Name, 'Destroy(', AIntf.Name, ' ItemHandle);');
    WriteSpace;
  end;

  procedure WriteLibLoad(AIntf: TPWIGInterface; AMethod: TPWIGMethod; AGetter: Boolean; const APrefix: string);
  begin
    Writeln(F, Indent, '[System.Runtime.InteropServices.DllImport(cLibName, EntryPoint="', AClass.Name, APrefix, AIntf.Name, AMethod.Name, '", CallingConvention=', CallingConvToString(AMethod),')]');
    WriteMethod(AClass.Name, AIntf, AMethod, msLibExport, False, AGetter, APrefix, 'public static extern');
    WriteSpace;
  end;

  procedure WriteLibLoadEventSetter(AIntf, AIntfDef: TPWIGInterface; AMethod: TPWIGMethod);
  begin
    Writeln(F, Indent, '[System.Runtime.InteropServices.UnmanagedFunctionPointerAttribute(', CallingConvToString(AMethod),')]');
    WriteMethod(AClass.Name, AIntf, AMethod, msLibExport, False, False, '', 'public delegate');
    WriteSpace;
    Writeln(F, Indent, '[System.Runtime.InteropServices.DllImport(cLibName, EntryPoint="Set', AClass.Name, AIntf.Name, AMethod.Name, '", CallingConvention=', CallingConvToString(AMethod),')]');
    Writeln(F, Indent, 'public static extern System.Boolean Set', AClass.Name, AIntf.Name, AMethod.Name, '(', AIntfDef.Name, ' ItemHandle, ', AIntf.Name,  ' EventSink, ', AClass.Name, AIntf.Name, AMethod.Name, ' EventHandler);');
    WriteSpace;
  end;

var
  Ref: TPWIGInterfaceRef;
  LIntf, LIntfDef: TPWIGInterface;
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
        WriteLibLoadConstructor(LIntf);
        Writeln(F, Indent, '// Destructor:');
        WriteLibLoadDestructor(LIntf);
      end;
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        WriteLibLoad(LIntf, Method, False, '');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteLibLoad(LIntf, Prop, True, 'Get');
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteLibLoad(LIntf, Prop, False, 'Set');
        end;
      end;
    end;
  end;
  // write event handler setters
  LIntfDef := FPWIG.FindDefaultIntf(AClass, False);
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and LIntf.FlagDispEvents then
    begin
      Writeln(F, Indent, '// Event handler setters:');
      for Method in LIntf.Methods do
      begin
        WriteLibLoadEventSetter(LIntf, LIntfDef, Method);
      end;
    end;
  end;
end;

procedure TPWIGGenCSharp.WriteCallerImplementations(AClass: TPWIGClass);
var
  Ref: TPWIGInterfaceRef;
  LIntf: TPWIGInterface;
  Method: TPWIGMethod;
  Prop: TPWIGProperty;
  IntfName, Visibility: string;
begin
  WriteIntfElementProps(AClass);

  Writeln(F, Indent, 'public class ', AClass.Name, ': System.IDisposable');
  WriteCurlyBegin;
  WriteSpace;

  // write event handler declarations
  Writeln(F, Indent, '// Event handlers:');
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
        WriteMethod(AClass.Name, LIntf, Method, msEventDeclaration, Ref.FlagDefault, False, '', 'public');
        Writeln(F, Indent, 'public event ', LIntf.Name, Method.Name, ' ', IntfName, Method.Name, ';');
        WriteSpace;
      end;
    end;
  end;

  // write event handler callbacks
  Writeln(F, Indent, '// Event handler callbacks:');
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and LIntf.FlagDispEvents then
    begin
      for Method in LIntf.Methods do
      begin
        // declare the new callback as private field, avoid garbage collection
        Writeln(F, Indent, 'private static LibInvoke.', AClass.Name, LIntf.Name, Method.Name, ' F', AClass.Name, LIntf.Name, Method.Name, ' = new LibInvoke.', AClass.Name, LIntf.Name, Method.Name, '(', AClass.Name, LIntf.Name, Method.Name, ');');
        WriteSpace;
        WriteCallerEventCallback(AClass, LIntf, Method, Ref.FlagDefault);
      end;
    end;
  end;

  // write class methods
  for Ref in AClass.InterfaceRefs do
  begin
    LIntf := FPWIG.Interfaces.Find(Ref.RefGUID, Ref.RefName);
    if (LIntf <> nil) and not LIntf.FlagDispEvents then
    begin
      if Ref.FlagDefault then
      begin
        Writeln(F, Indent, '// Default interface handle:');
        Writeln(F, Indent, 'private ', LIntf.Name, ' FItemHandle;');
        WriteSpace;
        Writeln(F, Indent, 'public ', LIntf.Name, ' Handle { get { return FItemHandle; }}');
        WriteSpace;
        IntfName := '';
      end else
        IntfName := LIntf.Name;
      Writeln(F, Indent, '// Constructor:');
      WriteCallerConstructor(AClass, LIntf);
      Writeln(F, Indent, '// Destructor:');
      WriteCallerDestructor(AClass, LIntf);
      Writeln(F, Indent, '// Methods:');
      for Method in LIntf.Methods do
      begin
        WriteCallerMethod(AClass, LIntf, Method, Ref.FlagDefault, False, '', 'public');
      end;
      Writeln(F, Indent, '// Properties:');
      for Prop in LIntf.Properties do
      begin
        if Prop.Params.Count = 1 then
          Visibility := 'private'
        else
          Visibility := 'public';
        if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
        begin
          // write getter
          WriteCallerMethod(AClass, LIntf, Prop, Ref.FlagDefault, True, 'Get', Visibility);
        end;
        if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
        begin
          // write setter
          WriteCallerMethod(AClass, LIntf, Prop, Ref.FlagDefault, False, 'Set', Visibility);
        end;
        // write property wrapper
        if Prop.Params.Count = 1 then
        begin
          // no clean way to describe indexed properties in C#!
          WriteMethod(AClass.Name, LIntf, Prop, msPropertyDeclaration, Ref.FlagDefault, True, '', 'public');
          WriteCurlyBegin;
          if Prop.PropertyType in [ptReadOnly, ptReadWrite] then
          begin
            Writeln(F, Indent, 'get');
            WriteCurlyBegin;
            Writeln(F, Indent, 'return Get', IntfName, Prop.Name, '();');
            WriteCurlyEnd;
          end;
          if Prop.PropertyType in [ptWriteOnly, ptReadWrite] then
          begin
            Writeln(F, Indent, 'set');
            WriteCurlyBegin;
            Writeln(F, Indent, 'Set', IntfName, Prop.Name, '(value);');
            WriteCurlyEnd;
          end;
          WriteCurlyEnd;
          WriteSpace;
        end;
      end;
    end;
  end;

  WriteCurlyEnd;
  WriteSpace;
end;

function TPWIGGenCSharp.WriteCallerOutputParamInits(
  AMethod: TPWIGMethod; AGetter: Boolean): Boolean;
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
        Writeln(F, Indent, ParamName, ' = ', TypeToNullValue(Param.ParamType),';')
    end;
  Result := True;
end;

function TPWIGGenCSharp.WriteCallerUnicodeStringDeclarations(
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
      Result := True;
      Writeln(F, Indent, 'System.String String__', ParamName, ' = ""; System.Int32 Length__', ParamName, ' = 0;');
    end;
end;

function TPWIGGenCSharp.WriteCallerUnicodeStringManagement1(
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
      Writeln(F, Indent, 'System.IntPtr Array__', ParamName, ' = System.Runtime.InteropServices.Marshal.AllocHGlobal(Length__', ParamName, ' + 1);');
    end;
  Result := True;
end;

function TPWIGGenCSharp.WriteCallerUnicodeStringManagement2(
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
      Writeln(F, Indent, 'String__', ParamName, ' = LibInvoke.StringFromNativeUtf8(Array__', ParamName, ', Length__', ParamName, ');');
      Writeln(F, Indent, 'System.Runtime.InteropServices.Marshal.FreeHGlobal(Array__', ParamName, ');');
      Writeln(F, Indent, 'if (Length__', ParamName, ' > 0) ', ParamName, ' = String__', ParamName, ';');
    end;
  Result := True;
end;

procedure TPWIGGenCSharp.WriteInterfaceFile(const AFileName: string);
var
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

      Writeln(F, Indent, '// Library properties:');
      WriteIntfElementProps(FPWIG);
      WriteSpace;

      // begin to write library
      Writeln(F, Indent, 'namespace ', FPWIG.Name);
      WriteCurlyBegin;
      WriteSpace;

      // write enums
      Writeln(F, Indent, '// Enumerated types:');
      WriteSpace;
      for LEnum in FPWIG.Enums do
        WriteIntfEnumProps(LEnum);

      Writeln(F, Indent, 'public static class Global');
      WriteCurlyBegin;

      // write library ID
      Writeln(F, Indent, 'public const string cLibGUID = "', FPWIG.GUID, '";');
      WriteSpace;

      WriteCurlyEnd;

      // finish
      WriteCurlyEnd;

      Writeln('C# common interface file generated: ', GeneratedFile);
    except
      Writeln('Could not generate C# common interface file: ', GeneratedFile);
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TPWIGGenCSharp.WriteCalleeFile(const AFileName: string);
var
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
      Writeln(F, Indent, 'namespace ', FPWIG.Name);
      WriteCurlyBegin;
      WriteSpace;

      Writeln(F, Indent, '// Library properties:');
      WriteIntfElementProps(FPWIG);
      WriteSpace;

      Writeln(F, Indent,  'Callee wrappers not implemented.');
      WriteSpace;

      // finish
      WriteCurlyEnd;

      Writeln('C# callee interface file generated: ', GeneratedFile);
    except
      Writeln('Could not generate C# callee interface file: ', GeneratedFile);
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TPWIGGenCSharp.WriteCallerFile(const AFileName: string);
var
  LIntf: TPWIGInterface;
  LAlias: TPWIGAlias;
  LCls: TPWIGClass;
  Name, Path, Ext, GeneratedFile, IntfName, ImplName, LibName: string;
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

      // write conditional defines
      Writeln(F, Indent, '// Note: You cannot use platform specific things in a Xamarin PCL!');
      Writeln(F, Indent, '// Following should be defined in Xamarin iOS project, but kept here for testing purposes');
      Writeln(F, Indent, '// #define __IOS__');
      WriteSpace;
      WriteSpace;

      // begin to write library
      Writeln(F, Indent, 'namespace ', FPWIG.Name);
      WriteCurlyBegin;
      WriteSpace;
      Writeln(F, Indent, '// Library properties:');
      WriteIntfElementProps(FPWIG);

      Writeln(F, Indent, '// Add platform specific namespaces');
      Writeln(F, Indent, '#if __IOS__');
      Writeln(F, Indent, 'using ObjCRuntime;');
      Writeln(F, Indent, '#endif');
      WriteSpace;

      // write interface aliases
      Writeln(F, Indent, '// Interface aliases:');
      WriteSpace;
      for LIntf in FPWIG.Interfaces do
        Writeln(F, Indent, 'using ', LIntf.Name, ' = System.Int64;');
      WriteSpace;

      // write user defined aliases
      Writeln(F, Indent, '// User defined aliases:');
      WriteSpace;
      for LAlias in FPWIG.Aliases do
        WriteIntfAliasProps(LAlias);

      // write class implementations
      Writeln(F, Indent, '// User defined classes:');
      WriteSpace;
      for LCls in FPWIG.Classes do
        WriteCallerImplementations(LCls);

      // write PInvoke helper class
      Writeln(F, Indent, '// Helper class for platform invoke calls:');
      Writeln(F, Indent, 'public class LibInvoke');
      WriteCurlyBegin;
      WriteSpace;

      // write library name
      if FPWIG.StaticLibraryName <> '' then
        LibName := FPWIG.StaticLibraryName
      else
        LibName := LowerCase(Name);
      Writeln(F, Indent, '#if __IOS__');
      Writeln(F, Indent, 'private const string cLibName = "@rpath/', LibName, '.framework/', LibName, '"; // needed to access library from local embedded framework');
      Writeln(F, Indent, '#else');
      Writeln(F, Indent, 'private const string cLibName = "', LibName, '";');
      Writeln(F, Indent, '#endif');
      WriteSpace;

      // write helpers
      Writeln(F, Indent, 'public static System.IntPtr NativeUtf8FromString(string managedString)');
      WriteCurlyBegin;
      Writeln(F, Indent, 'int len = System.Text.Encoding.UTF8.GetByteCount(managedString);');
      Writeln(F, Indent, 'byte[] buffer = new byte[len + 1];');
      Writeln(F, Indent, 'System.Text.Encoding.UTF8.GetBytes(managedString, 0, managedString.Length, buffer, 0);');
      Writeln(F, Indent, 'System.IntPtr nativeUtf8 = System.Runtime.InteropServices.Marshal.AllocHGlobal(buffer.Length);');
      Writeln(F, Indent, 'System.Runtime.InteropServices.Marshal.Copy(buffer, 0, nativeUtf8, buffer.Length);');
      Writeln(F, Indent, 'return nativeUtf8;');
      WriteCurlyEnd;
      WriteSpace;

      Writeln(F, Indent, 'public static string StringFromNativeUtf8(System.IntPtr nativeUtf8, System.Int32 length = 0)');
      WriteCurlyBegin;
      Writeln(F, Indent, 'int len = 0;');
      Writeln(F, Indent, 'if (length == 0)');
      IncIndent;
      Writeln(F, Indent, 'while (System.Runtime.InteropServices.Marshal.ReadByte(nativeUtf8, len) != 0) ++len;');
      DecIndent;
      Writeln(F, Indent, 'else');
      IncIndent;
      Writeln(F, Indent, 'len = length;');
      DecIndent;
      Writeln(F, Indent, 'byte[] buffer = new byte[len];');
      Writeln(F, Indent, 'System.Runtime.InteropServices.Marshal.Copy(nativeUtf8, buffer, 0, buffer.Length);');
      Writeln(F, Indent, 'return System.Text.Encoding.UTF8.GetString(buffer, 0, buffer.Length);');
      WriteCurlyEnd;
      WriteSpace;

      // write library error stub
      Writeln(F, Indent, 'public static void LibCallError(System.String AFuncName)');
      WriteCurlyBegin;
      Writeln(F, Indent, 'throw new System.Exception(System.String.Format("Error while calling library function {0}!", AFuncName));');
      WriteCurlyEnd;
      WriteSpace;

      Writeln(F, Indent, '[global::System.Runtime.InteropServices.DllImport(cLibName, EntryPoint="GetLibGUID", CallingConvention=' + CallingConvToString(nil) + ')]');
      Writeln(F, Indent, 'public static extern System.IntPtr GetLibGUID();');
      WriteSpace;

      // write library identification code checker
      Writeln(F, Indent, 'public static System.Boolean LibCheck()');
      WriteCurlyBegin;
      Writeln(F, Indent, 'System.Boolean Result = false;');
      Writeln(F, Indent, 'try');
      WriteCurlyBegin;
      Writeln(F, Indent, 'System.String LibGUID = StringFromNativeUtf8(GetLibGUID());');
      Writeln(F, Indent, 'if (LibGUID != Global.cLibGUID)');
      IncIndent;
      Writeln(F, Indent, 'throw new System.Exception("Incompatible library interface!");');
      DecIndent;
      Writeln(F, Indent, 'Result = true;');
      WriteCurlyEnd;
      Writeln(F, Indent, 'catch (System.Exception e)');
      WriteCurlyBegin;
      Writeln(F, Indent, 'LibCallError(e.Message);');
      WriteCurlyEnd;
      Writeln(F, Indent, 'return Result;');
      WriteCurlyEnd;
      WriteSpace;

      for LCls in FPWIG.Classes do
        WriteCallerLibLoads(LCls);

      WriteCurlyEnd;

      // finish
      WriteCurlyEnd;

      Writeln('C# caller interface file generated: ', GeneratedFile);
    except
      Writeln('Could not generate C# caller interface file: ', GeneratedFile);
    end;
  finally
    CloseFile(F);
  end;
end;

end.

