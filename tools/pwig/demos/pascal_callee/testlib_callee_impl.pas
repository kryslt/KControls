unit testlib_callee_impl;

{$mode delphi}

interface

uses
  Classes, SysUtils, testlib_callee, testlib_intf;

type

  // Forward declarations:
  TProjectGroup = class;
  TProject = class;

  // Name: ProjectGroup
  // GUID: 7C12BB43-A6AB-4A52-8B1D-EDD5D94B344B
  // Description: ProjectGroup Object

  { TProjectGroup }

  TProjectGroup = class(TObject)
  private
    FEvents: TIProjectGroupEvents;
  protected
    procedure Error(ErrorCode: TErrorCode; const ErrorText: string);
  public
    constructor Create;
    destructor Destroy; override;
    // Methods:
    function AddProject: IProject;
    procedure RunPeriodic;
    procedure Finalize;
    // Properties:
    // Setup event handlers:
    procedure SetupOnError(const EventSink: IProjectGroupEvents; const EventHandler: TIProjectGroupEventsOnError);
    procedure SetupOnProgress(const EventSink: IProjectGroupEvents; const EventHandler: TIProjectGroupEventsOnProgress);
  end;

  // Name: Project
  // GUID: D96EA22B-D750-4C05-9F32-8C5C8E9F846D
  // Description: Project Object

  TProject = class(TObject)
  private
    FProjectGroup: TProjectGroup;
  protected
    property ProjectGroup: TProjectGroup read FProjectGroup write FProjectGroup;
  public
    constructor Create;
    destructor Destroy; override;
    // Methods:
    procedure Connect;
    procedure Disconnect;
    function LoadFromFile(const Path: AnsiString): TBool;
    function SaveToFile(const Path: AnsiString): TBool;
    // Properties:
    function GetConnectionFRC: LongInt;
    function GetConnectionString: AnsiString;
  end;

implementation


{ TProjectGroup }

constructor TProjectGroup.Create;
begin
  FEvents := TIProjectGroupEvents.Create;
end;

destructor TProjectGroup.Destroy;
begin
  FEvents.Free;
  inherited Destroy;
end;

function TProjectGroup.AddProject: IProject;
var
  Prj: TProject;
begin
  Prj := TProject.Create;
  Prj.ProjectGroup := Self;
  Result := IProject(Prj);
end;

procedure TProjectGroup.RunPeriodic;
begin
  // nothing
end;

procedure TProjectGroup.Finalize;
begin
  // nothing
end;

procedure TProjectGroup.SetupOnError(const EventSink: IProjectGroupEvents;
  const EventHandler: TIProjectGroupEventsOnError);
begin
  FEvents.SetupOnError(EventSink, EventHandler);
end;

procedure TProjectGroup.SetupOnProgress(const EventSink: IProjectGroupEvents;
  const EventHandler: TIProjectGroupEventsOnProgress);
begin
  FEvents.SetupOnProgress(EventSink, EventHandler);
end;

procedure TProjectGroup.Error(ErrorCode: TErrorCode; const ErrorText: string);
begin
  FEvents.OnError(ErrorCode, ErrorText);
end;

{ TProject }

constructor TProject.Create;
begin

end;

destructor TProject.Destroy;
begin
  inherited Destroy;
end;

procedure TProject.Connect;
begin
  ProjectGroup.Error(errError, 'Cannot connect, not implemented!');
end;

procedure TProject.Disconnect;
begin
  ProjectGroup.Error(errError, 'Cannot disconnect, not implemented!');
end;

function TProject.LoadFromFile(const Path: AnsiString): TBool;
begin
  ProjectGroup.Error(errError, 'Cannot load from file, not implemented!');
  Result := bFalse;
end;

function TProject.SaveToFile(const Path: AnsiString): TBool;
begin
  ProjectGroup.Error(errError, 'Cannot save to file, not implemented!');
  Result := bFalse;
end;

function TProject.GetConnectionFRC: LongInt;
begin
  Result := 0; // not implemented
end;

function TProject.GetConnectionString: AnsiString;
begin
  Result := 'No connection, not implemented in library!';
end;

end.

