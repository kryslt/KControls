library pascal_callee;

{$mode delphi}

uses
  Classes,
  testlib_callee;

exports
  // ProjectGroup
  ProjectGroupCreate,
  ProjectGroupDestroy,
  ProjectGroupIProjectGroupAddProject,
  ProjectGroupIProjectGroupRunPeriodic,
  ProjectGroupIProjectGroupFinalize,
  SetProjectGroupIProjectGroupEventsOnError,
  SetProjectGroupIProjectGroupEventsOnProgress
  // Project
,
  ProjectCreate,
  ProjectDestroy,
  ProjectIProjectConnect,
  ProjectIProjectDisconnect,
  ProjectIProjectLoadFromFile,
  ProjectIProjectSaveToFile,
  ProjectGetIProjectConnectionFRC,
  ProjectGetIProjectConnectionString
  ;

begin
end.

