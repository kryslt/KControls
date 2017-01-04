// ************************************************************************
// This file declares the callee wrapper classes.
// These declarations must be held in sync with the commented out section in
// testlib_callee.h manually by the library author.


#ifndef TESTLIB_CALLEE_IMPL_H
#define TESTLIB_CALLEE_IMPL_H

// Name: ProjectGroup
// GUID: 7C12BB43-A6AB-4A52-8B1D-EDD5D94B344B
// Description: ProjectGroup Object

class ProjectGroup : public BaseTestLib
{
private:
  AuxIProjectGroupEvents * m_Events;
public:
  ProjectGroup();
  ~ProjectGroup();
  // Methods:
  IProject AddProject();
  void RunPeriodic();
  void Finalize();
  // Properties:
  // Setup event handlers:
  void SetupOnError(IProjectGroupEvents EventSink, TIProjectGroupEventsOnError EventHandler);
  void SetupOnProgress(IProjectGroupEvents EventSink, TIProjectGroupEventsOnProgress EventHandler);

  // Our custom members:
  void Error(TErrorCode ErrorCode, std::wstring ErrorText);
};

// Name: Project
// GUID: D96EA22B-D750-4C05-9F32-8C5C8E9F846D
// Description: Project Object

class Project : public BaseTestLib
{
private:
public:
  Project();
  ~Project();
  // Methods:
  void Connect();
  void Disconnect();
  TBool LoadFromFile(std::wstring Path);
  TBool SaveToFile(std::wstring Path);
  // Properties:
  int32_t GetConnectionFRC();
  std::wstring GetConnectionString();

  // Our custom members:
  ProjectGroup * m_ProjectGroup;
};

#endif TESTLIB_CALLEE_IMPL_H

