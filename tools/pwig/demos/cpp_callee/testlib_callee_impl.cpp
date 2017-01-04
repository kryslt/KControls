// ************************************************************************
// This file implements the callee wrapper classes.

#include <string>
#include "stdafx.h"
#include "stdint.h"
#include "testlib_intf.h"
#include "testlib_callee.h"
#include "testlib_callee_impl.h"

// Name: ProjectGroup
// GUID: 7C12BB43-A6AB-4A52-8B1D-EDD5D94B344B
// Description: ProjectGroup Object

// Constructor:
ProjectGroup::ProjectGroup()
{
  m_Events = new AuxIProjectGroupEvents();
}

// Destructor:
ProjectGroup::~ProjectGroup()
{
  delete m_Events;
}

// Methods:
IProject ProjectGroup::AddProject()
{
  Project * Prj = new Project();
  Prj->m_ProjectGroup = this;
  return (IProject)Prj;
}

void ProjectGroup::RunPeriodic()
{
  // nothing
}

void ProjectGroup::Finalize()
{
  // nothing
}

// Properties:

// Setup event handlers:
void ProjectGroup::SetupOnError(IProjectGroupEvents EventSink, TIProjectGroupEventsOnError EventHandler)
{
  m_Events->SetupOnError(EventSink, EventHandler);
}

void ProjectGroup::SetupOnProgress(IProjectGroupEvents EventSink, TIProjectGroupEventsOnProgress EventHandler)
{
  m_Events->SetupOnProgress(EventSink, EventHandler);
}

void ProjectGroup::Error(TErrorCode ErrorCode, std::wstring ErrorText)
{
  m_Events->OnError(ErrorCode, ErrorText);
}


// Name: Project
// GUID: D96EA22B-D750-4C05-9F32-8C5C8E9F846D
// Description: Project Object

// Constructor:
Project::Project()
{
  // nothing
}

// Destructor:
Project::~Project()
{
  // nothing
}

// Methods:
void Project::Connect()
{
  m_ProjectGroup->Error(errError, L"Cannot connect, not implemented!");
}

void Project::Disconnect()
{
  m_ProjectGroup->Error(errError, L"Cannot disconnect, not implemented!");
}

TBool Project::LoadFromFile(std::wstring Path)
{
  m_ProjectGroup->Error(errError, L"Cannot load from file, not implemented!");
  return bFalse;
}

TBool Project::SaveToFile(std::wstring Path)
{
  m_ProjectGroup->Error(errError, L"Cannot save to file, not implemented!");
  return bFalse;
}

// Properties:
int32_t Project::GetConnectionFRC()
{
  return 0; // not implemented
}

std::wstring Project::GetConnectionString()
{
  return L"No connection, not implemented in library!";
}
