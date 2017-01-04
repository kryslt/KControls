
// cpp_caller.h : main header file for the PROJECT_NAME application
//

#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "resource.h"		// main symbols


// Ccpp_callerApp:
// See cpp_caller.cpp for the implementation of this class
//

class Ccpp_callerApp : public CWinApp
{
public:
	Ccpp_callerApp();

// Overrides
public:
	virtual BOOL InitInstance();

// Implementation

	DECLARE_MESSAGE_MAP()
};

extern Ccpp_callerApp theApp;