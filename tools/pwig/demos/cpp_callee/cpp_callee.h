// cpp_callee.h : main header file for the cpp_callee DLL
//

#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "resource.h"		// main symbols


// Ccpp_calleeApp
// See cpp_callee.cpp for the implementation of this class
//

class Ccpp_calleeApp : public CWinApp
{
public:
	Ccpp_calleeApp();

// Overrides
public:
	virtual BOOL InitInstance();

	DECLARE_MESSAGE_MAP()
};
