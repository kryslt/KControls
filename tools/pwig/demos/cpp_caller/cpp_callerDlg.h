
// cpp_callerDlg.h : header file
//

#pragma once

#include "testlib_intf.h"
#include "testlib_caller.h"

class Ccpp_callerDlg;

// MyProjectGroup: must override ProjectGroup because of missing events in C++!
class MyProjectGroup : public ProjectGroup
{
private:
  Ccpp_callerDlg * m_dlg;
public:
  MyProjectGroup(Ccpp_callerDlg * dlg);
  virtual void CallEventOnError(TErrorCode ErrorCode, std::wstring ErrorText);
  virtual void CallEventOnProgress(TProgressEvent EventCode, int32_t ProgressValue, std::wstring EventText);
};

// Ccpp_callerDlg dialog
class Ccpp_callerDlg : public CDialogEx
{
// Construction
public:
	Ccpp_callerDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
#ifdef AFX_DESIGN_TIME
	enum { IDD = IDD_CPP_CALLER_DIALOG };
#endif

	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support


// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
  afx_msg void OnClose();
	DECLARE_MESSAGE_MAP()

public:
  MyProjectGroup * m_ProjectGroup;
  Project * m_Project;
  CEdit Label;
  void EventError(TErrorCode  ErrorCode, std::wstring  ErrorText);
};
