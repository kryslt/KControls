
// cpp_callerDlg.cpp : implementation file
//

#include "stdafx.h"
#include "cpp_caller.h"
#include "cpp_callerDlg.h"
#include "afxdialogex.h"
#include "testlib_intf.h"
#include "testlib_caller.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

// MyProjectGroup: must override ProjectGroup because of missing events in C++!

MyProjectGroup::MyProjectGroup(Ccpp_callerDlg * dlg)
{
  m_dlg = dlg;
}

void MyProjectGroup::CallEventOnError(TErrorCode ErrorCode, std::wstring ErrorText)
{
  m_dlg->EventError(ErrorCode, ErrorText);
}

void MyProjectGroup::CallEventOnProgress(TProgressEvent EventCode, int32_t ProgressValue, std::wstring EventText)
{
  // not implemented
}


// CAboutDlg dialog used for App About

class CAboutDlg : public CDialogEx
{
public:
	CAboutDlg();

// Dialog Data
#ifdef AFX_DESIGN_TIME
	enum { IDD = IDD_ABOUTBOX };
#endif

	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

// Implementation
protected:
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialogEx(IDD_ABOUTBOX)
{
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialogEx::DoDataExchange(pDX);
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialogEx)
END_MESSAGE_MAP()


// Ccpp_callerDlg dialog



Ccpp_callerDlg::Ccpp_callerDlg(CWnd* pParent /*=NULL*/)
	: CDialogEx(IDD_CPP_CALLER_DIALOG, pParent)
{
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void Ccpp_callerDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialogEx::DoDataExchange(pDX);
  DDX_Control(pDX, IDC_STATIC, Label);
}

BEGIN_MESSAGE_MAP(Ccpp_callerDlg, CDialogEx)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
  ON_WM_CLOSE()
END_MESSAGE_MAP()


// Ccpp_callerDlg message handlers

BOOL Ccpp_callerDlg::OnInitDialog()
{
	CDialogEx::OnInitDialog();

	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);

	CMenu* pSysMenu = GetSystemMenu(FALSE);
	if (pSysMenu != NULL)
	{
		BOOL bNameValid;
		CString strAboutMenu;
		bNameValid = strAboutMenu.LoadString(IDS_ABOUTBOX);
		ASSERT(bNameValid);
		if (!strAboutMenu.IsEmpty())
		{
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
		}
	}

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon

	// TODO: Add extra initialization here

  if (TestLibLibLoad(L"cpp_callee.dll"))
  {
    // create new project group
    m_ProjectGroup = new MyProjectGroup(this);

    // set events - unfortunately C++ does not natively support events, so use overridden class MyProjectGroup instead
    //m_ProjectGroup->OnError = &EventError;

    // add new project to the project group
    m_Project = new Project(m_ProjectGroup->AddProject());

    // call functions
    m_Project->Connect();
    m_Project->Disconnect();

    // work with properties
    Label.SetWindowText(m_Project->ConnectionString.c_str());
  }
  else
    MessageBox(L"Shared library not loaded!", L"Error", MB_ICONSTOP);

	return TRUE;  // return TRUE  unless you set the focus to a control
}

void Ccpp_callerDlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == IDM_ABOUTBOX)
	{
		CAboutDlg dlgAbout;
		dlgAbout.DoModal();
	}
	else
	{
		CDialogEx::OnSysCommand(nID, lParam);
	}
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void Ccpp_callerDlg::OnPaint()
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, reinterpret_cast<WPARAM>(dc.GetSafeHdc()), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialogEx::OnPaint();
	}
}

// The system calls this function to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR Ccpp_callerDlg::OnQueryDragIcon()
{
	return static_cast<HCURSOR>(m_hIcon);
}

void Ccpp_callerDlg::OnClose()
{
  // free library resources in required order
  delete m_Project;
  delete m_ProjectGroup;

  CDialogEx::OnClose();
}

void Ccpp_callerDlg::EventError(TErrorCode  ErrorCode, std::wstring  ErrorText)
{
  Label.SetWindowText(ErrorText.c_str());
}
