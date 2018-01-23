unit main;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, DynLibs, testlib_intf, testlib_caller;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    LabeledEdit1: TLabeledEdit;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    // class instances
    FProjectGroup: TProjectGroup;
    FProject: TProject;
    procedure EventError(const ErrorCode: TErrorCode; const ErrorText: AnsiString);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
{$ifdef Unix}
  SharedPrefix = 'lib';
{$else}
  SharedPrefix = '';
{$endif}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  if TestLibLibLoad(ExtractFilePath(Application.Exename) + SharedPrefix + 'pascal_callee.' + SharedSuffix) then
  begin
    // create new project group
    FProjectGroup := TProjectGroup.Create;

    // set events
    FProjectGroup.OnError := EventError;

    // add new project to the project group
    FProject := TProject.Create(FProjectGroup.AddProject);

    // call functions
    FProject.Connect;
    FProject.Disconnect;

    // work with properties
    LabeledEdit1.Caption := FProject.ConnectionString;

  end else
    ShowMessage('Shared library not found!');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // free used resources (both here and in the library)
  FProject.Free;
  FProjectGroup.Free;
end;

procedure TForm1.EventError(const ErrorCode: TErrorCode;
  const ErrorText: AnsiString);
begin
  Memo1.Lines.Insert(0, ErrorText);
end;

end.
