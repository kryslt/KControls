unit kcontrolsdesign;

{$include ..\..\source\kcontrols.inc}

interface

uses
{$IFDEF FPC}
  ComponentEditors, PropEdits
{$ELSE}
  DesignIntf, DesignEditors
{$ENDIF}
  ;

type
  TKOpenDialogEditor = class(TClassProperty)
    function GetAttributes: TPropertyAttributes; override;
  end;

  TKFileNameEditor = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TKFolderEditor = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

{$IFNDEF FPC}
  {$R *.dcr}
{$ENDIF}

uses
  Classes, Dialogs, SysUtils, Forms, 
  KControls, KDialogs, KGrids, KHexEditor,
  KEdits, KLabels, KLog, KProgress, KRes
{$IFDEF TKDBGRID_USE}
  , KDBGrids
{$ENDIF}
{$IFDEF FPC}
  , LResources
{$ENDIF}
  ;

procedure Register;
begin
  RegisterComponents('TK', [
    TKGrid, 
{$IFDEF TKDBGRID_USE}
    TKDBGrid,
{$ENDIF}
    TKHexEditor, 
    TKNumberEdit,
    TKFileNameEdit,
    TKLog,
    TKPercentProgressBar,
    TKLinkLabel,
    TKGradientLabel,
    TKPrintPreview,
    TKPrintSetupDialog,
    TKPrintPreviewDialog,
    TKBrowseFolderDialog
  ]);

  // editors
  RegisterPropertyEditor(TypeInfo(TKFileNameEditDlgProperties), nil, '', TKOpenDialogEditor);
  RegisterPropertyEditor(TypeInfo(TFileName), nil, 'FileName', TKFileNameEditor);
  RegisterPropertyEditor(TypeInfo(TFolder), nil, '', TKFolderEditor);
end;

{ TKOpenDialogEditor }

function TKOpenDialogEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly];
end;

{ TFileNameEditor }

function TKFileNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paRevertable, paDialog];
end;

procedure TKFileNameEditor.Edit;

  procedure DoOpenDlg;
  var
    OD: TOpenDialog;
  begin
    OD := TOpenDialog.Create(Application);
    try
      OD.Filter := sEDAllFiles;
      OD.FileName := GetValue;
      if OD.Execute then SetValue(OD.FileName);
    finally
      OD.Free;
    end;
  end;

var
  BF: TKBrowseFolderDialog;
  P: TPersistent;
begin
  inherited;
  P := GetComponent(0);
  if P is TKFileNameEdit then with TKFileNameEdit(P) do
  begin
    if foFolderOnly in Options then
    begin
      BF := TKBrowseFolderDialog.Create(Application);
      try
        BF.Position := poScreenCenter;
        BF.Folder := GetValue;
        if BF.Execute then SetValue(BF.Folder);
      finally
        BF.Free;
      end;
    end else
      DoOpenDlg;
  end else
    DoOpenDlg;
end;

{ TKFolderEditor }

function TKFolderEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paRevertable, paDialog, paMultiSelect];
end;

procedure TKFolderEditor.Edit;
var
  BF: TKBrowseFolderDialog;
begin
  inherited;
  BF := TKBrowseFolderDialog.Create(Application);
  try
    BF.Position := poScreenCenter;
    if BF.Execute then
      SetValue(BF.Folder);
  finally
    BF.Free;
  end;
end;

{$IFDEF FPC}
initialization
  {$i kcontrolsdesign.lrs}
{$ENDIF}
end.
