unit KDesign;

{$include KControls.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, Forms, DesignIntf,
  DesignEditors, TypInfo, Dialogs;

type
  TOpenDialogEditor = class(TClassProperty)
    function GetAttributes: TPropertyAttributes; override;
  end;

  TKFileNameEditor = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TFolderEditor = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

{$R *.dcr}

uses
  KBrowseForFolder, KButtons, KEdits, KFunctions, KLabels, KMisc, KRes,
  KVentilator;

procedure Register;
begin
  // KBrowseForFolder
  RegisterComponents('TK', [TKBrowseFolderDialog]);
  // KButtons
  RegisterComponents('TK', [TKSpeedButton]);
  RegisterComponents('TK', [TKColorButton]);
  // KEdits
  RegisterComponents('TK', [TKNumberEdit]);
  RegisterComponents('TK', [TKFileNameEdit]);
  // KLabels
  RegisterComponents('TK', [TKGradientLabel]);
  RegisterComponents('TK', [TKWebLabel]);
  // KMisc
  RegisterComponents('TK', [TKPercentProgressBar]);
  RegisterComponents('TK', [TKPaintPanel]);
  RegisterComponents('TK', [TKHintWindowWrapper]);
  RegisterComponents('TK', [TKSplitter]);
  RegisterComponents('TK', [TKBevel]);
  // KVentilator
  RegisterComponents('TK Tech', [TKVentilator]);

  // editors
  RegisterPropertyEditor(TypeInfo(TKFileNameEditDlgProperties), nil,
   '', TClassProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TKFileNameEdit,
   'FileName', TKFileNameEditor);
  RegisterPropertyEditor(TypeInfo(TFolder), nil,
   '', TFolderEditor);
end;

{ TODPropertyEditor }

function TOpenDialogEditor.GetAttributes: TPropertyAttributes;
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
      OD.Filter := sAllFiles;
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

{ TFolderEditor }

function TFolderEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paRevertable, paDialog, paMultiSelect];
end;

procedure TFolderEditor.Edit;
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

end.
