unit khexeditordesign;

{$include ..\..\source\kcontrols.inc}

interface

procedure Register;

implementation

{$IFNDEF FPC}
  {$R *.dcr}
{$ENDIF}

uses
  Classes, KControls, KDialogs, KHexEditor
{$IFDEF FPC}
  , LResources
{$ENDIF}
  ;

procedure Register;
begin
  RegisterComponents('TK', [
    TKHexEditor,
    TKPrintPreview,
    TKPrintSetupDialog,
    TKPrintPreviewDialog
  ]);
end;

{$IFDEF FPC}
initialization
  {$i khexeditordesign.lrs}
{$ENDIF}
end.
