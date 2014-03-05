unit kgriddesign;

{$include ..\..\source\kcontrols.inc}

interface

procedure Register;

implementation

{$IFNDEF FPC}
  {$R *.dcr}
{$ENDIF}

uses
  Classes, KControls, KDialogs, KGrids
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
    TKPrintPreview,
    TKPrintSetupDialog,
    TKPrintPreviewDialog
  ]);
end;

{$IFDEF FPC}
initialization
  {$i kgriddesign.lrs}
{$ENDIF}
end.
