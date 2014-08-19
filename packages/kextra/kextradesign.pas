unit kextradesign;

{$include ..\..\source\kcontrols.inc}

interface

uses
{$IFDEF FPC}
  ComponentEditors, PropEdits
{$ELSE}
  DesignIntf, DesignEditors
{$ENDIF}
  ;

procedure Register;

implementation

//{$IFNDEF FPC}
//  {$R *.dcr}
//{$ENDIF}

uses
  Classes, Dialogs, SysUtils, Forms, 
  KButtons, KMisc, KVentilator, KChart
{$IFDEF FPC}
  , LResources
{$ENDIF}
  ;

procedure Register;
begin
  RegisterComponents('TK addons', [
    TKPaintPanel, 
    TKHintWindowWrapper,
    TKBevel,
    TKVentilator,
    TKChart
  ]);
end;

//{$IFDEF FPC}
//initialization
//  {$i kextradesign.lrs}
//{$ENDIF}
end.
