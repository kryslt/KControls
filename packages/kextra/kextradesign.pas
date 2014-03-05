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
  KButtons, KMisc, KVentilator
{$IFDEF FPC}
  , LResources
{$ENDIF}
  ;

procedure Register;
begin
  RegisterComponents('TK addons', [
    TKColorButton,
    TKPaintPanel, 
    TKHintWindowWrapper,
    TKSplitter,
    TKBevel,
    TKVentilator
  ]);
end;

//{$IFDEF FPC}
//initialization
//  {$i kextradesign.lrs}
//{$ENDIF}
end.
