{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit KHexEditorLaz; 

interface

uses
    KControls, khexeditordesign, KDialogs, KFunctions, KGraphics, KHexEditor, 
  KPrintPreview, KPrintSetup, KWideWinProcs, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('khexeditordesign', @khexeditordesign.Register); 
end; 

initialization
  RegisterPackage('KHexEditorLaz', @Register); 
end.
