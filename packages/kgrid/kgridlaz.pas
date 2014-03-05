{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit kgridlaz; 

interface

uses
    kgriddesign, KControls, KDBGrids, KFunctions, KGraphics, KGrids, 
  KPrintPreview, KPrintSetup, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('kgriddesign', @kgriddesign.Register); 
end; 

initialization
  RegisterPackage('KGridLaz', @Register); 
end.
