{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit KControlsLaz;

interface

uses
  kfunctions, kgraphics, kcontrols, kdialogs, keditcommon, kgrids, khexeditor, 
  kicon, kprintpreview, kprintsetup, kwidewinprocs, kcontrolsdesign, KDBGrids, 
  kedits, kmessagebox, klog, kprogress, klabels, kmemo, kbuttons, 
  kmemodlghyperlink, kmemodlgparastyle, kmemodlgtextstyle, kmemofrm, KMemoRTF, 
  kpagecontrol, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('kcontrolsdesign', @kcontrolsdesign.Register);
end;

initialization
  RegisterPackage('KControlsLaz', @Register);
end.
