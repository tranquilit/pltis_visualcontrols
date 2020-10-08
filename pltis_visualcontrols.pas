{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pltis_visualcontrols;

{$warn 5023 off : no warning about unused units}
interface

uses
  WaptSearchEdit, WaptToggleButton, wapttagedit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('WaptSearchEdit', @WaptSearchEdit.Register);
  RegisterUnit('WaptToggleButton', @WaptToggleButton.Register);
  RegisterUnit('wapttagedit', @wapttagedit.Register);
end;

initialization
  RegisterPackage('pltis_visualcontrols', @Register);
end.
