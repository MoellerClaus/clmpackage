{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit clmpackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  clmCombobox, clmDBDateEdit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('clmCombobox', @clmCombobox.Register);
end;

initialization
  RegisterPackage('clmpackage', @Register);
end.
