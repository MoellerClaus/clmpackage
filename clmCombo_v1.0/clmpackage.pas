{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit clmpackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  clmCombobox, clmDBLabeledEdit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('clmCombobox', @clmCombobox.Register);
  RegisterUnit('clmDBLabeledEdit', @clmDBLabeledEdit.Register);
end;

initialization
  RegisterPackage('clmpackage', @Register);
end.
