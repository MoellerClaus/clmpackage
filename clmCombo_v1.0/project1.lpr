program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, DemoclmCombo;

{$R *.res}

begin
  // RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TComboDemoForm, ComboDemoForm);
  Application.Run;
end.
