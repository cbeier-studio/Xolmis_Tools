program XolmisDevTools;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  frm_devtools,
  dev_types, frm_classgenerator, frm_settingseditor, frm_logviewer;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'Xolmis Developer Tools';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmDevTools, frmDevTools);
  Application.Run;
end.

