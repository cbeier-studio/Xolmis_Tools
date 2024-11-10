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
  lazcontrols,
  dm_dev,
  dev_types;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='Xolmis Developer Tools';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDMD, DMD);
  Application.CreateForm(TfrmDevTools, frmDevTools);
  Application.Run;
end.

