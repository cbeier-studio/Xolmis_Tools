program taxaeditor_test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, FrameViewer09, GuiTestRunner, test_birdnames;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

