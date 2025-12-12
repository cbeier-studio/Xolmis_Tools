unit frm_devtools;

{$mode ObjFPC}{$H+}

interface

uses
  BCPanel, Classes, ExtCtrls, StdCtrls, SysUtils, Forms, Controls, Graphics,
  dev_types;

type

  { TfrmDevTools }

  TfrmDevTools = class(TForm)
    pCodeGenerator: TBCPanel;
    pLogViewer: TBCPanel;
    pUsageDataViewer: TBCPanel;
    pSettingsEditor: TBCPanel;
    pDataGenerator: TBCPanel;
    pDataMigration: TBCPanel;
    pDataSchemaViewer: TBCPanel;
    btnOpenCodeGenerator: TButton;
    btnOpenLogViewer: TButton;
    btnOpenUsageDataViewer: TButton;
    btnOpenSettingsEditor: TButton;
    btnOpenDataGenerator: TButton;
    btnOpenDataMigration: TButton;
    btnOpenSchemaViewer: TButton;
    iIcons: TImageList;
    icoCodeGenerator: TImage;
    icoLogViewer: TImage;
    icoUsageDataViewer: TImage;
    icoSettingsEditor: TImage;
    icoDataGenerator: TImage;
    icoDataMigration: TImage;
    icoDataSchemaViewer: TImage;
    titleCodeGenerator: TLabel;
    lblDataGenerator: TLabel;
    titleDataMigration: TLabel;
    lblDataMigration: TLabel;
    titleDataSchemaViewer: TLabel;
    lblDataSchemaViewer: TLabel;
    lblCodeGenerator: TLabel;
    titleLogViewer: TLabel;
    lblLogViewer: TLabel;
    titleUsageDataViewer: TLabel;
    lblUsageDataViewer: TLabel;
    titleSettingsEditor: TLabel;
    lblSettingsEditor: TLabel;
    titleDataGenerator: TLabel;
    procedure btnOpenCodeGeneratorClick(Sender: TObject);
    procedure btnOpenDataMigrationClick(Sender: TObject);
    procedure btnOpenLogViewerClick(Sender: TObject);
    procedure btnOpenSettingsEditorClick(Sender: TObject);
  private

  public

  end;

var
  frmDevTools: TfrmDevTools;

implementation

uses
  frm_classgenerator, frm_logviewer, frm_settingseditor, frm_docsgenerator;

{$R *.lfm}

{ TfrmDevTools }

procedure TfrmDevTools.btnOpenCodeGeneratorClick(Sender: TObject);
begin
  frmClassGenerator := TfrmClassGenerator.Create(nil);
  with frmClassGenerator do
  try
    ShowModal;
  finally
    FreeAndNil(frmClassGenerator);
  end;
end;

procedure TfrmDevTools.btnOpenDataMigrationClick(Sender: TObject);
begin
  frmDocsGenerator := TfrmDocsGenerator.Create(nil);
  with frmDocsGenerator do
  try
    ShowModal;
  finally
    FreeAndNil(frmDocsGenerator);
  end;
end;

procedure TfrmDevTools.btnOpenLogViewerClick(Sender: TObject);
begin
  frmLogViewer := TfrmLogViewer.Create(nil);
  with frmLogViewer do
  try
    ShowModal;
  finally
    FreeAndNil(frmLogViewer);
  end;
end;

procedure TfrmDevTools.btnOpenSettingsEditorClick(Sender: TObject);
begin
  frmSettingsEditor := TfrmSettingsEditor.Create(nil);
  with frmSettingsEditor do
  try
    ShowModal;
  finally
    FreeAndNil(frmSettingsEditor);
  end;
end;

end.

