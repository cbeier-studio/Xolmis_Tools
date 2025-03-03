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
    pSQLQuery: TBCPanel;
    btnOpenCodeGenerator: TButton;
    btnOpenLogViewer: TButton;
    btnOpenUsageDataViewer: TButton;
    btnOpenSettingsEditor: TButton;
    btnOpenDataGenerator: TButton;
    btnOpenDataMigration: TButton;
    btnOpenSchemaViewer: TButton;
    btnOpenSQLQuery: TButton;
    iIcons: TImageList;
    icoCodeGenerator: TImage;
    icoLogViewer: TImage;
    icoUsageDataViewer: TImage;
    icoSettingsEditor: TImage;
    icoDataGenerator: TImage;
    icoDataMigration: TImage;
    icoDataSchemaViewer: TImage;
    icoSQLQuery: TImage;
    titleCodeGenerator: TLabel;
    lblDataGenerator: TLabel;
    titleDataMigration: TLabel;
    lblDataMigration: TLabel;
    titleDataSchemaViewer: TLabel;
    lblDataSchemaViewer: TLabel;
    titleSQLQuery: TLabel;
    lblSQLQuery: TLabel;
    lblCodeGenerator: TLabel;
    titleLogViewer: TLabel;
    lblLogViewer: TLabel;
    titleUsageDataViewer: TLabel;
    lblUsageDataViewer: TLabel;
    titleSettingsEditor: TLabel;
    lblSettingsEditor: TLabel;
    titleDataGenerator: TLabel;
  private

  public

  end;

var
  frmDevTools: TfrmDevTools;

implementation

{$R *.lfm}

end.

