unit frm_devtools;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, Buttons, EditBtn,
  DBCtrls, StdCtrls, DBGrids, Types, RegExpr, dev_types, attabs;

type

  { TfrmDevTools }

  TfrmDevTools = class(TForm)
    btnForeignNumInterno: TButton;
    btnFormatarNomes: TButton;
    btnGeneros: TButton;
    btnHierarquia: TButton;
    btnNumInterno: TButton;
    btnParar: TBitBtn;
    btnQuickCode: TButton;
    btnSepararNomes: TButton;
    cbFieldKind: TDBComboBox;
    cbFieldType: TDBComboBox;
    ckTableVisible: TDBCheckBox;
    ckTableCanFilter: TDBCheckBox;
    ckTableCanImport: TDBCheckBox;
    ckTableCanExport: TDBCheckBox;
    ckFieldVisible: TDBCheckBox;
    ckFieldCanSort: TDBCheckBox;
    ckAlphabeticKey: TDBCheckBox;
    ckNumericKey: TDBCheckBox;
    cbTableName: TDBComboBox;
    cbFieldName: TDBComboBox;
    cbLookupTable: TDBComboBox;
    cbLookupKey: TDBComboBox;
    cbLookupResultField: TDBComboBox;
    eTableDisplayName: TDBEdit;
    eFieldDisplayName: TDBEdit;
    eDarwinCoreName: TDBEdit;
    eFieldPosition: TDBEdit;
    eLookupName: TDBEdit;
    eMinValue: TDBEdit;
    eMaxValue: TDBEdit;
    eFindTable: TEditButton;
    eFindField: TEditButton;
    gridTables: TDBGrid;
    ckMarcadoTaxa: TDBCheckBox;
    gridFields: TDBGrid;
    mValuesList: TDBMemo;
    eFindReport: TEditButton;
    lblTableName: TLabel;
    lblFieldKind: TLabel;
    lblFieldType: TLabel;
    lblLookupTable: TLabel;
    lblLookupKey: TLabel;
    lblLookupResultField: TLabel;
    lblValuesList: TLabel;
    lblTableDiplayName: TLabel;
    lblFieldName: TLabel;
    lblFieldDisplayName: TLabel;
    lblDarwinCoreName: TLabel;
    lblFieldPosition: TLabel;
    lblLookupName: TLabel;
    lblMinValue: TLabel;
    lblMaxValue: TLabel;
    lblRecCount: TLabel;
    LS: TLabel;
    navMapFields: TDBNavigator;
    navReport: TDBNavigator;
    navMapTables: TDBNavigator;
    navTabs: TATTabs;
    PG: TNotebook;
    pgDataMap: TPage;
    pgReports: TPage;
    pgBatch: TPage;
    pBatchTools: TPanel;
    PBar: TProgressBar;
    pMapEdit: TPanel;
    pMapEditFields: TPanel;
    pMapFields: TPanel;
    pDBTaxa: TPanel;
    pMapEditTable: TPanel;
    pReportTools: TPanel;
    pBatchProgress: TPanel;
    pMapTables: TPanel;
    pMapEditTools: TPanel;
    pMapTablesTools: TPanel;
    sboxMapEdit: TScrollBox;
    splitFieldEdit: TSplitter;
    splitTableField: TSplitter;
    TimerFind: TTimer;
    procedure FormShow(Sender: TObject);
    procedure navTabsTabChanged(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
  private
    Parar: Boolean;
    SearchStr: TSearch;
  public

  end;

var
  frmDevTools: TfrmDevTools;

implementation

uses dm_dev;

{$R *.lfm}

{ TfrmDevTools }

procedure TfrmDevTools.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;


end;

procedure TfrmDevTools.FormShow(Sender: TObject);
var
  OldPPI: Integer;
begin
  OldPPI := Self.PixelsPerInch;
  if OldPPI <> 96 then
  begin
    navTabs.OptScalePercents := (OldPPI * 100) div 96;
  end;

  PG.PageIndex := 0;

end;

procedure TfrmDevTools.navTabsTabChanged(Sender: TObject);
begin
  PG.PageIndex := navTabs.TabIndex;
end;

end.

