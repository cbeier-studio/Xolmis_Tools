unit ufrm_TaxaEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, Menus, DB, DBCtrls,
  ActnList, StdCtrls, attabs, ATStatusBar, BCPanel, BCButton, ColorSpeedButton, StrUtils, RegExpr,
  BGRABitmap, SQLDB, CheckLst, rxswitch, Grids, DBGrids, DBEditButton, lib_taxa;

type

  { TfrmTaxaEditor }

  TfrmTaxaEditor = class(TForm)
    actExit: TAction;
    actBatchActions: TAction;
    actFormatSciNames: TAction;
    actImport: TAction;
    actExport: TAction;
    actAbout: TAction;
    actList: TActionList;
    AppProperties: TApplicationProperties;
    cbtCbroRank: TDBLookupComboBox;
    cbtIocRank: TDBLookupComboBox;
    cbtRank: TDBLookupComboBox;
    cktCbro: TDBCheckBox;
    cktClements: TDBCheckBox;
    cktExtinct: TDBCheckBox;
    cktIoc: TDBCheckBox;
    dsTaxa: TDataSource;
    etParentTaxon: TDBEditButton;
    eFindRank: TEdit;
    etAuthorship: TDBEdit;
    etCbroOtherPtNames: TDBEdit;
    etCbroSortNr: TDBEdit;
    etEbirdCode: TDBEdit;
    etEnglishName: TDBEdit;
    etExtinctionYear: TDBEdit;
    etFullname: TDBEdit;
    etIocEnglishName: TDBEdit;
    etIocSortNr: TDBEdit;
    etIocParentTaxon: TDBEditButton;
    etIocValidName: TDBEditButton;
    etCbroParentTaxon: TDBEditButton;
    etCbroValidName: TDBEditButton;
    etValidName: TDBEditButton;
    etPortugueseName: TDBEdit;
    etQuickcode: TDBEdit;
    etSortNr: TDBEdit;
    etSpanishName: TDBEdit;
    etSubspecificGroup: TDBEdit;
    iconFindRank: TImage;
    lbltAuthorship: TLabel;
    lbltCbroOtherPtNames: TLabel;
    lbltCbroParentTaxon: TLabel;
    lbltCbroRank: TLabel;
    lbltCbroSortNr: TLabel;
    lbltCbroValidName: TLabel;
    lbltDistribution: TLabel;
    lbltEbirdCode: TLabel;
    lbltEnglishName: TLabel;
    lbltFullname: TLabel;
    lbltIocDistribution: TLabel;
    lbltIocEnglishName: TLabel;
    lbltIocParentTaxon: TLabel;
    lbltIocRank: TLabel;
    lbltIocSortNr: TLabel;
    lbltIocValidName: TLabel;
    lbltParentTaxon: TLabel;
    lbltPortugueseName: TLabel;
    lbltQuickCode: TLabel;
    lbltRank: TLabel;
    lbltSortNr: TLabel;
    lbltSpanishName: TLabel;
    lbltSubspecificGroup: TLabel;
    lbltValidName: TLabel;
    mtDistribution: TDBMemo;
    mtIocDistribution: TDBMemo;
    peTaxa: TPanel;
    pFindRank: TBCPanel;
    ptAuthorship: TBCPanel;
    pRankToolbar: TBCPanel;
    ptCbroOtherPtNames: TBCPanel;
    ptCbroParentTaxon: TBCPanel;
    ptCbroRank: TBCPanel;
    ptCbroSortNr: TBCPanel;
    ptCbroValidName: TBCPanel;
    ptContent: TBCPanel;
    gridTaxa: TDBGrid;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    ptDistribution: TBCPanel;
    ptEbirdCode: TBCPanel;
    ptEnglishName: TBCPanel;
    ptExtinct: TBCPanel;
    ptFullName: TBCPanel;
    ptIocDistribution: TBCPanel;
    ptIocEnglishName: TBCPanel;
    ptIocParentTaxon: TBCPanel;
    ptIocRank: TBCPanel;
    ptIocSortNr: TBCPanel;
    ptIocValidName: TBCPanel;
    ptParentTaxon: TBCPanel;
    ptPortugueseName: TBCPanel;
    ptQuickCode: TBCPanel;
    ptRank: TBCPanel;
    ptSortNr: TBCPanel;
    ptSpanishName: TBCPanel;
    ptSubspecificGroup: TBCPanel;
    ptToolbar: TBCPanel;
    ptValidName: TBCPanel;
    sbCancelRank: TSpeedButton;
    sbClearFiltersRanks: TSpeedButton;
    sbClearFindTaxa: TColorSpeedButton;
    sbClearSearchRank: TColorSpeedButton;
    sbDelRecord: TSpeedButton;
    sbEditRecord: TSpeedButton;
    sbFirstRank: TSpeedButton;
    sbGroupRanks: TSpeedButton;
    sbInsertRank: TSpeedButton;
    sbLastRank: TSpeedButton;
    sbMoreOptionsRanks: TSpeedButton;
    sbNextRank: TSpeedButton;
    sboxTaxa: TScrollBox;
    sbPriorRank: TSpeedButton;
    sbRecordHistory: TSpeedButton;
    sbLumpTaxon: TSpeedButton;
    sbSplitTaxon: TSpeedButton;
    sbMoveTaxon: TSpeedButton;
    sbRefreshRanks: TSpeedButton;
    sbSaveRank: TSpeedButton;
    sbSortRanks: TSpeedButton;
    Separator2: TMenuItem;
    mmBatchActions: TMenuItem;
    mmExit: TMenuItem;
    mmFormatSciNames: TMenuItem;
    pmMain: TPopupMenu;
    clbTaxonRanksFilter: TCheckListBox;
    eFindTaxa: TEdit;
    icoBandSizeFilter11: TImage;
    icoBandSizeFilter12: TImage;
    icoBandSizeFilter9: TImage;
    icoExtinctFilter: TImage;
    icoMarkedFilter: TImage;
    iconFindTaxa: TImage;
    icoSynonymsFilter: TImage;
    icoTaxonomiesFilter: TImage;
    icoTaxonRanksFilter: TImage;
    icoUnmarkedFilter: TImage;
    lblClementsFilter: TLabel;
    lblCountTaxonRanksFilter: TLabel;
    lblExtinctFilter: TLabel;
    lblHasSynonymsFilter: TLabel;
    lblMarkedFilter: TLabel;
    lblSynonymFilter: TLabel;
    lblTaxonomyCbroFilter: TLabel;
    lblTaxonomyIocFilter: TLabel;
    lblTaxonRanksFilter: TLabel;
    lblUnmarkedFilter: TLabel;
    navTabs: TATTabs;
    nbTaxaSide: TNotebook;
    pFindTaxa: TBCPanel;
    pExtinctFilter: TBCPanel;
    pgTaxaFilters: TPage;
    pHasSynonymsFilter: TBCPanel;
    pIsSynonymFilter: TBCPanel;
    pMainMenu: TBCPanel;
    bMenu: TImageList;
    bStatusBar: TImageList;
    nbPages: TNotebook;
    pgRanks: TPage;
    pgTaxonomies: TPage;
    pgTaxa: TPage;
    pMarkedFilter: TBCPanel;
    pTaxaList: TBCPanel;
    pTaxaRightBar: TBCPanel;
    pTaxaToolbar: TBCPanel;
    pTaxonomyCbroFilter: TBCPanel;
    pTaxonomyClementsFilter: TBCPanel;
    pTaxonomyIocFilter: TBCPanel;
    pTaxonRanksFilters: TBCPanel;
    pTitleTaxonRanksFilter: TPanel;
    pUnmarkedFilter: TBCPanel;
    sbAdvancedFilters: TSpeedButton;
    SBar: TATStatus;
    sbCancelRecord: TSpeedButton;
    sbClearFilters: TSpeedButton;
    sbFileMenu: TBCButton;
    sbFirstRecord: TSpeedButton;
    sbGroupRecords: TSpeedButton;
    sbInsertRecord: TSpeedButton;
    sbLastRecord: TSpeedButton;
    sbMoreOptions: TSpeedButton;
    sbNextRecord: TSpeedButton;
    sboxTaxaFilters: TScrollBox;
    sbPriorRecord: TSpeedButton;
    sbRefreshRecords: TSpeedButton;
    sbSaveRecord: TSpeedButton;
    sbShowAudio: TSpeedButton;
    sbShowDocs: TSpeedButton;
    sbShowImages: TSpeedButton;
    sbShowQuickFilters: TSpeedButton;
    sbShowRecycle: TSpeedButton;
    sbSortRecords: TSpeedButton;
    Separator1: TMenuItem;
    splitTaxaLeft: TSplitter;
    splitTaxaRight: TSplitter;
    TimerFind: TTimer;
    tsfMarked: TRxSwitch;
    tsfUnmarked: TRxSwitch;
    tsHasSynonyms: TRxSwitch;
    tsIsSynonym: TRxSwitch;
    tsTaxonExtinct: TRxSwitch;
    tsTaxonomyCbro: TRxSwitch;
    tsTaxonomyClements: TRxSwitch;
    tsTaxonomyIoc: TRxSwitch;
    procedure actAboutExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure dsTaxaStateChange(Sender: TObject);
    procedure eFindTaxaChange(Sender: TObject);
    procedure etAuthorshipChange(Sender: TObject);
    procedure etAuthorshipKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure etCbroParentTaxonButtonClick(Sender: TObject);
    procedure etCbroValidNameButtonClick(Sender: TObject);
    procedure etFullnameExit(Sender: TObject);
    procedure etIocParentTaxonButtonClick(Sender: TObject);
    procedure etIocValidNameButtonClick(Sender: TObject);
    procedure etParentTaxonButtonClick(Sender: TObject);
    procedure etValidNameButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gridTaxaPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn; AState: TGridDrawState);
    procedure navTabsTabChanged(Sender: TObject);
    procedure sbCancelRecordClick(Sender: TObject);
    procedure sbClearFindTaxaClick(Sender: TObject);
    procedure sbEditRecordClick(Sender: TObject);
    procedure sbFirstRecordClick(Sender: TObject);
    procedure sbInsertRecordClick(Sender: TObject);
    procedure sbLastRecordClick(Sender: TObject);
    procedure sbLumpTaxonClick(Sender: TObject);
    procedure sbNextRecordClick(Sender: TObject);
    procedure sbPriorRecordClick(Sender: TObject);
    procedure sbRefreshRecordsClick(Sender: TObject);
    procedure sbSaveRecordClick(Sender: TObject);
    procedure sbSplitTaxonClick(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
  private
    FSearch: TCustomSearch;
    SkipCompleteAutor: Boolean;
    CanToggle: Boolean;
    Working: Boolean;
    procedure GetTaxaFilters;
    function SearchTaxa(aValue: String): Boolean;
    procedure UpdateButtons(aDataSet: TDataSet);
  public

  end;

var
  frmTaxaEditor: TfrmTaxaEditor;

implementation

uses
  udm_taxa, udlg_about, udlg_desttaxon, udlg_edithierarchy, udlg_sqlfilter;

{$R *.lfm}

{ TfrmTaxaEditor }

procedure TfrmTaxaEditor.actAboutExecute(Sender: TObject);
begin
  dlgAbout := TdlgAbout.Create(nil);
  try
    dlgAbout.ShowModal;
  finally
    FreeAndNil(dlgAbout);
  end;
end;

procedure TfrmTaxaEditor.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmTaxaEditor.dsTaxaStateChange(Sender: TObject);
begin
  UpdateButtons(dsTaxa.DataSet);
end;

procedure TfrmTaxaEditor.eFindTaxaChange(Sender: TObject);
begin
  sbClearFindTaxa.Visible := Length(Trim(eFindTaxa.Text)) > 0;

  TimerFind.Enabled := False;
  TimerFind.Enabled := True;
end;

procedure TfrmTaxaEditor.etAuthorshipChange(Sender: TObject);
var toComplete, completeName: String;
    Qry: TSQLQuery;
begin
  inherited;
  if SkipCompleteAutor then
    Exit;

  if (dsTaxa.DataSet.State in [dsInsert, dsEdit]) and (Length(etAuthorship.Text) >= 3) then
  begin
    Qry := TSQLQuery.Create(dmTaxa.sqlCon);
    with Qry, SQL do
    try
      Database := dmTaxa.sqlCon;
      Clear;
      Add('SELECT DISTINCT authorship FROM zoo_taxa');
      Add('WHERE authorship LIKE :author');
      Add('ORDER BY authorship ASC');
      ParamByName('author').AsString := etAuthorship.Text + '%';
      Open;
      if RecordCount > 0 then
      begin
        First;
        toComplete := FieldByName('authorship').AsString;
        completeName := toComplete;
        toComplete := StringReplace(toComplete, etAuthorship.Text, '', [rfIgnoreCase]);
        etAuthorship.Text := completeName;
        etAuthorship.SelStart := Length(etAuthorship.Text);
        etAuthorship.SelLength := Length(toComplete);
      end;
    finally
      Close;
      FreeAndNil(Qry);
    end;
  end;
end;

procedure TfrmTaxaEditor.etAuthorshipKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //FormKeyDown(Sender, Key, Shift);
  if (dsTaxa.DataSet.State in [dsInsert, dsEdit]) then
  begin
    if (Key = VK_DELETE) then
    begin
      if etAuthorship.Focused then
        SkipCompleteAutor := True;
    end;
  end;
end;

procedure TfrmTaxaEditor.etCbroParentTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], etCbroParentTaxon, dmTaxa.qTaxa, 'cbrp_parent_taxon_id', 'cbro_parent_taxon_name', True, False);
end;

procedure TfrmTaxaEditor.etCbroValidNameButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], etCbroValidName, dmTaxa.qTaxa, 'cbro_valid_id', 'cbro_valid_name', True, False);
end;

procedure TfrmTaxaEditor.etFullnameExit(Sender: TObject);
var
  DS: TDataSet;
  wc: Integer;
  FSciName: String;
begin
  DS := etFullname.DataSource.DataSet;

  if not (DS.State in [dsInsert, dsEdit]) then
    Exit;

  FSciName := Trim(DS.FieldByName('full_name').AsString);
  if not (FSciName = EmptyStr) then
  begin
    wc := WordCount(FSciName,[' ']);
    case wc of
      1:
      begin
        if ExecRegExpr('^.+ini$', FSciName) then
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trTribe)])
        else
        if ExecRegExpr('^.+inae$', FSciName) then
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trSubfamily)])
        else
        if ExecRegExpr('^.+idae$', FSciName) then
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trFamily)])
        else
        if ExecRegExpr('^.+formes$', FSciName) then
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trOrder)])
        else
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trGenus)]);
        DS.FieldByName('genus_epithet').AsString := FSciName;
      end;
      2:
      begin
        if (ExtractWord(1, FSciName, [' ']) = 'sp.') then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trSpuh)]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').Clear;
        end else
        if (Pos('/', ExtractWord(1, FSciName, [' '])) > 0) then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trSlash)]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').Clear;
        end else
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trSpecies)]);
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').AsString := ExtractWord(1, FSciName, [' ']);
        end;
        if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
        begin
          DS.FieldByName('parent_taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']));
        end;
      end;
      3:
      begin
        if (ExtractWord(1, FSciName, [' ']) = 'sp.') then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trSpuh)]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').Clear;
          if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
          begin
            DS.FieldByName('parent_taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']));
          end;
        end else
        if (Pos('/', ExtractWord(1, FSciName, [' '])) > 0) then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trSlash)]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').Clear;
          DS.FieldByName('species_epithet').Clear;
        end else
        if (Pos('/', ExtractWord(2, FSciName, [' '])) > 0) then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trPolitypicGroup)]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').AsString := ExtractWord(1, FSciName, [' ']);
          if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
          begin
            DS.FieldByName('parent_taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']) + ' ' + ExtractWord(1, FSciName, [' ']));
          end;
        end else
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trSubspecies)]);
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').AsString := ExtractWord(1, FSciName, [' ']);
          DS.FieldByName('subspecies_epithet').AsString := ExtractWord(2, FSciName, [' ']);
          if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
          begin
            DS.FieldByName('parent_taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']) + ' ' + ExtractWord(1, FSciName, [' ']));
          end;
        end;
      end;
      4:
      begin
        if (ExtractWord(1, FSciName, [' ']) = 'sp.') then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trSpuh)]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').Clear;
          if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
          begin
            DS.FieldByName('parent_taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']));
          end;
        end else
        if (ExtractWord(2, FSciName, [' ']) = 'x') then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trHybrid)]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').Clear;
          if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
          begin
            DS.FieldByName('parent_taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']));
          end;
        end else
        if (Pos('[', ExtractWord(2, FSciName, [' '])) > 0) then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trPolitypicGroup)]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').AsString := ExtractWord(1, FSciName, [' ']);
          if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
          begin
            DS.FieldByName('parent_taxon_id').AsInteger:=
              GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']) + ' ' + ExtractWord(1, FSciName, [' ']));
          end;
        end else
        begin
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').AsString := ExtractWord(1, FSciName, [' ']);
          DS.FieldByName('subspecies_epithet').AsString := ExtractWord(2, FSciName, [' ']) + ' ' +
                                                           ExtractWord(3, FSciName, [' ']);
        end;
      end;
      5:
      begin
        if (ExtractWord(2, FSciName, [' ']) = 'x') then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZooRanks[Ord(trHybrid)]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').Clear;
        end else
        begin
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').AsString := ExtractWord(1, FSciName, [' ']);
          DS.FieldByName('subspecies_epithet').AsString := ExtractWord(2, FSciName, [' ']) + ' ' +
                                                           ExtractWord(3, FSciName, [' ']) + ' ' +
                                                           ExtractWord(4, FSciName, [' ']);
        end;
      end;
      6:
      begin
        DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
        DS.FieldByName('species_epithet').AsString := ExtractWord(1, FSciName, [' ']);
        DS.FieldByName('subspecies_epithet').AsString := ExtractWord(2, FSciName, [' ']) + ' ' +
                                                         ExtractWord(3, FSciName, [' ']) + ' ' +
                                                         ExtractWord(4, FSciName, [' ']) + ' ' +
                                                         ExtractWord(5, FSciName, [' ']);
      end;
      7:
      begin
        DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
        DS.FieldByName('species_epithet').AsString := ExtractWord(1, FSciName, [' ']);
        DS.FieldByName('subspecies_epithet').AsString := ExtractWord(2, FSciName, [' ']) + ' ' +
                                                         ExtractWord(3, FSciName, [' ']) + ' ' +
                                                         ExtractWord(4, FSciName, [' ']) + ' ' +
                                                         ExtractWord(5, FSciName, [' ']) + ' ' +
                                                         ExtractWord(6, FSciName, [' ']);
      end;
      8:
      begin
        DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
        DS.FieldByName('species_epithet').AsString := ExtractWord(1, FSciName, [' ']);
        DS.FieldByName('subspecies_epithet').AsString := ExtractWord(2, FSciName, [' ']) + ' ' +
                                                         ExtractWord(3, FSciName, [' ']) + ' ' +
                                                         ExtractWord(4, FSciName, [' ']) + ' ' +
                                                         ExtractWord(5, FSciName, [' ']) + ' ' +
                                                         ExtractWord(6, FSciName, [' ']) + ' ' +
                                                         ExtractWord(7, FSciName, [' ']);
      end;
    end;
  end;
end;

procedure TfrmTaxaEditor.etIocParentTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], etIocParentTaxon, dsTaxa.DataSet, 'ioc_parent_taxon_id', 'ioc_parent_taxon_name', True, False);
end;

procedure TfrmTaxaEditor.etIocValidNameButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], etIocValidName, dsTaxa.DataSet, 'ioc_valid_id', 'ioc_valid_name', True, False);
end;

procedure TfrmTaxaEditor.etParentTaxonButtonClick(Sender: TObject);
var
  FRank: TZooRank;
  FFilter: TTaxonFilters;
begin
  FRank := GetRankType(dmTaxa.qTaxa.FieldByName('rank_id').AsInteger);

  case FRank of
    //trDomain: ;
    //trSubDomain: ;
    //trHyperkingdom: ;
    //trSuperkingdom: ;
    //trKingdom: ;
    //trSubkingdom: ;
    //trInfrakingdom: ;
    //trParvkingdom: ;
    //trSuperphylum: ;
    //trPhylum: ;
    //trSubphylum: ;
    //trInfraphylum: ;
    //trMicrophylum: ;
    //trSuperclass: ;
    //trClass: ;
    //trSubclass: ;
    //trInfraclass: ;
    //trSubterclass: ;
    //trParvclass: ;
    //trSuperdivision: ;
    //trDivision: ;
    //trSubdivision: ;
    //trInfradivision: ;
    //trSuperlegion: ;
    //trLegion: ;
    //trSublegion: ;
    //trInfralegion: ;
    //trSupercohort: ;
    //trCohort: ;
    //trSubcohort: ;
    //trInfracohort: ;
    //trGigaorder: ;
    //trMegaorder: ;
    //trGrandorder: ;
    //trHyperorder: ;
    //trSuperorder: ;
    //trSeriesOrder: ;
    //trOrder: ;
    //trNanorder,
    //trHypoorder,
    //trMinorder,
    trSuborder,
    trInfraorder,
    trParvorder,
    trSection,
    trSubsection:     FFilter := [tfOrders];
    //trGigafamily,
    //trMegafamily,
    //trGrandfamily,
    //trHyperfamily,
    trSuperfamily,
    trFamily:         FFilter := [tfOrders];
    trSubfamily,
    trInfrafamily,
    trSupertribe,
    trTribe:          FFilter := [tfFamilies];
    trSubtribe,
    trInfratribe:     FFilter := [tfTribes];
    trSupergenus,
    trGenus:          FFilter := [tfFamilies, tfTribes];
    trSubgenus,
    trSuperspecies,
    trSpecies:        FFilter := [tfTribes, tfGenera];
    trSubspecies,
    trMonotypicGroup,
    trPolitypicGroup: FFilter := [tfSpecies];
    trSpuh:           FFilter := [tfGenera];
    trDomestic:       FFilter := [tfSpecies];
    trForm,
    trHybrid,
    trIntergrade,
    trSlash:          FFilter := [tfMain];
  end;

  if FindTaxonDlg(FFilter, etParentTaxon, dmTaxa.qTaxa, 'parent_taxon_id', 'parent_taxon_name', True, False) then
    SelectNext(Sender as TWinControl, True, True);
end;

procedure TfrmTaxaEditor.etValidNameButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], etValidName, dmTaxa.qTaxa, 'valid_id', 'valid_name', True, False);
end;

procedure TfrmTaxaEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  dmTaxa.sqlCon.CloseDataSets;
end;

procedure TfrmTaxaEditor.FormCreate(Sender: TObject);
begin
  CanToggle := False;
end;

procedure TfrmTaxaEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSearch);
end;

procedure TfrmTaxaEditor.FormShow(Sender: TObject);
begin
  FSearch := TCustomSearch.Create(tbZooTaxa);
  FSearch.DataSet := TSQLQuery(dsTaxa.DataSet);

  dmTaxa.lookRanks.Open;
  dsTaxa.DataSet.Open;

  LoadTaxaRanks(dmTaxa.sqlCon, clbTaxonRanksFilter);

  if Self.PixelsPerInch <> 96 then
  begin
    navTabs.OptScalePercents := (Self.PixelsPerInch * 100) div 96;
  end;

  UpdateButtons(dsTaxa.DataSet);
  CanToggle := True;
end;

procedure TfrmTaxaEditor.gridTaxaPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
  AState: TGridDrawState);
//var
//  aRank: Integer;
begin
  //aRank := TDBGrid(Sender).DataSource.DataSet.FieldByName('rank_id').AsInteger;
  //if GetRankType(aRank) >= trSuperGenus then
  //  Column.Font.Style := [fsItalic]
  //else
  //  Column.Font.Style := [fsBold];
end;

procedure TfrmTaxaEditor.navTabsTabChanged(Sender: TObject);
begin
  nbPages.PageIndex := navTabs.TabIndex;
end;

procedure TfrmTaxaEditor.sbCancelRecordClick(Sender: TObject);
begin
  dsTaxa.DataSet.Cancel;
end;

procedure TfrmTaxaEditor.sbClearFindTaxaClick(Sender: TObject);
begin
  eFindTaxa.Clear;
end;

procedure TfrmTaxaEditor.sbEditRecordClick(Sender: TObject);
begin
  dsTaxa.DataSet.Edit;
end;

procedure TfrmTaxaEditor.sbFirstRecordClick(Sender: TObject);
begin
  dsTaxa.DataSet.First;
end;

procedure TfrmTaxaEditor.sbInsertRecordClick(Sender: TObject);
begin
  dsTaxa.DataSet.Insert;
end;

procedure TfrmTaxaEditor.sbLastRecordClick(Sender: TObject);
begin
  dsTaxa.DataSet.Last;
end;

procedure TfrmTaxaEditor.sbLumpTaxonClick(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  dlgDestTaxon := TdlgDestTaxon.Create(nil);
  with dlgDestTaxon do
  try
    TaxonomyAction:= taLump;
    if ShowModal = mrOK then
    begin
      case ApplyTo of
        acSelected:
        begin
          LumpTaxon(dsTaxa.DataSet.FieldByName('taxon_id').AsInteger, Taxon, Taxonomies, True);
        end;
        acMarked:
        begin
          Qry := TSQLQuery.Create(dmTaxa.sqlCon);
          Qry.SQLConnection := dmTaxa.sqlCon;
          with Qry, SQL do
          try
            Add('SELECT taxon_id FROM zoo_taxa WHERE (marked_status = 1) AND (active_status = 1)');
            Open;
            First;
            repeat
              LumpTaxon(Qry.FieldByName('taxon_id').AsInteger, Taxon, Taxonomies, True);
              Next;
            until Eof;
            Close;
            Clear;
            Add('UPDATE zoo_taxa SET marked_status = 0 WHERE marked_status = 1');
            ExecSQL;
          finally
            FreeAndNil(Qry);
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(dlgDestTaxon);
  end;
end;

procedure TfrmTaxaEditor.sbNextRecordClick(Sender: TObject);
begin
  dsTaxa.DataSet.Next;
end;

procedure TfrmTaxaEditor.sbPriorRecordClick(Sender: TObject);
begin
  dsTaxa.DataSet.Prior;
end;

procedure TfrmTaxaEditor.sbRefreshRecordsClick(Sender: TObject);
begin
  dsTaxa.DataSet.Refresh;
end;

procedure TfrmTaxaEditor.sbSaveRecordClick(Sender: TObject);
begin
  dsTaxa.DataSet.Post;
end;

procedure TfrmTaxaEditor.sbSplitTaxonClick(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  dlgDestTaxon := TdlgDestTaxon.Create(nil);
  with dlgDestTaxon do
  try
    TaxonomyAction:= taSplit;
    if ShowModal = mrOK then
    begin
      case ApplyTo of
        acSelected:
        begin
          SplitTaxon(dsTaxa.DataSet.FieldByName('taxon_id').AsInteger, Taxonomies, True);
        end;
        acMarked:
        begin
          Qry := TSQLQuery.Create(dmTaxa.sqlCon);
          Qry.SQLConnection := dmTaxa.sqlCon;
          with Qry, SQL do
          try
            Add('SELECT taxon_id FROM zoo_taxa WHERE (marked_status = 1) AND (active_status = 1)');
            Open;
            First;
            repeat
              SplitTaxon(Qry.FieldByName('taxon_id').AsInteger, Taxonomies, True);
              Next;
            until Eof;
            Close;
            Clear;
            Add('UPDATE zoo_taxa SET marked_status = 0 WHERE marked_status = 1');
            ExecSQL;
          finally
            FreeAndNil(Qry);
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(dlgDestTaxon);
  end;
end;

procedure TfrmTaxaEditor.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;
  if not CanToggle then
    Exit;

  SearchTaxa(eFindTaxa.Text);
end;

procedure TfrmTaxaEditor.GetTaxaFilters;
var
  sf: Integer;
begin
  if not CanToggle then
    Exit;

  CanToggle := False;

  if (tsfMarked.StateOn = sw_on) then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean,
      crEqual, False, '1'));
  end
  else
  if (tsfUnmarked.StateOn = sw_on) then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean,
      crEqual, False, '0'));
  end;

  if tsTaxonomyClements.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('clements_taxonomy', 'Clements/eBird', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsTaxonomyIoc.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('ioc_taxonomy', 'IOC', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsTaxonomyCbro.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('cbro_taxonomy', 'CBRO', sdtBoolean,
      crEqual, False, '1'));
  end;

  if tsTaxonExtinct.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('extinct', 'Extinct', sdtBoolean,
      crEqual, False, '1'));
  end;

  if tsIsSynonym.StateOn = sw_on then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
      crDistinct, False, '0'));
  end;
  //if tsHasSynonyms.StateOn = sw_on then
  //begin
  //  sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
  //  FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
  //    crMoreThan, False, '1'));
  //end;

  CanToggle := True;
end;

function TfrmTaxaEditor.SearchTaxa(aValue: String): Boolean;
var
  Crit: TCriteriaType;
  g: Integer;
  i: Longint;
begin
  Result := False;

  if Working then
    Exit;

  Working := True;
  FSearch.Fields.Clear;
  FSearch.QuickFilters.Clear;

  Crit := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      Crit := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      Crit := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, i) then
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('taxon_id', 'Taxon (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      g := FSearch.Fields.Add(TSearchGroup.Create);
      FSearch.Fields[g].Fields.Add(TSearchField.Create('full_name', 'Scientific name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('english_name', 'English name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('ioc_english_name', 'English name (IOC)', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('spanish_name', 'Spanish name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('portuguese_name', 'Portuguese name', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('other_portuguese_names', 'Other portuguese names', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('ebird_code', 'eBird code', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('quick_code', 'Quick code', sdtText, Crit,
        False, aValue));
    end;
  end;

  GetTaxaFilters;

  Result := FSearch.RunSearch > 0;

  Working := False;

  UpdateButtons(dsTaxa.DataSet);
end;

procedure TfrmTaxaEditor.UpdateButtons(aDataSet: TDataSet);
begin
  if (aDataSet.State in [dsInsert, dsEdit]) then
  begin
    sbInsertRecord.Enabled := False;
    sbEditRecord.Enabled := False;
    sbDelRecord.Enabled := False;
    sbFirstRecord.Enabled := False;
    sbPriorRecord.Enabled := False;
    sbNextRecord.Enabled := False;
    sbLastRecord.Enabled := False;
    sbRecordHistory.Enabled := False;
    sbSortRecords.Enabled := False;

    sbShowQuickFilters.Enabled := False;
    sbShowImages.Enabled := False;
    sbShowAudio.Enabled := False;
    sbShowDocs.Enabled := False;
    //sbShowSummary.Enabled := False;
    sbShowRecycle.Enabled := False;

    sbCancelRecord.Visible := True;
    sbSaveRecord.Visible := True;

    //pmgRefresh.Enabled := False;

    //navGrid.Enabled := False;
    pTaxaRightBar.Enabled := False;
  end
  else
  begin
    if (aDataSet.Active) and not (TSQLQuery(aDataSet).ReadOnly) then
    begin
      sbInsertRecord.Enabled := True;
      sbEditRecord.Enabled := (aDataSet.RecordCount > 0);
      sbDelRecord.Enabled := (aDataSet.RecordCount > 0);
      sbRecordHistory.Enabled := (aDataSet.RecordCount > 0);
      sbSortRecords.Enabled := (aDataSet.RecordCount > 0);
    end
    else
    begin
      sbEditRecord.Enabled := False;
      sbEditRecord.Enabled := False;
      sbDelRecord.Enabled := False;
      sbRecordHistory.Enabled := False;
      sbSortRecords.Enabled := False;
    end;
    sbFirstRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
    sbPriorRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
    sbNextRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);
    sbLastRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);

    sbShowQuickFilters.Enabled := True;
    sbShowImages.Enabled := True;
    sbShowAudio.Enabled := True;
    sbShowDocs.Enabled := True;
    //sbShowSummary.Enabled := True;
    sbShowRecycle.Enabled := True;

    //pmgRefresh.Enabled := True;

    sbSaveRecord.Visible := False;
    sbCancelRecord.Visible := False;

    //navGrid.Enabled := True;
    pTaxaRightBar.Enabled := True;
  end;
  //pmgEdit.Enabled := sbEditRecord.Enabled;
  //pmgDel.Enabled := sbDelRecord.Enabled;
  //pmgRecordHistory.Enabled := sbChildHistory.Enabled;

  //if dsLink.DataSet.RecordCount = 1 then
  //  lblRecordStatus.Caption := Format(rsRecordsFound, [dsLink.DataSet.RecordCount, rsRecords])
  //else
  //if dsLink.DataSet.RecordCount > 1 then
  //  lblRecordStatus.Caption := Format(rsRecordsFound, [dsLink.DataSet.RecordCount, rsRecordsPlural])
  //else
  //  lblRecordStatus.Caption := rsNoRecordsFound;
end;

end.

