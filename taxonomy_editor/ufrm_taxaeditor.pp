unit ufrm_TaxaEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLIntf, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, Menus, DB, DBCtrls,
  ActnList, StdCtrls, BCPanel, BCButton, ColorSpeedButton, StrUtils, RegExpr, Character,
  BGRABitmap, SQLDB, CheckLst, Grids, DBGrids, ComCtrls, DBEditButton, ToggleSwitch, utils_global, data_types,
  Types, ImgList, HtmlView;

type

  { TfrmTaxaEditor }

  TfrmTaxaEditor = class(TForm)
    actExit: TAction;
    actBatchActions: TAction;
    actFormatSciNames: TAction;
    actImport: TAction;
    actExport: TAction;
    actAbout: TAction;
    actImportIOCNames: TAction;
    actImportClements: TAction;
    actNormalizeSynonyms: TAction;
    actSspVernacularNames: TAction;
    actRewriteHierarchy: TAction;
    actList: TActionList;
    AppProperties: TApplicationProperties;
    bMenu: TImageList;
    cbAuthorship: TDBComboBox;
    cbtIucnStatus: TDBComboBox;
    cbtRank: TDBLookupComboBox;
    ckAccepted: TDBCheckBox;
    cktExtinct: TDBCheckBox;
    dbgCountries: TDBGrid;
    dbgVernacular: TDBGrid;
    dsTaxa: TDataSource;
    eFind: TEdit;
    etEbirdCode: TDBEdit;
    etConceptId: TDBEdit;
    etExtinctionYear: TDBEdit;
    etFullname: TDBEdit;
    etParentTaxon: TDBEditButton;
    etQuickcode: TDBEdit;
    etSortNr: TDBEdit;
    gridPacks: TDBGrid;
    gridTaxa: TDBGrid;
    HtmlView: THtmlViewer;
    iconFind: TImage;
    lblCountTaxa: TLabel;
    lbltAuthorship: TLabel;
    lbltCountries: TLabel;
    lbltDistribution: TLabel;
    lbltEbirdCode: TLabel;
    lbltConceptId: TLabel;
    lbltFullname: TLabel;
    lblTitleHierarchy: TLabel;
    lbltIucnStatus: TLabel;
    lbltParentTaxon: TLabel;
    lbltQuickCode: TLabel;
    lbltRank: TLabel;
    lbltSortNr: TLabel;
    lbltVernacular: TLabel;
    pmgnSetCurrentName: TMenuItem;
    mmNormalizeSynonyms: TMenuItem;
    pmgUnmarkAll: TMenuItem;
    pmgMarkAll: TMenuItem;
    pmgRemovePolitypicGroup: TMenuItem;
    pmgNewPolitypicGroup: TMenuItem;
    mtDistribution: TDBMemo;
    peTaxa: TPanel;
    pFind: TBCPanel;
    pmGridNames: TPopupMenu;
    pPacksList: TPanel;
    ptAuthorship: TPanel;
    pTaxaList: TPanel;
    pTaxaRightBar: TPanel;
    ptContent: TPanel;
    ptCountries: TPanel;
    ptDistribution: TPanel;
    ptFullName: TPanel;
    ptIucnStatus: TPanel;
    pToolbar: TPanel;
    ptParentTaxon: TPanel;
    ptQuickCode: TPanel;
    ptRank: TPanel;
    ptSliverFullname: TBCPanel;
    ptToolbar: TPanel;
    ptVernacular: TPanel;
    sbAddVernacular: TSpeedButton;
    sbAdvancedFilters: TSpeedButton;
    sbCancelRecord: TSpeedButton;
    sbClearFilters: TSpeedButton;
    sbClearFind: TColorSpeedButton;
    sbDelCountry: TSpeedButton;
    sbDelRecord: TSpeedButton;
    sbDelVernacular: TSpeedButton;
    sbEditCountry: TSpeedButton;
    sbEditHierarchy: TSpeedButton;
    sbEditRecord: TSpeedButton;
    sbEditSpeciesList: TSpeedButton;
    sbExport: TSpeedButton;
    sbFirstRecord: TSpeedButton;
    sbInsertRecord: TSpeedButton;
    sbLastRecord: TSpeedButton;
    sbLumpTaxon: TSpeedButton;
    sbMoreOptions: TSpeedButton;
    sbMoveTaxon: TSpeedButton;
    sbNextRecord: TSpeedButton;
    sboxTaxa: TScrollBox;
    sbPriorRecord: TSpeedButton;
    sbRefreshRecords: TSpeedButton;
    sbSaveRecord: TSpeedButton;
    sbShowImages: TSpeedButton;
    sbShowQuickFilters: TSpeedButton;
    sbShowRecycle: TSpeedButton;
    sbSortRecords: TSpeedButton;
    gridChildTaxa: TDBGrid;
    gridCountries: TDBGrid;
    gridLanguages: TDBGrid;
    gridSynonyms: TDBGrid;
    gridChanges: TDBGrid;
    gridRanks: TDBGrid;
    icoAcceptedFilter: TImage;
    icoMarkedFilter: TImage;
    iIucnStatus: TImageList;
    bNavigation: TImageList;
    lblAcceptedFilter: TLabel;
    lblMarkedFilter: TLabel;
    lblTitleSynonyms: TLabel;
    lblTitleChilds: TLabel;
    mmSspVernacularNames: TMenuItem;
    mmRewriteHierarchy: TMenuItem;
    pAcceptedFilter: TBCPanel;
    pChangesToolbar: TPanel;
    pNavigation: TPanel;
    pgLanguages: TPage;
    pgCountries: TPage;
    pDetails: TPanel;
    pMarkedFilter: TBCPanel;
    pmgRefresh: TMenuItem;
    pmgNewSubspecies: TMenuItem;
    pmgMove: TMenuItem;
    pmgMoveToSpecies: TMenuItem;
    pmgMoveToGenus: TMenuItem;
    pmgMoveToFamily: TMenuItem;
    pmgMoveToOrder: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    pmgEdit: TMenuItem;
    pmgDel: TMenuItem;
    pmgSplit: TMenuItem;
    pmgLump: TMenuItem;
    OpenDlg: TOpenDialog;
    pmGridTaxa: TPopupMenu;
    pmtSortTaxonomic: TMenuItem;
    pmtSortAlphabetical: TMenuItem;
    pmvMoveToSpecies: TMenuItem;
    pmvMoveToGenus: TMenuItem;
    pmvMoveToFamily: TMenuItem;
    pmvMoveToOrder: TMenuItem;
    pmMove: TPopupMenu;
    pmSortTaxa: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    rbAcceptedAll: TRadioButton;
    rbAcceptedNo: TRadioButton;
    rbAcceptedYes: TRadioButton;
    rbMarkedAll: TRadioButton;
    rbExtinctAll: TRadioButton;
    rbHasSynonymsAll: TRadioButton;
    rbExtinctNo: TRadioButton;
    rbHasSynonymsNo: TRadioButton;
    rbMarkedYes: TRadioButton;
    rbMarkedNo: TRadioButton;
    rbExtinctYes: TRadioButton;
    rbHasSynonymsYes: TRadioButton;
    sbSplitTaxon: TSpeedButton;
    Separator2: TMenuItem;
    mmBatchActions: TMenuItem;
    mmExit: TMenuItem;
    mmFormatSciNames: TMenuItem;
    pmMain: TPopupMenu;
    clbTaxonRanksFilter: TCheckListBox;
    icoBandSizeFilter12: TImage;
    icoExtinctFilter: TImage;
    icoTaxonRanksFilter: TImage;
    lblCountTaxonRanksFilter: TLabel;
    lblExtinctFilter: TLabel;
    lblHasSynonymsFilter: TLabel;
    lblTaxonRanksFilter: TLabel;
    nbTaxaSide: TNotebook;
    pExtinctFilter: TBCPanel;
    pgTaxaFilters: TPage;
    pHasSynonymsFilter: TBCPanel;
    nbPages: TNotebook;
    pgRanks: TPage;
    pgPackages: TPage;
    pgTaxa: TPage;
    pTaxonRanksFilters: TBCPanel;
    pTitleTaxonRanksFilter: TPanel;
    sboxTaxaFilters: TScrollBox;
    Separator1: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    sbMainMenu: TSpeedButton;
    sbTaxa: TSpeedButton;
    sbPackages: TSpeedButton;
    sbTaxonRanks: TSpeedButton;
    sbCountries: TSpeedButton;
    sbLanguages: TSpeedButton;
    sbDelSynonym: TSpeedButton;
    sbAddSynonym: TSpeedButton;
    sbInsertChange: TSpeedButton;
    sbEditChange: TSpeedButton;
    sbSaveChange: TSpeedButton;
    sbCancelChange: TSpeedButton;
    sbDelChange: TSpeedButton;
    Separator6: TMenuItem;
    splitTaxaLeft: TSplitter;
    splitPacksLeft: TSplitter;
    splitTaxaRight: TSplitter;
    TimerUpdate: TTimer;
    TimerOpen: TTimer;
    TimerFind: TTimer;
    tvHierarchy: TTreeView;
    txtTaxaFullName: TDBText;
    procedure actAboutExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actFormatSciNamesExecute(Sender: TObject);
    procedure actImportClementsExecute(Sender: TObject);
    procedure actImportIOCNamesExecute(Sender: TObject);
    procedure actNormalizeSynonymsExecute(Sender: TObject);
    procedure actRewriteHierarchyExecute(Sender: TObject);
    procedure actSspVernacularNamesExecute(Sender: TObject);
    procedure cbAuthorshipKeyPress(Sender: TObject; var Key: char);
    procedure cbtIucnStatusDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure clbTaxonRanksFilterClickCheck(Sender: TObject);
    procedure dbgVernacularCellClick(Column: TColumn);
    procedure dbgVernacularDblClick(Sender: TObject);
    procedure dsTaxaDataChange(Sender: TObject; Field: TField);
    procedure dsTaxaStateChange(Sender: TObject);
    procedure eFindChange(Sender: TObject);
    procedure eFindEnter(Sender: TObject);
    procedure etFullnameExit(Sender: TObject);
    procedure etParentTaxonButtonClick(Sender: TObject);
    procedure etParentTaxonKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure gridSynonymsPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
      AState: TGridDrawState);
    procedure gridTaxaCellClick(Column: TColumn);
    procedure gridTaxaDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    procedure gridTaxaMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure gridTaxaPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn; AState: TGridDrawState);
    procedure pmgMarkAllClick(Sender: TObject);
    procedure pmgNewPolitypicGroupClick(Sender: TObject);
    procedure pmgNewSubspeciesClick(Sender: TObject);
    procedure pmgnSetCurrentNameClick(Sender: TObject);
    procedure pmgRemovePolitypicGroupClick(Sender: TObject);
    procedure pmgUnmarkAllClick(Sender: TObject);
    procedure pmtSortClick(Sender: TObject);
    procedure pmvMoveToGenusClick(Sender: TObject);
    procedure pmvMoveToSpeciesClick(Sender: TObject);
    procedure rbMarkedYesClick(Sender: TObject);
    procedure sbAddSynonymClick(Sender: TObject);
    procedure sbAddVernacularClick(Sender: TObject);
    procedure sbAdvancedFiltersClick(Sender: TObject);
    procedure sbCancelChangeClick(Sender: TObject);
    procedure sbCancelRecordClick(Sender: TObject);
    procedure sbClearFiltersClick(Sender: TObject);
    procedure sbClearFindClick(Sender: TObject);
    procedure sbDelChangeClick(Sender: TObject);
    procedure sbDelCountryClick(Sender: TObject);
    procedure sbDelRecordClick(Sender: TObject);
    procedure sbDelSynonymClick(Sender: TObject);
    procedure sbDelVernacularClick(Sender: TObject);
    procedure sbEditChangeClick(Sender: TObject);
    procedure sbEditCountryClick(Sender: TObject);
    procedure sbEditHierarchyClick(Sender: TObject);
    procedure sbEditRecordClick(Sender: TObject);
    procedure sbEditSpeciesListClick(Sender: TObject);
    procedure sbFirstRecordClick(Sender: TObject);
    procedure sbInsertChangeClick(Sender: TObject);
    procedure sbInsertRecordClick(Sender: TObject);
    procedure sbLastRecordClick(Sender: TObject);
    procedure sbLumpTaxonClick(Sender: TObject);
    procedure sbMainMenuClick(Sender: TObject);
    procedure sbMoveTaxonClick(Sender: TObject);
    procedure sbNextRecordClick(Sender: TObject);
    procedure sbPriorRecordClick(Sender: TObject);
    procedure sbRefreshRecordsClick(Sender: TObject);
    procedure sbSaveChangeClick(Sender: TObject);
    procedure sbSaveRecordClick(Sender: TObject);
    procedure sbShowQuickFiltersClick(Sender: TObject);
    procedure sbSortRecordsClick(Sender: TObject);
    procedure sbSplitTaxonClick(Sender: TObject);
    procedure sbTaxaClick(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure TimerOpenTimer(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure tsTaxonExtinctOff(Sender: TObject);
    procedure tsTaxonExtinctOn(Sender: TObject);
  private
    FSearchTaxa, FSearchPacks, FSearchRanks, FSearchCountries, FSearchLanguages: TCustomSearch;
    FTaxaSearchString, FPackageSearchString,
      FRankSearchString, FCountrySearchString, FLanguageSearchString: String;
    CanToggle, canSearch: Boolean;
    Working: Boolean;
    procedure AddSortedField(aTable: TTableType; aFieldName: String; aDirection: TSortDirection; aCollation: String = '';
      IsAnAlias: Boolean = False);
    procedure ClearTaxaFilters;
    procedure GetTaxaFilters;
    procedure OpenAsync;
    function SearchCountries(aValue: String): Boolean;
    function SearchLanguages(aValue: String): Boolean;
    function SearchPackages(aValue: String): Boolean;
    function SearchRanks(aValue: String): Boolean;
    function SearchTaxa(aValue: String): Boolean;
    procedure UpdateButtons;
    procedure UpdateTree;
    function ValidateTaxon: Boolean;
  public

  end;

var
  frmTaxaEditor: TfrmTaxaEditor;

implementation

uses
  data_core, data_crud, data_getvalue, data_select, data_validations,
  models_rank, models_taxon,
  utils_dialogs, utils_taxonomy,
  io_clements, io_ioc,
  udm_taxa, udlg_about, udlg_loading, udlg_find, udlg_desttaxon, udlg_edithierarchy, udlg_newsubspecies, udlg_sqlfilter,
  uedt_occurrence, uedt_specieslist, uedt_vernacular, uedt_familysplit;

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

procedure TfrmTaxaEditor.actFormatSciNamesExecute(Sender: TObject);
begin
  with dmTaxa.qTaxa do
  try
    First;
    DisableControls;
    dlgLoading.Show;
    dlgLoading.Max := RecordCount;
    dlgLoading.UpdateProgress('Formatting scientific names...', 0);
    repeat
      Edit;
      FieldByName('formatted_name').AsString := FormattedBirdName(FieldByName('full_name').AsString, FieldByName('rank_id').AsInteger);
      Post;

      dlgLoading.Progress := RecNo;
      Application.ProcessMessages;
      Next;
    until Eof or Parar;
  finally
    First;
    EnableControls;
    dlgLoading.Hide;
    dlgLoading.Max := 100;
  end;
end;

procedure TfrmTaxaEditor.actImportClementsExecute(Sender: TObject);
begin
  if OpenDlg.Execute then
    ImportClementsData(OpenDlg.FileName);

  dmTaxa.qTaxa.Refresh;
end;

procedure TfrmTaxaEditor.actImportIOCNamesExecute(Sender: TObject);
begin
  if OpenDlg.Execute then
    ImportIocData(OpenDlg.FileName);

  dmTaxa.qTaxa.Refresh;
end;

procedure TfrmTaxaEditor.actNormalizeSynonymsExecute(Sender: TObject);
begin
  if not dmTaxa.sqlTrans.Active then
    dmTaxa.sqlTrans.StartTransaction;
  try
    NormalizeSynonyms;
    dmTaxa.sqlTrans.CommitRetaining;
  except
    dlgLoading.Hide;
    dmTaxa.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure TfrmTaxaEditor.actRewriteHierarchyExecute(Sender: TObject);
var
  Qry: TSQLQuery;
  iOrder, iFamily, iSubfamily, iGenus, iSpecies, iMonoGroup, iPoliGroup, iSubspecies: Integer;
begin
  try
    dlgLoading.Show;
    dlgLoading.Max := 8;
    Qry := TSQLQuery.Create(nil);
    with Qry, SQL do
    try
      DataBase := dmTaxa.sqlCon;
      Transaction := dmTaxa.sqlTrans;
      MacroCheck := True;

      iOrder := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ord.');
      iFamily := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'fam.');
      iSubfamily := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'subfam.');
      iGenus := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'g.');
      iSpecies := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'sp.');
      iMonoGroup := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'grp. (mono)');
      iPoliGroup := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'grp. (poli)');
      iSubspecies := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ssp.');

      dlgLoading.UpdateProgress('Rewriting taxa hierarchy...', 0);
      if not dmTaxa.sqlTrans.Active then
        dmTaxa.sqlTrans.StartTransaction;
      try
        { Order }
        dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Order...', 0);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET order_id = taxon_id');
        Add('WHERE zoo_taxa.rank_id = :rank_id');
        ParamByName('RANK_ID').AsInteger := iOrder;
        ExecSQL;

        { Family }
        dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Family...', dlgLoading.Progress + 1);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET family_id = zoo_taxa.taxon_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iFamily;
        ExecSQL;

        { Subfamily }
        dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Subfamily...', dlgLoading.Progress + 1);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET subfamily_id = zoo_taxa.taxon_id, family_id = parent.family_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iSubfamily;
        ExecSQL;

        { Genus }
        dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Genus...', dlgLoading.Progress + 1);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET genus_id = zoo_taxa.taxon_id, subfamily_id = parent.subfamily_id, ');
        Add('family_id = parent.family_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iGenus;
        ExecSQL;

        { Species }
        dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Species...', dlgLoading.Progress + 1);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET species_id = zoo_taxa.taxon_id, genus_id = parent.genus_id, ');
        Add('subfamily_id = parent.subfamily_id, family_id = parent.family_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iSpecies;
        ExecSQL;

        { Mono and politypic groups }
        dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Subspecies groups...', dlgLoading.Progress + 1);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET subspecies_group_id = zoo_taxa.taxon_id, species_id = parent.species_id, genus_id = parent.genus_id, ');
        Add('subfamily_id = parent.subfamily_id, family_id = parent.family_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id, species_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iMonoGroup;
        ExecSQL;
        dlgLoading.Progress := dlgLoading.Progress + 1;
        Application.ProcessMessages;
        ParamByName('RANK_ID').AsInteger := iPoliGroup;
        ExecSQL;

        { Subspecies, domestic, form }
        dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Subspecies, domestic, form...', dlgLoading.Progress + 1);
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET subspecies_group_id = parent.subspecies_group_id, species_id = parent.species_id, ' +
          'genus_id = parent.genus_id, subfamily_id = parent.subfamily_id, family_id = parent.family_id, ' +
          'order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id, species_id, ' +
          'subspecies_group_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iSubspecies;
        ExecSQL;
        dlgLoading.UpdateProgress('Finished rewriting taxa hierarchy!', dlgLoading.Progress + 1);
        Application.ProcessMessages;

        dmTaxa.sqlTrans.CommitRetaining;
      except
        dmTaxa.sqlTrans.RollbackRetaining;
        raise Exception.Create(rsErrorRewritingHierarchy);
      end;
    finally
      FreeAndNil(Qry);
    end;
  finally
    dlgLoading.Hide;
    dlgLoading.Max := 100;
  end;
end;

procedure TfrmTaxaEditor.actSspVernacularNamesExecute(Sender: TObject);
var
  Sp: Integer;
begin
  with dmTaxa.qTaxa do
  try
    First;
    DisableControls;
    dlgLoading.Show;
    dlgLoading.Max := RecordCount;
    dlgLoading.UpdateProgress('Writing subspecies'' vernacular names...', 0);
    repeat
      { #todo : Update the subspecies vernacular names rewrite routine }
      if GetRankType(FieldByName('rank_id').AsInteger) = trSubspecies then
      begin
        Sp := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                      ExtractWord(1, FieldByName('full_name').AsString, [' ']) + ' ' +
                      ExtractWord(2, FieldByName('full_name').AsString, [' ']));
        Edit;
        FieldByName('english_name').AsString := GetName('zoo_taxa', 'english_name', 'taxon_id', Sp) + ' (' +
            ExtractWord(3, FieldByName('full_name').AsString, [' ']) + ')';
        if GetName('zoo_taxa', 'portuguese_name', 'taxon_id', Sp) <> EmptyStr then
          FieldByName('portuguese_name').AsString := GetName('zoo_taxa', 'portuguese_name', 'taxon_id', Sp) + ' (' +
            ExtractWord(3, FieldByName('full_name').AsString, [' ']) + ')';
        if GetName('zoo_taxa', 'spanish_name', 'taxon_id', Sp) <> EmptyStr then
          FieldByName('spanish_name').AsString := GetName('zoo_taxa', 'spanish_name', 'taxon_id', Sp) + ' (' +
            ExtractWord(3, FieldByName('full_name').AsString, [' ']) + ')';
        Post;

      end;

      dlgLoading.Progress := RecNo;
      Application.ProcessMessages;
      Next;
    until Eof or Parar;
  finally
    First;
    EnableControls;
    dlgLoading.Hide;
    dlgLoading.Max := 100;
  end;
end;

procedure TfrmTaxaEditor.AddSortedField(aTable: TTableType; aFieldName: String; aDirection: TSortDirection;
  aCollation: String; IsAnAlias: Boolean);
var
  p: Integer;
begin
  case aTable of
    tbNone: ;
    tbTaxonRanks:
    begin
      p := FSearchRanks.SortFields.Add(TSortedField.Create);
      FSearchRanks.SortFields.Items[p].FieldName := aFieldName;
      FSearchRanks.SortFields.Items[p].Direction := aDirection;
      FSearchRanks.SortFields.Items[p].Collation := aCollation;
      FSearchRanks.SortFields.Items[p].Lookup := IsAnAlias;
    end;
    tbZooTaxa:
    begin
      p := FSearchTaxa.SortFields.Add(TSortedField.Create);
      FSearchTaxa.SortFields.Items[p].FieldName := aFieldName;
      FSearchTaxa.SortFields.Items[p].Direction := aDirection;
      FSearchTaxa.SortFields.Items[p].Collation := aCollation;
      FSearchTaxa.SortFields.Items[p].Lookup := IsAnAlias;
    end;
    tbPackages:
    begin
      p := FSearchPacks.SortFields.Add(TSortedField.Create);
      FSearchPacks.SortFields.Items[p].FieldName := aFieldName;
      FSearchPacks.SortFields.Items[p].Direction := aDirection;
      FSearchPacks.SortFields.Items[p].Collation := aCollation;
      FSearchPacks.SortFields.Items[p].Lookup := IsAnAlias;
    end;
    tbTaxonChanges: ;
    tbCountries:
    begin
      p := FSearchCountries.SortFields.Add(TSortedField.Create);
      FSearchCountries.SortFields.Items[p].FieldName := aFieldName;
      FSearchCountries.SortFields.Items[p].Direction := aDirection;
      FSearchCountries.SortFields.Items[p].Collation := aCollation;
      FSearchCountries.SortFields.Items[p].Lookup := IsAnAlias;
    end;
    tbLanguages:
    begin
      p := FSearchLanguages.SortFields.Add(TSortedField.Create);
      FSearchLanguages.SortFields.Items[p].FieldName := aFieldName;
      FSearchLanguages.SortFields.Items[p].Direction := aDirection;
      FSearchLanguages.SortFields.Items[p].Collation := aCollation;
      FSearchLanguages.SortFields.Items[p].Lookup := IsAnAlias;
    end;
    tbVernacularNames: ;
    tbSynonyms: ;
    tbTaxonCountries: ;
  end;
end;

procedure TfrmTaxaEditor.cbAuthorshipKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TfrmTaxaEditor.cbtIucnStatusDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var
  aTextStyle: TTextStyle;
begin
  aTextStyle := cbtIucnStatus.Canvas.TextStyle;
  aTextStyle.Layout := tlCenter;
  cbtIucnStatus.Canvas.FillRect(ARect);
  cbtIucnStatus.Canvas.TextStyle := aTextStyle;
  cbtIucnStatus.Canvas.TextRect(ARect, 24, ARect.Top, cbtIucnStatus.Items[Index]);
  if Index < cbtIucnStatus.Items.Count then
    iIucnStatus.DrawForControl(cbtIucnStatus.Canvas, ARect.Left + 1, ARect.Top + 1, Index, 20, cbtIucnStatus);
end;

procedure TfrmTaxaEditor.clbTaxonRanksFilterClickCheck(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  SearchTaxa(eFind.Text);
end;

procedure TfrmTaxaEditor.ClearTaxaFilters;
begin
  CanToggle := False;

  rbMarkedAll.Checked := True;
  rbExtinctAll.Checked := True;
  rbAcceptedAll.Checked := True;
  rbHasSynonymsAll.Checked := True;

  //tsTaxonomyClements.Checked := False;
  //tsTaxonomyIoc.Checked := False;
  //tsTaxonomyCbro.Checked := False;

  clbTaxonRanksFilter.CheckAll(cbUnchecked);

  CanToggle := True;
end;

procedure TfrmTaxaEditor.dbgVernacularCellClick(Column: TColumn);
begin
  // if clicked on a checkbox column, save immediatelly
  if (Column.Index = 2) then
    if (dmTaxa.qVernacular.State = dsEdit) then
      dmTaxa.qVernacular.Post;
end;

procedure TfrmTaxaEditor.dbgVernacularDblClick(Sender: TObject);
var
  Repo: TVernacularRepository;
begin
  Repo := TVernacularRepository.Create(dmTaxa.sqlCon);
  edtVernacular := TedtVernacular.Create(nil);
  with edtVernacular do
  try
    IsNew := False;
    Repo.GetById(dmTaxa.qVernacular.FieldByName('vernacular_id').AsInteger, Vernacular);
    TaxonId := dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger;
    //Vernacular.LanguageId := dmTaxa.qVernacular.FieldByName('language_id').AsInteger;
    //Vernacular.VernacularName := dmTaxa.qVernacular.FieldByName('vernacular_name').AsString;
    //Vernacular.Preferred := dmTaxa.qVernacular.FieldByName('preferred').AsBoolean;
    if ShowModal = mrOK then
    begin
      Repo.Update(Vernacular);
      dmTaxa.qVernacular.Refresh;
    end;
  finally
    FreeAndNil(edtVernacular);
    Repo.Free;
  end;
end;

procedure TfrmTaxaEditor.dsTaxaDataChange(Sender: TObject; Field: TField);
begin
  UpdateButtons;

  TimerUpdate.Enabled := False;
  TimerUpdate.Enabled := True;
end;

procedure TfrmTaxaEditor.dsTaxaStateChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmTaxaEditor.eFindChange(Sender: TObject);
begin
  sbClearFind.Visible := Length(Trim(eFind.Text)) > 0;

  TimerFind.Enabled := False;
  if canSearch then
    TimerFind.Enabled := True;
end;

procedure TfrmTaxaEditor.eFindEnter(Sender: TObject);
begin
  if eFind.Text <> EmptyStr then
    eFind.SelectAll;
end;

procedure TfrmTaxaEditor.etFullnameExit(Sender: TObject);
var
  DS: TDataSet;
  FSciName: String;

  function GetRankId(const FSciName: String): Integer;
  var
    wc: Integer;
    rankType: TZooRank;
  begin
    wc := WordCount(FSciName, [' ']);
    rankType := trGenus;

    if ExecRegExpr('.+ini$', FSciName) then
      rankType := trTribe
    else if ExecRegExpr('.+inae$', FSciName) then
      rankType := trSubfamily
    else if ExecRegExpr('.+idae$', FSciName) then
      rankType := trFamily
    else if ExecRegExpr('.+formes$', FSciName) then
      rankType := trOrder
    else if IsWordPresent('intergrade', FSciName, [' '] + Brackets) then
      rankType := trIntergrade
    else if IsWordPresent('hybrid', FSciName, [' '] + Brackets) then
      rankType := trHybrid
    else if (Pos(' x ', FSciName) > 0) then
      rankType := trHybrid
    else if ((Pos('[', FSciName) > 0) and (wc = 4)) or ((Pos('/', FSciName) > 0) and (wc = 3)) then
      rankType := trPolitypicGroup
    else if (Pos('/', FSciName) > 0) then
      rankType := trSlash
    else if IsWordPresent('sp.', FSciName, [' ']) then
      rankType := trSpuh
    else if (wc = 2) then
      rankType := trSpecies
    else if (wc = 3) then
      rankType := trSubspecies;

    Result := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[rankType]);
  end;

  procedure GetParentIds(const FSciName: String);
  var
    parentFullName: String;
  begin
    if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
    begin
      case WordCount(FSciName, [' ']) of
        2: parentFullName := ExtractWord(0, FSciName, [' ']);
        3, 4:
          if IsWordPresent('sp.', FSciName, [' ']) then
            parentFullName := ExtractWord(0, FSciName, [' '])
          else
            parentFullName := ExtractWord(0, FSciName, [' ']) + ' ' + ExtractWord(1, FSciName, [' ']);
        else
          parentFullName := EmptyStr;
      end;

      if parentFullName <> EmptyStr then
      begin
        DS.FieldByName('parent_taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', parentFullName);
        DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
      end;
    end;
  end;

begin
  DS := etFullname.DataSource.DataSet;

  if not (DS.State in [dsInsert, dsEdit]) then
    Exit;

  FSciName := Trim(DS.FieldByName('full_name').AsString);

  if not (FSciName = EmptyStr) then
  begin
    DS.FieldByName('rank_id').AsInteger := GetRankId(FSciName);

    GetParentIds(FSciName);
  end;
end;

procedure TfrmTaxaEditor.etParentTaxonButtonClick(Sender: TObject);
var
  FRank: TZooRank;
  FFilter: TTaxonFilters;
begin
  FRank := GetRankType(dmTaxa.qTaxa.FieldByName('rank_id').AsInteger);
  FFilter := [tfAll];

  case FRank of
    trSuborder,
    trInfraorder,
    trParvorder,
    trSection,
    trSubsection:     FFilter := [tfOrders];
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

  CanEdit(dmTaxa.qTaxa);
  if FindTaxonDlg(FFilter, etParentTaxon, dmTaxa.qTaxa, 'parent_taxon_id', 'parent_taxon_name', True, False) then
    SelectNext(Sender as TWinControl, True, True);
end;

procedure TfrmTaxaEditor.etParentTaxonKeyPress(Sender: TObject; var Key: char);
var
  FRank: TZooRank;
  FFilter: TTaxonFilters;
begin
  FormKeyPress(Sender, Key);

  FRank := GetRankType(dmTaxa.qTaxa.FieldByName('rank_id').AsInteger);
  FFilter := [tfAll];

  case FRank of
    trSuborder,
    trInfraorder,
    trParvorder,
    trSection,
    trSubsection:     FFilter := [tfOrders];
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
    trSubspecies:     FFilter := [tfSpecies, tfSubspeciesGroups];
    trMonotypicGroup,
    trPolitypicGroup: FFilter := [tfSpecies];
    trSpuh:           FFilter := [tfGenera];
    trDomestic:       FFilter := [tfSpecies];
    trForm,
    trHybrid,
    trIntergrade,
    trSlash:          FFilter := [tfMain];
  end;

  CanEdit(dmTaxa.qTaxa);
  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    if FindTaxonDlg(FFilter, etParentTaxon, dmTaxa.qTaxa, 'parent_taxon_id', 'parent_taxon_name', True, False, Key) then
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dmTaxa.qTaxa.FieldByName('parent_taxon_id').Clear;
    dmTaxa.qTaxa.FieldByName('parent_taxon_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TfrmTaxaEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  dmTaxa.sqlCon.CloseDataSets;
end;

procedure TfrmTaxaEditor.FormCreate(Sender: TObject);
begin
  CanToggle := False;

  dlgLoading := TdlgLoading.Create(nil);
  dlgLoading.Show;
  dlgLoading.UpdateProgress('Starting Xolmis Taxonomy Editor...', -1);

  nbTaxaSide.Visible := False;

  LoadTaxaRanks(dmTaxa.sqlCon, clbTaxonRanksFilter);
  LoadAuthorships(dmTaxa.sqlCon, cbAuthorship.Items);

  FSearchTaxa := TCustomSearch.Create(tbZooTaxa);
  FSearchTaxa.DataSet := dmTaxa.qTaxa;

  FSearchPacks := TCustomSearch.Create(tbPackages);
  FSearchPacks.DataSet := dmTaxa.qPacks;
  FSearchPacks.TableAlias := EmptyStr;

  FSearchRanks := TCustomSearch.Create(tbTaxonRanks);
  FSearchRanks.DataSet := dmTaxa.qRanks;
  FSearchRanks.TableAlias := EmptyStr;

  FSearchCountries := TCustomSearch.Create(tbCountries);
  FSearchCountries.DataSet := dmTaxa.qCountries;
  FSearchCountries.TableAlias := EmptyStr;

  FSearchLanguages := TCustomSearch.Create(tbLanguages);
  FSearchLanguages.DataSet := dmTaxa.qLanguages;
  FSearchLanguages.TableAlias := EmptyStr;
end;

procedure TfrmTaxaEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSearchTaxa);
  FreeAndNil(FSearchPacks);
  FreeAndNil(FSearchRanks);
  FreeAndNil(FSearchCountries);
  FreeAndNil(FSearchLanguages);

  if Assigned(dlgLoading) then
    FreeAndNil(dlgLoading);
end;

procedure TfrmTaxaEditor.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    { Cancel insert/edit }
    case nbPages.PageIndex of
      0:
      begin
        if (dmTaxa.qTaxa.State in [dsInsert, dsEdit]) then
        begin
          dmTaxa.qTaxa.Cancel;
        end
        else
        begin
          eFind.SetFocus;
          eFind.Clear;
        end;
      end;
      1: ;
      2:
      begin
        if (dmTaxa.qRanks.State in [dsInsert, dsEdit]) then
        begin
          dmTaxa.qRanks.Cancel;
        end
        else
        begin
          eFind.SetFocus;
          eFind.Clear;
        end;
      end;
      3:
      begin
        if (dmTaxa.qCountries.State in [dsInsert, dsEdit]) then
        begin
          dmTaxa.qCountries.Cancel;
        end
        else
        begin
          eFind.SetFocus;
          eFind.Clear;
        end;
      end;
      4:
      begin
        if (dmTaxa.qLanguages.State in [dsInsert, dsEdit]) then
        begin
          dmTaxa.qLanguages.Cancel;
        end
        else
        begin
          eFind.SetFocus;
          eFind.Clear;
        end;
      end;
    end;
  end;
end;

procedure TfrmTaxaEditor.FormShow(Sender: TObject);
begin
  //LoadTaxaRanks(dmTaxa.sqlCon, clbTaxonRanksFilter);
  //LoadAuthorships(dmTaxa.sqlCon, cbAuthorship.Items);

  //if Self.PixelsPerInch <> 96 then
  //begin
  //  navTabs.OptScalePercents := (Self.PixelsPerInch * 100) div 96;
  //end;

  if dmTaxa.sqlCon.Connected then
    UpgradeDatabaseSchema;

  TimerOpen.Enabled := True;
end;

procedure TfrmTaxaEditor.gridTaxaPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
  AState: TGridDrawState);
begin
  if (Column.FieldName = 'taxon_name') then
  begin
    if TDBGrid(Sender).Columns[4].Field.IsNull then
      Exit;

    // Italics
    if GetRankType(TDBGrid(Sender).Columns[4].Field.AsInteger) >= trSuperGenus then
      TDBGrid(Sender).Canvas.Font.Style := [fsItalic]
    else
      TDBGrid(Sender).Canvas.Font.Style := [fsBold];

    if not (gdSelected in AState) then
    begin
      // Rank colors
      case GetRankType(TDBGrid(Sender).Columns[4].Field.AsInteger) of
        trSpecies:        TDBGrid(Sender).Canvas.Font.Color := clNavy;
        trMonotypicGroup,
        trPolitypicGroup: TDBGrid(Sender).Canvas.Font.Color := clGreen;
        trForm:           TDBGrid(Sender).Canvas.Brush.Color := $00E6C6B4;
        trSpuh:           TDBGrid(Sender).Canvas.Brush.Color := $00D2AEBE;
        trHybrid:         TDBGrid(Sender).Canvas.Brush.Color := $00A5BFFF;
        trIntergrade:     TDBGrid(Sender).Canvas.Brush.Color := $0065EFFF;
        trDomestic:       TDBGrid(Sender).Canvas.Brush.Color := $00E4D9B0;
        trSlash:          TDBGrid(Sender).Canvas.Brush.Color := $009597E7;
      end;

      // Extinct
      if (TDBGrid(Sender).Columns[5].Field.AsBoolean = True) then
      begin
        TDBGrid(Sender).Canvas.Brush.Color := clBlack;
        TDBGrid(Sender).Canvas.Font.Color := clWhite;
      end;

      // Taxon not accepted
      if (TDBGrid(Sender).Columns[3].Field.AsBoolean = False) then
        TDBGrid(Sender).Canvas.Font.Color := $00646464;
    end;
  end;
end;

procedure TfrmTaxaEditor.OpenAsync;
begin
  dmTaxa.lookRanks.Open;
  dmTaxa.qTaxa.Open;
  dmTaxa.qRanks.Open;
  dmTaxa.qPacks.Open;
  dmTaxa.qTaxaChanges.Open;
  dmTaxa.qCountries.Open;
  dmTaxa.qLanguages.Open;

  UpdateButtons;

  dlgLoading.Hide;
  CanToggle := True;
  canSearch := True;
end;

procedure TfrmTaxaEditor.pmgMarkAllClick(Sender: TObject);
var
  BM: TBookMark;
begin
  with dmTaxa.qTaxa do
  begin
    BM := GetBookmark;
    DisableControls;
    try
      First;
      while not EOF do
      begin
        if FieldByName('active_status').AsBoolean = True then
        begin
          Edit;
          FieldByName('marked_status').AsBoolean := True;
          Post;
        end;
        Next;
      end;
    finally
      EnableControls;
      if BookmarkValid(BM) then
        GotoBookmark(BM);
    end;
  end;
end;

procedure TfrmTaxaEditor.pmgNewPolitypicGroupClick(Sender: TObject);
var
  GroupStr, SpName, NewName: String;
  Repo: TTaxonRepository;
  Group: TTaxon;
  Qry: TSQLQuery;
begin
  GroupStr := InputBox('New politypic group', 'Politypic group epithet:', '');

  if GroupStr <> EmptyStr then
  begin
    SpName := GetName('zoo_taxa', 'full_name', 'taxon_id', dmTaxa.qTaxa.FieldByName('species_id').AsInteger);
    NewName := SpName + ' ' + GroupStr;
    Repo := TTaxonRepository.Create(dmTaxa.sqlCon);
    Group := TTaxon.Create();
    try
      try
        Repo.FindBy('full_name', NewName, Group);
        if Group.IsNew then
        begin
          Group.FullName := NewName;
          Group.FormattedName := FormattedBirdName(NewName, GetRankKey(trPolitypicGroup));
          Group.RankId := trPolitypicGroup;
          Group.ParentTaxonId := dmTaxa.qTaxa.FieldByName('species_id').AsInteger;
          Group.Accepted := True;

          Repo.Insert(Group);
        end;

        // Move subspecies to the new group
        Qry := TSQLQuery.Create(nil);
        with Qry, SQL do
        try
          DataBase := dmTaxa.sqlCon;
          Add('UPDATE zoo_taxa SET ');
          Add('  parent_taxon_id = :parent_taxon_id,');
          Add('  update_date = datetime(''now'',''subsec''),');
          Add('  marked_status = 0');
          Add('WHERE (marked_status = 1)');
          ParamByName('parent_taxon_id').AsInteger := Group.Id;
          ExecSQL;
        finally
          FreeAndNil(Qry);
        end;

        dmTaxa.sqlTrans.CommitRetaining;
      except
        dmTaxa.sqlTrans.RollbackRetaining;
        raise;
      end;

      dmTaxa.qTaxa.Refresh;
    finally
      FreeAndNil(Group);
      Repo.Free;
    end;
  end;
end;

procedure TfrmTaxaEditor.pmgNewSubspeciesClick(Sender: TObject);
var
  NewName: String;
  Repo: TTaxonRepository;
  Ssp: TTaxon;
  BM: TBookmark;
begin
  dlgNewSubspecies := TdlgNewSubspecies.Create(nil);
  with dlgNewSubspecies do
  try
    BM := dmTaxa.qTaxa.Bookmark;
    if ShowModal = mrOK then
    begin
      Repo := TTaxonRepository.Create(dmTaxa.sqlCon);
      Ssp := TTaxon.Create();
      NewName := dmTaxa.qTaxa.FieldByName('full_name').AsString + ' ' + Epythet;
      try
        Ssp.FullName := NewName;
        Ssp.FormattedName := FormattedBirdName(NewName, GetRankKey(Rank));
        Ssp.RankId := Rank;
        Ssp.ParentTaxonId := dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger;
        Ssp.Distribution := GeographicalRange;
        Ssp.Accepted := True;

        Repo.Insert(Ssp);
      finally
        FreeAndNil(Ssp);
        Repo.Free;
      end;
    end;
  finally
    FreeAndNil(dlgNewSubspecies);
    dmTaxa.qTaxa.Refresh;
    if dmTaxa.qTaxa.BookmarkValid(BM) then
      dmTaxa.qTaxa.Bookmark := BM;
  end;
end;

procedure TfrmTaxaEditor.pmgnSetCurrentNameClick(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;
    try
      Add('UPDATE zoo_taxa_synonyms SET');
      Add('  valid_status = 0');
      //Add('  update_date = datetime(''now'',''subsec'')');
      Add('WHERE (taxon_id = :taxon_id)');
      ParamByName('taxon_id').AsInteger := dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger;
      ExecSQL;

      Clear;
      Add('UPDATE zoo_taxa_synonyms SET');
      Add('  valid_status = 1,');
      Add('  update_date = datetime(''now'',''subsec'')');
      Add('WHERE (synonym_id = :synonym_id)');
      ParamByName('synonym_id').AsInteger := dmTaxa.qSynonyms.FieldByName('synonym_id').AsInteger;
      ExecSQL;

      dmTaxa.sqlTrans.CommitRetaining;
    except
      dmTaxa.sqlTrans.RollbackRetaining;
      raise;
    end;

    dmTaxa.qSynonyms.Refresh;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxaEditor.pmgRemovePolitypicGroupClick(Sender: TObject);
var
  GroupName, toSpName: String;
  GroupId, toSpId: Integer;
  Qry: TSQLQuery;
begin
  GroupId := dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger;
  GroupName := dmTaxa.qTaxa.FieldByName('full_name').AsString;
  toSpName := ExtractWord(1, GroupName, [' ']) + ' ' + ExtractWord(2, GroupName, [' ']);
  toSpId := GetKey('zoo_taxa', 'taxon_id', 'full_name', toSpName);

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;
    try
      Add('UPDATE zoo_taxa SET');
      Add('  parent_taxon_id = :parent_taxon_id,');
      Add('  update_date = datetime(''now'',''subsec'')');
      Add('WHERE (parent_taxon_id = :group_id)');
      ParamByName('parent_taxon_id').AsInteger := toSpId;
      ParamByName('group_id').AsInteger := GroupId;
      ExecSQL;

      Clear;
      Add('UPDATE zoo_taxa SET');
      Add('  accepted_status = 0,');
      Add('  update_date = datetime(''now'',''subsec'')');
      Add('WHERE (taxon_id = :group_id)');
      ParamByName('group_id').AsInteger := GroupId;
      ExecSQL;

      dmTaxa.sqlTrans.CommitRetaining;
    except
      dmTaxa.sqlTrans.RollbackRetaining;
      raise;
    end;

    dmTaxa.qTaxa.Refresh;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxaEditor.pmgUnmarkAllClick(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;
    Add('UPDATE zoo_taxa SET marked_status = 0 WHERE (marked_status = 1)');
    ExecSQL;
    Refresh;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TfrmTaxaEditor.pmtSortClick(Sender: TObject);
begin
  FSearchTaxa.SortFields.Clear;

  SearchTaxa(eFind.Text);
end;

procedure TfrmTaxaEditor.pmvMoveToGenusClick(Sender: TObject);
var
  needRefresh: Boolean;
  Qry: TSQLQuery;
  //BM: TBookmark;
begin
  needRefresh := False;
  dlgDestTaxon := TdlgDestTaxon.Create(nil);
  with dlgDestTaxon do
  try
    //BM := dsTaxa.DataSet.Bookmark;
    dmTaxa.qTaxa.DisableControls;
    TaxonomyAction:= taMove;
    if ShowModal = mrOK then
    begin
      needRefresh := True;
      try
        case ApplyTo of
          acSelected:
          begin
            MoveToGenus(dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger, Taxon, ChangeSuffix);
          end;
          acMarked:
          begin
            dlgLoading.Show;
            dlgLoading.UpdateProgress('Moving marked taxa to genus...', 0);
            Qry := TSQLQuery.Create(dmTaxa.sqlCon);
            Qry.SQLConnection := dmTaxa.sqlCon;
            with Qry, SQL do
            try
              Add('SELECT taxon_id FROM zoo_taxa WHERE (marked_status = 1) AND (active_status = 1)');
              Open;
              dlgLoading.Max := RecordCount;
              First;
              repeat
                MoveToGenus(Qry.FieldByName('taxon_id').AsInteger, Taxon, ChangeSuffix);
                dlgLoading.Progress := RecNo;
                Next;
              until Eof;
              Close;
              Clear;
              Add('UPDATE zoo_taxa SET marked_status = 0 WHERE marked_status = 1');
              ExecSQL;
            finally
              FreeAndNil(Qry);
              dlgLoading.Hide;
              dlgLoading.Max := 100;
            end;
          end;
        end;
        dmTaxa.sqlTrans.CommitRetaining;
      except
        dmTaxa.sqlTrans.RollbackRetaining;
        raise;
      end;
    end;
  finally
    FreeAndNil(dlgDestTaxon);
    dmTaxa.qTaxa.EnableControls;
    //if dsTaxa.DataSet.BookmarkValid(BM) then
    //  dsTaxa.DataSet.Bookmark := BM;
  end;

  if needRefresh then
    dmTaxa.qTaxa.Refresh;
end;

procedure TfrmTaxaEditor.pmvMoveToSpeciesClick(Sender: TObject);
var
  needRefresh: Boolean;
  Qry: TSQLQuery;
  //BM: TBookmark;
begin
  needRefresh := False;
  dlgDestTaxon := TdlgDestTaxon.Create(nil);
  with dlgDestTaxon do
  try
    //BM := dsTaxa.DataSet.Bookmark;
    dmTaxa.qTaxa.DisableControls;
    TaxonomyAction:= taMove;
    if ShowModal = mrOK then
    begin
      needRefresh := True;
      try
        case ApplyTo of
          acSelected:
          begin
            MoveToSpecies(dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger, Taxon, ChangeSuffix);
          end;
          acMarked:
          begin
            dlgLoading.Show;
            dlgLoading.UpdateProgress('Moving marked taxa to species...', 0);
            Qry := TSQLQuery.Create(dmTaxa.sqlCon);
            Qry.SQLConnection := dmTaxa.sqlCon;
            with Qry, SQL do
            try
              Add('SELECT taxon_id FROM zoo_taxa WHERE (marked_status = 1) AND (active_status = 1)');
              Open;
              dlgLoading.Max := RecordCount;
              First;
              repeat
                MoveToSpecies(Qry.FieldByName('taxon_id').AsInteger, Taxon, ChangeSuffix);
                dlgLoading.Progress := RecNo;
                Next;
              until Eof;
              Close;
              Clear;
              Add('UPDATE zoo_taxa SET marked_status = 0 WHERE marked_status = 1');
              ExecSQL;
            finally
              FreeAndNil(Qry);
              dlgLoading.Hide;
              dlgLoading.Max := 100;
            end;
          end;
        end;
        dmTaxa.sqlTrans.CommitRetaining;
      except
        dmTaxa.sqlTrans.RollbackRetaining;
        raise;
      end;
    end;
  finally
    FreeAndNil(dlgDestTaxon);
    dmTaxa.qTaxa.EnableControls;
    //if dsTaxa.DataSet.BookmarkValid(BM) then
    //  dsTaxa.DataSet.Bookmark := BM;
  end;

  if needRefresh then
    dmTaxa.qTaxa.Refresh;
end;

procedure TfrmTaxaEditor.rbMarkedYesClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  SearchTaxa(eFind.Text);
end;

procedure TfrmTaxaEditor.sbAddSynonymClick(Sender: TObject);
var
  Repo: TSynonymRepository;
  Synonym: TSynonym;
  SynonymStr: String;
begin
  SynonymStr := InputBox('New synonym', 'Synonym:', '');

  if SynonymStr <> EmptyStr then
  begin
    Repo := TSynonymRepository.Create(dmTaxa.sqlCon);
    Synonym := TSynonym.Create();
    try
      Synonym.TaxonId := dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger;
      Synonym.FullName := SynonymStr;
      if dmTaxa.qSynonyms.RecordCount = 0 then
        Synonym.Valid := True
      else
        Synonym.Valid := False;

      Repo.Insert(Synonym);

      dmTaxa.qSynonyms.Refresh;
    finally
      FreeAndNil(Synonym);
      Repo.Free;
    end;
  end;
end;

procedure TfrmTaxaEditor.sbAddVernacularClick(Sender: TObject);
var
  Repo: TVernacularRepository;
begin
  Repo := TVernacularRepository.Create(dmTaxa.sqlCon);
  edtVernacular := TedtVernacular.Create(nil);
  with edtVernacular do
  try
    IsNew := True;
    TaxonId := dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger;
    if ShowModal = mrOK then
    begin
      Repo.Insert(Vernacular);
      dmTaxa.qVernacular.Refresh;
    end;
  finally
    FreeAndNil(edtVernacular);
    Repo.Free;
  end;
end;

procedure TfrmTaxaEditor.sbAdvancedFiltersClick(Sender: TObject);
begin
  dlgSqlFilter := TdlgSqlFilter.Create(nil);
  with dlgSqlFilter do
  try
    if ShowModal = mrOk then
    begin
      dmTaxa.qTaxa.Close;
      TSQLQuery(dmTaxa.qTaxa).SQL.Text := FilterText;
      dmTaxa.qTaxa.Open;
    end;
  finally
    FreeAndNil(dlgSqlFilter);
  end;
end;

procedure TfrmTaxaEditor.sbCancelChangeClick(Sender: TObject);
begin
  dmTaxa.qTaxaChanges.Cancel;
end;

procedure TfrmTaxaEditor.sbCancelRecordClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0:
    begin
      dmTaxa.qTaxa.Cancel;
    end;
    1:
    begin
      dmTaxa.qPacks.Cancel;
    end;
    2:
    begin
      dmTaxa.qRanks.Cancel;
    end;
    3:
    begin
      dmTaxa.qCountries.Cancel;
    end;
    4:
    begin
      dmTaxa.qLanguages.Cancel;
    end;
  end;

  UpdateButtons;
end;

procedure TfrmTaxaEditor.sbClearFiltersClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0:
    begin
      FSearchTaxa.QuickFilters.Clear;
      ClearTaxaFilters;
      SearchTaxa(FTaxaSearchString);

      UpdateButtons;
    end;
    1:
    begin

    end;
    2:
    begin

    end;
    3:
    begin

    end;
    4:
    begin

    end;
  end;
end;

procedure TfrmTaxaEditor.sbClearFindClick(Sender: TObject);
begin
  eFind.Clear;
end;

procedure TfrmTaxaEditor.sbDelChangeClick(Sender: TObject);
begin
  DeleteRecord(tbTaxonChanges, dmTaxa.qTaxaChanges);
end;

procedure TfrmTaxaEditor.sbDelCountryClick(Sender: TObject);
begin
  DeleteRecord(tbTaxonCountries, dmTaxa.qTaxonCountries);
end;

procedure TfrmTaxaEditor.sbDelRecordClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0: DeleteRecord(tbZooTaxa, dmTaxa.qTaxa);
    1: DeleteRecord(tbPackages, dmTaxa.qPacks);
    2: DeleteRecord(tbTaxonRanks, dmTaxa.qRanks);
    3: DeleteRecord(tbCountries, dmTaxa.qCountries);
    4: DeleteRecord(tbLanguages, dmTaxa.qLanguages);
  end;
end;

procedure TfrmTaxaEditor.sbDelSynonymClick(Sender: TObject);
begin
  DeleteRecord(tbSynonyms, dmTaxa.qSynonyms);
end;

procedure TfrmTaxaEditor.sbDelVernacularClick(Sender: TObject);
begin
  DeleteRecord(tbVernacularNames, dmTaxa.qVernacular);
end;

procedure TfrmTaxaEditor.sbEditChangeClick(Sender: TObject);
begin
  dmTaxa.qTaxaChanges.Edit;
end;

procedure TfrmTaxaEditor.sbEditCountryClick(Sender: TObject);
begin
  edtOccurrence := TedtOccurrence.Create(nil);
  with edtOccurrence do
  try
    TaxonId := dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger;
    TaxonName := dmTaxa.qTaxa.FieldByName('formatted_name').AsString;
    if ShowModal = mrOK then
      dmTaxa.qTaxonCountries.Refresh;
  finally
    FreeAndNil(edtOccurrence);
  end;
end;

procedure TfrmTaxaEditor.sbEditHierarchyClick(Sender: TObject);
var
  Qry, Q: TSQLQuery;
  Hierarchy: TTaxonHierarchy;
  needRefresh: Boolean;

  procedure SetHierarchyUpdateSQL(aDataSet: TSQLQuery; aHierarchy: TTaxonHierarchy);
  begin
    with aDataSet, SQL do
    begin
      Add('UPDATE zoo_taxa SET');
      if aHierarchy.ParentTaxon.Clear then
        Add('parent_taxon_id = NULL,')
      else
      if aHierarchy.ParentTaxon.Id > 0 then
        Add('parent_taxon_id = :parent_taxon_id,');

      if aHierarchy.Order.Clear then
        Add('order_id = NULL,')
      else
      if aHierarchy.Order.Id > 0 then
        Add('order_id = :order_id,');

      if aHierarchy.Family.Clear then
        Add('family_id = NULL,')
      else
      if aHierarchy.Family.Id > 0 then
        Add('family_id = :family_id,');

      if aHierarchy.Subfamily.Clear then
        Add('subfamily_id = NULL,')
      else
      if aHierarchy.Subfamily.Id > 0 then
        Add('subfamily_id = :subfamily_id,');

      if aHierarchy.Genus.Clear then
        Add('genus_id = NULL,')
      else
      if aHierarchy.Genus.Id > 0 then
        Add('genus_id = :genus_id,');

      if aHierarchy.Species.Clear then
        Add('species_id = NULL,')
      else
      if aHierarchy.Species.Id > 0 then
        Add('species_id = :species_id,');

      if aHierarchy.SubspeciesGroup.Clear then
        Add('subspecies_group_id = NULL,')
      else
      if aHierarchy.SubspeciesGroup.Id > 0 then
        Add('subspecies_group_id = :subspecies_group_id,');

      Add('update_date = datetime(''now'',''subsec'')');
      Add('WHERE (taxon_id = :taxon_id)');
    end;
  end;

  procedure SetHierarchyParams(aDataset: TSQLQuery; aHierarchy: TTaxonHierarchy);
  begin
    with aDataset do
    begin
      ParamByName('taxon_id').AsInteger := aHierarchy.TaxonId;
      if Params.FindParam('parent_taxon_id') <> nil then
        ParamByName('parent_taxon_id').AsInteger := aHierarchy.ParentTaxon.Id;
      if Params.FindParam('order_id') <> nil then
        ParamByName('order_id').AsInteger := aHierarchy.Order.Id;
      if Params.FindParam('family_id') <> nil then
        ParamByName('family_id').AsInteger := aHierarchy.Family.Id;
      if Params.FindParam('subfamily_id') <> nil then
        ParamByName('subfamily_id').AsInteger := aHierarchy.Subfamily.Id;
      if Params.FindParam('genus_id') <> nil then
        ParamByName('genus_id').AsInteger := aHierarchy.Genus.Id;
      if Params.FindParam('species_id') <> nil then
        ParamByName('species_id').AsInteger := aHierarchy.Species.Id;
      if Params.FindParam('subspecies_group_id') <> nil then
        ParamByName('subspecies_group_id').AsInteger := aHierarchy.SubspeciesGroup.Id;
    end;
  end;

begin
  needRefresh := False;

  dlgEditHierarchy := TdlgEditHierarchy.Create(nil);
  with dlgEditHierarchy do
  try
    //BM := dmTaxa.qTaxa.Bookmark;
    dmTaxa.qTaxa.DisableControls;
    CurrentTaxon := dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger;
    //ParentTaxon := dmTaxa.qTaxa.FieldByName('parent_taxon_id').AsInteger;
    //Order := dmTaxa.qTaxa.FieldByName('order_id').AsInteger;
    //Family := dmTaxa.qTaxa.FieldByName('family_id').AsInteger;
    //Subfamily := dmTaxa.qTaxa.FieldByName('subfamily_id').AsInteger;
    //Genus := dmTaxa.qTaxa.FieldByName('genus_id').AsInteger;
    //Species := dmTaxa.qTaxa.FieldByName('species_id').AsInteger;
    //SubspeciesGroup := dmTaxa.qTaxa.FieldByName('subspecies_group_id').AsInteger;

    if ShowModal = mrOK then
    begin
      needRefresh := True;
      Hierarchy.TaxonId := CurrentTaxon;
      Hierarchy.ParentTaxon.Id := ParentTaxon;
      Hierarchy.ParentTaxon.Clear := ClearParentTaxon;
      Hierarchy.Order.Id := Order;
      Hierarchy.Order.Clear := ClearOrder;
      Hierarchy.Family.Id := Family;
      Hierarchy.Family.Clear := ClearFamily;
      Hierarchy.Subfamily.Id := Subfamily;
      Hierarchy.Subfamily.Clear := ClearSubfamily;
      Hierarchy.Genus.Id := Genus;
      Hierarchy.Genus.Clear := ClearGenus;
      Hierarchy.Species.Id := Species;
      Hierarchy.Species.Clear := ClearSpecies;
      Hierarchy.SubspeciesGroup.Id := SubspeciesGroup;
      Hierarchy.SubspeciesGroup.Clear := ClearSubspeciesGroup;
      try
        case ApplyTo of
          acSelected:
          begin
            Qry := TSQLQuery.Create(dmTaxa.sqlCon);
            Qry.SQLConnection := dmTaxa.sqlCon;
            try
              SetHierarchyUpdateSQL(Qry, Hierarchy);
              SetHierarchyParams(Qry, Hierarchy);
              Qry.ExecSQL;
            finally
              FreeAndNil(Qry);
            end;
          end;
          acMarked:
          begin
            dlgLoading.Show;
            dlgLoading.UpdateProgress('Updating taxa hierarchy...', 0);
            Qry := TSQLQuery.Create(dmTaxa.sqlCon);
            Qry.SQLConnection := dmTaxa.sqlCon;
            Q := TSQLQuery.Create(dmTaxa.sqlCon);
            Q.SQLConnection := dmTaxa.sqlCon;
            with Qry, SQL do
            try
              Add('SELECT taxon_id FROM zoo_taxa WHERE (marked_status = 1) AND (active_status = 1)');
              Open;
              dlgLoading.Max := RecordCount;
              SetHierarchyUpdateSQL(Q, Hierarchy);
              First;
              repeat
                Hierarchy.TaxonId := Qry.FieldByName('taxon_id').AsInteger;
                SetHierarchyParams(Q, Hierarchy);
                Q.ExecSQL;

                dlgLoading.Progress := RecNo;
                Next;
              until Eof;
              Close;
              Clear;
              Add('UPDATE zoo_taxa SET marked_status = 0 WHERE marked_status = 1');
              ExecSQL;
            finally
              FreeAndNil(Q);
              FreeAndNil(Qry);
              dlgLoading.Hide;
              dlgLoading.Max := 100;
            end;
          end;
        end;
        dmTaxa.sqlTrans.CommitRetaining;
      except
        dmTaxa.sqlTrans.RollbackRetaining;
        raise;
      end;
    end;
  finally
    FreeAndNil(dlgEditHierarchy);
    dmTaxa.qTaxa.EnableControls;
    //if dmTaxa.qTaxa.BookmarkValid(BM) then
    //  dmTaxa.qTaxa.Bookmark := BM;
  end;

  if needRefresh then
    dmTaxa.qTaxa.Refresh;
end;

procedure TfrmTaxaEditor.sbEditRecordClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0: dmTaxa.qTaxa.Edit;
    1: dmTaxa.qPacks.Edit;
    2: dmTaxa.qRanks.Edit;
    3: dmTaxa.qCountries.Edit;
    4: dmTaxa.qLanguages.Edit;
  end;
end;

procedure TfrmTaxaEditor.sbEditSpeciesListClick(Sender: TObject);
begin
  edtSpeciesList := TedtSpeciesList.Create(nil);
  with edtSpeciesList do
  try
    ShowModal;
  finally
    FreeAndNil(edtSpeciesList);
  end;
  dmTaxa.qTaxonCountries.Refresh;
end;

procedure TfrmTaxaEditor.sbFirstRecordClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0: dmTaxa.qTaxa.First;
    1: dmTaxa.qPacks.First;
    2: dmTaxa.qRanks.First;
    3: dmTaxa.qCountries.First;
    4: dmTaxa.qLanguages.First;
  end;
end;

procedure TfrmTaxaEditor.sbInsertChangeClick(Sender: TObject);
begin
  dmTaxa.qTaxaChanges.Insert;
end;

procedure TfrmTaxaEditor.sbInsertRecordClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0: dmTaxa.qTaxa.Insert;
    1: dmTaxa.qPacks.Insert;
    2: dmTaxa.qRanks.Insert;
    3: dmTaxa.qCountries.Insert;
    4: dmTaxa.qLanguages.Insert;
  end;
end;

procedure TfrmTaxaEditor.sbLastRecordClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0: dmTaxa.qTaxa.Last;
    1: dmTaxa.qPacks.Last;
    2: dmTaxa.qRanks.Last;
    3: dmTaxa.qCountries.Last;
    4: dmTaxa.qLanguages.Last;
  end;
end;

procedure TfrmTaxaEditor.sbLumpTaxonClick(Sender: TObject);
var
  needRefresh: Boolean;
  Qry: TSQLQuery;
  //BM: TBookmark;
begin
  needRefresh := False;
  dlgDestTaxon := TdlgDestTaxon.Create(nil);
  with dlgDestTaxon do
  try
    //BM := dmTaxa.qTaxa.Bookmark;
    dmTaxa.qTaxa.DisableControls;
    TaxonomyAction:= taLump;
    if ShowModal = mrOK then
    begin
      needRefresh := True;
      try
        case ApplyTo of
          acSelected:
          begin
            LumpTaxon(dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger, Taxon);
          end;
          acMarked:
          begin
            dlgLoading.Show;
            dlgLoading.UpdateProgress('Lumping marked taxa...', 0);
            Qry := TSQLQuery.Create(dmTaxa.sqlCon);
            Qry.SQLConnection := dmTaxa.sqlCon;
            with Qry, SQL do
            try
              Add('SELECT taxon_id FROM zoo_taxa WHERE (marked_status = 1) AND (active_status = 1)');
              Open;
              dlgLoading.Max := RecordCount;
              First;
              repeat
                LumpTaxon(Qry.FieldByName('taxon_id').AsInteger, Taxon);
                dlgLoading.Progress := RecNo;
                Next;
              until Eof;
              Close;
              Clear;
              Add('UPDATE zoo_taxa SET marked_status = 0 WHERE marked_status = 1');
              ExecSQL;
            finally
              FreeAndNil(Qry);
              dlgLoading.Hide;
              dlgLoading.Max := 100;
            end;
          end;
        end;
        dmTaxa.sqlTrans.CommitRetaining;
      except
        dmTaxa.sqlTrans.RollbackRetaining;
        raise;
      end;
    end;
  finally
    FreeAndNil(dlgDestTaxon);
    dmTaxa.qTaxa.EnableControls;
    //if dmTaxa.qTaxa.BookmarkValid(BM) then
    //  dmTaxa.qTaxa.Bookmark := BM;
  end;

  if needRefresh then
    dmTaxa.qTaxa.Refresh;
end;

procedure TfrmTaxaEditor.sbMainMenuClick(Sender: TObject);
begin
  with sbMainMenu.ClientToScreen(point(0, sbMainMenu.Height + 1)) do
    pmMain.Popup(X, Y);
end;

procedure TfrmTaxaEditor.sbMoveTaxonClick(Sender: TObject);
begin
  with sbMoveTaxon.ClientToScreen(point(0, sbMoveTaxon.Height + 1)) do
    pmMove.Popup(X, Y);
end;

procedure TfrmTaxaEditor.sbNextRecordClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0: dmTaxa.qTaxa.Next;
    1: dmTaxa.qPacks.Next;
    2: dmTaxa.qRanks.Next;
    3: dmTaxa.qCountries.Next;
    4: dmTaxa.qLanguages.Next;
  end;
end;

procedure TfrmTaxaEditor.sbPriorRecordClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0: dmTaxa.qTaxa.Prior;
    1: dmTaxa.qPacks.Prior;
    2: dmTaxa.qRanks.Prior;
    3: dmTaxa.qCountries.Prior;
    4: dmTaxa.qLanguages.Prior;
  end;
end;

procedure TfrmTaxaEditor.sbRefreshRecordsClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0:
    begin
      dmTaxa.qTaxa.Refresh;
      dmTaxa.qSynonyms.Refresh;
      dmTaxa.qVernacular.Refresh;
      dmTaxa.qTaxonCountries.Refresh;
    end;
    1:
    begin
      dmTaxa.qPacks.Refresh;
      dmTaxa.qTaxaChanges.Refresh;
    end;
    2: dmTaxa.qRanks.Refresh;
    3: dmTaxa.qCountries.Refresh;
    4: dmTaxa.qLanguages.Refresh;
  end;
end;

procedure TfrmTaxaEditor.sbSaveChangeClick(Sender: TObject);
begin
  dmTaxa.qTaxaChanges.Post;
end;

procedure TfrmTaxaEditor.sbSaveRecordClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0:
    begin
      if not ValidateTaxon then
        Exit;

      dmTaxa.qTaxa.Post;
    end;
    1:
    begin
      dmTaxa.qPacks.Post;
    end;
    2:
    begin
      dmTaxa.qRanks.Post;
    end;
    3:
    begin
      dmTaxa.qCountries.Post;
    end;
    4:
    begin
      dmTaxa.qLanguages.Post;
    end;
  end;
end;

procedure TfrmTaxaEditor.sbShowQuickFiltersClick(Sender: TObject);
begin
  if TSpeedButton(Sender).Down then
  begin
    nbTaxaSide.PageIndex := TSpeedButton(Sender).Tag;
    nbTaxaSide.Visible := True;
    splitTaxaRight.Visible := True;
  end
  else
  begin
    nbTaxaSide.Visible := False;
    splitTaxaRight.Visible := False;
  end;
  pDetails.Visible := not nbTaxaSide.Visible;
end;

procedure TfrmTaxaEditor.sbSortRecordsClick(Sender: TObject);
begin
  with sbSortRecords.ClientToScreen(point(0, sbSortRecords.Height + 1)) do
    pmSortTaxa.Popup(X, Y);
end;

procedure TfrmTaxaEditor.sbSplitTaxonClick(Sender: TObject);
var
  needRefresh: Boolean;
  Qry: TSQLQuery;
  aTaxonKey: Integer;
  aTaxonName: String;
  //BM: TBookmark;
begin
  needRefresh := False;

  // Split order
  if GetRankType(dmTaxa.qTaxa.FieldByName('rank_id').AsInteger) = trOrder then
  begin
    dlgFind := TdlgFind.Create(nil);
    with dlgFind do
    try
      TextHint := 'Select the destination order';
      TableType := tbZooTaxa;
      TaxonFilter := [tfOrders];
      Position := poScreenCenter;
      if ShowModal = mrOK then
      begin
        aTaxonKey := dlgFind.KeySelected;
        aTaxonName := dlgFind.NameSelected;

        edtFamilySplit := TedtFamilySplit.Create(nil);
        with edtFamilySplit do
        try
          RankType := trOrder;
          FromTaxonId := dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger;
          ToTaxonId := aTaxonKey;
          ToTaxonName := aTaxonName;
          if ShowModal = mrOK then
            needRefresh := True;
        finally
          FreeAndNil(edtFamilySplit);
        end;
      end;
    finally
      FreeAndNil(dlgFind);
    end;
  end
  else
  // Split family
  if GetRankType(dmTaxa.qTaxa.FieldByName('rank_id').AsInteger) = trFamily then
  begin
    dlgFind := TdlgFind.Create(nil);
    with dlgFind do
    try
      TextHint := 'Select the destination family';
      TableType := tbZooTaxa;
      TaxonFilter := [tfFamilies];
      Position := poScreenCenter;
      if ShowModal = mrOK then
      begin
        aTaxonKey := dlgFind.KeySelected;
        aTaxonName := dlgFind.NameSelected;

        edtFamilySplit := TedtFamilySplit.Create(nil);
        with edtFamilySplit do
        try
          RankType := trFamily;
          FromTaxonId := dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger;
          ToTaxonId := aTaxonKey;
          ToTaxonName := aTaxonName;
          if ShowModal = mrOK then
            needRefresh := True;
        finally
          FreeAndNil(edtFamilySplit);
        end;
      end;
    finally
      FreeAndNil(dlgFind);
    end;
  end
  else
  // Split species
  begin
    dlgDestTaxon := TdlgDestTaxon.Create(nil);
    with dlgDestTaxon do
    try
      //BM := dmTaxa.qTaxa.Bookmark;
      dmTaxa.qTaxa.DisableControls;
      TaxonomyAction:= taSplit;
      if ShowModal = mrOK then
      begin
        needRefresh := True;
        try
          case ApplyTo of
            acSelected:
            begin
              SplitTaxon(dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger);
            end;
            acMarked:
            begin
              dlgLoading.Show;
              dlgLoading.UpdateProgress('Splitting marked taxa...', 0);
              Qry := TSQLQuery.Create(dmTaxa.sqlCon);
              Qry.SQLConnection := dmTaxa.sqlCon;
              with Qry, SQL do
              try
                Add('SELECT taxon_id FROM zoo_taxa WHERE (marked_status = 1) AND (active_status = 1)');
                Open;
                dlgLoading.Max := RecordCount;
                First;
                repeat
                  SplitTaxon(Qry.FieldByName('taxon_id').AsInteger);
                  dlgLoading.Progress := RecNo;
                  Next;
                until Eof;
                Close;
                Clear;
                Add('UPDATE zoo_taxa SET marked_status = 0 WHERE marked_status = 1');
                ExecSQL;
              finally
                FreeAndNil(Qry);
                dlgLoading.Hide;
                dlgLoading.Max := 100;
              end;
            end;
          end;
          dmTaxa.sqlTrans.CommitRetaining;
        except
          dmTaxa.sqlTrans.RollbackRetaining;
          raise;
        end;
      end;
    finally
      FreeAndNil(dlgDestTaxon);
      dmTaxa.qTaxa.EnableControls;
      //if dmTaxa.qTaxa.BookmarkValid(BM) then
      //  dmTaxa.qTaxa.Bookmark := BM;
    end;
  end;

  if needRefresh then
    dmTaxa.qTaxa.Refresh;
end;

procedure TfrmTaxaEditor.sbTaxaClick(Sender: TObject);
begin
  if Sender is TSpeedButton then
    nbPages.PageIndex := TSpeedButton(Sender).Tag;

  canSearch := False;
  case nbPages.PageIndex of
    0: eFind.Text := FTaxaSearchString;
    1: eFind.Text := FPackageSearchString;
    2: eFind.Text := FRankSearchString;
    3: eFind.Text := FCountrySearchString;
    4: eFind.Text := FLanguageSearchString;
  end;
  canSearch := True;

  UpdateButtons;
end;

function TfrmTaxaEditor.SearchCountries(aValue: String): Boolean;
var
  FCriteria: TCriteriaType;
  aGroup: Integer;
  dummyInt: Longint;
begin
  Result := False;

  if Working then
    Exit;

  Working := True;
  FSearchCountries.Fields.Clear;
  FSearchCountries.QuickFilters.Clear;

  FCriteria := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      FCriteria := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      FCriteria := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, dummyInt) then
    begin
      aGroup := FSearchCountries.Fields.Add(TSearchGroup.Create);
      FSearchCountries.Fields[aGroup].Fields.Add(TSearchField.Create('country_id', 'Country (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      aGroup := FSearchCountries.Fields.Add(TSearchGroup.Create);
      FSearchCountries.Fields[aGroup].Fields.Add(TSearchField.Create('country_name', 'Name', sdtText, FCriteria,
        False, aValue));
      FSearchCountries.Fields[aGroup].Fields.Add(TSearchField.Create('country_code', 'Code', sdtText, FCriteria,
        False, aValue));
    end;
  end;

  //GetTaxaFilters;

  FSearchCountries.SortFields.Clear;
  AddSortedField(tbCountries, 'country_name', sdAscending);

  Result := FSearchCountries.RunSearch > 0;

  Working := False;

  UpdateButtons;
end;

function TfrmTaxaEditor.SearchLanguages(aValue: String): Boolean;
var
  FCriteria: TCriteriaType;
  aGroup: Integer;
  dummyInt: Longint;
begin
  Result := False;

  if Working then
    Exit;

  Working := True;
  FSearchLanguages.Fields.Clear;
  FSearchLanguages.QuickFilters.Clear;

  FCriteria := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      FCriteria := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      FCriteria := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, dummyInt) then
    begin
      aGroup := FSearchLanguages.Fields.Add(TSearchGroup.Create);
      FSearchLanguages.Fields[aGroup].Fields.Add(TSearchField.Create('language_id', 'Language (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      aGroup := FSearchLanguages.Fields.Add(TSearchGroup.Create);
      FSearchLanguages.Fields[aGroup].Fields.Add(TSearchField.Create('language_name', 'Name', sdtText, FCriteria,
        False, aValue));
      FSearchLanguages.Fields[aGroup].Fields.Add(TSearchField.Create('macrolanguage_code', 'Macrolanguage code', sdtText, FCriteria,
        False, aValue));
      FSearchLanguages.Fields[aGroup].Fields.Add(TSearchField.Create('country_code', 'Country code', sdtText, FCriteria,
        False, aValue));
      FSearchLanguages.Fields[aGroup].Fields.Add(TSearchField.Create('variation_code', 'Variant code', sdtText, FCriteria,
        False, aValue));
    end;
  end;

  //GetTaxaFilters;

  FSearchLanguages.SortFields.Clear;
  AddSortedField(tbLanguages, 'language_name', sdAscending);

  Result := FSearchLanguages.RunSearch > 0;

  Working := False;

  UpdateButtons;
end;

function TfrmTaxaEditor.SearchPackages(aValue: String): Boolean;
var
  FCriteria: TCriteriaType;
  aGroup: Integer;
  dummyInt: Longint;
begin
  Result := False;

  if Working then
    Exit;

  Working := True;
  FSearchPacks.Fields.Clear;
  FSearchPacks.QuickFilters.Clear;

  FCriteria := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      FCriteria := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      FCriteria := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, dummyInt) then
    begin
      aGroup := FSearchPacks.Fields.Add(TSearchGroup.Create);
      FSearchPacks.Fields[aGroup].Fields.Add(TSearchField.Create('package_id', 'Package (ID)', sdtInteger, crEqual,
        False, aValue));
      FSearchPacks.Fields[aGroup].Fields.Add(TSearchField.Create('package_year', 'Year', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      aGroup := FSearchPacks.Fields.Add(TSearchGroup.Create);
      FSearchPacks.Fields[aGroup].Fields.Add(TSearchField.Create('package_name', 'Name', sdtText, FCriteria,
        False, aValue));
    end;
  end;

  //GetTaxaFilters;

  FSearchPacks.SortFields.Clear;
  AddSortedField(tbPackages, 'package_name', sdAscending);

  Result := FSearchPacks.RunSearch > 0;

  Working := False;

  UpdateButtons;
end;

function TfrmTaxaEditor.SearchRanks(aValue: String): Boolean;
var
  FCriteria: TCriteriaType;
  aGroup: Integer;
  dummyInt: Longint;
begin
  Result := False;

  if Working then
    Exit;

  Working := True;
  FSearchRanks.Fields.Clear;
  FSearchRanks.QuickFilters.Clear;

  FCriteria := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      FCriteria := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      FCriteria := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, dummyInt) then
    begin
      aGroup := FSearchRanks.Fields.Add(TSearchGroup.Create);
      FSearchRanks.Fields[aGroup].Fields.Add(TSearchField.Create('rank_id', 'Rank (ID)', sdtInteger, crEqual,
        False, aValue));
      FSearchRanks.Fields[aGroup].Fields.Add(TSearchField.Create('rank_seq', 'Sequence', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      aGroup := FSearchRanks.Fields.Add(TSearchGroup.Create);
      FSearchRanks.Fields[aGroup].Fields.Add(TSearchField.Create('rank_name', 'Name', sdtText, FCriteria,
        False, aValue));
      FSearchRanks.Fields[aGroup].Fields.Add(TSearchField.Create('rank_acronym', 'Abbreviation', sdtText, FCriteria,
        False, aValue));
    end;
  end;

  //GetTaxaFilters;

  FSearchRanks.SortFields.Clear;
  AddSortedField(tbTaxonRanks, 'rank_seq', sdAscending);

  Result := FSearchRanks.RunSearch > 0;

  Working := False;

  UpdateButtons;
end;

procedure TfrmTaxaEditor.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;
  if not canSearch then
    Exit;
  //if not CanToggle then
  //  Exit;

  case nbPages.PageIndex of
    0:
    begin
      FTaxaSearchString := eFind.Text;
      SearchTaxa(FTaxaSearchString);
    end;
    1:
    begin
      FPackageSearchString := eFind.Text;
      SearchPackages(FPackageSearchString);
    end;
    2:
    begin
      FRankSearchString := eFind.Text;
      SearchRanks(FRankSearchString);
    end;
    3:
    begin
      FCountrySearchString := eFind.Text;
      SearchCountries(FCountrySearchString);
    end;
    4:
    begin
      FLanguageSearchString := eFind.Text;
      SearchLanguages(FLanguageSearchString);
    end;
  end;
end;

procedure TfrmTaxaEditor.TimerOpenTimer(Sender: TObject);
begin
  TimerOpen.Enabled := False;

  OpenAsync;
end;

procedure TfrmTaxaEditor.TimerUpdateTimer(Sender: TObject);
begin
  TimerUpdate.Enabled := False;

  UpdateTree;
end;

procedure TfrmTaxaEditor.tsTaxonExtinctOff(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  SearchTaxa(eFind.Text);
end;

procedure TfrmTaxaEditor.tsTaxonExtinctOn(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  SearchTaxa(eFind.Text);
end;

procedure TfrmTaxaEditor.GetTaxaFilters;
var
  sf, cc, i: Integer;
begin
  if not CanToggle then
    Exit;

  CanToggle := False;

  if (rbMarkedYes.Checked) then
  begin
    sf := FSearchTaxa.QuickFilters.Add(TSearchGroup.Create);
    FSearchTaxa.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean,
      crEqual, False, '1'));
  end
  else
  if (rbMarkedNo.Checked) then
  begin
    sf := FSearchTaxa.QuickFilters.Add(TSearchGroup.Create);
    FSearchTaxa.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean,
      crEqual, False, '0'));
  end;

  cc := 0;
  for i := 0 to clbTaxonRanksFilter.Count - 1 do
    if clbTaxonRanksFilter.Checked[i] then
      Inc(cc);
  if cc > 0 then
  begin
    sf := FSearchTaxa.QuickFilters.Add(TSearchGroup.Create);
    for i := 0 to clbTaxonRanksFilter.Count - 1 do
      if clbTaxonRanksFilter.Checked[i] then
        FSearchTaxa.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('rank_id', 'Rank', sdtInteger,
          crEqual, False, IntToStr(GetKey('taxon_ranks', 'rank_id', 'rank_name', clbTaxonRanksFilter.Items[i]))));
  end;

  //if tsTaxonomyClements.Checked then
  //begin
  //  sf := FSearchTaxa.QuickFilters.Add(TSearchGroup.Create);
  //  FSearchTaxa.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('clements_taxonomy', 'Clements/eBird', sdtBoolean,
  //    crEqual, False, '1'));
  //end;
  //if tsTaxonomyIoc.Checked then
  //begin
  //  sf := FSearchTaxa.QuickFilters.Add(TSearchGroup.Create);
  //  FSearchTaxa.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('ioc_taxonomy', 'IOC', sdtBoolean,
  //    crEqual, False, '1'));
  //end;
  //if tsTaxonomyCbro.Checked then
  //begin
  //  sf := FSearchTaxa.QuickFilters.Add(TSearchGroup.Create);
  //  FSearchTaxa.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('cbro_taxonomy', 'CBRO', sdtBoolean,
  //    crEqual, False, '1'));
  //end;

  if rbExtinctYes.Checked then
  begin
    sf := FSearchTaxa.QuickFilters.Add(TSearchGroup.Create);
    FSearchTaxa.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('extinct', 'Extinct', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbExtinctNo.Checked then
  begin
    sf := FSearchTaxa.QuickFilters.Add(TSearchGroup.Create);
    FSearchTaxa.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('extinct', 'Extinct', sdtBoolean,
      crEqual, False, '0'));
  end;

  if rbAcceptedYes.Checked then
  begin
    sf := FSearchTaxa.QuickFilters.Add(TSearchGroup.Create);
    FSearchTaxa.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('accepted_status', 'Accepted', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbAcceptedNo.Checked then
  begin
    sf := FSearchTaxa.QuickFilters.Add(TSearchGroup.Create);
    FSearchTaxa.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('accepted_status', 'Accepted', sdtBoolean,
      crEqual, False, '0'));
  end;
  //if tsHasSynonyms.StateOn = sw_on then
  //begin
  //  sf := FSearchTaxa.QuickFilters.Add(TSearchGroup.Create);
  //  FSearchTaxa.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
  //    crMoreThan, False, '1'));
  //end;

  CanToggle := True;
end;

procedure TfrmTaxaEditor.gridSynonymsPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
  AState: TGridDrawState);
begin
  // Valid name
  if (TDBGrid(Sender).Columns[1].Field.AsBoolean = True) then
    TDBGrid(Sender).Canvas.Font.Style := [fsBold];
end;

procedure TfrmTaxaEditor.gridTaxaCellClick(Column: TColumn);
begin
  // if clicked on a checkbox column, save immediatelly
  if (Column.Index = 0) then
    if (dmTaxa.qTaxa.State = dsEdit) then
      dmTaxa.qTaxa.Post;
end;

procedure TfrmTaxaEditor.gridTaxaDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
begin
  //if Column.FieldName = 'formatted_name' then
  //begin
  //  HtmlView.Width := Column.Width;
  //  HtmlView.LoadFromString(Column.Field.AsString);
  //  HtmlView.PaintTo(gridTaxa.Canvas.Handle, Rect.Left, Rect.Top);
  //end
  //else
  //begin
  //  gridTaxa.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  //end;
end;

procedure TfrmTaxaEditor.gridTaxaMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);

  function GetNumScrollLines: Integer;
  begin
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @Result, 0);
  end;

var
  Direction: Shortint;
begin
  Direction := 1;
  if WheelDelta = 0 then
    Exit
  else if WheelDelta > 0 then
    Direction := -1;

  with TDBGrid(Sender) do
  begin
    if Assigned(DataSource) and Assigned(DataSource.DataSet) then
      DataSource.DataSet.MoveBy(Direction * GetNumScrollLines);
    Invalidate;
  end;
end;

function TfrmTaxaEditor.SearchTaxa(aValue: String): Boolean;
var
  FCriteria: TCriteriaType;
  aGroup: Integer;
  dummyInt: Longint;
begin
  Result := False;

  if Working then
    Exit;

  Working := True;
  FSearchTaxa.Fields.Clear;
  FSearchTaxa.QuickFilters.Clear;

  FCriteria := crLike;
  aValue := Trim(aValue);

  if aValue <> EmptyStr then
  begin
    if ExecRegExpr('^=.+$', aValue) then
    begin
      FCriteria := crEqual;
      aValue := StringReplace(aValue, '=', '', [rfReplaceAll]);
    end
    else
    if ExecRegExpr('^:.+$', aValue) then
    begin
      FCriteria := crStartLike;
      aValue := StringReplace(aValue, ':', '', [rfReplaceAll]);
    end;

    if TryStrToInt(aValue, dummyInt) then
    begin
      aGroup := FSearchTaxa.Fields.Add(TSearchGroup.Create);
      FSearchTaxa.Fields[aGroup].Fields.Add(TSearchField.Create('taxon_id', 'Taxon (ID)', sdtInteger, crEqual,
        False, aValue));
    end
    else
    begin
      aGroup := FSearchTaxa.Fields.Add(TSearchGroup.Create);
      FSearchTaxa.Fields[aGroup].Fields.Add(TSearchField.Create('taxon_name', 'Scientific name', sdtText, FCriteria,
        True, aValue));
      FSearchTaxa.Fields[aGroup].Fields.Add(TSearchField.Create('ebird_code', 'eBird code', sdtText, FCriteria,
        False, aValue));
      FSearchTaxa.Fields[aGroup].Fields.Add(TSearchField.Create('quick_code', 'Quick code', sdtText, FCriteria,
        False, aValue));
    end;
  end;

  GetTaxaFilters;

  FSearchTaxa.SortFields.Clear;
  if pmtSortTaxonomic.Checked then
    AddSortedField(tbZooTaxa, 'sort_num', sdAscending)
  else
  if pmtSortAlphabetical.Checked then
    AddSortedField(tbZooTaxa, 'taxon_name', sdAscending, '', True);

  Result := FSearchTaxa.RunSearch > 0;

  Working := False;

  UpdateButtons;
end;

procedure TfrmTaxaEditor.UpdateButtons;
var
  aDataSet: TSQLQuery;
begin
  aDataSet := nil;
  case nbPages.PageIndex of
    0: aDataSet := dmTaxa.qTaxa;
    1: aDataSet := dmTaxa.qPacks;
    2: aDataSet := dmTaxa.qRanks;
    3: aDataSet := dmTaxa.qCountries;
    4: aDataSet := dmTaxa.qLanguages;
  end;

  case aDataSet.State of
    dsInactive:
    begin
      sbInsertRecord.Enabled := False;
      sbEditRecord.Enabled := False;
      sbDelRecord.Enabled := False;

      sbFirstRecord.Enabled := False;
      sbPriorRecord.Enabled := False;
      sbNextRecord.Enabled := False;
      sbLastRecord.Enabled := False;

      sbRefreshRecords.Enabled := False;
      sbClearFilters.Enabled := False;
      sbMoreOptions.Enabled := False;

      if (nbPages.PageIndex = 0) then
      begin
        sbSortRecords.Enabled := False;
        sbAdvancedFilters.Enabled := False;
        sbSplitTaxon.Enabled := False;
        sbLumpTaxon.Enabled := False;
        sbMoveTaxon.Enabled := False;
        sbShowQuickFilters.Enabled := True;
        sbShowImages.Enabled := True;
        sbShowRecycle.Enabled := True;
      end;

      sbCancelRecord.Visible := False;
      sbSaveRecord.Visible := False;

      //gridTaxa.Enabled := False;
      //pFindTaxa.Enabled := False;
      //pTaxaRightBar.Enabled := True;
      //sbFileMenu.Enabled := True;
      //navTabs.Enabled := True;
    end;
    dsBrowse:
    begin
      sbInsertRecord.Enabled := True;
      sbEditRecord.Enabled := (aDataSet.RecordCount > 0);
      sbDelRecord.Enabled := (aDataSet.RecordCount > 0);

      sbFirstRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
      sbPriorRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
      sbNextRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);
      sbLastRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);

      sbRefreshRecords.Enabled := True;
      sbClearFilters.Enabled := FSearchTaxa.QuickFilters.Count > 0;
      sbMoreOptions.Enabled := True;

      if (nbPages.PageIndex = 0) then
      begin
        sbSortRecords.Enabled := True;
        sbAdvancedFilters.Enabled := True;
        sbSplitTaxon.Enabled := (aDataSet.RecordCount > 0);
        sbLumpTaxon.Enabled := (aDataSet.RecordCount > 0);
        sbMoveTaxon.Enabled := (aDataSet.RecordCount > 0);
        sbShowQuickFilters.Enabled := True;
        sbShowImages.Enabled := True;
        sbShowRecycle.Enabled := True;
      end;

      sbSaveRecord.Visible := False;
      sbCancelRecord.Visible := False;

      //gridTaxa.Enabled := True;
      //pFindTaxa.Enabled := True;
      //pTaxaRightBar.Enabled := True;
      //sbFileMenu.Enabled := True;
      //navTabs.Enabled := True;
    end;
    dsEdit, dsInsert:
    begin
      sbInsertRecord.Enabled := False;
      sbEditRecord.Enabled := False;
      sbDelRecord.Enabled := False;

      sbFirstRecord.Enabled := False;
      sbPriorRecord.Enabled := False;
      sbNextRecord.Enabled := False;
      sbLastRecord.Enabled := False;

      sbRefreshRecords.Enabled := False;
      sbClearFilters.Enabled := False;
      sbMoreOptions.Enabled := False;

      if (nbPages.PageIndex = 0) then
      begin
        sbSortRecords.Enabled := False;
        sbAdvancedFilters.Enabled := False;
        sbSplitTaxon.Enabled := False;
        sbLumpTaxon.Enabled := False;
        sbMoveTaxon.Enabled := False;
        sbShowQuickFilters.Enabled := False;
        sbShowImages.Enabled := False;
        sbShowRecycle.Enabled := False;
      end;

      sbCancelRecord.Visible := True;
      sbSaveRecord.Visible := True;

      //gridTaxa.Enabled := False;
      //pFindTaxa.Enabled := False;
      //pTaxaRightBar.Enabled := False;
      //sbFileMenu.Enabled := False;
      //navTabs.Enabled := False;
    end;
  end;

  sbEditSpeciesList.Visible := (nbPages.PageIndex = 3);
  sbSortRecords.Visible := (nbPages.PageIndex = 0);
  sbAdvancedFilters.Visible := (nbPages.PageIndex = 0);

  pmgRefresh.Enabled := sbRefreshRecords.Enabled;
  pmgEdit.Enabled := sbEditRecord.Enabled;
  pmgDel.Enabled := sbDelRecord.Enabled;

  pmgNewSubspecies.Enabled := sbSplitTaxon.Enabled;
  pmgSplit.Enabled := sbSplitTaxon.Enabled;
  pmgLump.Enabled := sbLumpTaxon.Enabled;
  pmgMove.Enabled := sbMoveTaxon.Enabled;

  if (nbPages.PageIndex = 0) then
  begin
    if dmTaxa.qTaxa.RecordCount > 0 then
      lblCountTaxa.Caption := Format(rsRecordNumber, [dmTaxa.qTaxa.RecNo, dmTaxa.qTaxa.RecordCount])
    else
      lblCountTaxa.Caption := rsRecNoEmpty;
    lblTitleSynonyms.Caption := Format('Synonyms (%d)', [dmTaxa.qSynonyms.RecordCount]);
    //lblTitleChilds.Caption := Format('ChildTaxa (%d)', [dmTaxa.qChildTaxa.RecordCount]);
  end;
end;

procedure TfrmTaxaEditor.UpdateTree;
var
  node, parentNode: TTreeNode;
  Qry: TSQLQuery;
begin
  tvHierarchy.Items.Clear;

  if not (dsTaxa.DataSet.Active) or (dmTaxa.qTaxa.RecordCount = 0) then
    Exit;

  if not (dsTaxa.State in [dsInsert, dsEdit]) then
  begin
    if dmTaxa.qTaxa.FieldByName('order_id').AsInteger > 0 then
    begin
      node := tvHierarchy.Items.Add(nil, GetName('zoo_taxa', 'full_name', 'taxon_id', dmTaxa.qTaxa.FieldByName('order_id').AsInteger));
      if dmTaxa.qTaxa.FieldByName('family_id').AsInteger > 0 then
      begin
        parentNode := node;
        node := tvHierarchy.Items.AddChild(parentNode, GetName('zoo_taxa', 'full_name', 'taxon_id', dmTaxa.qTaxa.FieldByName('family_id').AsInteger));
        if dmTaxa.qTaxa.FieldByName('genus_id').AsInteger > 0 then
        begin
          parentNode := node;
          node := tvHierarchy.Items.AddChild(parentNode, GetName('zoo_taxa', 'full_name', 'taxon_id', dmTaxa.qTaxa.FieldByName('genus_id').AsInteger));
          if dmTaxa.qTaxa.FieldByName('species_id').AsInteger > 0 then
          begin
            parentNode := node;
            node := tvHierarchy.Items.AddChild(parentNode, GetName('zoo_taxa', 'full_name', 'taxon_id', dmTaxa.qTaxa.FieldByName('species_id').AsInteger));
            if dmTaxa.qTaxa.FieldByName('subspecies_group_id').AsInteger > 0 then
            begin
              parentNode := node;
              node := tvHierarchy.Items.AddChild(parentNode, GetName('zoo_taxa', 'full_name', 'taxon_id', dmTaxa.qTaxa.FieldByName('subspecies_group_id').AsInteger));
            end;
          end;
        end;
      end;
    end;
    Qry := TSQLQuery.Create(nil);
    with Qry, SQL do
    try
      DataBase := dmTaxa.sqlCon;
      Add('SELECT full_name FROM zoo_taxa');
      Add('WHERE (parent_taxon_id = :taxon_id) AND (accepted_status = 1) AND (active_status = 1)');
      Add('ORDER BY full_name ASC');
      ParamByName('taxon_id').AsInteger := dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger;
      Open;
      if (RecordCount > 0) then
      begin
        parentNode := node;
        First;
        while not EOF do
        begin
          tvHierarchy.Items.AddChild(parentNode, FieldByName('full_name').AsString);
          Next;
        end;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
    tvHierarchy.FullExpand;
  end;
end;

function TfrmTaxaEditor.ValidateTaxon: Boolean;
begin
  Result := True;

  //Result := RecordDuplicated(tbZooTaxa, 'taxon_id', 'full_name',
  //            dmTaxa.qTaxa.FieldByName('full_name').AsString,
  //            dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger);
end;

end.

