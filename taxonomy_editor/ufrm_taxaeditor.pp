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
    actSspVernacularNames: TAction;
    actRewriteHierarchy: TAction;
    actList: TActionList;
    AppProperties: TApplicationProperties;
    bMenu: TImageList;
    cbtRank: TDBLookupComboBox;
    cktExtinct: TDBCheckBox;
    cbtIucnStatus: TDBComboBox;
    cbAuthorship: TDBComboBox;
    DBCheckBox1: TDBCheckBox;
    dbgVernacular: TDBGrid;
    dbgCountries: TDBGrid;
    eFind: TEdit;
    gridChildTaxa: TDBGrid;
    gridCountries: TDBGrid;
    gridLanguages: TDBGrid;
    gridSynonyms: TDBGrid;
    etFullname: TDBEdit;
    etEbirdCode: TDBEdit;
    etExtinctionYear: TDBEdit;
    etParentTaxon: TDBEditButton;
    etQuickcode: TDBEdit;
    etSortNr: TDBEdit;
    gridPacks: TDBGrid;
    gridChanges: TDBGrid;
    gridRanks: TDBGrid;
    HtmlView: THtmlViewer;
    icoAcceptedFilter: TImage;
    icoMarkedFilter: TImage;
    iconFind: TImage;
    iIucnStatus: TImageList;
    bNavigation: TImageList;
    lblAcceptedFilter: TLabel;
    lblMarkedFilter: TLabel;
    lbltVernacular: TLabel;
    lbltCountries: TLabel;
    lblTitleSynonyms: TLabel;
    lblCountTaxa: TLabel;
    lbltAuthorship: TLabel;
    lbltDistribution: TLabel;
    lbltEbirdCode: TLabel;
    lbltFullname: TLabel;
    lblTitleChilds: TLabel;
    lbltParentTaxon: TLabel;
    lbltQuickCode: TLabel;
    lbltIucnStatus: TLabel;
    lbltRank: TLabel;
    lbltSortNr: TLabel;
    mmSspVernacularNames: TMenuItem;
    mmRewriteHierarchy: TMenuItem;
    pAcceptedFilter: TBCPanel;
    pFind: TBCPanel;
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
    ptAuthorship: TBCPanel;
    pToolbar: TBCPanel;
    ptVernacular: TBCPanel;
    ptCountries: TBCPanel;
    ptFullName: TBCPanel;
    ptParentTaxon: TBCPanel;
    ptIucnStatus: TBCPanel;
    ptRank: TBCPanel;
    ptToolbar: TPanel;
    pmtSortTaxonomic: TMenuItem;
    pmtSortAlphabetical: TMenuItem;
    pmvMoveToSpecies: TMenuItem;
    pmvMoveToGenus: TMenuItem;
    pmvMoveToFamily: TMenuItem;
    pmvMoveToOrder: TMenuItem;
    mtDistribution: TDBMemo;
    peTaxa: TPanel;
    pmMove: TPopupMenu;
    pmSortTaxa: TPopupMenu;
    pPacksList: TBCPanel;
    ptContent: TBCPanel;
    gridTaxa: TDBGrid;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    ptDistribution: TBCPanel;
    ptEbirdCode: TBCPanel;
    ptExtinct: TBCPanel;
    ptQuickCode: TBCPanel;
    ptSortNr: TBCPanel;
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
    sbAdvancedFilters: TSpeedButton;
    sbCancelRecord: TSpeedButton;
    sbClearFilters: TSpeedButton;
    sbClearFind: TColorSpeedButton;
    sbDelRecord: TSpeedButton;
    sbExport: TSpeedButton;
    sbEditRecord: TSpeedButton;
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
    sbSortRecords: TSpeedButton;
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
    pTaxaList: TBCPanel;
    pTaxaRightBar: TBCPanel;
    pTaxonRanksFilters: TBCPanel;
    pTitleTaxonRanksFilter: TPanel;
    sboxTaxaFilters: TScrollBox;
    sbShowImages: TSpeedButton;
    sbShowQuickFilters: TSpeedButton;
    sbShowRecycle: TSpeedButton;
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
    sbDelCountry: TSpeedButton;
    sbAddCountry: TSpeedButton;
    sbDelVernacular: TSpeedButton;
    sbAddVernacular: TSpeedButton;
    sbDelSynonym: TSpeedButton;
    sbAddSynonym: TSpeedButton;
    splitTaxaLeft: TSplitter;
    splitPacksLeft: TSplitter;
    splitTaxaRight: TSplitter;
    TimerOpen: TTimer;
    TimerFind: TTimer;
    tvHierarchy: TTreeView;
    procedure actAboutExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actFormatSciNamesExecute(Sender: TObject);
    procedure actImportClementsExecute(Sender: TObject);
    procedure actImportIOCNamesExecute(Sender: TObject);
    procedure actRewriteHierarchyExecute(Sender: TObject);
    procedure actSspVernacularNamesExecute(Sender: TObject);
    procedure cbtIucnStatusDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure clbTaxonRanksFilterClickCheck(Sender: TObject);
    procedure dsTaxaDataChange(Sender: TObject; Field: TField);
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
    procedure gridTaxaDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    procedure gridTaxaMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure gridTaxaPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn; AState: TGridDrawState);
    procedure pmgNewSubspeciesClick(Sender: TObject);
    procedure pmtSortClick(Sender: TObject);
    procedure pmvMoveToGenusClick(Sender: TObject);
    procedure pmvMoveToSpeciesClick(Sender: TObject);
    procedure rbMarkedYesClick(Sender: TObject);
    procedure sbAdvancedFiltersClick(Sender: TObject);
    procedure sbCancelRecordClick(Sender: TObject);
    procedure sbClearFiltersClick(Sender: TObject);
    procedure sbClearFindClick(Sender: TObject);
    procedure sbDelRecordClick(Sender: TObject);
    procedure sbEditRecordClick(Sender: TObject);
    procedure sbFirstRecordClick(Sender: TObject);
    procedure sbInsertRecordClick(Sender: TObject);
    procedure sbLastRecordClick(Sender: TObject);
    procedure sbLumpTaxonClick(Sender: TObject);
    procedure sbMainMenuClick(Sender: TObject);
    procedure sbMoveTaxonClick(Sender: TObject);
    procedure sbNextRecordClick(Sender: TObject);
    procedure sbPriorRecordClick(Sender: TObject);
    procedure sbRefreshRecordsClick(Sender: TObject);
    procedure sbSaveRecordClick(Sender: TObject);
    procedure sbShowQuickFiltersClick(Sender: TObject);
    procedure sbSortRecordsClick(Sender: TObject);
    procedure sbSplitTaxonClick(Sender: TObject);
    procedure sbTaxaClick(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure TimerOpenTimer(Sender: TObject);
    procedure tsTaxonExtinctOff(Sender: TObject);
    procedure tsTaxonExtinctOn(Sender: TObject);
  private
    FSearch: TCustomSearch;
    FTaxaSearchString, FPackageSearchString,
      FRankSearchString, FCountrySearchString, FLanguageSearchString: String;
    CanToggle: Boolean;
    Working: Boolean;
    procedure AddSortedField(aFieldName: String; aDirection: TSortDirection; aCollation: String = '';
      IsAnAlias: Boolean = False);
    procedure ClearTaxaFilters;
    procedure GetTaxaFilters;
    procedure OpenAsync;
    function SearchTaxa(aValue: String): Boolean;
    procedure UpdateButtons;
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
  udm_taxa, udlg_about, udlg_loading, udlg_desttaxon, udlg_edithierarchy, udlg_newsubspecies, udlg_sqlfilter;

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

procedure TfrmTaxaEditor.AddSortedField(aFieldName: String; aDirection: TSortDirection; aCollation: String;
  IsAnAlias: Boolean);
var
  p: Integer;
begin
  p := FSearch.SortFields.Add(TSortedField.Create);
  FSearch.SortFields.Items[p].FieldName := aFieldName;
  FSearch.SortFields.Items[p].Direction := aDirection;
  FSearch.SortFields.Items[p].Collation := aCollation;
  FSearch.SortFields.Items[p].Lookup := IsAnAlias;
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
  if Index < cbtIucnStatus.Items.Count - 1 then
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

procedure TfrmTaxaEditor.dsTaxaDataChange(Sender: TObject; Field: TField);
var
  nOrder, nFamily, nGenus, nSpecies, nGroup: TTreeNode;
begin
  UpdateButtons;

  tvHierarchy.Items.Clear;
  if dmTaxa.qTaxa.FieldByName('order_id').AsInteger > 0 then
  begin
    nOrder := tvHierarchy.Items.Add(nil, GetName('zoo_taxa', 'full_name', 'taxon_id', dmTaxa.qTaxa.FieldByName('order_id').AsInteger));
    if dmTaxa.qTaxa.FieldByName('family_id').AsInteger > 0 then
    begin
      nFamily := tvHierarchy.Items.AddChild(nOrder, GetName('zoo_taxa', 'full_name', 'taxon_id', dmTaxa.qTaxa.FieldByName('family_id').AsInteger));
      if dmTaxa.qTaxa.FieldByName('genus_id').AsInteger > 0 then
      begin
        nGenus := tvHierarchy.Items.AddChild(nFamily, GetName('zoo_taxa', 'full_name', 'taxon_id', dmTaxa.qTaxa.FieldByName('genus_id').AsInteger));
        if dmTaxa.qTaxa.FieldByName('species_id').AsInteger > 0 then
        begin
          nSpecies := tvHierarchy.Items.AddChild(nGenus, GetName('zoo_taxa', 'full_name', 'taxon_id', dmTaxa.qTaxa.FieldByName('species_id').AsInteger));
          if dmTaxa.qTaxa.FieldByName('subspecies_group_id').AsInteger > 0 then
          begin
            nGroup := tvHierarchy.Items.AddChild(nSpecies, GetName('zoo_taxa', 'full_name', 'taxon_id', dmTaxa.qTaxa.FieldByName('subspecies_group_id').AsInteger));
          end;
        end;
      end;
    end;
    tvHierarchy.FullExpand;
  end;
end;

procedure TfrmTaxaEditor.eFindChange(Sender: TObject);
begin
  sbClearFind.Visible := Length(Trim(eFind.Text)) > 0;

  TimerFind.Enabled := False;
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
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trTribe])
        else
        if ExecRegExpr('^.+inae$', FSciName) then
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trSubfamily])
        else
        if ExecRegExpr('^.+idae$', FSciName) then
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trFamily])
        else
        if ExecRegExpr('^.+formes$', FSciName) then
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trOrder])
        else
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trGenus]);
        DS.FieldByName('genus_epithet').AsString := FSciName;
      end;
      2:
      begin
        if (ExtractWord(1, FSciName, [' ']) = 'sp.') then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trSpuh]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').Clear;
        end else
        if (Pos('/', ExtractWord(1, FSciName, [' '])) > 0) then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trSlash]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').Clear;
        end else
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trSpecies]);
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').AsString := ExtractWord(1, FSciName, [' ']);
        end;
        if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
        begin
          DS.FieldByName('parent_taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']));
          DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
        end;
      end;
      3:
      begin
        if (ExtractWord(1, FSciName, [' ']) = 'sp.') then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trSpuh]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').Clear;
          if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
          begin
            DS.FieldByName('parent_taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']));
            DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
          end;
        end else
        if (Pos('/', ExtractWord(1, FSciName, [' '])) > 0) then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trSlash]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').Clear;
          DS.FieldByName('species_epithet').Clear;
        end else
        if (Pos('/', ExtractWord(2, FSciName, [' '])) > 0) then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trPolitypicGroup]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').AsString := ExtractWord(1, FSciName, [' ']);
          if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
          begin
            DS.FieldByName('parent_taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']) + ' ' + ExtractWord(1, FSciName, [' ']));
            DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
          end;
        end else
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trSubspecies]);
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').AsString := ExtractWord(1, FSciName, [' ']);
          DS.FieldByName('subspecies_epithet').AsString := ExtractWord(2, FSciName, [' ']);
          if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
          begin
            DS.FieldByName('parent_taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']) + ' ' + ExtractWord(1, FSciName, [' ']));
            DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
          end;
        end;
      end;
      4:
      begin
        if (ExtractWord(1, FSciName, [' ']) = 'sp.') then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trSpuh]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').Clear;
          if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
          begin
            DS.FieldByName('parent_taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']));
            DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
          end;
        end else
        if (ExtractWord(2, FSciName, [' ']) = 'x') then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trHybrid]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').Clear;
          if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
          begin
            DS.FieldByName('parent_taxon_id').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']));
            DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
          end;
        end else
        if (Pos('[', ExtractWord(2, FSciName, [' '])) > 0) then
        begin
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trPolitypicGroup]);
          DS.FieldByName('clements_taxonomy').AsBoolean := True;
          DS.FieldByName('genus_epithet').AsString := ExtractWord(0, FSciName, [' ']);
          DS.FieldByName('species_epithet').AsString := ExtractWord(1, FSciName, [' ']);
          if DS.FieldByName('parent_taxon_id').AsInteger = 0 then
          begin
            DS.FieldByName('parent_taxon_id').AsInteger:=
              GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(0, FSciName, [' ']) + ' ' + ExtractWord(1, FSciName, [' ']));
            DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
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
          DS.FieldByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[trHybrid]);
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

  nbTaxaSide.Visible := False;

  dlgLoading := TdlgLoading.Create(nil);
  dlgLoading.Show;
  dlgLoading.UpdateProgress('Starting Xolmis Taxonomy Editor...', -1);
end;

procedure TfrmTaxaEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSearch);

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
          dmTaxa.qRanks.Cancel;
      end;
    end;
  end;
end;

procedure TfrmTaxaEditor.FormShow(Sender: TObject);
begin
  FSearch := TCustomSearch.Create(tbZooTaxa);
  FSearch.DataSet := dmTaxa.qTaxa;

  LoadTaxaRanks(dmTaxa.sqlCon, clbTaxonRanksFilter);
  LoadAuthorships(dmTaxa.sqlCon, cbAuthorship.Items);

  //if Self.PixelsPerInch <> 96 then
  //begin
  //  navTabs.OptScalePercents := (Self.PixelsPerInch * 100) div 96;
  //end;
  ptParentTaxon.Top := ptRank.Top + ptRank.Height - 2;

  TimerOpen.Enabled := True;
end;

procedure TfrmTaxaEditor.gridTaxaPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
  AState: TGridDrawState);
begin
  if (Column.FieldName = 'full_name') then
  begin
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
end;

procedure TfrmTaxaEditor.pmgNewSubspeciesClick(Sender: TObject);
var
  Qry: TSQLQuery;
  NewName: String;
  SspRank: Integer;
  BM: TBookmark;
begin
  dlgNewSubspecies := TdlgNewSubspecies.Create(nil);
  with dlgNewSubspecies do
  try
    BM := dmTaxa.qTaxa.Bookmark;
    if ShowModal = mrOK then
    begin
      NewName := dmTaxa.qTaxa.FieldByName('full_name').AsString + ' ' + Epythet;
      SspRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ssp.');

      Qry := TSQLQuery.Create(nil);
      Qry.SQLConnection := dmTaxa.sqlCon;
      with Qry, SQL do
      try
        Add('INSERT INTO zoo_taxa (full_name, formatted_name, ');
        Add('rank_id, parent_taxon_id, species_id, genus_id,');
        Add('genus_epithet, species_epithet,');
        //if (btClements in Taxonomies) then
        //  SQL.Add('clements_taxonomy, ');
        //if (btIOC in Taxonomies) then
        //  SQL.Add('ioc_taxonomy, ioc_rank_id, ioc_parent_taxon_id, ');
        //if (btCBRO in Taxonomies) then
        //  SQL.Add('cbro_taxonomy, cbro_rank_id, cbro_parent_taxon_id,');
        SQL.Add('insert_date) ');
        // List values
        SQL.Add('VALUES (:aname, :aformattedname, :anivel, :asup,');
        SQL.Add(':aspecies, :agenus, ');
        SQL.Add(':agenusname, :aepithet,');
        //if (btClements in Taxonomies) then
        //begin
        //  SQL.Add('1, ');
        //end;
        //if (btIOC in Taxonomies) then
        //begin
        //  SQL.Add('1, :anivelioc, :asupioc, ');
        //end;
        //if (btCBRO in Taxonomies) then
        //begin
        //  SQL.Add('1, :anivelcbro, :asupcbro,');
        //end;
        SQL.Add('datetime(''now'',''localtime''));');
        ParamByName('ANAME').AsString := NewName;
        ParamByName('AFORMATTEDNAME').AsString := FormattedBirdName(NewName, SspRank);
        ParamByName('ANIVEL').AsInteger := SspRank;
        ParamByName('ASUP').AsInteger := dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger;
        ParamByName('ASPECIES').AsInteger := dmTaxa.qTaxa.FieldByName('taxon_id').AsInteger;
        ParamByName('AGENUS').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(1, NewName, [' ']));
        //ParamByName('ASUBFAMILY').AsInteger := Ssp.SubfamilyId;
        //ParamByName('AFAMILY').AsInteger := Ssp.FamilyId;
        //ParamByName('AORDER').AsInteger := Ssp.OrderId;
        ParamByName('AGENUSNAME').AsString := ExtractWord(1, NewName, [' ']);
        ParamByName('AEPITHET').AsString := ExtractWord(2, NewName, [' ']);
        //if (btIOC in Taxonomies) then
        //begin
        //  ParamByName('ANIVELIOC').AsInteger := SspRank;
        //  ParamByName('ASUPIOC').AsInteger := dsTaxa.DataSet.FieldByName('taxon_id').AsInteger;
        //end;
        //if (btCBRO in Taxonomies) then
        //begin
        //  ParamByName('ANIVELCBRO').AsInteger := SspRank;
        //  ParamByName('ASUPCBRO').AsInteger := dsTaxa.DataSet.FieldByName('taxon_id').AsInteger;
        //end;
        //ParamByName('AUSER').AsInteger := AdminId;

        ExecSQL;
      finally
        FreeAndNil(Qry);
      end;
    end;
  finally
    FreeAndNil(dlgNewSubspecies);
    dmTaxa.qTaxa.Refresh;
    if dmTaxa.qTaxa.BookmarkValid(BM) then
      dmTaxa.qTaxa.Bookmark := BM;
  end;
end;

procedure TfrmTaxaEditor.pmtSortClick(Sender: TObject);
begin
  FSearch.SortFields.Clear;

  SearchTaxa(eFind.Text);
end;

procedure TfrmTaxaEditor.pmvMoveToGenusClick(Sender: TObject);
var
  Qry: TSQLQuery;
  //BM: TBookmark;
begin
  dlgDestTaxon := TdlgDestTaxon.Create(nil);
  with dlgDestTaxon do
  try
    //BM := dsTaxa.DataSet.Bookmark;
    dmTaxa.qTaxa.DisableControls;
    TaxonomyAction:= taLump;
    if ShowModal = mrOK then
    begin
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
    end;
  finally
    FreeAndNil(dlgDestTaxon);
    dmTaxa.qTaxa.EnableControls;
    dmTaxa.qTaxa.Refresh;
    //if dsTaxa.DataSet.BookmarkValid(BM) then
    //  dsTaxa.DataSet.Bookmark := BM;
  end;
end;

procedure TfrmTaxaEditor.pmvMoveToSpeciesClick(Sender: TObject);
var
  Qry: TSQLQuery;
  //BM: TBookmark;
begin
  dlgDestTaxon := TdlgDestTaxon.Create(nil);
  with dlgDestTaxon do
  try
    //BM := dsTaxa.DataSet.Bookmark;
    dmTaxa.qTaxa.DisableControls;
    TaxonomyAction:= taLump;
    if ShowModal = mrOK then
    begin
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
    end;
  finally
    FreeAndNil(dlgDestTaxon);
    dmTaxa.qTaxa.EnableControls;
    dmTaxa.qTaxa.Refresh;
    //if dsTaxa.DataSet.BookmarkValid(BM) then
    //  dsTaxa.DataSet.Bookmark := BM;
  end;
end;

procedure TfrmTaxaEditor.rbMarkedYesClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  SearchTaxa(eFind.Text);
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

procedure TfrmTaxaEditor.sbCancelRecordClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0:
    begin
      dmTaxa.qTaxa.Cancel;
    end;
    1:
    begin

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
      FSearch.QuickFilters.Clear;
      ClearTaxaFilters;
      SearchTaxa(eFind.Text);

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

procedure TfrmTaxaEditor.sbDelRecordClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0:
    begin
      with dmTaxa.qTaxa do
      begin
        Edit;
        FieldByName('active_status').AsBoolean := False;
        Post;
      end;
    end;
    1:
    begin

    end;
    2:
    begin
      with dmTaxa.qRanks do
      begin
        Edit;
        FieldByName('active_status').AsBoolean := False;
        Post;
      end;
    end;
    3:
    begin
      with dmTaxa.qCountries do
      begin
        Edit;
        FieldByName('active_status').AsBoolean := False;
        Post;
      end;
    end;
    4:
    begin
      with dmTaxa.qLanguages do
      begin
        Edit;
        FieldByName('active_status').AsBoolean := False;
        Post;
      end;
    end;
  end;
end;

procedure TfrmTaxaEditor.sbEditRecordClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0: dmTaxa.qTaxa.Edit;
    1: ;
    2: dmTaxa.qRanks.Edit;
    3: dmTaxa.qCountries.Edit;
    4: dmTaxa.qLanguages.Edit;
  end;
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

procedure TfrmTaxaEditor.sbInsertRecordClick(Sender: TObject);
begin
  case nbPages.PageIndex of
    0: dmTaxa.qTaxa.Insert;
    1: ;
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
  Qry: TSQLQuery;
  //BM: TBookmark;
begin
  dlgDestTaxon := TdlgDestTaxon.Create(nil);
  with dlgDestTaxon do
  try
    //BM := dmTaxa.qTaxa.Bookmark;
    dmTaxa.qTaxa.DisableControls;
    TaxonomyAction:= taLump;
    if ShowModal = mrOK then
    begin
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
    end;
  finally
    FreeAndNil(dlgDestTaxon);
    dmTaxa.qTaxa.EnableControls;
    dmTaxa.qTaxa.Refresh;
    //if dmTaxa.qTaxa.BookmarkValid(BM) then
    //  dmTaxa.qTaxa.Bookmark := BM;
  end;
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
  Qry: TSQLQuery;
  //BM: TBookmark;
begin
  dlgDestTaxon := TdlgDestTaxon.Create(nil);
  with dlgDestTaxon do
  try
    //BM := dmTaxa.qTaxa.Bookmark;
    dmTaxa.qTaxa.DisableControls;
    TaxonomyAction:= taSplit;
    if ShowModal = mrOK then
    begin
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
    end;
  finally
    FreeAndNil(dlgDestTaxon);
    dmTaxa.qTaxa.EnableControls;
    dmTaxa.qTaxa.Refresh;
    //if dmTaxa.qTaxa.BookmarkValid(BM) then
    //  dmTaxa.qTaxa.Bookmark := BM;
  end;
end;

procedure TfrmTaxaEditor.sbTaxaClick(Sender: TObject);
begin
  if Sender is TSpeedButton then
    nbPages.PageIndex := TSpeedButton(Sender).Tag;
end;

procedure TfrmTaxaEditor.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;
  if not CanToggle then
    Exit;

  case nbPages.PageIndex of
    0: SearchTaxa(eFind.Text);
    1: ;
    2: ;
    3: ;
    4: ;
  end;
end;

procedure TfrmTaxaEditor.TimerOpenTimer(Sender: TObject);
begin
  TimerOpen.Enabled := False;

  OpenAsync;
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
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean,
      crEqual, False, '1'));
  end
  else
  if (rbMarkedNo.Checked) then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('marked_status', 'Marked', sdtBoolean,
      crEqual, False, '0'));
  end;

  cc := 0;
  for i := 0 to clbTaxonRanksFilter.Count - 1 do
    if clbTaxonRanksFilter.Checked[i] then
      Inc(cc);
  if cc > 0 then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    for i := 0 to clbTaxonRanksFilter.Count - 1 do
      if clbTaxonRanksFilter.Checked[i] then
        FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('rank_id', 'Rank', sdtInteger,
          crEqual, False, IntToStr(GetKey('taxon_ranks', 'rank_id', 'rank_name', clbTaxonRanksFilter.Items[i]))));
  end;

  //if tsTaxonomyClements.Checked then
  //begin
  //  sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
  //  FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('clements_taxonomy', 'Clements/eBird', sdtBoolean,
  //    crEqual, False, '1'));
  //end;
  //if tsTaxonomyIoc.Checked then
  //begin
  //  sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
  //  FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('ioc_taxonomy', 'IOC', sdtBoolean,
  //    crEqual, False, '1'));
  //end;
  //if tsTaxonomyCbro.Checked then
  //begin
  //  sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
  //  FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('cbro_taxonomy', 'CBRO', sdtBoolean,
  //    crEqual, False, '1'));
  //end;

  if rbExtinctYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('extinct', 'Extinct', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbExtinctNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('extinct', 'Extinct', sdtBoolean,
      crEqual, False, '0'));
  end;

  if rbAcceptedYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('accepted_status', 'Accepted', sdtBoolean,
      crEqual, False, '1'));
  end;
  if rbAcceptedNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('accepted_status', 'Accepted', sdtBoolean,
      crEqual, False, '0'));
  end;
  //if tsHasSynonyms.StateOn = sw_on then
  //begin
  //  sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
  //  FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
  //    crMoreThan, False, '1'));
  //end;

  CanToggle := True;
end;

procedure TfrmTaxaEditor.gridTaxaDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
begin
  if Column.FieldName = 'formatted_name' then
  begin
    HtmlView.Width := Column.Width;
    HtmlView.LoadFromString(Column.Field.AsString);
    HtmlView.PaintTo(gridTaxa.Canvas.Handle, Rect.Left, Rect.Top);
  end
  else
  begin
    gridTaxa.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
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
      FSearch.Fields[g].Fields.Add(TSearchField.Create('ebird_code', 'eBird code', sdtText, Crit,
        False, aValue));
      FSearch.Fields[g].Fields.Add(TSearchField.Create('quick_code', 'Quick code', sdtText, Crit,
        False, aValue));
    end;
  end;

  GetTaxaFilters;

  if pmtSortTaxonomic.Checked then
    AddSortedField('sort_num', sdAscending)
  else
  if pmtSortAlphabetical.Checked then
    AddSortedField('full_name', sdAscending);

  Result := FSearch.RunSearch > 0;

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
      sbClearFilters.Enabled := FSearch.QuickFilters.Count > 0;
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
    lblTitleChilds.Caption := Format('ChildTaxa (%d)', [dmTaxa.qChildTaxa.RecordCount]);
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

