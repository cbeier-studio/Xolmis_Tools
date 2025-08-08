unit ufrm_TaxaEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLIntf, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, Menus, DB, DBCtrls,
  ActnList, StdCtrls, attabs, BCPanel, BCButton, ColorSpeedButton, StrUtils, RegExpr, Character,
  BGRABitmap, SQLDB, CheckLst, Grids, DBGrids, ComCtrls, DBEditButton, ToggleSwitch, lib_taxa,
  IBLookupComboEditBox, Types, ImgList, HtmlView;

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
    btnCancelProgress: TBitBtn;
    cbAuthorship: TIBLookupComboEditBox;
    cbtIocRank: TDBLookupComboBox;
    cbtRank: TDBLookupComboBox;
    cktCbro: TDBCheckBox;
    cktClements: TDBCheckBox;
    cktExtinct: TDBCheckBox;
    cktIoc: TDBCheckBox;
    cbtIucnStatus: TDBComboBox;
    gridChildTaxa: TDBGrid;
    dsSynonyms: TDataSource;
    dsChildTaxa: TDataSource;
    gridSynonyms: TDBGrid;
    dsPacks: TDataSource;
    dsRanks: TDataSource;
    dsTaxa: TDataSource;
    dsTaxaUpdates: TDataSource;
    eFindRanks: TEdit;
    eFindTaxa2: TEdit;
    etFullname: TDBEdit;
    etCbroOtherPtNames: TDBEdit;
    etEbirdCode: TDBEdit;
    etEnglishName: TDBEdit;
    etExtinctionYear: TDBEdit;
    etIocEnglishName: TDBEdit;
    etIocSortNr: TDBEdit;
    etIocParentTaxon: TDBEditButton;
    etIocValidName: TDBEditButton;
    etParentTaxon: TDBEditButton;
    etPortugueseName: TDBEdit;
    etQuickcode: TDBEdit;
    etSortNr: TDBEdit;
    etSpanishName: TDBEdit;
    etSubspecificGroup: TDBEdit;
    etValidName: TDBEditButton;
    gridTaxa1: TDBGrid;
    gridTaxa2: TDBGrid;
    gridRanks: TDBGrid;
    HtmlView: THtmlViewer;
    icoMarkedFilter: TImage;
    iconFindRanks: TImage;
    iconFindTaxa2: TImage;
    iIucnStatus: TImageList;
    imgSplash: TImage;
    lblMarkedFilter: TLabel;
    lblTitleSynonyms: TLabel;
    lblCountTaxa: TLabel;
    lblLoading: TLabel;
    lblProgress: TLabel;
    lbltAuthorship: TLabel;
    lbltCbroOtherPtNames: TLabel;
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
    lblTitleChilds: TLabel;
    lbltParentTaxon: TLabel;
    lbltPortugueseName: TLabel;
    lbltQuickCode: TLabel;
    lbltIucnStatus: TLabel;
    lbltRank: TLabel;
    lbltSortNr: TLabel;
    lbltSpanishName: TLabel;
    lbltSubspecificGroup: TLabel;
    lbltValidName: TLabel;
    mmSspVernacularNames: TMenuItem;
    mmRewriteHierarchy: TMenuItem;
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
    pmGrid: TPopupMenu;
    pProgress: TPanel;
    PBar: TProgressBar;
    pSplash: TPanel;
    ptAuthorship: TBCPanel;
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
    mtIocDistribution: TDBMemo;
    peTaxa: TPanel;
    pFindRanks: TBCPanel;
    pFindTaxa2: TBCPanel;
    pmMove: TPopupMenu;
    pmSortTaxa: TPopupMenu;
    pTaxaList1: TBCPanel;
    pRanksToolbar: TBCPanel;
    pTaxaToolbar2: TBCPanel;
    ptCbroOtherPtNames: TBCPanel;
    ptContent: TBCPanel;
    gridTaxa: TDBGrid;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    ptDistribution: TBCPanel;
    ptEbirdCode: TBCPanel;
    ptEnglishName: TBCPanel;
    ptExtinct: TBCPanel;
    ptIocDistribution: TBCPanel;
    ptIocEnglishName: TBCPanel;
    ptIocParentTaxon: TBCPanel;
    ptIocRank: TBCPanel;
    ptIocSortNr: TBCPanel;
    ptIocValidName: TBCPanel;
    ptPortugueseName: TBCPanel;
    ptQuickCode: TBCPanel;
    ptSortNr: TBCPanel;
    ptSpanishName: TBCPanel;
    ptSubspecificGroup: TBCPanel;
    ptValidName: TBCPanel;
    rbMarkedAll: TRadioButton;
    rbExtinctAll: TRadioButton;
    rbIsSynonymAll: TRadioButton;
    rbHasSynonymsAll: TRadioButton;
    rbExtinctNo: TRadioButton;
    rbIsSynonymNo: TRadioButton;
    rbHasSynonymsNo: TRadioButton;
    rbMarkedYes: TRadioButton;
    rbMarkedNo: TRadioButton;
    rbExtinctYes: TRadioButton;
    rbIsSynonymYes: TRadioButton;
    rbHasSynonymsYes: TRadioButton;
    sbAdvancedFilters2: TSpeedButton;
    sbCancelRank: TSpeedButton;
    sbCancelRecord2: TSpeedButton;
    sbClearRankFilters: TSpeedButton;
    sbClearFilters2: TSpeedButton;
    sbClearFindTaxa: TColorSpeedButton;
    sbClearFindRanks: TColorSpeedButton;
    sbClearFindTaxa2: TColorSpeedButton;
    sbDelRecord: TSpeedButton;
    sbEditRecord: TSpeedButton;
    sbFirstRank: TSpeedButton;
    sbFirstRecord2: TSpeedButton;
    sbInsertRank: TSpeedButton;
    sbInsertRecord2: TSpeedButton;
    sbLastRank: TSpeedButton;
    sbLastRecord2: TSpeedButton;
    sbLumpTaxon: TSpeedButton;
    sbMoveTaxon: TSpeedButton;
    sbNextRank: TSpeedButton;
    sbNextRecord2: TSpeedButton;
    sboxTaxa: TScrollBox;
    sbPriorRank: TSpeedButton;
    sbPriorRecord2: TSpeedButton;
    sbRefreshRanks: TSpeedButton;
    sbRefreshRecords2: TSpeedButton;
    sbSaveRank: TSpeedButton;
    sbSaveRecord2: TSpeedButton;
    sbSplitTaxon: TSpeedButton;
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
    iconFindTaxa: TImage;
    icoSynonymsFilter: TImage;
    icoTaxonomiesFilter: TImage;
    icoTaxonRanksFilter: TImage;
    lblClementsFilter: TLabel;
    lblCountTaxonRanksFilter: TLabel;
    lblExtinctFilter: TLabel;
    lblHasSynonymsFilter: TLabel;
    lblSynonymFilter: TLabel;
    lblTaxonomyCbroFilter: TLabel;
    lblTaxonomyIocFilter: TLabel;
    lblTaxonRanksFilter: TLabel;
    navTabs: TATTabs;
    nbTaxaSide: TNotebook;
    pFindTaxa: TBCPanel;
    pExtinctFilter: TBCPanel;
    pgTaxaFilters: TPage;
    pHasSynonymsFilter: TBCPanel;
    pIsSynonymFilter: TBCPanel;
    pMainMenu: TBCPanel;
    nbPages: TNotebook;
    pgRanks: TPage;
    pgTaxonomies: TPage;
    pgTaxa: TPage;
    pTaxaList: TBCPanel;
    pTaxaRightBar: TBCPanel;
    pTaxaToolbar: TBCPanel;
    pTaxonomyCbroFilter: TBCPanel;
    pTaxonomyClementsFilter: TBCPanel;
    pTaxonomyIocFilter: TBCPanel;
    pTaxonRanksFilters: TBCPanel;
    pTitleTaxonRanksFilter: TPanel;
    sbAdvancedFilters: TSpeedButton;
    sbCancelRecord: TSpeedButton;
    sbClearFilters: TSpeedButton;
    sbFileMenu: TBCButton;
    sbFirstRecord: TSpeedButton;
    sbInsertRecord: TSpeedButton;
    sbLastRecord: TSpeedButton;
    sbMoreOptions: TSpeedButton;
    sbNextRecord: TSpeedButton;
    sboxTaxaFilters: TScrollBox;
    sbPriorRecord: TSpeedButton;
    sbRefreshRecords: TSpeedButton;
    sbSaveRecord: TSpeedButton;
    sbShowImages: TSpeedButton;
    sbShowQuickFilters: TSpeedButton;
    sbShowRecycle: TSpeedButton;
    sbSortRecords: TSpeedButton;
    Separator1: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    splitTaxaLeft: TSplitter;
    splitTaxaLeft1: TSplitter;
    splitTaxaRight: TSplitter;
    TimerFind: TTimer;
    tsTaxonomyClements: TToggleSwitch;
    tsTaxonomyIoc: TToggleSwitch;
    tsTaxonomyCbro: TToggleSwitch;
    tvHierarchy: TTreeView;
    procedure actAboutExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actFormatSciNamesExecute(Sender: TObject);
    procedure actImportClementsExecute(Sender: TObject);
    procedure actImportIOCNamesExecute(Sender: TObject);
    procedure actRewriteHierarchyExecute(Sender: TObject);
    procedure actSspVernacularNamesExecute(Sender: TObject);
    procedure cbtIucnStatusDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure cktCbroClick(Sender: TObject);
    procedure cktIocClick(Sender: TObject);
    procedure clbTaxonRanksFilterClickCheck(Sender: TObject);
    procedure dsTaxaDataChange(Sender: TObject; Field: TField);
    procedure dsTaxaStateChange(Sender: TObject);
    procedure eFindTaxaChange(Sender: TObject);
    procedure eFindTaxaEnter(Sender: TObject);
    procedure etFullnameExit(Sender: TObject);
    procedure etIocParentTaxonButtonClick(Sender: TObject);
    procedure etIocParentTaxonKeyPress(Sender: TObject; var Key: char);
    procedure etIocValidNameButtonClick(Sender: TObject);
    procedure etIocValidNameKeyPress(Sender: TObject; var Key: char);
    procedure etParentTaxonButtonClick(Sender: TObject);
    procedure etParentTaxonKeyPress(Sender: TObject; var Key: char);
    procedure etValidNameButtonClick(Sender: TObject);
    procedure etValidNameKeyPress(Sender: TObject; var Key: char);
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
    procedure navTabsTabClick(Sender: TObject);
    procedure pmgNewSubspeciesClick(Sender: TObject);
    procedure pmtSortClick(Sender: TObject);
    procedure pmvMoveToGenusClick(Sender: TObject);
    procedure pmvMoveToSpeciesClick(Sender: TObject);
    procedure pTaxaListResize(Sender: TObject);
    procedure rbMarkedYesClick(Sender: TObject);
    procedure sbAdvancedFiltersClick(Sender: TObject);
    procedure sbCancelRecordClick(Sender: TObject);
    procedure sbClearFiltersClick(Sender: TObject);
    procedure sbClearFindTaxaClick(Sender: TObject);
    procedure sbDelRecordClick(Sender: TObject);
    procedure sbEditRecordClick(Sender: TObject);
    procedure sbFirstRecordClick(Sender: TObject);
    procedure sbInsertRecordClick(Sender: TObject);
    procedure sbLastRecordClick(Sender: TObject);
    procedure sbLumpTaxonClick(Sender: TObject);
    procedure sbMoveTaxonClick(Sender: TObject);
    procedure sbNextRecordClick(Sender: TObject);
    procedure sbPriorRecordClick(Sender: TObject);
    procedure sbRefreshRecordsClick(Sender: TObject);
    procedure sbSaveRecordClick(Sender: TObject);
    procedure sbShowQuickFiltersClick(Sender: TObject);
    procedure sbSortRecordsClick(Sender: TObject);
    procedure sbSplitTaxonClick(Sender: TObject);
    procedure TimerFindTimer(Sender: TObject);
    procedure tsTaxonExtinctOff(Sender: TObject);
    procedure tsTaxonExtinctOn(Sender: TObject);
  private
    FSearch: TCustomSearch;
    SkipCompleteAutor: Boolean;
    CanToggle: Boolean;
    Working: Boolean;
    procedure AddSortedField(aFieldName: String; aDirection: TSortDirection; aCollation: String = '';
      IsAnAlias: Boolean = False);
    procedure ClearTaxaFilters;
    procedure GetTaxaFilters;
    function SearchTaxa(aValue: String): Boolean;
    procedure UpdateButtons(aDataSet: TDataSet);
    function ValidateTaxon: Boolean;
  public

  end;

var
  frmTaxaEditor: TfrmTaxaEditor;

implementation

uses
  udm_taxa, udlg_about, udlg_desttaxon, udlg_edithierarchy, udlg_newsubspecies, udlg_sqlfilter;

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
  with dsTaxa.DataSet do
  try
    First;
    DisableControls;
    PBar.Position := 0;
    PBar.Max := RecordCount;
    pProgress.Visible := True;
    repeat
      Edit;
      FieldByName('formatted_name').AsString := FormattedBirdName(FieldByName('full_name').AsString, FieldByName('rank_id').AsInteger);
      Post;

      PBar.Position := RecNo;
      Application.ProcessMessages;
      Next;
    until Eof or Parar;
  finally
    First;
    EnableControls;
    pProgress.Visible := False;
  end;

end;

procedure TfrmTaxaEditor.actImportClementsExecute(Sender: TObject);
begin
  if OpenDlg.Execute then
    ImportClementsData(OpenDlg.FileName);

  dsTaxa.DataSet.Refresh;
end;

procedure TfrmTaxaEditor.actImportIOCNamesExecute(Sender: TObject);
begin
  if OpenDlg.Execute then
    ImportIocData(OpenDlg.FileName);

  dsTaxa.DataSet.Refresh;
end;

procedure TfrmTaxaEditor.actRewriteHierarchyExecute(Sender: TObject);
var
  Qry: TSQLQuery;
  iOrder, iFamily, iSubfamily, iGenus, iSpecies, iMonoGroup, iPoliGroup, iSubspecies: Integer;
begin
  try
    PBar.Style := pbstMarquee;
    pProgress.Visible := True;
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

      PBar.Position := 0;
      PBar.Max := 8;
      PBar.Style := pbstNormal;
      if not dmTaxa.sqlTrans.Active then
        dmTaxa.sqlTrans.StartTransaction;
      try
        { Order }
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET order_id = taxon_id');
        Add('WHERE zoo_taxa.rank_id = :rank_id');
        ParamByName('RANK_ID').AsInteger := iOrder;
        ExecSQL;
        PBar.Position := PBar.Position + 1;
        Application.ProcessMessages;

        { Family }
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET family_id = zoo_taxa.taxon_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iFamily;
        ExecSQL;
        PBar.Position := PBar.Position + 1;
        Application.ProcessMessages;

        { Subfamily }
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET subfamily_id = zoo_taxa.taxon_id, family_id = parent.family_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iSubfamily;
        ExecSQL;
        PBar.Position := PBar.Position + 1;
        Application.ProcessMessages;

        { Genus }
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET genus_id = zoo_taxa.taxon_id, subfamily_id = parent.subfamily_id, ');
        Add('family_id = parent.family_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iGenus;
        ExecSQL;
        PBar.Position := PBar.Position + 1;
        Application.ProcessMessages;

        { Species }
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET species_id = zoo_taxa.taxon_id, genus_id = parent.genus_id, ');
        Add('subfamily_id = parent.subfamily_id, family_id = parent.family_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iSpecies;
        ExecSQL;
        PBar.Position := PBar.Position + 1;
        Application.ProcessMessages;

        { Mono and politypic groups }
        Clear;
        Add('UPDATE zoo_taxa');
        Add('SET subspecies_group_id = zoo_taxa.taxon_id, species_id = parent.species_id, genus_id = parent.genus_id, ');
        Add('subfamily_id = parent.subfamily_id, family_id = parent.family_id, order_id = parent.order_id');
        Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id, species_id FROM zoo_taxa) AS parent');
        Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
        ParamByName('RANK_ID').AsInteger := iMonoGroup;
        ExecSQL;
        PBar.Position := PBar.Position + 1;
        Application.ProcessMessages;
        ParamByName('RANK_ID').AsInteger := iPoliGroup;
        ExecSQL;
        PBar.Position := PBar.Position + 1;
        Application.ProcessMessages;

        { Subspecies, domestic, form }
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
        PBar.Position := PBar.Position + 1;
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
    pProgress.Visible := False;
    PBar.Style := pbstNormal;
  end;
end;

procedure TfrmTaxaEditor.actSspVernacularNamesExecute(Sender: TObject);
var
  Sp: Integer;
begin
  with dsTaxa.DataSet do
  try
    First;
    DisableControls;
    PBar.Position := 0;
    PBar.Max := RecordCount;
    pProgress.Visible := True;
    repeat
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

      PBar.Position := RecNo;
      Application.ProcessMessages;
      Next;
    until Eof or Parar;
  finally
    First;
    EnableControls;
    pProgress.Visible := False;
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

procedure TfrmTaxaEditor.cktCbroClick(Sender: TObject);
begin
  if (dsTaxa.DataSet.State in [dsInsert, dsEdit]) then
    if dsTaxa.DataSet.FieldByName('cbro_rank_id').AsInteger = 0 then
    begin
      dsTaxa.DataSet.FieldByName('cbro_rank_id').AsInteger := dsTaxa.DataSet.FieldByName('rank_id').AsInteger;
      dsTaxa.DataSet.FieldByName('cbro_parent_taxon_id').AsInteger := dsTaxa.DataSet.FieldByName('parent_taxon_id').AsInteger;
      dsTaxa.DataSet.FieldByName('cbro_parent_taxon_name').AsString := dsTaxa.DataSet.FieldByName('parent_taxon_name').AsString;
    end;
end;

procedure TfrmTaxaEditor.cktIocClick(Sender: TObject);
begin
  if (dsTaxa.DataSet.State in [dsInsert, dsEdit]) then
    if dsTaxa.DataSet.FieldByName('ioc_rank_id').AsInteger = 0 then
    begin
      dsTaxa.DataSet.FieldByName('ioc_rank_id').AsInteger := dsTaxa.DataSet.FieldByName('rank_id').AsInteger;
      dsTaxa.DataSet.FieldByName('ioc_parent_taxon_id').AsInteger := dsTaxa.DataSet.FieldByName('parent_taxon_id').AsInteger;
      dsTaxa.DataSet.FieldByName('ioc_parent_taxon_name').AsString := dsTaxa.DataSet.FieldByName('parent_taxon_name').AsString;
      dsTaxa.DataSet.FieldByName('ioc_english_name').AsString := dsTaxa.DataSet.FieldByName('english_name').AsString;
    end;
end;

procedure TfrmTaxaEditor.clbTaxonRanksFilterClickCheck(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  SearchTaxa(eFindTaxa.Text);
end;

procedure TfrmTaxaEditor.ClearTaxaFilters;
begin
  CanToggle := False;

  rbMarkedAll.Checked := True;
  rbExtinctAll.Checked := True;
  rbIsSynonymAll.Checked := True;
  rbHasSynonymsAll.Checked := True;

  tsTaxonomyClements.Checked := False;
  tsTaxonomyIoc.Checked := False;
  tsTaxonomyCbro.Checked := False;

  clbTaxonRanksFilter.CheckAll(cbUnchecked);

  CanToggle := True;
end;

procedure TfrmTaxaEditor.dsTaxaDataChange(Sender: TObject; Field: TField);
var
  nOrder, nFamily, nGenus, nSpecies, nGroup: TTreeNode;
begin
  UpdateButtons(dsTaxa.DataSet);

  tvHierarchy.Items.Clear;
  if dsTaxa.DataSet.FieldByName('order_id').AsInteger > 0 then
  begin
    nOrder := tvHierarchy.Items.Add(nil, GetName('zoo_taxa', 'full_name', 'taxon_id', dsTaxa.DataSet.FieldByName('order_id').AsInteger));
    if dsTaxa.DataSet.FieldByName('family_id').AsInteger > 0 then
    begin
      nFamily := tvHierarchy.Items.AddChild(nOrder, GetName('zoo_taxa', 'full_name', 'taxon_id', dsTaxa.DataSet.FieldByName('family_id').AsInteger));
      if dsTaxa.DataSet.FieldByName('genus_id').AsInteger > 0 then
      begin
        nGenus := tvHierarchy.Items.AddChild(nFamily, GetName('zoo_taxa', 'full_name', 'taxon_id', dsTaxa.DataSet.FieldByName('genus_id').AsInteger));
        if dsTaxa.DataSet.FieldByName('species_id').AsInteger > 0 then
        begin
          nSpecies := tvHierarchy.Items.AddChild(nGenus, GetName('zoo_taxa', 'full_name', 'taxon_id', dsTaxa.DataSet.FieldByName('species_id').AsInteger));
          if dsTaxa.DataSet.FieldByName('subspecies_group_id').AsInteger > 0 then
          begin
            nGroup := tvHierarchy.Items.AddChild(nSpecies, GetName('zoo_taxa', 'full_name', 'taxon_id', dsTaxa.DataSet.FieldByName('subspecies_group_id').AsInteger));
          end;
        end;
      end;
    end;
    tvHierarchy.FullExpand;
  end;
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

procedure TfrmTaxaEditor.eFindTaxaEnter(Sender: TObject);
begin
  if eFindTaxa.Text <> EmptyStr then
    eFindTaxa.SelectAll;
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
          DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
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
            DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
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
            DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
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
            DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
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
            DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
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
            DS.FieldByName('parent_taxon_name').AsString := GetName('zoo_taxa', 'full_name', 'taxon_id', DS.FieldByName('parent_taxon_id').AsInteger);
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
var
  FRank: TZooRank;
  FFilter: TTaxonFilters;
begin
  FRank := GetRankType(dmTaxa.qTaxa.FieldByName('ioc_rank_id').AsInteger);
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

  if FindTaxonDlg(FFilter, etIocParentTaxon, dsTaxa.DataSet, 'ioc_parent_taxon_id', 'ioc_parent_taxon_name', True, False) then
    SelectNext(Sender as TWinControl, True, True);
end;

procedure TfrmTaxaEditor.etIocParentTaxonKeyPress(Sender: TObject; var Key: char);
var
  FRank: TZooRank;
  FFilter: TTaxonFilters;
begin
  FormKeyPress(Sender, Key);

  FRank := GetRankType(dmTaxa.qTaxa.FieldByName('ioc_rank_id').AsInteger);
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

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    if FindTaxonDlg(FFilter, etIocParentTaxon, dsTaxa.DataSet, 'ioc_parent_taxon_id', 'ioc_parent_taxon_name', True, False, Key) then
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsTaxa.DataSet.FieldByName('ioc_parent_taxon_id').Clear;
    dsTaxa.DataSet.FieldByName('ioc_parent_taxon_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;

end;

procedure TfrmTaxaEditor.etIocValidNameButtonClick(Sender: TObject);
begin
  if FindTaxonDlg([tfAll], etIocValidName, dsTaxa.DataSet, 'ioc_valid_id', 'ioc_valid_name', True, False) then
    SelectNext(Sender as TWinControl, True, True);
end;

procedure TfrmTaxaEditor.etIocValidNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    if FindTaxonDlg([tfAll], etIocValidName, dsTaxa.DataSet, 'ioc_valid_id', 'ioc_valid_name', True, False, Key) then
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsTaxa.DataSet.FieldByName('ioc_valid_id').Clear;
    dsTaxa.DataSet.FieldByName('ioc_valid_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
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
    dsTaxa.DataSet.FieldByName('parent_taxon_id').Clear;
    dsTaxa.DataSet.FieldByName('parent_taxon_name').Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TfrmTaxaEditor.etValidNameButtonClick(Sender: TObject);
begin
  if FindTaxonDlg([tfAll], etValidName, dmTaxa.qTaxa, 'valid_id', 'valid_name', True, False) then
    SelectNext(Sender as TWinControl, True, True);
end;

procedure TfrmTaxaEditor.etValidNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    if FindTaxonDlg([tfAll], etValidName, dmTaxa.qTaxa, 'valid_id', 'valid_name', True, False, Key) then
      SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    dsTaxa.DataSet.FieldByName('valid_id').Clear;
    dsTaxa.DataSet.FieldByName('valid_name').Clear;
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

  pSplash.Top := 0;
  pSplash.Left := 0;
  pSplash.Width := Self.ClientWidth;
  pSplash.Height := Self.ClientHeight;
  pSplash.Visible := True;
end;

procedure TfrmTaxaEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSearch);
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
        if (dsTaxa.DataSet.State in [dsInsert, dsEdit]) then
        begin
          dsTaxa.DataSet.Cancel;
        end
        else
        begin
          eFindTaxa.SetFocus;
          eFindTaxa.Clear;
        end;
      end;
      1: ;
      2:
      begin
        if (dsRanks.DataSet.State in [dsInsert, dsEdit]) then
          dsRanks.DataSet.Cancel;
      end;
    end;
  end;
end;

procedure TfrmTaxaEditor.FormShow(Sender: TObject);
begin
  //pSplash.Top := 0;
  //pSplash.Left := 0;
  //pSplash.Width := Self.ClientWidth;
  //pSplash.Height := Self.ClientHeight;
  //pSplash.Visible := True;
  //Application.ProcessMessages;

  FSearch := TCustomSearch.Create(tbZooTaxa);
  FSearch.DataSet := TSQLQuery(dsTaxa.DataSet);

  dmTaxa.lookRanks.Open;
  dmTaxa.lookAuthors.Open;
  dsTaxa.DataSet.Open;
  dsRanks.DataSet.Open;
  dsPacks.DataSet.Open;
  dsTaxaUpdates.DataSet.Open;

  LoadTaxaRanks(dmTaxa.sqlCon, clbTaxonRanksFilter);

  if Self.PixelsPerInch <> 96 then
  begin
    navTabs.OptScalePercents := (Self.PixelsPerInch * 100) div 96;
  end;
  ptParentTaxon.Top := ptRank.Top + ptRank.Height - 2;

  UpdateButtons(dsTaxa.DataSet);
  CanToggle := True;

  Application.ProcessMessages;
  //Sleep(500);

  pSplash.Visible := False;
end;

procedure TfrmTaxaEditor.gridTaxaPrepareCanvas(sender: TObject; DataCol: Integer; Column: TColumn;
  AState: TGridDrawState);
begin
  if (Column.FieldName = 'full_name') then
  begin
    if GetRankType(TDBGrid(Sender).Columns[4].Field.AsInteger) >= trSuperGenus then
      TDBGrid(Sender).Canvas.Font.Style := [fsItalic]
    else
      TDBGrid(Sender).Canvas.Font.Style := [fsBold];

    if not (gdSelected in AState) then
    begin
      if GetRankType(TDBGrid(Sender).Columns[4].Field.AsInteger) = trSpecies then
        TDBGrid(Sender).Canvas.Font.Color := clNavy;

      if GetRankType(TDBGrid(Sender).Columns[4].Field.AsInteger) in [trMonotypicGroup, trPolitypicGroup,
                                                trForm, trSpuh, trHybrid, trIntergrade, trDomestic, trSlash] then
        TDBGrid(Sender).Canvas.Font.Color := clGreen;

      if (TDBGrid(Sender).Columns[3].Field.AsInteger > 0) then
        TDBGrid(Sender).Canvas.Font.Color := $00646464;
    end;
  end;
end;

procedure TfrmTaxaEditor.navTabsTabClick(Sender: TObject);
begin
  nbPages.PageIndex := navTabs.TabIndex;
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
    BM := dsTaxa.DataSet.Bookmark;
    if ShowModal = mrOK then
    begin
      NewName := dsTaxa.DataSet.FieldByName('full_name').AsString + ' ' + Epythet;
      SspRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ssp.');

      Qry := TSQLQuery.Create(nil);
      Qry.SQLConnection := dmTaxa.sqlCon;
      with Qry, SQL do
      try
        Add('INSERT INTO zoo_taxa (full_name, formatted_name, ');
        Add('rank_id, parent_taxon_id, species_id, genus_id,');
        Add('genus_epithet, species_epithet,');
        if (btClements in Taxonomies) then
          SQL.Add('clements_taxonomy, ');
        if (btIOC in Taxonomies) then
          SQL.Add('ioc_taxonomy, ioc_rank_id, ioc_parent_taxon_id, ');
        if (btCBRO in Taxonomies) then
          SQL.Add('cbro_taxonomy, cbro_rank_id, cbro_parent_taxon_id,');
        SQL.Add('insert_date, user_inserted) ');
        // List values
        SQL.Add('VALUES (:aname, :aformattedname, :anivel, :asup,');
        SQL.Add(':aspecies, :agenus, ');
        SQL.Add(':agenusname, :aepithet,');
        if (btClements in Taxonomies) then
        begin
          SQL.Add('1, ');
        end;
        if (btIOC in Taxonomies) then
        begin
          SQL.Add('1, :anivelioc, :asupioc, ');
        end;
        if (btCBRO in Taxonomies) then
        begin
          SQL.Add('1, :anivelcbro, :asupcbro,');
        end;
        SQL.Add('datetime(''now'',''localtime''), :auser);');
        ParamByName('ANAME').AsString := NewName;
        ParamByName('AFORMATTEDNAME').AsString := FormattedBirdName(NewName, SspRank);
        ParamByName('ANIVEL').AsInteger := SspRank;
        ParamByName('ASUP').AsInteger := dsTaxa.DataSet.FieldByName('taxon_id').AsInteger;
        ParamByName('ASPECIES').AsInteger := dsTaxa.DataSet.FieldByName('taxon_id').AsInteger;
        ParamByName('AGENUS').AsInteger := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(1, NewName, [' ']));
        //ParamByName('ASUBFAMILY').AsInteger := Ssp.SubfamilyId;
        //ParamByName('AFAMILY').AsInteger := Ssp.FamilyId;
        //ParamByName('AORDER').AsInteger := Ssp.OrderId;
        ParamByName('AGENUSNAME').AsString := ExtractWord(1, NewName, [' ']);
        ParamByName('AEPITHET').AsString := ExtractWord(2, NewName, [' ']);
        if (btIOC in Taxonomies) then
        begin
          ParamByName('ANIVELIOC').AsInteger := SspRank;
          ParamByName('ASUPIOC').AsInteger := dsTaxa.DataSet.FieldByName('taxon_id').AsInteger;
        end;
        if (btCBRO in Taxonomies) then
        begin
          ParamByName('ANIVELCBRO').AsInteger := SspRank;
          ParamByName('ASUPCBRO').AsInteger := dsTaxa.DataSet.FieldByName('taxon_id').AsInteger;
        end;
        ParamByName('AUSER').AsInteger := AdminId;

        ExecSQL;
      finally
        FreeAndNil(Qry);
      end;
    end;
  finally
    FreeAndNil(dlgNewSubspecies);
    dsTaxa.DataSet.Refresh;
    if dsTaxa.DataSet.BookmarkValid(BM) then
      dsTaxa.DataSet.Bookmark := BM;
  end;
end;

procedure TfrmTaxaEditor.pmtSortClick(Sender: TObject);
begin
  FSearch.SortFields.Clear;

  SearchTaxa(eFindTaxa.Text);
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
    dsTaxa.DataSet.DisableControls;
    TaxonomyAction:= taLump;
    if ShowModal = mrOK then
    begin
      case ApplyTo of
        acSelected:
        begin
          MoveToGenus(dsTaxa.DataSet.FieldByName('taxon_id').AsInteger, Taxon, Taxonomies, ChangeSuffix, True);
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
              MoveToGenus(Qry.FieldByName('taxon_id').AsInteger, Taxon, Taxonomies, ChangeSuffix, True);
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
    dsTaxa.DataSet.EnableControls;
    dsTaxa.DataSet.Refresh;
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
    dsTaxa.DataSet.DisableControls;
    TaxonomyAction:= taLump;
    if ShowModal = mrOK then
    begin
      case ApplyTo of
        acSelected:
        begin
          MoveToSpecies(dsTaxa.DataSet.FieldByName('taxon_id').AsInteger, Taxon, Taxonomies, ChangeSuffix, True);
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
              MoveToSpecies(Qry.FieldByName('taxon_id').AsInteger, Taxon, Taxonomies, ChangeSuffix, True);
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
    dsTaxa.DataSet.EnableControls;
    dsTaxa.DataSet.Refresh;
    //if dsTaxa.DataSet.BookmarkValid(BM) then
    //  dsTaxa.DataSet.Bookmark := BM;
  end;
end;

procedure TfrmTaxaEditor.pTaxaListResize(Sender: TObject);
begin
  pFindTaxa.Width := pTaxaList.Width - pFindTaxa.Left;
end;

procedure TfrmTaxaEditor.rbMarkedYesClick(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  SearchTaxa(eFindTaxa.Text);
end;

procedure TfrmTaxaEditor.sbAdvancedFiltersClick(Sender: TObject);
begin
  dlgSqlFilter := TdlgSqlFilter.Create(nil);
  with dlgSqlFilter do
  try
    if ShowModal = mrOk then
    begin
      dsTaxa.DataSet.Close;
      TSQLQuery(dsTaxa.DataSet).SQL.Text := FilterText;
      dsTaxa.DataSet.Open;
    end;
  finally
    FreeAndNil(dlgSqlFilter);
  end;
end;

procedure TfrmTaxaEditor.sbCancelRecordClick(Sender: TObject);
begin
  dsTaxa.DataSet.Cancel;

  UpdateButtons(dsTaxa.DataSet);
end;

procedure TfrmTaxaEditor.sbClearFiltersClick(Sender: TObject);
begin
  FSearch.QuickFilters.Clear;
  ClearTaxaFilters;
  SearchTaxa(eFindTaxa.Text);

  UpdateButtons(dsTaxa.DataSet);
end;

procedure TfrmTaxaEditor.sbClearFindTaxaClick(Sender: TObject);
begin
  eFindTaxa.Clear;
end;

procedure TfrmTaxaEditor.sbDelRecordClick(Sender: TObject);
begin
  //DeleteRecord(tbZooTaxa, dsTaxa.DataSet);
  dsTaxa.DataSet.Edit;
  dsTaxa.DataSet.FieldByName('active_status').AsBoolean := False;
  dsTaxa.DataSet.Post;
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
  //etFullname.SetFocus;
end;

procedure TfrmTaxaEditor.sbLastRecordClick(Sender: TObject);
begin
  dsTaxa.DataSet.Last;
end;

procedure TfrmTaxaEditor.sbLumpTaxonClick(Sender: TObject);
var
  Qry: TSQLQuery;
  //BM: TBookmark;
begin
  dlgDestTaxon := TdlgDestTaxon.Create(nil);
  with dlgDestTaxon do
  try
    //BM := dsTaxa.DataSet.Bookmark;
    dsTaxa.DataSet.DisableControls;
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
    dsTaxa.DataSet.EnableControls;
    dsTaxa.DataSet.Refresh;
    //if dsTaxa.DataSet.BookmarkValid(BM) then
    //  dsTaxa.DataSet.Bookmark := BM;
  end;
end;

procedure TfrmTaxaEditor.sbMoveTaxonClick(Sender: TObject);
begin
  with sbMoveTaxon.ClientToScreen(point(0, sbMoveTaxon.Height + 1)) do
    pmMove.Popup(X, Y);
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
  if not ValidateTaxon then
    Exit;

  dmTaxa.qTaxa.Post;

  //dmTaxa.sqlTrans.CommitRetaining;

  //UpdateButtons(dsTaxa.DataSet);
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
    //BM := dsTaxa.DataSet.Bookmark;
    dsTaxa.DataSet.DisableControls;
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
    dsTaxa.DataSet.EnableControls;
    dsTaxa.DataSet.Refresh;
    //if dsTaxa.DataSet.BookmarkValid(BM) then
    //  dsTaxa.DataSet.Bookmark := BM;
  end;
end;

procedure TfrmTaxaEditor.TimerFindTimer(Sender: TObject);
begin
  TimerFind.Enabled := False;
  if not CanToggle then
    Exit;

  SearchTaxa(eFindTaxa.Text);
end;

procedure TfrmTaxaEditor.tsTaxonExtinctOff(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  SearchTaxa(eFindTaxa.Text);
end;

procedure TfrmTaxaEditor.tsTaxonExtinctOn(Sender: TObject);
begin
  if not CanToggle then
    Exit;

  SearchTaxa(eFindTaxa.Text);
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

  if tsTaxonomyClements.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('clements_taxonomy', 'Clements/eBird', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsTaxonomyIoc.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('ioc_taxonomy', 'IOC', sdtBoolean,
      crEqual, False, '1'));
  end;
  if tsTaxonomyCbro.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('cbro_taxonomy', 'CBRO', sdtBoolean,
      crEqual, False, '1'));
  end;

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

  if rbIsSynonymYes.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
      crDistinct, False, '0'));
  end;
  if rbIsSynonymNo.Checked then
  begin
    sf := FSearch.QuickFilters.Add(TSearchGroup.Create);
    FSearch.QuickFilters.Items[sf].Fields.Add(TSearchField.Create('valid_id', 'Valid name', sdtInteger,
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

  if pmtSortTaxonomic.Checked then
    AddSortedField('sort_num', sdAscending)
  else
  if pmtSortAlphabetical.Checked then
    AddSortedField('full_name', sdAscending);

  Result := FSearch.RunSearch > 0;

  Working := False;

  UpdateButtons(dsTaxa.DataSet);
end;

procedure TfrmTaxaEditor.UpdateButtons(aDataSet: TDataSet);
begin
  case aDataSet.State of
    dsInactive:
    begin
      sbInsertRecord.Enabled := False;
      sbEditRecord.Enabled := False;
      sbDelRecord.Enabled := False;

      sbSplitTaxon.Enabled := False;
      sbLumpTaxon.Enabled := False;
      sbMoveTaxon.Enabled := False;

      sbFirstRecord.Enabled := False;
      sbPriorRecord.Enabled := False;
      sbNextRecord.Enabled := False;
      sbLastRecord.Enabled := False;

      sbRefreshRecords.Enabled := False;
      sbSortRecords.Enabled := False;
      sbAdvancedFilters.Enabled := False;
      sbClearFilters.Enabled := False;
      sbMoreOptions.Enabled := False;

      sbShowQuickFilters.Enabled := True;
      sbShowImages.Enabled := True;
      sbShowRecycle.Enabled := True;

      sbCancelRecord.Visible := False;
      sbSaveRecord.Visible := False;

      //gridTaxa.Enabled := False;
      //pFindTaxa.Enabled := False;
      pTaxaRightBar.Enabled := True;
      sbFileMenu.Enabled := True;
      navTabs.Enabled := True;
    end;
    dsBrowse:
    begin
      sbInsertRecord.Enabled := True;
      sbEditRecord.Enabled := (aDataSet.RecordCount > 0);
      sbDelRecord.Enabled := (aDataSet.RecordCount > 0);

      sbSplitTaxon.Enabled := (aDataSet.RecordCount > 0);
      sbLumpTaxon.Enabled := (aDataSet.RecordCount > 0);
      sbMoveTaxon.Enabled := (aDataSet.RecordCount > 0);

      sbFirstRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
      sbPriorRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo > 1);
      sbNextRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);
      sbLastRecord.Enabled := (aDataSet.RecordCount > 1) and (aDataSet.RecNo < aDataSet.RecordCount);

      sbRefreshRecords.Enabled := True;
      sbSortRecords.Enabled := True;
      sbAdvancedFilters.Enabled := True;
      sbClearFilters.Enabled := FSearch.QuickFilters.Count > 0;
      sbMoreOptions.Enabled := True;

      sbShowQuickFilters.Enabled := True;
      sbShowImages.Enabled := True;
      sbShowRecycle.Enabled := True;

      sbSaveRecord.Visible := False;
      sbCancelRecord.Visible := False;

      //gridTaxa.Enabled := True;
      //pFindTaxa.Enabled := True;
      pTaxaRightBar.Enabled := True;
      sbFileMenu.Enabled := True;
      navTabs.Enabled := True;
    end;
    dsEdit, dsInsert:
    begin
      sbInsertRecord.Enabled := False;
      sbEditRecord.Enabled := False;
      sbDelRecord.Enabled := False;

      sbSplitTaxon.Enabled := False;
      sbLumpTaxon.Enabled := False;
      sbMoveTaxon.Enabled := False;

      sbFirstRecord.Enabled := False;
      sbPriorRecord.Enabled := False;
      sbNextRecord.Enabled := False;
      sbLastRecord.Enabled := False;

      sbRefreshRecords.Enabled := False;
      sbSortRecords.Enabled := False;
      sbAdvancedFilters.Enabled := False;
      sbClearFilters.Enabled := False;
      sbMoreOptions.Enabled := False;

      sbShowQuickFilters.Enabled := False;
      sbShowImages.Enabled := False;
      sbShowRecycle.Enabled := False;

      sbCancelRecord.Visible := True;
      sbSaveRecord.Visible := True;

      //gridTaxa.Enabled := False;
      //pFindTaxa.Enabled := False;
      pTaxaRightBar.Enabled := False;
      sbFileMenu.Enabled := False;
      navTabs.Enabled := False;
    end;
  end;

  pmgRefresh.Enabled := sbRefreshRecords.Enabled;
  pmgEdit.Enabled := sbEditRecord.Enabled;
  pmgDel.Enabled := sbDelRecord.Enabled;
  pmgNewSubspecies.Enabled := sbSplitTaxon.Enabled;
  pmgSplit.Enabled := sbSplitTaxon.Enabled;
  pmgLump.Enabled := sbLumpTaxon.Enabled;
  pmgMove.Enabled := sbMoveTaxon.Enabled;

  if dsTaxa.DataSet.RecordCount > 0 then
    lblCountTaxa.Caption := Format(rsRecordNumber, [dsTaxa.DataSet.RecNo, dsTaxa.DataSet.RecordCount])
  else
    lblCountTaxa.Caption := rsRecNoEmpty;
  lblTitleSynonyms.Caption := Format('Synonyms (%d)', [dsSynonyms.DataSet.RecordCount]);
  lblTitleChilds.Caption := Format('ChildTaxa (%d)', [dsChildTaxa.DataSet.RecordCount]);
end;

function TfrmTaxaEditor.ValidateTaxon: Boolean;
begin
  Result := True;

  //Result := RecordDuplicated(tbZooTaxa, 'taxon_id', 'full_name',
  //            dsTaxa.DataSet.FieldByName('full_name').AsString,
  //            dsTaxa.DataSet.FieldByName('taxon_id').AsInteger);
end;

end.

