unit udm_taxa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, SQLDBLib, DB, LResources, Forms, Dialogs, UniqueInstance, Controls, SQLite3Conn;

type

  { TdmTaxa }

  TdmTaxa = class(TDataModule)
    dbLibLoader: TSQLDBLibraryLoader;
    dsChildTaxa: TDataSource;
    dsCountries: TDataSource;
    dsLanguages: TDataSource;
    dslookRanks: TDataSource;
    dsPacks: TDataSource;
    dsRanks: TDataSource;
    dsSynonyms: TDataSource;
    dsTaxa: TDataSource;
    dsTaxaChanges: TDataSource;
    dsTaxonCountries: TDataSource;
    dsVernacular: TDataSource;
    lookRanks: TSQLQuery;
    lookRanksrank_id: TLongintField;
    lookRanksrank_name: TStringField;
    qChildTaxa: TSQLQuery;
    qChildTaxaformatted_name: TStringField;
    qChildTaxafull_name: TStringField;
    qCountries: TSQLQuery;
    qCountriesactive_status: TBooleanField;
    qCountriescountry_code: TStringField;
    qCountriescountry_id: TLongintField;
    qCountriescountry_name: TStringField;
    qCountriesinsert_date: TDateTimeField;
    qCountriesmarked_status: TBooleanField;
    qCountriesupdate_date: TDateTimeField;
    qLanguages: TSQLQuery;
    qLanguagesactive_status: TBooleanField;
    qLanguagescountry_code: TStringField;
    qLanguagesinsert_date: TDateTimeField;
    qLanguageslanguage_id: TLongintField;
    qLanguageslanguage_name: TStringField;
    qLanguagesmacrolanguage_code: TStringField;
    qLanguagesmarked_status: TBooleanField;
    qLanguagesupdate_date: TDateTimeField;
    qLanguagesvariation_code: TStringField;
    qPacks: TSQLQuery;
    qPacksactive_status: TBooleanField;
    qPacksinsert_date: TDateTimeField;
    qPacksmarked_status: TBooleanField;
    qPackspackage_id: TLongintField;
    qPackspackage_month: TLongintField;
    qPackspackage_name: TStringField;
    qPackspackage_year: TLongintField;
    qPackspending_status: TBooleanField;
    qPacksupdate_date: TDateTimeField;
    qPacksversion: TStringField;
    qRanks: TSQLQuery;
    qRanksactive_status: TBooleanField;
    qRanksbotanical_code: TBooleanField;
    qRanksinfrarank: TBooleanField;
    qRanksinfraspecific: TBooleanField;
    qRanksinsert_date: TDateTimeField;
    qRanksmain_rank: TBooleanField;
    qRanksmarked_status: TBooleanField;
    qRanksrank_acronym: TStringField;
    qRanksrank_id: TLongintField;
    qRanksrank_name: TStringField;
    qRanksrank_seq: TLongintField;
    qRankssubrank: TBooleanField;
    qRanksupdate_date: TDateTimeField;
    qRankszoological_code: TBooleanField;
    qSynonyms: TSQLQuery;
    qSynonymsactive_status: TBooleanField;
    qSynonymsfull_name: TStringField;
    qSynonymsinsert_date: TDateTimeField;
    qSynonymsmarked_status: TBooleanField;
    qSynonymssynonym_id: TLongintField;
    qSynonymstaxon_id: TLongintField;
    qSynonymsupdate_date: TDateTimeField;
    qTaxa: TSQLQuery;
    qTaxaaccepted_status: TBooleanField;
    qTaxaactive_status: TBooleanField;
    qTaxaauthorship: TStringField;
    qTaxaChanges: TSQLQuery;
    qTaxaChangesaction_type: TStringField;
    qTaxaChangesactive_status: TBooleanField;
    qTaxaChangeschange_id: TLongintField;
    qTaxaChangesinsert_date: TDateTimeField;
    qTaxaChangesmarked_status: TBooleanField;
    qTaxaChangesnew_value: TStringField;
    qTaxaChangespackage_id: TLongintField;
    qTaxaChangestaxon_name: TStringField;
    qTaxaChangestaxon_new_name: TStringField;
    qTaxaChangestrait_name: TStringField;
    qTaxaChangesupdate_date: TDateTimeField;
    qTaxadistribution: TMemoField;
    qTaxaebird_code: TStringField;
    qTaxaextinct: TBooleanField;
    qTaxaextinction_year: TStringField;
    qTaxafamily_id: TLongintField;
    qTaxaformatted_name: TStringField;
    qTaxafull_name: TStringField;
    qTaxagenus_id: TLongintField;
    qTaxaincertae_sedis: TLongintField;
    qTaxainsert_date: TDateTimeField;
    qTaxaiucn_status: TStringField;
    qTaxamarked_status: TBooleanField;
    qTaxaorder_id: TLongintField;
    qTaxaparent_taxon_id: TLongintField;
    qTaxaparent_taxon_name: TStringField;
    qTaxaquick_code: TStringField;
    qTaxarank_id: TLongintField;
    qTaxasort_num: TFloatField;
    qTaxaspecies_id: TLongintField;
    qTaxasubfamily_id: TLongintField;
    qTaxasubspecies_group_id: TLongintField;
    qTaxataxon_id: TLongintField;
    qTaxaupdate_date: TDateTimeField;
    qTaxonCountries: TSQLQuery;
    qTaxonCountriesactive_status: TBooleanField;
    qTaxonCountriescountry_id: TLongintField;
    qTaxonCountriescountry_name: TStringField;
    qTaxonCountriesinsert_date: TDateTimeField;
    qTaxonCountriesmarked_status: TBooleanField;
    qTaxonCountriestaxon_country_id: TLongintField;
    qTaxonCountriestaxon_id: TLongintField;
    qTaxonCountriesupdate_date: TDateTimeField;
    qVernacular: TSQLQuery;
    qVernacularactive_status: TBooleanField;
    qVernacularinsert_date: TDateTimeField;
    qVernacularlanguage_id: TLongintField;
    qVernacularlanguage_name: TStringField;
    qVernacularmarked_status: TBooleanField;
    qVernacularpreferred: TBooleanField;
    qVernaculartaxon_id: TLongintField;
    qVernacularupdate_date: TDateTimeField;
    qVernacularvernacular_id: TLongintField;
    qVernacularvernacular_name: TStringField;
    sqlCon: TSQLConnector;
    sqlTrans: TSQLTransaction;
    TaskDlg: TTaskDialog;
    UniqueInstance1: TUniqueInstance;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure qCountriesBeforePost(DataSet: TDataSet);
    procedure qLanguagesBeforePost(DataSet: TDataSet);
    procedure qPacksBeforePost(DataSet: TDataSet);
    procedure qRanksBeforePost(DataSet: TDataSet);
    procedure qSynonymsBeforePost(DataSet: TDataSet);
    procedure qTaxaAfterInsert(DataSet: TDataSet);
    procedure qTaxaAfterOpen(DataSet: TDataSet);
    procedure qTaxaBeforeClose(DataSet: TDataSet);
    procedure qTaxaBeforePost(DataSet: TDataSet);
    procedure qTaxaChangesBeforePost(DataSet: TDataSet);
    procedure qTaxaiucn_statusGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qTaxaiucn_statusSetText(Sender: TField; const aText: string);
    procedure qTaxonCountriesBeforePost(DataSet: TDataSet);
    procedure qVernacularBeforePost(DataSet: TDataSet);
    procedure UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer;
      const Parameters: array of String);
  private

  public

  end;

var
  dmTaxa: TdmTaxa;

implementation

uses
  utils_global, utils_dialogs, data_types, uedt_database;

{ TdmTaxa }

procedure TdmTaxa.DataModuleCreate(Sender: TObject);
var
  canOpen: Boolean;
begin
  dbLibLoader.LibraryName := ConcatPaths([InstallDir, SQLITE_LIBRARY]);
  dbLibLoader.Enabled := True;

  canOpen := False;

  databaseConnection.LoadParams;

  if databaseConnection.Database = EmptyStr then
  begin
    edtDatabase := TedtDatabase.Create(nil);
    try
      edtDatabase.IsNew := True;
      if edtDatabase.ShowModal = mrOK then
        canOpen := True;
    finally
      FreeAndNil(edtDatabase);
    end;
  end
  else
    canOpen := True;

  if canOpen then
  begin
    sqlCon.DatabaseName := databaseConnection.Database;
    //sqlCon.UserName := databaseConnection.UserName;
    //sqlCon.Password := databaseConnection.Password;
    sqlCon.Open;
  end
  else
  begin
    MsgDlg('Error', 'Cannot open the database. The application will be terminated.', mtError);
    Application.Terminate;
  end;
end;

procedure TdmTaxa.DataModuleDestroy(Sender: TObject);
begin
  if sqlCon.Connected then
    sqlCon.Close;
end;

procedure TdmTaxa.qCountriesBeforePost(DataSet: TDataSet);
begin
  if DataSet.State = dsInsert then
    DataSet.FieldByName('insert_date').AsDateTime := Now;
  DataSet.FieldByName('update_date').AsDateTime := Now;
end;

procedure TdmTaxa.qLanguagesBeforePost(DataSet: TDataSet);
begin
  if DataSet.State = dsInsert then
    DataSet.FieldByName('insert_date').AsDateTime := Now;
  DataSet.FieldByName('update_date').AsDateTime := Now;
end;

procedure TdmTaxa.qPacksBeforePost(DataSet: TDataSet);
begin
  if DataSet.State = dsInsert then
    DataSet.FieldByName('insert_date').AsDateTime := Now;
  DataSet.FieldByName('update_date').AsDateTime := Now;
end;

procedure TdmTaxa.qRanksBeforePost(DataSet: TDataSet);
begin
  if DataSet.State = dsInsert then
    DataSet.FieldByName('insert_date').AsDateTime := Now;
  DataSet.FieldByName('update_date').AsDateTime := Now;
end;

procedure TdmTaxa.qSynonymsBeforePost(DataSet: TDataSet);
begin
  if DataSet.State = dsInsert then
    DataSet.FieldByName('insert_date').AsDateTime := Now;
  DataSet.FieldByName('update_date').AsDateTime := Now;
end;

procedure TdmTaxa.qTaxaAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('extinct').AsBoolean:= False;
end;

procedure TdmTaxa.qTaxaAfterOpen(DataSet: TDataSet);
begin
  qSynonyms.Open;
  qChildTaxa.Open;
  qVernacular.Open;
  qTaxonCountries.Open;
end;

procedure TdmTaxa.qTaxaBeforeClose(DataSet: TDataSet);
begin
  qTaxonCountries.Close;
  qVernacular.Close;
  qChildTaxa.Close;
  qSynonyms.Close;
end;

procedure TdmTaxa.qTaxaBeforePost(DataSet: TDataSet);
begin
  if DataSet.State = dsInsert then
    DataSet.FieldByName('insert_date').AsDateTime := Now;
  DataSet.FieldByName('update_date').AsDateTime := Now;
end;

procedure TdmTaxa.qTaxaChangesBeforePost(DataSet: TDataSet);
begin
  if DataSet.State = dsInsert then
    DataSet.FieldByName('insert_date').AsDateTime := Now;
  DataSet.FieldByName('update_date').AsDateTime := Now;
end;

procedure TdmTaxa.qTaxaiucn_statusGetText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if Sender.AsString = EmptyStr then
    Exit;

  case Sender.AsString of
    'LC': aText := 'Least Concern';
    'NT': aText := 'Near Threatened';
    'VU': aText := 'Vulnerable';
    'EN': aText := 'Endangered';
    'CR': aText := 'Critically Endangered';
    'EW': aText := 'Extinct in the Wild';
    'EX': aText := 'Extinct';
    'DD': aText := 'Data Deficient';
    'NE': aText := 'Not Evaluated';
  end;

  DisplayText := True;
end;

procedure TdmTaxa.qTaxaiucn_statusSetText(Sender: TField; const aText: string);
begin
  if aText = EmptyStr then
    Exit;

  case aText of
    'Least Concern':          Sender.AsString := 'LC';
    'Near Threatened':        Sender.AsString := 'NT';
    'Vulnerable':             Sender.AsString := 'VU';
    'Endangered':             Sender.AsString := 'EN';
    'Critically Endangered':  Sender.AsString := 'CR';
    'Extinct in the Wild':    Sender.AsString := 'EW';
    'Extinct':                Sender.AsString := 'EX';
    'Data Deficient':         Sender.AsString := 'DD';
    'Not Evaluated':          Sender.AsString := 'NE';
  end;
end;

procedure TdmTaxa.qTaxonCountriesBeforePost(DataSet: TDataSet);
begin
  if DataSet.State = dsInsert then
    DataSet.FieldByName('insert_date').AsDateTime := Now;
  DataSet.FieldByName('update_date').AsDateTime := Now;
end;

procedure TdmTaxa.qVernacularBeforePost(DataSet: TDataSet);
begin
  if DataSet.State = dsInsert then
    DataSet.FieldByName('insert_date').AsDateTime := Now;
  DataSet.FieldByName('update_date').AsDateTime := Now;
end;

procedure TdmTaxa.UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer;
  const Parameters: array of String);
begin
  Application.Restore;
  Application.BringToFront;
end;

initialization
  {$I udm_taxa.lrs}

end.

