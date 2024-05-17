unit udm_taxa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Dialogs, Controls, UniqueInstance, SQLDBLib, SQLDB, DB, SdfData,
  SQLite3Conn;

type

  { TdmTaxa }

  TdmTaxa = class(TDataModule)
    dbLibLoader: TSQLDBLibraryLoader;
    dslookRanks: TDataSource;
    dslookAuthors: TDataSource;
    iLogos: TImageList;
    lookAuthorsauthorship: TStringField;
    lookRanks: TSQLQuery;
    lookAuthors: TSQLQuery;
    lookRanksrank_id: TLongintField;
    lookRanksrank_name: TStringField;
    qChildTaxa: TSQLQuery;
    qChildTaxaformatted_name: TStringField;
    qChildTaxafull_name: TStringField;
    qPacks: TSQLQuery;
    qPacksactive_status: TBooleanField;
    qPacksinsert_date: TDateTimeField;
    qPacksmarked_status: TBooleanField;
    qPackspackage_id: TLongintField;
    qPackspackage_month: TLongintField;
    qPackspackage_name: TStringField;
    qPackspackage_year: TLongintField;
    qPackstaxonomy: TStringField;
    qPacksupdate_date: TDateTimeField;
    qPacksversion: TStringField;
    qRanks: TSQLQuery;
    qRanksactive_status: TBooleanField;
    qRanksbotanical_code: TBooleanField;
    qRanksexported_status: TBooleanField;
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
    qRanksuser_inserted: TLongintField;
    qRanksuser_updated: TLongintField;
    qRankszoological_code: TBooleanField;
    qSynonymTaxa: TSQLQuery;
    qSynonymTaxaformatted_name: TStringField;
    qSynonymTaxafull_name: TStringField;
    qSynonymTaxavalid_id: TLargeintField;
    qTaxa: TSQLQuery;
    qTaxaactive_status: TBooleanField;
    qTaxaauthorship: TStringField;
    qTaxacbro_taxonomy: TBooleanField;
    qTaxaclements_taxonomy: TBooleanField;
    qTaxadistribution: TMemoField;
    qTaxaebird_code: TStringField;
    qTaxaenglish_name: TStringField;
    qTaxaexported_status: TBooleanField;
    qTaxaextinct: TBooleanField;
    qTaxaextinction_year: TStringField;
    qTaxafamily_id: TLongintField;
    qTaxaformatted_name: TStringField;
    qTaxafull_name: TStringField;
    qTaxagenus_id: TLongintField;
    qTaxagroup_name: TStringField;
    qTaxaincertae_sedis: TLongintField;
    qTaxainsert_date: TDateTimeField;
    qTaxaioc_distribution: TMemoField;
    qTaxaioc_english_name: TStringField;
    qTaxaioc_parent_taxon_id: TLongintField;
    qTaxaioc_parent_taxon_name: TStringField;
    qTaxaioc_rank_id: TLongintField;
    qTaxaioc_sort_num: TFloatField;
    qTaxaioc_taxonomy: TBooleanField;
    qTaxaioc_valid_id: TLongintField;
    qTaxaioc_valid_name: TStringField;
    qTaxaiucn_status: TStringField;
    qTaxamarked_status: TBooleanField;
    qTaxaorder_id: TLongintField;
    qTaxaother_portuguese_names: TStringField;
    qTaxaparent_taxon_id: TLongintField;
    qTaxaparent_taxon_name: TStringField;
    qTaxaportuguese_name: TStringField;
    qTaxaquick_code: TStringField;
    qTaxarank_id: TLongintField;
    qTaxasort_num: TFloatField;
    qTaxaspanish_name: TStringField;
    qTaxaspecies_id: TLongintField;
    qTaxasubfamily_id: TLongintField;
    qTaxasubspecies_group_id: TLongintField;
    qTaxataxon_id: TLongintField;
    qTaxaUpdates: TSQLQuery;
    qTaxaUpdatesaction_type: TStringField;
    qTaxaUpdatesactive_status: TBooleanField;
    qTaxaUpdateschange_id: TLongintField;
    qTaxaUpdatesinsert_date: TDateTimeField;
    qTaxaUpdatesmarked_status: TBooleanField;
    qTaxaUpdatesnew_value: TStringField;
    qTaxaUpdatespackage_id: TLongintField;
    qTaxaUpdatestaxon_name: TStringField;
    qTaxaUpdatestaxon_new_name: TStringField;
    qTaxaUpdatestrait_name: TStringField;
    qTaxaUpdatesupdate_date: TDateTimeField;
    qTaxaupdate_date: TDateTimeField;
    qTaxauser_inserted: TLongintField;
    qTaxauser_updated: TLongintField;
    qTaxavalid_id: TLongintField;
    qTaxavalid_name: TStringField;
    sqlCon: TSQLConnector;
    sqlTrans: TSQLTransaction;
    TaskDlg: TTaskDialog;
    UniqueInstance1: TUniqueInstance;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure qTaxaAfterInsert(DataSet: TDataSet);
    procedure qTaxaAfterOpen(DataSet: TDataSet);
    procedure qTaxaBeforeClose(DataSet: TDataSet);
    procedure qTaxaiucn_statusGetText(Sender: TField; var aText: string; DisplayText: Boolean);
    procedure qTaxaiucn_statusSetText(Sender: TField; const aText: string);
    procedure UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer;
      const Parameters: array of String);
  private

  public

  end;

var
  dmTaxa: TdmTaxa;

implementation

uses lib_taxa;

{ TdmTaxa }

procedure TdmTaxa.DataModuleCreate(Sender: TObject);
begin
  dbLibLoader.LibraryName := ConcatPaths([InstallDir, 'sqlite3.dll']);
  dbLibLoader.Enabled := True;
  sqlCon.Open;
end;

procedure TdmTaxa.DataModuleDestroy(Sender: TObject);
begin
  sqlCon.Close;
end;

procedure TdmTaxa.qTaxaAfterInsert(DataSet: TDataSet);
begin
  DataSet.FieldByName('extinct').AsBoolean:= False;
  DataSet.FieldByName('clements_taxonomy').AsBoolean:= False;
  DataSet.FieldByName('ioc_taxonomy').AsBoolean:= False;
  DataSet.FieldByName('cbro_taxonomy').AsBoolean:= False;
end;

procedure TdmTaxa.qTaxaAfterOpen(DataSet: TDataSet);
begin
  qSynonymTaxa.Open;
  qChildTaxa.Open;
end;

procedure TdmTaxa.qTaxaBeforeClose(DataSet: TDataSet);
begin
  qChildTaxa.Close;
  qSynonymTaxa.Close;
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

procedure TdmTaxa.UniqueInstance1OtherInstance(Sender: TObject; ParamCount: Integer;
  const Parameters: array of String);
begin
  Application.Restore;
  Application.BringToFront;
end;

initialization
  {$I udm_taxa.lrs}

end.

