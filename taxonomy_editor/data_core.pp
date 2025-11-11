unit data_core;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, DB, SQLDB;

const
  SCHEMA_VERSION: Integer = 2;

  function CreateTaxonomyDatabase(aFilename: String): Boolean;
  function UpgradeDatabaseSchema: Boolean;
  function ReadDatabaseMetadata(Connection: TSQLConnector; aKey: String): String;
  procedure WriteDatabaseMetadata(Connection: TSQLConnector; aKey, aValue: String);

  procedure CreateDBMetadataTable(Connection: TSQLConnector);
  procedure CreateCountriesTable(Connection: TSQLConnector);
  procedure CreateLanguagesTable(Connection: TSQLConnector);
  procedure CreateTaxonRanksTable(Connection: TSQLConnector);
  procedure CreateZooTaxaTable(Connection: TSQLConnector);
  procedure CreateVernacularNamesTable(Connection: TSQLConnector);
  procedure CreateSynonymsTable(Connection: TSQLConnector);
  procedure CreateTaxonCountriesTable(Connection: TSQLConnector);
  procedure CreatePackagesTable(Connection: TSQLConnector);
  procedure CreateTaxonChangesTable(Connection: TSQLConnector);

implementation

uses
  data_types, utils_dialogs, utils_global, udm_taxa, udlg_loading;

function CreateTaxonomyDatabase(aFilename: String): Boolean;
var
  Conn: TSQLConnector;
  Trans: TSQLTransaction;
begin
  Result := False;

  //LogEvent(leaStart, Format('Create database: %s', [aFilename]));
  //dlgProgress := TdlgProgress.Create(nil);
  Conn := TSQLConnector.Create(nil);
  Trans := TSQLTransaction.Create(nil);
  try
    dlgLoading.Show;
    dlgLoading.Min := 0;
    dlgLoading.Max := 10;
    dlgLoading.UpdateProgress('Creating database...', 0);
    //dlgProgress.Title := rsTitleCreateDatabase;
    //dlgProgress.Text := rsProgressPreparing;
    //dlgProgress.Max := 50; // Number of tables and views to create
    //dlgProgress.Position := 0;
    //dlgProgress.Show;
    Application.ProcessMessages;

    Conn.ConnectorType := 'SQLite3';
    Conn.CharSet := 'UTF-8';
    Conn.LoginPrompt := False;
    Conn.DatabaseName := aFileName;
    Conn.Transaction := Trans;
    Trans.DataBase := Conn;
    Trans.Action := caRollbackRetaining;

    //{$IFDEF DEBUG}
    //LogDebug('Creating database: ' + aFilename);
    //{$ENDIF}
    try
      Conn.Open;
      if not Trans.Active then
        Trans.StartTransaction;

      // Create database file
      //dlgProgress.Text := rsProgressPreparing;
      //Application.ProcessMessages;

      Conn.ExecuteDirect('PRAGMA foreign_keys = off;');

      try
        // >> Create tables
        // Database metadata
        //dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleDBMetadata, dlgProgress.Position + 1, dlgProgress.Max]);
        dlgLoading.UpdateProgress('Creating table: Database metadata...', dlgLoading.Progress + 1);
        Application.ProcessMessages;
        CreateDBMetadataTable(Conn);
        //dlgProgress.Position := dlgProgress.Position + 1;

        // Countries
        //dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleUsers, dlgProgress.Position + 1, dlgProgress.Max]);
        dlgLoading.UpdateProgress('Creating table: Countries...', dlgLoading.Progress + 1);
        Application.ProcessMessages;
        CreateCountriesTable(Conn);
        //dlgProgress.Position := dlgProgress.Position + 1;

        // Languages
        //dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleHistory, dlgProgress.Position + 1, dlgProgress.Max]);
        dlgLoading.UpdateProgress('Creating table: Languages...', dlgLoading.Progress + 1);
        Application.ProcessMessages;
        CreateLanguagesTable(Conn);
        //dlgProgress.Position := dlgProgress.Position + 1;

        // Taxon ranks
        //dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleTaxonRanks, dlgProgress.Position + 1, dlgProgress.Max]);
        dlgLoading.UpdateProgress('Creating table: Taxon ranks...', dlgLoading.Progress + 1);
        Application.ProcessMessages;
        CreateTaxonRanksTable(Conn);
        //dlgProgress.Position := dlgProgress.Position + 1;

        // Zoo taxa
        //dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleZooTaxa, dlgProgress.Position + 1, dlgProgress.Max]);
        dlgLoading.UpdateProgress('Creating table: Zoological taxa...', dlgLoading.Progress + 1);
        Application.ProcessMessages;
        CreateZooTaxaTable(Conn);
        //dlgProgress.Position := dlgProgress.Position + 1;

        // Vernacular names
        //dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleBotanicalTaxa, dlgProgress.Position + 1, dlgProgress.Max]);
        dlgLoading.UpdateProgress('Creating table: Vernacular names...', dlgLoading.Progress + 1);
        Application.ProcessMessages;
        CreateVernacularNamesTable(Conn);
        //dlgProgress.Position := dlgProgress.Position + 1;

        // Synonyms
        //dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleMethods, dlgProgress.Position + 1, dlgProgress.Max]);
        dlgLoading.UpdateProgress('Creating table: Synonyms...', dlgLoading.Progress + 1);
        Application.ProcessMessages;
        CreateSynonymsTable(Conn);
        //dlgProgress.Position := dlgProgress.Position + 1;

        // Taxon countries
        //dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleGazetteer, dlgProgress.Position + 1, dlgProgress.Max]);
        dlgLoading.UpdateProgress('Creating table: Taxon countries...', dlgLoading.Progress + 1);
        Application.ProcessMessages;
        CreateTaxonCountriesTable(Conn);
        //dlgProgress.Position := dlgProgress.Position + 1;

        // Packages
        //dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleInstitutions, dlgProgress.Position + 1, dlgProgress.Max]);
        dlgLoading.UpdateProgress('Creating table: Packages...', dlgLoading.Progress + 1);
        Application.ProcessMessages;
        CreatePackagesTable(Conn);
        //dlgProgress.Position := dlgProgress.Position + 1;

        // Taxon changes
        //dlgProgress.Text := Format(rsProgressCreatingTable, [rsTitleResearchers, dlgProgress.Position + 1, dlgProgress.Max]);
        dlgLoading.UpdateProgress('Creating table: Taxon changes...', dlgLoading.Progress + 1);
        Application.ProcessMessages;
        CreateTaxonChangesTable(Conn);
        //dlgProgress.Position := dlgProgress.Position + 1;

        // >> Create views
        // Next birthdays
        //dlgProgress.Text := Format(rsProgressCreatingView, [rsTitleNextBirthdays, dlgProgress.Position + 1, dlgProgress.Max]);
        //Application.ProcessMessages;
        //CreateNextBirthdaysView(Conn);
        //dlgProgress.Position := dlgProgress.Position + 1;

        Trans.CommitRetaining;
        if not Trans.Active then
        Trans.StartTransaction;

        // Populate tables
        //dlgProgress.Text := rsProgressPopulatingTables;
        //dlgProgress.PBar.Style := TProgressBarStyle.pbstMarquee;
        //Application.ProcessMessages;
        //LogDebug('Populating database');
        // Populate users (admin), taxon ranks, methods, botanic taxa
        //DMM.scriptUserDBInit.DataBase := Conn;
        //DMM.scriptUserDBInit.Transaction := Conn.Transaction;
        //DMM.scriptUserDBInit.ExecuteScript;

        //Trans.CommitRetaining;
        //if not Trans.Active then
        //Trans.StartTransaction;

        // Populate bird taxa
        //PopulateZooTaxaTable(Conn, dlgProgress.PBar);

        //Trans.CommitRetaining;
        //if not Trans.Active then
        //Trans.StartTransaction;

      finally
        Conn.ExecuteDirect('PRAGMA foreign_keys = on;');
      end;

      // Write metadata to the database
      //dlgProgress.Text := rsProgressFinishing;
      //dlgProgress.PBar.Style := TProgressBarStyle.pbstMarquee;
      dlgLoading.UpdateProgress('Finishing: Updating database metadata...', -1);
      Application.ProcessMessages;
      //LogDebug('Adding database metadata');
      WriteDatabaseMetadata(Conn, 'version', IntToStr(SCHEMA_VERSION));
      WriteDatabaseMetadata(Conn, 'creation date', DateTimeToStr(Now));

      Trans.CommitRetaining;

      // Optimize the database
      //dlgProgress.Text := rsProgressOptimizingDatabase;
      //Application.ProcessMessages;
      //LogDebug('Optimize database');
      //Conn.ExecuteDirect('PRAGMA optimize;');

      //dlgProgress.Hide;
      dlgLoading.Hide;

      //MsgDlg(rsTitleInformation, rsSuccessfulDatabaseCreation, mtInformation);
      //LogInfo(Format('User database succesfully created (SQLite): %s', [aFileName]));
      Result := True;
    except
      on E: Exception do
      begin
        Trans.RollbackRetaining;
        MsgDlg(rsTitleError, Format('Unable to create the taxonomy database: %s', [E.Message]), mtError);
        //LogError(Format('Unable to create the user database (SQLite): %s', [aFileName]));
        Result := False;
      end;
    end;

  finally
    FreeAndNil(Trans);
    FreeAndNil(Conn);
    //if Assigned(dlgProgress) then
    //  FreeAndNil(dlgProgress);
    //LogEvent(leaFinish, 'Create database');
  end;
end;

// Do not forget to update the Create...Table/View procedures
// and the SCHEMA_VERSION constant
function UpgradeDatabaseSchema: Boolean;
var
  OldVersion: Integer;
begin
  Result := False;

  OldVersion := StrToIntDef(ReadDatabaseMetadata(dmTaxa.sqlCon, 'version'), 1);

  if OldVersion = SCHEMA_VERSION then
    Exit;

  if not dmTaxa.sqlCon.Connected then
    dmTaxa.sqlCon.Open;

  //LogEvent(leaStart, 'Upgrade database schema');
  dmTaxa.sqlCon.ExecuteDirect('PRAGMA foreign_keys = off;');

  if not dmTaxa.sqlTrans.Active then
    dmTaxa.sqlTrans.StartTransaction;

  try
    if not dlgLoading.Visible then
      dlgLoading.Show;
    try
      if OldVersion < 2 then
      begin
        //LogDebug('Upgrading database to version 2');
        dlgLoading.UpdateProgress('Upgrading database schema to v2...', -1);

        dmTaxa.sqlCon.ExecuteDirect('ALTER TABLE zoo_taxa ADD COLUMN taxon_concept_id VARCHAR (30)');

        Result := True;
      end;

      //if OldVersion < 3 then
      //begin
      //  LogDebug(Format('Upgrading database to version %d', [OldVersion]));
      //
      //  Result := True;
      //end;

      if Result then
      begin
        dmTaxa.sqlTrans.CommitRetaining;
        //MsgDlg(rsTitleInformation, rsSuccessfulDatabaseUpgrade, mtInformation);
        //LogInfo('User database succesfully upgraded');
      end;
    except
      on E: Exception do
      begin
        dmTaxa.sqlTrans.RollbackRetaining;
        MsgDlg(rsTitleError, Format('Error upgrading the database schema: %s', [E.Message]), mtError);
        //LogError('Unable to upgrade the database schema');
        Result := False;
      end;
    end;

  finally
    dmTaxa.sqlCon.ExecuteDirect('PRAGMA foreign_keys = on;');
    //LogEvent(leaFinish, 'Upgrade database schema');
  end;

  if Result then
    WriteDatabaseMetadata(dmTaxa.sqlCon, 'version', IntToStr(SCHEMA_VERSION));
end;

function ReadDatabaseMetadata(Connection: TSQLConnector; aKey: String): String;
var
  Qry: TSQLQuery;
begin
  Result := EmptyStr;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := Connection;
    SQLTransaction := Connection.Transaction;

    Add('SELECT * FROM db_metadata');
    Add('WHERE property_name = :aname');
    ParamByName('aname').AsString := aKey;

    Open;
    if RecordCount > 0 then
      Result := FieldByName('property_value').AsString;

    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure WriteDatabaseMetadata(Connection: TSQLConnector; aKey, aValue: String);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    SQLConnection := Connection;
    SQLTransaction := Connection.Transaction;

    Add('INSERT OR REPLACE INTO db_metadata (property_name, property_value)');
    Add('VALUES (:aname, :avalue)');

    ParamByName('aname').AsString := aKey;
    ParamByName('avalue').AsString := aValue;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure CreateDBMetadataTable(Connection: TSQLConnector);
begin
  { Create table "db_metadata" }
  //LogDebug('Creating db_metadata table');
  Connection.ExecuteDirect('CREATE TABLE IF NOT EXISTS db_metadata (' +
        'property_name  VARCHAR (40)  PRIMARY KEY UNIQUE NOT NULL, ' +
        'property_value VARCHAR (150) );');
end;

procedure CreateCountriesTable(Connection: TSQLConnector);
begin
  //LogDebug('Creating users table');
  Connection.ExecuteDirect('CREATE TABLE IF NOT EXISTS countries (' +
    'country_id    INTEGER       PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'country_code  VARCHAR (5)   UNIQUE NOT NULL,' +
    'country_name  VARCHAR (100) UNIQUE NOT NULL,' +
    'insert_date   DATETIME,' +
    'update_date   DATETIME,' +
    'marked_status BOOLEAN       DEFAULT (0),' +
    'active_status BOOLEAN       DEFAULT (1)' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_country_code ON countries (' +
    'country_code COLLATE NOCASE' +
  ');');
end;

procedure CreateLanguagesTable(Connection: TSQLConnector);
begin
  //LogDebug('Creating users table');
  Connection.ExecuteDirect('CREATE TABLE IF NOT EXISTS languages (' +
    'language_id        INTEGER       PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'macrolanguage_code VARCHAR (3)   NOT NULL,' +
    'country_code       VARCHAR (3),' +
    'variation_code     VARCHAR (10),' +
    'language_name      VARCHAR (100) UNIQUE NOT NULL,' +
    'insert_date        DATETIME      DEFAULT (DATETIME(''now'', ''localtime'') ),' +
    'update_date        DATETIME      DEFAULT (DATETIME(''now'', ''localtime'') ),' +
    'marked_status      BOOLEAN       DEFAULT (0),' +
    'active_status      BOOLEAN       DEFAULT (1)' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_language_code ON languages (' +
    'macrolanguage_code COLLATE NOCASE,' +
    'country_code COLLATE NOCASE,' +
    'variation_code COLLATE NOCASE' +
  ');');
end;

procedure CreateTaxonRanksTable(Connection: TSQLConnector);
begin
  //LogDebug('Creating users table');
  Connection.ExecuteDirect('CREATE TABLE IF NOT EXISTS taxon_ranks (' +
    'rank_id         INTEGER      UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'rank_seq        INTEGER      NOT NULL,' +
    'rank_name       VARCHAR (30) NOT NULL UNIQUE,' +
    'rank_acronym    VARCHAR (15),' +
    'zoological_code BOOLEAN      DEFAULT (1),' +
    'botanical_code  BOOLEAN      DEFAULT (0),' +
    'main_rank       BOOLEAN      DEFAULT (1),' +
    'subrank         BOOLEAN      DEFAULT (0),' +
    'infrarank       BOOLEAN      DEFAULT (0),' +
    'infraspecific   BOOLEAN      DEFAULT (0),' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'marked_status   BOOLEAN      DEFAULT (0),' +
    'active_status   BOOLEAN      DEFAULT (1)' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_rank_abbreviation ON taxon_ranks (' +
    'rank_acronym COLLATE NOCASE' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_rank_name ON taxon_ranks (' +
    'rank_name COLLATE NOCASE' +
  ');');
end;

procedure CreateZooTaxaTable(Connection: TSQLConnector);
begin
  //LogDebug('Creating users table');
  Connection.ExecuteDirect('CREATE TABLE IF NOT EXISTS zoo_taxa (' +
    'taxon_id               INTEGER       UNIQUE PRIMARY KEY AUTOINCREMENT NOT NULL,' +
    'full_name              VARCHAR (100) NOT NULL UNIQUE,' +
    'authorship             VARCHAR (150),' +
    'formatted_name         VARCHAR (250),' +
    'taxon_concept_id       VARCHAR (30),' +
    'quick_code             VARCHAR (10),' +
    'rank_id                INTEGER       NOT NULL REFERENCES taxon_ranks (rank_id) ON UPDATE CASCADE,' +
    'parent_taxon_id        INTEGER,' +
    //'valid_id               INTEGER,' +
    'iucn_status            VARCHAR (5),' +
    'extinct                BOOLEAN       DEFAULT (0),' +
    'extinction_year        VARCHAR (25),' +
    'sort_num               REAL,' +
    'group_name             VARCHAR (40),' +
    'order_id               INTEGER,' +
    'family_id              INTEGER,' +
    'subfamily_id           INTEGER,' +
    'genus_id               INTEGER,' +
    'species_id             INTEGER,' +
    'subspecies_group_id    INTEGER,' +
    'incertae_sedis         INTEGER,' +
    'ebird_code             VARCHAR (20),' +
    'distribution           TEXT,' +
    'insert_date            DATETIME,' +
    'update_date            DATETIME,' +
    'marked_status          BOOLEAN       DEFAULT (0),' +
    'active_status          BOOLEAN       DEFAULT (1)' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_authorship ON zoo_taxa (' +
    'authorship COLLATE NOCASE' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_ebird_code ON zoo_taxa (' +
    'ebird_code COLLATE BINARY' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_order ON zoo_taxa (' +
    'order_id COLLATE BINARY' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_family ON zoo_taxa (' +
    'family_id COLLATE BINARY' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_genus ON zoo_taxa (' +
    'genus_id COLLATE BINARY' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_species ON zoo_taxa (' +
    'species_id COLLATE BINARY' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_subspecies_group ON zoo_taxa (' +
    'subspecies_group_id COLLATE BINARY' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_parent_taxon ON zoo_taxa (' +
    'parent_taxon_id COLLATE BINARY' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_valid ON zoo_taxa (' +
    'valid_id COLLATE BINARY' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_quickcodes ON zoo_taxa (' +
    'quick_code COLLATE BINARY' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_rank ON zoo_taxa (' +
    'rank_id COLLATE BINARY' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_zoo_taxa_sort_num ON zoo_taxa (' +
    'sort_num COLLATE BINARY' +
  ');');
end;

procedure CreateVernacularNamesTable(Connection: TSQLConnector);
begin
  //LogDebug('Creating users table');
  Connection.ExecuteDirect('CREATE TABLE IF NOT EXISTS vernacular_names (' +
    'vernacular_id   INTEGER       PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'taxon_id        INTEGER       REFERENCES zoo_taxa (taxon_id) ON DELETE CASCADE,' +
    'language_id     INTEGER       REFERENCES languages (language_id) ON UPDATE CASCADE,' +
    'vernacular_name VARCHAR (150) NOT NULL,' +
    'preferred       BOOLEAN       DEFAULT (0),' +
    'insert_date     DATETIME,' +
    'update_date     DATETIME,' +
    'marked_status   BOOLEAN       DEFAULT (0),' +
    'active_status   BOOLEAN       DEFAULT (1)' +
  ');');

  //Connection.ExecuteDirect('CREATE INDEX idx_country_code ON countries (' +
  //  'country_code COLLATE NOCASE' +
  //');');
end;

procedure CreateSynonymsTable(Connection: TSQLConnector);
begin
  //LogDebug('Creating users table');
  Connection.ExecuteDirect('CREATE TABLE IF NOT EXISTS zoo_taxa_synonyms (' +
    'synonym_id    INTEGER       PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'taxon_id      INTEGER       REFERENCES zoo_taxa (taxon_id) ON DELETE CASCADE ON UPDATE CASCADE NOT NULL,' +
    'full_name     VARCHAR (100) NOT NULL,' +
    'insert_date   DATETIME,' +
    'update_date   DATETIME,' +
    'marked_status BOOLEAN       DEFAULT (0),' +
    'active_status BOOLEAN       DEFAULT (1)' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_synonym_name ON zoo_taxa_synonyms (' +
    'full_name COLLATE NOCASE' +
  ');');
end;

procedure CreateTaxonCountriesTable(Connection: TSQLConnector);
begin
  //LogDebug('Creating users table');
  Connection.ExecuteDirect('CREATE TABLE IF NOT EXISTS zoo_taxa_countries (' +
    'taxon_country_id INTEGER  PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'taxon_id         INTEGER  REFERENCES zoo_taxa (taxon_id) ON DELETE CASCADE ON UPDATE CASCADE NOT NULL,' +
    'country_id       INTEGER  REFERENCES countries (country_id) ON UPDATE CASCADE NOT NULL,' +
    'insert_date      DATETIME,' +
    'update_date      DATETIME,' +
    'marked_status    BOOLEAN       DEFAULT (0),' +
    'active_status    BOOLEAN       DEFAULT (1)' +
  ');');

  //Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_country_code ON countries (' +
  //  'country_code COLLATE NOCASE' +
  //');');
end;

procedure CreatePackagesTable(Connection: TSQLConnector);
begin
  //LogDebug('Creating users table');
  Connection.ExecuteDirect('CREATE TABLE IF NOT EXISTS packages (' +
    'package_id     INTEGER      PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'package_name   VARCHAR (80) NOT NULL,' +
    'package_year   INTEGER,' +
    'package_month  INTEGER,' +
    'version        VARCHAR (30),' +
    'pending_status BOOLEAN,' +
    'insert_date    DATETIME,' +
    'update_date    DATETIME,' +
    'marked_status  BOOLEAN       DEFAULT (0),' +
    'active_status  BOOLEAN       DEFAULT (1)' +
  ');');

  Connection.ExecuteDirect('CREATE INDEX IF NOT EXISTS idx_package_name ON packages (' +
    'package_name,' +
    'package_year,' +
    'package_month' +
  ');');
end;

procedure CreateTaxonChangesTable(Connection: TSQLConnector);
begin
  //LogDebug('Creating users table');
  Connection.ExecuteDirect('CREATE TABLE IF NOT EXISTS taxon_changes (' +
    'change_id      INTEGER       PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,' +
    'package_id     INTEGER       REFERENCES packages (package_id) ON DELETE RESTRICT ON UPDATE CASCADE,' +
    'action_type    VARCHAR (10)  NOT NULL,' +
    'taxon_name     VARCHAR (120),' +
    'taxon_new_name VARCHAR (120),' +
    'trait_name     VARCHAR (50),' +
    'new_value      VARCHAR (250),' +
    'insert_date   DATETIME,' +
    'update_date   DATETIME,' +
    'marked_status BOOLEAN       DEFAULT (0),' +
    'active_status BOOLEAN       DEFAULT (1)' +
  ');');

  //Connection.ExecuteDirect('CREATE INDEX idx_country_code ON countries (' +
  //  'country_code COLLATE NOCASE' +
  //');');
end;

end.

