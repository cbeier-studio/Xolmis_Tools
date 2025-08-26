unit utils_global;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, DB, SQLDB, StrUtils;

const
  APP_NAME: String          = 'Xolmis Taxonomy Editor';
  PRE_RELEASE_STAGE: String = 'Alpha';
  DEFAULT_SETTINGS_FILE     = 'settings.json';
  SQLITE_LIBRARY            = 'sqlite3.dll';

  NULL_DATE_STR: String           = '30/12/1500';
  NULL_TIME_STR: String           = '00:00:00';

{ Hashtags }
const
  AllQS: array of String        = ('#tudo', '#all');
  MarkedQS: array of String     = ('#marcados', '#marked');
  UnmarkedQS: array of String   = ('#naomarcados', '#unmarked');
  FilterQS: array of String     = ('#filtro', '#filter');
  DeletedQS: array of String    = ('#lixo', '#deleted');
  PrintQueueQS: array of String = ('#fila', '#queued', '#toprint');
  OrderQS: array of String      = ('#ordem', '#order', '#ord');
  FamilyQS: array of String     = ('#familia', '#family', '#fam');
  GenusQS: array of String      = ('#genero', '#genus', '#gen');
  SpeciesQS: array of String    = ('#especie', '#species', '#sp');
  SiteQS: array of String       = ('#local', '#site');
  QualifierQS: array of String  = ('#quali', '#qualifier');
  ParentQS: array of String     = ('#superior', '#parent');
  RankQS: array of String       = ('#nivel', '#categoria', '#rank');
  ListsQS: array of String      = ('#listas', '#lists');
  SqlQS: array of String        = ('#sql', '#sqlfilter');

  { System variables }
  function InstallDir: String;
  function AppDataDir: String;
  function TempDir: String;
  function NullDate: TDate;
  function NullTime: TTime;
  function NullDateTime: TDateTime;

  procedure LoadDatabaseParams(aConnector: TSQLConnector);

var
  Closing: Boolean;
  Parar: Boolean;

resourcestring
  rsTitleError = 'Error';
  rsTitleConfirmation = 'Confirmation';
  rsTitleInformation = 'Information';
  rsTitleCaution = 'Warning';
  rsTitleImportFile = 'Import file';
  rsCaptionFind = 'Find';
  rsErrorTableNotFound = 'Table %s not found.';
  rsErrorFileNotFound = 'File %s not found.';
  rsErrorFileIsEmpty = 'Selected file is empty.';
  rsErrorRewritingHierarchy = 'An error occurred during hierarchy rewriting. All changes will be reverted back.';
  rsActiveRecordDuplicated = 'A record with the same %s value already exists (%s).';
  rsInactiveRecordDuplicated = 'An inactive record with the same %s value already exists (%s).';
  rsSuccessfulImport = 'The selected file was sucessfully imported.';
  rsImportCanceledByUser = 'Import canceled by the user.';
  rsDeleteRecordTitle = 'Delete record';
  rsDeleteRecordPrompt = 'Do you really want to delete this record?';
  rsDeleteRecordFooter = 'Deleted record stay in recycle bin for the time period defined in ' +
    'Settings before being permanently deleted.';
  rsRestoreRecordTitle = 'Restore record';
  rsRestoreRecordPrompt = 'Do you really want to restore this record?';
  rsWorkingOnIt = 'Working on it...';
  rsRecordNumber = 'record %d of %d';
  rsRecNoEmpty = 'record 0 of 0';

implementation

uses
  data_types, udlg_find, udm_taxa, ufrm_taxaeditor;

{ ---------------------------------------------------------------------------------------- }
{ System variables }
{ ---------------------------------------------------------------------------------------- }

// Get the application/installation path
function InstallDir: String;
var
  s: String;
begin
  s := ExtractFilePath(Application.ExeName);
  s := IncludeTrailingPathDelimiter(s);

  Result := s;
end;

// Get the Xolmis' AppData path
function AppDataDir: String;
var
  s: String;
begin
  s := GetAppConfigDir(False);
  s := IncludeTrailingPathDelimiter(s);

  Result:= s;
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

// Get the Xolmis' Temp path
function TempDir: String;
var
  s: String;
begin
  s := ConcatPaths([GetTempDir(False), APP_NAME]);
  s := IncludeTrailingPathDelimiter(s);

  Result := s;
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function NullDate: TDate;
begin
  Result := StrToDate(NULL_DATE_STR);
end;

function NullTime: TTime;
begin
  Result := StrToTime(NULL_TIME_STR);
end;

function NullDateTime: TDateTime;
begin
  Result := StrToDateTime(NULL_DATE_STR + ' ' + NULL_TIME_STR);
end;

procedure LoadDatabaseParams(aConnector: TSQLConnector);
begin
  databaseConnection.Clear;

  databaseConnection.LoadParams;

  aConnector.Params.Clear;
  aConnector.ConnectorType := 'SQLite3';
  aConnector.DatabaseName := databaseConnection.Database;
end;

end.

