unit data_types;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, DB, SQLDB, Dialogs, jsonconf, fgl, RegExpr;

type

  { TDBParams }

  TDBParams = record
    VendorLib: String;
    Database: String;
    UserName: String;
    Password: String;
    LastBackup: TDateTime;
    procedure Clear;
    function IntegrityCheck(const ShowResults: Boolean = True): Boolean;
    procedure LoadParams;
    procedure Optimize;
    procedure SaveParams;
    function TestConnection: Boolean;
    procedure Vacuum;
  end;

  TTableType = (tbNone,
    tbTaxonRanks,
    tbZooTaxa,
    tbPackages,
    tbTaxonChanges,
    tbCountries,
    tbLanguages,
    tbVernacularNames,
    tbSynonyms,
    tbTaxonCountries);

  TFilterValue = (fvNone, fvReset, fvAll, fvMarked, fvUnmarked, fvDeleted, fvQueued);
  TCriteriaType = (crNone,
    crLike, crStartLike, crEqual, crDistinct,
    crBetween, crMoreThan, crLessThan,
    crNull, crNotNull);
  TSortDirection = (sdNone, sdAscending, sdDescending);
  TSortType = (stNone, stAlphanumeric, stNumeric, stDateTime, stBoolean, stTaxonomic);
  TSearchDataType = (sdtText, sdtInteger, sdtFloat, sdtDate, sdtTime, sdtDateTime, sdtBoolean, sdtList,
    sdtLookup);
  TSortedField = class
    FieldName: String;
    Direction: TSortDirection;
    Collation: String;
    Lookup: Boolean;
  end;
  TSortedFields = specialize TFPGObjectList<TSortedField>;
  TFilterType = (tcTexto, tcInteiro, tcDecimal, tcData, tcHora, tcDataHora, tcLista, tcBool, tcLookup);
  TSQLAndOr = (aoNone, aoAnd, aoOr);
  TRecordActiveStatus = (rsAll, rsActive, rsInactive, rsNone);

type

  TCustomSearchField = class
  private
    FDataType: TSearchDataType;
    FCriteria: TCriteriaType;
    FValue1: String;
    FValue2: String;
    FLookup: Boolean;
  public

  published
    property DataType: TSearchDataType read FDataType write FDataType default sdtText;
    property Criteria: TCriteriaType read FCriteria write FCriteria default crLike;
    property Value1: String read FValue1 write FValue1;
    property Value2: String read FValue2 write FValue2;
    property Lookup: Boolean read FLookup write FLookup default False;
  end;

  { TSearchField }

  TSearchField = class(TCustomSearchField)
  private
    FFieldName: String;
    FDisplayName: String;
  public
    constructor Create(aFieldName, aDisplayName: String; aDataType: TSearchDataType = sdtText;
      aCriteria: TCriteriaType = crLike; IsLookup: Boolean = False;
      aValue1: String = ''; aValue2: String = '');
  published
    property FieldName: String read FFieldName write FFieldName;
    property DisplayName: String read FDisplayName write FDisplayName;
  end;

  TSearchFields = specialize TFPGObjectList<TSearchField>;

type

  { TSearchGroup }

  TSearchGroup = class
  private
    FFields: TSearchFields;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Fields: TSearchFields read FFields write FFields;
  end;

  TSearchGroups = specialize TFPGObjectList<TSearchGroup>;

  { TCustomSearch }

  TCustomSearch = class
  private
    FTableType: TTableType;
    FTableAlias: String;
    FFields: TSearchGroups;
    FQuickFilters: TSearchGroups;
    FSortFields: TSortedFields;
    FDataSet: TSQLQuery;
    FRecordActive: TRecordActiveStatus;
    function GetCount: Integer;
    function GetSQLString: String;
  public
    constructor Create(aTable: TTableType);
    destructor Destroy; override;
    procedure Reset;
    function RunSearch: Integer;
  published
    property TableType: TTableType read FTableType write FTableType;
    property TableAlias: String read FTableAlias write FTableAlias;
    property Fields: TSearchGroups read FFields write FFields;
    property QuickFilters: TSearchGroups read FQuickFilters write FQuickFilters;
    property SortFields: TSortedFields read FSortFields write FSortFields;
    property Count: Integer read GetCount;
    property DataSet: TSQLQuery read FDataSet write FDataSet;
    property RecordActive: TRecordActiveStatus read FRecordActive write FRecordActive default rsActive;
  end;

const
  TABLE_ALIASES: array[TTableType] of String = ('',
    'tr',
    'z',
    'pk',
    'tc',
    'c',
    'l',
    'zv',
    'zs',
    'zc');
  TABLE_NAMES: array[TTableType] of String = ('',
    'taxon_ranks',
    'zoo_taxa',
    'packages',
    'taxon_changes',
    'countries',
    'languages',
    'vernacular_names',
    'zoo_taxa_synonyms',
    'zoo_taxa_countries');
  CRITERIA_OPERATORS: array[TCriteriaType] of String = ('',
    'LIKE', 'LIKE', '=', 'DISTINCT',
    'BETWEEN', '>=', '<=',
    'ISNULL', 'NOTNULL');
  SORT_DIRECTIONS: array [TSortDirection] of String = ('', 'ASC', 'DESC');
  SEARCH_DATA_TYPES: array[TFilterType] of String = ('Text', 'Integer', 'Float', 'Date', 'Time',
    'DateTime', 'Boolean', 'List', 'Lookup');
  SQL_AND_OR_STR: array [TSQLAndOr] of String = ('', 'AND', 'OR');

var
  databaseConnection: TDBParams;

implementation

uses
  utils_global, utils_dialogs, utils_conversions, data_select, udm_taxa, udlg_validate;

{ TDBParams }

procedure TDBParams.Clear;
begin
  VendorLib := 'sqlite3.dll';
  Database := EmptyStr;
  UserName := EmptyStr;
  Password := EmptyStr;
  LastBackup := StrToDateTime('30/12/1500 00:00:00');
end;

function TDBParams.IntegrityCheck(const ShowResults: Boolean): Boolean;
var
  uCon: TSQLConnector;
  uTrans: TSQLTransaction;
  Qry: TSQLQuery;
begin
  Result := False;

  uCon := TSQLConnector.Create(nil);
  uTrans := TSQLTransaction.Create(uCon);
  Qry := TSQLQuery.Create(nil);
  try
    LoadDatabaseParams(uCon);

    try
      uTrans.Action := caCommitRetaining;
      uCon.Transaction := uTrans;
      uCon.Open;
      Qry.SQLConnection := uCon;
      Qry.SQLTransaction := uTrans;

      Qry.SQL.Clear;
      Qry.SQL.Add('PRAGMA integrity_check;');
      Qry.Open;
      Qry.First;
      if Qry.Fields[0].AsString = 'ok' then
      begin
        Result := True;
        if ShowResults then
          MsgDlg(rsTitleInformation, 'Database integrity is OK.', mtInformation);
      end
      else
      begin
        Result := False;
        if ShowResults then
        begin
          dlgValidate := TdlgValidate.Create(nil);
          try
            dlgValidate.Header := 'Database integrity check returned errors.';
            while not Qry.EOF do
            begin
              dlgValidate.MessageList.Add(Qry.Fields[0].AsString);
              Qry.Next;
            end;
            dlgValidate.ShowModal;
          finally
            FreeAndNil(dlgValidate);
          end;
        end;
      end;
      Qry.Close;
      uCon.Close;
    except
      on E: Exception do
        MsgDlg(rsTitleError, Format('An error occurred while checking database integrity: %s.', [E.Message]), mtError);
    end;
  finally
    FreeAndNil(Qry);
    FreeAndNil(uTrans);
    FreeAndNil(uCon);
  end;
end;

procedure TDBParams.LoadParams;
var
  FConfig: TJSONConfig;
  FFileName: String;
begin
  FFileName := ConcatPaths([AppDataDir, DEFAULT_SETTINGS_FILE]);
  FConfig := TJSONConfig.Create(nil);
  try
    FConfig.Formatted:= True;
    FConfig.Filename:= FFileName;

    Database := FConfig.GetValue('/DATABASE/DatabaseFile', '');
    UserName := FConfig.GetValue('/DATABASE/UserName', '');
    Password := FConfig.GetValue('/DATABASE/Password', '');
    LastBackup := FConfig.GetValue('/DATABASE/LastBackup', StrToDateTime('30/12/1500 00:00:00'));
    VendorLib := 'sqlite3.dll';

  finally
    FConfig.Free;
  end;
end;

procedure TDBParams.Optimize;
var
  uCon: TSQLConnector;
  uTrans: TSQLTransaction;
begin
  uCon := TSQLConnector.Create(nil);
  uTrans := TSQLTransaction.Create(uCon);
  try
    LoadDatabaseParams(uCon);

    try
      uTrans.Action := caCommitRetaining;
      uCon.Transaction := uTrans;
      uCon.Open;
      uCon.ExecuteDirect('PRAGMA optimize;');
      uCon.Close;

      MsgDlg(rsTitleInformation, 'Database was successfully optimized.', mtInformation);
    except
      on E: Exception do
        MsgDlg(rsTitleError, Format('An error occurred while optimizing the database: %s', [E.Message]), mtError);
    end;
  finally
    FreeAndNil(uTrans);
    FreeAndNil(uCon);
  end;
end;

procedure TDBParams.SaveParams;
var
  FConfig: TJSONConfig;
  FFileName: String;
begin
  FFileName := ConcatPaths([AppDataDir, DEFAULT_SETTINGS_FILE]);
  FConfig := TJSONConfig.Create(nil);
  try
    FConfig.Formatted:= True;
    FConfig.Filename:= FFileName;

    FConfig.SetValue('/DATABASE/DatabaseFile', Database);
    FConfig.SetValue('/DATABASE/UserName', UserName);
    FConfig.SetValue('/DATABASE/Password', Password);
    FConfig.SetValue('/DATABASE/LastBackup', LastBackup);

    FConfig.Flush;
  finally
    FConfig.Free;
  end;
end;

function TDBParams.TestConnection: Boolean;
var
  uCon: TSQLConnector;
  uTrans: TSQLTransaction;
  aMsg: String;
begin
  Result := False;

  uCon := TSQLConnector.Create(nil);
  try
    LoadDatabaseParams(uCon);

    try
      uTrans := TSQLTransaction.Create(uCon);
      uTrans.Action := caRollbackRetaining;
      uCon.Transaction := uTrans;
      uCon.Open;
      Result := uCon.Connected;
    except
      Result := False;
    end;
  finally
    FreeAndNil(uTrans);
    FreeAndNil(uCon);
  end;
end;

procedure TDBParams.Vacuum;
var
  uCon: TSQLConnector;
  uTrans: TSQLTransaction;
begin
  uCon := TSQLConnector.Create(nil);
  uTrans := TSQLTransaction.Create(uCon);
  try
    LoadDatabaseParams(uCon);

    try
      uTrans.Action := caCommitRetaining;
      uCon.Transaction := uTrans;
      uCon.Open;
      uCon.ExecuteDirect('END TRANSACTION;');
      uCon.ExecuteDirect('VACUUM;');
      uCon.ExecuteDirect('BEGIN TRANSACTION;');
      uCon.Close;

      MsgDlg(rsTitleInformation, 'Database was vaccumed.', mtInformation);
    except
      on E: Exception do
        MsgDlg(rsTitleError, Format('An error occurred while vaccuming the database: %s', [E.Message]), mtError);
    end;
  finally
    FreeAndNil(uTrans);
    FreeAndNil(uCon);
  end;
end;

{ TSearchField }

constructor TSearchField.Create(aFieldName, aDisplayName: String; aDataType: TSearchDataType;
  aCriteria: TCriteriaType; IsLookup: Boolean; aValue1: String; aValue2: String);
begin
  FFieldName := aFieldName;
  FDisplayName := aDisplayName;
  FDataType := aDataType;
  FCriteria := aCriteria;
  FLookup := IsLookup;
  FValue1 := aValue1;
  FValue2 := aValue2;
end;

{ TSearchGroup }

constructor TSearchGroup.Create;
begin
  FFields := TSearchFields.Create(True);
end;

destructor TSearchGroup.Destroy;
begin
  FFields.Clear;
  FFields.Free;
  inherited Destroy;
end;

{ TCustomSearch }

constructor TCustomSearch.Create(aTable: TTableType);
begin
  FTableType := aTable;
  FTableAlias := TABLE_ALIASES[aTable];
  FFields := TSearchGroups.Create(True);
  FQuickFilters := TSearchGroups.Create(True);
  FSortFields := TSortedFields.Create(True);
  FRecordActive := rsActive;
end;

destructor TCustomSearch.Destroy;
begin
  FFields.Clear;
  FQuickFilters.Clear;
  FSortFields.Clear;
  FFields.Free;
  FQuickFilters.Free;
  FSortFields.Free;
  inherited Destroy;
end;

function TCustomSearch.GetCount: Integer;
begin
  Result := 0;
  if not Assigned(FFields) then
    Exit;

  Result := FFields.Count + FQuickFilters.Count;
end;

function TCustomSearch.GetSQLString: String;
const
  MaskNull: String = '(%s %s) ';
  MaskV1: String = '(%s %s %s) ';
  MaskV2: String = '(%s %s %s AND %s) ';
  MaskDateV1: String = '(date(%s) %s date(%s)) ';
  MaskDateV2: String = '(date(%s) %s date(%s) AND date(%s)) ';
  MaskTimeV1: String = '(time(%s) %s time(%s)) ';
  MaskTimeV2: String = '(time(%s) %s time(%s) AND time(%s)) ';
  MaskDateTimeV1: String = '(datetime(%s) %s datetime(%s)) ';
  MaskDateTimeV2: String = '(datetime(%s) %s datetime(%s) AND datetime(%s)) ';
var
  i, f: Integer;
  V1, V2, S, AndOrWhere, Msk, aCriteria, aSort: String;

  // Select mask using criteria and data type
  function GetValueMask(aCriteriaType: TCriteriaType; aDataType: TSearchDataType): String;
  begin
    Result := MaskV1;

    if aCriteriaType in [crNull, crNotNull] then
      Result := MaskNull;

    case aDataType of
      sdtText, sdtList, sdtLookup:
        Result := MaskV1;
      sdtBoolean:
        Result := MaskV1;
      sdtInteger, sdtFloat:
        begin
          if aCriteriaType = crBetween then
            Result := MaskV2
          else
            Result := MaskV1;
        end;
      sdtDate:
        begin
          if aCriteriaType = crBetween then
            Result := MaskDateV2
          else
            Result := MaskDateV1;
        end;
      sdtTime:
        begin
          if aCriteriaType = crBetween then
            Result := MaskTimeV2
          else
            Result := MaskTimeV1;
        end;
      sdtDateTime:
        begin
          if aCriteriaType = crBetween then
            Result := MaskDateTimeV2
          else
            Result := MaskDateTimeV1;
        end;
    end;
  end;

  procedure PrepareValues(aCriteriaType: TCriteriaType; aDataType: TSearchDataType;
      var aValue1, aValue2: String);
  begin
    aValue1 := Trim(aValue1);
    aValue2 := Trim(aValue2);

    case aDataType of
      sdtText, sdtList, sdtLookup:
        begin
          if ExecRegExpr('^.+\+.+$', aValue1) then
            aValue1 := WildcardSyllables(aValue1) + '%'
          else
            aValue1 := WildcardWords(aValue1) + '%';

          if aCriteriaType = crLike then
            aValue1 := '%' + aValue1;

          aValue1 := QuotedStr(aValue1);
        end;
      sdtInteger, sdtFloat:
        begin
          aValue1 := StringReplace(aValue1, ',', '.', [rfReplaceAll]);
          aValue2 := StringReplace(aValue2, ',', '.', [rfReplaceAll]);
        end;
      sdtBoolean: ;
      sdtDate: ;
      sdtTime: ;
      sdtDateTime: ;
    end;
  end;

begin
  Result := EmptyStr;
  Msk := MaskV1;
  V1 := EmptyStr;
  V2 := EmptyStr;

  FDataSet.SQL.Clear;

  SetSelectSQL(FDataSet.SQL, FTableType, FTableAlias);
  AndOrWhere := 'WHERE ';

  // Add fields in WHERE clause
  if FQuickFilters.Count > 0 then
  begin
    // Iterate groups
    for i := 0 to (FQuickFilters.Count - 1) do
    begin
      if FQuickFilters[i].Fields.Count > 0 then
      begin
        FDataSet.SQL.Add(AndOrWhere + '(');
        // Iterate group fields
        for f := 0 to (FQuickFilters[i].Fields.Count - 1) do
        begin
          S := EmptyStr;
          with FQuickFilters[i].Fields[f] do
          begin
            PrepareValues(FCriteria, FDataType, FValue1, FValue2);
            Msk := GetValueMask(FCriteria, FDataType);

            // Fieldname, criteria, and values
            case Criteria of
              crLike, crStartLike, crEqual, crMoreThan, crLessThan:
              begin
                if (FTableAlias = EmptyStr) or (FQuickFilters[i].Fields[f].Lookup) then
                  S := S + Format(Msk, [FieldName, CRITERIA_OPERATORS[Criteria], Value1])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FieldName, CRITERIA_OPERATORS[Criteria], Value1]);
              end;
              crBetween:
              begin
                if (FTableAlias = EmptyStr) or (FQuickFilters[i].Fields[f].Lookup) then
                  S := S + Format(Msk, [FieldName, CRITERIA_OPERATORS[Criteria], Value1, Value2])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FieldName, CRITERIA_OPERATORS[Criteria], Value1, Value2]);
              end;
              crNull, crNotNull:
              begin
                if (FTableAlias = EmptyStr) or (FQuickFilters[i].Fields[f].Lookup) then
                  S := S + Format(Msk, [FieldName, CRITERIA_OPERATORS[Criteria]])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FieldName, CRITERIA_OPERATORS[Criteria]]);
              end;
            end;

            // Close parenthesis, and AND/OR
            if f < (FQuickFilters[i].Fields.Count - 1) then
            //  S := Trim(S) + ')'
            //else
              S := S + 'OR ';
          end;

          //if i = 0 then
          //  FDataSet.SQL.Add(AndOrWhere + S)
          //else
            FDataSet.SQL.Add(S);
        end;
        FDataSet.SQL.Add(')');
        AndOrWhere := 'AND ';
      end;
    end;
  end;

  if FFields.Count > 0 then
  begin
    // Iterate groups
    for i := 0 to (FFields.Count - 1) do
    begin
      if FFields[i].Fields.Count > 0 then
      begin
        FDataSet.SQL.Add(AndOrWhere + '(');
        // Iterate group fields
        for f := 0 to (FFields[i].Fields.Count - 1) do
        begin
          S := EmptyStr;
          with FFields[i].Fields[f] do
          begin
            PrepareValues(FCriteria, FDataType, FValue1, FValue2);
            Msk := GetValueMask(FCriteria, FDataType);

            // Fieldname, criteria, and values
            case Criteria of
              crLike, crStartLike, crEqual, crMoreThan, crLessThan:
              begin
                if (FTableAlias = EmptyStr) or (FFields[i].Fields[f].Lookup) then
                  S := S + Format(Msk, [FieldName, CRITERIA_OPERATORS[Criteria], Value1])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FieldName, CRITERIA_OPERATORS[Criteria], Value1]);
              end;
              crBetween:
              begin
                if (FTableAlias = EmptyStr) or (FFields[i].Fields[f].Lookup) then
                  S := S + Format(Msk, [FieldName, CRITERIA_OPERATORS[Criteria], Value1, Value2])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FieldName, CRITERIA_OPERATORS[Criteria], Value1, Value2]);
              end;
              crNull, crNotNull:
              begin
                if (FTableAlias = EmptyStr) or (FFields[i].Fields[f].Lookup) then
                  S := S + Format(Msk, [FieldName, CRITERIA_OPERATORS[Criteria]])
                else
                  S := S + Format(Msk, [FTableAlias+'.'+FieldName, CRITERIA_OPERATORS[Criteria]]);
              end;
            end;

            // Close parenthesis, and AND/OR
            if f < (FFields[i].Fields.Count - 1) then
            //  S := S + ') '
            //else
              S := S + 'OR ';
          end;

          //if i = 0 then
          //  FDataSet.SQL.Add(AndOrWhere + S)
          //else
            FDataSet.SQL.Add(S);
        end;
        FDataSet.SQL.Add(')');
        AndOrWhere := 'AND ';
      end;
    end;
  end;
  S := EmptyStr;

  case FRecordActive of
    rsAll: S := EmptyStr;
    rsActive: S := '1';
    rsInactive: S := '0';
    rsNone: S := EmptyStr;
  end;
  if S <> EmptyStr then
    if FTableAlias <> EmptyStr then
      FDataSet.SQL.Add(AndOrWhere + '(' + FTableAlias + '.active_status = ' + S + ')')
    else
      FDataSet.SQL.Add(AndOrWhere + '(active_status = ' + S + ')');

  // Add fields in ORDER BY clause
  if FSortFields.Count > 0 then
  begin
    aSort := EmptyStr;

    for i := 0 to (FSortFields.Count - 1) do
    begin
      // Field name
      if (FSortFields.Items[i].Lookup) or (FTableAlias = EmptyStr) then
        aSort := aSort + FSortFields.Items[i].FieldName
      else
        aSort := aSort + FTableAlias + '.' + FSortFields.Items[i].FieldName;

      // Collation
      if FSortFields.Items[i].Collation <> EmptyStr then
        aSort := aSort + ' COLLATE ' + FSortFields.Items[i].Collation;

      // Direction
      aSort := aSort + ' ' + SORT_DIRECTIONS[FSortFields.Items[i].Direction];

      if i < (FSortFields.Count - 1) then
        aSort := aSort + ', ';
    end;
    FDataSet.SQL.Add('ORDER BY ' + aSort);
  end;

  {$IFDEF DEBUG}
  LogSQL(FDataSet.SQL);
  {$ENDIF}
end;

procedure TCustomSearch.Reset;
begin
  FRecordActive := rsActive;
  FFields.Clear;
  FQuickFilters.Clear;
end;

function TCustomSearch.RunSearch: Integer;
begin
  if FDataSet.Active then
    FDataSet.Close;

  GetSQLString;

  FDataSet.Open;
  Result := FDataSet.RecordCount;
end;

end.

