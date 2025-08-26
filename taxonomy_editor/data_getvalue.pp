unit data_getvalue;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, DateUtils, data_types;

  function GetKey(aTable, aKeyField, aNameField, aNameValue: String): Integer;
  function GetName(aTable, aNameField, aKeyField: String; aKeyValue: Integer): String;
  function GetPrimaryKey(aDataSet: TDataSet): String;

  function GetLastInsertedKey(aTableType: TTableType): Integer;

  procedure GetTimeStamp(aField: TField; aTimeStampField: TDateTime);

implementation

uses
  udm_taxa;

function GetKey(aTable, aKeyField, aNameField, aNameValue: String): Integer;
var
  Qry: TSQLQuery;
begin
  if aNameValue = '' then
    Result := 0
  else
  begin
    Qry := TSQLQuery.Create(dmTaxa.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := dmTaxa.sqlCon;
      Clear;
      Add('SELECT %keyf FROM %tabname WHERE %uniquef = :uniquev');
      MacroByName('TABNAME').Value := aTable;
      MacroByName('KEYF').Value := aKeyField;
      MacroByName('UNIQUEF').Value := aNameField;
      ParamByName('UNIQUEV').AsString := aNameValue;
      // GravaLogSQL(SQL);
      Open;
      if not(IsEmpty) then
        Result := FieldByName(aKeyField).AsInteger
      else
        Result := 0;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function GetName(aTable, aNameField, aKeyField: String; aKeyValue: Integer): String;
var
  Qry: TSQLQuery;
begin
  if aKeyValue > 0 then
  begin
    Qry := TSQLQuery.Create(dmTaxa.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := dmTaxa.sqlCon;
      Clear;
      Add('SELECT %uniquef FROM %tabname WHERE %keyf = :keyv');
      MacroByName('UNIQUEF').Value := aNameField;
      MacroByName('TABNAME').Value := aTable;
      MacroByName('KEYF').Value := aKeyField;
      ParamByName('KEYV').AsInteger := aKeyValue;
      // GravaLogSQL(SQL);
      Open;
      if not(IsEmpty) then
      begin
        Result := FieldByName(aNameField).AsString;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function GetPrimaryKey(aDataSet: TDataSet): String;
var
  i: Integer;
begin
  Result := EmptyStr;

  for i := 0 to aDataSet.FieldCount - 1 do
  begin
    if pfInKey in aDataSet.Fields[i].ProviderFlags then
      Result := aDataSet.Fields[i].FieldName;
    Break;
  end;
end;

function GetLastInsertedKey(aTableType: TTableType): Integer;
var
  Qry: TSQLQuery;
begin
  Result := 0;

  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := dmTaxa.sqlCon;
    Transaction := dmTaxa.sqlTrans;
    Clear;
    { SQLite }
    Add('SELECT DISTINCT last_insert_rowid() FROM %tabname');
    MacroByName('TABNAME').Value := TABLE_NAMES[aTableType];
    Open;
    Result := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure GetTimeStamp(aField: TField; aTimeStampField: TDateTime);
var
  vTimeStamp: TDateTime;
begin
  if not (aField.IsNull) then
    if TryISOStrToDateTime(aField.AsString, vTimeStamp) then
      aTimeStampField := vTimeStamp
    else
      aTimeStampField := aField.AsDateTime;
end;

end.

