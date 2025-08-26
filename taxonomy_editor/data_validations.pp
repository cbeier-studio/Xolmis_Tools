unit data_validations;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, DB, SQLDB, data_types;

  function RecordExists(aTable: TTableType; aField, aValue: String): Boolean;
  function IsRecordActive(aTable: TTableType; aFieldName, aValue: String): Boolean;
  function RecordDuplicated(aTable: TTableType; aKeyField, aNameField, aNameValue: String; aKeyValue: Integer;
    aMessageList: TStrings = nil): Boolean;

implementation

uses
  utils_dialogs, udm_taxa;

function RecordExists(aTable: TTableType; aField, aValue: String): Boolean;
var
  i: Integer;
  Qry: TSQLQuery;
begin
  Result := False;
  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := dmTaxa.sqlCon;
    Clear;
    Add('SELECT count(%afield) FROM %tabname WHERE %afield = :keyv');
    MacroByName('AFIELD').Value := aField;
    MacroByName('TABNAME').Value := TableNames[aTable];
    ParamByName('KEYV').AsString := aValue;
    // GravaLogSQL(SQL);
    Open;
    i := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;

  Result := i > 0;
end;

function RecordDuplicated(aTable: TTableType; aKeyField, aNameField, aNameValue: String; aKeyValue: Integer;
  aMessageList: TStrings): Boolean;
var
  M: String;
  Qry: TSQLQuery;
begin
  Result := False;
  if (Trim(aNameValue) = '') then
    Exit;

  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := dmTaxa.sqlCon;
    Clear;
    Add('SELECT %keyf FROM %tabname');
    Add('WHERE (%uniquef = :uniquev) AND (%keyf != :keyv)');
    MacroByName('KEYF').Value := aKeyField;
    MacroByName('TABNAME').Value := TableNames[aTable];
    MacroByName('UNIQUEF').Value := aNameField;
    ParamByName('UNIQUEV').AsString := aNameValue;
    ParamByName('KEYV').AsInteger := aKeyValue;
    //GravaLogSQL(SQL);
    Open;
    Result := RecordCount > 0;
    Close;
  finally
    FreeAndNil(Qry);
  end;

  if Result then
  begin
    if IsRecordActive(aTable, aNameField, aNameValue) then
      M := Format('There is an active record with the same values.', [aNameField, aNameValue])
    else
      M := Format('There is an inactive record with the same values.', [aNameField, aNameValue]);
    if (Assigned(aMessageList)) then
    begin
      //LogError(M);
      aMessageList.Add(M);
    end
    else
      MsgDlg('', M, mtInformation);
  end;
end;

function IsRecordActive(aTable: TTableType; aFieldName, aValue: String): Boolean;
var
  a: Boolean;
  Qry: TSQLQuery;
begin
  Result := True;
  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    MacroCheck := True;
    DataBase := dmTaxa.sqlCon;
    Clear;
    Add('SELECT active_status FROM %tabname WHERE (%keyf = :keyv)');
    MacroByName('TABNAME').Value := TableNames[aTable];
    MacroByName('KEYF').Value := aFieldName;
    ParamByName('KEYV').AsInteger := StrToInt(aValue);
    // GravaLogSQL(SQL);
    Open;
    a := FieldByName('reg_ativo').AsBoolean;
    Close;
  finally
    FreeAndNil(Qry);
  end;
  Result := a;
end;

end.

