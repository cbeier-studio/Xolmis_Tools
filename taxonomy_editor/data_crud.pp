unit data_crud;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Dialogs, DB, SQLDB, data_types;

  procedure DeleteRecord(aTable: TTableType; aDataSet: TDataSet);
  procedure RestoreRecord(aTable: TTableType; aDataSet: TDataSet);

  function CanEdit(aDataset: TDataset): Boolean;

implementation

uses
  utils_global, utils_dialogs, data_core, data_getvalue, udm_taxa;

procedure DeleteRecord(aTable: TTableType; aDataSet: TDataSet);
var
  Qry: TSQLQuery;
  aKeyField: String;
  aKeyValue: Integer;
begin
  // Confirmation dialog
  with dmTaxa.TaskDlg do
  begin
    Title := rsDeleteRecordTitle;
    Text := rsDeleteRecordPrompt;
    Caption := rsTitleConfirmation;
    CommonButtons := [tcbYes, tcbNo];
    MainIcon := tdiNone;
    DefaultButton := tcbNo;
    FooterIcon := tdiInformation;
    FooterText := rsDeleteRecordFooter;
    if Execute then
      if ModalResult = mrNo then
        Exit;
  end;

  case aTable of
    tbNone: ;
    tbTaxonRanks: aKeyField := 'rank_id';
    tbZooTaxa: aKeyField := 'taxon_id';
    tbPackages: aKeyField := 'package_id';
    tbTaxonChanges: aKeyField := 'change_id';
    tbCountries: aKeyField := 'country_id';
    tbLanguages: aKeyField := 'language_id';
    tbVernacularNames: aKeyField := 'vernacular_id';
    tbSynonyms: aKeyField := 'synonym_id';
    tbTaxonCountries: aKeyField := 'taxon_country_id';
  else
    aKeyField := GetPrimaryKey(aDataSet);
  end;
  aKeyValue := aDataSet.FieldByName(aKeyField).AsInteger;

  //{$IFDEF DEBUG}
  //LogDebug(Format('Record %d from %s set inactive', [aKeyValue, TABLE_NAMES[aTable]]));
  //{$ENDIF}
  try
    //dmTaxa.sqlTrans.StartTransaction;
    Qry := TSQLQuery.Create(nil);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := dmTaxa.sqlCon;
      Clear;
      Add('UPDATE %tablename');
      Add('SET active_status = 0,');
      Add('update_date = datetime(''now'',''localtime'')');
      Add('WHERE %keyfield = :id');
      MacroByName('tablename').Value := TABLE_NAMES[aTable];
      MacroByName('keyfield').Value := aKeyField;
      ParamByName('id').AsInteger := aKeyValue;
      //{$IFDEF DEBUG}
      //LogSQL(SQL);
      //{$ENDIF}
      ExecSQL;
    finally
      FreeAndNil(Qry);
    end;
    dmTaxa.sqlTrans.CommitRetaining;
    aDataSet.Refresh;
  except
    dmTaxa.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

procedure RestoreRecord(aTable: TTableType; aDataSet: TDataSet);
var
  Qry: TSQLQuery;
  aKeyField: String;
  aKeyValue: Integer;
begin
  if not MsgDlg(rsRestoreRecordTitle, rsRestoreRecordPrompt, mtConfirmation) then
    Exit;

  aKeyField := GetPrimaryKey(aDataSet);
  aKeyValue := aDataSet.FieldByName(aKeyField).AsInteger;

  //{$IFDEF DEBUG}
  //LogDebug(Format('Record %d from %s set active', [aKeyValue, TABLE_NAMES[aTable]]));
  //{$ENDIF}
  try
    //dmTaxa.sqlTrans.StartTransaction;
    Qry := TSQLQuery.Create(nil);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := dmTaxa.sqlCon;
      Clear;
      Add('UPDATE %tablename');
      Add('SET active_status = 1,');
      Add('update_date = datetime(''now'',''localtime'')');
      Add('WHERE %keyfield = :id');
      MacroByName('tablename').Value := TABLE_NAMES[aTable];
      MacroByName('keyfield').Value := aKeyField;
      ParamByName('id').AsInteger := aKeyValue;
      //{$IFDEF DEBUG}
      //LogSQL(SQL);
      //{$ENDIF}
      ExecSQL;
    finally
      FreeAndNil(Qry);
    end;
    dmTaxa.sqlTrans.CommitRetaining;
    aDataSet.Refresh;
  except
    dmTaxa.sqlTrans.RollbackRetaining;
    raise;
  end;
end;

function CanEdit(aDataset: TDataset): Boolean;
begin
  Result := False;

  with aDataSet do
  begin
    if not Active then
    begin
      Open;
      Append;

      Result := True;
    end else
    begin
      if not(State in [dsInsert, dsEdit]) then
      begin
        if (RecordCount > 0) then
          Edit
        else
          Append;

        Result := True;
      end;
    end;
  end;
end;

end.

