unit io_ioc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, DB, SQLDB, SdfData, StrUtils;

  procedure ImportIocData(aFilename: String);

implementation

uses
  utils_global, utils_dialogs, utils_taxonomy, data_getvalue, udm_taxa, udlg_loading;

procedure ImportIocData(aFilename: String);
var
  CSV: TSdfDataSet;
  Qry: TSQLQuery;
  Range: TStrings;
  nRank: Integer;
begin
  if not FileExists(aFilename) then
  begin
    MsgDlg(rsTitleImportFile, Format(rsErrorFileNotFound, [aFilename]), mtError);
    Exit;
  end;

  Parar := False;
  dlgLoading.Show;
  dlgLoading.UpdateProgress('Importing IOC checklist...', 0);

  CSV := TSdfDataSet.Create(nil);
  Qry := TSQLQuery.Create(nil);
  Range := TStringList.Create;
  try
    { Define CSV format settings }
    with CSV do
    begin
      CSV.AllowMultiLine := True;
      Delimiter := ';';
      FirstLineAsSchema := True;
      CodePage := 'Windows-1252';    // ToDo: try to detect the file codepage
      //Schema.AddDelimitedText(BandingSchema, ';', True);
      FileName := aFilename;
      Open;
      Last;
    end;

    dlgLoading.Max := CSV.RecordCount;

    if CSV.RecordCount > 0 then
    begin
      Qry.SQLConnection := dmTaxa.sqlCon;
      Qry.SQL.Add('SELECT * FROM zoo_taxa WHERE full_name = :aname');
      if not dmTaxa.sqlTrans.Active then
        dmTaxa.sqlTrans.StartTransaction;
      try
        CSV.First;
        repeat
          Range.Clear;
          Qry.Close;
          Qry.ParamByName('ANAME').AsString := CSV.FieldByName('Scientific Name').AsString;
          Qry.Open;

          if Qry.RecordCount > 0 then
          begin
            Qry.Edit;

            if CSV.Fields[0].AsString <> EmptyStr then
              Qry.FieldByName('ioc_sort_num').AsFloat := CSV.Fields[0].AsFloat;

            Qry.FieldByName('english_name').AsString := CSV.FieldByName('English name').AsString;
            Qry.FieldByName('authorship').AsString := CSV.FieldByName('Authority').AsString;

            if CSV.FieldByName('Breeding Range').AsString <> EmptyStr then
              Range.Add('Breeding: ' + CSV.FieldByName('Breeding Range').AsString);
            if CSV.FieldByName('Nonbreeding Range').AsString <> EmptyStr then
              Range.Add('Nonbreeding: ' + CSV.FieldByName('Nonbreeding Range').AsString);
            Qry.FieldByName('ioc_distribution').AsString := Range.Text;

            Qry.Post;
          end
          else
          begin
            Qry.Append;

            if CSV.FieldByName('Rank').AsString = 'Genus' then
              nRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'g.')
            else
            if CSV.FieldByName('Rank').AsString = 'Species' then
              nRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'sp.')
            else
            if CSV.FieldByName('Rank').AsString = 'ssp' then
              nRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ssp.');

            Qry.FieldByName('full_name').AsString := CSV.FieldByName('Scientific Name').AsString;
            Qry.FieldByName('formatted_name').AsString := FormattedBirdName(CSV.FieldByName('Scientific Name').AsString, nRank);
            Qry.FieldByName('rank_id').AsInteger := nRank;

            if CSV.FieldByName('Rank').AsString = 'Species' then
              Qry.FieldByName('parent_taxon_id').AsInteger :=
                GetKey('zoo_taxa', 'taxon_id', 'full_name',
                  ExtractWord(0, CSV.FieldByName('Scientific Name').AsString, [' ']))
            else
            if CSV.FieldByName('Rank').AsString = 'ssp' then
              Qry.FieldByName('parent_taxon_id').AsInteger :=
                GetKey('zoo_taxa', 'taxon_id', 'full_name',
                  ExtractWord(0, CSV.FieldByName('Scientific Name').AsString, [' ']) + ' ' +
                  ExtractWord(1, CSV.FieldByName('Scientific Name').AsString, [' ']));

            Qry.FieldByName('ioc_taxonomy').AsBoolean := True;
            Qry.FieldByName('ioc_parent_taxon_id').AsInteger := Qry.FieldByName('parent_taxon_id').AsInteger;
            Qry.FieldByName('extinct').AsBoolean := Pos('†', CSV.Fields[3].AsString) > 0;

            if CSV.Fields[0].AsString <> EmptyStr then
              Qry.FieldByName('ioc_sort_num').AsFloat := CSV.Fields[0].AsFloat;

            Qry.FieldByName('english_name').AsString := CSV.FieldByName('English name').AsString;
            Qry.FieldByName('authorship').AsString := CSV.FieldByName('Authority').AsString;

            if CSV.FieldByName('Breeding Range').AsString <> EmptyStr then
              Range.Add('Breeding: ' + CSV.FieldByName('Breeding Range').AsString);
            if CSV.FieldByName('Nonbreeding Range').AsString <> EmptyStr then
              Range.Add('Nonbreeding: ' + CSV.FieldByName('Nonbreeding Range').AsString);
            Qry.FieldByName('ioc_distribution').AsString := Range.Text;

            Qry.Post;
          end;

          Qry.ApplyUpdates;
          dlgLoading.Progress := CSV.RecNo;
          Application.ProcessMessages;
          CSV.Next;
        until CSV.Eof or Parar;

        if Parar then
        begin
          dmTaxa.sqlTrans.RollbackRetaining;
          MsgDlg(rsTitleImportFile, rsImportCanceledByUser, mtWarning);
        end
        else
        begin
          dmTaxa.sqlTrans.CommitRetaining;
          MsgDlg(rsTitleImportFile, rsSuccessfulImport, mtInformation);
        end;
      except
        dmTaxa.sqlTrans.RollbackRetaining;
        raise;
      end;

    end
    else
      MsgDlg(rsTitleImportFile, rsErrorFileIsEmpty, mtError);
  finally
    dlgLoading.Hide;
    dlgLoading.Max := 100;
    CSV.Close;
    FreeAndNil(CSV);
    FreeAndNil(Qry);
    FreeAndNil(Range);
  end;
end;

end.

