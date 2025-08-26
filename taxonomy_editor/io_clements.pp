unit io_clements;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, DB, SQLDB, SdfData, StrUtils;

  procedure ImportClementsData(aFilename: String);

implementation

uses
  utils_global, utils_dialogs, utils_taxonomy, data_getvalue, udm_taxa, udlg_loading;

procedure ImportClementsData(aFilename: String);
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
  dlgLoading.UpdateProgress('Importing Clements/eBird checklist...', 0);

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
          Qry.ParamByName('ANAME').AsString := CSV.FieldByName('scientific name').AsString;
          Qry.Open;

          if Qry.RecordCount > 0 then
          begin
            Qry.Edit;

            if CSV.Fields[0].AsString <> EmptyStr then
              Qry.FieldByName('sort_num').AsFloat := CSV.Fields[0].AsFloat;
            if CSV.FindField('species_code') <> nil then
              Qry.FieldByName('ebird_code').AsString := CSV.FieldByName('species_code').AsString;

            Qry.FieldByName('english_name').AsString := CSV.FieldByName('English name').AsString;
            Qry.FieldByName('authorship').AsString := CSV.FieldByName('authority').AsString;

            if CSV.FieldByName('range').AsString <> EmptyStr then
              Qry.FieldByName('distribution').AsString := CSV.FieldByName('range').AsString;
            Qry.FieldByName('extinct').AsBoolean := CSV.FieldByName('extinct').AsString = '1';
            if CSV.FieldByName('extinct year').AsString <> EmptyStr then
              Qry.FieldByName('extinction_year').AsString := CSV.FieldByName('extinct year').AsString;

            Qry.Post;
          end
          else
          begin
            Qry.Append;

            if CSV.FieldByName('category').AsString = 'species' then
              nRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'sp.')
            else
            if CSV.FieldByName('category').AsString = 'subspecies' then
              nRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ssp.')
            else
            if CSV.FieldByName('category').AsString = 'group (monotypic)' then
              nRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'grp. (mono)')
            else
            if CSV.FieldByName('category').AsString = 'group (polytypic)' then
              nRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'grp. (poli)')
            else
            if CSV.FieldByName('category').AsString = 'domestic' then
              nRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'domest.')
            else
            if CSV.FieldByName('category').AsString = 'form' then
              nRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'form')
            else
            if CSV.FieldByName('category').AsString = 'spuh' then
              nRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'spuh')
            else
            if CSV.FieldByName('category').AsString = 'slash' then
              nRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'slash')
            else
            if CSV.FieldByName('category').AsString = 'hybrid' then
              nRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'hybrid')
            else
            if CSV.FieldByName('category').AsString = 'intergrade' then
              nRank := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'intergrade');

            Qry.FieldByName('full_name').AsString := CSV.FieldByName('scientific name').AsString;
            Qry.FieldByName('formatted_name').AsString := FormattedBirdName(CSV.FieldByName('scientific name').AsString, nRank);
            Qry.FieldByName('rank_id').AsInteger := nRank;

            if (CSV.FieldByName('category').AsString = 'species') or
              (CSV.FieldByName('category').AsString = 'spuh') then
              Qry.FieldByName('parent_taxon_id').AsInteger :=
                GetKey('zoo_taxa', 'taxon_id', 'full_name',
                  ExtractWord(0, CSV.FieldByName('scientific name').AsString, [' ']))
            else
            if (CSV.FieldByName('category').AsString = 'subspecies') or
              (CSV.FieldByName('category').AsString = 'group (monotypic)') or
              (CSV.FieldByName('category').AsString = 'group (polytypic)') then
              Qry.FieldByName('parent_taxon_id').AsInteger :=
                GetKey('zoo_taxa', 'taxon_id', 'full_name',
                  ExtractWord(0, CSV.FieldByName('scientific name').AsString, [' ']) + ' ' +
                  ExtractWord(1, CSV.FieldByName('scientific name').AsString, [' ']));

            Qry.FieldByName('clements_taxonomy').AsBoolean := True;
            Qry.FieldByName('extinct').AsBoolean := CSV.FieldByName('extinct').AsString = '1';
            if CSV.FieldByName('extinct year').AsString <> EmptyStr then
              Qry.FieldByName('extinction_year').AsString := CSV.FieldByName('extinct year').AsString;

            if CSV.Fields[0].AsString <> EmptyStr then
              Qry.FieldByName('sort_num').AsFloat := CSV.Fields[0].AsFloat;
            if CSV.FindField('species_code') <> nil then
              Qry.FieldByName('ebird_code').AsString := CSV.FieldByName('species_code').AsString;

            Qry.FieldByName('english_name').AsString := CSV.FieldByName('English name').AsString;
            Qry.FieldByName('authorship').AsString := CSV.FieldByName('authority').AsString;

            if CSV.FieldByName('range').AsString <> EmptyStr then
              Qry.FieldByName('distribution').AsString := CSV.FieldByName('range').AsString;

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

