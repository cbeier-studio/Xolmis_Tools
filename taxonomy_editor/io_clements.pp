unit io_clements;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, DB, SQLDB, SdfData, StrUtils;

  procedure ImportClementsData(aFilename: String);

implementation

uses
  utils_global, utils_dialogs, utils_taxonomy, data_getvalue, models_taxon, udm_taxa, udlg_loading;

procedure ImportClementsData(aFilename: String);
var
  CSV: TSdfDataSet;
  nRank: TZooRank;
  Repo: TTaxonRepository;
  FTaxon: TTaxon;
  VernRepo: TVernacularRepository;
  FVernacular: TVernacularName;
  Qry: TSQLQuery;
begin
  if not FileExists(aFilename) then
  begin
    MsgDlg(rsTitleImportFile, Format(rsErrorFileNotFound, [aFilename]), mtError);
    Exit;
  end;

  Parar := False;
  dlgLoading.Show;
  dlgLoading.UpdateProgress('Importing Clements/eBird checklist...', 0);

  Repo := TTaxonRepository.Create(dmTaxa.sqlCon);
  VernRepo := TVernacularRepository.Create(dmTaxa.sqlCon);
  FTaxon := TTaxon.Create();
  FVernacular := TVernacularName.Create();
  CSV := TSdfDataSet.Create(nil);
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
      if not dmTaxa.sqlTrans.Active then
        dmTaxa.sqlTrans.StartTransaction;
      try
        Qry := TSQLQuery.Create(nil);
        with Qry, SQL do
        try
          DataBase := dmTaxa.sqlCon;
          Add('UPDATE zoo_taxa SET accepted_status = 0');
          ExecSQL;
        finally
          FreeAndNil(Qry);
        end;

        CSV.First;
        repeat
          FTaxon.Clear;
          FVernacular.Clear;
          Repo.FindBy('full_name', CSV.FieldByName('scientific name').AsString, FTaxon);

          if not FTaxon.IsNew then
          begin
            case CSV.FieldByName('category').AsString of
              'species':            FTaxon.RankId := trSpecies;
              'subspecies':         FTaxon.RankId := trSubspecies;
              'group (monotypic)':  FTaxon.RankId := trMonotypicGroup;
              'group (polytypic)':  FTaxon.RankId := trPolitypicGroup;
              'domestic':           FTaxon.RankId := trDomestic;
              'form':               FTaxon.RankId := trForm;
              'spuh':               FTaxon.RankId := trSpuh;
              'slash':              FTaxon.RankId := trSlash;
              'hybrid':             FTaxon.RankId := trHybrid;
              'intergrade':         FTaxon.RankId := trIntergrade;
            end;
            FTaxon.Authorship := CSV.FieldByName('authority').AsString;
            if CSV.Fields[0].AsString <> EmptyStr then
              FTaxon.SortNum := CSV.Fields[0].AsFloat;
            if CSV.FindField('species_code') <> nil then
              FTaxon.EbirdCode := CSV.FieldByName('species_code').AsString;
            if CSV.FieldByName('range').AsString <> EmptyStr then
              FTaxon.Distribution := CSV.FieldByName('range').AsString;
            FTaxon.Extinct := CSV.FieldByName('extinct').AsString = '1';
            if CSV.FieldByName('extinct year').AsString <> EmptyStr then
              FTaxon.ExtinctionYear := CSV.FieldByName('extinct year').AsString;
            FTaxon.Accepted := True;

            Repo.Update(FTaxon);

            VernRepo.FindByTaxon(FTaxon.Id, CSV.FieldByName('English name').AsString, FVernacular);
            if not FVernacular.IsNew then
            begin
              FVernacular.Active := True;
              VernRepo.Update(FVernacular);
            end
            else
            begin
              FVernacular.TaxonId := FTaxon.Id;
              FVernacular.LanguageId := GetKey('languages', 'language_id', 'macrolanguage_code', 'en');
              FVernacular.VernacularName := CSV.FieldByName('English name').AsString;
              VernRepo.Insert(FVernacular);
            end;
          end
          else
          begin
            case CSV.FieldByName('category').AsString of
              'species':            FTaxon.RankId := trSpecies;
              'subspecies':         FTaxon.RankId := trSubspecies;
              'group (monotypic)':  FTaxon.RankId := trMonotypicGroup;
              'group (polytypic)':  FTaxon.RankId := trPolitypicGroup;
              'domestic':           FTaxon.RankId := trDomestic;
              'form':               FTaxon.RankId := trForm;
              'spuh':               FTaxon.RankId := trSpuh;
              'slash':              FTaxon.RankId := trSlash;
              'hybrid':             FTaxon.RankId := trHybrid;
              'intergrade':         FTaxon.RankId := trIntergrade;
            end;

            FTaxon.FullName := CSV.FieldByName('scientific name').AsString;
            FTaxon.FormattedName := FormattedBirdName(CSV.FieldByName('scientific name').AsString, GetRankKey(FTaxon.RankId));

            if (FTaxon.RankId = trSpecies) or (FTaxon.RankId = trSpuh) then
              FTaxon.ParentTaxonId := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                  ExtractWord(0, CSV.FieldByName('scientific name').AsString, [' ']))
            else
            if (FTaxon.RankId = trSubspecies) or (FTaxon.RankId = trMonotypicGroup) or
              (FTaxon.RankId = trPolitypicGroup) then
              FTaxon.ParentTaxonId := GetKey('zoo_taxa', 'taxon_id', 'full_name',
                  ExtractWord(0, CSV.FieldByName('scientific name').AsString, [' ']) + ' ' +
                  ExtractWord(1, CSV.FieldByName('scientific name').AsString, [' ']));

            FTaxon.Extinct := CSV.FieldByName('extinct').AsString = '1';
            if CSV.FieldByName('extinct year').AsString <> EmptyStr then
              FTaxon.ExtinctionYear := CSV.FieldByName('extinct year').AsString;

            if CSV.Fields[0].AsString <> EmptyStr then
              FTaxon.SortNum := CSV.Fields[0].AsFloat;
            if CSV.FindField('species_code') <> nil then
              FTaxon.EbirdCode := CSV.FieldByName('species_code').AsString;

            FTaxon.Authorship := CSV.FieldByName('authority').AsString;

            if CSV.FieldByName('range').AsString <> EmptyStr then
              FTaxon.Distribution := CSV.FieldByName('range').AsString;
            FTaxon.Accepted := True;

            Repo.Insert(FTaxon);

            FVernacular.TaxonId := FTaxon.Id;
            FVernacular.LanguageId := GetKey('languages', 'language_id', 'macrolanguage_code', 'en');
            FVernacular.VernacularName := CSV.FieldByName('English name').AsString;
            VernRepo.Insert(FVernacular);
          end;

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
    FreeAndNil(FVernacular);
    FreeAndNil(FTaxon);
    VernRepo.Free;
    Repo.Free;
  end;
end;

end.

