unit io_json;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, jsonparser;

  function ExportSpeciesList(aFileName: String; CountryId, LanguageId: Integer; VernacularNames: Boolean = False): Boolean;
  function ExportCompleteTaxonomy(aFileName: String): Boolean;

implementation

uses
  utils_global, udm_taxa, udlg_loading;

procedure AddNullableInteger(const Obj: TJSONObject; const Name: String; const Field: TField);
begin
  if (Field = nil) or Field.IsNull then
    Obj.Add(Name, TJSONNull.Create)
  else
    Obj.Add(Name, Field.AsInteger);
end;

procedure AddNullableFloat(const Obj: TJSONObject; const Name: String; const Field: TField);
begin
  if (Field = nil) or Field.IsNull then
    Obj.Add(Name, TJSONNull.Create)
  else
    Obj.Add(Name, Field.AsFloat);
end;

procedure AddNullableBoolean(const Obj: TJSONObject; const Name: String; const Field: TField);
begin
  if (Field = nil) or Field.IsNull then
    Obj.Add(Name, TJSONNull.Create)
  else
    Obj.Add(Name, Field.AsBoolean);
end;

procedure AddNullableString(const Obj: TJSONObject; const Name: String; const Field: TField);
begin
  if (Field = nil) or Field.IsNull then
    Obj.Add(Name, TJSONNull.Create)
  else
    Obj.Add(Name, Field.AsString);
end;

procedure WriteJSONLLine(Stream: TStream; const Line: String);
var
  OutputLine: String;
begin
  OutputLine := Line + LineEnding;
  if OutputLine <> EmptyStr then
    Stream.WriteBuffer(Pointer(OutputLine)^, Length(OutputLine));
end;

function ExportSpeciesList(aFileName: String; CountryId, LanguageId: Integer; VernacularNames: Boolean
  ): Boolean;
var
  jArr: TJSONArray;
  jObj: TJSONObject;
  Qry: TSQLQuery;
  FS: TFileStream;
  S: String;
begin
  Result := False;
  jArr := TJSONArray.Create;
  Qry := TSQLQuery.Create(nil);
  with Qry do
  try
    DataBase := dmTaxa.sqlCon;

    SQL.Add('SELECT');
    SQL.Add('  s.full_name AS taxon_name,');
    SQL.Add('  v.vernacular_name,');
    SQL.Add('  z.sort_num');
    SQL.Add('FROM zoo_taxa_countries AS c');
    SQL.Add('LEFT JOIN zoo_taxa_synonyms AS s ON c.taxon_id = s.taxon_id AND s.valid_status = 1');
    SQL.Add('LEFT JOIN vernacular_names AS v ON c.taxon_id = v.taxon_id AND v.preferred = 1 AND v.language_id = :language_id');
    SQL.Add('LEFT JOIN zoo_taxa AS z ON c.taxon_id = z.taxon_id');
    SQL.Add('WHERE c.country_id = :country_id');
    SQL.Add('ORDER BY z.sort_num ASC');

    ParamByName('language_id').AsInteger := LanguageId;
    ParamByName('country_id').AsInteger := CountryId;

    Open;

    First;

    while not EOF do
    begin
      jObj := TJSONObject.Create;
      try
        jObj.Add('scientificName', FieldByName('taxon_name').AsString);
        if VernacularNames then
          jObj.Add('vernacularName', FieldByName('vernacular_name').AsString);

        jArr.Add(jObj);

        // O objeto foi adicionado ao array, então o array é responsável pela sua liberação.
        // Se você não o adicionasse, precisaria de um 'finally' para liberá-lo aqui.
      finally
        // Para garantir que o objeto seja liberado em caso de erro no Add
        // ou se você decidir não adicioná-lo ao array (não é o caso aqui)
      end;

      Next;
    end;

    S := jArr.FormatJSON();

    FS := TFileStream.Create(aFileName, fmCreate);
    try
      FS.WriteBuffer(Pointer(S)^, Length(S));
      Result := True;
    finally
      FS.Free;
    end;
  finally
    jArr.Free;
    FreeAndNil(Qry);
  end;
end;

function ExportCompleteTaxonomy(aFileName: String): Boolean;
var
  TaxaQry: TSQLQuery;
  SynQry: TSQLQuery;
  VernQry: TSQLQuery;
  CountryQry: TSQLQuery;
  CountQry: TSQLQuery;
  FS: TFileStream;
  TaxonObj: TJSONObject;
  ChildObj: TJSONObject;
  SynonymsArr: TJSONArray;
  VernacularArr: TJSONArray;
  CountriesArr: TJSONArray;
  TotalTaxa: Integer;
  Exported: Integer;
  Percent: Integer;
begin
  Result := False;

  TaxaQry := TSQLQuery.Create(nil);
  SynQry := TSQLQuery.Create(nil);
  VernQry := TSQLQuery.Create(nil);
  CountryQry := TSQLQuery.Create(nil);
  CountQry := TSQLQuery.Create(nil);
  FS := nil;
  TotalTaxa := 0;
  Exported := 0;

  try
    Parar := False;
    if not dlgLoading.Visible then
      dlgLoading.Show;
    dlgLoading.UpdateProgress('Exporting complete taxonomy...', -1, True);

    TaxaQry.DataBase := dmTaxa.sqlCon;
    SynQry.DataBase := dmTaxa.sqlCon;
    VernQry.DataBase := dmTaxa.sqlCon;
    CountryQry.DataBase := dmTaxa.sqlCon;
    CountQry.DataBase := dmTaxa.sqlCon;

    CountQry.SQL.Text := 'SELECT COUNT(*) AS total_taxa FROM zoo_taxa WHERE accepted_status = 1 AND active_status = 1';
    CountQry.Open;
    TotalTaxa := CountQry.FieldByName('total_taxa').AsInteger;
    CountQry.Close;

    if TotalTaxa = 0 then
    begin
      dlgLoading.UpdateProgress('Exporting complete taxonomy... (0/0)', 100, False);
      Result := True;
      Exit;
    end;

    TaxaQry.SQL.Add('SELECT');
    TaxaQry.SQL.Add('  z.taxon_id,');
    TaxaQry.SQL.Add('  z.full_name,');
    TaxaQry.SQL.Add('  z.authorship,');
    TaxaQry.SQL.Add('  z.formatted_name,');
    TaxaQry.SQL.Add('  z.taxon_concept_id,');
    TaxaQry.SQL.Add('  z.quick_code,');
    TaxaQry.SQL.Add('  z.rank_id,');
    TaxaQry.SQL.Add('  r.rank_name,');
    TaxaQry.SQL.Add('  r.rank_acronym,');
    TaxaQry.SQL.Add('  z.parent_taxon_id,');
    TaxaQry.SQL.Add('  p.full_name AS parent_taxon_name,');
    TaxaQry.SQL.Add('  z.iucn_status,');
    TaxaQry.SQL.Add('  z.extinct,');
    TaxaQry.SQL.Add('  z.extinction_year,');
    TaxaQry.SQL.Add('  z.sort_num,');
    TaxaQry.SQL.Add('  z.order_id,');
    TaxaQry.SQL.Add('  o.full_name AS order_name,');
    TaxaQry.SQL.Add('  z.family_id,');
    TaxaQry.SQL.Add('  f.full_name AS family_name,');
    TaxaQry.SQL.Add('  z.subfamily_id,');
    TaxaQry.SQL.Add('  sf.full_name AS subfamily_name,');
    TaxaQry.SQL.Add('  z.genus_id,');
    TaxaQry.SQL.Add('  g.full_name AS genus_name,');
    TaxaQry.SQL.Add('  z.species_id,');
    TaxaQry.SQL.Add('  sp.full_name AS species_name,');
    TaxaQry.SQL.Add('  z.subspecies_group_id,');
    TaxaQry.SQL.Add('  sg.full_name AS subspecies_group_name,');
    TaxaQry.SQL.Add('  z.incertae_sedis,');
    TaxaQry.SQL.Add('  z.ebird_code,');
    TaxaQry.SQL.Add('  z.distribution,');
    TaxaQry.SQL.Add('  z.accepted_status,');
    TaxaQry.SQL.Add('  z.marked_status,');
    TaxaQry.SQL.Add('  z.active_status,');
    TaxaQry.SQL.Add('  z.insert_date,');
    TaxaQry.SQL.Add('  z.update_date');
    TaxaQry.SQL.Add('FROM zoo_taxa AS z');
    TaxaQry.SQL.Add('LEFT JOIN taxon_ranks AS r ON z.rank_id = r.rank_id');
    TaxaQry.SQL.Add('LEFT JOIN zoo_taxa AS p ON z.parent_taxon_id = p.taxon_id');
    TaxaQry.SQL.Add('LEFT JOIN zoo_taxa AS o ON z.order_id = o.taxon_id');
    TaxaQry.SQL.Add('LEFT JOIN zoo_taxa AS f ON z.family_id = f.taxon_id');
    TaxaQry.SQL.Add('LEFT JOIN zoo_taxa AS sf ON z.subfamily_id = sf.taxon_id');
    TaxaQry.SQL.Add('LEFT JOIN zoo_taxa AS g ON z.genus_id = g.taxon_id');
    TaxaQry.SQL.Add('LEFT JOIN zoo_taxa AS sp ON z.species_id = sp.taxon_id');
    TaxaQry.SQL.Add('LEFT JOIN zoo_taxa AS sg ON z.subspecies_group_id = sg.taxon_id');
    TaxaQry.SQL.Add('WHERE z.accepted_status = 1 AND z.active_status = 1');
    TaxaQry.SQL.Add('ORDER BY z.sort_num ASC, z.taxon_id ASC');

    SynQry.SQL.Add('SELECT');
    SynQry.SQL.Add('  synonym_id,');
    SynQry.SQL.Add('  taxon_id,');
    SynQry.SQL.Add('  full_name,');
    SynQry.SQL.Add('  valid_status,');
    SynQry.SQL.Add('  marked_status,');
    SynQry.SQL.Add('  active_status,');
    SynQry.SQL.Add('  insert_date,');
    SynQry.SQL.Add('  update_date');
    SynQry.SQL.Add('FROM zoo_taxa_synonyms');
    SynQry.SQL.Add('WHERE taxon_id = :taxon_id');
    SynQry.SQL.Add('  AND LOWER(TRIM(full_name)) <> LOWER(TRIM(:parent_name))');
    SynQry.SQL.Add('ORDER BY full_name ASC');

    VernQry.SQL.Add('SELECT');
    VernQry.SQL.Add('  v.vernacular_id,');
    VernQry.SQL.Add('  v.taxon_id,');
    VernQry.SQL.Add('  v.language_id,');
    VernQry.SQL.Add('  l.language_name,');
    VernQry.SQL.Add('  l.macrolanguage_code,');
    VernQry.SQL.Add('  l.country_code,');
    VernQry.SQL.Add('  l.variation_code,');
    VernQry.SQL.Add('  v.vernacular_name,');
    VernQry.SQL.Add('  v.preferred,');
    VernQry.SQL.Add('  v.marked_status,');
    VernQry.SQL.Add('  v.active_status,');
    VernQry.SQL.Add('  v.insert_date,');
    VernQry.SQL.Add('  v.update_date');
    VernQry.SQL.Add('FROM vernacular_names AS v');
    VernQry.SQL.Add('LEFT JOIN languages AS l ON v.language_id = l.language_id');
    VernQry.SQL.Add('WHERE v.taxon_id = :taxon_id');
    VernQry.SQL.Add('ORDER BY v.preferred DESC, v.vernacular_name ASC');

    CountryQry.SQL.Add('SELECT');
    CountryQry.SQL.Add('  zc.taxon_country_id,');
    CountryQry.SQL.Add('  zc.taxon_id,');
    CountryQry.SQL.Add('  zc.country_id,');
    CountryQry.SQL.Add('  c.country_code,');
    CountryQry.SQL.Add('  c.country_name,');
    CountryQry.SQL.Add('  zc.marked_status,');
    CountryQry.SQL.Add('  zc.active_status,');
    CountryQry.SQL.Add('  zc.insert_date,');
    CountryQry.SQL.Add('  zc.update_date');
    CountryQry.SQL.Add('FROM zoo_taxa_countries AS zc');
    CountryQry.SQL.Add('LEFT JOIN countries AS c ON zc.country_id = c.country_id');
    CountryQry.SQL.Add('WHERE zc.taxon_id = :taxon_id');
    CountryQry.SQL.Add('ORDER BY c.country_name ASC');

    FS := TFileStream.Create(aFileName, fmCreate);

    TaxaQry.Open;
    dlgLoading.UpdateProgress(Format('Exporting complete taxonomy... (0/%d)', [TotalTaxa]), 0, True);
    while not TaxaQry.EOF do
    begin
      if Parar then
        Break;

      TaxonObj := TJSONObject.Create;
      try
        AddNullableInteger(TaxonObj, 'taxon_id', TaxaQry.FieldByName('taxon_id'));
        AddNullableString(TaxonObj, 'full_name', TaxaQry.FieldByName('full_name'));
        AddNullableString(TaxonObj, 'authorship', TaxaQry.FieldByName('authorship'));
        AddNullableString(TaxonObj, 'formatted_name', TaxaQry.FieldByName('formatted_name'));
        AddNullableString(TaxonObj, 'taxon_concept_id', TaxaQry.FieldByName('taxon_concept_id'));
        AddNullableString(TaxonObj, 'quick_code', TaxaQry.FieldByName('quick_code'));
        AddNullableInteger(TaxonObj, 'rank_id', TaxaQry.FieldByName('rank_id'));
        AddNullableString(TaxonObj, 'rank_name', TaxaQry.FieldByName('rank_name'));
        AddNullableString(TaxonObj, 'rank_acronym', TaxaQry.FieldByName('rank_acronym'));
        AddNullableInteger(TaxonObj, 'parent_taxon_id', TaxaQry.FieldByName('parent_taxon_id'));
        AddNullableString(TaxonObj, 'parent_taxon_name', TaxaQry.FieldByName('parent_taxon_name'));
        AddNullableString(TaxonObj, 'iucn_status', TaxaQry.FieldByName('iucn_status'));
        AddNullableBoolean(TaxonObj, 'extinct', TaxaQry.FieldByName('extinct'));
        AddNullableString(TaxonObj, 'extinction_year', TaxaQry.FieldByName('extinction_year'));
        AddNullableFloat(TaxonObj, 'sort_num', TaxaQry.FieldByName('sort_num'));
        AddNullableInteger(TaxonObj, 'order_id', TaxaQry.FieldByName('order_id'));
        AddNullableString(TaxonObj, 'order_name', TaxaQry.FieldByName('order_name'));
        AddNullableInteger(TaxonObj, 'family_id', TaxaQry.FieldByName('family_id'));
        AddNullableString(TaxonObj, 'family_name', TaxaQry.FieldByName('family_name'));
        AddNullableInteger(TaxonObj, 'subfamily_id', TaxaQry.FieldByName('subfamily_id'));
        AddNullableString(TaxonObj, 'subfamily_name', TaxaQry.FieldByName('subfamily_name'));
        AddNullableInteger(TaxonObj, 'genus_id', TaxaQry.FieldByName('genus_id'));
        AddNullableString(TaxonObj, 'genus_name', TaxaQry.FieldByName('genus_name'));
        AddNullableInteger(TaxonObj, 'species_id', TaxaQry.FieldByName('species_id'));
        AddNullableString(TaxonObj, 'species_name', TaxaQry.FieldByName('species_name'));
        AddNullableInteger(TaxonObj, 'subspecies_group_id', TaxaQry.FieldByName('subspecies_group_id'));
        AddNullableString(TaxonObj, 'subspecies_group_name', TaxaQry.FieldByName('subspecies_group_name'));
        AddNullableInteger(TaxonObj, 'incertae_sedis', TaxaQry.FieldByName('incertae_sedis'));
        AddNullableString(TaxonObj, 'ebird_code', TaxaQry.FieldByName('ebird_code'));
        AddNullableString(TaxonObj, 'distribution', TaxaQry.FieldByName('distribution'));
        AddNullableBoolean(TaxonObj, 'accepted_status', TaxaQry.FieldByName('accepted_status'));
        //AddNullableBoolean(TaxonObj, 'marked_status', TaxaQry.FieldByName('marked_status'));
        //AddNullableBoolean(TaxonObj, 'active_status', TaxaQry.FieldByName('active_status'));
        AddNullableString(TaxonObj, 'insert_date', TaxaQry.FieldByName('insert_date'));
        AddNullableString(TaxonObj, 'update_date', TaxaQry.FieldByName('update_date'));

        SynonymsArr := TJSONArray.Create;
        SynQry.ParamByName('taxon_id').AsInteger := TaxaQry.FieldByName('taxon_id').AsInteger;
        SynQry.ParamByName('parent_name').AsString := TaxaQry.FieldByName('full_name').AsString;
        SynQry.Open;
        SynQry.First;
        while not SynQry.EOF do
        begin
          ChildObj := TJSONObject.Create;
          //AddNullableInteger(ChildObj, 'synonym_id', SynQry.FieldByName('synonym_id'));
          //AddNullableInteger(ChildObj, 'taxon_id', SynQry.FieldByName('taxon_id'));
          AddNullableString(ChildObj, 'full_name', SynQry.FieldByName('full_name'));
          //AddNullableBoolean(ChildObj, 'valid_status', SynQry.FieldByName('valid_status'));
          //AddNullableBoolean(ChildObj, 'marked_status', SynQry.FieldByName('marked_status'));
          //AddNullableBoolean(ChildObj, 'active_status', SynQry.FieldByName('active_status'));
          //AddNullableString(ChildObj, 'insert_date', SynQry.FieldByName('insert_date'));
          //AddNullableString(ChildObj, 'update_date', SynQry.FieldByName('update_date'));
          SynonymsArr.Add(ChildObj);
          SynQry.Next;
        end;
        SynQry.Close;
        TaxonObj.Add('synonyms', SynonymsArr);

        VernacularArr := TJSONArray.Create;
        VernQry.ParamByName('taxon_id').AsInteger := TaxaQry.FieldByName('taxon_id').AsInteger;
        VernQry.Open;
        VernQry.First;
        while not VernQry.EOF do
        begin
          ChildObj := TJSONObject.Create;
          //AddNullableInteger(ChildObj, 'vernacular_id', VernQry.FieldByName('vernacular_id'));
          //AddNullableInteger(ChildObj, 'taxon_id', VernQry.FieldByName('taxon_id'));
          //AddNullableInteger(ChildObj, 'language_id', VernQry.FieldByName('language_id'));
          AddNullableString(ChildObj, 'language_name', VernQry.FieldByName('language_name'));
          AddNullableString(ChildObj, 'macrolanguage_code', VernQry.FieldByName('macrolanguage_code'));
          AddNullableString(ChildObj, 'country_code', VernQry.FieldByName('country_code'));
          AddNullableString(ChildObj, 'variation_code', VernQry.FieldByName('variation_code'));
          AddNullableString(ChildObj, 'vernacular_name', VernQry.FieldByName('vernacular_name'));
          AddNullableBoolean(ChildObj, 'preferred', VernQry.FieldByName('preferred'));
          //AddNullableBoolean(ChildObj, 'marked_status', VernQry.FieldByName('marked_status'));
          //AddNullableBoolean(ChildObj, 'active_status', VernQry.FieldByName('active_status'));
          //AddNullableString(ChildObj, 'insert_date', VernQry.FieldByName('insert_date'));
          //AddNullableString(ChildObj, 'update_date', VernQry.FieldByName('update_date'));
          VernacularArr.Add(ChildObj);
          VernQry.Next;
        end;
        VernQry.Close;
        TaxonObj.Add('vernacular_names', VernacularArr);

        CountriesArr := TJSONArray.Create;
        CountryQry.ParamByName('taxon_id').AsInteger := TaxaQry.FieldByName('taxon_id').AsInteger;
        CountryQry.Open;
        CountryQry.First;
        while not CountryQry.EOF do
        begin
          ChildObj := TJSONObject.Create;
          //AddNullableInteger(ChildObj, 'taxon_country_id', CountryQry.FieldByName('taxon_country_id'));
          //AddNullableInteger(ChildObj, 'taxon_id', CountryQry.FieldByName('taxon_id'));
          //AddNullableInteger(ChildObj, 'country_id', CountryQry.FieldByName('country_id'));
          AddNullableString(ChildObj, 'country_code', CountryQry.FieldByName('country_code'));
          //AddNullableString(ChildObj, 'country_name', CountryQry.FieldByName('country_name'));
          //AddNullableBoolean(ChildObj, 'marked_status', CountryQry.FieldByName('marked_status'));
          //AddNullableBoolean(ChildObj, 'active_status', CountryQry.FieldByName('active_status'));
          //AddNullableString(ChildObj, 'insert_date', CountryQry.FieldByName('insert_date'));
          //AddNullableString(ChildObj, 'update_date', CountryQry.FieldByName('update_date'));
          CountriesArr.Add(ChildObj);
          CountryQry.Next;
        end;
        CountryQry.Close;
        TaxonObj.Add('countries', CountriesArr);

        WriteJSONLLine(FS, TaxonObj.AsJSON);
      finally
        TaxonObj.Free;
      end;

      Inc(Exported);
      if (Exported mod 100 = 0) or (Exported = TotalTaxa) then
      begin
        Percent := Round((Exported * 100.0) / TotalTaxa);
        dlgLoading.UpdateProgress(
          Format('Exporting complete taxonomy... (%d/%d)', [Exported, TotalTaxa]),
          Percent,
          True
        );
      end;

      TaxaQry.Next;
    end;
    TaxaQry.Close;

    if Parar then
    begin
      if FileExists(aFileName) then
        DeleteFile(aFileName);
      Exit(False);
    end;

    dlgLoading.UpdateProgress(Format('Exporting complete taxonomy... (%d/%d)', [Exported, TotalTaxa]), 100, False);
    Result := True;
  finally
    if dlgLoading.Visible then
      dlgLoading.Hide;
    dlgLoading.Max := 100;
    dlgLoading.Progress := 0;
    dlgLoading.ShowCancel := False;

    FreeAndNil(FS);
    FreeAndNil(CountQry);
    FreeAndNil(CountryQry);
    FreeAndNil(VernQry);
    FreeAndNil(SynQry);
    FreeAndNil(TaxaQry);
  end;
end;

end.

