unit io_json;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, jsonparser;

  function ExportSpeciesList(aFileName: String; CountryId, LanguageId: Integer; VernacularNames: Boolean = False): Boolean;

implementation

uses udm_taxa;

function ExportSpeciesList(aFileName: String; CountryId, LanguageId: Integer; VernacularNames: Boolean
  ): Boolean;
var
  jArr: TJSONArray;
  jObj: TJSONObject;
  Qry: TSQLQuery;
  FS: TFileStream;
  S: String;
begin
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
    finally
      FS.Free;
    end;
  finally
    jArr.Free;
    FreeAndNil(Qry);
  end;
end;

end.

