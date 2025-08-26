unit data_select;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, data_types;

  procedure SetSelectSQL(const aSQL: TStrings; aTable: TTableType; var aAlias: String);
  procedure SetCountriesSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: TSortDirection = sdNone);
  procedure SetLanguagesSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: TSortDirection = sdNone);
  procedure SetPackagesSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: TSortDirection = sdNone);
  procedure SetTaxonRanksSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: TSortDirection = sdNone);
  procedure SetZooTaxaSQL(const aSQL: TStrings; aFilter: TFilterValue;
    aSorting: String = ''; aDirection: TSortDirection = sdNone);

implementation

uses
  udm_taxa;

procedure SetSelectSQL(const aSQL: TStrings; aTable: TTableType; var aAlias: String);
begin
  case aTable of
    tbNone: ;
    tbTaxonRanks:       SetTaxonRanksSQL(aSQL, fvNone);
    tbZooTaxa:          SetZooTaxaSQL(aSQL, fvNone);
    tbPackages:         SetPackagesSQL(aSQL, fvNone);
    tbTaxonChanges: ;
    tbCountries:        SetCountriesSQL(aSQL, fvNone);
    tbLanguages:        SetLanguagesSQL(aSQL, fvNone);
    tbVernacularNames: ;
    tbSynonyms: ;
    tbTaxonCountries: ;
  end;
end;

procedure SetTaxonRanksSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String;
  aDirection: TSortDirection);
begin
  with aSQL do
  begin
    Clear;
    Add('SELECT * FROM taxon_ranks');
    case aFilter of
      fvNone: ;   // do nothing
      fvReset:    Add('WHERE (active_status = 1)');
      fvAll:      Add('WHERE (active_status = 1)');
      fvMarked:   Add('WHERE (active_status = 1) AND (marked_status = 1)');
      fvDeleted:  Add('WHERE (active_status = 0)');
    end;
    if Trim(aSorting) <> EmptyStr then
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} SORT_DIRECTIONS[aDirection]);
  end;
end;

procedure SetZooTaxaSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String;
  aDirection: TSortDirection);
begin
  with aSQL do
  begin
    Clear;
    Add('SELECT z.*,');
    Add('    u.full_name AS parent_taxon_name,');
    Add('    o.full_name AS order_name,');
    Add('    f.full_name AS family_name,');
    Add('    s.full_name AS subfamily_name,');
    Add('    n.full_name AS genero_name,');
    Add('    e.full_name AS species_name,');
    Add('    g.full_name AS subspecies_group_name');
    Add('FROM zoo_taxa AS z');
    Add('LEFT JOIN zoo_taxa AS u ON z.parent_taxon_id = u.taxon_id');
    Add('LEFT JOIN zoo_taxa AS o ON z.order_id = o.taxon_id');
    Add('LEFT JOIN zoo_taxa AS f ON z.family_id = f.taxon_id');
    Add('LEFT JOIN zoo_taxa AS s ON z.subfamily_id = s.taxon_id');
    Add('LEFT JOIN zoo_taxa AS n ON z.genus_id = n.taxon_id');
    Add('LEFT JOIN zoo_taxa AS e ON z.species_id = e.taxon_id');
    Add('LEFT JOIN zoo_taxa AS g ON z.subspecies_group_id = g.taxon_id');
    case aFilter of
      fvNone: ;   // do nothing
      fvReset:    Add('WHERE (z.taxon_id = -1) AND (z.active_status = 1)');
      fvAll:      Add('WHERE (z.active_status = 1)');
      fvMarked:   Add('WHERE (z.active_status = 1) AND (z.marked_status = 1)');
      fvDeleted:  Add('WHERE (z.active_status = 0)');
    end;
    if Trim(aSorting) <> EmptyStr then
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} SORT_DIRECTIONS[aDirection]);
  end;
end;

procedure SetCountriesSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String;
  aDirection: TSortDirection);
begin
  with aSQL do
  begin
    Clear;
    Add('SELECT * FROM countries');
    case aFilter of
      fvNone: ;   // do nothing
      fvReset:    Add('WHERE (active_status = 1)');
      fvAll:      Add('WHERE (active_status = 1)');
      fvMarked:   Add('WHERE (active_status = 1) AND (marked_status = 1)');
      fvDeleted:  Add('WHERE (active_status = 0)');
    end;
    if Trim(aSorting) <> EmptyStr then
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} SORT_DIRECTIONS[aDirection]);
  end;
end;

procedure SetLanguagesSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String;
  aDirection: TSortDirection);
begin
  with aSQL do
  begin
    Clear;
    Add('SELECT * FROM languages');
    case aFilter of
      fvNone: ;   // do nothing
      fvReset:    Add('WHERE (active_status = 1)');
      fvAll:      Add('WHERE (active_status = 1)');
      fvMarked:   Add('WHERE (active_status = 1) AND (marked_status = 1)');
      fvDeleted:  Add('WHERE (active_status = 0)');
    end;
    if Trim(aSorting) <> EmptyStr then
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} SORT_DIRECTIONS[aDirection]);
  end;
end;

procedure SetPackagesSQL(const aSQL: TStrings; aFilter: TFilterValue; aSorting: String;
  aDirection: TSortDirection);
begin
  with aSQL do
  begin
    Clear;
    Add('SELECT * FROM packages');
    case aFilter of
      fvNone: ;   // do nothing
      fvReset:    Add('WHERE (active_status = 1)');
      fvAll:      Add('WHERE (active_status = 1)');
      fvMarked:   Add('WHERE (active_status = 1) AND (marked_status = 1)');
      fvDeleted:  Add('WHERE (active_status = 0)');
    end;
    if Trim(aSorting) <> EmptyStr then
      Add('ORDER BY ' + aSorting + {' COLLATE pt_BR ' +} SORT_DIRECTIONS[aDirection]);
  end;
end;

end.

