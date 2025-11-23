unit utils_taxonomy;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, DB, SQLDB, StrUtils, RegExpr, CheckLst;

type
  TZooRank = (trNone, {Domain} trDomain, trSubDomain,
    {Kingdom}
    trHyperkingdom, trSuperkingdom, trKingdom, trSubkingdom, trInfrakingdom, trParvkingdom,
    {Phylum}
    trSuperphylum, trPhylum, trSubphylum, trInfraphylum, trMicrophylum,
    {Class}
    trSuperclass, trClass, trSubclass, trInfraclass, trSubterclass, trParvclass,
    {Division}
    trSuperdivision, trDivision, trSubdivision, trInfradivision,
    {Legion}
    trSuperlegion, trLegion, trSublegion, trInfralegion,
    {Cohort}
    trSupercohort, trCohort, trSubcohort, trInfracohort,
    {Order}
    trGigaorder, trMegaorder, trGrandorder, trHyperorder, trSuperorder, trSeriesOrder,
    trOrder, trNanorder, trHypoorder, trMinorder, trSuborder, trInfraorder, trParvorder,
    {Section}
    trSection, trSubsection,
    {Family}
    trGigafamily, trMegafamily, trGrandfamily, trHyperfamily, trSuperfamily, trEpifamily,
    trSeriesFamily, trGroupFamily, trFamily, trSubfamily, trInfrafamily,
    {Tribe}
    trSupertribe, trTribe, trSubtribe, trInfratribe,
    {Genus}
    trSupergenus, trGenus, trSubgenus,
    {Species}
    trSuperspecies, trSpecies,
    {Subspecies}
    trSubspecies, trMonotypicGroup, trPolitypicGroup,
    {eBird special taxa}
    trForm, trSpuh, trHybrid, trIntergrade, trDomestic, trSlash);

  TTaxonFilter = (tfAll, tfMain, {tfKingdoms, tfPhyla, tfClasses,} tfOrders, tfFamilies, tfTribes,
    tfGenera, tfSpecies, tfSubspecies, tfSubspeciesGroups, tfSpuhs, tfSlashes, tfForms, tfDomestics,
    tfHybrids, tfIntergrades);
  TTaxonFilters = set of TTaxonFilter;

  TBirdTaxonomy = (btClements, btIoc, btCbro);
  TBirdTaxonomies = set of TBirdTaxonomy;

  TTaxonomyAction = (taNew, taSplit, taLump, taMove, taUpdate);
  TApplyChangesTo = (acSelected, acMarked);
  TChangeSuffix   = (csKeep, csA, csUs, csUm, csI, csE);

  TBrackets = (brParenthesis, brSquare, brCurly);

  THierarchyLevel = record
    Id: Integer;
    Clear: Boolean;
  end;

  TTaxonHierarchy = record
    TaxonId: Integer;
    ParentTaxon: THierarchyLevel;
    Order: THierarchyLevel;
    Family: THierarchyLevel;
    Subfamily: THierarchyLevel;
    Genus: THierarchyLevel;
    Species: THierarchyLevel;
    SubspeciesGroup: THierarchyLevel;
  end;

const
  ZOOLOGICAL_RANKS: array [TZooRank] of String = ('', 'D.', 'SD.', 'HK.', 'SK.', 'K.', 'sk.', 'ik.', 'pk.', 'SPh.', 'ph.',
    'subph.', 'infraph.', 'microph.', 'sc.', 'c.', 'subc.', 'infrac.', 'stc.', 'parvc.', 'sdiv.',
    'div.', 'subdiv.', 'infradiv.', 'sleg.', 'leg.', 'subleg.', 'infraleg.', 'scoh.', 'coh.',
    'subcoh.', 'infracoh.', 'Gord.', 'Mord.', 'grandord.', 'Hord.', 'superod.', 'seriesord.',
    'ord.', 'nord.', 'hypoord.', 'minord.', 'subord.', 'infraord.', 'parvord.', 'sect.', 'subsect.',
    'Gfam.', 'Mfam.', 'grandfam.', 'hyperfam.', 'superfam.', 'epifam.', 'seriesfam.', 'groupfam.',
    'fam.', 'subfam.', 'infrafam.', 'supertr.', 'tr.', 'subtr.', 'infratr.', 'superg.', 'g.',
    'subg.', 'supersp.', 'sp.', 'ssp.', 'grp. (mono)', 'grp. (poli)', 'f.', 'spuh', 'hybrid',
    'intergrade', 'domest.', 'slash');
  Suffixes: array [TChangeSuffix] of String = ('', 'a', 'us', 'um', 'i', 'e');

const
  colorGroup: String      = 'green';
  colorSlash: String      = 'maroon';
  //colorSp: String         = 'black';
  colorSpuh: String       = 'purple';
  colorEnglish: String    = 'teal';
  colorDomestic: String   = 'cornflowerblue'; //'$00FF870F';
  colorForm: String       = 'cadetblue'; //'$00CCA400';
  colorHybrid: String     = 'darkslateblue'; //'$00D2003F';
  colorIntergrade: String = 'goldenrod'; //'$0000D2D2';
  colorAuthorship: String = 'gray';
  Bracks: array of String = ('(', ')', '[', ']');

  function Italic(const AText: String): String; inline;
  function Colored(const AText: String; const AColor: String): String; inline;
  function Bold(const AText: String): String; inline;
  function Enclosed(const AText: String; ABracket: TBrackets): String; inline;
  procedure ExtractParents(const AText: String; out Parent1, Parent2: String);
  function ChangeSuffix(const Suffix: TChangeSuffix; AText: String): String;

  function HaveMarkedTaxa: Boolean;

  function GetRankType(aRankKey: Integer): TZooRank;
  function GetRankFromTaxon(aTaxonKey: Integer): Integer;
  function GetRankKey(aRank: TZooRank): Integer;
  function GetTaxonHierarchy(aTaxonId: Integer): TTaxonHierarchy;

  function ResolveValidID(TaxonID: Integer): Integer;

  function FormatDomestic(const aName: String): String;
  function FormatForm(const aName: String): String;
  function FormatHybrid(const aName: String): String;
  function FormatIntergrade(const aName: String): String;
  function FormatMonotypicGroup(const aName: String): String;
  function FormatPolitypicGroup(const aName: String): String;
  function FormatSlash(const aName: String): String;
  function FormatSpuh(const aName: String): String;
  function FormattedBirdName(aName: String; aRank: Integer; aAuthor: String = ''): String;

  procedure LoadAuthorships(aConnection: TSQLConnection; aList: TStrings);
  procedure LoadTaxaRanks(aConnection: TSQLConnection; aList: TCheckListBox);

  procedure RewriteTaxonHierarchy;

  procedure SplitTaxon(aSubspeciesId: Integer);
  procedure LumpTaxon(aSpeciesId, ToSpeciesId: Integer);

  procedure MoveToSpecies(aSubspecies, ToSpecies: Integer; Suffix: TChangeSuffix = csKeep);
  procedure MoveToGenus(aSpecies, ToGenus: Integer; Suffix: TChangeSuffix = csKeep);
  procedure MoveToFamily(aTaxonId, toFamilyId: Integer);
  procedure MoveToOrder(aTaxonId, toOrderId: Integer);

  procedure UpdateScientificName(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery;
    ExecNow: Boolean = True);
  procedure UpdateVernacularName(aTaxonId, aLanguageId: Integer; aNewName: String; isPreferred: Boolean;
    aDataset: TSQLQuery; ExecNow: Boolean = True);
  procedure UpdateAuthorship(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery; ExecNow: Boolean = True);
  procedure UpdateDistribution(aTaxon: Integer; aDist: String; aDataset: TSQLQuery; ExecNow: Boolean = True);
  procedure UpdateExtinction(aTaxon: Integer; IsExtinct: Boolean; aYear: String; aDataset: TSQLQuery;
    ExecNow: Boolean = True);
  procedure UpdateCountryOccurrence(FromTaxonId, ToTaxonId: Integer);

  procedure CopySynonyms(FromTaxonId, ToTaxonId: Integer);
  procedure NormalizeSynonyms;

implementation

uses
  models_rank, models_taxon, data_types, data_getvalue, data_validations, udm_taxa, udlg_loading;

function Italic(const AText: String): String; inline;
begin
  Result := '<i>' + AText + '</i>';
end;

function Colored(const AText: String; const AColor: String): String; inline;
begin
  Result := Format('<font color="%s">%s</font>', [AColor, AText]);
end;

function Bold(const AText: String): String; inline;
begin
  Result := '<b>' + AText + '</b>';
end;

function Enclosed(const AText: String; ABracket: TBrackets): String;
begin
  case ABracket of
    brParenthesis:  Result := '(' + AText + ')';
    brSquare:       Result := '[' + AText + ']';
    brCurly:        Result := '{' + AText + '}';
  end;
end;

procedure ExtractParents(const AText: String; out Parent1, Parent2: String);
var
  aName: String;
begin
  if (Pos(' x ', AText) > 0) then
  begin
    aName := StringReplace(AText, ' x ', ' | ', [rfReplaceAll]);
    Parent1 := Trim(ExtractDelimited(1, aName, ['|']));
    Parent2 := Trim(ExtractDelimited(2, aName, ['|']));
  end
  else
  begin
    Parent1 := AText;
    Parent2 := EmptyStr;
  end;
end;

function ChangeSuffix(const Suffix: TChangeSuffix; AText: String): String;
begin
  case Suffix of
    csKeep: Result := AText;
    csA:  Result := ReplaceRegExpr('(us|um)\b', AText, 'a');
    csUs: Result := ReplaceRegExpr('(a|um)\b', AText, 'us');
    csUm: Result := ReplaceRegExpr('(a|e|us)\b', AText, 'um');
    csI:  Result := ReplaceRegExpr('(a|us|um)\b', AText, 'i');
    csE:  Result := ReplaceRegExpr('(us|um)\b', AText, 'e');
  else
    Result := AText;
  end;
end;

function GetRankType(aRankKey: Integer): TZooRank;
var
  aRank: TRank;
  aRepo: TRankRepository;
  aZooRank: TZooRank;
begin
  Result := trNone;
  aRepo := TRankRepository.Create(dmTaxa.sqlCon);
  aRank := TRank.Create(aRankKey);
  aRepo.GetById(aRankKey, aRank);
  try
    for aZooRank in TZooRank do
      if aRank.Abbreviation = ZOOLOGICAL_RANKS[aZooRank] then
      begin
        Result := aZooRank;
        Break;
      end;
  finally
    FreeAndNil(aRank);
    aRepo.Free;
  end;
end;

function GetRankFromTaxon(aTaxonKey: Integer): Integer;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;
    Clear;
    Add('SELECT rank_id FROM zoo_taxa WHERE taxon_id = :keyv');
    ParamByName('KEYV').AsInteger := aTaxonKey;
    // GravaLogSQL(SQL);
    Open;
    if not(IsEmpty) then
      Result := FieldByName('rank_id').AsInteger
    else
      Result := 0;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function GetRankKey(aRank: TZooRank): Integer;
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;
    Clear;
    Add('SELECT rank_id FROM taxon_ranks WHERE rank_acronym = :abbreviation');
    ParamByName('abbreviation').AsString := ZOOLOGICAL_RANKS[aRank];
    // GravaLogSQL(SQL);
    Open;
    if not EOF then
      Result := FieldByName('rank_id').AsInteger
    else
      Result := 0;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function FormatDomestic(const aName: String): String;
var
  nome: String;
begin
  if (Pos('(', aName) > 0) then
    nome := Italic(Trim(ExtractDelimited(1, aName, Brackets))) + ' ' +
      Colored(Enclosed(Trim(ExtractDelimited(2, aName, Brackets)), brParenthesis), colorDomestic)
  else
    nome := aName;

  Result := nome;
end;

function FormatForm(const aName: String): String;
var
  nome: String;
  aBracket: TBrackets;
begin
  aBracket := brParenthesis;

  if (Pos('(', aName) > 0) or (Pos('[', aName) > 0) then
  begin
    if (Pos('(', aName) > 0) then
      aBracket := brParenthesis
    else
    if (Pos('[', aName) > 0) then
      aBracket := brSquare;

    if ExecRegExpr('.+(formes|idae|inae)', ExtractDelimited(1, aName, Brackets)) then
      nome := Trim(ExtractDelimited(1, aName, Brackets)) + ' ' +
        Colored(Enclosed(Trim(ExtractDelimited(2, aName, Brackets)), aBracket), colorForm)
    else
      nome := Italic(Trim(ExtractDelimited(1, aName, Brackets))) + ' ' +
        Colored(Enclosed(Trim(ExtractDelimited(2, aName, Brackets)), aBracket), colorForm);
  end
  else
  if (WordCount(aName, [' ']) = 3) then
    nome := Italic(ExtractWord(1, aName, [' ']) + ' ' + ExtractWord(2, aName, [' ']) + ' ' +
      Colored(ExtractWord(3, aName, [' ']), colorForm));

  Result := nome;
end;

function FormatHybrid(const aName: String): String;
var
  Parent1, Parent2, aBracket, nome: String;
begin
  ExtractParents(aName, Parent1, Parent2);

  if (Pos('(', Parent1) > 0) then
  begin
    if IsWordPresent('Domestic', Parent1, [' '] + Brackets) then
      aBracket := Colored(Enclosed(Trim(ExtractDelimited(2, Parent1, Brackets)), brParenthesis), colorDomestic)
    else
    if IsWordPresent('hybrid', Parent1, [' '] + Brackets) then
      aBracket := Colored(Enclosed(Trim(ExtractDelimited(2, Parent1, Brackets)), brParenthesis), colorHybrid);
    Parent1 := Trim(ExtractDelimited(1, Parent1, Brackets));
  end;

  if (Pos('sp.', Parent1) > 0) then
    Parent1 := Italic(Trim(ExtractDelimited(1, Parent1, [' ']))) + ' ' + Bold('sp.')
  else
    Parent1 := Italic(Parent1);

  if (aBracket <> EmptyStr) then
    Parent1 := Parent1 + ' ' + aBracket;

  aBracket := EmptyStr;
  if (Parent2 <> EmptyStr) then
  begin
    if (Pos('(', Parent2) > 0) then
    begin
      if IsWordPresent('Domestic', Parent2, [' '] + Brackets) then
        aBracket := Colored(Enclosed(Trim(ExtractDelimited(2, Parent2, Brackets)), brParenthesis), colorDomestic)
      else
      if IsWordPresent('hybrid', Parent2, [' '] + Brackets) then
        aBracket := Colored(Enclosed(Trim(ExtractDelimited(2, Parent2, Brackets)), brParenthesis), colorHybrid)
      else
        aBracket := Colored(Enclosed(Trim(ExtractDelimited(2, Parent2, Brackets)), brParenthesis), colorEnglish);
      Parent2 := Trim(ExtractDelimited(1, Parent2, Brackets));
    end;

    if (Pos('sp.', Parent2) > 0) then
    begin
      if ExecRegExpr('.+(formes|idae|inae)', ExtractDelimited(1, Parent2, [' '])) then
        Parent2 := Trim(ExtractDelimited(1, Parent2, [' '])) + ' ' + Bold('sp.')
      else
        Parent2 := Italic(Trim(ExtractDelimited(1, Parent2, [' ']))) + ' ' + Bold('sp.');
    end
    else
      Parent2 := Italic(Parent2);

    if (aBracket <> EmptyStr) then
      Parent2 := Parent2 + ' ' + aBracket;
  end;

  if (Parent2 <> EmptyStr) then
    nome := Parent1 + ' ' + Colored(Bold('×'), colorHybrid) + ' ' + Parent2
  else
    nome := Parent1;

  Result := nome;
end;

function FormatIntergrade(const aName: String): String;
var
  aBracket, nome, Parent1, Parent2: String;
begin
  aBracket := Trim(ExtractDelimited(2, aName, Brackets));
  if (aBracket <> EmptyStr) then
  begin
    if (Pos(' x ', aBracket) > 0) then
      aBracket := Colored(Enclosed(Italic(ExtractWord(1, aBracket, [' '])) + ' Group ' +
        Colored(Bold('×'), colorIntergrade) + ' ' + Italic(ExtractWord(4, aBracket, [' '])) +
        ' Group', brSquare), colorGroup)
    else
    if IsWordPresent('intergrade', aBracket, [' ']) then
      aBracket := Colored(Enclosed(Italic(ExtractWord(1, aBracket, [' '])) + ' intergrade', brParenthesis), colorIntergrade)
    else
    if IsWordPresent('Group', aBracket, [' ']) then
      aBracket := Colored(Enclosed(Italic(ExtractWord(1, aBracket, [' '])) + ' Group', brSquare), colorGroup);
  end;

  if (Pos(' x ', aName) = 0) then
  begin
    nome := Italic(Trim(ExtractDelimited(1, aName, Brackets))) + ' ' + aBracket;
  end
  else
  begin
    if ExecRegExpr('.+ \[.+ x .+\]', aName) then
    begin
      nome := Italic(Trim(ExtractDelimited(1, aName, Brackets))) + ' ' + aBracket;
    end
    else
    begin
      ExtractParents(aName, Parent1, Parent2);

      if (Pos(']', Parent1) > 0) then
        Parent1 := Italic(Trim(ExtractDelimited(1, aName, Brackets))) + ' ' + aBracket
      else
      if (Pos('/', Parent1) > 0) then
        Parent1 := Italic(ExtractDelimited(1, Parent1, [' ']) + ' ' + ExtractWord(2, Parent1, [' ']) + ' ' +
          Colored(ExtractWord(3, Parent1, [' ']), colorGroup))
      else
        Parent1 := Italic(Parent1);

      if (Pos('[', Parent2) > 0) then
        Parent2 := aBracket
      else
      if (Pos('/', Parent2) > 0) then
        Parent2 := Colored(Italic(Parent2), colorGroup)
      else
        Parent2 := Italic(Parent2);

      nome := Parent1 + ' ' + Colored(Bold('×'), colorIntergrade) + ' ' + Parent2;
    end;
  end;

  Result := nome;
end;

function FormatMonotypicGroup(const aName: String): String;
var
  nome: String;
begin
  nome := Italic(Format('%s %s %s', [ExtractWord(1, aName, [' ']),
    ExtractWord(2, aName, [' ']), Colored(ExtractWord(3, aName, [' ']), colorGroup)]));

  Result := nome;
end;

function FormatPolitypicGroup(const aName: String): String;
var
  nome, aBracket: String;
begin
  if (Pos('/', aName) > 0) then
    nome := Italic(Format('%s %s %s', [ExtractWord(1, aName, [' ']),
      ExtractWord(2, aName, [' ']), Colored(ExtractWord(3, aName, [' ']), colorGroup)]))
  else
  if (Pos('[', aName) > 0) then
  begin
    aBracket := Trim(ExtractDelimited(2, aName, ['[',']']));
    nome := Italic(ExtractWord(1, aName, [' ']) + ' ' + ExtractWord(2, aName, [' '])) + ' ' +
      Colored(Enclosed(Italic(ExtractWord(1, aBracket, [' '])) + ' ' + ExtractWord(2, aBracket, [' ']), brSquare), colorGroup);
  end;

  Result := nome;
end;

function FormatSlash(const aName: String): String;
var
  outBrackets, aBracket, nome: String;
begin
  if (Pos('(', aName) > 0) then
  begin
    outBrackets := Trim(ExtractDelimited(1, aName, Brackets));
    aBracket := Colored(Enclosed(Trim(ExtractDelimited(2, aName, Brackets)), brParenthesis), colorEnglish);
  end
  else
    outBrackets := aName;

  if (Pos('sp.', outBrackets) > 0) then
  begin
    if ExecRegExpr('.+ sp.\/.+', outBrackets) then
    begin
      nome := Colored(Italic(ExtractWord(1, outBrackets, [' '])), colorSlash) + ' ' + Bold('sp.') +
        Colored(ExtractWord(2, outBrackets, ['/']), colorSlash);
    end
    else
    begin
      outBrackets := StringReplace(outBrackets, ' sp.', '', []);
      nome := Colored(Italic(outBrackets), colorSlash) + ' ' + Bold('sp.');
    end;
  end
  else
  if ExecRegExpr('.+\/[A-Z].+', outBrackets) then
    nome := Colored(Italic(outBrackets), colorSlash)
  else
  if (WordCount(outBrackets, [' ']) = 2) then
    nome := Italic(ExtractWord(1, outBrackets, [' ']) + ' ' + Colored(ExtractWord(2, outBrackets, [' ']), colorSlash));

  if (aBracket <> EmptyStr) then
    nome := nome + ' ' + aBracket;

  Result := nome;
end;

function FormatSpuh(const aName: String): String;
var
  outBrackets, aBracket, nome: String;
begin
  if (Pos('(', aName) > 0) then
  begin
    outBrackets := Trim(ExtractDelimited(1, aName, Brackets));
    aBracket := Trim(ExtractDelimited(2, aName, Brackets));

    if IsWordPresent('Domestic', aBracket, [' ']) then
      aBracket := Colored(Enclosed(aBracket, brParenthesis), colorDomestic)
    else
    if ExecRegExpr('^[a-z].+ complex$', aBracket) then
      aBracket := Colored(Enclosed(Italic(ExtractWord(1, aBracket, [' '])) + ' complex', brParenthesis), colorEnglish)
    else
    if ExecRegExpr('^former .+ sp.$', aBracket) then
      aBracket := Colored(Enclosed('former ' + Italic(ExtractWord(2, aBracket, [' '])) + ' sp.', brParenthesis), colorEnglish)
    else
      aBracket := Colored(Enclosed(aBracket, brParenthesis), colorEnglish);
  end
  else
    outBrackets := aName;

  if (Pos('/', outBrackets) > 0) then
  begin
    if not IsWordPresent('sp.', outBrackets, [' ']) then
    begin
      nome := Colored(Italic(outBrackets), colorSpuh);
    end
    else
    begin
      if ExecRegExpr('.+(formes|idae|inae).*', outBrackets) then
      begin
        nome := Colored(ExtractWord(1, outBrackets, [' ']), colorSpuh) + ' ' + Bold('sp.');
      end
      else
      if IsWordPresent('eagle', outBrackets, ['/', ' ']) then
      begin
        nome := Colored(Italic(ExtractWord(1, outBrackets, ['/', ' '])) + '/' +
          ExtractWord(2, outBrackets, ['/', ' ']), colorSpuh) + ' ' + Bold('sp.');
      end
      else
      begin
        nome := Colored(Italic(ExtractWord(1, outBrackets, [' '])), colorSpuh) + ' ' + Bold('sp.');
      end;
    end;
  end
  else
  begin
    if ExecRegExpr('.+(formes|idae|inae).*', outBrackets) then
    begin
      nome := Colored(ExtractWord(1, outBrackets, [' ']), colorSpuh) + ' ' + Bold('sp.');
    end
    else
    begin
      nome := Colored(Italic(ExtractWord(1, outBrackets, [' '])), colorSpuh) + ' ' + Bold('sp.');
    end;
  end;

  if (aBracket <> EmptyStr) then
    nome := nome + ' ' + aBracket;

  Result := nome;
end;

function FormattedBirdName(aName: String; aRank: Integer; aAuthor: String): String;
var
  nome: String;
begin
  Result := EmptyStr;
  nome := EmptyStr;

  case GetRankType(aRank) of
    trDomain..trInfratribe:     nome := aName;
    trSupergenus..trSubspecies: nome := Italic(aName);
    trMonotypicGroup:           nome := FormatMonotypicGroup(aName);
    trPolitypicGroup:           nome := FormatPolitypicGroup(aName);
    trSpuh:                     nome := FormatSpuh(aName);
    trSlash:                    nome := FormatSlash(aName);
    trHybrid:                   nome := FormatHybrid(aName);
    trIntergrade:               nome := FormatIntergrade(aName);
    trForm:                     nome := FormatForm(aName);
    trDomestic:                 nome := FormatDomestic(aName);
  end;
  { Authorship }
  if aAuthor <> EmptyStr then
    nome := nome + ' ' + Colored(aAuthor, colorAuthorship);

  Result := nome;
end;

procedure LoadAuthorships(aConnection: TSQLConnection; aList: TStrings);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(aConnection);
  Qry.Database := aConnection;
  with Qry do
  try
    SQL.Clear;

    SQL.Add('SELECT DISTINCT authorship');
    SQL.Add('FROM zoo_taxa');
    SQL.Add('WHERE (authorship NOT NULL) AND (authorship <> '''')');
    SQL.Add('  AND (active_status = 1)');
    SQL.Add('ORDER BY authorship ASC');

    Open;
    if RecordCount > 0 then
    begin
      aList.BeginUpdate;
      aList.Clear;

      First;
      repeat
        aList.Add(FieldByName('authorship').AsString);

        Next;
      until EOF;
      aList.EndUpdate;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure LoadTaxaRanks(aConnection: TSQLConnection; aList: TCheckListBox);
var
  Qry: TSQLQuery;
  Lista: TStrings;
begin
  Lista := TStringList.Create;
  Qry := TSQLQuery.Create(aConnection);
  Qry.Database := aConnection;
  with Qry do
  try
    SQL.Clear;

    SQL.Add('SELECT DISTINCT z.rank_id,');
    SQL.Add('  r.rank_name AS rank_name,');
    SQL.Add('  r.rank_seq  AS sort_num');
    SQL.Add('FROM zoo_taxa AS z');
    SQL.Add('LEFT JOIN taxon_ranks AS r ON z.rank_id = r.rank_id');
    SQL.Add('WHERE (z.rank_id > 0)');
    SQL.Add('  AND (z.active_status = 1)');
    SQL.Add('ORDER BY sort_num ASC');

    Open;
    if RecordCount > 0 then
    begin
      aList.Items.BeginUpdate;
      aList.Items.Clear;

      First;
      repeat
        Lista.Add(FieldByName('rank_name').AsString);

        Next;
      until EOF;
      aList.Items.Assign(Lista);
      aList.Items.EndUpdate;
    end;
    Close;
  finally
    FreeAndNil(Qry);
    Lista.Free;
  end;
end;

procedure RewriteTaxonHierarchy;
var
  Qry: TSQLQuery;
  iOrder, iFamily, iSubfamily, iGenus, iSpecies, iMonoGroup, iPoliGroup, iSubspecies: Integer;
begin
  dlgLoading.Show;
  dlgLoading.Max := 7;
  dlgLoading.UpdateProgress('Rewriting taxa hierarchy...', 0);
  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;
    Transaction := dmTaxa.sqlTrans;
    MacroCheck := True;

    iOrder := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ord.');
    iFamily := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'fam.');
    iSubfamily := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'subfam.');
    iGenus := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'g.');
    iSpecies := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'sp.');
    iMonoGroup := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'grp. (mono)');
    iPoliGroup := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'grp. (poli)');
    iSubspecies := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', 'ssp.');

    dmTaxa.sqlTrans.StartTransaction;
    try
      { Order }
      dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Orders...', dlgLoading.Progress + 1);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET order_id = taxon_id');
      Add('WHERE (zoo_taxa.rank_id = :rank_id)');
      ParamByName('RANK_ID').AsInteger := iOrder;
      ExecSQL;
      Application.ProcessMessages;

      { Family }
      dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Families...', dlgLoading.Progress + 1);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET family_id = zoo_taxa.taxon_id, order_id = parent.order_id');
      Add('FROM (SELECT taxon_id, order_id FROM zoo_taxa) AS parent');
      Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
      ParamByName('RANK_ID').AsInteger := iFamily;
      ExecSQL;
      Application.ProcessMessages;

      { Subfamily }
      dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Subfamilies...', dlgLoading.Progress + 1);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET subfamily_id = zoo_taxa.taxon_id, family_id = parent.family_id, order_id = parent.order_id');
      Add('FROM (SELECT taxon_id, order_id, family_id FROM zoo_taxa) AS parent');
      Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
      ParamByName('RANK_ID').AsInteger := iSubfamily;
      ExecSQL;
      Application.ProcessMessages;

      { Genus }
      dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Genera...', dlgLoading.Progress + 1);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET genus_id = zoo_taxa.taxon_id, subfamily_id = parent.subfamily_id, ');
      Add('  family_id = parent.family_id, order_id = parent.order_id');
      Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id FROM zoo_taxa) AS parent');
      Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
      ParamByName('RANK_ID').AsInteger := iGenus;
      ExecSQL;
      Application.ProcessMessages;

      { Species }
      dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Species...', dlgLoading.Progress + 1);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET species_id = zoo_taxa.taxon_id, genus_id = parent.genus_id, ');
      Add('  subfamily_id = parent.subfamily_id, family_id = parent.family_id, order_id = parent.order_id');
      Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id FROM zoo_taxa) AS parent');
      Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
      ParamByName('RANK_ID').AsInteger := iSpecies;
      ExecSQL;
      Application.ProcessMessages;

      { Mono and politypic groups }
      dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Species groups...', dlgLoading.Progress + 1);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET subspecies_group_id = zoo_taxa.taxon_id, species_id = parent.species_id, genus_id = parent.genus_id, ');
      Add('  subfamily_id = parent.subfamily_id, family_id = parent.family_id, order_id = parent.order_id');
      Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id, species_id FROM zoo_taxa) AS parent');
      Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
      ParamByName('RANK_ID').AsInteger := iMonoGroup;
      ExecSQL;
      Application.ProcessMessages;
      ParamByName('RANK_ID').AsInteger := iPoliGroup;
      ExecSQL;
      Application.ProcessMessages;

      { Subspecies, domestic, form }
      dlgLoading.UpdateProgress('Rewriting taxa hierarchy: Subspecies and others...', dlgLoading.Progress + 1);
      Clear;
      Add('UPDATE zoo_taxa');
      Add('SET subspecies_group_id = parent.subspecies_group_id, species_id = parent.species_id, ');
      Add('  genus_id = parent.genus_id, subfamily_id = parent.subfamily_id, family_id = parent.family_id, ');
      Add('  order_id = parent.order_id');
      Add('FROM (SELECT taxon_id, order_id, family_id, subfamily_id, genus_id, species_id, ');
      Add('  subspecies_group_id FROM zoo_taxa) AS parent');
      Add('WHERE (zoo_taxa.rank_id = :rank_id) AND (zoo_taxa.parent_taxon_id = parent.taxon_id)');
      ParamByName('RANK_ID').AsInteger := iSubspecies;
      ExecSQL;
      Application.ProcessMessages;

      dlgLoading.Hide;
      dlgLoading.Max := 100;
      dmTaxa.sqlTrans.Commit;
    except
      dmTaxa.sqlTrans.Rollback;
      raise Exception.Create('Error rewriting the taxa hierarchy');
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure SplitTaxon(aSubspeciesId: Integer);
var
  OldName, NewName, NewEpithet: String;
  ParentGenusId: Integer;
  Repo: TTaxonRepository;
  Ssp, toSp: TTaxon;
  SynRepo: TSynonymRepository;
  Synonym: TSynonym;
  Qry: TSQLQuery;
  SameSp: Boolean;
begin
  Repo := TTaxonRepository.Create(dmTaxa.sqlCon);
  Ssp := TTaxon.Create();
  Repo.GetById(aSubspeciesId, Ssp);
  toSp := TTaxon.Create();

  SynRepo := TSynonymRepository.Create(dmTaxa.sqlCon);
  Synonym := TSynonym.Create();

  OldName := GetName('zoo_taxa', 'full_name', 'taxon_id', aSubspeciesId);
  if Ssp.RankId = trPolitypicGroup then
  begin
    if Pos('/', OldName) > 0 then
    begin
      NewEpithet := InputBox('Split politypic group', 'New species epithet for ' + OldName, ExtractWord(3, OldName, [' ','/']));
      NewName := ExtractWord(1, OldName, [' ']) + ' ' + NewEpithet;
    end
    else
      NewName := ExtractWord(1, OldName, [' ']) + ' ' + Trim(ExtractWord(3, OldName, [' '] + Brackets))
  end
  else
    NewName := ExtractWord(1, OldName, [' ']) + ' ' + ExtractWord(3, OldName, [' ']);
  Repo.FindBy('full_name', NewName, toSp);
  SameSp := NewName = GetName('zoo_taxa', 'full_name', 'taxon_id', Ssp.ParentTaxonId);

  ParentGenusId := 0;

  try
    // If taxon exists, activate it
    if not toSp.IsNew then
    begin
      toSp.Distribution := Ssp.Distribution;
      toSp.Accepted := True;

      Repo.Update(toSp);
    end
    else
    // If taxon does not exist, create it
    begin
      ParentGenusId := GetKey('zoo_taxa', 'taxon_id', 'full_name', ExtractWord(1, OldName, [' ']));

      toSp.FullName := NewName;
      toSp.FormattedName := FormattedBirdName(NewName, GetRankKey(trSpecies));
      toSp.Authorship := Ssp.Authorship;
      toSp.RankId := trSpecies;
      toSp.ParentTaxonId := ParentGenusId;
      toSp.Extinct := Ssp.Extinct;
      toSp.ExtinctionYear := Ssp.ExtinctionYear;
      toSp.Distribution := Ssp.Distribution;
      toSp.EbirdCode := Ssp.EbirdCode;
      toSp.Accepted := True;

      Repo.Insert(toSp);

      Synonym.TaxonId := toSp.Id;
      Synonym.FullName := NewName;
      Synonym.Valid := True;

      SynRepo.Insert(Synonym);
    end;

    // Update subspecies
    //if not (Ssp.RankId = trPolitypicGroup) and (not SameSp) then
    begin
      Ssp.Accepted := False;
      Repo.Update(Ssp);
    end;

    // Update synonyms
    Synonym.Clear;
    SynRepo.FindByTaxon(toSp.Id, OldName, Synonym);
    if Synonym.IsNew then
    begin
      Synonym.TaxonId := toSp.Id;
      Synonym.FullName := OldName;

      SynRepo.Insert(Synonym);
    end;

    // Move subspecies when it is a politypic subspecies group
    if Ssp.RankId = trPolitypicGroup then
    begin
      Qry := TSQLQuery.Create(nil);
      with Qry, SQL do
      try
        DataBase := dmTaxa.sqlCon;
        Add('SELECT taxon_id FROM zoo_taxa');
        Add('WHERE (parent_taxon_id = :parent_taxon_id)');
        ParamByName('parent_taxon_id').AsInteger := aSubspeciesId;
        Open;
        if not EOF then
        begin
          First;
          repeat
            MoveToSpecies(FieldByName('taxon_id').AsInteger, toSp.Id);
            Next;
          until EOF;
        end;
        Close;
      finally
        FreeAndNil(Qry);
      end;
    end;
  finally
    FreeAndNil(Synonym);
    SynRepo.Free;
    FreeAndNil(Ssp);
    FreeAndNil(toSp);
    Repo.Free;
  end;
end;

procedure LumpTaxon(aSpeciesId, ToSpeciesId: Integer);
var
  OldName, LumpToName, NewName: String;
  Repo: TTaxonRepository;
  Species, toSsp: TTaxon;
  SynRepo: TSynonymRepository;
  Synonym: TSynonym;
  Qry: TSQLQuery;
begin
  Repo := TTaxonRepository.Create(dmTaxa.sqlCon);
  Species := TTaxon.Create();
  Repo.GetById(aSpeciesId, Species);
  toSsp := TTaxon.Create();

  SynRepo := TSynonymRepository.Create(dmTaxa.sqlCon);
  Synonym := TSynonym.Create();

  OldName := GetName('zoo_taxa', 'full_name', 'taxon_id', aSpeciesId);
  LumpToName := GetName('zoo_taxa', 'full_name', 'taxon_id', ToSpeciesId);
  NewName := LumpToName + ' ' + ExtractWord(2, OldName, [' ']);
  Repo.FindBy('full_name', NewName, toSsp);

  try
    // If taxon exists, activate it
    if not toSsp.IsNew then
    begin
      toSsp.Accepted := True;
      toSsp.Distribution := Species.Distribution;

      Repo.Update(toSsp);
    end
    else
    // If taxon does not exist, create it
    begin
      toSsp.FullName := NewName;
      toSsp.FormattedName := FormattedBirdName(NewName, GetRankKey(trSubspecies));
      toSsp.Authorship := Species.Authorship;
      toSsp.RankId := trSubspecies;
      toSsp.ParentTaxonId := ToSpeciesId;
      toSsp.Extinct := Species.Extinct;
      toSsp.ExtinctionYear := Species.ExtinctionYear;
      toSsp.Distribution := Species.Distribution;
      toSsp.EbirdCode := Species.EbirdCode;
      toSsp.Accepted := True;

      Repo.Insert(toSsp);

      Synonym.TaxonId := toSsp.Id;
      Synonym.FullName := NewName;
      Synonym.Valid := True;

      SynRepo.Insert(Synonym);
    end;

    // Update subspecies
    Species.Accepted := False;
    Repo.Update(Species);

    // Update synonyms
    Synonym.Clear;
    SynRepo.FindByTaxon(toSsp.Id, OldName, Synonym);
    if Synonym.IsNew then
    begin
      Synonym.TaxonId := toSsp.Id;
      Synonym.FullName := OldName;

      SynRepo.Insert(Synonym);
    end;

    // Move subspecies groups and subspecies
    Qry := TSQLQuery.Create(nil);
    with Qry, SQL do
    try
      DataBase := dmTaxa.sqlCon;
      Add('SELECT taxon_id FROM zoo_taxa');
      Add('WHERE (parent_taxon_id = :parent_taxon_id)');
      ParamByName('parent_taxon_id').AsInteger := aSpeciesId;
      Open;
      if not EOF then
      begin
        First;
        repeat
          MoveToSpecies(FieldByName('taxon_id').AsInteger, ToSpeciesId);
          Next;
        until EOF;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  finally
    FreeAndNil(Synonym);
    SynRepo.Free;
    FreeAndNil(toSsp);
    FreeAndNil(Species);
    Repo.Free;
  end;
end;

procedure MoveToSpecies(aSubspecies, ToSpecies: Integer; Suffix: TChangeSuffix);
var
  OldName, MoveToName, NewName: String;
  Repo: TTaxonRepository;
  Ssp, toSsp: TTaxon;
  SynRepo: TSynonymRepository;
  Synonym: TSynonym;
  Qry: TSQLQuery;
begin
  Repo := TTaxonRepository.Create(dmTaxa.sqlCon);
  Ssp := TTaxon.Create();
  Repo.GetById(aSubspecies, Ssp);
  toSsp := TTaxon.Create();

  SynRepo := TSynonymRepository.Create(dmTaxa.sqlCon);
  Synonym := TSynonym.Create();

  OldName := GetName('zoo_taxa', 'full_name', 'taxon_id', aSubspecies);
  MoveToName := GetName('zoo_taxa', 'full_name', 'taxon_id', ToSpecies);
  if (WordCount(MoveToName, [' ']) > 2) then
    MoveToName := ExtractWord(1, MoveToName, [' ']) + ' ' + ExtractWord(2, MoveToName, [' ']);
  if Ssp.RankId = trPolitypicGroup then
  begin
    if Pos('/', OldName) > 0 then
      NewName := MoveToName + ' ' + ChangeSuffix(Suffix, Trim(ExtractWord(3, OldName, [' '])))
    else
      NewName := MoveToName + ' ' + ChangeSuffix(Suffix, Trim(ExtractWord(3, OldName, [' '] + Brackets)));
  end
  else
  if Ssp.RankId = trForm then
  begin
    if (Pos('[', OldName) > 0) or (Pos('(', OldName) > 0) then
      NewName := MoveToName + ' ' + ChangeSuffix(Suffix, Trim(ExtractWord(3, OldName, Brackets)))
    else
      NewName := MoveToName + ' ' + ChangeSuffix(Suffix, Trim(ExtractWord(3, OldName, [' '])));
  end
  else
    NewName := MoveToName + ' ' + ChangeSuffix(Suffix, ExtractWord(3, OldName, [' ']));
  // Suffix
  //NewName := ChangeSuffix(Suffix, NewName);

  Repo.FindBy('full_name', NewName, toSsp);

  try
    // If taxon exists, update it
    if not toSsp.IsNew then
    begin
      toSsp.Accepted := True;
      toSsp.Distribution := Ssp.Distribution;
      toSsp.RankId := Ssp.RankId;
      toSsp.FormattedName := FormattedBirdName(NewName, GetRankKey(Ssp.RankId));

      Repo.Update(toSsp);
    end
    else
    // If taxon does not exist, create it
    begin
      toSsp.FullName := NewName;
      toSsp.FormattedName := FormattedBirdName(NewName, GetRankKey(Ssp.RankId));
      toSsp.Authorship := Ssp.Authorship;
      toSsp.RankId := Ssp.RankId;
      toSsp.ParentTaxonId := ToSpecies;
      toSsp.Extinct := Ssp.Extinct;
      toSsp.ExtinctionYear := Ssp.ExtinctionYear;
      toSsp.Distribution := Ssp.Distribution;
      toSsp.EbirdCode := Ssp.EbirdCode;
      toSsp.Accepted := True;

      Repo.Insert(toSsp);

      Synonym.TaxonId := toSsp.Id;
      Synonym.FullName := NewName;
      Synonym.Valid := True;

      SynRepo.Insert(Synonym);
    end;

    // Update subspecies
    Ssp.Accepted := False;
    Repo.Update(Ssp);

    // Update synonyms
    Synonym.Clear;
    SynRepo.FindByTaxon(toSsp.Id, OldName, Synonym);
    if Synonym.IsNew then
    begin
      Synonym.TaxonId := toSsp.Id;
      Synonym.FullName := OldName;

      SynRepo.Insert(Synonym);
    end;
    CopySynonyms(Ssp.Id, toSsp.Id);

    // Update country lists
    UpdateCountryOccurrence(Ssp.Id, toSsp.Id);

    // Move subspecies from subspecies group
    if Ssp.RankId = trPolitypicGroup then
    begin
      Qry := TSQLQuery.Create(nil);
      with Qry, SQL do
      try
        DataBase := dmTaxa.sqlCon;
        Add('SELECT taxon_id FROM zoo_taxa');
        Add('WHERE (parent_taxon_id = :parent_taxon_id)');
        ParamByName('parent_taxon_id').AsInteger := aSubspecies;
        Open;
        if not EOF then
        begin
          First;
          repeat
            MoveToSpecies(FieldByName('taxon_id').AsInteger, toSsp.Id, Suffix);
            Next;
          until EOF;
        end;
        Close;
      finally
        FreeAndNil(Qry);
      end;
    end;
  finally
    FreeAndNil(Synonym);
    SynRepo.Free;
    FreeAndNil(toSsp);
    FreeAndNil(Ssp);
    Repo.Free;
  end;
end;

procedure MoveToGenus(aSpecies, ToGenus: Integer; Suffix: TChangeSuffix);
var
  OldName, MoveToName, NewName: String;
  Repo: TTaxonRepository;
  Species, toSp: TTaxon;
  SynRepo: TSynonymRepository;
  Synonym: TSynonym;
  Qry: TSQLQuery;
begin
  Repo := TTaxonRepository.Create(dmTaxa.sqlCon);
  Species := TTaxon.Create();
  Repo.GetById(aSpecies, Species);
  toSp := TTaxon.Create();

  SynRepo := TSynonymRepository.Create(dmTaxa.sqlCon);
  Synonym := TSynonym.Create();

  OldName := GetName('zoo_taxa', 'full_name', 'taxon_id', aSpecies);
  MoveToName := GetName('zoo_taxa', 'full_name', 'taxon_id', ToGenus);
  NewName := MoveToName + ' ' + ChangeSuffix(Suffix, ExtractWord(2, OldName, [' ']));
  // Suffix
  //NewName := ChangeSuffix(Suffix, NewName);

  Repo.FindBy('full_name', NewName, toSp);

  try
    // If taxon exists, update it
    if not toSp.IsNew then
    begin
      toSp.Accepted := True;
      toSp.Distribution := Species.Distribution;

      Repo.Update(toSp);
    end
    else
    // If taxon does not exist, create it
    begin
      toSp.FullName := NewName;
      toSp.FormattedName := FormattedBirdName(NewName, GetRankKey(trSpecies));
      toSp.Authorship := Species.Authorship;
      toSp.RankId := trSpecies;
      toSp.ParentTaxonId := ToGenus;
      toSp.Extinct := Species.Extinct;
      toSp.ExtinctionYear := Species.ExtinctionYear;
      toSp.Distribution := Species.Distribution;
      toSp.EbirdCode := Species.EbirdCode;
      toSp.Accepted := True;

      Repo.Insert(toSp);

      Synonym.TaxonId := toSp.Id;
      Synonym.FullName := NewName;
      Synonym.Valid := True;

      SynRepo.Insert(Synonym);
    end;

    // Update subspecies
    Species.Accepted := False;
    Repo.Update(Species);

    // Update synonyms
    Synonym.Clear;
    SynRepo.FindByTaxon(toSp.Id, OldName, Synonym);
    if Synonym.IsNew then
    begin
      Synonym.TaxonId := toSp.Id;
      Synonym.FullName := OldName;

      SynRepo.Insert(Synonym);
    end;
    CopySynonyms(Species.Id, toSp.Id);

    // Update country lists
    UpdateCountryOccurrence(Species.Id, toSp.Id);

    // Move subspecies groups and subspecies
    Qry := TSQLQuery.Create(nil);
    with Qry, SQL do
    try
      DataBase := dmTaxa.sqlCon;
      Add('SELECT taxon_id FROM zoo_taxa');
      Add('WHERE (parent_taxon_id = :parent_taxon_id)');
      ParamByName('parent_taxon_id').AsInteger := aSpecies;
      Open;
      if not EOF then
      begin
        First;
        repeat
          MoveToSpecies(FieldByName('taxon_id').AsInteger, toSp.Id, Suffix);
          Next;
        until EOF;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  finally
    FreeAndNil(Synonym);
    SynRepo.Free;
    FreeAndNil(Species);
    FreeAndNil(toSp);
    Repo.Free;
  end;
end;

procedure MoveToFamily(aTaxonId, toFamilyId: Integer);
var
  Repo: TTaxonRepository;
  Taxon: TTaxon;
begin
  Repo := TTaxonRepository.Create(dmTaxa.sqlCon);
  Taxon := TTaxon.Create();
  try
    Repo.GetById(aTaxonId, Taxon);
    if not Taxon.IsNew then
    begin
      if Taxon.RankId = trGenus then
        Taxon.ParentTaxonId := toFamilyId;
      Taxon.FamilyId := toFamilyId;

      Repo.Update(Taxon);
    end;
  finally
    FreeAndNil(Taxon);
    Repo.Free;
  end;
end;

procedure MoveToOrder(aTaxonId, toOrderId: Integer);
var
  Repo: TTaxonRepository;
  Taxon: TTaxon;
begin
  Repo := TTaxonRepository.Create(dmTaxa.sqlCon);
  Taxon := TTaxon.Create();
  try
    Repo.GetById(aTaxonId, Taxon);
    if not Taxon.IsNew then
    begin
      if Taxon.RankId = trFamily then
        Taxon.ParentTaxonId := toOrderId;
      Taxon.OrderId := toOrderId;

      Repo.Update(Taxon);
    end;
  finally
    FreeAndNil(Taxon);
    Repo.Free;
  end;
end;

procedure UpdateScientificName(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery; ExecNow: Boolean);
var
  RankId: Integer;
begin
  RankId := GetRankFromTaxon(aTaxon);
  with aDataset, SQL do
  begin
    Clear;
    Add('UPDATE zoo_taxa SET full_name = :full_name, formatted_name = :formatted_name WHERE (taxon_id = :id);');
    ParamByName('full_name').AsString := aNewName;
    ParamByName('formatted_name').AsString := FormattedBirdName(aNewName, RankId);
    ParamByName('id').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdateVernacularName(aTaxonId, aLanguageId: Integer; aNewName: String; isPreferred: Boolean; aDataset: TSQLQuery;
  ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    Add('INSERT INTO vernacular_names (taxon_id, language_id, vernacular_name, preferred)');
    Add('VALUES (:taxon_id, :language_id, :vernacular_name, :preferred);');
    ParamByName('taxon_id').AsInteger := aTaxonId;
    ParamByName('language_id').AsInteger := aLanguageId;
    ParamByName('verncaular_name').AsString := aNewName;
    ParamByName('preferred').AsBoolean := isPreferred;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdateAuthorship(aTaxon: Integer; aNewName: String; aDataset: TSQLQuery; ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    Add('UPDATE zoo_taxa SET authorship = :authorship WHERE (taxon_id = :id);');
    ParamByName('authorship').AsString := aNewName;
    ParamByName('id').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdateDistribution(aTaxon: Integer; aDist: String; aDataset: TSQLQuery; ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    Add('UPDATE zoo_taxa SET distribution = :distribution WHERE (taxon_id = :id);');
    ParamByName('distribution').AsString := aDist;
    ParamByName('id').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

procedure UpdateExtinction(aTaxon: Integer; IsExtinct: Boolean; aYear: String; aDataset: TSQLQuery;
  ExecNow: Boolean);
begin
  with aDataset, SQL do
  begin
    Clear;
    if IsExtinct = True then
    begin
      Add('UPDATE zoo_taxa SET extinct = 1, extinction_year = :extinction_year WHERE (taxon_id = :id);');
      ParamByName('extinction_year').AsString := aYear;
    end
    else
      Add('UPDATE zoo_taxa SET extinct = 0 WHERE taxon_id = :id;');
    ParamByName('id').AsInteger := aTaxon;

    if ExecNow then
      ExecSQL;
  end;
end;

function HaveMarkedTaxa: Boolean;
var
  Qry: TSQLQuery;
begin
  Result := False;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;
    Add('SELECT COUNT(*) AS marked_count FROM zoo_taxa');
    Add('WHERE (active_status = 1) AND (marked_status = 1)');
    Open;
    Result := FieldByName('marked_count').AsInteger > 0;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function ResolveValidID(TaxonID: Integer): Integer;
var
  Q: TSQLQuery;
  CurrentID, NextID: Integer;
  IterCount: Integer;
begin
  CurrentID := TaxonID;
  IterCount := 0;

  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := dmTaxa.sqlCon;
    while True do
    begin
      Inc(IterCount);
      if IterCount > 50 then
        raise Exception.CreateFmt('Loop detectado ao resolver valid_id para taxon %d', [TaxonID]);

      Q.Close;
      Q.SQL.Text := 'SELECT valid_id FROM zoo_taxa WHERE taxon_id = :id';
      Q.ParamByName('id').AsInteger := CurrentID;
      Q.Open;

      if Q.FieldByName('valid_id').IsNull or (Q.FieldByName('valid_id').AsInteger = 0) then
        Exit(CurrentID)
      else
      begin
        NextID := Q.FieldByName('valid_id').AsInteger;
        if NextID = CurrentID then
          Exit(CurrentID); // proteção contra autorreferência
        CurrentID := NextID;
      end;
    end;
  finally
    Q.Free;
  end;
end;

procedure NormalizeSynonyms;
var
  Q: TSQLQuery;
  U: TSQLQuery;
  SynonymID, OriginalTaxonID, FinalTaxonID, PercentDone: Integer;
begin
  Q := TSQLQuery.Create(nil);
  U := TSQLQuery.Create(nil);
  try
    Q.DataBase := dmTaxa.sqlCon;
    U.DataBase := dmTaxa.sqlCon;
    dlgLoading.Max := 100;
    dlgLoading.Show;
    dlgLoading.UpdateProgress('Normalizing synonyms...', -1);
    Q.SQL.Text := 'SELECT synonym_id, taxon_id FROM zoo_taxa_synonyms';
    Q.Open;
    Q.First;
    PercentDone := 0;
    while not Q.EOF do
    begin
      SynonymID := Q.FieldByName('synonym_id').AsInteger;
      OriginalTaxonID := Q.FieldByName('taxon_id').AsInteger;
      FinalTaxonID := ResolveValidID(OriginalTaxonID);

      if FinalTaxonID <> OriginalTaxonID then
      begin
        U.Close;
        U.SQL.Text := 'UPDATE zoo_taxa_synonyms SET taxon_id = :new_id WHERE synonym_id = :sid';
        U.ParamByName('new_id').AsInteger := FinalTaxonID;
        U.ParamByName('sid').AsInteger := SynonymID;
        U.ExecSQL;
      end;

      PercentDone := Round((Q.RecNo * 100) / Q.RecordCount);
      dlgLoading.UpdateProgress(Format('Normalizing synonyms (%d%%)', [PercentDone]), PercentDone);
      Q.Next;
    end;
  finally
    dlgLoading.Hide;
    Q.Free;
    U.Free;
  end;
end;

procedure UpdateCountryOccurrence(FromTaxonId, ToTaxonId: Integer);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  with Q, SQL do
  try
    DataBase := dmTaxa.sqlCon;

    Clear;
    Add('UPDATE zoo_taxa_countries SET taxon_id = :new_taxon_id');
    Add('WHERE (taxon_id = :old_taxon_id);');
    ParamByName('new_taxon_id').AsInteger := ToTaxonId;
    ParamByName('old_taxon_id').AsInteger := FromTaxonId;

    ExecSQL;

  finally
    FreeAndNil(Q);
  end;
end;

procedure CopySynonyms(FromTaxonId, ToTaxonId: Integer);
var
  Q: TSQLQuery;
  SynonymID, PercentDone: Integer;
  SynonymName: String;
  Synonym: TSynonym;
  SynRepo: TSynonymRepository;
begin
  Q := TSQLQuery.Create(nil);
  SynRepo := TSynonymRepository.Create(dmTaxa.sqlCon);
  Synonym := TSynonym.Create();
  try
    Q.DataBase := dmTaxa.sqlCon;
    dlgLoading.Max := 100;
    dlgLoading.Show;
    dlgLoading.UpdateProgress('Copying synonyms...', -1);
    Q.SQL.Add('SELECT synonym_id, full_name FROM zoo_taxa_synonyms');
    Q.SQL.ADD('WHERE (taxon_id = :old_taxon_id)');
    Q.ParamByName('old_taxon_id').AsInteger := FromTaxonId;
    Q.Open;
    Q.First;
    PercentDone := 0;
    while not Q.EOF do
    begin
      Synonym.Clear;
      SynonymID := Q.FieldByName('synonym_id').AsInteger;
      SynonymName := Q.FieldByName('full_name').AsString;
      //FinalTaxonID := ResolveValidID(OriginalTaxonID);

      SynRepo.FindByTaxon(ToTaxonId, SynonymName, Synonym);
      if Synonym.IsNew then
      begin
        Synonym.TaxonId := ToTaxonId;
        Synonym.Valid := False;

        SynRepo.Insert(Synonym);
      end;

      PercentDone := Round((Q.RecNo * 100) / Q.RecordCount);
      dlgLoading.UpdateProgress(Format('Copying synonyms (%d%%)', [PercentDone]), PercentDone);
      Q.Next;
    end;
  finally
    dlgLoading.Hide;
    SynRepo.Free;
    Synonym.Free;
    Q.Free;
  end;
end;

function GetTaxonHierarchy(aTaxonId: Integer): TTaxonHierarchy;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  with Q, SQL do
  try
    DataBase := dmTaxa.sqlCon;

    Add('SELECT parent_taxon_id, order_id, family_id, subfamily_id, genus_id, species_id, subspecies_group_id');
    Add('FROM zoo_taxa');
    Add('WHERE (taxon_id = :taxon_id)');
    ParamByName('taxon_id').AsInteger := aTaxonId;
    Open;
    if not IsEmpty then
    begin
      Result.TaxonId := aTaxonId;
      Result.ParentTaxon.Id := FieldByName('parent_taxon_id').AsInteger;
      Result.Order.Id := FieldByName('order_id').AsInteger;
      Result.Family.Id := FieldByName('family_id').AsInteger;
      Result.Subfamily.Id := FieldByName('subfamily_id').AsInteger;
      Result.Genus.Id := FieldByName('genus_id').AsInteger;
      Result.Species.Id := FieldByName('species_id').AsInteger;
      Result.SubspeciesGroup.Id := FieldByName('subspecies_group_id').AsInteger;
    end;
    Close;
  finally
    FreeAndNil(Q);
  end;
end;

end.

