unit models_taxon;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, models_base, utils_taxonomy;

type

  { TCustomTaxon }

  TCustomTaxon = class(TXolmisRecord)
    protected
      FFullName: String;
      FFormattedName: String;
      FConceptId: String;
      FAuthorship: String;
      FRankId: TZooRank;
      FParentTaxonId: Integer;
      FOrderId: Integer;
      FFamilyId: Integer;
      FGenusId: Integer;
      FSpeciesId: Integer;
    public
      procedure Clear; override;
      procedure Assign(Source: TPersistent); override;
    published
      property FullName: String read FFullName write FFullName;
      property FormattedName: String read FFormattedName write FFormattedName;
      property ConceptId: String read FConceptId write FConceptId;
      property Authorship: String read FAuthorship write FAuthorship;
      property RankId: TZooRank read FRankId write FRankId;
      property ParentTaxonId: Integer read FParentTaxonId write FParentTaxonId;
      property OrderId: Integer read FOrderId write FOrderId;
      property FamilyId: Integer read FFamilyId write FFamilyId;
      property GenusId: Integer read FGenusId write FGenusId;
      property SpeciesId: Integer read FSpeciesId write FSpeciesId;
    end;

  { TTaxon }

  TTaxon = class(TCustomTaxon)
  protected
    FSortNum: Double;
    FQuickCode: String;
    FIucnStatus: String;
    FExtinct: Boolean;
    FExtinctionYear: String;
    FDistribution: String;
    FEbirdCode: String;
    FSubfamilyId: Integer;
    FSubspeciesGroupId: Integer;
    FIncertaeSedis: Integer;
    FAccepted: Boolean;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TTaxon; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TTaxon): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property SortNum: Double read FSortNum write FSortNum;
    property QuickCode: String read FQuickCode write FQuickCode;
    property IucnStatus: String read FIucnStatus write FIucnStatus;
    property Extinct: Boolean read FExtinct write FExtinct;
    property ExtinctionYear: String read FExtinctionYear write FExtinctionYear;
    property Distribution: String read FDistribution write FDistribution;
    property EbirdCode: String read FEbirdCode write FEbirdCode;
    property SubfamilyId: Integer read FSubfamilyId write FSubfamilyId;
    property SubspeciesGroupId: Integer read FSubspeciesGroupId write FSubspeciesGroupId;
    property IncertaeSedis: Integer read FIncertaeSedis write FIncertaeSedis;
    property Accepted: Boolean read FAccepted write FAccepted;
  end;

  { TTaxonRepository }

  TTaxonRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

  { TSynonym }

  TSynonym = class(TXolmisRecord)
  protected
    FTaxonId: Integer;
    FFullName: String;
    FValid: Boolean;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TSynonym; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TSynonym): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property FullName: String read FFullName write FFullName;
    property Valid: Boolean read FValid write FValid;
  end;

  { TSynonymRepository }

  TSynonymRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByTaxon(const aTaxonId: Integer; const aSynonym: String; E: TSynonym);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

  { TVernacularName }

  TVernacularName = class(TXolmisRecord)
  protected
    FTaxonId: Integer;
    FLanguageId: Integer;
    FVernacularName: String;
    FPreferred: Boolean;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TVernacularName; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TVernacularName): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property LanguageId: Integer read FLanguageId write FLanguageId;
    property VernacularName: String read FVernacularName write FVernacularName;
    property Preferred: Boolean read FPreferred write FPreferred;
  end;

  { TVernacularRepository }

  TVernacularRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure FindByTaxon(const aTaxonId: Integer; const aVernacularName: String; E: TVernacularName);
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

implementation

uses
  data_getvalue, data_setparam, udm_taxa;

{ TCustomTaxon }

procedure TCustomTaxon.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCustomTaxon then
  begin
    FFullName := TCustomTaxon(Source).FullName;
    FFormattedName := TCustomTaxon(Source).FormattedName;
    FConceptId := TCustomTaxon(Source).ConceptId;
    FAuthorship := TCustomTaxon(Source).Authorship;
    FRankId := TCustomTaxon(Source).RankId;
    FParentTaxonId := TCustomTaxon(Source).ParentTaxonId;
    FOrderId := TCustomTaxon(Source).OrderId;
    FFamilyId := TCustomTaxon(Source).FamilyId;
    FGenusId := TCustomTaxon(Source).GenusId;
    FSpeciesId := TCustomTaxon(Source).SpeciesId;
  end;
end;

procedure TCustomTaxon.Clear;
begin
  inherited Clear;
  FFullName := EmptyStr;
  FFormattedName := EmptyStr;
  FConceptId := EmptyStr;
  FAuthorship := EmptyStr;
  FRankId := trNone;
  FParentTaxonId := 0;
  FOrderId := 0;
  FFamilyId := 0;
  FGenusId := 0;
  FSpeciesId := 0;
end;

{ TTaxon }

constructor TTaxon.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TTaxon.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TTaxon then
  begin
    FSortNum := TTaxon(Source).SortNum;
    FQuickCode := TTaxon(Source).QuickCode;
    FExtinct := TTaxon(Source).Extinct;
    FExtinctionYear := TTaxon(Source).ExtinctionYear;
    FDistribution := TTaxon(Source).Distribution;
    FEbirdCode := TTaxon(Source).EbirdCode;
    FSubfamilyId := TTaxon(Source).SubfamilyId;
    FSubspeciesGroupId := TTaxon(Source).SubspeciesGroupId;
    FIncertaeSedis := TTaxon(Source).IncertaeSedis;
  end;
end;

procedure TTaxon.Clear;
begin
  inherited Clear;
  FSortNum := 0.0;
  FQuickCode := EmptyStr;
  FIucnStatus := EmptyStr;
  FExtinct := False;
  FExtinctionYear := EmptyStr;
  FDistribution := EmptyStr;
  FEbirdCode := EmptyStr;
  FSubfamilyId := 0;
  FSubspeciesGroupId := 0;
  FIncertaeSedis := 0;
  FAccepted := False;
end;

function TTaxon.Clone: TXolmisRecord;
begin
  Result := TTaxon(inherited Clone);
end;

function TTaxon.Diff(const aOld: TTaxon; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  //if FieldValuesDiff(rscName, aOld.Name, FName, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff(rscAbbreviation, aOld.Abbreviation, FAbbreviation, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff(rscCategory, aOld.Category, FCategory, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff(rscEBirdName, aOld.EbirdName, FEbirdName, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff(rscDescription, aOld.Description, FDescription, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff(rscRecommendedUses, aOld.RecommendedUses, FRecommendedUses, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff(rscNotes, aOld.Notes, FNotes, R) then
  //  Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TTaxon.EqualsTo(const Other: TTaxon): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TTaxon.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FFullName           := Obj.Get('full_name', '');
    FFormattedName      := Obj.Get('formatted_name', '');
    FConceptId          := Obj.Get('concept_id', '');
    FAuthorship         := Obj.Get('authorship', '');
    FRankId             := TZooRank(Obj.Get('rank_id', 0));
    FParentTaxonId      := Obj.Get('parent_taxon_id', 0);
    FSortNum            := Obj.Get('sort_num', 0.0);
    FQuickCode          := Obj.Get('quick_code', '');
    FIucnStatus         := Obj.Get('iucn_status', '');
    FExtinct            := Obj.Get('extinct', False);
    FExtinctionYear     := Obj.Get('extinction_year', '');
    FDistribution       := Obj.Get('distribution', '');
    FEbirdCode          := Obj.Get('ebird_code', '');
    FOrderId            := Obj.Get('order_id', 0);
    FFamilyId           := Obj.Get('family_id', 0);
    FSubfamilyId        := Obj.Get('subfamily_id', 0);
    FGenusId            := Obj.Get('genus_id', 0);
    FSpeciesId          := Obj.Get('species_id', 0);
    FSubspeciesGroupId  := Obj.Get('subspecies_group_id', 0);
    FIncertaeSedis      := Obj.Get('incertae_sedis', 0);
    FAccepted           := Obj.Get('accepted', False);
  finally
    Obj.Free;
  end;
end;

function TTaxon.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('formatted_name', FFormattedName);
    JSONObject.Add('concept_id', FConceptId);
    JSONObject.Add('authorship', FAuthorship);
    JSONObject.Add('rank_id', Ord(FRankId));
    JSONObject.Add('parent_taxon_id', FParentTaxonId);
    JSONObject.Add('sort_num', FSortNum);
    JSONObject.Add('quick_code', FQuickCode);
    JSONObject.Add('iucn_status', FIucnStatus);
    JSONObject.Add('extinct', FExtinct);
    JSONObject.Add('extinction_year', FExtinctionYear);
    JSONObject.Add('distribution', FDistribution);
    JSONObject.Add('ebird_code', FEbirdCode);
    JSONObject.Add('order_id', FOrderId);
    JSONObject.Add('family_id', FFamilyId);
    JSONObject.Add('subfamily_id', FSubfamilyId);
    JSONObject.Add('genus_id', FGenusId);
    JSONObject.Add('species_id', FSpeciesId);
    JSONObject.Add('subspecies_group_id', FSubspeciesGroupId);
    JSONObject.Add('incertae_sedis', FIncertaeSedis);
    JSONObject.Add('accepted', FAccepted);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TTaxon.ToString: String;
begin
  Result := Format('Taxon(Id=%d, FullName=%s, FormattedName=%s, ConceptId=%s, Authorship=%s, RankId=%d, ParentTaxonId=%d, ' +
    'SortNum=%f, QuickCode=%s, IucnStatus=%s, Extinct=%s, ExtinctionYear=%s, Distribution=%s, EbirdCode=%s, ' +
    'OrderId=%d, FamilyId=%d, SubfamilyId=%d, GenusId=%d, SpeciesId=%d, SubspeciesGroupId=%d, IncertaeSedis=%d, ' +
    'Accepted=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FFullName, FFormattedName, FConceptId, FAuthorship, Ord(FRankId), FParentTaxonId, FSortNum, FQuickCode, FIucnStatus,
    BoolToStr(FExtinct, 'True', 'False'), FExtinctionYear, FDistribution, FEbirdCode, FOrderId, FFamilyId,
    FSubfamilyId, FGenusId, FSpeciesId, FSubspeciesGroupId, FIncertaeSedis,
    BoolToStr(FAccepted, 'True', 'False'),
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TTaxon.Validate(out Msg: string): Boolean;
begin
  if FFullName = EmptyStr then
  begin
    Msg := 'FullName required.';
    Exit(False);
  end;
  if FRankId = trNone then
  begin
    Msg := 'RankId required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TTaxonRepository }

procedure TTaxonRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TTaxon;
begin
  if not (E is TTaxon) then
    raise Exception.Create('Delete: Expected TTaxon');

  R := TTaxon(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TTaxonRepository.Delete: %s.', ['ID is empty']);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := 'taxon_id';
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TTaxonRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := 'taxon_id';
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TTaxonRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..3] of string = ('taxon_id', 'full_name', 'quick_code', 'ebird_code'); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TTaxon) then
    raise Exception.Create('FindBy: Expected TTaxon');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt('Field %s not allowed in FindBy', [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Add('SELECT * FROM %tablename');
    Add('WHERE %afield = :avalue');
    MacroByName('tablename').Value := TableName;
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TTaxon(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TTaxonRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TTaxon) then
    raise Exception.Create('GetById: Expected TTaxon');

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Clear;
    Add('SELECT * FROM %tablename');
    Add('WHERE %idname = :id');
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := 'taxon_id';
    ParamByName('id').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TTaxon(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TTaxonRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TTaxon;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TTaxon) then
    raise Exception.Create('Hydrate: Expected TTaxon');

  R := TTaxon(E);
  with aDataSet do
  begin
    R.Id := FieldByName('taxon_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.FormattedName := FieldByName('formatted_name').AsString;
    R.ConceptId := FieldByName('taxon_concept_id').AsString;
    R.ParentTaxonId := FieldByName('parent_taxon_id').AsInteger;
    R.RankId := GetRankType(FieldByName('rank_id').AsInteger);
    R.Authorship := FieldByName('authorship').AsString;
    R.SortNum := FieldByName('sort_num').AsFloat;
    R.QuickCode := FieldByName('quick_code').AsString;
    R.IucnStatus := FieldByName('iucn_status').AsString;
    R.Extinct := FieldByName('extinct').AsBoolean;
    R.ExtinctionYear := FieldByName('extinction_year').AsString;
    R.Distribution := FieldByName('distribution').AsString;
    R.EbirdCode := FieldByName('ebird_code').AsString;
    R.OrderId := FieldByName('order_id').AsInteger;
    R.FamilyId := FieldByName('family_id').AsInteger;
    R.SubfamilyId := FieldByName('subfamily_id').AsInteger;
    R.GenusId := FieldByName('genus_id').AsInteger;
    R.SpeciesId := FieldByName('species_id').AsInteger;
    R.SubspeciesGroupId := FieldByName('subspecies_group_id').AsInteger;
    R.IncertaeSedis := FieldByName('incertae_sedis').AsInteger;
    R.Accepted := FieldByName('accepted_status').AsBoolean;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TTaxonRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TTaxon;
begin
  if not (E is TTaxon) then
    raise Exception.Create('Insert: Expected TTaxon');

  R := TTaxon(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO zoo_taxa (' +
      'full_name, ' +
      'authorship, ' +
      'formatted_name, ' +
      'taxon_concept_id, ' +
      'quick_code, ' +
      'rank_id, ' +
      'parent_taxon_id, ' +
      'iucn_status, ' +
      'extinct, ' +
      'extinction_year, ' +
      'sort_num, ' +
      'incertae_sedis, ' +
      'ebird_code, ' +
      'distribution, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':full_name, ' +
      ':authorship, ' +
      ':formatted_name, ' +
      ':taxon_concept_id, ' +
      ':quick_code, ' +
      ':rank_id, ' +
      ':parent_taxon_id, ' +
      ':iucn_status, ' +
      ':extinct, ' +
      ':extinction_year, ' +
      ':sort_num, ' +
      ':incertae_sedis, ' +
      ':ebird_code, ' +
      ':distribution, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('full_name').AsString := R.FullName;
    SetStrParam(ParamByName('authorship'), R.Authorship);
    ParamByName('formatted_name').AsString := R.FormattedName;
    SetStrParam(ParamByName('taxon_concept_id'), R.ConceptId);
    SetStrParam(ParamByName('quick_code'), R.QuickCode);
    ParamByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[R.RankId]);
    SetForeignParam(ParamByName('parent_taxon_id'), R.ParentTaxonId);
    SetStrParam(ParamByName('iucn_status'), R.IucnStatus);
    ParamByName('extinct').AsBoolean := R.Extinct;
    SetStrParam(ParamByName('extinction_year'), R.ExtinctionYear);
    SetFloatParam(ParamByName('sort_num'), R.SortNum);
    SetForeignParam(ParamByName('incertae_sedis'), R.IncertaeSedis);
    SetStrParam(ParamByName('ebird_code'), R.EbirdCode);
    SetStrParam(ParamByName('distribution'), R.Distribution);

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;

    // Get the taxon hierarchy
    if (R.ParentTaxonId > 0) then
    begin
      Clear;
      Add('SELECT order_id, family_id, subfamily_id, genus_id, species_id, subspecies_group_id FROM zoo_taxa');
      Add('WHERE taxon_id = :ataxon');
      ParamByName('ataxon').AsInteger := R.ParentTaxonId;
      Open;
      R.OrderId := FieldByName('order_id').AsInteger;
      R.FamilyId := FieldByName('family_id').AsInteger;
      R.SubfamilyId := FieldByName('subfamily_id').AsInteger;
      R.GenusId := FieldByName('genus_id').AsInteger;
      R.SpeciesId := FieldByName('species_id').AsInteger;
      R.SubspeciesGroupId := FieldByName('subspecies_group_id').AsInteger;
      Close;
    end;
    case R.RankId of
      trOrder:          R.OrderId := R.Id;
      trFamily:         R.FamilyId := R.Id;
      trSubfamily:      R.SubfamilyId := R.Id;
      trGenus:          R.GenusId := R.Id;
      trSpecies:        R.SpeciesId := R.Id;
      trMonotypicGroup,
      trPolitypicGroup: R.SubspeciesGroupId := R.Id;
    end;
    // Save the taxon hierarchy
    Clear;
    Add('UPDATE zoo_taxa SET');
    Add('  order_id = :order_id,');
    Add('  family_id = :family_id,');
    Add('  subfamily_id = :subfamily_id,');
    Add('  genus_id = :genus_id,');
    Add('  species_id = :species_id,');
    Add('  subspecies_group_id = :subspecies_group_id');
    Add('WHERE taxon_id = :aid');
    SetForeignParam(ParamByName('order_id'), R.OrderId);
    SetForeignParam(ParamByName('family_id'), R.FamilyId);
    SetForeignParam(ParamByName('subfamily_id'), R.SubfamilyId);
    SetForeignParam(ParamByName('genus_id'), R.GenusId);
    SetForeignParam(ParamByName('species_id'), R.SpeciesId);
    SetForeignParam(ParamByName('subspecies_group_id'), R.SubspeciesGroupId);
    ParamByName('aid').AsInteger := R.Id;
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

function TTaxonRepository.TableName: string;
begin
  Result := 'zoo_taxa';
end;

procedure TTaxonRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TTaxon;
begin
  if not (E is TTaxon) then
    raise Exception.Create('Update: Expected TTaxon');

  R := TTaxon(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TTaxonRepository.Update: %s.', ['ID is empty']);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE zoo_taxa SET ' +
      'full_name = :full_name, ' +
      'authorship = :authorship, ' +
      'formatted_name = :formatted_name, ' +
      'taxon_concept_id = :taxon_concept_id, ' +
      'quick_code = :quick_code, ' +
      'rank_id = :rank_id, ' +
      'parent_taxon_id = :parent_taxon_id, ' +
      'iucn_status = :iucn_status, ' +
      'extinct = :extinct, ' +
      'extinction_year = :extinction_year, ' +
      'sort_num = :sort_num, ' +
      'incertae_sedis = :incertae_sedis, ' +
      'ebird_code = :ebird_code, ' +
      'distribution = :distribution, ' +
      'accepted_status = :accepted_status, ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status, ' +
      'update_date = datetime(''now'',''subsec'') ');
    Add('WHERE (taxon_id = :taxon_id)');

    ParamByName('full_name').AsString := R.FullName;
    SetStrParam(ParamByName('authorship'), R.Authorship);
    ParamByName('formatted_name').AsString := R.FormattedName;
    SetStrParam(ParamByName('taxon_concept_id'), R.ConceptId);
    SetStrParam(ParamByName('quick_code'), R.QuickCode);
    ParamByName('rank_id').AsInteger := GetKey('taxon_ranks', 'rank_id', 'rank_acronym', ZOOLOGICAL_RANKS[R.RankId]);
    SetForeignParam(ParamByName('parent_taxon_id'), R.ParentTaxonId);
    SetStrParam(ParamByName('iucn_status'), R.IucnStatus);
    ParamByName('extinct').AsBoolean := R.Extinct;
    SetStrParam(ParamByName('extinction_year'), R.ExtinctionYear);
    SetFloatParam(ParamByName('sort_num'), R.SortNum);
    SetForeignParam(ParamByName('incertae_sedis'), R.IncertaeSedis);
    SetStrParam(ParamByName('ebird_code'), R.EbirdCode);
    SetStrParam(ParamByName('distribution'), R.Distribution);
    ParamByName('accepted_status').AsBoolean := R.Accepted;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('taxon_id').AsInteger := R.Id;

    ExecSQL;

    // Get the taxon hierarchy
    if (R.ParentTaxonId > 0) then
    begin
      Clear;
      Add('SELECT order_id, family_id, subfamily_id, genus_id, species_id, subspecies_group_id FROM zoo_taxa');
      Add('WHERE taxon_id = :ataxon');
      ParamByName('ataxon').AsInteger := R.ParentTaxonId;
      Open;
      R.OrderId := FieldByName('order_id').AsInteger;
      R.FamilyId := FieldByName('family_id').AsInteger;
      R.SubfamilyId := FieldByName('subfamily_id').AsInteger;
      R.GenusId := FieldByName('genus_id').AsInteger;
      R.SpeciesId := FieldByName('species_id').AsInteger;
      R.SubspeciesGroupId := FieldByName('subspecies_group_id').AsInteger;
      Close;
    end;
    case R.RankId of
      trOrder:          R.OrderId := R.Id;
      trFamily:         R.FamilyId := R.Id;
      trSubfamily:      R.SubfamilyId := R.Id;
      trGenus:          R.GenusId := R.Id;
      trSpecies:        R.SpeciesId := R.Id;
      trMonotypicGroup,
      trPolitypicGroup: R.SubspeciesGroupId := R.Id;
    end;
    // Save the taxon hierarchy
    Clear;
    Add('UPDATE zoo_taxa SET');
    Add('  order_id = :order_id,');
    Add('  family_id = :family_id,');
    Add('  subfamily_id = :subfamily_id,');
    Add('  genus_id = :genus_id,');
    Add('  species_id = :species_id,');
    Add('  subspecies_group_id = :subspecies_group_id');
    Add('WHERE taxon_id = :aid');
    SetForeignParam(ParamByName('order_id'), R.OrderId);
    SetForeignParam(ParamByName('family_id'), R.FamilyId);
    SetForeignParam(ParamByName('subfamily_id'), R.SubfamilyId);
    SetForeignParam(ParamByName('genus_id'), R.GenusId);
    SetForeignParam(ParamByName('species_id'), R.SpeciesId);
    SetForeignParam(ParamByName('subspecies_group_id'), R.SubspeciesGroupId);
    ParamByName('aid').AsInteger := R.Id;
    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TSynonym }

constructor TSynonym.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TSynonym.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TSynonym then
  begin
    FTaxonId := TSynonym(Source).TaxonId;
    FFullName := TSynonym(Source).FullName;
    FValid := TSynonym(Source).Valid;
  end;
end;

procedure TSynonym.Clear;
begin
  inherited Clear;
  FTaxonId := 0;
  FFullName := EmptyStr;
  FValid := False;
end;

function TSynonym.Clone: TXolmisRecord;
begin
  Result := TSynonym(inherited Clone);
end;

function TSynonym.Diff(const aOld: TSynonym; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  //if FieldValuesDiff(rscName, aOld.Name, FName, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff(rscAbbreviation, aOld.Abbreviation, FAbbreviation, R) then
  //  Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TSynonym.EqualsTo(const Other: TSynonym): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TSynonym.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FTaxonId  := Obj.Get('taxon_id', 0);
    FFullName := Obj.Get('full_name', '');
    FValid := Obj.Get('valid', False);
  finally
    Obj.Free;
  end;
end;

function TSynonym.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('full_name', FFullName);
    JSONObject.Add('valid', FValid);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TSynonym.ToString: String;
begin
  Result := Format('Synonym(Id=%d, TaxonId=%d, FullName=%s, Valid=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FTaxonId, FFullName, BoolToStr(FValid, 'True', 'False'),
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TSynonym.Validate(out Msg: string): Boolean;
begin
  if FTaxonId = 0 then
  begin
    Msg := 'TaxonId required.';
    Exit(False);
  end;
  if FFullName = EmptyStr then
  begin
    Msg := 'FullName required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TSynonymRepository }

procedure TSynonymRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSynonym;
begin
  if not (E is TSynonym) then
    raise Exception.Create('Delete: Expected TSynonym');

  R := TSynonym(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSynonymRepository.Delete: %s.', ['ID is empty']);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := 'synonym_id';
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TSynonymRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := 'synonym_id';
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSynonymRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = ('synonym_id', 'full_name', 'taxon_id'); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TSynonym) then
    raise Exception.Create('FindBy: Expected TSynonym');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt('Field %s not allowed in FindBy', [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Add('SELECT * FROM %tablename');
    Add('WHERE %afield = :avalue');
    MacroByName('tablename').Value := TableName;
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TSynonym(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TSynonymRepository.FindByTaxon(const aTaxonId: Integer; const aSynonym: String; E: TSynonym);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT * FROM %tablename WHERE (taxon_id=:taxon_id) AND (full_name=:full_name)';
    MacroByName('tablename').Value := TableName;
    ParamByName('taxon_id').AsInteger := aTaxonId;
    ParamByName('full_name').AsString := aSynonym;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, E);
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSynonymRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TSynonym) then
    raise Exception.Create('GetById: Expected TSynonym');

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Clear;
    Add('SELECT * FROM %tablename');
    Add('WHERE %idname = :id');
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := 'synonym_id';
    ParamByName('id').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TSynonym(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TSynonymRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TSynonym;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TSynonym) then
    raise Exception.Create('Hydrate: Expected TSynonym');

  R := TSynonym(E);
  with aDataSet do
  begin
    R.Id := FieldByName('synonym_id').AsInteger;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.FullName := FieldByName('full_name').AsString;
    R.Valid := FieldByName('valid_status').AsBoolean;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TSynonymRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSynonym;
begin
  if not (E is TSynonym) then
    raise Exception.Create('Insert: Expected TSynonym');

  R := TSynonym(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO zoo_taxa_synonyms (' +
      'taxon_id, ' +
      'full_name, ' +
      'valid_status, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':taxon_id, ' +
      ':full_name, ' +
      ':valid_status, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('taxon_id').AsInteger := R.TaxonId;
    ParamByName('full_name').AsString := R.FullName;
    ParamByName('valid_status').AsBoolean := R.Valid;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TSynonymRepository.TableName: string;
begin
  Result := 'zoo_taxa_synonyms';
end;

procedure TSynonymRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TSynonym;
begin
  if not (E is TSynonym) then
    raise Exception.Create('Update: Expected TSynonym');

  R := TSynonym(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TSynonymRepository.Update: %s.', ['ID is empty']);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE zoo_taxa_synonyms SET ' +
      'taxon_id = :taxon_id, ' +
      'full_name = :full_name, ' +
      'valid_status = :valid_status, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status');
    Add('WHERE (synonym_id = :synonym_id)');

    ParamByName('taxon_id').AsInteger := R.TaxonId;
    ParamByName('full_name').AsString := R.FullName;
    ParamByName('valid_status').AsBoolean := R.Valid;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('synonym_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TVernacularName }

constructor TVernacularName.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TVernacularName.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TVernacularName then
  begin
    FTaxonId := TVernacularName(Source).TaxonId;
    FLanguageId := TVernacularName(Source).LanguageId;
    FVernacularName := TVernacularName(Source).VernacularName;
    FPreferred := TVernacularName(Source).Preferred;
  end;
end;

procedure TVernacularName.Clear;
begin
  inherited Clear;
  FTaxonId := 0;
  FLanguageId := 0;
  FVernacularName := EmptyStr;
  FPreferred := False;
end;

function TVernacularName.Clone: TXolmisRecord;
begin
  Result := TVernacularName(inherited Clone);
end;

function TVernacularName.Diff(const aOld: TVernacularName; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  //if FieldValuesDiff(rscName, aOld.Name, FName, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff(rscAbbreviation, aOld.Abbreviation, FAbbreviation, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff(rscCategory, aOld.Category, FCategory, R) then
  //  Changes.Add(R);
  //if FieldValuesDiff(rscEBirdName, aOld.EbirdName, FEbirdName, R) then
  //  Changes.Add(R);

  Result := Changes.Count > 0;
end;

function TVernacularName.EqualsTo(const Other: TVernacularName): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TVernacularName.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FTaxonId        := Obj.Get('taxon_id', 0);
    FLanguageId     := Obj.Get('language_id', 0);
    FVernacularName := Obj.Get('vernacular_name', '');
    FPreferred      := Obj.Get('preferred', False);
  finally
    Obj.Free;
  end;
end;

function TVernacularName.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('taxon_id', FTaxonId);
    JSONObject.Add('language_id', FLanguageId);
    JSONObject.Add('vernacular_name', FVernacularName);
    JSONObject.Add('preferred', FPreferred);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TVernacularName.ToString: String;
begin
  Result := Format('VernacularName(Id=%d, TaxonId=%d, LanguageId=%d, VernacularName=%s, Preferred=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FTaxonId, FLanguageId, FVernacularName, BoolToStr(FPreferred, 'True', 'False'),
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TVernacularName.Validate(out Msg: string): Boolean;
begin
  if FTaxonId = 0 then
  begin
    Msg := 'TaxonId required.';
    Exit(False);
  end;
  if FLanguageId = 0 then
  begin
    Msg := 'LanguageId required.';
    Exit(False);
  end;
  if FVernacularName = EmptyStr then
  begin
    Msg := 'VernacularName required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TVernacularRepository }

procedure TVernacularRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TVernacularName;
begin
  if not (E is TVernacularName) then
    raise Exception.Create('Delete: Expected TVernacularName');

  R := TVernacularName(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TVernacularRepository.Delete: %s.', ['ID is empty']);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (%idname = :aid)');

      MacroByName('tablename').Value := TableName;
      MacroByName('idname').Value := 'vernacular_id';
      ParamByName('aid').AsInteger := R.Id;

      ExecSQL;

      FTrans.CommitRetaining;
    except
      FTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

function TVernacularRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := 'vernacular_id';
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TVernacularRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..1] of string = ('vernacular_id', 'vernacular_name'); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TVernacularName) then
    raise Exception.Create('FindBy: Expected TVernacularName');

  // Avoid FieldName injection: check in whitelist
  Ok := False;
  for I := Low(ALLOWED) to High(ALLOWED) do
    if SameText(FieldName, ALLOWED[I]) then
    begin
      Ok := True;
      Break;
    end;
  if not Ok then
    raise Exception.CreateFmt('Field %s not allowed in FindBy', [FieldName]);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Add('SELECT * FROM %tablename');
    Add('WHERE %afield = :avalue');
    MacroByName('tablename').Value := TableName;
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, TVernacularName(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TVernacularRepository.FindByTaxon(const aTaxonId: Integer; const aVernacularName: String;
  E: TVernacularName);
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT * FROM %tablename WHERE (taxon_id=:taxon_id) AND (vernacular_name=:vernacular_name)';
    MacroByName('tablename').Value := TableName;
    ParamByName('taxon_id').AsInteger := aTaxonId;
    ParamByName('vernacular_name').AsString := aVernacularName;
    Open;

    if not EOF then
    begin
      Hydrate(Qry, E);
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TVernacularRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TVernacularName) then
    raise Exception.Create('GetById: Expected TVernacularName');

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Clear;
    Add('SELECT * FROM %tablename');
    Add('WHERE %idname = :id');
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := 'vernacular_id';
    ParamByName('id').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TVernacularName(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TVernacularRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TVernacularName;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TVernacularName) then
    raise Exception.Create('Hydrate: Expected TVernacularName');

  R := TVernacularName(E);
  with aDataSet do
  begin
    R.Id := FieldByName('vernacular_id').AsInteger;
    R.TaxonId := FieldByName('taxon_id').AsInteger;
    R.LanguageId := FieldByName('language_id').AsInteger;
    R.VernacularName := FieldByName('vernacular_name').AsString;
    R.Preferred := FieldByName('preferred').AsBoolean;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TVernacularRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TVernacularName;
begin
  if not (E is TVernacularName) then
    raise Exception.Create('Insert: Expected TVernacularName');

  R := TVernacularName(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO vernacular_names (' +
      'taxon_id, ' +
      'language_id, ' +
      'vernacular_name, ' +
      'preferred, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':taxon_id, ' +
      ':language_id, ' +
      ':vernacular_name, ' +
      ':preferred, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('taxon_id').AsInteger := R.TaxonId;
    ParamByName('language_id').AsInteger := R.LanguageId;
    ParamByName('vernacular_name').AsString := R.VernacularName;
    ParamByName('preferred').AsBoolean := R.Preferred;

    ExecSQL;

    // Get the record ID
    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    R.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

function TVernacularRepository.TableName: string;
begin
  Result := 'vernacular_names';
end;

procedure TVernacularRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TVernacularName;
begin
  if not (E is TVernacularName) then
    raise Exception.Create('Update: Expected TVernacularName');

  R := TVernacularName(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TVernacularRepository.Update: %s.', ['ID is empty']);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE vernacular_names SET ' +
      'taxon_id = :taxon_id, ' +
      'language_id = :language_id, ' +
      'vernacular_name = :vernacular_name, ' +
      'preferred = :preferred, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status');
    Add('WHERE (vernacular_id = :vernacular_id)');

    ParamByName('taxon_id').AsInteger := R.TaxonId;
    ParamByName('language_id').AsInteger := R.LanguageId;
    ParamByName('vernacular_name').AsString := R.VernacularName;
    ParamByName('preferred').AsBoolean := R.Preferred;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('vernacular_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

