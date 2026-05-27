unit models_rank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, models_base;

type

  { TRank }

  TRank = class(TXolmisRecord)
  protected
    FName: String;
    FAbbreviation: String;
    FRankIndex: Integer;
    FMainRank: Boolean;
    FSubrank: Boolean;
    FInfrarank: Boolean;
    FInfraspecific: Boolean;
    FZoologicalCode: Boolean;
    FBotanicalCode: Boolean;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TRank; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TRank): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property Name: String read FName write FName;
    property Abbreviation: String read FAbbreviation write FAbbreviation;
    property RankIndex: Integer read FRankIndex write FRankIndex;
    property MainRank: Boolean read FMainRank write FMainRank;
    property Subrank: Boolean read FSubrank write FSubrank;
    property Infrarank: Boolean read FInfrarank write FInfrarank;
    property Infraspecific: Boolean read FInfraspecific write FInfraspecific;
    property ZoologicalCode: Boolean read FZoologicalCode write FZoologicalCode;
    property BotanicalCode: Boolean read FBotanicalCode write FBotanicalCode;
  end;

  { TRankRepository }

  TRankRepository = class(TXolmisRepository)
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

  { TRankI18n }

  TRankI18n = class(TXolmisRecord)
  protected
    FRankId: Integer;
    FLanguageId: Integer;
    FRankName: String;
  public
    constructor Create(aRankId: Integer = 0; aLanguageId: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TRankI18n; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TRankI18n): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property RankId: Integer read FRankId write FRankId;
    property LanguageId: Integer read FLanguageId write FLanguageId;
    property RankName: String read FRankName write FRankName;
  end;

  { TRankI18nRepository }

  TRankI18nRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure GetByRankAndLanguage(const aRankId, aLanguageId: Integer; E: TRankI18n);
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

implementation

uses
  data_getvalue, data_setparam, udm_taxa;

{ TRank }

constructor TRank.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TRank.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TRank then
  begin
    FName := TRank(Source).Name;
    FAbbreviation := TRank(Source).Abbreviation;
    FRankIndex := TRank(Source).RankIndex;
    FMainRank := TRank(Source).MainRank;
    FSubrank := TRank(Source).Subrank;
    FInfrarank := TRank(Source).Infrarank;
    FInfraspecific := TRank(Source).Infraspecific;
    FZoologicalCode := TRank(Source).ZoologicalCode;
    FBotanicalCode := TRank(Source).BotanicalCode;
  end;
end;

procedure TRank.Clear;
begin
  inherited Clear;
  FName := EmptyStr;
  FAbbreviation := EmptyStr;
  FRankIndex := 0;
  FMainRank := False;
  FSubrank := False;
  FInfrarank := False;
  FInfraspecific := False;
  FZoologicalCode := False;
  FBotanicalCode := False;
end;

function TRank.Clone: TXolmisRecord;
begin
  Result := TRank(inherited Clone);
end;

function TRank.Diff(const aOld: TRank; var Changes: TStrings): Boolean;
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

function TRank.EqualsTo(const Other: TRank): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TRank.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(AJSONString));
  try
    FId             := Obj.Get('rank_id', 0);
    FName           := Obj.Get('rank_name', '');
    FAbbreviation   := Obj.Get('abbreviation', '');
    FRankIndex      := Obj.Get('rank_seq', 0);
    FMainRank       := Obj.Get('main_rank', False);
    FSubrank        := Obj.Get('subrank', False);
    FInfrarank      := Obj.Get('infrarank', False);
    FInfraspecific  := Obj.Get('infraspecific', False);
    FZoologicalCode := Obj.Get('iczn', True);
    FBotanicalCode  := Obj.Get('icbn', False);
  finally
    Obj.Free;
  end;
end;

function TRank.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('rank_id', FId);
    JSONObject.Add('rank_name', FName);
    JSONObject.Add('abbreviation', FAbbreviation);
    JSONObject.Add('rank_seq', FRankIndex);
    JSONObject.Add('main_rank', FMainRank);
    JSONObject.Add('subrank', FSubrank);
    JSONObject.Add('infrarank', FInfrarank);
    JSONObject.Add('infraspecific', FInfraspecific);
    JSONObject.Add('iczn', FZoologicalCode);
    JSONObject.Add('icbn', FBotanicalCode);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TRank.ToString: String;
begin
  Result := Format('Rank(Id=%d, Name=%s, Abbreviation=%s, RankIndex=%d, MainRank=%s, Subrank=%s, ' +
    'Infrarank=%s, Infraspecific=%s, ZoologicalCode=%s, BotanicalCode=%s, ' +
    'InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FName, FAbbreviation, FRankIndex, BoolToStr(FMainRank, 'True', 'False'),
    BoolToStr(FSubrank, 'True', 'False'), BoolToStr(FInfrarank, 'True', 'False'),
    BoolToStr(FInfraspecific, 'True', 'False'), BoolToStr(FZoologicalCode, 'True', 'False'),
    BoolToStr(FBotanicalCode, 'True', 'False'),
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TRank.Validate(out Msg: string): Boolean;
begin
  if FName = EmptyStr then
  begin
    Msg := 'Name required.';
    Exit(False);
  end;
  if FAbbreviation = EmptyStr then
  begin
    Msg := 'Abbreviation required.';
    Exit(False);
  end;
  if FRankIndex = 0 then
  begin
    Msg := 'RankIndex required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TRankRepository }

procedure TRankRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TRank;
begin
  if not (E is TRank) then
    raise Exception.Create('Delete: Expected TRank');

  R := TRank(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TRankRepository.Delete: %s.', ['ID is empty']);

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
      MacroByName('idname').Value := 'rank_id';
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

function TRankRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := 'rank_id';
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TRankRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..3] of string = ('rank_id', 'rank_name', 'rank_acronym', 'rank_seq'); // whitelist
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TRank) then
    raise Exception.Create('FindBy: Expected TRank');

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
      Hydrate(Qry, TRank(E));
    end;

    Close;
  finally
    Qry.Free;
  end;
end;

procedure TRankRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TRank) then
    raise Exception.Create('GetById: Expected TRank');

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Clear;
    Add('SELECT * FROM %tablename');
    Add('WHERE %idname = :id');
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := 'rank_id';
    ParamByName('id').AsInteger := Id;
    Open;
    if not EOF then
    begin
      Hydrate(Qry, TRank(E));
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TRankRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TRank;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TRank) then
    raise Exception.Create('Hydrate: Expected TRank');

  R := TRank(E);
  with aDataSet do
  begin
    R.Id := FieldByName('rank_id').AsInteger;
    R.Name := FieldByName('rank_name').AsString;
    R.Abbreviation := FieldByName('rank_acronym').AsString;
    R.RankIndex := FieldByName('rank_seq').AsInteger;
    R.MainRank := FieldByName('main_rank').AsBoolean;
    R.Subrank := FieldByName('subrank').AsBoolean;
    R.Infrarank := FieldByName('infrarank').AsBoolean;
    R.Infraspecific := FieldByName('infraspecific').AsBoolean;
    R.ZoologicalCode := FieldByName('zoological_code').AsBoolean;
    R.BotanicalCode := FieldByName('botanical_code').AsBoolean;
    // SQLite may store date and time data as ISO8601 string or Julian date real formats
    // so it checks in which format it is stored before load the value
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
    R.Marked := FieldByName('marked_status').AsBoolean;
    R.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TRankRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TRank;
begin
  if not (E is TRank) then
    raise Exception.Create('Insert: Expected TRank');

  R := TRank(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO taxon_ranks (' +
      'rank_name, ' +
      'rank_acronym, ' +
      'rank_seq, ' +
      'main_rank, ' +
      'subrank, ' +
      'infrarank, ' +
      'infraspecific, ' +
      'zoological_code, ' +
      'botanical_code, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':rank_name, ' +
      ':rank_acronym, ' +
      ':rank_seq, ' +
      ':main_rank, ' +
      ':subrank, ' +
      ':infrarank, ' +
      ':infraspecific, ' +
      ':zoological_code, ' +
      ':botanical_code, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('rank_name').AsString := R.Name;
    ParamByName('rank_acronym').AsString := R.Abbreviation;
    SetIntParam(ParamByName('rank_seq'), R.RankIndex);
    ParamByName('main_rank').AsBoolean := R.MainRank;
    ParamByName('subrank').AsBoolean := R.Subrank;
    ParamByName('infrarank').AsBoolean := R.Infrarank;
    ParamByName('infraspecific').AsBoolean := R.Infraspecific;
    ParamByName('zoological_code').AsBoolean := R.ZoologicalCode;
    ParamByName('botanical_code').AsBoolean := R.BotanicalCode;

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

function TRankRepository.TableName: string;
begin
  Result := 'taxon_ranks';
end;

procedure TRankRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TRank;
begin
  if not (E is TRank) then
    raise Exception.Create('Update: Expected TRank');

  R := TRank(E);
  if R.Id = 0 then
    raise Exception.CreateFmt('TRankRepository.Update: %s.', ['ID is empty']);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE taxon_ranks SET ' +
      'rank_name = :rank_name, ' +
      'rank_acronym = :rank_acronym, ' +
      'rank_seq = :rank_seq, ' +
      'main_rank = :main_rank, ' +
      'subrank = :subrank, ' +
      'infrarank = :infrarank, ' +
      'infraspecific = :infraspecific, ' +
      'zoological_code = :zoological_code, ' +
      'botanical_code = :botanical_code, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status');
    Add('WHERE (rank_id = :rank_id)');

    ParamByName('rank_name').AsString := R.Name;
    ParamByName('rank_acronym').AsString := R.Abbreviation;
    SetIntParam(ParamByName('rank_seq'), R.RankIndex);
    ParamByName('main_rank').AsBoolean := R.MainRank;
    ParamByName('subrank').AsBoolean := R.Subrank;
    ParamByName('infrarank').AsBoolean := R.Infrarank;
    ParamByName('infraspecific').AsBoolean := R.Infraspecific;
    ParamByName('zoological_code').AsBoolean := R.ZoologicalCode;
    ParamByName('botanical_code').AsBoolean := R.BotanicalCode;
    ParamByName('marked_status').AsBoolean := R.Marked;
    ParamByName('active_status').AsBoolean := R.Active;
    ParamByName('rank_id').AsInteger := R.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

{ TRankI18n }

constructor TRankI18n.Create(aRankId: Integer; aLanguageId: Integer);
begin
  inherited Create;
  FRankId := aRankId;
  FLanguageId := aLanguageId;
end;

procedure TRankI18n.Clear;
begin
  inherited Clear;
  FRankId := 0;
  FLanguageId := 0;
  FRankName := EmptyStr;
end;

procedure TRankI18n.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TRankI18n then
  begin
    FRankId := TRankI18n(Source).RankId;
    FLanguageId := TRankI18n(Source).LanguageId;
    FRankName := TRankI18n(Source).RankName;
  end;
end;

function TRankI18n.Clone: TXolmisRecord;
begin
  Result := TRankI18n(inherited Clone);
end;

function TRankI18n.Diff(const aOld: TRankI18n; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if not SameText(aOld.RankName, FRankName) then
  begin
    R := Format('RankName: "%s" -> "%s"', [aOld.RankName, FRankName]);
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  Result := Assigned(Changes) and (Changes.Count > 0);
end;

function TRankI18n.EqualsTo(const Other: TRankI18n): Boolean;
begin
  Result := Assigned(Other) and
    (FRankId = Other.RankId) and
    (FLanguageId = Other.LanguageId);
end;

procedure TRankI18n.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(aJSONString));
  try
    FRankId := Obj.Get('rank_id', 0);
    FLanguageId := Obj.Get('language_id', 0);
    FRankName := Obj.Get('rank_name', '');
  finally
    Obj.Free;
  end;
end;

function TRankI18n.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('rank_id', FRankId);
    JSONObject.Add('language_id', FLanguageId);
    JSONObject.Add('rank_name', FRankName);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TRankI18n.ToString: String;
begin
  Result := Format('RankI18n(RankId=%d, LanguageId=%d, RankName=%s, InsertDate=%s, UpdateDate=%s)',
    [FRankId, FLanguageId, FRankName, DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate)]);
end;

function TRankI18n.Validate(out Msg: string): Boolean;
begin
  if FRankId = 0 then
  begin
    Msg := 'RankId required.';
    Exit(False);
  end;
  if FLanguageId = 0 then
  begin
    Msg := 'LanguageId required.';
    Exit(False);
  end;
  if FRankName = EmptyStr then
  begin
    Msg := 'RankName required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TRankI18nRepository }

function TRankI18nRepository.TableName: string;
begin
  Result := 'taxon_rank_i18n';
end;

function TRankI18nRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE rank_id=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TRankI18nRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..2] of string = ('rank_id', 'language_id', 'rank_name');
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TRankI18n) then
    raise Exception.Create('FindBy: Expected TRankI18n');

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
    Add('ORDER BY rank_id, language_id');

    MacroByName('tablename').Value := TableName;
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
      Hydrate(Qry, TRankI18n(E));

    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TRankI18nRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TRankI18n) then
    raise Exception.Create('GetById: Expected TRankI18n');

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Clear;
    Add('SELECT * FROM %tablename');
    Add('WHERE rank_id = :id');
    Add('ORDER BY language_id LIMIT 1');
    MacroByName('tablename').Value := TableName;
    ParamByName('id').AsInteger := Id;
    Open;
    if not EOF then
      Hydrate(Qry, TRankI18n(E));
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TRankI18nRepository.GetByRankAndLanguage(const aRankId, aLanguageId: Integer; E: TRankI18n);
var
  Qry: TSQLQuery;
begin
  if E = nil then
    Exit;

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Clear;
    Add('SELECT * FROM %tablename');
    Add('WHERE rank_id = :rank_id');
    Add('AND language_id = :language_id');
    MacroByName('tablename').Value := TableName;
    ParamByName('rank_id').AsInteger := aRankId;
    ParamByName('language_id').AsInteger := aLanguageId;
    Open;
    if not EOF then
      Hydrate(Qry, E);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TRankI18nRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  R: TRankI18n;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TRankI18n) then
    raise Exception.Create('Hydrate: Expected TRankI18n');

  R := TRankI18n(E);
  with aDataSet do
  begin
    R.RankId := FieldByName('rank_id').AsInteger;
    R.LanguageId := FieldByName('language_id').AsInteger;
    R.RankName := FieldByName('rank_name').AsString;
    GetTimeStamp(FieldByName('insert_date'), R.InsertDate);
    GetTimeStamp(FieldByName('update_date'), R.UpdateDate);
  end;
end;

procedure TRankI18nRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TRankI18n;
begin
  if not (E is TRankI18n) then
    raise Exception.Create('Insert: Expected TRankI18n');

  R := TRankI18n(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO taxon_rank_i18n (' +
      'rank_id, ' +
      'language_id, ' +
      'rank_name, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':rank_id, ' +
      ':language_id, ' +
      ':rank_name, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('rank_id').AsInteger := R.RankId;
    ParamByName('language_id').AsInteger := R.LanguageId;
    ParamByName('rank_name').AsString := R.RankName;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TRankI18nRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TRankI18n;
begin
  if not (E is TRankI18n) then
    raise Exception.Create('Update: Expected TRankI18n');

  R := TRankI18n(E);
  if (R.RankId = 0) or (R.LanguageId = 0) then
    raise Exception.CreateFmt('TRankI18nRepository.Update: %s.', ['RankId or LanguageId is empty']);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE taxon_rank_i18n SET ' +
      'rank_name = :rank_name, ' +
      'update_date = datetime(''now'', ''subsec'')');
    Add('WHERE (rank_id = :rank_id) AND (language_id = :language_id)');

    ParamByName('rank_name').AsString := R.RankName;
    ParamByName('rank_id').AsInteger := R.RankId;
    ParamByName('language_id').AsInteger := R.LanguageId;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TRankI18nRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  R: TRankI18n;
begin
  if not (E is TRankI18n) then
    raise Exception.Create('Delete: Expected TRankI18n');

  R := TRankI18n(E);
  if (R.RankId = 0) or (R.LanguageId = 0) then
    raise Exception.CreateFmt('TRankI18nRepository.Delete: %s.', ['RankId or LanguageId is empty']);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (rank_id = :rank_id) AND (language_id = :language_id)');

      MacroByName('tablename').Value := TableName;
      ParamByName('rank_id').AsInteger := R.RankId;
      ParamByName('language_id').AsInteger := R.LanguageId;

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

end.

