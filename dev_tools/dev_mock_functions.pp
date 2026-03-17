unit dev_mock_functions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Math, fgl, dev_mock_types;

type
  TStringIntMap = specialize TFPGMap<string, Integer>;

  TExternalRepo = class
  public
    Name: string;
    Data: TJSONArray; // or TJSONData
  end;

  TExternalRepoMap = specialize TFPGMap<string, TExternalRepo>;

  TTableDataCache = class
  public
    TableName: string;
    Rows: array of TStringList; // each line is Name=Value
  end;

  TTableCacheMap = specialize TFPGMap<string, TTableDataCache>;

  { TValueGenerator }

  TValueGenerator = class
  private
    FConfig: TGeneratorConfig;
    FRepos: TExternalRepoMap;
    FTableCache: TTableCacheMap;
    FAutoInc: TStringIntMap;

    function NextAutoInc(const ColName: string; StartAt, IncBy: Integer): Integer;

    function ApplyEnum(Col: TColumn): string;
    function ApplyRange(Col: TColumn): string;
    function ApplyMask(const Mask: string): string;

    function ApplyFunction(Col: TColumn; Row: TStringList; Schema: TTableSchema): string;

    function GetFromExternalSource(Col: TColumn; Schema: TTableSchema): string;
    function GetFromSourceTable(Col: TColumn): string;

  public
    constructor Create(AConfig: TGeneratorConfig);
    destructor Destroy; override;

    procedure RegisterTableData(const TableName: string; const Rows: array of TStringList);

    function Generate(Col: TColumn; Row: TStringList; Schema: TTableSchema): string;
  end;


implementation

{ TValueGenerator }

constructor TValueGenerator.Create(AConfig: TGeneratorConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FRepos := TExternalRepoMap.Create;
  FTableCache := TTableCacheMap.Create;
  FAutoInc := TStringIntMap.Create;

  Randomize;
end;

function TValueGenerator.ApplyEnum(Col: TColumn): string;
var
  i: Integer;
  R, Sum, Acc: Double;
begin
  if Col.EnumValues.Count = 0 then Exit('');

  if Col.Weights.Count = 0 then
    Exit(Col.EnumValues[Random(Col.EnumValues.Count)]);

  Sum := 0;
  for i := 0 to Col.Weights.Count - 1 do
    Sum += StrToFloat(Col.Weights[i]);

  R := Random * Sum;
  Acc := 0;

  for i := 0 to Col.Weights.Count - 1 do
  begin
    Acc += StrToFloat(Col.Weights[i]);
    if R <= Acc then
      Exit(Col.EnumValues[i]);
  end;

  Result := Col.EnumValues[Col.EnumValues.Count - 1];
end;

function TValueGenerator.ApplyFunction(Col: TColumn; Row: TStringList; Schema: TTableSchema): string;
var
  A, B: Integer;
  BaseDate: TDateTime;
  S: string;
begin
  case Col.Func of

    'random_int':
      begin
        A := StrToInt(Col.Args.Raw.Values['min']);
        B := StrToInt(Col.Args.Raw.Values['max']);
        Exit(IntToStr(A + Random(B - A + 1)));
      end;

    'random_float':
      begin
        A := StrToInt(Col.Args.Raw.Values['min']);
        B := StrToInt(Col.Args.Raw.Values['max']);
        Exit(FloatToStr(A + Random * (B - A)));
      end;

    'lorem_sentence':
      Exit(LoremSentence);

    'lorem_text':
      begin
        A := StrToInt(Col.Args.Raw.Values['min']);
        B := StrToInt(Col.Args.Raw.Values['max']);
        Exit(LoremTextRange(A, B));
      end;

    'days_after':
      begin
        S := Row.Values[Col.Args.Raw.Values['field']];
        BaseDate := ISO8601ToDate(S, False);
        A := StrToInt(Col.Args.Raw.Values['min']);
        B := StrToInt(Col.Args.Raw.Values['max']);
        Exit(FormatDateTime('yyyy-mm-dd', BaseDate + Random(B - A + 1)));
      end;

    'concat':
      begin
        Result := '';
        for S in Col.Args.Raw.Values['fields'].Split(',') do
          Result += Row.Values[S];
      end;

    'initials':
      Exit(Abbreviate(Row.Values[Col.Args.Raw.Values['field']]));

    'email_from_name':
      Exit(EmailFromName(Row.Values[Col.Args.Raw.Values['field']]));

    'random_latitude':
      Exit(FloatToStr(-90 + Random * 180));

    'random_longitude':
      Exit(FloatToStr(-180 + Random * 360));

  end;
end;

function TValueGenerator.ApplyMask(const Mask: string): string;
var
  i: Integer;
  C: Char;
begin
  Result := '';
  for i := 1 to Length(Mask) do
  begin
    C := Mask[i];
    case C of
      '#': Result += IntToStr(Random(10));
      'A': Result += Chr(Ord('A') + Random(26));
      'a': Result += Chr(Ord('a') + Random(26));
      'X': Result += Chr(Ord('A') + Random(26));
      'x': Result += Chr(Ord('a') + Random(26));
    else
      Result += C;
    end;
  end;
end;

function TValueGenerator.ApplyRange(Col: TColumn): string;
var
  MinI, MaxI: Integer;
  MinF, MaxF: Double;
  MinD, MaxD: TDateTime;
begin
  if (Col.RangeMin = '') or (Col.RangeMax = '') then Exit('');

  case Col.DataType of
    'integer':
      begin
        MinI := StrToInt(Col.RangeMin);
        MaxI := StrToInt(Col.RangeMax);
        Exit(IntToStr(MinI + Random(MaxI - MinI + 1)));
      end;

    'float':
      begin
        MinF := StrToFloat(Col.RangeMin);
        MaxF := StrToFloat(Col.RangeMax);
        Exit(FloatToStr(MinF + Random * (MaxF - MinF)));
      end;

    'date', 'datetime':
      begin
        MinD := ISO8601ToDate(Col.RangeMin, False);
        MaxD := ISO8601ToDate(Col.RangeMax, False);
        Exit(FormatDateTime('yyyy-mm-dd', MinD + Random * (MaxD - MinD)));
      end;
  end;
end;

destructor TValueGenerator.Destroy;
var
  i, j: Integer;
  Repo: TExternalRepo;
  Cache: TTableDataCache;
begin
  for i := 0 to FRepos.Count - 1 do
  begin
    Repo := FRepos.Data[i];
    Repo.Data.Free;
    Repo.Free;
  end;
  FRepos.Free;

  for i := 0 to FTableCache.Count - 1 do
  begin
    Cache := FTableCache.Data[i];
    for j := 0 to High(Cache.Rows) do
      Cache.Rows[j].Free;
    Cache.Free;
  end;
  FTableCache.Free;

  FAutoInc.Free;

  inherited Destroy;
end;

function TValueGenerator.Generate(Col: TColumn; Row: TStringList; Schema: TTableSchema): string;
begin
  if Col.Ignore then Exit('');

  if Col.FixedValue <> '' then Exit(Col.FixedValue);

  if Col.AutoIncrement then
    Exit(IntToStr(NextAutoInc(Col.Name, Col.StartAt, Col.IncrementBy)));

  if Col.EnumValues.Count > 0 then
    Exit(ApplyEnum(Col));

  if (Col.RangeMin <> '') and (Col.RangeMax <> '') then
    Exit(ApplyRange(Col));

  if Col.Mask <> '' then
    Exit(ApplyMask(Col.Mask));

  if Col.Source <> '' then
    Exit(GetFromExternalSource(Col, Schema));

  if Col.SourceTable <> '' then
    Exit(GetFromSourceTable(Col));

  if Col.Func <> '' then
    Exit(ApplyFunction(Col, Row, Schema));

  Result := '';
end;

function TValueGenerator.GetFromExternalSource(Col: TColumn; Schema: TTableSchema): string;
var
  Ext: TExternalSource;
  Repo: TExternalRepo;
  i: Integer;
begin
  if not Schema.ExternalSources.TryGetData(Col.Source, Ext) then
    Exit('');

  if not FRepos.TryGetData(Ext.Name, Repo) then
    Exit('');

  i := Random(Repo.Data.Count);
  Result := Repo.Data.Items[i].AsString;
end;

function TValueGenerator.GetFromSourceTable(Col: TColumn): string;
var
  Cache: TTableDataCache;
  Row: TStringList;
  i: Integer;
begin
  if not FTableCache.TryGetData(Col.SourceTable, Cache) then
    Exit('');

  i := Random(Length(Cache.Rows));
  Row := Cache.Rows[i];

  Result := Row.Values[Col.SourceField];
end;

function TValueGenerator.NextAutoInc(const ColName: string; StartAt, IncBy: Integer): Integer;
begin
  if not FAutoInc.TryGetData(ColName, Result) then
  begin
    Result := StartAt;
    FAutoInc.Add(ColName, Result);
  end
  else
  begin
    Result := Result + IncBy;
    FAutoInc[ColName] := Result;
  end;
end;

procedure TValueGenerator.RegisterTableData(const TableName: string; const Rows: array of TStringList);
var
  Cache: TTableDataCache;
  i: Integer;
begin
  if FTableCache.TryGetData(TableName, Cache) then
  begin
    // already exists: clear and replace
    for i := 0 to High(Cache.Rows) do
      Cache.Rows[i].Free;
  end
  else
  begin
    Cache := TTableDataCache.Create;
    Cache.TableName := TableName;
    FTableCache.Add(TableName, Cache);
  end;

  SetLength(Cache.Rows, Length(Rows));
  for i := 0 to High(Rows) do
    Cache.Rows[i] := Rows[i]; // the caller is the owner of all TStringList, or you can clone if you prefere
end;

end.

