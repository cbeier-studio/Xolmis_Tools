unit dev_mock_functions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Math, fgl, fpjson, jsonparser, dev_mock_types;

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

function Abbreviate(const FullName: string): string;
var
  Parts: TStringList;
  i: Integer;
  Token: string;
begin
  Result := '';
  Parts := TStringList.Create;
  try
    ExtractStrings([' ', #9, '-', '_'], [], PChar(Trim(FullName)), Parts);
    for i := 0 to Parts.Count - 1 do
    begin
      Token := Trim(Parts[i]);
      if Token <> '' then
        Result := Result + UpCase(Token[1]);
    end;
  finally
    Parts.Free;
  end;
end;

function LoremSentence: string;
const
  Words: array[0..18] of string = (
    'lorem', 'ipsum', 'dolor', 'sit', 'amet', 'consectetur',
    'adipiscing', 'elit', 'sed', 'do', 'eiusmod', 'tempor',
    'incididunt', 'ut', 'labore', 'et', 'dolore', 'magna', 'aliqua'
  );
var
  i, WordCount: Integer;
  W: string;
begin
  WordCount := 6 + Random(9); // 6..14 words
  Result := '';

  for i := 1 to WordCount do
  begin
    W := Words[Random(Length(Words))];
    if i = 1 then
      W[1] := UpCase(W[1]);

    if Result = '' then
      Result := W
    else
      Result := Result + ' ' + W;
  end;

  Result := Result + '.';
end;

function LoremTextRange(MinParagraphs, MaxParagraphs: Integer): string;
const
  Paragraphs: array[0..5] of string = (
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.',
    'Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
    'Integer posuere erat a ante venenatis dapibus posuere velit aliquet. Maecenas faucibus mollis interdum. Cras mattis consectetur purus sit amet fermentum. Aenean lacinia bibendum nulla sed consectetur.',
    'Vivamus sagittis lacus vel augue laoreet rutrum faucibus dolor auctor. Curabitur blandit tempus porttitor. Morbi leo risus, porta ac consectetur ac, vestibulum at eros. Praesent commodo cursus magna vel scelerisque nisl consectetur.',
    'Suspendisse potenti. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Nam volutpat, sem at feugiat posuere, odio lorem interdum neque, non posuere arcu odio a nibh.',
    'Phasellus viverra nulla ut metus varius laoreet. Quisque rutrum aenean imperdiet. Etiam ultricies nisi vel augue. Curabitur ullamcorper ultricies nisi nam eget dui etiam rhoncus.'
  );
var
  i: Integer;
  ParagraphCount: Integer;
  Tmp: Integer;
begin
  if MinParagraphs > MaxParagraphs then
  begin
    Tmp := MinParagraphs;
    MinParagraphs := MaxParagraphs;
    MaxParagraphs := Tmp;
  end;

  if MaxParagraphs < 1 then
    Exit('');

  if MinParagraphs < 1 then
    MinParagraphs := 1;

  ParagraphCount := MinParagraphs + Random(MaxParagraphs - MinParagraphs + 1);
  Result := '';

  for i := 1 to ParagraphCount do
  begin
    if Result = '' then
      Result := Paragraphs[Random(Length(Paragraphs))]
    else
      Result := Result + LineEnding + LineEnding + Paragraphs[Random(Length(Paragraphs))];
  end;
end;

function EmailFromName(const FullName: string): string;
var
  Parts: TStringList;
  i: Integer;
  Token: string;

  function NormalizeEmailPart(const S: string): string;
  var
    j: Integer;
    C: Char;
  begin
    Result := '';
    for j := 1 to Length(LowerCase(S)) do
    begin
      C := LowerCase(S)[j];
      if ((C >= 'a') and (C <= 'z')) or ((C >= '0') and (C <= '9')) then
        Result := Result + C;
    end;
  end;

begin
  Parts := TStringList.Create;
  try
    ExtractStrings([' ', #9, '-', '_'], [], PChar(Trim(FullName)), Parts);

    for i := Parts.Count - 1 downto 0 do
    begin
      Token := NormalizeEmailPart(Parts[i]);
      if Token = '' then
        Parts.Delete(i)
      else
        Parts[i] := Token;
    end;

    if Parts.Count = 0 then
      Result := 'user' + IntToStr(1000 + Random(9000))
    else if Parts.Count = 1 then
      Result := Parts[0]
    else
      Result := Parts[0] + '.' + Parts[Parts.Count - 1];

    Result := Result + '@fakemail.com';
  finally
    Parts.Free;
  end;
end;

function FindExternalSourceByName(Sources: TExternalSourceList; const AName: string): TExternalSource;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Sources.Count - 1 do
    if SameText(Sources[i].Name, AName) then
      Exit(Sources[i]);
end;

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
  A, B, D: Integer;
  BaseDate: TDateTime;
  S: string;
begin
  case Col.Func of

    // Number functions
    'random_int':
      begin
        A := StrToIntDef(Col.Args.Raw.Values['min'], 0);
        B := StrToIntDef(Col.Args.Raw.Values['max'], 0);
        Exit(IntToStr(A + Random(B - A + 1)));
      end;

    'random_int_range': ;

    'random_float':
      begin
        A := StrToIntDef(Col.Args.Raw.Values['min'], 0);
        B := StrToIntDef(Col.Args.Raw.Values['max'], 0);
        D := StrToInt(Col.Args.Raw.Values['decimals']);
        Exit(FormatFloat(',0.' + StringOfChar('0', D), A + Random * (B - A)));
      end;

    // Date functions
    'days_after':
      begin
        S := Row.Values[Col.Args.Raw.Values['field']];
        BaseDate := ISO8601ToDate(S, False);
        A := StrToInt(Col.Args.Raw.Values['min']);
        B := StrToInt(Col.Args.Raw.Values['max']);
        Exit(FormatDateTime('yyyy-mm-dd', BaseDate + Random(B - A + 1)));
      end;

    'add_days_random': ;
    'add_years_random': ;
    'inherit_or_random_date': ;
    'extract_day': ;
    'extract_month': ;
    'extract_year': ;

    // Time functions
    'random_time': ;
    'random_time_after': ;

    // Text functions
    'concat':
      begin
        Result := '';
        for S in Col.Args.Raw.Values['fields'].Split(',') do
          Result += Row.Values[S];
      end;

    'initials':
      Exit(Abbreviate(Row.Values[Col.Args.Raw.Values['field']]));

    'citation': ;

    'lorem_sentence':
      Exit(LoremSentence);

    'lorem_text':
      begin
        A := StrToInt(Col.Args.Raw.Values['min']);
        B := StrToInt(Col.Args.Raw.Values['max']);
        Exit(LoremTextRange(A, B));
      end;

    'email_from_name':
      Exit(EmailFromName(Row.Values[Col.Args.Raw.Values['field']]));

    'fake_url': ;
    'random_color': ;
    'tally_values': ;

    // Coordinate functions
    'random_latitude':
      Exit(FloatToStr(-90 + Random * 180));

    'random_longitude':
      Exit(FloatToStr(-180 + Random * 360));

    // Hierarchy functions
    'inherit_hierarchy': ;
    'hierarchical_name': ;

    // Taxonomy functions
    'inherit_taxonomy': ;
    'assign_valid_taxon': ;
    'format_botanical_name': ;
    'format_zoological_name': ;

    // Sequence functions
    'sequence_within_group': ;

    // Polymorphic functions
    'random_record_id': ;
    'inherit_or_source': ;

    // Conversion functions
    'convert_beaufort_to_kmh': ;

    // Formula functions
    'egg_volume_formula': ;
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
  i, RepoIndex: Integer;
begin
  Ext := FindExternalSourceByName(Schema.ExternalSources, Col.Source);
  if Ext = nil then
    Exit('');

  RepoIndex := FRepos.IndexOf(Ext.Name);
  if RepoIndex = -1 then
    Exit('');

  Repo := FRepos.Data[RepoIndex];
  if (Repo = nil) or (Repo.Data = nil) or (Repo.Data.Count = 0) then
    Exit('');

  i := Random(Repo.Data.Count);
  Result := Repo.Data.Items[i].AsString;
end;

function TValueGenerator.GetFromSourceTable(Col: TColumn): string;
var
  Cache: TTableDataCache;
  Row: TStringList;
  i, CacheIndex: Integer;
begin
  CacheIndex := FTableCache.IndexOf(Col.SourceTable);
  if CacheIndex = -1 then
    Exit('');

  Cache := FTableCache.Data[CacheIndex];
  if (Cache = nil) or (Length(Cache.Rows) = 0) then
    Exit('');

  i := Random(Length(Cache.Rows));
  Row := Cache.Rows[i];

  Result := Row.Values[Col.SourceField];
end;

function TValueGenerator.NextAutoInc(const ColName: string; StartAt, IncBy: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := FAutoInc.IndexOf(ColName);
  if Idx = -1 then
  begin
    Result := StartAt;
    FAutoInc.Add(ColName, Result);
  end
  else
  begin
    Result := FAutoInc.Data[Idx];
    Result := Result + IncBy;
    FAutoInc.Data[Idx] := Result;
  end;
end;

procedure TValueGenerator.RegisterTableData(const TableName: string; const Rows: array of TStringList);
var
  Cache: TTableDataCache;
  i, CacheIndex: Integer;
begin
  CacheIndex := FTableCache.IndexOf(TableName);
  if CacheIndex <> -1 then
  begin
    Cache := FTableCache.Data[CacheIndex];
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

