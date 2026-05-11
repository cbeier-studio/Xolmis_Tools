unit dev_mock_functions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Math, fgl, fpjson, jsonparser, FileUtil, StrUtils,
  dev_mock_types;

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

    procedure LoadRepositories;
    function NormalizeName(const S: string): string;
    function FindRepoByName(const AName: string): TExternalRepo;
    function ExtractValueFromObject(Obj: TJSONObject; RepoDef: TRepositoryRef): string;
    function FindRowInTable(const TableName, KeyField, KeyValue: string): TStringList;
    function GetRandomFieldFromTable(const TableName, FieldName: string): string;
    function ParseFieldsArg(const RawValue: string): TStringList;
    function MaybeNull(const Col: TColumn): Boolean;
    function DateFromRow(const Row: TStringList; const FieldName: string;
      out Value: TDateTime): Boolean;

    function GetFromExternalSource(Col: TColumn; Schema: TTableSchema): string;
    function GetFromSourceTable(Col: TColumn): string;

  public
    constructor Create(AConfig: TGeneratorConfig);
    destructor Destroy; override;

    procedure RegisterTableData(const TableName: string; const Rows: array of TStringList);

    function Generate(Col: TColumn; Row: TStringList; Schema: TTableSchema): string;
  end;


implementation

function NormalizeListToken(const S: string): string;
begin
  Result := Trim(S);
  Result := StringReplace(Result, '"', '', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '', [rfReplaceAll]);
  Result := StringReplace(Result, '[', '', [rfReplaceAll]);
  Result := StringReplace(Result, ']', '', [rfReplaceAll]);
  Result := Trim(Result);
end;

function TryParseISODateTime(const S: string; out Value: TDateTime): Boolean;
var
  Raw: string;
  DatePart: string;
  TimePart: string;
  YearValue, MonthValue, DayValue: Word;
  HourValue, MinuteValue, SecondValue, MilliValue: Word;

  function ParseDatePart(const ADate: string; out AYear, AMonth, ADay: Word): Boolean;
  begin
    Result := (Length(ADate) = 10)
      and (ADate[5] = '-')
      and (ADate[8] = '-');
    if not Result then
      Exit;

    AYear := StrToIntDef(Copy(ADate, 1, 4), -1);
    AMonth := StrToIntDef(Copy(ADate, 6, 2), -1);
    ADay := StrToIntDef(Copy(ADate, 9, 2), -1);
    Result := (AYear > 0) and (AMonth >= 1) and (AMonth <= 12) and (ADay >= 1) and (ADay <= 31);
  end;

  function ParseTimePart(const ATime: string; out AHour, AMinute, ASecond, AMilli: Word): Boolean;
  var
    TimeCore: string;
    MilliText: string;
    DotPos: Integer;
  begin
    AHour := 0;
    AMinute := 0;
    ASecond := 0;
    AMilli := 0;

    if ATime = '' then
      Exit(True);

    TimeCore := ATime;
    if EndsText('Z', TimeCore) then
      Delete(TimeCore, Length(TimeCore), 1);

    DotPos := Pos('.', TimeCore);
    if DotPos > 0 then
    begin
      MilliText := Copy(TimeCore, DotPos + 1, MaxInt);
      TimeCore := Copy(TimeCore, 1, DotPos - 1);
      while Length(MilliText) < 3 do
        MilliText := MilliText + '0';
      AMilli := StrToIntDef(Copy(MilliText, 1, 3), 0);
    end;

    Result := ((Length(TimeCore) = 5) or (Length(TimeCore) = 8))
      and (TimeCore[3] = ':');
    if not Result then
      Exit;

    AHour := StrToIntDef(Copy(TimeCore, 1, 2), -1);
    AMinute := StrToIntDef(Copy(TimeCore, 4, 2), -1);

    if Length(TimeCore) = 8 then
    begin
      Result := TimeCore[6] = ':';
      if not Result then
        Exit;
      ASecond := StrToIntDef(Copy(TimeCore, 7, 2), -1);
    end;

    Result := (AHour >= 0) and (AHour <= 23)
      and (AMinute >= 0) and (AMinute <= 59)
      and (ASecond >= 0) and (ASecond <= 59)
      and (AMilli >= 0) and (AMilli <= 999);
  end;

var
  SepPos: Integer;
begin
  Result := False;
  Raw := Trim(S);
  if Raw = '' then
    Exit;

  SepPos := Pos('T', Raw);
  if SepPos = 0 then
    SepPos := Pos(' ', Raw);

  if SepPos > 0 then
  begin
    DatePart := Copy(Raw, 1, SepPos - 1);
    TimePart := Copy(Raw, SepPos + 1, MaxInt);
  end
  else
  begin
    DatePart := Raw;
    TimePart := '';
  end;

  if not ParseDatePart(DatePart, YearValue, MonthValue, DayValue) then
    Exit;

  if not ParseTimePart(TimePart, HourValue, MinuteValue, SecondValue, MilliValue) then
    Exit;

  try
    Value := EncodeDateTime(YearValue, MonthValue, DayValue,
      HourValue, MinuteValue, SecondValue, MilliValue);
    Result := True;
  except
    Result := False;
  end;
end;

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

function ApplyTextCase(const S, Mode: string): string;
begin
  if SameText(Mode, 'uppercase') then
    Exit(UpperCase(S));

  if SameText(Mode, 'lowercase') then
    Exit(LowerCase(S));

  if SameText(Mode, 'titlecase') then
    Exit(StringReplace(AnsiProperCase(LowerCase(S), [' ', #9, '-', '_']), '_', ' ', [rfReplaceAll]));

  if SameText(Mode, 'sentencecase') then
  begin
    Result := LowerCase(S);
    if Result <> '' then
      Result[1] := UpCase(Result[1]);
    Exit;
  end;

  Result := S;
end;

function Slugify(const S: string): string;
var
  i: Integer;
  C: Char;
  LastHyphen: Boolean;
begin
  Result := '';
  LastHyphen := False;

  for i := 1 to Length(LowerCase(S)) do
  begin
    C := LowerCase(S)[i];
    if ((C >= 'a') and (C <= 'z')) or ((C >= '0') and (C <= '9')) then
    begin
      Result := Result + C;
      LastHyphen := False;
    end
    else if not LastHyphen then
    begin
      Result := Result + '-';
      LastHyphen := True;
    end;
  end;

  while (Result <> '') and (Result[1] = '-') do
    Delete(Result, 1, 1);

  while (Result <> '') and (Result[Length(Result)] = '-') do
    Delete(Result, Length(Result), 1);

  if Result = '' then
    Result := 'resource';
end;

{ TValueGenerator }

constructor TValueGenerator.Create(AConfig: TGeneratorConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FRepos := TExternalRepoMap.Create;
  FTableCache := TTableCacheMap.Create;
  FAutoInc := TStringIntMap.Create;

  LoadRepositories;
  Randomize;
end;

function TValueGenerator.NormalizeName(const S: string): string;
begin
  Result := LowerCase(Trim(StringReplace(StringReplace(S, '-', '', [rfReplaceAll]),
    '_', '', [rfReplaceAll])));
end;

function TValueGenerator.FindRepoByName(const AName: string): TExternalRepo;
var
  i: Integer;
  KeyNorm: string;
begin
  Result := nil;
  KeyNorm := NormalizeName(AName);

  for i := 0 to FRepos.Count - 1 do
    if NormalizeName(FRepos.Keys[i]) = KeyNorm then
      Exit(FRepos.Data[i]);
end;

function TValueGenerator.ExtractValueFromObject(Obj: TJSONObject;
  RepoDef: TRepositoryRef): string;
var
  i, j: Integer;
  FieldName: string;
begin
  Result := '';

  for i := 0 to RepoDef.Fields.Count - 1 do
  begin
    FieldName := RepoDef.Fields[i];
    if Obj.Find(FieldName) <> nil then
    begin
      j := Obj.IndexOfName(FieldName);
      Exit(Obj.Items[i].AsString);
    end;
  end;

  for i := 0 to Obj.Count - 1 do
    if Obj.Items[i] <> nil then
      Exit(Obj.Items[i].AsString);
end;

function TValueGenerator.FindRowInTable(const TableName, KeyField,
  KeyValue: string): TStringList;
var
  Cache: TTableDataCache;
  i, CacheIndex: Integer;
begin
  Result := nil;

  CacheIndex := FTableCache.IndexOf(TableName);
  if CacheIndex = -1 then
    Exit;

  Cache := FTableCache.Data[CacheIndex];
  if Cache = nil then
    Exit;

  for i := 0 to High(Cache.Rows) do
    if SameText(Cache.Rows[i].Values[KeyField], KeyValue) then
      Exit(Cache.Rows[i]);
end;

function TValueGenerator.GetRandomFieldFromTable(const TableName,
  FieldName: string): string;
var
  Cache: TTableDataCache;
  CacheIndex, i: Integer;
begin
  Result := '';

  CacheIndex := FTableCache.IndexOf(TableName);
  if CacheIndex = -1 then
    Exit;

  Cache := FTableCache.Data[CacheIndex];
  if (Cache = nil) or (Length(Cache.Rows) = 0) then
    Exit;

  i := Random(Length(Cache.Rows));
  Result := Cache.Rows[i].Values[FieldName];
end;

function TValueGenerator.ParseFieldsArg(const RawValue: string): TStringList;
var
  Tmp: string;
  Parts: TStringArray;
  P, S: string;
begin
  Result := TStringList.Create;

  Tmp := Trim(RawValue);
  if Tmp = '' then
    Exit;

  Parts := Tmp.Split(',');
  for P in Parts do
  begin
    S := NormalizeListToken(P);
    if S <> '' then
      Result.Add(S);
  end;
end;

function TValueGenerator.MaybeNull(const Col: TColumn): Boolean;
var
  Prob: Double;
begin
  Result := False;
  if not Col.Nullable then
    Exit;

  Prob := StrToFloatDef(Col.Args.Raw.Values['nullableProbability'], -1);
  if Prob < 0 then
    Exit(False);

  if Prob > 1 then
    Prob := 1;

  Result := Random < Prob;
end;

function TValueGenerator.DateFromRow(const Row: TStringList;
  const FieldName: string; out Value: TDateTime): Boolean;
var
  S: string;
begin
  Result := False;
  S := Trim(Row.Values[FieldName]);
  if (S = '') or SameText(S, 'null') then
    Exit;

  Result := TryParseISODateTime(S, Value);
end;

procedure TValueGenerator.LoadRepositories;
var
  i, j: Integer;
  RepoDef: TRepositoryRef;
  Repo: TExternalRepo;
  JSON: TJSONData;
  Arr, Values: TJSONArray;
  Obj: TJSONObject;
  V: string;
begin
  for i := 0 to FConfig.Repositories.Count - 1 do
  begin
    RepoDef := FConfig.Repositories[i];

    if not FileExists(RepoDef.FileName) then
      raise Exception.CreateFmt('Repository file not found: %s', [RepoDef.FileName]);

    JSON := GetJSON(ReadFileToString(RepoDef.FileName));
    Repo := TExternalRepo.Create;
    Repo.Name := RepoDef.Name;

    try
      if SameText(RepoDef.RepoType, 'array') then
      begin
        if JSON.JSONType <> jtArray then
          raise Exception.CreateFmt('Repository "%s" must be an array.', [RepoDef.Name]);
        Repo.Data := TJSONArray(JSON);
        JSON := nil;
      end
      else
      begin
        if JSON.JSONType <> jtArray then
          raise Exception.CreateFmt('Repository "%s" must be an array of objects.',
            [RepoDef.Name]);

        Arr := TJSONArray(JSON);
        Values := TJSONArray.Create;
        for j := 0 to Arr.Count - 1 do
        begin
          if Arr.Items[j].JSONType <> jtObject then
            Continue;

          Obj := TJSONObject(Arr.Items[j]);
          V := ExtractValueFromObject(Obj, RepoDef);
          if V <> '' then
            Values.Add(V);
        end;

        Repo.Data := Values;
      end;

      FRepos.Add(Repo.Name, Repo);
    except
      Repo.Free;
      JSON.Free;
      raise;
    end;
  end;
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
  S, RefField, Delim, Prefix, Suffix, GroupField, GroupValue, KeyName: string;
  Fields: TStringList;
  C: Integer;
  ParentTable, ParentField, DateField, ParentId: string;
  ParentRow: TStringList;
  H, M: Integer;
  Beaufort: Integer;
  LenV, WidV: Double;
  SkipNulls: Boolean;
  TextCase: string;
  ValueField: string;
begin
  if MaybeNull(Col) then
    Exit('null');

  case Col.Func of

    // Number functions
    'random_int':
      begin
        A := StrToIntDef(Col.Args.Raw.Values['min'], 0);
        B := StrToIntDef(Col.Args.Raw.Values['max'], 0);
        if B < A then
          B := A;
        Exit(IntToStr(A + Random(B - A + 1)));
      end;

    'random_int_range':
      begin
        A := StrToIntDef(Col.Args.Raw.Values['min'], 0);
        B := StrToIntDef(Col.Args.Raw.Values['max'], 0);
        if B < A then
          B := A;
        Exit(IntToStr(A + Random(B - A + 1)));
      end;

    'random_float':
      begin
        A := StrToIntDef(Col.Args.Raw.Values['min'], 0);
        B := StrToIntDef(Col.Args.Raw.Values['max'], 0);
        if B < A then
        begin
          D := A;
          A := B;
          B := D;
        end;

        D := StrToIntDef(Col.Args.Raw.Values['decimals'], 2);
        if D < 0 then
          D := 0;
        if D > 10 then
          D := 10;

        Exit(FormatFloat('0.' + StringOfChar('0', D),
          A + Random * (B - A)));
      end;

    // Date functions
    'days_after':
      begin
        RefField := Col.Args.Raw.Values['field'];
        if (RefField = '') and (Col.DependsOn.Count > 0) then
          RefField := Col.DependsOn[0];

        if not DateFromRow(Row, RefField, BaseDate) then
          BaseDate := Today;

        A := StrToIntDef(Col.Args.Raw.Values['min'], 1);
        B := StrToIntDef(Col.Args.Raw.Values['max'], A);
        if B < A then
          B := A;
        Exit(FormatDateTime('yyyy-mm-dd', IncDay(BaseDate, A + Random(B - A + 1))));
      end;

    'add_days_random':
      begin
        RefField := Col.Args.Raw.Values['field'];
        if (RefField = '') and (Col.DependsOn.Count > 0) then
          RefField := Col.DependsOn[0];

        if not DateFromRow(Row, RefField, BaseDate) then
          BaseDate := Today;

        A := StrToIntDef(Col.Args.Raw.Values['min'], 1);
        B := StrToIntDef(Col.Args.Raw.Values['max'], A);
        if B < A then
          B := A;
        Exit(FormatDateTime('yyyy-mm-dd', IncDay(BaseDate, A + Random(B - A + 1))));
      end;

    'add_years_random':
      begin
        RefField := Col.Args.Raw.Values['field'];
        if (RefField = '') and (Col.DependsOn.Count > 0) then
          RefField := Col.DependsOn[0];

        if not DateFromRow(Row, RefField, BaseDate) then
          BaseDate := Today;

        A := Trunc(StrToFloatDef(Col.Args.Raw.Values['min'], 1));
        B := Trunc(StrToFloatDef(Col.Args.Raw.Values['max'], A));
        if B < A then
          B := A;
        Exit(FormatDateTime('yyyy-mm-dd', IncYear(BaseDate, A + Random(B - A + 1))));
      end;

    'inherit_or_random_date':
      begin
        ParentTable := Col.Args.Raw.Values['parentSource'];
        ParentField := Col.Args.Raw.Values['parentField'];
        DateField := Col.Args.Raw.Values['dateField'];
        ParentId := Row.Values[ParentField];

        if (ParentTable <> '') and (ParentField <> '') and (DateField <> '') and (ParentId <> '') then
        begin
          ParentRow := FindRowInTable(ParentTable, ParentField, ParentId);
          if (ParentRow <> nil) and (Trim(ParentRow.Values[DateField]) <> '') then
            Exit(ParentRow.Values[DateField]);
        end;

        Exit(FormatDateTime('yyyy-mm-dd', IncDay(Today, -Random(3650))));
      end;

    'extract_day':
      begin
        RefField := Col.Args.Raw.Values['field'];
        if (RefField = '') and (Col.DependsOn.Count > 0) then
          RefField := Col.DependsOn[0];

        if DateFromRow(Row, RefField, BaseDate) then
          Exit(IntToStr(DayOf(BaseDate)))
        else
          Exit('');
      end;

    'extract_month':
      begin
        RefField := Col.Args.Raw.Values['field'];
        if (RefField = '') and (Col.DependsOn.Count > 0) then
          RefField := Col.DependsOn[0];

        if DateFromRow(Row, RefField, BaseDate) then
          Exit(IntToStr(MonthOf(BaseDate)))
        else
          Exit('');
      end;

    'extract_year':
      begin
        RefField := Col.Args.Raw.Values['field'];
        if (RefField = '') and (Col.DependsOn.Count > 0) then
          RefField := Col.DependsOn[0];

        if DateFromRow(Row, RefField, BaseDate) then
          Exit(IntToStr(YearOf(BaseDate)))
        else
          Exit('');
      end;

    // Time functions
    'random_time':
      begin
        H := Random(24);
        M := Random(60);
        Exit(Format('%.2d:%.2d', [H, M]));
      end;

    'random_time_after':
      begin
        RefField := Col.Args.Raw.Values['field'];
        S := Row.Values[RefField];
        if Length(S) >= 5 then
        begin
          H := StrToIntDef(Copy(S, 1, 2), 8);
          M := StrToIntDef(Copy(S, 4, 2), 0);
        end
        else
        begin
          H := 8;
          M := 0;
        end;

        A := StrToIntDef(Col.Args.Raw.Values['min'], 10);
        B := StrToIntDef(Col.Args.Raw.Values['max'], A);
        if B < A then
          B := A;
        BaseDate := EncodeTime(H, M, 0, 0) + (A + Random(B - A + 1)) / (24 * 60);
        Exit(FormatDateTime('hh:nn', BaseDate));
      end;

    'random_time_optional':
      begin
        if MaybeNull(Col) then
          Exit('null');
        H := Random(24);
        M := Random(60);
        Exit(Format('%.2d:%.2d', [H, M]));
      end;

    'random_time_after_optional':
      begin
        if MaybeNull(Col) then
          Exit('null');

        RefField := Col.Args.Raw.Values['field'];
        S := Row.Values[RefField];
        if Length(S) >= 5 then
        begin
          H := StrToIntDef(Copy(S, 1, 2), 8);
          M := StrToIntDef(Copy(S, 4, 2), 0);
        end
        else
        begin
          H := 8;
          M := 0;
        end;

        A := StrToIntDef(Col.Args.Raw.Values['min'], 10);
        B := StrToIntDef(Col.Args.Raw.Values['max'], A);
        if B < A then
          B := A;
        BaseDate := EncodeTime(H, M, 0, 0) + (A + Random(B - A + 1)) / (24 * 60);
        Exit(FormatDateTime('hh:nn', BaseDate));
      end;

    // Text functions
    'concat':
      begin
        Fields := ParseFieldsArg(Col.Args.Raw.Values['fields']);
        try
          Delim := Col.Args.Raw.Values['delimiter'];
          Prefix := Col.Args.Raw.Values['prefix'];
          Suffix := Col.Args.Raw.Values['suffix'];
          SkipNulls := SameText(Trim(Col.Args.Raw.Values['skipNulls']), 'true');

          Result := Prefix;
          for C := 0 to Fields.Count - 1 do
          begin
            S := Trim(Row.Values[Fields[C]]);
            if SkipNulls and ((S = '') or SameText(S, 'null')) then
              Continue;

            if (Result <> Prefix) and (Delim <> '') then
              Result := Result + Delim;

            if Result = Prefix then
              Result := Result + S
            else
              Result := Result + S;
          end;
          Result := Result + Suffix;
          Exit(Result);
        finally
          Fields.Free;
        end;
      end;

    'abbreviate':
      begin
        RefField := Col.Args.Raw.Values['field'];
        if (RefField = '') and (Col.DependsOn.Count > 0) then
          RefField := Col.DependsOn[0];
        if RefField = '' then
          RefField := 'project_title';
        Exit(Abbreviate(Row.Values[RefField]));
      end;

    'format_individual_name':
      begin
        Fields := ParseFieldsArg(Col.Args.Raw.Values['fields']);
        try
          if Fields.Count > 0 then
            S := Row.Values[Fields[0]]
          else
            S := Row.Values['full_name'];

          if Fields.Count > 1 then
            Exit(Trim(S + ' #' + Row.Values[Fields[1]]))
          else
            Exit(Trim(S));
        finally
          Fields.Free;
        end;
      end;

    'initials':
      begin
        RefField := Col.Args.Raw.Values['field'];
        if RefField = '' then
        begin
          Fields := ParseFieldsArg(Col.Args.Raw.Values['fields']);
          try
            if Fields.Count > 0 then
              RefField := Fields[0];
          finally
            Fields.Free;
          end;
        end;
        TextCase := Col.Args.Raw.Values['textcase'];
        Exit(ApplyTextCase(Abbreviate(Row.Values[RefField]), TextCase));
      end;

    'citation':
      begin
        Fields := ParseFieldsArg(Col.Args.Raw.Values['fields']);
        try
          if Fields.Count > 0 then
            S := Trim(Row.Values[Fields[0]])
          else
            S := Trim(Row.Values['full_name']);
        finally
          Fields.Free;
        end;

        if S = '' then
          Exit('Anon.');

        Fields := ParseFieldsArg(S);
        try
          if Fields.Count > 0 then
            Exit(Fields[Fields.Count - 1] + ', ' + IntToStr(1990 + Random(36)))
          else
            Exit('Anon.');
        finally
          Fields.Free;
        end;
      end;

    'lorem_sentence':
      Exit(LoremSentence);

    'lorem_text':
      begin
        A := StrToIntDef(Col.Args.Raw.Values['min'], 1);
        B := StrToIntDef(Col.Args.Raw.Values['max'], 2);
        Exit(LoremTextRange(A, B));
      end;

    'email_from_name':
      begin
        RefField := Col.Args.Raw.Values['field'];
        if RefField = '' then
        begin
          Fields := ParseFieldsArg(Col.Args.Raw.Values['fields']);
          try
            if Fields.Count > 0 then
              RefField := Fields[0];
          finally
            Fields.Free;
          end;
        end;
        Exit(EmailFromName(Row.Values[RefField]));
      end;

    'fake_url':
      begin
        Fields := ParseFieldsArg(Col.Args.Raw.Values['fields']);
        try
          if Fields.Count > 0 then
            S := LowerCase(Trim(Row.Values[Fields[0]]))
          else
            S := 'project';
        finally
          Fields.Free;
        end;

        Exit('https://example.org/' + Slugify(S));
      end;

    'random_color':
      begin
        if SameText(Col.Args.Raw.Values['colorFormat'], 'rgb') then
          Exit(Format('rgb(%d,%d,%d)', [Random(256), Random(256), Random(256)]))
        else
          Exit(Format('#%.2x%.2x%.2x', [Random(256), Random(256), Random(256)]));
      end;

    'tally_values':
      begin
        if Random < 0.15 then
          Exit('x');
        A := StrToIntDef(Col.Args.Raw.Values['min'], 0);
        B := StrToIntDef(Col.Args.Raw.Values['max'], 10);
        if B < A then
          B := A;
        Exit(IntToStr(A + Random(B - A + 1)));
      end;

    // Coordinate functions
    'random_latitude':
      Exit(FloatToStr(-90 + Random * 180));

    'random_longitude':
      Exit(FloatToStr(-180 + Random * 360));

    // Hierarchy functions
    'inherit_hierarchy':
      begin
        ParentField := Col.Args.Raw.Values['parentField'];
        if ParentField = '' then
          ParentField := Schema.Hierarchy.ParentField;

        ParentTable := Col.Args.Raw.Values['parentSource'];
        if ParentTable = '' then
          ParentTable := Schema.Relations.Parent;

        ValueField := Col.Args.Raw.Values['level'];
        if ValueField = '' then
          ValueField := Col.Args.Raw.Values['rankField'];

        if (ParentTable <> '') and (ParentField <> '') and (ValueField <> '') then
        begin
          ParentId := Row.Values[ParentField];
          if ParentId <> '' then
          begin
            ParentRow := FindRowInTable(ParentTable, ParentField, ParentId);
            if ParentRow <> nil then
            begin
              S := ParentRow.Values[ValueField];
              if S <> '' then
                Exit(S);
            end;
          end;
        end;

        S := LowerCase(Col.Args.Raw.Values['level']);
        if S = 'country' then
          Exit(Row.Values['country_id'])
        else if S = 'state' then
          Exit(Row.Values['state_id'])
        else if S = 'municipality' then
          Exit(Row.Values['municipality_id'])
        else
          Exit('');
      end;

    'hierarchical_name':
      begin
        RefField := Col.Args.Raw.Values['field'];
        if RefField = '' then
          RefField := 'site_name';

        ParentField := Col.Args.Raw.Values['parentField'];
        if ParentField = '' then
          ParentField := Schema.Hierarchy.ParentField;

        ParentTable := Col.Args.Raw.Values['parentSource'];
        if ParentTable = '' then
          ParentTable := Schema.Relations.Parent;

        S := Row.Values[RefField];
        ParentId := Row.Values[ParentField];

        if (ParentTable <> '') and (ParentField <> '') and (ParentId <> '') then
        begin
          ParentRow := FindRowInTable(ParentTable, ParentField, ParentId);
          if ParentRow <> nil then
          begin
            A := ParentRow.IndexOfName(RefField);
            if A <> -1 then
            begin
              B := Pos('=', ParentRow[A]);
              if B > 0 then
                Exit(Trim(Copy(ParentRow[A], B + 1, MaxInt) + ' / ' + S));
            end;
          end;
        end;

        Exit(S);
      end;

    // Taxonomy functions
    'inherit_taxonomy':
      begin
        ParentField := Col.Args.Raw.Values['parentField'];
        if ParentField = '' then
          ParentField := Schema.Relations.ForeignKey;

        ParentTable := Col.Args.Raw.Values['parentSource'];
        if ParentTable = '' then
          ParentTable := Schema.Relations.Parent;

        ValueField := Col.Args.Raw.Values['level'];
        if ValueField = '' then
          ValueField := Col.Args.Raw.Values['rankField'];
        if ValueField = '' then
          ValueField := Col.Name;

        ParentId := Row.Values[ParentField];
        if (ParentTable <> '') and (ParentField <> '') and (ParentId <> '') then
        begin
          ParentRow := FindRowInTable(ParentTable, ParentField, ParentId);
          if ParentRow <> nil then
          begin
            S := ParentRow.Values[ValueField];
            if S <> '' then
              Exit(S);
          end;
        end;

        if Col.SourceTable <> '' then
          Exit(GetFromSourceTable(Col));

        if Col.Source <> '' then
          Exit(GetFromExternalSource(Col, Schema));

        Exit('');
      end;

    'assign_valid_taxon':
      begin
        ParentTable := Col.Args.Raw.Values['sourceTable'];
        if ParentTable = '' then
          ParentTable := Col.SourceTable;
        if ParentTable = '' then
          ParentTable := 'botanic_taxa';

        ValueField := Col.Args.Raw.Values['sourceField'];
        if ValueField = '' then
          ValueField := Col.SourceField;
        if ValueField = '' then
          ValueField := 'taxon_id';

        Exit(GetRandomFieldFromTable(ParentTable, ValueField));
      end;

    'format_botanical_name':
      begin
        Fields := ParseFieldsArg(Col.Args.Raw.Values['fields']);
        try
          if Fields.Count >= 2 then
          begin
            S := Trim(Row.Values[Fields[0]] + ' ' + Row.Values[Fields[1]]);
            if Fields.Count >= 3 then
              S := S + ' ' + Trim(Row.Values[Fields[2]]);
            Exit('<i>' + Trim(S) + '</i>');
          end
          else if Fields.Count = 1 then
            Exit('<i>' + Trim(Row.Values[Fields[0]]) + '</i>')
          else
            Exit('');
        finally
          Fields.Free;
        end;
      end;

    'format_zoological_name':
      begin
        Fields := ParseFieldsArg(Col.Args.Raw.Values['fields']);
        try
          if Fields.Count >= 2 then
          begin
            S := Trim(Row.Values[Fields[0]] + ' ' + Row.Values[Fields[1]]);
            if Fields.Count >= 3 then
              S := S + ' ' + Trim(Row.Values[Fields[2]]);
            Exit('<i>' + Trim(S) + '</i>');
          end
          else if Fields.Count = 1 then
            Exit('<i>' + Trim(Row.Values[Fields[0]]) + '</i>')
          else
            Exit('');
        finally
          Fields.Free;
        end;
      end;

    // Sequence functions
    'sequence_within_group':
      begin
        GroupField := Col.Args.Raw.Values['groupField'];
        GroupValue := Row.Values[GroupField];
        KeyName := Col.Name + '|' + GroupField + '=' + GroupValue;
        Exit(IntToStr(NextAutoInc(KeyName,
          StrToIntDef(Col.Args.Raw.Values['min'], 1),
          1)));
      end;

    // Polymorphic functions
    'random_record_id':
      begin
        RefField := Col.Args.Raw.Values['tableField'];
        if RefField <> '' then
          ParentTable := Row.Values[RefField]
        else
          ParentTable := '';

        if ParentTable <> '' then
          Exit(GetRandomFieldFromTable(ParentTable, ParentTable + '_id'))
        else
          Exit('');
      end;

    'inherit_or_source':
      begin
        ParentField := Col.Args.Raw.Values['parentField'];
        ParentTable := Col.Args.Raw.Values['parentSource'];
        RefField := Col.Args.Raw.Values['sourceField'];
        ParentId := Row.Values[ParentField];

        if (ParentField <> '') and (ParentTable <> '') and (RefField <> '') and (ParentId <> '') then
        begin
          ParentRow := FindRowInTable(ParentTable, ParentField, ParentId);
          if ParentRow <> nil then
          begin
            S := ParentRow.Values[RefField];
            if S <> '' then
              Exit(S);
          end;
        end;

        if Col.SourceTable <> '' then
          Exit(GetFromSourceTable(Col));

        if Col.Source <> '' then
          Exit(GetFromExternalSource(Col, Schema));

        S := Col.Args.Raw.Values['directSource'];
        if S <> '' then
          Exit(GetRandomFieldFromTable(S, S + '_id'));

        Exit('');
      end;

    // Conversion functions
    'convert_beaufort_to_kmh':
      begin
        Beaufort := StrToIntDef(Row.Values[Col.Args.Raw.Values['field']], 0);
        case Beaufort of
          0: Exit('0');
          1: Exit('3');
          2: Exit('9');
          3: Exit('16');
          4: Exit('24');
          5: Exit('34');
          6: Exit('44');
          7: Exit('55');
          8: Exit('67');
          9: Exit('80');
          10: Exit('94');
          11: Exit('109');
        else
          Exit('120');
        end;
      end;

    // Formula functions
    'egg_volume_formula':
      begin
        LenV := StrToFloatDef(Row.Values[Col.Args.Raw.Values['lengthField']], 0);
        WidV := StrToFloatDef(Row.Values[Col.Args.Raw.Values['widthField']], 0);
        Exit(FormatFloat('0.000', 0.51 * LenV * Sqr(WidV)));
      end;

  else
    Result := '';
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
        if not TryParseISODateTime(Col.RangeMin, MinD) then
          raise Exception.CreateFmt('Invalid date/time in range min for column "%s": %s',
            [Col.Name, Col.RangeMin]);

        if not TryParseISODateTime(Col.RangeMax, MaxD) then
          raise Exception.CreateFmt('Invalid date/time in range max for column "%s": %s',
            [Col.Name, Col.RangeMax]);

        if SameText(Col.DataType, 'datetime') then
          Exit(FormatDateTime('yyyy-mm-dd hh:nn:ss', MinD + Random * (MaxD - MinD)));

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

  if Col.NowValue then
  begin
    if SameText(Col.DataType, 'date') then
      Exit(FormatDateTime('yyyy-mm-dd', Now))
    else
      Exit(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  end;

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

  if Col.DefaultValue <> '' then
    Exit(Col.DefaultValue);

  Result := '';
end;

function TValueGenerator.GetFromExternalSource(Col: TColumn; Schema: TTableSchema): string;
var
  Ext: TExternalSource;
  Repo: TExternalRepo;
  i, RepoIndex: Integer;
begin
  Repo := FindRepoByName(Col.Source);
  if (Repo <> nil) and (Repo.Data <> nil) and (Repo.Data.Count > 0) then
  begin
    i := Random(Repo.Data.Count);
    Exit(Repo.Data.Items[i].AsString);
  end;

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

