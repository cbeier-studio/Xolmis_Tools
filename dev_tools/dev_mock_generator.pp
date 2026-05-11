unit dev_mock_generator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, jsonparser, dev_mock_types, dev_mock_functions;

type

  { TMockDataGenerator }

  TMockDataGenerator = class
  private
    FConfigFileName: string;
    FConfig: TGeneratorConfig;
    FSchemas: specialize TFPGMap<string, TTableSchema>;
    FValueGen: TValueGenerator;

    procedure LoadSchemas;
    function ResolveTableOrder: TStringList;
    function ResolveColumnOrder(ASchema: TTableSchema): TStringList;

    function GenerateColumnValue(Col: TColumn; Row: TStringList; Schema: TTableSchema): string;
    procedure GenerateTable(const Schema: TTableSchema; const OutputFile: string;
      const RowCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadConfig(const FileName: string);
    procedure GenerateAll;
  end;

implementation

function FindColumnByName(const Schema: TTableSchema; const AName: string): TColumn;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Schema.Columns.Count - 1 do
    if SameText(Schema.Columns[i].Name, AName) then
      Exit(Schema.Columns[i]);
end;

function CsvEscape(const S: string): string;
begin
  Result := '"' + StringReplace(S, '"', '""', [rfReplaceAll]) + '"';
end;

{ TMockDataGenerator }

constructor TMockDataGenerator.Create;
begin
  FConfigFileName := '';
  FConfig := nil;
  FSchemas := specialize TFPGMap<string, TTableSchema>.Create;
  FValueGen := nil;
end;

destructor TMockDataGenerator.Destroy;
var
  i: Integer;
begin
  for i := 0 to FSchemas.Count - 1 do
    FSchemas.Data[i].Free;
  FSchemas.Free;
  FConfig.Free;
  FValueGen.Free;
  inherited Destroy;
end;

procedure TMockDataGenerator.GenerateAll;
var
  Order: TStringList;
  i: Integer;
  TableName: string;
  Schema: TTableSchema;
  SRef: TSchemaRef;
  RowCount: Integer;
  OutputBaseDir: string;
  c: Integer;
begin
  Order := ResolveTableOrder;
  try
    OutputBaseDir := IncludeTrailingPathDelimiter(FConfig.GlobalOptions.OutputDir);

    for i := 0 to Order.Count - 1 do
    begin
      TableName := Order[i];
      Schema := FSchemas[TableName];
      SRef := FConfig.GetSchemaByName(TableName);

      if SRef = nil then
        raise Exception.Create('Schema not found in config: ' + TableName);

      RowCount := SRef.DefaultCount;
      if RowCount <= 0 then
        RowCount := SRef.MaxCount;

      if (SRef.MaxCount > 0) and (RowCount > SRef.MaxCount) then
        RowCount := SRef.MaxCount;

      if (FConfig.GlobalOptions.MaxRecordsPerTable > 0)
         and (RowCount > FConfig.GlobalOptions.MaxRecordsPerTable) then
        RowCount := FConfig.GlobalOptions.MaxRecordsPerTable;

      if SRef.OutputFiles.Count = 0 then
        GenerateTable(Schema, OutputBaseDir + TableName + '.csv', RowCount)
      else
        for c := 0 to SRef.OutputFiles.Count - 1 do
          GenerateTable(Schema, OutputBaseDir + SRef.OutputFiles[c], RowCount);
    end;
  finally
    Order.Free;
  end;
end;

function TMockDataGenerator.GenerateColumnValue(Col: TColumn; Row: TStringList; Schema: TTableSchema): string;
begin
  Result := FValueGen.Generate(Col, Row, Schema);
end;

procedure TMockDataGenerator.GenerateTable(const Schema: TTableSchema;
  const OutputFile: string; const RowCount: Integer);
var
  ColOrder: TStringList;
  OutLines: TStringList;
  Values: array of string;
  Row: TStringList;
  GeneratedRows: array of TStringList;
  Col: TColumn;
  i, c: Integer;
  Line: string;
begin
  ColOrder := ResolveColumnOrder(Schema);
  OutLines := TStringList.Create;
  try
    SetLength(Values, ColOrder.Count);
    SetLength(GeneratedRows, RowCount);

    Line := '';
    for i := 0 to ColOrder.Count - 1 do
    begin
      if i > 0 then
        Line := Line + ',';
      Line := Line + CsvEscape(ColOrder[i]);
    end;
    OutLines.Add(Line);

    for i := 0 to RowCount - 1 do
    begin
      Row := TStringList.Create;
      Row.NameValueSeparator := '=';
      GeneratedRows[i] := Row;

      for c := 0 to ColOrder.Count - 1 do
      begin
        Col := FindColumnByName(Schema, ColOrder[c]);
        if Col = nil then
          raise Exception.CreateFmt('Column "%s" not found in schema "%s"',
            [ColOrder[c], Schema.TableName]);

        Values[c] := GenerateColumnValue(
          Col,
          Row,
          Schema
        );
        Row.Values[ColOrder[c]] := Values[c];
      end;

      Line := '';
      for c := 0 to High(Values) do
      begin
        if c > 0 then
          Line := Line + ',';
        Line := Line + CsvEscape(Values[c]);
      end;
      OutLines.Add(Line);
    end;

    ForceDirectories(ExtractFileDir(OutputFile));
    OutLines.SaveToFile(OutputFile);

    FValueGen.RegisterTableData(Schema.TableName, GeneratedRows);

  finally
    OutLines.Free;
    ColOrder.Free;
  end;
end;

procedure TMockDataGenerator.LoadConfig(const FileName: string);
var
  BaseDir: string;
  i: Integer;

  function IsAbsolutePath(const APath: string): Boolean;
  begin
    Result := (ExtractFileDrive(APath) <> '')
      or ((Length(APath) >= 2) and (APath[1] = '\\') and (APath[2] = '\\'))
      or ((APath <> '') and ((APath[1] = '/') or (APath[1] = '\\')));
  end;

  function ToAbsoluteFromBase(const APath: string): string;
  begin
    if IsAbsolutePath(APath) then
      Exit(APath);
    Result := ExpandFileName(IncludeTrailingPathDelimiter(BaseDir) + APath);
  end;
begin
  if Assigned(FConfig) then
    FreeAndNil(FConfig);

  FConfigFileName := ExpandFileName(FileName);

  if not FileExists(FConfigFileName) then
    raise Exception.CreateFmt('Generator config file not found: %s', [FConfigFileName]);

  BaseDir := ExtractFileDir(FConfigFileName);

  FConfig := TGeneratorConfig.Create;
  FConfig.LoadFromJSON(FConfigFileName);

  for i := 0 to FConfig.Schemas.Count - 1 do
    FConfig.Schemas[i].FileName := ToAbsoluteFromBase(FConfig.Schemas[i].FileName);

  for i := 0 to FConfig.Repositories.Count - 1 do
    FConfig.Repositories[i].FileName := ToAbsoluteFromBase(FConfig.Repositories[i].FileName);

  { Validate file paths before proceeding }
  for i := 0 to FConfig.Schemas.Count - 1 do
    if not FileExists(FConfig.Schemas[i].FileName) then
      raise Exception.CreateFmt('Schema file not found for "%s": %s',
        [FConfig.Schemas[i].Name, FConfig.Schemas[i].FileName]);

  for i := 0 to FConfig.Repositories.Count - 1 do
    if not FileExists(FConfig.Repositories[i].FileName) then
      raise Exception.CreateFmt('Repository file not found for "%s": %s',
        [FConfig.Repositories[i].Name, FConfig.Repositories[i].FileName]);

  FConfig.GlobalOptions.OutputDir := ToAbsoluteFromBase(FConfig.GlobalOptions.OutputDir);

  FConfig.Validate;

  LoadSchemas;

  FreeAndNil(FValueGen);
  FValueGen := TValueGenerator.Create(FConfig);
end;

procedure TMockDataGenerator.LoadSchemas;
var
  i: Integer;
  SRef: TSchemaRef;
  Schema: TTableSchema;
begin
  for i := 0 to FSchemas.Count - 1 do
    FSchemas.Data[i].Free;
  FSchemas.Clear;

  for i := 0 to FConfig.Schemas.Count - 1 do
  begin
    SRef := FConfig.Schemas[i];

    Schema := TTableSchema.Create;
    Schema.LoadFromJSON(SRef.FileName);
    Schema.Validate;

    FSchemas.Add(SRef.Name, Schema);
  end;
end;

function TMockDataGenerator.ResolveColumnOrder(ASchema: TTableSchema): TStringList;
var
  Resolver: TColumnDependencyResolver;
begin
  Resolver := TColumnDependencyResolver.Create;
  try
    Result := Resolver.Resolve(ASchema);
  finally
    Resolver.Free;
  end;
end;

function TMockDataGenerator.ResolveTableOrder: TStringList;
var
  Resolver: TTableDependencyResolver;
begin
  Resolver := TTableDependencyResolver.Create;
  try
    Result := Resolver.Resolve(FConfig);
  finally
    Resolver.Free;
  end;
end;

end.

