unit dev_mock_generator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, jsonparser, dev_mock_types, dev_mock_functions;

type

  { TMockDataGenerator }

  TMockDataGenerator = class
  private
    FConfig: TGeneratorConfig;
    FSchemas: specialize TFPGMap<string, TTableSchema>;
    FValueGen: TValueGenerator;

    procedure LoadSchemas;
    function ResolveTableOrder: TStringList;
    function ResolveColumnOrder(ASchema: TTableSchema): TStringList;

    function GenerateColumnValue(Col: TColumn; Row: TStringList; Schema: TTableSchema): string;
    procedure GenerateTable(const Schema: TTableSchema; const OutputFile: string);
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
begin
  Order := ResolveTableOrder;
  try
    for i := 0 to Order.Count - 1 do
    begin
      TableName := Order[i];
      Schema := FSchemas[TableName];
      SRef := FConfig.GetSchemaByName(TableName);

      if SRef.OutputFiles.Count = 0 then
        GenerateTable(Schema, FConfig.GlobalOptions.OutputDir + '/' + TableName + '.csv')
      else
        GenerateTable(Schema, FConfig.GlobalOptions.OutputDir + '/' + SRef.OutputFiles[0]);
    end;
  finally
    Order.Free;
  end;
end;

function TMockDataGenerator.GenerateColumnValue(Col: TColumn; Row: TStringList; Schema: TTableSchema): string;
begin
  Result := FValueGen.Generate(Col, Row, Schema);
end;

procedure TMockDataGenerator.GenerateTable(const Schema: TTableSchema; const OutputFile: string);
var
  ColOrder: TStringList;
  OutLines: TStringList;
  Values: array of string;
  Row: TStringList;
  GeneratedRows: array of TStringList;
  SRef: TSchemaRef;
  Col: TColumn;
  i, c: Integer;
  Count: Integer;
  Line: string;
begin
  ColOrder := ResolveColumnOrder(Schema);
  OutLines := TStringList.Create;
  try
    SRef := FConfig.GetSchemaByName(Schema.TableName);
    if SRef = nil then
      raise Exception.Create('Schema not found in config: ' + Schema.TableName);

    Count := SRef.MaxCount;

    SetLength(Values, ColOrder.Count);
    SetLength(GeneratedRows, Count);

    Line := '';
    for i := 0 to ColOrder.Count - 1 do
    begin
      if i > 0 then
        Line := Line + ',';
      Line := Line + CsvEscape(ColOrder[i]);
    end;
    OutLines.Add(Line);

    for i := 0 to Count - 1 do
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
begin
  if Assigned(FConfig) then
    FreeAndNil(FConfig);

  FConfig := TGeneratorConfig.Create;
  FConfig.LoadFromJSON(FileName);
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

