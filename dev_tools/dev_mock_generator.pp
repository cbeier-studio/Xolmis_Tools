unit dev_mock_generator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpjson, jsonparser, csvdocument, DOM, XMLWrite, fpSpreadsheet, xlsxooxml, fpsopendocument,
  DB, dbf,
  dev_mock_types, dev_mock_functions;

type

  { TCsvExporter }

  TCsvExporter = class(TInterfacedObject, ITableExporter)
  public
    procedure ExportTable(const Schema: TTableSchema; const ColOrder: TStringList; const Rows: array of TStringList;
      const OutputFile: string);
  end;

  { TJsonExporter }

  TJsonExporter = class(TInterfacedObject, ITableExporter)
  public
    procedure ExportTable(const Schema: TTableSchema; const ColOrder: TStringList; const Rows: array of TStringList;
      const OutputFile: string);
  end;

  { TXmlExporter }

  TXmlExporter = class(TInterfacedObject, ITableExporter)
  public
    procedure ExportTable(const Schema: TTableSchema; const ColOrder: TStringList; const Rows: array of TStringList;
      const OutputFile: string);
  end;

  { TXlsxExporter }

  TXlsxExporter = class(TInterfacedObject, ITableExporter)
    procedure ExportTable(const Schema: TTableSchema; const ColOrder: TStringList; const Rows: array of TStringList;
      const OutputFile: string);
  end;

  { TOdsExporter }

  TOdsExporter = class(TInterfacedObject, ITableExporter)
  public
    procedure ExportTable(const Schema: TTableSchema; const ColOrder: TStringList; const Rows: array of TStringList;
      const OutputFile: string);
  end;

  { TDbfExporter }

  TDbfExporter = class(TInterfacedObject, ITableExporter)
  public
    procedure ExportTable(const Schema: TTableSchema; const ColOrder: TStringList; const Rows: array of TStringList;
      const OutputFile: string);
  end;

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
    procedure GenerateTable(const Schema: TTableSchema; const OutputBaseName: string;
      const RowCount: Integer; const Formats: array of string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadConfig(const FileName: string);
    procedure GenerateAll;
  end;

implementation

uses
  udlg_loading;

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
  if (Pos(' ', S) > 0) or (Pos(';', S) > 0) then
    Result := '"' + StringReplace(S, '"', '""', [rfReplaceAll]) + '"'
  else
    Result := StringReplace(S, '"', '""', [rfReplaceAll]);
end;

{ TCsvExporter }

procedure TCsvExporter.ExportTable(const Schema: TTableSchema; const ColOrder: TStringList;
  const Rows: array of TStringList; const OutputFile: string);
var
  Doc: TCSVDocument;
  r, c: Integer;
begin
  Doc := TCSVDocument.Create;
  try
    Doc.Delimiter := ';';
    Doc.QuoteChar := '"';

    // header
    for c := 0 to ColOrder.Count - 1 do
      Doc.AddCell(0, ColOrder[c]);

    // rows
    for r := 0 to High(Rows) do
      for c := 0 to ColOrder.Count - 1 do
        Doc.AddCell(r+1, Rows[r].Values[ColOrder[c]]);

    Doc.SaveToFile(OutputFile);
  finally
    Doc.Free;
  end;
end;

{ TJsonExporter }

procedure TJsonExporter.ExportTable(const Schema: TTableSchema; const ColOrder: TStringList;
  const Rows: array of TStringList; const OutputFile: string);
var
  Arr: TJSONArray;
  Obj: TJSONObject;
  r, c: Integer;
  SL: TStringList;
begin
  Arr := TJSONArray.Create;
  try
    for r := 0 to High(Rows) do
    begin
      Obj := TJSONObject.Create;
      for c := 0 to ColOrder.Count - 1 do
        Obj.Add(ColOrder[c], Rows[r].Values[ColOrder[c]]);
      Arr.Add(Obj);
    end;

    // save formatted JSON
    SL := TStringList.Create;
    try
      SL.Text := Arr.FormatJSON;
      SL.SaveToFile(OutputFile);
    finally
      SL.Free;
    end;
  finally
    Arr.Free;
  end;
end;

{ TXmlExporter }

procedure TXmlExporter.ExportTable(const Schema: TTableSchema; const ColOrder: TStringList;
  const Rows: array of TStringList; const OutputFile: string);
var
  Doc: TXMLDocument;
  Root, RowNode, ColNode: TDOMElement;
  r, c: Integer;
begin
  Doc := TXMLDocument.Create;
  try
    Root := Doc.CreateElement(Schema.TableName);
    Doc.AppendChild(Root);

    for r := 0 to High(Rows) do
    begin
      RowNode := Doc.CreateElement('row');
      Root.AppendChild(RowNode);

      for c := 0 to ColOrder.Count - 1 do
      begin
        ColNode := Doc.CreateElement(ColOrder[c]);
        ColNode.AppendChild(Doc.CreateTextNode(Rows[r].Values[ColOrder[c]]));
        RowNode.AppendChild(ColNode);
      end;
    end;

    WriteXMLFile(Doc, OutputFile);
  finally
    Doc.Free;
  end;
end;

{ TXlsxExporter }

procedure TXlsxExporter.ExportTable(const Schema: TTableSchema; const ColOrder: TStringList;
  const Rows: array of TStringList; const OutputFile: string);
var
  WB: TsWorkbook;
  WS: TsWorksheet;
  r, c: Integer;
begin
  WB := TsWorkbook.Create;
  try
    WS := WB.AddWorksheet(Schema.TableName);

    for c := 0 to ColOrder.Count - 1 do
      WS.WriteText(0, c, ColOrder[c]);

    for r := 0 to High(Rows) do
      for c := 0 to ColOrder.Count - 1 do
        WS.WriteText(r+1, c, Rows[r].Values[ColOrder[c]]);

    WB.WriteToFile(OutputFile, sfidOOXML, True);
  finally
    WB.Free;
  end;
end;

{ TOdsExporter }

procedure TOdsExporter.ExportTable(const Schema: TTableSchema; const ColOrder: TStringList;
  const Rows: array of TStringList; const OutputFile: string);
var
  WB: TsWorkbook;
  WS: TsWorksheet;
  r, c: Integer;
begin
  WB := TsWorkbook.Create;
  try
    WS := WB.AddWorksheet(Schema.TableName);

    for c := 0 to ColOrder.Count - 1 do
      WS.WriteText(0, c, ColOrder[c]);

    for r := 0 to High(Rows) do
      for c := 0 to ColOrder.Count - 1 do
        WS.WriteText(r+1, c, Rows[r].Values[ColOrder[c]]);

    WB.WriteToFile(OutputFile, sfidOpenDocument, True);
  finally
    WB.Free;
  end;
end;

{ TDbfExporter }

procedure TDbfExporter.ExportTable(const Schema: TTableSchema; const ColOrder: TStringList;
  const Rows: array of TStringList; const OutputFile: string);
var
  D: TDbf;
  r, c: Integer;
  Col: TColumn;
begin
  if FileExists(OutputFile) then
    DeleteFile(OutputFile);

  D := TDbf.Create(nil);
  try
    D.FilePathFull := ExtractFileDir(OutputFile);
    D.TableName := ExtractFileName(OutputFile);
    D.TableLevel := 7;

    // define fields
    for c := 0 to ColOrder.Count - 1 do
    begin
      Col := FindColumnByName(Schema, ColOrder[c]);
      case Col.DataType of
        'Integer': D.FieldDefs.Add(ColOrder[c], ftInteger);
        'Float':   D.FieldDefs.Add(ColOrder[c], ftFloat);
        'Date':    D.FieldDefs.Add(ColOrder[c], ftDate);
      else
        D.FieldDefs.Add(ColOrder[c], ftString, 255);
      end;
    end;

    D.CreateTable;
    D.Open;

    // insert data
    for r := 0 to High(Rows) do
    begin
      D.Append;
      for c := 0 to ColOrder.Count - 1 do
        D.FieldByName(ColOrder[c]).AsString := Rows[r].Values[ColOrder[c]];
      D.Post;
    end;

  finally
    D.Free;
  end;
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
  i, c: Integer;
  TableName: string;
  Schema: TTableSchema;
  SRef: TSchemaRef;
  RowCount: Integer;
  OutputBaseDir: string;
  OutputBaseName: string;
  Formats: array of string;
  LoadingDlg: TdlgLoading;
  StepCount: Integer;
  TotalSteps: Integer;
  DisplayName: string;

  function ProgressPercent: Integer;
  begin
    if TotalSteps <= 0 then
      Exit(100);
    Result := Round((StepCount * 100.0) / TotalSteps);
  end;
begin
  Order := ResolveTableOrder;
  try
    LoadingDlg := TdlgLoading.Create(nil);
    try
      LoadingDlg.UpdateProgress('Preparing generation...', 0);
      LoadingDlg.Show;

      TotalSteps := 0;
      for i := 0 to Order.Count - 1 do
      begin
        SRef := FConfig.GetSchemaByName(Order[i]);
        if SRef = nil then
          raise Exception.Create('Schema not found in config: ' + Order[i]);

        if SRef.OutputFiles.Count = 0 then
          Inc(TotalSteps)
        else
          Inc(TotalSteps, SRef.OutputFiles.Count);
      end;

      StepCount := 0;

      OutputBaseDir := IncludeTrailingPathDelimiter(FConfig.GlobalOptions.OutputDir);

      for i := 0 to Order.Count - 1 do
      begin
        TableName := Order[i];
        Schema := FSchemas[TableName];
        SRef := FConfig.GetSchemaByName(TableName);

        if SRef = nil then
          raise Exception.Create('Schema not found in config: ' + TableName);

        if SRef.Enabled =  False then
        begin
          LoadingDlg.UpdateProgress(
            Format('Generating %s (%d/%d)...', [TableName, StepCount + 1, TotalSteps]),
            ProgressPercent
          );
          Inc(StepCount);
          Continue;
        end;

        // determine row quantity
        RowCount := SRef.DefaultCount;
        if RowCount <= 0 then
          RowCount := SRef.MaxCount;

        if (SRef.MaxCount > 0) and (RowCount > SRef.MaxCount) then
          RowCount := SRef.MaxCount;

        if (FConfig.GlobalOptions.MaxRecordsPerTable > 0)
           and (RowCount > FConfig.GlobalOptions.MaxRecordsPerTable) then
          RowCount := FConfig.GlobalOptions.MaxRecordsPerTable;

        // determine formats
        if SRef.OutputFiles.Count = 0 then
        begin
          // old mode: generates only CSV
          SetLength(Formats, 1);
          Formats[0] := 'csv';

          OutputBaseName := OutputBaseDir + TableName;

          LoadingDlg.UpdateProgress(
            Format('Generating %s (%d/%d)...', [TableName, StepCount + 1, TotalSteps]),
            ProgressPercent
          );

          GenerateTable(
            Schema,
            OutputBaseName,
            RowCount,
            Formats
          );

          Inc(StepCount);
        end
        else
        begin
          // new mode: multiple formats defined in config
          for c := 0 to SRef.OutputFiles.Count - 1 do
          begin
            // extract extension
            Formats := SRef.OutputFiles[c].Split(['.']);

            if Length(Formats) < 2 then
              raise Exception.CreateFmt(
                'Invalid output file "%s" for table "%s". Expected something like "file.csv".',
                [SRef.OutputFiles[c], TableName]
              );

            // base name = all except extension
            OutputBaseName := OutputBaseDir +
              ChangeFileExt(SRef.OutputFiles[c], '');

            DisplayName := ExtractFileName(SRef.OutputFiles[c]);
            if DisplayName = '' then
              DisplayName := SRef.OutputFiles[c];

            LoadingDlg.UpdateProgress(
              Format('Generating %s (%d/%d)...', [DisplayName, StepCount + 1, TotalSteps]),
              ProgressPercent
            );

            // format = extension
            SetLength(Formats, 1);
            Formats[0] := StringReplace(LowerCase(ExtractFileExt(SRef.OutputFiles[c])), '.', '', [rfReplaceAll]);

            GenerateTable(
              Schema,
              OutputBaseName,
              RowCount,
              Formats
            );

            Inc(StepCount);
          end;
        end;
      end;

      LoadingDlg.UpdateProgress('Generation finished.', 100);

    finally
      LoadingDlg.Close;
      LoadingDlg.Free;
    end;

  finally
    Order.Free;
  end;
end;

function TMockDataGenerator.GenerateColumnValue(Col: TColumn; Row: TStringList; Schema: TTableSchema): string;
begin
  Result := FValueGen.Generate(Col, Row, Schema);
end;

procedure TMockDataGenerator.GenerateTable(const Schema: TTableSchema; const OutputBaseName: string;
  const RowCount: Integer; const Formats: array of string);
var
  ColOrder: TStringList;
  Rows: array of TStringList;
  Row: TStringList;
  Values: array of string;
  Col: TColumn;
  i, c: Integer;
  Ext: string;
  Exporter: ITableExporter;
  OutputFile: string;
begin
  ColOrder := ResolveColumnOrder(Schema);
  try
    // prepare structure
    SetLength(Values, ColOrder.Count);
    SetLength(Rows, RowCount);

    // generate mock data
    for i := 0 to RowCount - 1 do
    begin
      Row := TStringList.Create;
      Row.NameValueSeparator := '=';
      Rows[i] := Row;

      for c := 0 to ColOrder.Count - 1 do
      begin
        Col := FindColumnByName(Schema, ColOrder[c]);
        if Col = nil then
          raise Exception.CreateFmt(
            'Column "%s" not found in schema "%s"',
            [ColOrder[c], Schema.TableName]
          );

        Values[c] := GenerateColumnValue(Col, Row, Schema);
        Row.Values[ColOrder[c]] := Values[c];
      end;
    end;

    // register for internal use (FKs, dependencies etc.)
    FValueGen.RegisterTableData(Schema.TableName, Rows);

    // export in selected formats
    for Ext in Formats do
    begin
      Exporter := TExporterRegistry.GetExporter(Ext);
      if Exporter = nil then
        raise Exception.CreateFmt('No exporter registered for format "%s"', [Ext]);

      OutputFile := OutputBaseName + '.' + Ext;
      ForceDirectories(ExtractFileDir(OutputFile));

      Exporter.ExportTable(Schema, ColOrder, Rows, OutputFile);
    end;

  finally
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

initialization
  TExporterRegistry.RegisterExporter('csv', TCsvExporter.Create);
  TExporterRegistry.RegisterExporter('json', TJsonExporter.Create);
  TExporterRegistry.RegisterExporter('xml', TXmlExporter.Create);
  TExporterRegistry.RegisterExporter('xlsx', TXlsxExporter.Create);
  TExporterRegistry.RegisterExporter('ods', TOdsExporter.Create);
  TExporterRegistry.RegisterExporter('dbf', TDbfExporter.Create);

end.

