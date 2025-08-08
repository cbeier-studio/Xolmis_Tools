unit frm_classgenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, ExtCtrls, Grids, StdCtrls, SynEdit, SynHighlighterPas,
  SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TfrmClassGenerator }

  TfrmClassGenerator = class(TForm)
    btnGenerate: TBitBtn;
    btnClose: TBitBtn;
    eClassName: TEdit;
    eTableName: TEdit;
    eKeyField: TEdit;
    iButtons: TImageList;
    lblTableName: TLabel;
    lblClassName: TLabel;
    lblKeyField: TLabel;
    pNames: TPanel;
    gridMap: TStringGrid;
    splitMapCode: TSplitter;
    synOutput: TSynEdit;
    hlPasSyn: TSynPasSyn;
    procedure btnCloseClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure eClassNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gridMapEditingDone(Sender: TObject);
  private
    procedure GenerateClass;
    procedure GenerateClassCreate;
    procedure GenerateClassClear;
    procedure GenerateClassCopy;
    procedure GenerateClassDelete;
    procedure GenerateClassDiff;
    procedure GenerateClassFind;
    procedure GenerateClassGetData;
    procedure GenerateClassInsert;
    procedure GenerateClassLoadFromDataSet;
    procedure GenerateClassSave;
    procedure GenerateClassToJSON;
    procedure GenerateClassUpdate;

    function IsRequiredFilled: Boolean;
  public

  end;

var
  frmClassGenerator: TfrmClassGenerator;

implementation

{$R *.lfm}

{ TfrmClassGenerator }

procedure TfrmClassGenerator.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmClassGenerator.btnGenerateClick(Sender: TObject);
begin
  synOutput.Lines.Clear;

  GenerateClass;
  GenerateClassCreate;
  GenerateClassClear;
  GenerateClassCopy;
  GenerateClassDelete;
  GenerateClassDiff;
  GenerateClassFind;
  GenerateClassGetData;
  GenerateClassInsert;
  GenerateClassLoadFromDataSet;
  GenerateClassSave;
  GenerateClassToJSON;
  GenerateClassUpdate;
end;

procedure TfrmClassGenerator.eClassNameChange(Sender: TObject);
begin
  btnGenerate.Enabled := IsRequiredFilled;
end;

procedure TfrmClassGenerator.FormCreate(Sender: TObject);
begin
  synOutput.Font.Quality := fqDefault;
end;

procedure TfrmClassGenerator.GenerateClass;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('  ' + eClassName.Text + ' = class(TXolmisRecord)');
    Add('  protected');
    for r := 1 to gridMap.RowCount - 1 do
      Add('    F' + gridMap.Cells[1, r] + ': ' + gridMap.Cells[2, r] + ';');
    Add('  public');
    Add('    constructor Create(aValue: Integer = 0);');
    Add('    procedure Clear; override;');
    Add('    procedure Copy(aFrom: ' + eClassName.Text + ');');
    Add('    procedure Delete;');
    Add('    function Diff(aOld: ' + eClassName.Text + '; var aList: TStrings): Boolean;');
    Add('    function Find(const FieldName: String; const Value: Variant): Boolean;');
    Add('    procedure GetData(aKey: Integer);');
    Add('    procedure Insert;');
    Add('    procedure LoadFromDataSet(aDataSet: TDataSet);');
    Add('    procedure Save;');
    Add('    function ToJSON: String;');
    Add('    procedure Update;');
    Add('  published');
    for r := 1 to gridMap.RowCount - 1 do
      Add('    property ' + gridMap.Cells[1, r] + ': ' + gridMap.Cells[2, r] +
        ' read F' + gridMap.Cells[1, r] + ' write F' + gridMap.Cells[1, r] + ';');
    Add('  end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassClear;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + '.Clear;');
    Add('begin');
    Add('  inherited Clear;');
    for r := 1 to gridMap.RowCount - 1 do
      case gridMap.Cells[2, r] of
        'String':   Add('  F' + gridMap.Cells[1, r] + ' := EmptyStr;');
        'Integer':  Add('  F' + gridMap.Cells[1, r] + ' := 0;');
        'Double',
        'Extended': Add('  F' + gridMap.Cells[1, r] + ' := 0.0;');
        'Date':     Add('  F' + gridMap.Cells[1, r] + ' := NullDate;');
        'Time':     Add('  F' + gridMap.Cells[1, r] + ' := NullTime;');
        'DateTime': Add('  F' + gridMap.Cells[1, r] + ' := NullDateTime;');
        'Boolean':  Add('  F' + gridMap.Cells[1, r] + ' := False;');
      end;
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassCopy;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + '.Copy(aFrom: ' + eClassName.Text + ');');
    Add('begin');
    for r := 1 to gridMap.RowCount - 1 do
      Add('  F' + gridMap.Cells[1, r] + ' := aFrom.' + gridMap.Cells[1, r] + ';');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassCreate;
begin
  with synOutput, Lines do
  begin
    Add('constructor ' + eClassName.Text + '.Create(aValue: Integer);');
    Add('begin');
    Add('  if aValue > 0 then');
    Add('    GetData(aValue)');
    Add('  else');
    Add('    Clear;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassDelete;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + '.Delete;');
    Add('var');
    Add('  Qry: TSQLQuery;');
    Add('begin');
    Add('  if FId = 0 then');
    Add('    raise Exception.CreateFmt(''' + eClassName.Text + '.Delete: %s.'', [rsErrorEmptyId]);');
    Add('');
    Add('  Qry := TSQLQuery.Create(DMM.sqlCon);');
    Add('  with Qry, SQL do');
    Add('  try');
    Add('    DataBase := DMM.sqlCon;');
    Add('    Transaction := DMM.sqlTrans;');
    Add('');
    Add('    if not DMM.sqlTrans.Active then');
    Add('      DMM.sqlTrans.StartTransaction;');
    Add('    try');
    Add('      Clear;');
    Add('      Add(''DELETE FROM ' + eTableName.Text + ''');');
    Add('      Add(''WHERE (' + eKeyField.Text + ' = :aid)'');');
    Add('');
    Add('      ParamByName(''aid'').AsInteger := FId;');
    Add('');
    Add('      ExecSQL;');
    Add('');
    Add('      DMM.sqlTrans.CommitRetaining;');
    Add('    except');
    Add('      DMM.sqlTrans.RollbackRetaining;');
    Add('      raise;');
    Add('    end;');
    Add('  finally');
    Add('    FreeAndNil(Qry);');
    Add('  end;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassDiff;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('function ' + eClassName.Text + '.Diff(aOld: ' + eClassName.Text + '; var aList: TStrings): Boolean;');
    Add('var');
    Add('  R: String;');
    Add('begin');
    Add('  Result := False;');
    Add('  R := EmptyStr;');
    Add('');
    for r := 1 to gridMap.RowCount - 1 do
    begin
      Add('  if FieldValuesDiff(rsc' + gridMap.Cells[1, r] + ', aOld.' + gridMap.Cells[1, r] + ', F' + gridMap.Cells[1, r] + ', R) then');
      Add('    aList.Add(R);');
    end;
    Add('');
    Add('  Result := aList.Count > 0;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassFind;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('function ' + eClassName.Text + '.Find(const FieldName: String; const Value: Variant): Boolean;');
    Add('var');
    Add('  Qry: TSQLQuery;');
    Add('begin');
    Add('  Result := False;');
    Add('');
    Add('  Qry := TSQLQuery.Create(nil);');
    Add('  with Qry, SQL do');
    Add('  try');
    Add('    SQLConnection := DMM.sqlCon;');
    Add('    SQLTransaction := DMM.sqlTrans;');
    Add('    MacroCheck := True;');
    Add('');
    Add('    Add(''SELECT '' +');
    Add('        ''' + eKeyField.Text + ', '' +');
    for r := 1 to gridMap.RowCount - 1 do
      Add('        ''' + gridMap.Cells[3, r] + ', '' +');
    Add('        ''user_inserted, '' +');
    Add('        ''user_updated, '' +');
    Add('        ''datetime(insert_date, ''''localtime'''') AS insert_date, '' +');
    Add('        ''datetime(update_date, ''''localtime'''') AS update_date, '' +');
    Add('        ''exported_status, '' +');
    Add('        ''marked_status, '' +');
    Add('        ''active_status '' +');
    Add('      ''FROM ' + eTableName.Text + ''');');
    Add('    Add(''WHERE %afield = :avalue'');');
    Add('    MacroByName(''afield'').Value := FieldName;');
    Add('    ParamByName(''avalue'').Value := Value;');
    Add('    Open;');
    Add('    if not EOF then');
    Add('    begin');
    Add('      LoadFromDataSet(Qry);');
    Add('');
    Add('      Result := True;');
    Add('    end;');
    Add('    Close;');
    Add('  finally');
    Add('    Qry.Free;');
    Add('  end;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassGetData;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + '.GetData(aKey: Integer);');
    Add('var');
    Add('  Qry: TSQLQuery;');
    Add('begin');
    Add('  Qry := TSQLQuery.Create(DMM.sqlCon);');
    Add('  with Qry, SQL do');
    Add('  try');
    Add('    DataBase := DMM.sqlCon;');
    Add('    Clear;');
    Add('    Add(''SELECT '' +');
    Add('      ''' + eKeyField.Text + ', '' +');
    for r := 1 to gridMap.RowCount - 1 do
      Add('      ''' + gridMap.Cells[3, r] + ', '' +');
    Add('      ''user_inserted, '' +');
    Add('      ''user_updated, '' +');
    Add('      ''datetime(insert_date, ''''localtime'''') AS insert_date, '' +');
    Add('      ''datetime(update_date, ''''localtime'''') AS update_date, '' +');
    Add('      ''exported_status, '' +');
    Add('      ''marked_status, '' +');
    Add('      ''active_status '' +');
    Add('      ''FROM ' + eTableName.Text + ''');');
    Add('    Add(''WHERE ' + eKeyField.Text + ' = :cod'');');
    Add('    ParamByName(''COD'').AsInteger := aKey;');
    Add('    Open;');
    Add('    if RecordCount > 0 then');
    Add('      LoadFromDataSet(Qry);');
    Add('    Close;');
    Add('  finally');
    Add('    FreeAndNil(Qry);');
    Add('  end;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassInsert;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + '.Insert;');
    Add('var');
    Add('  Qry: TSQLQuery;');
    Add('begin');
    Add('  Qry := TSQLQuery.Create(DMM.sqlCon);');
    Add('  with Qry, SQL do');
    Add('  try');
    Add('    Database := DMM.sqlCon;');
    Add('    Transaction := DMM.sqlTrans;');
    Add('');
    Add('    Clear;');
    Add('    Add(''INSERT INTO ' + eTableName.Text + ' ('' +');
    for r := 1 to gridMap.RowCount - 1 do
      Add('      ''' + gridMap.Cells[3, r] + ', '' +');
    Add('      ''user_inserted, '' +');
    Add('      ''insert_date)'');');
    Add('    Add(''VALUES ('' +');
    for r := 1 to gridMap.RowCount - 1 do
      case gridMap.Cells[2, r] of
        'Date':     Add('      ''date(:' + gridMap.Cells[3, r] + '), '' +');
        'Time':     Add('      ''time(:' + gridMap.Cells[3, r] + '), '' +');
        'DateTime': Add('      ''datetime(:' + gridMap.Cells[3, r] + '), '' +');
      else
        Add('      '':' + gridMap.Cells[3, r] + ', '' +');
      end;
    Add('      '':user_inserted, '' +');
    Add('      ''datetime(''''now'''',''''subsec''''))'');');
    Add('');
    for r := 1 to gridMap.RowCount - 1 do
      case gridMap.Cells[2, r] of
        'String':   Add('    SetStrParam(ParamByName(''' + gridMap.Cells[3, r] + '''), F' + gridMap.Cells[1, r] + ');');
        'Integer':  Add('    SetIntParam(ParamByName(''' + gridMap.Cells[3, r] + '''), F' + gridMap.Cells[1, r] + ');');
        'Double',
        'Extended': Add('    SetFloatParam(ParamByName(''' + gridMap.Cells[3, r] + '''), F' + gridMap.Cells[1, r] + ');');
        'Date':     Add('    SetDateParam(ParamByName(''' + gridMap.Cells[3, r] + '''), F' + gridMap.Cells[1, r] + ');');
        'Time':     Add('    SetTimeParam(ParamByName(''' + gridMap.Cells[3, r] + '''), F' + gridMap.Cells[1, r] + ');');
        'DateTime': Add('    ParamByName(''' + gridMap.Cells[3, r] + ''') := F' + gridMap.Cells[1, r] + ');');
        'Boolean':  Add('    ParamByName(''' + gridMap.Cells[3, r] + ''') := F' + gridMap.Cells[1, r] + ');');
      end;
    Add('    ParamByName(''user_inserted'').AsInteger := ActiveUser.Id;');
    Add('');
    Add('    ExecSQL;');
    Add('');
    Add('    // Get the autoincrement key inserted');
    Add('    Clear;');
    Add('    Add(''SELECT last_insert_rowid()'');');
    Add('    Open;');
    Add('    FId := Fields[0].AsInteger;');
    Add('    Close;');
    Add('  finally');
    Add('    FreeAndNil(Qry);');
    Add('  end;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassLoadFromDataSet;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + '.LoadFromDataSet(aDataSet: TDataSet);');
    Add('begin');
    Add('if not aDataSet.Active then');
    Add('    Exit;');
    Add('');
    Add('  with aDataSet do');
    Add('  begin');
    Add('    FId := FieldByName(''' + eKeyField.Text + ''').AsInteger;');
    for r := 1 to gridMap.RowCount - 1 do
      case gridMap.Cells[2, r] of
        'String':   Add('    F' + gridMap.Cells[1, r] + ' := FieldByName(''' + gridMap.Cells[3, r] + ''').AsString;');
        'Integer':  Add('    F' + gridMap.Cells[1, r] + ' := FieldByName(''' + gridMap.Cells[3, r] + ''').AsInteger;');
        'Double',
        'Extended': Add('    F' + gridMap.Cells[1, r] + ' := FieldByName(''' + gridMap.Cells[3, r] + ''').AsFloat;');
        'Date',
        'DateTime':
        begin
          Add('    if not FieldByName(''' + gridMap.Cells[3, r] + ''').IsNull then');
          Add('      F' + gridMap.Cells[1, r] + ' := FieldByName(''' + gridMap.Cells[3, r] + ''').AsDateTime');
          Add('    else');
          Add('      F' + gridMap.Cells[1, r] + ' := NullDate;');
        end;
        'Time':
        begin
          Add('    if not FieldByName(''' + gridMap.Cells[3, r] + ''').IsNull then');
          Add('      F' + gridMap.Cells[1, r] + ' := FieldByName(''' + gridMap.Cells[3, r] + ''').AsDateTime');
          Add('    else');
          Add('      F' + gridMap.Cells[1, r] + ' := NullTime;');
        end;
        'Boolean':  Add('    F' + gridMap.Cells[1, r] + ' := FieldByName(''' + gridMap.Cells[3, r] + ''').AsBoolean;');
      end;
    Add('    // SQLite may store date and time data as ISO8601 string or Julian date real formats');
    Add('    // so it checks in which format it is stored before load the value');
    Add('    GetTimeStamp(FieldByName(''insert_date'').AsString, FInsertDate);');
    Add('    GetTimeStamp(FieldByName(''update_date'').AsString, FUpdateDate);');
    Add('    FUserInserted := FieldByName(''user_inserted'').AsInteger;');
    Add('    FUserUpdated := FieldByName(''user_updated'').AsInteger;');
    Add('    FExported := FieldByName(''exported_status'').AsBoolean;');
    Add('    FMarked := FieldByName(''marked_status'').AsBoolean;');
    Add('    FActive := FieldByName(''active_status'').AsBoolean;');
    Add('  end;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassSave;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + '.Save;');
    Add('begin');
    Add('  if FId = 0 then');
    Add('    Insert');
    Add('  else');
    Add('    Update;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassToJSON;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('function ' + eClassName.Text + '.ToJSON: String;');
    Add('var');
    Add('  JSONObject: TJSONObject;');
    Add('begin');
    Add('  JSONObject := TJSONObject.Create;');
    Add('  try');
    for r := 1 to gridMap.RowCount - 1 do
      Add('    JSONObject.Add(''' + gridMap.Cells[3, r] + ''', F' + gridMap.Cells[1, r] + ');');
    Add('');
    Add('    Result := JSONObject.AsJSON;');
    Add('  finally');
    Add('    JSONObject.Free;');
    Add('  end;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassUpdate;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + '.Update;');
    Add('var');
    Add('  Qry: TSQLQuery;');
    Add('begin');
    Add('  if FId = 0 then');
    Add('    raise Exception.CreateFmt(''' + eClassName.Text + '''.Update: %s.'', [rsErrorEmptyId]);');
    Add('');
    Add('  Qry := TSQLQuery.Create(DMM.sqlCon);');
    Add('  with Qry, SQL do');
    Add('  try');
    Add('    Database := DMM.sqlCon;');
    Add('    Transaction := DMM.sqlTrans;');
    Add('');
    Add('    Clear;');
    Add('    Add(''UPDATE ' + eTableName.Text + ' SET '' +');
    for r := 1 to gridMap.RowCount - 1 do
      case gridMap.Cells[2, r] of
        'Date':     Add('      ''' + gridMap.Cells[3, r] + ' = date(:' + gridMap.Cells[3, r] + '), '' +');
        'Time':     Add('      ''' + gridMap.Cells[3, r] + ' = time(:' + gridMap.Cells[3, r] + '), '' +');
        'DateTime': Add('      ''' + gridMap.Cells[3, r] + ' = datetime(:' + gridMap.Cells[3, r] + '), '' +');
      else
        Add('      ''' + gridMap.Cells[3, r] + ' = :' + gridMap.Cells[3, r] + ', '' +');
      end;
    Add('      ''user_updated = :user_updated, '' +');
    Add('      ''update_date = datetime(''''now'''',''''subsec'''')'');');
    Add('    Add(''WHERE (' + eKeyField.Text + ' = :' + eKeyField.Text + ')'');');
    Add('');
    for r := 1 to gridMap.RowCount - 1 do
      case gridMap.Cells[2, r] of
        'String':   Add('    SetStrParam(ParamByName(''' + gridMap.Cells[3, r] + '''), F' + gridMap.Cells[1, r] + ');');
        'Integer':  Add('    SetIntParam(ParamByName(''' + gridMap.Cells[3, r] + '''), F' + gridMap.Cells[1, r] + ');');
        'Double',
        'Extended': Add('    SetFloatParam(ParamByName(''' + gridMap.Cells[3, r] + '''), F' + gridMap.Cells[1, r] + ');');
        'Date':     Add('    SetDateParam(ParamByName(''' + gridMap.Cells[3, r] + '''), F' + gridMap.Cells[1, r] + ');');
        'Time':     Add('    SetTimeParam(ParamByName(''' + gridMap.Cells[3, r] + '''), F' + gridMap.Cells[1, r] + ');');
        'DateTime': Add('    ParamByName(''' + gridMap.Cells[3, r] + ''') := F' + gridMap.Cells[1, r] + ');');
        'Boolean':  Add('    ParamByName(''' + gridMap.Cells[3, r] + ''') := F' + gridMap.Cells[1, r] + ');');
      end;
    Add('    ParamByName(''user_updated'').AsInteger := ActiveUser.Id;');
    Add('    ParamByName(''' + eKeyField.Text + ''').AsInteger := FId;');
    Add('');
    Add('    ExecSQL;');
    Add('  finally');
    Add('    FreeAndNil(Qry);');
    Add('  end;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.gridMapEditingDone(Sender: TObject);
begin
  btnGenerate.Enabled := IsRequiredFilled;
end;

function TfrmClassGenerator.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eClassName.Text <> EmptyStr) and
    (eTableName.Text <> EmptyStr) and
    (eKeyField.Text <> EmptyStr) and
    (gridMap.Cells[1, 1] <> EmptyStr) and
    (gridMap.Cells[2, 1] <> EmptyStr) and
    (gridMap.Cells[3, 1] <> EmptyStr) then
    Result := True;
end;

end.

