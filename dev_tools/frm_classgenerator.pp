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
    procedure GenerateClassAssign;
    procedure GenerateClassClone;
    procedure GenerateClassCopy; deprecated;
    procedure GenerateClassDelete; deprecated;
    procedure GenerateClassDiff;
    procedure GenerateClassEqualsTo;
    procedure GenerateClassFromJSON;
    procedure GenerateClassFind; deprecated;
    procedure GenerateClassGetData; deprecated;
    procedure GenerateClassInsert; deprecated;
    procedure GenerateClassLoadFromDataSet; deprecated;
    procedure GenerateClassSave; deprecated;
    procedure GenerateClassToJSON;
    procedure GenerateClassToString;
    procedure GenerateClassUpdate; deprecated;
    procedure GenerateClassValidate;

    procedure GenerateRepository;
    procedure GenerateRepositoryDelete;
    procedure GenerateRepositoryExists;
    procedure GenerateRepositoryFindBy;
    procedure GenerateRepositoryGetById;
    procedure GenerateRepositoryHydrate;
    procedure GenerateRepositoryInsert;
    procedure GenerateRepositoryTableName;
    procedure GenerateRepositoryUpdate;

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

  with synOutput, Lines do
  begin
    Add('unit models_' + AnsiLowerCase(eTableName.Text) + ';');
    Add('');
    Add('{$mode ObjFPC}{$H+}');
    Add('');
    Add('interface');
    Add('');
    Add('uses');
    Add('  Classes, SysUtils, Variants, TypInfo, DB, SQLDB, fpjson, DateUtils,');
    Add('  models_record_types;');
    Add('');

    GenerateClass;
    GenerateRepository;

    Add('implementation');
    Add('');
    Add('uses');
    Add('  utils_locale, models_users, utils_validations, data_consts, data_columns, data_setparam, data_getvalue, udm_main;');
    Add('');

    GenerateClassCreate;
    GenerateClassClear;
    GenerateClassAssign;
    GenerateClassClone;
    GenerateClassDiff;
    GenerateClassEqualsTo;
    GenerateClassFromJSON;
    GenerateClassToJSON;
    GenerateClassToString;
    GenerateClassValidate;

    GenerateRepositoryDelete;
    GenerateRepositoryExists;
    GenerateRepositoryFindBy;
    GenerateRepositoryGetById;
    GenerateRepositoryHydrate;
    GenerateRepositoryInsert;
    GenerateRepositoryTableName;
    GenerateRepositoryUpdate;

    Add('end.');
  end;
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
    Add('    constructor Create(aValue: Integer = 0); reintroduce; virtual;');
    Add('    procedure Clear; override;');
    Add('    procedure Assign(Source: TPersistent); override;');
    Add('    function Clone: TXolmisRecord; reintroduce;');
    Add('    function Diff(const aOld: ' + eClassName.Text + '; var Changes: TStrings): Boolean; virtual;');
    Add('    function EqualsTo(const Other: ' + eClassName.Text + '): Boolean;');
    Add('    procedure FromJSON(const aJSONString: String); virtual;');
    Add('    function ToJSON: String; virtual;');
    Add('    function ToString: String; override; ');
    Add('    function Validate(out Msg: string): Boolean; virtual;');
    Add('  published');
    for r := 1 to gridMap.RowCount - 1 do
      Add('    property ' + gridMap.Cells[1, r] + ': ' + gridMap.Cells[2, r] +
        ' read F' + gridMap.Cells[1, r] + ' write F' + gridMap.Cells[1, r] + ';');
    Add('  end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassAssign;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + '.Assign(Source: TPersistent);');
    Add('begin');
    Add('  inherited Assign(Source);');
    Add('  if Source is TInstitution then');
    Add('  begin');
    for r := 1 to gridMap.RowCount - 1 do
      Add('    F' + gridMap.Cells[1, r] + ' := ' + eClassName.Text + '(Source).' + gridMap.Cells[1, r] + ';');
    Add('  end;');
    Add('end;');
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

procedure TfrmClassGenerator.GenerateClassClone;
begin
  with synOutput, Lines do
  begin
    Add('function ' + eClassName.Text + '.Clone: TXolmisRecord;');
    Add('begin');
    Add('  Result := ' + eClassName.Text + '(inherited Clone);');
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
    Add('  inherited Create;');
    Add('  if aValue > 0 then');
    Add('    FId := aValue;');
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
    Add('function ' + eClassName.Text + '.Diff(const aOld: ' + eClassName.Text + '; var Changes: TStrings): Boolean;');
    Add('var');
    Add('  R: String;');
    Add('begin');
    Add('  Result := False;');
    Add('  R := EmptyStr;');
    Add('  if Assigned(Changes) then');
    Add('    Changes.Clear;');
    Add('  if aOld = nil then');
    Add('    Exit(False);');
    Add('');
    for r := 1 to gridMap.RowCount - 1 do
    begin
      Add('  if FieldValuesDiff(rsc' + gridMap.Cells[1, r] + ', aOld.' + gridMap.Cells[1, r] + ', F' + gridMap.Cells[1, r] + ', R) then');
      Add('    Changes.Add(R);');
    end;
    Add('');
    Add('  Result := Changes.Count > 0;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateClassEqualsTo;
begin
  with synOutput, Lines do
  begin
    Add('function ' + eClassName.Text + '.EqualsTo(const Other: ' + eClassName.Text + '): Boolean;');
    Add('begin');
    Add('  Result := Assigned(Other) and (FId = Other.Id);');
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

procedure TfrmClassGenerator.GenerateClassFromJSON;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + '.FromJSON(const aJSONString: String);');
    Add('var');
    Add('  Obj: TJSONObject;');
    Add('begin');
    Add('  Obj := TJSONObject(GetJSON(AJSONString));');
    Add('  try');
    for r := 1 to gridMap.RowCount - 1 do
      case gridMap.Cells[2, r] of
        'String':   Add('    F' + gridMap.Cells[1, r] + ' := Obj.Get(' + QuotedStr(gridMap.Cells[3, r]) + ', '''');');
        'Integer':  Add('    F' + gridMap.Cells[1, r] + ' := Obj.Get(' + QuotedStr(gridMap.Cells[3, r]) + ', 0);');
        'Double',
        'Extended': Add('    F' + gridMap.Cells[1, r] + ' := Obj.Get(' + QuotedStr(gridMap.Cells[3, r]) + ', 0.0);');
        'Date':     Add('    F' + gridMap.Cells[1, r] + ' := StrToDate(Obj.Get(' + QuotedStr(gridMap.Cells[3, r]) + ', NULL_DATE_STR));');
        'Time':     Add('    F' + gridMap.Cells[1, r] + ' := StrToTime(Obj.Get(' + QuotedStr(gridMap.Cells[3, r]) + ', NULL_TIME_STR));');
        'DateTime': Add('    F' + gridMap.Cells[1, r] + ' := StrToDateTime(Obj.Get(' + QuotedStr(gridMap.Cells[3, r]) + ', NULL_DATE_STR + '' '' + NULL_TIME_STR));');
        'Boolean':  Add('    F' + gridMap.Cells[1, r] + ' := Obj.Get(' + QuotedStr(gridMap.Cells[3, r]) + ', False);');
      end;
    Add('  finally');
    Add('    Obj.Free;');
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

procedure TfrmClassGenerator.GenerateClassToString;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('function ' + eClassName.Text + '.ToString: String;');
    Add('begin');
    Add('  Result := Format(''' + eClassName.Text + '(Id=%d, '' +');
    for r := 1 to gridMap.RowCount - 1 do
      case gridMap.Cells[2, r] of
        'String':   Add('    ''' + gridMap.Cells[1, r] + '=%s, '' +');
        'Integer':  Add('    ''' + gridMap.Cells[1, r] + '=%d, '' +');
        'Double',
        'Extended': Add('    ''' + gridMap.Cells[1, r] + '=%f, '' +');
        'Date':     Add('    ''' + gridMap.Cells[1, r] + '=%s, '' +');
        'Time':     Add('    ''' + gridMap.Cells[1, r] + '=%s, '' +');
        'DateTime': Add('    ''' + gridMap.Cells[1, r] + '=%s, '' +');
        'Boolean':  Add('    ''' + gridMap.Cells[1, r] + '=%s, '' +');
      end;
    Add('    ''InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)'',');
    Add('    [FId, ');
    for r := 1 to gridMap.RowCount - 1 do
      case gridMap.Cells[2, r] of
        'String':   Add('    F' + gridMap.Cells[1, r] + ', ');
        'Integer':  Add('    F' + gridMap.Cells[1, r] + ', ');
        'Double',
        'Extended': Add('    F' + gridMap.Cells[1, r] + ', ');
        'Date':     Add('    DateToStr(F' + gridMap.Cells[1, r] + '), ');
        'Time':     Add('    TimeToStr(F' + gridMap.Cells[1, r] + '), ');
        'DateTime': Add('    DateTimeToStr(F' + gridMap.Cells[1, r] + '), ');
        'Boolean':  Add('    BoolToStr(F' + gridMap.Cells[1, r] + ', ''True'', ''False''), ');
      end;
    Add('    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, ''True'', ''False''),');
    Add('    BoolToStr(FActive, ''True'', ''False'')]);');
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

procedure TfrmClassGenerator.GenerateClassValidate;
begin
  with synOutput, Lines do
  begin
    Add('function ' + eClassName.Text + '.Validate(out Msg: string): Boolean;');
    Add('begin');
    Add('  if FFullName = EmptyStr then');
    Add('  begin');
    Add('    Msg := ''FullName required.'';');
    Add('    Exit(False);');
    Add('  end;');
    Add('');
    Add('  Msg := '''';');
    Add('  Result := True;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateRepository;
begin
  with synOutput, Lines do
  begin
    Add('  ' + eClassName.Text + 'Repository = class(TXolmisRepository)');
    Add('  protected');
    Add('    function TableName: string; override;');
    Add('  public');
    Add('    function Exists(const Id: Integer): Boolean; override;');
    Add('    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;');
    Add('    procedure GetById(const Id: Integer; E: TXolmisRecord); override;');
    Add('    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;');
    Add('    procedure Insert(E: TXolmisRecord); override;');
    Add('    procedure Update(E: TXolmisRecord); override;');
    Add('    procedure Delete(E: TXolmisRecord); override;');
    Add('  end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateRepositoryDelete;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + 'Repository.Delete(E: TXolmisRecord);');
    Add('var');
    Add('  Qry: TSQLQuery;');
    Add('  R: ' + eClassName.Text + ';');
    Add('begin');
    Add('  if not (E is ' + eClassName.Text + ') then');
    Add('    raise Exception.Create(''Delete: Expected ' + eClassName.Text + ''');');
    Add('');
    Add('  R := ' + eClassName.Text + '(E);');
    Add('  if R.Id = 0 then');
    Add('    raise Exception.CreateFmt(''' + eClassName.Text + '.Delete: %s.'', [rsErrorEmptyId]);');
    Add('');
    Add('  Qry := NewQuery;');
    Add('  with Qry, SQL do');
    Add('  try');
    Add('    MacroCheck := True;');
    Add('');
    Add('    if not FTrans.Active then');
    Add('      FTrans.StartTransaction;');
    Add('    try');
    Add('      Clear;');
    Add('      Add(''DELETE FROM %tablename'');');
    Add('      Add(''WHERE (%idname = :aid)'');');
    Add('');
    Add('      MacroByName(''tablename'').Value := TableName;');
    Add('      MacroByName(''idname'').Value := COL_' + AnsiUpperCase(eKeyField.Text) + ';');
    Add('      ParamByName(''aid'').AsInteger := R.Id;');
    Add('');
    Add('      ExecSQL;');
    Add('');
    Add('      FTrans.CommitRetaining;');
    Add('    except');
    Add('      FTrans.RollbackRetaining;');
    Add('      raise;');
    Add('    end;');
    Add('  finally');
    Add('    FreeAndNil(Qry);');
    Add('  end;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateRepositoryExists;
begin
  with synOutput, Lines do
  begin
    Add('function ' + eClassName.Text + 'Repository.Exists(const Id: Integer): Boolean;');
    Add('var');
    Add('  Qry: TSQLQuery;');
    Add('begin');
    Add('  Qry := NewQuery;');
    Add('  with Qry do');
    Add('  try');
    Add('    MacroCheck := True;');
    Add('    SQL.Text := ''SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1'';');
    Add('    MacroByName(''tablename'').Value := TableName;');
    Add('    MacroByName(''idname'').Value := COL_' + AnsiUpperCase(eKeyField.Text) + ';');
    Add('    ParamByName(''id'').AsInteger := Id;');
    Add('    Open;');
    Add('    Result := not EOF;');
    Add('  finally');
    Add('    FreeAndNil(Qry);');
    Add('  end;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateRepositoryFindBy;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + 'Repository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);');
    Add('const');
    Add('  ALLOWED: array[0..2] of string = (COL_' + AnsiUpperCase(eKeyField.Text) + ', COL_FULL_NAME); // whitelist');
    Add('var');
    Add('  Qry: TSQLQuery;');
    Add('  I: Integer;');
    Add('  Ok: Boolean;');
    Add('begin');
    Add('  if not (E is ' + eClassName.Text + ') then');
    Add('    raise Exception.Create(''FindBy: Expected ' + eClassName.Text + '''');
    Add('');
    Add('  // Avoid FieldName injection: check in whitelist');
    Add('  Ok := False;');
    Add('  for I := Low(ALLOWED) to High(ALLOWED) do');
    Add('    if SameText(FieldName, ALLOWED[I]) then');
    Add('    begin');
    Add('      Ok := True;');
    Add('      Break;');
    Add('    end;');
    Add('  if not Ok then');
    Add('    raise Exception.CreateFmt(rsFieldNotAllowedInFindBy, [FieldName]);');
    Add('');
    Add('  Qry := NewQuery;');
    Add('  with Qry, SQL do');
    Add('  try');
    Add('    MacroCheck := True;');
    Add('');
    Add('    Add(''SELECT '' +');
    for r := 1 to gridMap.RowCount - 1 do
      Add('      ''' + gridMap.Cells[3, r] + ', '' +');
    Add('      ''user_inserted, '' +');
    Add('      ''user_updated, '' +');
    Add('      ''datetime(insert_date, ''localtime'') AS insert_date, '' +');
    Add('      ''datetime(update_date, ''localtime'') AS update_date, '' +');
    Add('      ''exported_status, '' +');
    Add('      ''marked_status, '' +');
    Add('      ''active_status '' +');
    Add('      ''FROM ' + eTableName.Text + ''');');
    Add('    Add(''WHERE %afield = :avalue'');');
    Add('    MacroByName(''afield'').Value := FieldName;');
    Add('    ParamByName(''avalue'').Value := Value;');
    Add('    Open;');
    Add('');
    Add('    if not EOF then');
    Add('    begin');
    Add('      Hydrate(Qry, ' + eClassName.Text + '(E));');
    Add('    end;');
    Add('');
    Add('    Close;');
    Add('  finally');
    Add('    Qry.Free;');
    Add('  end;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateRepositoryGetById;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + 'Repository.GetById(const Id: Integer; E: TXolmisRecord);');
    Add('var');
    Add('  Qry: TSQLQuery;');
    Add('begin');
    Add('  if not (E is ' + eClassName.Text + ') then');
    Add('    raise Exception.Create(''GetById: Expected ' + eClassName.Text + '''');
    Add('');
    Add('  Qry := NewQuery;');
    Add('  with Qry, SQL do');
    Add('  try');
    Add('    Clear;');
    Add('    Add(''SELECT '' +');
    for r := 1 to gridMap.RowCount - 1 do
      Add('      ''' + gridMap.Cells[3, r] + ', '' +');
    Add('      ''user_inserted, '' +');
    Add('      ''user_updated, '' +');
    Add('      ''datetime(insert_date, ''localtime'') AS insert_date, '' +');
    Add('      ''datetime(update_date, ''localtime'') AS update_date, '' +');
    Add('      ''exported_status, '' +');
    Add('      ''marked_status, '' +');
    Add('      ''active_status '' +');
    Add('      ''FROM ' + eTableName.Text + ''');');
    Add('    Add(''WHERE ' + eKeyField.Text + ' = :cod'');');
    Add('    ParamByName(''COD'').AsInteger := Id;');
    Add('    Open;');
    Add('    if not EOF then');
    Add('    begin');
    Add('      Hydrate(Qry, ' + eClassName.Text + '(E));');
    Add('    end;');
    Add('    Close;');
    Add('  finally');
    Add('    FreeAndNil(Qry);');
    Add('  end;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateRepositoryHydrate;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + 'Repository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);');
    Add('var');
    Add('  R: ' + eClassName.Text + ';');
    Add('begin');
    Add('  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then');
    Add('    Exit;');
    Add('  if not (E is ' + eClassName.Text + ') then');
    Add('    raise Exception.Create(''Hydrate: Expected ' + eClassName.Text + '''');
    Add('');
    Add('  R := ' + eClassName.Text + '(E);');
    Add('  with aDataSet do');
    Add('  try');
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

procedure TfrmClassGenerator.GenerateRepositoryInsert;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + 'Repository.Insert(E: TXolmisRecord);');
    Add('var');
    Add('  Qry: TSQLQuery;');
    Add('  R: ' + eClassName.Text + ';');
    Add('begin');
    Add('  if not (E is ' + eClassName.Text + ') then');
    Add('    raise Exception.Create(''Insert: Expected ' + eClassName.Text + '''');
    Add('');
    Add('  R := ' + eClassName.Text + '(E);');
    Add('  Qry := NewQuery;');
    Add('  with Qry, SQL do');
    Add('  try');
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
        'String':   Add('    SetStrParam(ParamByName(''' + gridMap.Cells[3, r] + '''), R.' + gridMap.Cells[1, r] + ');');
        'Integer':  Add('    SetIntParam(ParamByName(''' + gridMap.Cells[3, r] + '''), R.' + gridMap.Cells[1, r] + ');');
        'Double',
        'Extended': Add('    SetFloatParam(ParamByName(''' + gridMap.Cells[3, r] + '''), R.' + gridMap.Cells[1, r] + ');');
        'Date':     Add('    SetDateParam(ParamByName(''' + gridMap.Cells[3, r] + '''), R.' + gridMap.Cells[1, r] + ');');
        'Time':     Add('    SetTimeParam(ParamByName(''' + gridMap.Cells[3, r] + '''), R.' + gridMap.Cells[1, r] + ');');
        'DateTime': Add('    ParamByName(''' + gridMap.Cells[3, r] + ''') := R.' + gridMap.Cells[1, r] + ');');
        'Boolean':  Add('    ParamByName(''' + gridMap.Cells[3, r] + ''') := R.' + gridMap.Cells[1, r] + ');');
      end;
    Add('    ParamByName(''user_inserted'').AsInteger := ActiveUser.Id;');
    Add('');
    Add('    ExecSQL;');
    Add('');
    Add('    // Get the autoincrement key inserted');
    Add('    Clear;');
    Add('    Add(''SELECT last_insert_rowid()'');');
    Add('    Open;');
    Add('    R.Id := Fields[0].AsInteger;');
    Add('    Close;');
    Add('  finally');
    Add('    FreeAndNil(Qry);');
    Add('  end;');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateRepositoryTableName;
begin
  with synOutput, Lines do
  begin
    Add('function ' + eClassName.Text + 'Repository.TableName: String;');
    Add('begin');
    Add('  Result := TBL_' + AnsiUpperCase(eTableName.Text) + ';');
    Add('end;');
    Add('');
  end;
end;

procedure TfrmClassGenerator.GenerateRepositoryUpdate;
var
  r: Integer;
begin
  with synOutput, Lines do
  begin
    Add('procedure ' + eClassName.Text + 'Repository.Update(E: TXolmisRecord);');
    Add('var');
    Add('  Qry: TSQLQuery;');
    Add('  R: ' + eClassName.Text + ';');
    Add('begin');
    Add('  if not (E is ' + eClassName.Text + ') then');
    Add('    raise Exception.Create(''Update: Expected ' + eClassName.Text + '''');
    Add('');
    Add('  R := ' + eClassName.Text + '(E);');
    Add('  if R.Id = 0 then');
    Add('    raise Exception.CreateFmt(''' + eClassName.Text + '.Update: %s.'', [rsErrorEmptyId]);');
    Add('');
    Add('  Qry := NewQuery;');
    Add('  with Qry, SQL do');
    Add('  try');
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
        'String':   Add('    SetStrParam(ParamByName(''' + gridMap.Cells[3, r] + '''), R.' + gridMap.Cells[1, r] + ');');
        'Integer':  Add('    SetIntParam(ParamByName(''' + gridMap.Cells[3, r] + '''), R.' + gridMap.Cells[1, r] + ');');
        'Double',
        'Extended': Add('    SetFloatParam(ParamByName(''' + gridMap.Cells[3, r] + '''), R.' + gridMap.Cells[1, r] + ');');
        'Date':     Add('    SetDateParam(ParamByName(''' + gridMap.Cells[3, r] + '''), R.' + gridMap.Cells[1, r] + ');');
        'Time':     Add('    SetTimeParam(ParamByName(''' + gridMap.Cells[3, r] + '''), R.' + gridMap.Cells[1, r] + ');');
        'DateTime': Add('    ParamByName(''' + gridMap.Cells[3, r] + ''') := R.' + gridMap.Cells[1, r] + ');');
        'Boolean':  Add('    ParamByName(''' + gridMap.Cells[3, r] + ''') := R.' + gridMap.Cells[1, r] + ');');
      end;
    Add('    ParamByName(''user_updated'').AsInteger := ActiveUser.Id;');
    Add('    ParamByName(''' + eKeyField.Text + ''').AsInteger := R.Id;');
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

