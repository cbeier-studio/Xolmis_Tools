unit models_method;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, fpjson, models_base;

type

  { TMethod }

  TMethod = class(TXolmisRecord)
  protected
    FName: String;
    FAbbreviation: String;
    FEbirdName: String;
    FCategory: String;
    FDescription: String;
    FRecommendedUses: String;
    FNotes: String;
    FCanDelete: Boolean;
  public
    constructor Create(aValue: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TMethod; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TMethod): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property Name: String read FName write FName;
    property Abbreviation: String read FAbbreviation write FAbbreviation;
    property EbirdName: String read FEbirdName write FEbirdName;
    property Category: String read FCategory write FCategory;
    property Description: String read FDescription write FDescription;
    property RecommendedUses: String read FRecommendedUses write FRecommendedUses;
    property Notes: String read FNotes write FNotes;
    property CanDelete: Boolean read FCanDelete write FCanDelete;
  end;

  { TMethodRepository }

  TMethodRepository = class(TXolmisRepository)
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

  { TMethodI18n }

  TMethodI18n = class(TXolmisRecord)
  protected
    FMethodId: Integer;
    FLanguageId: Integer;
    FMethodName: String;
    FCategory: String;
    FDescription: String;
    FRecommendedUses: String;
    FNotes: String;
  public
    constructor Create(aMethodId: Integer = 0; aLanguageId: Integer = 0); reintroduce; virtual;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; reintroduce;
    function Diff(const aOld: TMethodI18n; var Changes: TStrings): Boolean; virtual;
    function EqualsTo(const Other: TMethodI18n): Boolean;
    procedure FromJSON(const aJSONString: String); virtual;
    function ToJSON: String;
    function ToString: String; override;
    function Validate(out Msg: string): Boolean; virtual;
  published
    property MethodId: Integer read FMethodId write FMethodId;
    property LanguageId: Integer read FLanguageId write FLanguageId;
    property MethodName: String read FMethodName write FMethodName;
    property Category: String read FCategory write FCategory;
    property Description: String read FDescription write FDescription;
    property RecommendedUses: String read FRecommendedUses write FRecommendedUses;
    property Notes: String read FNotes write FNotes;
  end;

  { TMethodI18nRepository }

  TMethodI18nRepository = class(TXolmisRepository)
  protected
    function TableName: string; override;
  public
    function Exists(const Id: Integer): Boolean; override;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); override;
    procedure GetById(const Id: Integer; E: TXolmisRecord); override;
    procedure GetByMethodAndLanguage(const aMethodId, aLanguageId: Integer; E: TMethodI18n);
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); override;
    procedure Insert(E: TXolmisRecord); override;
    procedure Update(E: TXolmisRecord); override;
    procedure Delete(E: TXolmisRecord); override;
  end;

implementation

uses
  data_getvalue, data_setparam, udm_taxa;

{ TMethod }

constructor TMethod.Create(aValue: Integer);
begin
  inherited Create;
  if aValue <> 0 then
    FId := aValue;
end;

procedure TMethod.Clear;
begin
  inherited Clear;
  FName := EmptyStr;
  FAbbreviation := EmptyStr;
  FEbirdName := EmptyStr;
  FCategory := EmptyStr;
  FDescription := EmptyStr;
  FRecommendedUses := EmptyStr;
  FNotes := EmptyStr;
  FCanDelete := True;
end;

procedure TMethod.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TMethod then
  begin
    FName := TMethod(Source).Name;
    FAbbreviation := TMethod(Source).Abbreviation;
    FEbirdName := TMethod(Source).EbirdName;
    FCategory := TMethod(Source).Category;
    FDescription := TMethod(Source).Description;
    FRecommendedUses := TMethod(Source).RecommendedUses;
    FNotes := TMethod(Source).Notes;
    FCanDelete := TMethod(Source).CanDelete;
  end;
end;

function TMethod.Clone: TXolmisRecord;
begin
  Result := TMethod(inherited Clone);
end;

function TMethod.Diff(const aOld: TMethod; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if not SameText(aOld.Name, FName) then
  begin
    R := Format('Name: "%s" -> "%s"', [aOld.Name, FName]);
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  if not SameText(aOld.Abbreviation, FAbbreviation) then
  begin
    R := Format('Abbreviation: "%s" -> "%s"', [aOld.Abbreviation, FAbbreviation]);
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  if not SameText(aOld.EbirdName, FEbirdName) then
  begin
    R := Format('EbirdName: "%s" -> "%s"', [aOld.EbirdName, FEbirdName]);
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  if not SameText(aOld.Category, FCategory) then
  begin
    R := Format('Category: "%s" -> "%s"', [aOld.Category, FCategory]);
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  if not SameText(aOld.Description, FDescription) then
  begin
    R := 'Description changed.';
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  if not SameText(aOld.RecommendedUses, FRecommendedUses) then
  begin
    R := 'RecommendedUses changed.';
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  if not SameText(aOld.Notes, FNotes) then
  begin
    R := 'Notes changed.';
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  if aOld.CanDelete <> FCanDelete then
  begin
    R := Format('CanDelete: %s -> %s', [BoolToStr(aOld.CanDelete, 'True', 'False'), BoolToStr(FCanDelete, 'True', 'False')]);
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  Result := Assigned(Changes) and (Changes.Count > 0);
end;

function TMethod.EqualsTo(const Other: TMethod): Boolean;
begin
  Result := Assigned(Other) and (FId = Other.Id);
end;

procedure TMethod.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(aJSONString));
  try
    FId := Obj.Get('method_id', 0);
    FName := Obj.Get('method_name', '');
    FAbbreviation := Obj.Get('abbreviation', '');
    FEbirdName := Obj.Get('ebird_name', '');
    FCategory := Obj.Get('category', '');
    FDescription := Obj.Get('description', '');
    FRecommendedUses := Obj.Get('recommended_uses', '');
    FNotes := Obj.Get('notes', '');
    FCanDelete := Obj.Get('can_delete', True);
  finally
    Obj.Free;
  end;
end;

function TMethod.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('method_id', FId);
    JSONObject.Add('method_name', FName);
    JSONObject.Add('abbreviation', FAbbreviation);
    JSONObject.Add('ebird_name', FEbirdName);
    JSONObject.Add('category', FCategory);
    JSONObject.Add('description', FDescription);
    JSONObject.Add('recommended_uses', FRecommendedUses);
    JSONObject.Add('notes', FNotes);
    JSONObject.Add('can_delete', FCanDelete);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TMethod.ToString: String;
begin
  Result := Format('Method(Id=%d, Name=%s, Abbreviation=%s, EbirdName=%s, Category=%s, CanDelete=%s, InsertDate=%s, UpdateDate=%s, Marked=%s, Active=%s)',
    [FId, FName, FAbbreviation, FEbirdName, FCategory, BoolToStr(FCanDelete, 'True', 'False'),
    DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate), BoolToStr(FMarked, 'True', 'False'),
    BoolToStr(FActive, 'True', 'False')]);
end;

function TMethod.Validate(out Msg: string): Boolean;
begin
  if FName = EmptyStr then
  begin
    Msg := 'Name required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TMethodRepository }

function TMethodRepository.TableName: string;
begin
  Result := 'methods';
end;

function TMethodRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE %idname=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := 'method_id';
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..4] of string = ('method_id', 'method_name', 'abbreviation', 'ebird_name', 'category');
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TMethod) then
    raise Exception.Create('FindBy: Expected TMethod');

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
      Hydrate(Qry, TMethod(E));

    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TMethod) then
    raise Exception.Create('GetById: Expected TMethod');

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Clear;
    Add('SELECT * FROM %tablename');
    Add('WHERE %idname = :id');
    MacroByName('tablename').Value := TableName;
    MacroByName('idname').Value := 'method_id';
    ParamByName('id').AsInteger := Id;
    Open;
    if not EOF then
      Hydrate(Qry, TMethod(E));
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  M: TMethod;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TMethod) then
    raise Exception.Create('Hydrate: Expected TMethod');

  M := TMethod(E);
  with aDataSet do
  begin
    M.Id := FieldByName('method_id').AsInteger;
    M.Name := FieldByName('method_name').AsString;
    M.Abbreviation := FieldByName('abbreviation').AsString;
    M.EbirdName := FieldByName('ebird_name').AsString;
    M.Category := FieldByName('category').AsString;
    M.Description := FieldByName('description').AsString;
    M.RecommendedUses := FieldByName('recommended_uses').AsString;
    M.Notes := FieldByName('notes').AsString;
    M.CanDelete := FieldByName('can_delete').AsBoolean;
    GetTimeStamp(FieldByName('insert_date'), M.InsertDate);
    GetTimeStamp(FieldByName('update_date'), M.UpdateDate);
    M.Marked := FieldByName('marked_status').AsBoolean;
    M.Active := FieldByName('active_status').AsBoolean;
  end;
end;

procedure TMethodRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  M: TMethod;
begin
  if not (E is TMethod) then
    raise Exception.Create('Insert: Expected TMethod');

  M := TMethod(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO methods (' +
      'method_name, ' +
      'abbreviation, ' +
      'ebird_name, ' +
      'category, ' +
      'description, ' +
      'recommended_uses, ' +
      'notes, ' +
      'can_delete, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':method_name, ' +
      ':abbreviation, ' +
      ':ebird_name, ' +
      ':category, ' +
      ':description, ' +
      ':recommended_uses, ' +
      ':notes, ' +
      ':can_delete, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('method_name').AsString := M.Name;
    ParamByName('abbreviation').AsString := M.Abbreviation;
    ParamByName('ebird_name').AsString := M.EbirdName;
    ParamByName('category').AsString := M.Category;
    ParamByName('description').AsString := M.Description;
    ParamByName('recommended_uses').AsString := M.RecommendedUses;
    ParamByName('notes').AsString := M.Notes;
    ParamByName('can_delete').AsBoolean := M.CanDelete;

    ExecSQL;

    Clear;
    Add('SELECT last_insert_rowid()');
    Open;
    M.Id := Fields[0].AsInteger;
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  M: TMethod;
begin
  if not (E is TMethod) then
    raise Exception.Create('Update: Expected TMethod');

  M := TMethod(E);
  if M.Id = 0 then
    raise Exception.CreateFmt('TMethodRepository.Update: %s.', ['ID is empty']);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE methods SET ' +
      'method_name = :method_name, ' +
      'abbreviation = :abbreviation, ' +
      'ebird_name = :ebird_name, ' +
      'category = :category, ' +
      'description = :description, ' +
      'recommended_uses = :recommended_uses, ' +
      'notes = :notes, ' +
      'can_delete = :can_delete, ' +
      'update_date = datetime(''now'', ''subsec''), ' +
      'marked_status = :marked_status, ' +
      'active_status = :active_status');
    Add('WHERE (method_id = :method_id)');

    ParamByName('method_name').AsString := M.Name;
    ParamByName('abbreviation').AsString := M.Abbreviation;
    ParamByName('ebird_name').AsString := M.EbirdName;
    ParamByName('category').AsString := M.Category;
    ParamByName('description').AsString := M.Description;
    ParamByName('recommended_uses').AsString := M.RecommendedUses;
    ParamByName('notes').AsString := M.Notes;
    ParamByName('can_delete').AsBoolean := M.CanDelete;
    ParamByName('marked_status').AsBoolean := M.Marked;
    ParamByName('active_status').AsBoolean := M.Active;
    ParamByName('method_id').AsInteger := M.Id;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  M: TMethod;
begin
  if not (E is TMethod) then
    raise Exception.Create('Delete: Expected TMethod');

  M := TMethod(E);
  if M.Id = 0 then
    raise Exception.CreateFmt('TMethodRepository.Delete: %s.', ['ID is empty']);

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
      MacroByName('idname').Value := 'method_id';
      ParamByName('aid').AsInteger := M.Id;

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

{ TMethodI18n }

constructor TMethodI18n.Create(aMethodId: Integer; aLanguageId: Integer);
begin
  inherited Create;
  FMethodId := aMethodId;
  FLanguageId := aLanguageId;
end;

procedure TMethodI18n.Clear;
begin
  inherited Clear;
  FMethodId := 0;
  FLanguageId := 0;
  FMethodName := EmptyStr;
  FCategory := EmptyStr;
  FDescription := EmptyStr;
  FRecommendedUses := EmptyStr;
  FNotes := EmptyStr;
end;

procedure TMethodI18n.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TMethodI18n then
  begin
    FMethodId := TMethodI18n(Source).MethodId;
    FLanguageId := TMethodI18n(Source).LanguageId;
    FMethodName := TMethodI18n(Source).MethodName;
    FCategory := TMethodI18n(Source).Category;
    FDescription := TMethodI18n(Source).Description;
    FRecommendedUses := TMethodI18n(Source).RecommendedUses;
    FNotes := TMethodI18n(Source).Notes;
  end;
end;

function TMethodI18n.Clone: TXolmisRecord;
begin
  Result := TMethodI18n(inherited Clone);
end;

function TMethodI18n.Diff(const aOld: TMethodI18n; var Changes: TStrings): Boolean;
var
  R: String;
begin
  Result := False;
  R := EmptyStr;
  if Assigned(Changes) then
    Changes.Clear;
  if aOld = nil then
    Exit(False);

  if not SameText(aOld.MethodName, FMethodName) then
  begin
    R := Format('MethodName: "%s" -> "%s"', [aOld.MethodName, FMethodName]);
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  if not SameText(aOld.Category, FCategory) then
  begin
    R := Format('Category: "%s" -> "%s"', [aOld.Category, FCategory]);
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  if not SameText(aOld.Description, FDescription) then
  begin
    R := 'Description changed.';
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  if not SameText(aOld.RecommendedUses, FRecommendedUses) then
  begin
    R := 'RecommendedUses changed.';
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  if not SameText(aOld.Notes, FNotes) then
  begin
    R := 'Notes changed.';
    if Assigned(Changes) then
      Changes.Add(R);
  end;

  Result := Assigned(Changes) and (Changes.Count > 0);
end;

function TMethodI18n.EqualsTo(const Other: TMethodI18n): Boolean;
begin
  Result := Assigned(Other) and
    (FMethodId = Other.MethodId) and
    (FLanguageId = Other.LanguageId);
end;

procedure TMethodI18n.FromJSON(const aJSONString: String);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON(aJSONString));
  try
    FMethodId := Obj.Get('method_id', 0);
    FLanguageId := Obj.Get('language_id', 0);
    FMethodName := Obj.Get('method_name', '');
    FCategory := Obj.Get('category', '');
    FDescription := Obj.Get('description', '');
    FRecommendedUses := Obj.Get('recommended_uses', '');
    FNotes := Obj.Get('notes', '');
  finally
    Obj.Free;
  end;
end;

function TMethodI18n.ToJSON: String;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('method_id', FMethodId);
    JSONObject.Add('language_id', FLanguageId);
    JSONObject.Add('method_name', FMethodName);
    JSONObject.Add('category', FCategory);
    JSONObject.Add('description', FDescription);
    JSONObject.Add('recommended_uses', FRecommendedUses);
    JSONObject.Add('notes', FNotes);

    Result := JSONObject.AsJSON;
  finally
    JSONObject.Free;
  end;
end;

function TMethodI18n.ToString: String;
begin
  Result := Format('MethodI18n(MethodId=%d, LanguageId=%d, MethodName=%s, Category=%s, InsertDate=%s, UpdateDate=%s)',
    [FMethodId, FLanguageId, FMethodName, FCategory, DateTimeToStr(FInsertDate), DateTimeToStr(FUpdateDate)]);
end;

function TMethodI18n.Validate(out Msg: string): Boolean;
begin
  if FMethodId = 0 then
  begin
    Msg := 'MethodId required.';
    Exit(False);
  end;
  if FLanguageId = 0 then
  begin
    Msg := 'LanguageId required.';
    Exit(False);
  end;
  if FMethodName = EmptyStr then
  begin
    Msg := 'MethodName required.';
    Exit(False);
  end;

  Msg := '';
  Result := True;
end;

{ TMethodI18nRepository }

function TMethodI18nRepository.TableName: string;
begin
  Result := 'method_i18n';
end;

function TMethodI18nRepository.Exists(const Id: Integer): Boolean;
var
  Qry: TSQLQuery;
begin
  Qry := NewQuery;
  with Qry do
  try
    MacroCheck := True;
    SQL.Text := 'SELECT 1 AS x FROM %tablename WHERE method_id=:id LIMIT 1';
    MacroByName('tablename').Value := TableName;
    ParamByName('id').AsInteger := Id;
    Open;
    Result := not EOF;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodI18nRepository.FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord);
const
  ALLOWED: array[0..3] of string = ('method_id', 'language_id', 'method_name', 'category');
var
  Qry: TSQLQuery;
  I: Integer;
  Ok: Boolean;
begin
  if not (E is TMethodI18n) then
    raise Exception.Create('FindBy: Expected TMethodI18n');

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
    Add('ORDER BY method_id, language_id');

    MacroByName('tablename').Value := TableName;
    MacroByName('afield').Value := FieldName;
    ParamByName('avalue').Value := Value;
    Open;

    if not EOF then
      Hydrate(Qry, TMethodI18n(E));

    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodI18nRepository.GetById(const Id: Integer; E: TXolmisRecord);
var
  Qry: TSQLQuery;
begin
  if not (E is TMethodI18n) then
    raise Exception.Create('GetById: Expected TMethodI18n');

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    Clear;
    Add('SELECT * FROM %tablename');
    Add('WHERE method_id = :id');
    Add('ORDER BY language_id LIMIT 1');
    MacroByName('tablename').Value := TableName;
    ParamByName('id').AsInteger := Id;
    Open;
    if not EOF then
      Hydrate(Qry, TMethodI18n(E));
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodI18nRepository.GetByMethodAndLanguage(const aMethodId, aLanguageId: Integer; E: TMethodI18n);
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
    Add('WHERE method_id = :method_id');
    Add('AND language_id = :language_id');
    MacroByName('tablename').Value := TableName;
    ParamByName('method_id').AsInteger := aMethodId;
    ParamByName('language_id').AsInteger := aLanguageId;
    Open;
    if not EOF then
      Hydrate(Qry, E);
    Close;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodI18nRepository.Hydrate(aDataSet: TDataSet; E: TXolmisRecord);
var
  M: TMethodI18n;
begin
  if (aDataSet = nil) or (E = nil) or aDataSet.EOF then
    Exit;
  if not (E is TMethodI18n) then
    raise Exception.Create('Hydrate: Expected TMethodI18n');

  M := TMethodI18n(E);
  with aDataSet do
  begin
    M.MethodId := FieldByName('method_id').AsInteger;
    M.LanguageId := FieldByName('language_id').AsInteger;
    M.MethodName := FieldByName('method_name').AsString;
    M.Category := FieldByName('category').AsString;
    M.Description := FieldByName('description').AsString;
    M.RecommendedUses := FieldByName('recommended_uses').AsString;
    M.Notes := FieldByName('notes').AsString;
    GetTimeStamp(FieldByName('insert_date'), M.InsertDate);
    GetTimeStamp(FieldByName('update_date'), M.UpdateDate);
  end;
end;

procedure TMethodI18nRepository.Insert(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  M: TMethodI18n;
begin
  if not (E is TMethodI18n) then
    raise Exception.Create('Insert: Expected TMethodI18n');

  M := TMethodI18n(E);
  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('INSERT INTO method_i18n (' +
      'method_id, ' +
      'language_id, ' +
      'method_name, ' +
      'category, ' +
      'description, ' +
      'recommended_uses, ' +
      'notes, ' +
      'insert_date) ');
    Add('VALUES (' +
      ':method_id, ' +
      ':language_id, ' +
      ':method_name, ' +
      ':category, ' +
      ':description, ' +
      ':recommended_uses, ' +
      ':notes, ' +
      'datetime(''now'', ''subsec''))');

    ParamByName('method_id').AsInteger := M.MethodId;
    ParamByName('language_id').AsInteger := M.LanguageId;
    ParamByName('method_name').AsString := M.MethodName;
    ParamByName('category').AsString := M.Category;
    ParamByName('description').AsString := M.Description;
    ParamByName('recommended_uses').AsString := M.RecommendedUses;
    ParamByName('notes').AsString := M.Notes;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodI18nRepository.Update(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  M: TMethodI18n;
begin
  if not (E is TMethodI18n) then
    raise Exception.Create('Update: Expected TMethodI18n');

  M := TMethodI18n(E);
  if (M.MethodId = 0) or (M.LanguageId = 0) then
    raise Exception.CreateFmt('TMethodI18nRepository.Update: %s.', ['MethodId or LanguageId is empty']);

  Qry := NewQuery;
  with Qry, SQL do
  try
    Clear;
    Add('UPDATE method_i18n SET ' +
      'method_name = :method_name, ' +
      'category = :category, ' +
      'description = :description, ' +
      'recommended_uses = :recommended_uses, ' +
      'notes = :notes, ' +
      'update_date = datetime(''now'', ''subsec'')');
    Add('WHERE (method_id = :method_id) AND (language_id = :language_id)');

    ParamByName('method_name').AsString := M.MethodName;
    ParamByName('category').AsString := M.Category;
    ParamByName('description').AsString := M.Description;
    ParamByName('recommended_uses').AsString := M.RecommendedUses;
    ParamByName('notes').AsString := M.Notes;
    ParamByName('method_id').AsInteger := M.MethodId;
    ParamByName('language_id').AsInteger := M.LanguageId;

    ExecSQL;
  finally
    FreeAndNil(Qry);
  end;
end;

procedure TMethodI18nRepository.Delete(E: TXolmisRecord);
var
  Qry: TSQLQuery;
  M: TMethodI18n;
begin
  if not (E is TMethodI18n) then
    raise Exception.Create('Delete: Expected TMethodI18n');

  M := TMethodI18n(E);
  if (M.MethodId = 0) or (M.LanguageId = 0) then
    raise Exception.CreateFmt('TMethodI18nRepository.Delete: %s.', ['MethodId or LanguageId is empty']);

  Qry := NewQuery;
  with Qry, SQL do
  try
    MacroCheck := True;

    if not FTrans.Active then
      FTrans.StartTransaction;
    try
      Clear;
      Add('DELETE FROM %tablename');
      Add('WHERE (method_id = :method_id) AND (language_id = :language_id)');

      MacroByName('tablename').Value := TableName;
      ParamByName('method_id').AsInteger := M.MethodId;
      ParamByName('language_id').AsInteger := M.LanguageId;

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
