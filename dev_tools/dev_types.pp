unit dev_types;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Controls, DateUtils, RegExpr, DB, SQLDB, StrUtils, lcltype, lclintf, Windows,
  DBCtrls;

type
  TFormPosition = record
    X: Integer;
    Y: Integer;
    Height: Integer;
    Width: Integer;
  end;

type
  TDBManager = (dbSqlite = 0, dbPostgre = 1, dbFirebird = 2, dbMaria = 3, dbSqlserver = 4);

  TDBParams = record
    Name: String;
    Manager: TDBManager;
    IsLocal: Boolean;
    Database: String;
    Server: String;
    Port: Integer;
    Protocol: String;
    OpenMode: String;
    CharacterSet: String;
    StringFormat: String;
    UserName: String;
    Password: String;
    procedure Clear;
  end;

type
  TPartialDate = record
    Year: Integer;
    Month: Byte;
    Day: Byte;
    RomanMonth: Boolean;
    YearFirst: Boolean;
    Separator: Char;
    function ToString: String;
    procedure Clear;
    procedure Today;
  end;

type
  TSeparator = (spNone, spSemicolon, spComma, spColon, spPeriod, spPipe, spSlash, spHyphen,
    spUnderline);

const
  StrSeparators: array of Char = [#0, ';', ',', ':', '.', '|', '/', '-', '_'];

const
  StrCriteria: array of String = ['', 'like', 'like', '=', 'between', '>=', '<=', 'isnull', 'notnull'];

type
  TCriteriaType = (crNone, crLike, crStartLike, crEqual, crBetween, crMoreThan, crLessThan, crNull,
    crNotNull);
  TCampoType = (ctString, ctInteger, ctFloat, ctDate, ctTime, ctDateTime, ctBoolean);
  TFilterType = (tcTexto, tcInteiro, tcDecimal, tcData, tcHora, tcDataHora, tcLista, tcBool,
    tcLookup);
  TFilterValue = (fvNone, fvReset, fvAll, fvMarked, fvUnmarked, fvDeleted, fvQueued);
  TExternalFilter = (efNone, efId, efOrder, efFamily, efGenus, efSpecies, efQueued);
  TConsultaType = (csNone, csName, csNumber, csDate, csBoolean);

  TRecordStatus = record
    Status: (rsAll, rsActive, rsInactive);
    Mark: (rmAll, rmMarked, rmUnmarked);
    Queue: (rqAll, rqQueued, rqUnqueued);
    Share: (rxAll, rxExported, rxNotExported);
    procedure Clear;
  end;

  TFilterField = record
    FieldName: String;
    ReadableName: String;
    TableAlias: String;
    FilterType: TFilterType;
    Criteria: TCriteriaType;
    Value1: String;
    Value2: String;
    OpenParenthesis: Word;
    CloseParenthesis: Word;
    AndOr: (aoNone, aoAnd, aoOr);
    function ToSQL(aTabela, aLookupField, aKeyField: String): String;
    function ToHTML: String;
    function ToString: String;
    procedure Clear;
  end;

  TSearch = record
    FieldNames: TStrings;
    FilterType: TFilterType;
    Criteria: TCriteriaType;
    Value1: String;
    Value2: String;
    procedure Create;
    procedure Free;
    function ToSQL: String;
    function ToString: String;
    procedure Clear;
    function IsEmpty: Boolean;
  end;

  TSortDirection = (sdNone, sdAscending, sdDescending);
  TSortType = (stNone, stAlfanumeric, stNumeric, stDateTime, stBoolean, stTaxonomic);

  TSortedField = record
    FieldName: String;
    Direction: TSortDirection;
  end;

  TSortedFields = array of TSortedField;

type
  TCampo = record
    FieldName: String;
    Caption: String;
    NumericKey: Boolean;
    TextualKey: Boolean;
    DwCField: String;
    DataType: TCampoType;
    FilterType: TFilterType;
    LookupTable: String;
    LookupKey: String;
    LookupField: String;
    LookupFieldName: String;
    MinValue: Extended;
    MaxValue: Extended;
    ListValues: String;
    Visible: Boolean;
    CanSort: Boolean;
  end;

  TTabelaType = (tbNone, tbTablesMapping, tbFieldsMapping, tbReports, tbUsageStats, tbConnections);

const
  MesRomano: array of String = ['00', 'I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII', 'IX', 'X',
    'XI', 'XII'];

  TiposCampo: array of String = ['String', 'Integer', 'Float', 'Date', 'Time', 'DateTime',
    'Boolean'];
  TiposFiltro: array of String = ['Text', 'Integer', 'Decimal', 'Date', 'Time', 'DateTime',
    'List', 'Bool', 'Lookup'];

resourcestring
  rs_FilterAnd = 'and';
  rs_FilterOr = 'or';
  rs_FilterLike = 'contains';
  rs_FilterStartLike = 'start with';
  rs_FilterEqual = 'equal to';
  rs_FilterMoreThan = 'more than or equal to';
  rs_FilterLessThan = 'less than or equal to';
  rs_FilterBetween = 'between';
  rs_FilterNull = 'empty';
  rs_FilterNotNull = 'not empty';

  function FindSortedField(const aFieldName: String; aSortedFields: TSortedFields): Integer;
  procedure AddSortedField(const aFieldName: String; aDirection: TSortDirection;
    var aSortedFields: TSortedFields);
  procedure DeleteSortedField(const aFieldName: String; var aSortedFields: TSortedFields);
  procedure ResetSortedFields(const aDefaultField: String; aDefaultDirection: TSortDirection;
    var aSortedFields: TSortedFields);

  function CampoByName(const aCampoName: String): TCampoType;
  function FilterByName(const aFilterName: String): TFilterType;
  function GetModifier(aModifier: String): TFilterValue;

  procedure SetSelect(const aSQL: TStrings; aTable: TTabelaType; var aAlias: String);
  procedure SetCount(const aSQL: TStrings; aTable: TTabelaType);

  function TableSearch(aQuery: TSQLQuery; aTabela: TTabelaType; aSearch: TSearch;
    aQuickFilter: TStrings; aModifier: TRecordStatus; aOrder: TSortedFields;
    aWhere: TStrings): Boolean;
  function CarregaCodigo(aTabela, aCampoNome, aValor: String): Int64;
  function CarregaNome(aTabela, aCampoNome, aCampoCodigo: String; aValor: Int64): String;

  function WordSearch(aText: String): String;
  function SyllableSearch(aText: String): String;

implementation

uses dm_dev;

function FindSortedField(const aFieldName: String; aSortedFields: TSortedFields): Integer;
var
  i: Integer;
begin
  Result := -1;

  if not Assigned(aSortedFields) then
    Exit;

  if Length(aSortedFields) = 0 then
    Exit;

  for i := Low(aSortedFields) to High(aSortedFields) do
    if aSortedFields[i].FieldName = aFieldName then
      Result := i;
end;

procedure AddSortedField(const aFieldName: String; aDirection: TSortDirection;
  var aSortedFields: TSortedFields);
var
  n: Integer;
begin
  if not Assigned(aSortedFields) then
    Exit;

  n := FindSortedField(aFieldName, aSortedFields);
  // Se o campo já está na lista
  if n >= 0 then
  begin
    case aSortedFields[n].Direction of
      sdNone:
        aSortedFields[n].Direction := aDirection;
      sdAscending:
        aSortedFields[n].Direction := sdDescending;
      sdDescending:
        aSortedFields[n].Direction := sdAscending;
    end;
  end
  else
  // Se não está na lista, adiciona o campo
  begin
    n := Length(aSortedFields) + 1;
    SetLength(aSortedFields, n);
    aSortedFields[n].FieldName := aFieldName;
    aSortedFields[n].Direction := aDirection;
  end;
end;

procedure DeleteSortedField(const aFieldName: String; var aSortedFields: TSortedFields);
var
  n: Integer;
begin
  if not Assigned(aSortedFields) then
    Exit;

  n := FindSortedField(aFieldName, aSortedFields);
  // Se o campo já está na lista
  if n >= 0 then
  begin
    Delete(aSortedFields, n, 1);
  end;
end;

procedure ResetSortedFields(const aDefaultField: String; aDefaultDirection: TSortDirection;
  var aSortedFields: TSortedFields);
begin
  if not Assigned(aSortedFields) then
    Exit;

  SetLength(aSortedFields, 1);
  aSortedFields[0].FieldName := aDefaultField;
  aSortedFields[0].Direction := aDefaultDirection;
end;

function CampoByName(const aCampoName: String): TCampoType;
var
  i: Integer;
begin
  for i := 0 to High(TiposCampo) do
  begin
    if TiposCampo[i] = aCampoName then
    begin
      Result := TCampoType(i);
      Break;
    end;
  end;
end;

function FilterByName(const aFilterName: String): TFilterType;
var
  i: Integer;
begin
  for i := 0 to High(TiposFiltro) do
  begin
    if TiposFiltro[i] = aFilterName then
    begin
      Result := TFilterType(i);
      Break;
    end;
  end;
end;

function GetModifier(aModifier: String): TFilterValue;
begin
  Result := fvNone;
  //GravaLog('HASHTAG', aModifier);

  if MatchStr(aModifier, AllQS) then { #tudo }
    Result := fvAll
  else
  if MatchStr(aModifier, MarkedQS) then { #marcados }
    Result := fvMarked
  else
  if MatchStr(aModifier, UnmarkedQS) then { #naomarcados }
    Result := fvUnmarked
  else
  if MatchStr(aModifier, DeletedQS) then { #lixo }
    Result := fvDeleted
  else
  if MatchStr(aModifier, PrintQueueQS) then { #fila }
    Result := fvQueued;
end;

procedure SetSelect(const aSQL: TStrings; aTable: TTabelaType; var aAlias: String);
begin
  aAlias := '';
  case aTable of
    tbNone:
      ;
    tbVersions:
      ;

  end;
end;

procedure SetCount(const aSQL: TStrings; aTable: TTabelaType);
begin
  case aTable of
    tbNone:
      ;
    tbVersions:
      ;

  end;
end;

function TableSearch(aQuery: TSQLQuery; aTabela: TTabelaType; aSearch: TSearch;
  aQuickFilter: TStrings; aModifier: TRecordStatus; aOrder: TSortedFields;
  aWhere: TStrings): Boolean;
var
  AndWhere, aAlias, aSort, aDir: String;
  i: Integer;
begin
  aAlias := EmptyStr;
  AndWhere := 'where ';
  aWhere.Clear;
  Result := False;

  with aQuery, SQL do
  begin
    Close;
    Clear;
    SetSelect(SQL, aTabela, aAlias);

    // Valor digitado no campo Pesquisar
    if not aSearch.IsEmpty then
    begin
      Add(aSearch.ToSQL);
      aWhere.Add(aSearch.ToSQL);
      AndWhere := 'and ';
    end;

    // Filtros rápidos selecionados
    if aQuickFilter.Count > 0 then
    begin
      for i := 0 to aQuickFilter.Count - 1 do
      begin
        Add(AndWhere + aQuickFilter[i]);
        aWhere.Add(AndWhere + aQuickFilter[i]);
        AndWhere := 'and ';
      end;
    end;

    // Registro ativo ou inativo
    case aModifier.Status of
      rsAll:
        ;
      rsActive:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_ativo = 1)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_ativo = 1)');
          AndWhere := 'and ';
        end;
      rsInactive:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_ativo = 0)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_ativo = 0)');
          AndWhere := 'and ';
        end;
    end;
    // Registro marcado ou desmarcado
    case aModifier.Mark of
      rmAll:
        ;
      rmMarked:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_marcado = 1)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_marcado = 1)');
          AndWhere := 'and ';
        end;
      rmUnmarked:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_marcado = 0)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_marcado = 0)');
          AndWhere := 'and ';
        end;
    end;
    // Registro na fila de impressão ou não
    case aModifier.Queue of
      rqAll:
        ;
      rqQueued:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_fila_impressao = 1)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_fila_impressao = 1)');
          AndWhere := 'and ';
        end;
      rqUnqueued:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_fila_impressao = 0)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_fila_impressao = 0)');
          AndWhere := 'and ';
        end;
    end;
    // Registro já compartilhado ou não
    case aModifier.Share of
      rxAll:
        ;
      rxExported:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_exported = 1)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_exported = 1)');
          AndWhere := 'and ';
        end;
      rxNotExported:
        begin
          Add(AndWhere + '(' + aAlias + 'reg_exported = 0)');
          aWhere.Add(AndWhere + '(' + aAlias + 'reg_exported = 0)');
          AndWhere := 'and ';
        end;
    end;

    // Ordenação dos registros
    if Length(aOrder) > 0 then
    begin
      aSort := '';
      aDir := '';
      for i := Low(aOrder) to High(aOrder) do
      begin
        case aOrder[i].Direction of
          sdNone:
            ;
          sdAscending:
            aDir := 'asc';
          sdDescending:
            aDir := 'desc';
        end;
        if Pos('NOME_', aOrder[i].FieldName) = 1 then
          aSort := aSort + aOrder[i].FieldName + {' collate PT_BR ' +} aDir
        else
          aSort := aSort + aAlias + aOrder[i].FieldName + {' collate PT_BR ' +} aDir;
        if i < High(aOrder) then
          aSort := aSort + ', ';
      end;
      Add('order by ' + aSort);
    end;

    //GravaLogSQL(SQL);
    Open;
  end;

  Result := not aQuery.IsEmpty;
end;

function CarregaCodigo(aTabela, aCampoNome, aValor: String): Int64;
var
  //cpoCodigo: String;
  Qry: TSQLQuery;
begin
  if aValor = '' then
    Result := 0
  else
  begin
    //if aTabela = 'ADM_USERS' then
    //  cpoCodigo := 'USR_CODIGO'
    //else
    //  cpoCodigo := 'reg_num_interno';
    Qry := TSQLQuery.Create(DMD.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := DMD.sqlCon;
      Clear;
      Add('select reg_num_interno from %TABNAME where %UNIQUEF = :UNIQUEV');
      MacroByName('TABNAME').AsString := aTabela;
      MacroByName('UNIQUEF').AsString := aCampoNome;
      ParamByName('UNIQUEV').AsString := aValor;
      // GravaLogSQL(SQL);
      Open;
      if not(IsEmpty) then
        Result := FieldByName('reg_num_interno').AsLargeInt
      else
        Result := 0;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function CarregaNome(aTabela, aCampoNome, aCampoCodigo: String; aValor: Int64): String;
var
  Qry: TSQLQuery;
begin
  if aValor > 0 then
  begin
    Qry := TSQLQuery.Create(DMD.sqlCon);
    with Qry, SQL do
    try
      MacroCheck := True;
      DataBase := DMD.sqlCon;
      Clear;
      Add('select %UNIQUEF from %TABNAME where %KEYF = :KEYV');
      MacroByName('UNIQUEF').AsString := aCampoNome;
      MacroByName('TABNAME').AsString := aTabela;
      MacroByName('KEYF').AsString := aCampoCodigo;
      ParamByName('KEYV').AsLargeInt := aValor;
      // GravaLogSQL(SQL);
      Open;
      if not(IsEmpty) then
      begin
        Result := FieldByName(aCampoNome).AsString;
      end;
      Close;
    finally
      FreeAndNil(Qry);
    end;
  end;
end;

function WordSearch(aText: String): String;
var
  i, total: Integer;
  Words: TStringList;
begin
  Words := TStringList.Create; // ('"', ' ', [soStrictDelimiter]);
  total := ExtractStrings([' '], [' '], PAnsiChar(aText), Words);
  if total > 1 then
    for i := 0 to Words.Count - 1 do
      if ExecRegExpr('^#[a-z]+$', Words[i]) then
        Result := Result + ''
      else
        if i = 0 then
          Result := AnsiDequotedStr(Words[i], '"') + '% '
        else
          if i = Words.Count - 1 then
            Result := Result + '%' + AnsiDequotedStr(Words[i], '"')
          else
            Result := Result + '%' + AnsiDequotedStr(Words[i], '"') + '% ';
  if total = 1 then
    if ExecRegExpr('^#[a-z]+$', aText) then
      Result := ''
    else
      Result := AnsiDequotedStr(aText, '"');
end;

function SyllableSearch(aText: String): String;
var
  i, total: Integer;
  Syllables: TStringList;
begin
  Result := '';
  Syllables := TStringList.Create; // ('"', ' ', [soStrictDelimiter]);
  total := ExtractStrings([' ', '+'], [' '], PAnsiChar(aText), Syllables);
  if total > 1 then
    for i := 0 to Syllables.Count - 1 do
      if ExecRegExpr('^#[a-z]+$', Syllables[i]) then
        Result := Result + ''
      else
        if i = 0 then
          Result := AnsiDequotedStr(Syllables[i], '"') + '%'
        else
          if i = Syllables.Count - 1 then
            Result := Result + AnsiDequotedStr(Syllables[i], '"')
          else
            Result := Result + AnsiDequotedStr(Syllables[i], '"') + '%';
  if total = 1 then
    if ExecRegExpr('^#[a-z]+$', aText) then
      Result := ''
    else
      Result := AnsiDequotedStr(aText, '"');
end;

{ TPartialDate }

procedure TPartialDate.Clear;
begin
  Year := 0;
  Month := 0;
  Day := 0;
  RomanMonth := True;
  YearFirst := False;
  Separator := '.';
end;

procedure TPartialDate.Today;
var
  a, m, d: Word;
begin
  DecodeDate(DateUtils.Today, a, m, d);
  Year := a;
  Month := m;
  Day := d;
  RomanMonth := True;
  YearFirst := False;
  Separator := '.';
end;

function TPartialDate.ToString: String;
var
  S, m, d, Y: String;
begin
  if RomanMonth then
    m := MesRomano[Month]
  else
    m := Format('%2.2d', [Month]);
  d := Format('%2.2d', [Day]);
  Y := Format('%4.4d', [YearOf(EncodeDate(Year, 1, 1))]);
  if YearFirst then
    S := Y + Separator + m + Separator + d
  else
    S := d + Separator + m + Separator + Y;

  Result := S;
end;

{ TFilterField }

procedure TFilterField.Clear;
begin
  FieldName := EmptyStr;
  ReadableName := EmptyStr;
  TableAlias := EmptyStr;
  FilterType := tcTexto;
  Criteria := crLike;
  Value1 := EmptyStr;
  Value2 := EmptyStr;
  OpenParenthesis := 1;
  CloseParenthesis := 1;
  AndOr := aoNone;
end;

function TFilterField.ToHTML: String;
const
  ColorAndOr: String = 'clHotLight';
  ColorCriteria: String = '$000F87FF';
  FormatTagFont: String = '<font color="%s">%s</font> ';
  { (deprecated) for use with TNextStyleSheet
  idAndOr: String = 'plus';
  idCriteria: String = 'sinal';
  FormatTagSpan: String = '<span id="%s">%s</span>'; }
var
  OP, CP, CR, AO, VL1: String;
begin
  OP := StringOfChar('(', OpenParenthesis);
  CP := StringOfChar(')', CloseParenthesis);
  case AndOr of
    aoNone:
      AO := EmptyStr;
    aoAnd:
      AO := Format(FormatTagFont, [ColorAndOr, Trim(rs_FilterAnd)]);
    aoOr:
      AO := Format(FormatTagFont, [ColorAndOr, Trim(rs_FilterOr)]);
  end;
  case Criteria of
    crLike:
      begin
        if ExecRegExpr('^%[A-Za-z0-9 .,-@]+%$', Value1) then
          CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterLike)])
        else
          CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterStartLike)]);
      end;
    crEqual:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterEqual)]);
    crBetween:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterBetween)]);
    crMoreThan:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterMoreThan)]);
    crLessThan:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterLessThan)]);
    crNull:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterNull)]);
    crNotNull:
      CR := Format(FormatTagFont, [ColorCriteria, Trim(rs_FilterNotNull)]);
  end;
  VL1 := StringReplace(Value1, '%', '', [rfReplaceAll]);
  if (Criteria = crNull) or (Criteria = crNotNull) then
    Result := Format('%s%s<i>%s</i> <b>%s</b>%s', [AO, OP, ReadableName, CR, CP])
  else
    if Value2 <> EmptyStr then
      Result := Format('%s%s<i>%s</i> %s <b>%s</b> <font color="%s">%s</font> <b>%s</b>%s',
        [AO, OP, ReadableName, CR, Value1, ColorCriteria, Trim(rs_FilterAnd), Value2, CP])
    else
      Result := Format('%s%s<i>%s</i> %s <b>%s</b>%s', [AO, OP, ReadableName, CR, VL1, CP]);
end;

function TFilterField.ToSQL(aTabela, aLookupField, aKeyField: String): String;
var
  OP, CP, CR, AO, FN, VL1: String;
  v: Integer;
begin
  OP := StringOfChar('(', OpenParenthesis);
  CP := StringOfChar(')', CloseParenthesis);
  FN := TableAlias + FieldName;
  case FilterType of
    tcData:
      FN := 'date(' + FN + ')';
    tcHora:
      FN := 'time(' + FN + ')';
    tcDataHora:
      FN := 'datetime(' + FN + ')';
  end;
  case AndOr of
    aoNone:
      AO := '';
    aoAnd:
      AO := 'and';
    aoOr:
      AO := 'or';
  end;
  CR := StrCriteria[Ord(Criteria)];

  if FilterType = tcLookup then
  begin
    with TSQLQuery.Create(DMD.sqlCon) do
    try
      MacroCheck := True;
      DataBase := DMD.sqlCon;
      SQL.Add('select %KEYF from %TABNAME where %LOOKUP = :VLOOK');
      MacroByName('KEYF').AsString := aKeyField;
      MacroByName('TABNAME').AsString := aTabela;
      MacroByName('LOOKUP').AsString := aLookupField;
      ParamByName('VLOOK').AsString := Value1;
      Open;
      v := Fields[0].AsInteger;
      Close;
      VL1 := IntToStr(v);
    finally
      Free;
    end;
  end
  else
    VL1 := Value1;
  if (Criteria = crNull) or (Criteria = crNotNull) then
    Result := Format('%s %s%s %s%s', [AO, OP, FN, CR, CP])
  else
    if (Value2 <> '') then
      Result := Format('%s %s%s %s %s and %s%s', [AO, OP, FN, CR, Value1, Value2, CP])
    else
      Result := Format('%s %s%s %s %s%s', [AO, OP, FN, CR, VL1, CP]);
end;

function TFilterField.ToString: String;
var
  OP, CP, CR, AO, VL1: String;
begin
  OP := StringOfChar('(', OpenParenthesis);
  CP := StringOfChar(')', CloseParenthesis);
  case AndOr of
    aoNone:
      AO := EmptyStr;
    aoAnd:
      AO := rs_FilterAnd;
    aoOr:
      AO := rs_FilterOr;
  end;
  case Criteria of
    crLike:
      CR := rs_FilterLike;
    crStartLike:
      CR := rs_FilterStartLike;
    crEqual:
      CR := rs_FilterEqual;
    crBetween:
      CR := rs_FilterBetween;
    crMoreThan:
      CR := rs_FilterMoreThan;
    crLessThan:
      CR := rs_FilterLessThan;
    crNull:
      CR := rs_FilterNull;
    crNotNull:
      CR := rs_FilterNotNull;
  end;
  VL1 := StringReplace(Value1, '%', '', [rfReplaceAll]);
  if (Criteria = crNull) or (Criteria = crNotNull) then
    Result := Format('%s %s%s %s%s', [AO, OP, ReadableName, CR, CP])
  else
    if Value2 <> '' then
      Result := Format('%s %s%s %s %s e %s%s', [AO, OP, ReadableName, CR, Value1, Value2, CP])
    else
      Result := Format('%s %s%s %s %s%s', [AO, OP, ReadableName, CR, VL1, CP]);
end;

{ TDBParams }

procedure TDBParams.Clear;
begin
  Name := 'XolmisDB';
  Manager := dbSqlite;
  IsLocal := True;
  Database := EmptyStr;
  Server := EmptyStr;
  Port := 0;
  Protocol := EmptyStr;
  OpenMode := EmptyStr;
  CharacterSet := EmptyStr;
  StringFormat := EmptyStr;
  UserName := EmptyStr;
  Password := EmptyStr;
end;

{ TSearch }

procedure TSearch.Clear;
begin
  FieldNames.Clear;
  Criteria := crNone;
  FilterType := tcTexto;
  Value1 := EmptyStr;
  Value2 := EmptyStr;
end;

procedure TSearch.Create;
begin
  FieldNames := TStringList.Create;
end;

procedure TSearch.Free;
begin
  FreeAndNil(FieldNames);
end;

function TSearch.IsEmpty: Boolean;
begin
  Result := FieldNames.Count = 0;
end;

function TSearch.ToSQL: String;
const
  FMaskNull: String = '(%s %s) ';
  FMaskV1: String = '(%s %s %s) ';
  FMaskV2: String = '(%s %s %s and %s) ';
  FMaskDateV1: String = '(date(%s) %s date(%s)) ';
  FMaskDateV2: String = '(date(%s) %s date(%s) and date(%s)) ';
  FMaskTimeV1: String = '(time(%s) %s time(%s)) ';
  FMaskTimeV2: String = '(time(%s) %s time(%s) and time(%s)) ';
  FMaskDateTimeV1: String = '(datetime(%s) %s datetime(%s)) ';
  FMaskDateTimeV2: String = '(datetime(%s) %s datetime(%s) and datetime(%s)) ';
var
  i: Integer;
  V1, V2, S, Msk, Operador: String;
begin
  Result := EmptyStr;

  Operador := StrCriteria[Ord(Criteria)];
  S := 'where ';
  V1 := Trim(Value1);
  V2 := Trim(Value2);
  if Criteria in [crNull, crNotNull] then
    Msk := FMaskNull;
  if V1 <> EmptyStr then
  begin
    case FilterType of
      tcTexto, tcLista, tcLookup:
        begin
          if Pos('+', V1) > 0 then
            V1 := SyllableSearch(V1) + '%'
          else
            V1 := WordSearch(V1) + '%';
          if Criteria = crLike then
            V1 := '%' + V1;
          Msk := FMaskV1;
        end;
      tcBool:
        Msk := FMaskV1;
      tcInteiro, tcDecimal:
        begin
          V1 := StringReplace(V1, ',', '.', [rfReplaceAll]);
          V2 := StringReplace(V2, ',', '.', [rfReplaceAll]);
          if Criteria = crBetween then
            Msk := FMaskV2
          else
            Msk := FMaskV1;
        end;
      tcData:
        begin
          if Criteria = crBetween then
            Msk := FMaskDateV2
          else
            Msk := FMaskDateV1;
        end;
      tcHora:
        begin
          if Criteria = crBetween then
            Msk := FMaskTimeV2
          else
            Msk := FMaskTimeV1;
        end;
      tcDataHora:
        begin
          if Criteria = crBetween then
            Msk := FMaskDateTimeV2
          else
            Msk := FMaskDateTimeV1;
        end;
    end;
  end;

  if FieldNames.Count > 1 then
    S := S + '(';

  for i := 0 to FieldNames.Count - 1 do
  begin
    case Criteria of
      crNone: ;
      crLike, crStartLike, crEqual, crMoreThan, crLessThan:
        S := S + Format(Msk, [FieldNames[i], Operador, V1]);
      crBetween:
        S := S + Format(Msk, [FieldNames[i], Operador, V1, V2]);
      crNull, crNotNull:
        S := S + Format(Msk, [FieldNames[i], Operador]);
    end;

    if i < FieldNames.Count - 1 then
        S := S + 'or ';
  end;

  if FieldNames.Count > 1 then
    S := S + ')';

  Result := S;
end;

function TSearch.ToString: String;
begin
  Result := EmptyStr;

  { TODO TSearch.ToString }
end;

{ TRecordStatus }

procedure TRecordStatus.Clear;
begin
  Status := rsActive;
  Mark := rmAll;
  Queue := rqAll;
  Share := rxAll;
end;

end.

