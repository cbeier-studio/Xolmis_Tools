unit models_base;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB;

type

  { TXolmisRecord }

  TXolmisRecord = class(TPersistent)
  protected
    FId: Integer;
    FGuid: String;
    FInsertDate: TDateTime;
    FUpdateDate: TDateTime;
    FMarked: Boolean;
    FActive: Boolean;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    procedure Clear; virtual;
    procedure Assign(Source: TPersistent); override;
    function Clone: TXolmisRecord; virtual;
    function IsNew: Boolean; inline;
  published
    property Id: Integer read FId write FId;
    property Guid: String read FGuid write FGuid;
    property InsertDate: TDateTime read FInsertDate write FInsertDate;
    property UpdateDate: TDateTime read FUpdateDate write FUpdateDate;
    property Marked: Boolean read FMarked write FMarked;
    property Active: Boolean read FActive write FActive;
  end;

  TXolmisRecordClass = class of TXolmisRecord;

  { TXolmisRepository }

  TXolmisRepository = class
  protected
    FConn: TSQLConnection;
    FTrans: TSQLTransaction;

    function TableName: string; virtual; abstract;
    function NewQuery: TSQLQuery;
  public
    constructor Create(AConn: TSQLConnection);

    function Exists(const Id: Integer): Boolean; virtual; abstract;
    procedure FindBy(const FieldName: String; const Value: Variant; E: TXolmisRecord); virtual; abstract;
    procedure GetById(const Id: Integer; E: TXolmisRecord); virtual; abstract;
    procedure Hydrate(aDataSet: TDataSet; E: TXolmisRecord); virtual; abstract;
    procedure Insert(E: TXolmisRecord); virtual; abstract;
    procedure Update(E: TXolmisRecord); virtual; abstract;
    procedure Save(E: TXolmisRecord);
    procedure Delete(E: TXolmisRecord); virtual; abstract;
  end;

implementation

{ TXolmisRecord }

constructor TXolmisRecord.Create;
begin
  inherited Create;
  Clear;
end;

procedure TXolmisRecord.Assign(Source: TPersistent);
begin
  if Source is TXolmisRecord then
  begin
    FId := TXolmisRecord(Source).FId;
    FGuid := TXolmisRecord(Source).FGuid;
    FInsertDate := TXolmisRecord(Source).FInsertDate;
    FUpdateDate := TXolmisRecord(Source).FUpdateDate;
    FMarked := TXolmisRecord(Source).FMarked;
    FActive := TXolmisRecord(Source).FActive;
  end
  else
    inherited Assign(Source);
end;

procedure TXolmisRecord.AssignTo(Dest: TPersistent);
begin
  if Dest is TXolmisRecord then
    TXolmisRecord(Dest).Assign(Self)
  else
    inherited AssignTo(Dest);
end;

procedure TXolmisRecord.Clear;
begin
  FId := 0;
  FGuid := EmptyStr;
  FInsertDate := StrToDateTime('30/12/1500 00:00:00');
  FUpdateDate := StrToDateTime('30/12/1500 00:00:00');
  FMarked := False;
  FActive := False;
end;

function TXolmisRecord.Clone: TXolmisRecord;
var
  C: TClass;
begin
  C := ClassType;
  Result := TXolmisRecordClass(C).Create;
  Result.Assign(Self);
end;

function TXolmisRecord.IsNew: Boolean;
begin
  Result := FId = 0;
end;

{ TXolmisRepository }

constructor TXolmisRepository.Create(AConn: TSQLConnection);
begin
  if AConn = nil then
    raise Exception.Create('Repository connection cannot be nil.');
  FConn := AConn;
  FTrans := AConn.Transaction;
end;

function TXolmisRepository.NewQuery: TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  Result.DataBase := FConn;
  Result.Transaction := FTrans;
end;

procedure TXolmisRepository.Save(E: TXolmisRecord);
begin
  if E = nil then
    Exit;

  if E.IsNew then
    Insert(E)
  else
  begin
    if Exists(E.Id) then
      Update(E)
    else
      Insert(E);
  end;
end;

end.

