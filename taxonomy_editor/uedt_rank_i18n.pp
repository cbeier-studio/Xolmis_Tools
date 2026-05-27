unit uedt_rank_i18n;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, BCPanel, ATShapeLineBGRA,
  Buttons, DB, SQLDB, models_rank;

type

  { TedtRankI18n }

  TedtRankI18n = class(TForm)
    cbLanguage: TComboBox;
    eVernacular: TEdit;
    lblLanguage: TLabel;
    lblVernacular: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    pContent: TPanel;
    pLanguage: TBCPanel;
    pVernacular: TBCPanel;
    sbClose: TButton;
    sbSave: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FRankId: Integer;
    FVernacular: TRankI18n;
    procedure GetRecord;
    procedure SetRecord;
    function ValidateFields: Boolean;
  public
    property IsNew: Boolean read FIsNew write FIsNew;
    property RankId: Integer read FRankId write FRankId;
    property Vernacular: TRankI18n read FVernacular write FVernacular;
  end;

var
  edtRankI18n: TedtRankI18n;

implementation

uses
  data_getvalue, utils_global, utils_dialogs, udm_taxa;

{$R *.lfm}

{ TedtRankI18n }

procedure TedtRankI18n.FormCreate(Sender: TObject);
begin
  FVernacular := TRankI18n.Create();
end;

procedure TedtRankI18n.FormDestroy(Sender: TObject);
begin
  FVernacular.Free;
end;

procedure TedtRankI18n.FormShow(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;
    Add('SELECT language_name FROM languages');
    Add('WHERE (active_status = 1)');
    Add('ORDER BY language_name ASC');
    Open;
    First;
    while not EOF do
    begin
      cbLanguage.Items.Add(FieldByName('language_name').AsString);
      Next;
    end;
    Close;
  finally
    FreeAndNil(Qry);
  end;

  if not FIsNew then
    GetRecord;
end;

procedure TedtRankI18n.GetRecord;
begin
  cbLanguage.ItemIndex := cbLanguage.Items.IndexOf(GetName('languages', 'language_name', 'language_id', FVernacular.LanguageId));
  eVernacular.Text := FVernacular.RankName;
end;

procedure TedtRankI18n.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOK;
end;

procedure TedtRankI18n.SetRecord;
begin
  FVernacular.RankId := FRankId;
  FVernacular.LanguageId := GetKey('languages', 'language_id', 'language_name', cbLanguage.Text);
  FVernacular.RankName := eVernacular.Text;
end;

function TedtRankI18n.ValidateFields: Boolean;
begin
  Result := True;

  if cbLanguage.ItemIndex < 0 then
  begin
    MsgDlg(rsTitleInformation, 'Language is required.', mtInformation);
    Exit(False);
  end;
  if eVernacular.Text = EmptyStr then
  begin
    MsgDlg(rsTitleInformation, 'Vernacular name is required.', mtInformation);
    Exit(False);
  end;
end;

end.

