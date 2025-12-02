unit uedt_vernacular;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ToggleSwitch, Buttons,
  ATShapeLineBGRA, BCPanel, DB, SQLDB, models_taxon;

type

  { TedtVernacular }

  TedtVernacular = class(TForm)
    cbLanguage: TComboBox;
    eVernacular: TEdit;
    lblLanguage: TLabel;
    lblVernacular: TLabel;
    lblPreferred: TLabel;
    lineBottom: TShapeLineBGRA;
    pLanguage: TBCPanel;
    pBottom: TPanel;
    pContent: TPanel;
    pVernacular: TBCPanel;
    pPreferred: TBCPanel;
    sbSave: TBitBtn;
    sbClose: TButton;
    switchPreferred: TToggleSwitch;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FTaxonId: Integer;
    FVernacular: TVernacularName;
    procedure GetRecord;
    procedure SetRecord;
    function ValidateFields: Boolean;
  public
    property IsNew: Boolean read FIsNew write FIsNew;
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property Vernacular: TVernacularName read FVernacular write FVernacular;
  end;

var
  edtVernacular: TedtVernacular;

implementation

uses
  data_getvalue, utils_global, utils_dialogs, udm_taxa;

{ TedtVernacular }

procedure TedtVernacular.FormCreate(Sender: TObject);
begin
  FVernacular := TVernacularName.Create();
end;

procedure TedtVernacular.FormDestroy(Sender: TObject);
begin
  FVernacular.Free;
end;

procedure TedtVernacular.FormShow(Sender: TObject);
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

procedure TedtVernacular.GetRecord;
begin
  cbLanguage.ItemIndex := cbLanguage.Items.IndexOf(GetName('languages', 'language_name', 'language_id', FVernacular.LanguageId));
  eVernacular.Text := FVernacular.VernacularName;
  switchPreferred.Checked := FVernacular.Preferred;
end;

procedure TedtVernacular.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOK;
end;

procedure TedtVernacular.SetRecord;
begin
  FVernacular.TaxonId := FTaxonId;
  FVernacular.LanguageId := GetKey('languages', 'language_id', 'language_name', cbLanguage.Text);
  FVernacular.VernacularName := eVernacular.Text;
  FVernacular.Preferred := switchPreferred.Checked;
end;

function TedtVernacular.ValidateFields: Boolean;
var
  Qry: TSQLQuery;
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

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;
    Add('SELECT vernacular_id FROM vernacular_names');
    Add('WHERE (language_id = :language_id) AND (vernacular_name = :vernacular_name)');
    Add('  AND (vernacular_id != :id)');
    ParamByName('language_id').AsInteger := GetKey('languages', 'language_id', 'language_name', cbLanguage.Text);
    ParamByName('vernacular_name').AsString := eVernacular.Text;
    ParamByName('id').AsInteger := FVernacular.Id;
    Open;
    if RecordCount > 0 then
    begin
      MsgDlg(rsTitleInformation, 'Vernacular name already exists.', mtInformation);
      Exit(False);
    end;
  finally
    FreeAndNil(Qry);
  end;
end;

end.

