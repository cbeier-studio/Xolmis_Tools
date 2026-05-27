unit uedt_method_i18n;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ATShapeLineBGRA, BCPanel,
  Buttons, DB, SQLDB, models_method;

type

  { TedtMethodI18n }

  TedtMethodI18n = class(TForm)
    cbLanguage: TComboBox;
    eCategory: TEdit;
    eName: TEdit;
    lblCategory: TLabel;
    lblDescription: TLabel;
    lblLanguage: TLabel;
    lblName: TLabel;
    lblNotes: TLabel;
    lblRecommendedUses: TLabel;
    lineBottom: TShapeLineBGRA;
    mDescription: TMemo;
    mNotes: TMemo;
    mRecommendedUses: TMemo;
    pBottom: TPanel;
    pCategory: TBCPanel;
    pContent: TPanel;
    pDescription: TBCPanel;
    pLanguage: TBCPanel;
    pName: TBCPanel;
    pNotes: TBCPanel;
    pRecommendedUses: TBCPanel;
    sbClose: TButton;
    sbSave: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FMethodId: Integer;
    FVernacular: TMethodI18n;
    procedure GetRecord;
    procedure SetRecord;
    function ValidateFields: Boolean;
  public
    property IsNew: Boolean read FIsNew write FIsNew;
    property MethodId: Integer read FMethodId write FMethodId;
    property Vernacular: TMethodI18n read FVernacular write FVernacular;
  end;

var
  edtMethodI18n: TedtMethodI18n;

implementation

uses
  data_getvalue, utils_global, utils_dialogs, udm_taxa;

{$R *.lfm}

{ TedtMethodI18n }

procedure TedtMethodI18n.FormCreate(Sender: TObject);
begin
  FVernacular := TMethodI18n.Create();
end;

procedure TedtMethodI18n.FormDestroy(Sender: TObject);
begin
  FVernacular.Free;
end;

procedure TedtMethodI18n.FormShow(Sender: TObject);
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

procedure TedtMethodI18n.GetRecord;
begin
  cbLanguage.ItemIndex := cbLanguage.Items.IndexOf(GetName('languages', 'language_name', 'language_id', FVernacular.LanguageId));
  eName.Text := FVernacular.MethodName;
  eCategory.Text := FVernacular.Category;
  mDescription.Text := FVernacular.Description;
  mRecommendedUses.Text := FVernacular.RecommendedUses;
  mNotes.Text := FVernacular.Notes;
end;

procedure TedtMethodI18n.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOK;
end;

procedure TedtMethodI18n.SetRecord;
begin
  FVernacular.MethodId := FMethodId;
  FVernacular.LanguageId := GetKey('languages', 'language_id', 'language_name', cbLanguage.Text);
  FVernacular.MethodName            := eName.Text;
  FVernacular.Category        := eCategory.Text;
  FVernacular.Description     := mDescription.Text;
  FVernacular.RecommendedUses := mRecommendedUses.Text;
  FVernacular.Notes           := mNotes.Text;
end;

function TedtMethodI18n.ValidateFields: Boolean;
begin
  Result := True;

  if cbLanguage.ItemIndex < 0 then
  begin
    MsgDlg(rsTitleInformation, 'Language is required.', mtInformation);
    Exit(False);
  end;
  if eName.Text = EmptyStr then
  begin
    MsgDlg(rsTitleInformation, 'Name is required.', mtInformation);
    Exit(False);
  end;
end;

end.

