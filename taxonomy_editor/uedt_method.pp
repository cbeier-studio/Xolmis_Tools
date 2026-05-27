unit uedt_method;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ATShapeLineBGRA, BCPanel,
  Buttons, DB, SQLDB, ToggleSwitch, models_method;

type

  { TedtMethod }

  TedtMethod = class(TForm)
    eAbbreviation: TEdit;
    eEbirdName: TEdit;
    eCategory: TEdit;
    eName: TEdit;
    lblDescription: TLabel;
    lblNotes: TLabel;
    lblRecommendedUses: TLabel;
    lblEbirdName: TLabel;
    lblCategory: TLabel;
    lblName: TLabel;
    lblCanDelete: TLabel;
    lblAbbreviation: TLabel;
    lineBottom: TShapeLineBGRA;
    mDescription: TMemo;
    mNotes: TMemo;
    mRecommendedUses: TMemo;
    pDescription: TBCPanel;
    pNotes: TBCPanel;
    pRecommendedUses: TBCPanel;
    pEbirdName: TBCPanel;
    pCategory: TBCPanel;
    pBottom: TPanel;
    pContent: TPanel;
    pName: TBCPanel;
    pCanDelete: TBCPanel;
    pAbbreviation: TBCPanel;
    sbClose: TButton;
    sbSave: TBitBtn;
    switchCanDelete: TToggleSwitch;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    FMethodId: Integer;
    FMethod: TMethod;
    procedure GetRecord;
    procedure SetRecord;
    function ValidateFields: Boolean;
  public
    property IsNew: Boolean read FIsNew write FIsNew;
    property MethodId: Integer read FMethodId write FMethodId;
    property Method: TMethod read FMethod write FMethod;
  end;

var
  edtMethod: TedtMethod;

implementation

uses
  data_getvalue, utils_global, utils_dialogs, udm_taxa;

{$R *.lfm}

{ TedtMethod }

procedure TedtMethod.FormCreate(Sender: TObject);
begin
  FMethod := TMethod.Create();
end;

procedure TedtMethod.FormDestroy(Sender: TObject);
begin
  FMethod.Free;
end;

procedure TedtMethod.FormShow(Sender: TObject);
begin
  if not FIsNew then
    GetRecord;
end;

procedure TedtMethod.GetRecord;
var
  Repo: TMethodRepository;
begin
  if FMethodId > 0 then
  begin
    Repo := TMethodRepository.Create(dmTaxa.sqlCon);
    try
      Repo.GetById(FMethodId, Method);
      if Method.IsNew then
        raise Exception.Create('GetRecord: MethodId does not exists.');
    finally
      Repo.Free;
    end;
  end;

  eName.Text := FMethod.Name;
  eAbbreviation.Text := FMethod.Abbreviation;
  eCategory.Text := FMethod.Category;
  eEbirdName.Text := FMethod.EbirdName;
  mDescription.Text := FMethod.Description;
  mRecommendedUses.Text := FMethod.RecommendedUses;
  mNotes.Text := FMethod.Notes;
  switchCanDelete.Checked := FMethod.CanDelete;
end;

procedure TedtMethod.sbSaveClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  SetRecord;

  ModalResult := mrOK;
end;

procedure TedtMethod.SetRecord;
begin
  FMethod.Name := eName.Text;
  FMethod.Abbreviation    := eAbbreviation.Text;
  FMethod.Category        := eCategory.Text;
  FMethod.EbirdName       := eEbirdName.Text;
  FMethod.Description     := mDescription.Text;
  FMethod.RecommendedUses := mRecommendedUses.Text;
  FMethod.Notes           := mNotes.Text;
  FMethod.CanDelete := switchCanDelete.Checked;
end;

function TedtMethod.ValidateFields: Boolean;
begin
  Result := True;

  if eName.Text = EmptyStr then
  begin
    MsgDlg(rsTitleInformation, 'Method name is required.', mtInformation);
    Exit(False);
  end;
  if eAbbreviation.Text = EmptyStr then
  begin
    MsgDlg(rsTitleInformation, 'Abbreviation is required.', mtInformation);
    Exit(False);
  end;
end;

end.

