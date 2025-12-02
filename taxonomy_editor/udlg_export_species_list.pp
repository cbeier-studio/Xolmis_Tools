unit udlg_export_species_list;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBCtrls, ExtCtrls, CheckLst, Buttons, StdCtrls,
  DB, SQLDB;

type

  { TdlgExportSpeciesList }

  TdlgExportSpeciesList = class(TForm)
    ckScientificName: TCheckBox;
    ckVernacularName: TCheckBox;
    ckSynonyms: TCheckBox;
    cbVernacularLanguage: TComboBox;
    pSciName: TPanel;
    pVernacularName: TPanel;
    pSynonyms: TPanel;
    pBottom: TPanel;
    SaveDlg: TSaveDialog;
    sbOK: TBitBtn;
    sbClose: TButton;
    txtCountry: TDBText;
    procedure sbOKClick(Sender: TObject);
  private
    procedure LoadLanguageList;
    function ValidateFields: Boolean;
  public

  end;

var
  dlgExportSpeciesList: TdlgExportSpeciesList;

implementation

uses utils_dialogs, data_getvalue, io_json, udm_taxa;

{ TdlgExportSpeciesList }

procedure TdlgExportSpeciesList.LoadLanguageList;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  with Q, SQL do
  try
    DataBase := dmTaxa.sqlCon;

    Add('SELECT language_name FROM languages');
    Add('WHERE (active_status = 1)');
    Add('ORDER BY language_name ASC');
    Open;
    First;
    while not EOF do
    begin
      cbVernacularLanguage.Items.Add(FieldByName('language_name').AsString);

      Next;
    end;
    Close;
  finally
    FreeAndNil(Q);
  end;
end;

procedure TdlgExportSpeciesList.sbOKClick(Sender: TObject);
var
  FLanguageId, FCountryId: Integer;
  FCountryCode: String;
begin
  if not ValidateFields then
    Exit;

  FCountryId := dmTaxa.qCountries.FieldByName('country_id').AsInteger;
  FCountryCode := GetName('countries', 'country_code', 'country_id', FCountryId);
  FLanguageId := GetKey('languages', 'language_id', 'language_name', cbVernacularLanguage.Text);

  SaveDlg.FileName := Format('species_data_%s.json', [FCountryCode]);
  if SaveDlg.Execute then
    ExportSpeciesList(SaveDlg.FileName, FCountryId, FLanguageId, ckVernacularName.Checked);

  ModalResult := mrOK;
end;

function TdlgExportSpeciesList.ValidateFields: Boolean;
begin
  Result := True;

  if (ckScientificName.Checked = False) and (ckVernacularName.Checked = False) and
    (ckSynonyms.Checked = False) then
  begin
    MsgDlg('Validation', 'You must check at least one item to add to the exported file.', mtInformation);
    Exit(False);
  end;

  if (ckVernacularName.Checked) and (cbVernacularLanguage.ItemIndex < 0) then
  begin
    MsgDlg('Validation', 'You must select a language for vernacular names.', mtInformation);
    Exit(False);
  end;
end;

end.

