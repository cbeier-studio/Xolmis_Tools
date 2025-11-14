unit uedt_occurrence;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, CheckLst, ExtCtrls, Buttons, HtmlView,
  DB, SQLDB;

type

  { TedtOccurrence }

  TedtOccurrence = class(TForm)
    btnSave: TBitBtn;
    btnCancel: TBitBtn;
    ckList: TCheckListBox;
    txtTaxon: THtmlViewer;
    pBottom: TPanel;
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FTaxonId: Integer;
    FTaxonName: String;
    procedure LoadCountries;
    procedure SaveCountries;
  public
    property TaxonId: Integer read FTaxonId write FTaxonId;
    property TaxonName: String read FTaxonName write FTaxonName;
  end;

var
  edtOccurrence: TedtOccurrence;

implementation

uses
  data_getvalue, udm_taxa, udlg_loading;

{ TedtOccurrence }

procedure TedtOccurrence.btnSaveClick(Sender: TObject);
begin
  SaveCountries;

  ModalResult := mrOK;
end;

procedure TedtOccurrence.FormShow(Sender: TObject);
begin
  txtTaxon.LoadFromString(FTaxonName);

  LoadCountries;
end;

procedure TedtOccurrence.LoadCountries;
var
  Qry: TSQLQuery;
  Idx: Integer;
begin
  dlgLoading.Show;
  dlgLoading.UpdateProgress('Loading occurrence for taxon...', -1);

  ckList.Items.Clear;

  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;

    Add('SELECT country_name FROM countries');
    Add('WHERE (active_status = 1)');
    Add('ORDER BY country_name ASC');
    Open;
    First;
    while not EOF do
    begin
      ckList.Items.Add(FieldByName('country_name').AsString);
      Next;
    end;
    Close;

    Clear;
    Add('SELECT zc.country_id, c.country_name FROM zoo_taxa_countries AS zc');
    Add('LEFT JOIN countries AS c ON zc.country_id = c.country_id');
    Add('WHERE (zc.active_status = 1) AND (zc.taxon_id = :taxon_id)');
    Add('ORDER BY c.country_name ASC');
    ParamByName('taxon_id').AsInteger := FTaxonId;
    Open;
    First;
    while not EOF do
    begin
      Idx := ckList.Items.IndexOf(FieldByName('country_name').AsString);
      ckList.Checked[Idx] := True;
      Next;
    end;
    Close;
  finally
    FreeAndNil(Qry);
    dlgLoading.Hide;
  end;
end;

procedure TedtOccurrence.SaveCountries;
var
  i: Integer;
  Qry, qryEdit: TSQLQuery;
begin
  dlgLoading.Show;
  dlgLoading.UpdateProgress('Saving occurrence for taxon...', -1);

  qryEdit := TSQLQuery.Create(nil);
  qryEdit.DataBase := dmTaxa.sqlCon;

  Qry := TSQLQuery.Create(nil);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;
    Clear;
    Add('SELECT zc.*, c.country_name FROM zoo_taxa_countries AS zc');
    Add('LEFT JOIN countries AS c ON zc.country_id = c.country_id');
    Add('WHERE (zc.taxon_id = :taxon_id) AND (c.country_name = :country_name)');
    for i := 0 to ckList.Items.Count - 1 do
    begin
      ParamByName('taxon_id').AsInteger := FTaxonId;
      ParamByName('country_name').AsString := ckList.Items[i];
      Open;
      if ckList.Checked[i] then
      begin
        if not IsEmpty then
        begin
          qryEdit.SQL.Clear;
          qryEdit.SQL.Add('UPDATE zoo_taxa_countries SET active_status = 1');
          qryEdit.SQL.Add('WHERE (taxon_id = :taxon_id) AND (country_id = :country_id)');
          qryEdit.ParamByName('taxon_id').AsInteger := FTaxonId;
          qryEdit.ParamByName('country_id').AsInteger := GetKey('countries', 'country_id', 'country_name', ckList.Items[i]);
          qryEdit.ExecSQL;
        end
        else
        begin
          qryEdit.SQL.Clear;
          qryEdit.SQL.Add('INSERT INTO zoo_taxa_countries (taxon_id, country_id, insert_date, update_date) ');
          qryEdit.SQL.Add('VALUES (:taxon_id, :country_id, datetime(''now'', ''subsec''), datetime(''now'', ''subsec''))');
          qryEdit.ParamByName('taxon_id').AsInteger := FTaxonId;
          qryEdit.ParamByName('country_id').AsInteger := GetKey('countries', 'country_id', 'country_name', ckList.Items[i]);
          qryEdit.ExecSQL;
        end;
      end
      else
      begin
        if not IsEmpty then
        begin
          qryEdit.SQL.Clear;
          qryEdit.SQL.Add('UPDATE zoo_taxa_countries SET active_status = 0');
          //qryEdit.SQL.Add('DELETE FROM zoo_taxa_countries');
          qryEdit.SQL.Add('WHERE (taxon_id = :taxon_id) AND (country_id = :country_id)');
          qryEdit.ParamByName('taxon_id').AsInteger := FTaxonId;
          qryEdit.ParamByName('country_id').AsInteger := GetKey('countries', 'country_id', 'country_name', ckList.Items[i]);
          qryEdit.ExecSQL;
        end;
      end;
      Close;
    end;
  finally
    FreeAndNil(Qry);
    FreeAndNil(qryEdit);
    dlgLoading.Hide;
  end;
end;

initialization
  {$I uedt_occurrence.lrs}

end.

