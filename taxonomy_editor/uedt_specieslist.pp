unit uedt_specieslist;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, DBGrids, ExtCtrls, DBCtrls,
  Buttons, BCPanel, ColorSpeedButton, Grids, DB, SQLDB, Character;

type

  { TedtSpeciesList }

  TedtSpeciesList = class(TForm)
    btnClose: TBitBtn;
    gridSp: TDBGrid;
    lblSpeciesCount: TLabel;
    TimerMsg: TTimer;
    txtCountry: TDBText;
    iButtons: TImageList;
    eFind: TEdit;
    iconFind: TImage;
    pBottom: TPanel;
    pFind: TBCPanel;
    sbClearFind: TColorSpeedButton;
    procedure eFindKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gridSpDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    procedure gridSpEditButtonClick(Sender: TObject);
    procedure TimerMsgTimer(Sender: TObject);
  private
    procedure UpdateSpeciesCount;
  public

  end;

var
  edtSpeciesList: TedtSpeciesList;

implementation

uses
  utils_dialogs, utils_taxonomy, udm_taxa;

{ TedtSpeciesList }

procedure TedtSpeciesList.eFindKeyPress(Sender: TObject; var Key: char);
var
  aTaxonId: Integer;
  Qry: TSQLQuery;
  recExists: Boolean;
begin
  aTaxonId := 0;

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    if FindTaxonDlg([tfSpecies, tfSpuhs, tfSlashes, tfDomestics, tfSubspeciesGroups], eFind, True, aTaxonId, Key) then
    begin
      Qry := TSQLQuery.Create(dmTaxa.sqlCon);
      with Qry, SQL do
      try
        DataBase := dmTaxa.sqlCon;

        Clear;
        Add('SELECT taxon_id, country_id FROM zoo_taxa_countries ');
        Add('WHERE (taxon_id = :taxon_id) AND (country_id = :country_id)');
        ParamByName('taxon_id').AsInteger := aTaxonId;
        ParamByName('country_id').AsInteger := dmTaxa.qCountries.FieldByName('country_id').AsInteger;
        Open;
        recExists := RecordCount > 0;
        Close;

        if recExists then
        begin
          Clear;
          Add('UPDATE zoo_taxa_countries SET active_status = 1');
          Add('WHERE (taxon_id = :taxon_id) AND (country_id = :country_id)');
          ParamByName('taxon_id').AsInteger := aTaxonId;
          ParamByName('country_id').AsInteger := dmTaxa.qSpeciesList.FieldByName('country_id').AsInteger;
          ExecSQL;
          lblSpeciesCount.Caption := 'Species is on the list already!';
        end
        else
        begin
          Clear;
          Add('INSERT INTO zoo_taxa_countries (taxon_id, country_id, insert_date, update_date) ');
          Add('VALUES (:taxon_id, :country_id, datetime(''now'', ''subsec''), datetime(''now'', ''subsec''))');
          ParamByName('taxon_id').AsInteger := aTaxonId;
          ParamByName('country_id').AsInteger := dmTaxa.qCountries.FieldByName('country_id').AsInteger;
          ExecSQL;
        end;
      finally
        FreeAndNil(Qry);
      end;
      dmTaxa.qSpeciesList.Refresh;
      if recExists then
        TimerMsg.Enabled := True
      else
        UpdateSpeciesCount;
    end;
    Key := #0;
  end;

  eFind.Clear;
end;

procedure TedtSpeciesList.FormCreate(Sender: TObject);
begin
  dmTaxa.qSpeciesList.Open;
end;

procedure TedtSpeciesList.FormDestroy(Sender: TObject);
begin
  dmTaxa.qSpeciesList.Close;
end;

procedure TedtSpeciesList.FormShow(Sender: TObject);
begin
  UpdateSpeciesCount;
end;

procedure TedtSpeciesList.gridSpDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
var
  PT:TPoint;
begin
  if (DataCol = 1) then
  with Sender as TDBGrid do
  begin
    PT := Rect.CenterPoint;
    PT.Y := PT.Y - (Canvas.TextHeight('Remove') div 2);
    PT.X := PT.X - (Canvas.TextWidth('Remove') div 2);
    Canvas.Brush.Style:= bsClear;
    Canvas.TextOut(PT.X, PT.Y, 'Remove');
  end;
end;

procedure TedtSpeciesList.gridSpEditButtonClick(Sender: TObject);
var
  Qry: TSQLQuery;
begin
  if dmTaxa.qSpeciesList.RecordCount = 0 then
    Exit;

  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  begin
    DataBase := dmTaxa.sqlCon;

    Clear;
    Add('UPDATE zoo_taxa_countries SET active_status = 0');
    Add('WHERE (taxon_id = :taxon_id) AND (country_id = :country_id)');
    ParamByName('taxon_id').AsInteger := dmTaxa.qSpeciesList.FieldByName('taxon_id').AsInteger;
    ParamByName('country_id').AsInteger := dmTaxa.qSpeciesList.FieldByName('country_id').AsInteger;
    ExecSQL;
  end;
  dmTaxa.qSpeciesList.Refresh;
  UpdateSpeciesCount;
end;

procedure TedtSpeciesList.TimerMsgTimer(Sender: TObject);
begin
  TimerMsg.Enabled := False;

  UpdateSpeciesCount;
end;

procedure TedtSpeciesList.UpdateSpeciesCount;
begin
  lblSpeciesCount.Caption := Format('%d species', [dmTaxa.qSpeciesList.RecordCount]);
end;

initialization
  {$I uedt_specieslist.lrs}

end.

