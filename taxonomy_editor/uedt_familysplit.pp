unit uedt_familysplit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst, ExtCtrls, Buttons,
  DB, SQLDB, utils_taxonomy;

type

  { TedtFamilySplit }

  TedtFamilySplit = class(TForm)
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    ckList: TCheckListBox;
    lblFamily: TLabel;
    pBottom: TPanel;
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FToTaxonId, FFromTaxonId: Integer;
    FToTaxonName: String;
    FRankType: TZooRank;
    procedure LoadTaxonList;
    procedure SaveFamilySplit;
    procedure SaveOrderSplit;
    procedure SaveSplit;
  public
    property FromTaxonId: Integer read FFromTaxonId write FFromTaxonId;
    property ToTaxonId: Integer read FToTaxonId write FToTaxonId;
    property ToTaxonName: String read FToTaxonName write FToTaxonName;
    property RankType: TZooRank read FRankType write FRankType;
  end;

var
  edtFamilySplit: TedtFamilySplit;

implementation

uses
  models_taxon, udm_taxa, udlg_loading;

{ TedtFamilySplit }

procedure TedtFamilySplit.btnSaveClick(Sender: TObject);
begin
  SaveSplit;

  ModalResult := mrOK;
end;

procedure TedtFamilySplit.FormShow(Sender: TObject);
begin
  case FRankType of
    trOrder: Caption := 'Order split';
    trFamily: Caption := 'Family split';
  end;

  lblFamily.Caption := FToTaxonName;

  LoadTaxonList;
end;

procedure TedtFamilySplit.LoadTaxonList;
var
  Qry: TSQLQuery;
begin
  dlgLoading.Show;
  dlgLoading.UpdateProgress('Loading taxa list...', -1);

  ckList.Items.Clear;

  Qry := TSQLQuery.Create(dmTaxa.sqlCon);
  with Qry, SQL do
  try
    DataBase := dmTaxa.sqlCon;

    Add('SELECT full_name FROM zoo_taxa');
    Add('WHERE (parent_taxon_id = :parent_taxon_id) AND (active_status = 1)');
    Add('ORDER BY full_name ASC');
    ParamByName('parent_taxon_id').AsInteger := FFromTaxonId;
    Open;
    First;
    while not EOF do
    begin
      ckList.Items.Add(FieldByName('full_name').AsString);
      Next;
    end;
    Close;
  finally
    FreeAndNil(Qry);
    dlgLoading.Hide;
  end;
end;

procedure TedtFamilySplit.SaveFamilySplit;
var
  Repo: TTaxonRepository;
  Taxon: TTaxon;
  i: Integer;
  Qry, Q, QS: TSQLQuery;
begin
  Repo := TTaxonRepository.Create(dmTaxa.sqlCon);
  Taxon := TTaxon.Create();
  try
    try
      for i := 0 to ckList.Items.Count - 1 do
      begin
        Taxon.Clear;

        // Move genus
        if ckList.Checked[i] then
        begin
          Repo.FindBy('full_name', ckList.Items[i], Taxon);
          if not Taxon.IsNew then
          begin
            Taxon.ParentTaxonId := FToTaxonId;
            Taxon.FamilyId := FToTaxonId;

            Repo.Update(Taxon);

            Qry := TSQLQuery.Create(nil);
            Q := TSQLQuery.Create(nil);
            QS := TSQLQuery.Create(nil);
            with Qry, SQL do
            try
              DataBase := dmTaxa.sqlCon;
              Q.DataBase := dmTaxa.sqlCon;
              QS.DataBase := dmTaxa.sqlCon;

              Add('SELECT taxon_id FROM zoo_taxa');
              Add('WHERE (parent_taxon_id = :parent_taxon_id)');
              Q.SQL.Text := Qry.SQL.Text;
              QS.SQL.Text := Qry.SQL.Text;

              // Move species
              ParamByName('parent_taxon_id').AsInteger := Taxon.Id;
              Open;
              First;
              while not EOF do
              begin
                MoveToFamily(FieldByName('taxon_id').AsInteger, FToTaxonId);

                // Move subspecies and subspecies groups
                Q.ParamByName('parent_taxon_id').AsInteger := FieldByName('taxon_id').AsInteger;
                Q.Open;
                Q.First;
                while not Q.EOF do
                begin
                  MoveToFamily(Q.FieldByName('taxon_id').AsInteger, FToTaxonId);

                  // Move subspecies from groups
                  QS.ParamByName('parent_taxon_id').AsInteger := Q.FieldByName('taxon_id').AsInteger;
                  QS.Open;
                  QS.First;
                  while not QS.EOF do
                  begin
                    MoveToFamily(QS.FieldByName('taxon_id').AsInteger, FToTaxonId);
                    QS.Next;
                  end;
                  QS.Close;

                  Q.Next;
                end;
                Q.Close;

                Next;
              end;
              Close;
            finally
              FreeAndNil(QS);
              FreeAndNil(Q);
              FreeAndNil(Qry);
            end;
          end;
        end;
        dlgLoading.Progress := i + 1;
      end;
      dmTaxa.sqlTrans.CommitRetaining;
    except
      dmTaxa.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Taxon);
    Repo.Free;
    dlgLoading.Hide;
    dlgLoading.Progress := 0;
    dlgLoading.Max := 100;
  end;
end;

procedure TedtFamilySplit.SaveOrderSplit;
var
  Repo: TTaxonRepository;
  Taxon: TTaxon;
  i: Integer;
  Qry, QSp, Q, QS: TSQLQuery;
begin
  Repo := TTaxonRepository.Create(dmTaxa.sqlCon);
  Taxon := TTaxon.Create();
  try
    try
      for i := 0 to ckList.Items.Count - 1 do
      begin
        Taxon.Clear;

        // Move family
        if ckList.Checked[i] then
        begin
          Repo.FindBy('full_name', ckList.Items[i], Taxon);
          if not Taxon.IsNew then
          begin
            Taxon.ParentTaxonId := FToTaxonId;
            Taxon.OrderId := FToTaxonId;

            Repo.Update(Taxon);

            Qry := TSQLQuery.Create(nil);
            QSp := TSQLQuery.Create(nil);
            Q := TSQLQuery.Create(nil);
            QS := TSQLQuery.Create(nil);
            with Qry, SQL do
            try
              DataBase := dmTaxa.sqlCon;
              QSp.DataBase := dmTaxa.sqlCon;
              Q.DataBase := dmTaxa.sqlCon;
              QS.DataBase := dmTaxa.sqlCon;

              Add('SELECT taxon_id FROM zoo_taxa');
              Add('WHERE (parent_taxon_id = :parent_taxon_id)');
              QSp.SQL.Text := Qry.SQL.Text;
              Q.SQL.Text := Qry.SQL.Text;
              QS.SQL.Text := Qry.SQL.Text;

              // Move genus
              ParamByName('parent_taxon_id').AsInteger := Taxon.Id;
              Open;
              First;
              while not EOF do
              begin
                MoveToOrder(FieldByName('taxon_id').AsInteger, FToTaxonId);

                // Move species
                QSp.ParamByName('parent_taxon_id').AsInteger := FieldByName('taxon_id').AsInteger;
                QSp.Open;
                QSp.First;
                while not QSp.EOF do
                begin
                  MoveToOrder(QSp.FieldByName('taxon_id').AsInteger, FToTaxonId);

                  // Move subspecies and subspecies groups
                  Q.ParamByName('parent_taxon_id').AsInteger := Qsp.FieldByName('taxon_id').AsInteger;
                  Q.Open;
                  Q.First;
                  while not Q.EOF do
                  begin
                    MoveToOrder(Q.FieldByName('taxon_id').AsInteger, FToTaxonId);

                    // Move subspecies from groups
                    QS.ParamByName('parent_taxon_id').AsInteger := Q.FieldByName('taxon_id').AsInteger;
                    QS.Open;
                    QS.First;
                    while not QS.EOF do
                    begin
                      MoveToOrder(QS.FieldByName('taxon_id').AsInteger, FToTaxonId);
                      QS.Next;
                    end;
                    QS.Close;

                    Q.Next;
                  end;
                  Q.Close;

                  QSp.Next;
                end;
                QSp.Close;

                Next;
              end;
              Close;
            finally
              FreeAndNil(QS);
              FreeAndNil(Q);
              FreeAndNil(QSp);
              FreeAndNil(Qry);
            end;
          end;
        end;
        dlgLoading.Progress := i + 1;
      end;
      dmTaxa.sqlTrans.CommitRetaining;
    except
      dmTaxa.sqlTrans.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(Taxon);
    Repo.Free;
    dlgLoading.Hide;
    dlgLoading.Progress := 0;
    dlgLoading.Max := 100;
  end;
end;

procedure TedtFamilySplit.SaveSplit;
begin
  dlgLoading.Show;
  dlgLoading.Max := ckList.Items.Count;

  case FRankType of
    trOrder:
    begin
      dlgLoading.UpdateProgress('Saving order split...', 0);
      SaveOrderSplit;
    end;
    trFamily:
    begin
      dlgLoading.UpdateProgress('Saving family split...', 0);
      SaveFamilySplit;
    end;
  end;
end;

initialization
  {$I uedt_familysplit.lrs}

end.

