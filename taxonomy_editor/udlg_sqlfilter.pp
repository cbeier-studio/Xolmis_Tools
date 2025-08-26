unit udlg_sqlfilter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls,
  SynEdit, SynCompletion, SynHighlighterSQL, atshapelinebgra;

type

  { TdlgSqlFilter }

  TdlgSqlFilter = class(TForm)
    iButtons: TImageList;
    pToolbar: TPanel;
    pBottom: TPanel;
    sbApply: TBitBtn;
    sbClose: TButton;
    seScript: TSynEdit;
    sbReset: TSpeedButton;
    SynCompletion: TSynCompletion;
    SynSQLSyn: TSynSQLSyn;
    procedure sbApplyClick(Sender: TObject);
    procedure sbResetClick(Sender: TObject);
  private
    FFilterText: String;
  public
    property FilterText: String read FFilterText write FFilterText;
  end;

var
  dlgSqlFilter: TdlgSqlFilter;

implementation

{ TdlgSqlFilter }

procedure TdlgSqlFilter.sbApplyClick(Sender: TObject);
begin
  FFilterText := seScript.Lines.Text;

  ModalResult := mrOk;
end;

procedure TdlgSqlFilter.sbResetClick(Sender: TObject);
begin
  with seScript.Lines do
  begin
    Clear;
    Add('SELECT z.*,');
    Add('    u.full_name AS parent_taxon_name,');
    Add('    v.full_name AS valid_name,');
    Add('    o.full_name AS order_name,');
    Add('    f.full_name AS family_name,');
    Add('    s.full_name AS subfamily_name,');
    Add('    n.full_name AS genero_name,');
    Add('    e.full_name AS species_name,');
    Add('    g.full_name AS subspecies_group_name');
    Add('FROM zoo_taxa AS z');
    Add('LEFT JOIN zoo_taxa AS u ON z.parent_taxon_id = u.taxon_id');
    Add('LEFT JOIN zoo_taxa AS v ON z.valid_id = v.taxon_id');
    Add('LEFT JOIN zoo_taxa AS o ON z.order_id = o.taxon_id');
    Add('LEFT JOIN zoo_taxa AS f ON z.family_id = f.taxon_id');
    Add('LEFT JOIN zoo_taxa AS s ON z.subfamily_id = s.taxon_id');
    Add('LEFT JOIN zoo_taxa AS n ON z.genus_id = n.taxon_id');
    Add('LEFT JOIN zoo_taxa AS e ON z.species_id = e.taxon_id');
    Add('LEFT JOIN zoo_taxa AS g ON z.subspecies_group_id = g.taxon_id');
    Add('');
  end;

end;

end.

