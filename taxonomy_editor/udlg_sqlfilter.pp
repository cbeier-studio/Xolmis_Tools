unit udlg_sqlfilter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls,
  SynEdit, SynCompletion, SynHighlighterSQL, atshapelinebgra;

type

  { TdlgSqlFilter }

  TdlgSqlFilter = class(TForm)
    Panel1: TPanel;
    pBottom: TPanel;
    sbApply: TBitBtn;
    sbClose: TButton;
    seScript: TSynEdit;
    sbReset: TSpeedButton;
    SynCompletion: TSynCompletion;
    SynSQLSyn: TSynSQLSyn;
    procedure sbResetClick(Sender: TObject);
  private

  public

  end;

var
  dlgSqlFilter: TdlgSqlFilter;

implementation

{ TdlgSqlFilter }

procedure TdlgSqlFilter.sbResetClick(Sender: TObject);
begin
  with seScript.Lines do
  begin
    Clear;
    Add('SELECT z.*,');
    Add('(SELECT ep.full_name FROM zoo_taxa AS ep');
    Add('  WHERE ep.taxon_id = z.parent_taxon_id) AS parent_taxon_name,');
    Add('(SELECT ev.full_name FROM zoo_taxa AS ev');
    Add('  WHERE ev.valid_id = z.valid_id) AS valid_name,');
    Add('(SELECT ip.full_name FROM zoo_taxa AS ip');
    Add('  WHERE ip.taxon_id = z.ioc_parent_taxon_id) AS ioc_parent_taxon_name,');
    Add('(SELECT iv.full_name FROM zoo_taxa AS iv');
    Add('  WHERE iv.valid_id = z.ioc_valid_id) AS ioc_valid_name,');
    Add('(SELECT cp.full_name FROM zoo_taxa AS cp');
    Add('  WHERE cp.taxon_id = z.cbro_parent_taxon_id) AS cbro_parent_taxon_name,');
    Add('(SELECT cv.full_name FROM zoo_taxa AS cv');
    Add('  WHERE cv.valid_id = z.cbro_valid_id) AS cbro_valid_name');
    Add('FROM zoo_taxa AS z');
    Add('');
  end;

end;

initialization
  {$I udlg_sqlfilter.lrs}

end.

