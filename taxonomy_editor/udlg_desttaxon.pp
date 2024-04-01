unit udlg_desttaxon;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, CheckLst, EditBtn, StdCtrls,
  Buttons, atshapelinebgra, BCPanel, lib_taxa;

type

  { TdlgDestTaxon }

  TdlgDestTaxon = class(TForm)
    BCPanel1: TBCPanel;
    cklTaxonomy: TCheckListBox;
    eDestinationTaxon: TEditButton;
    lblApplyTo: TLabel;
    lblDestinationTaxon: TLabel;
    lineBottom: TShapeLineBGRA;
    Panel1: TPanel;
    pApplyTo: TBCPanel;
    pBottom: TPanel;
    sbApplyToMarked: TSpeedButton;
    sbApplyToSelected: TSpeedButton;
    sbClose: TButton;
    sbApply: TBitBtn;
    procedure eDestinationTaxonButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbApplyClick(Sender: TObject);
    procedure sbApplyToSelectedClick(Sender: TObject);
  private
    FTaxonomyAction: TTaxonomyAction;
    FApplyTo: TApplyChangesTo;
    FTaxonomies: TBirdTaxonomies;
    FTaxon: Integer;
    function ValidateFields: Boolean;
  public
    property TaxonomyAction: TTaxonomyAction read FTaxonomyAction write FTaxonomyAction;
    property ApplyTo: TApplyChangesTo read FApplyTo write FApplyTo;
    property Taxonomies: TBirdTaxonomies read FTaxonomies write FTaxonomies;
    property Taxon: Integer read FTaxon write FTaxon;
  end;

var
  dlgDestTaxon: TdlgDestTaxon;

implementation

{ TdlgDestTaxon }

procedure TdlgDestTaxon.sbApplyToSelectedClick(Sender: TObject);
begin
  if sbApplyToSelected.Down then
    FApplyTo := acSelected
  else
  if sbApplyToMarked.Down then
    FApplyTo := acMarked;
end;

function TdlgDestTaxon.ValidateFields: Boolean;
begin
  Result:= True;

  // Required fields
  if (cklTaxonomy.Checked[0] = False) and (cklTaxonomy.Checked[1] = False) and (cklTaxonomy.Checked[2] = False) then
  begin
    MsgDlg('', 'Select at least one taxonomy to update!', mtInformation);
    Result:= False;
  end;
  if (TaxonomyAction <> taSplit) and (Taxon = 0) then
  begin
    MsgDlg('', 'Select a destination taxon to update!', mtInformation);
    Result:= False;
  end;
end;

procedure TdlgDestTaxon.sbApplyClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  if cklTaxonomy.Checked[0] then
    FTaxonomies := FTaxonomies + [btClements];
  if cklTaxonomy.Checked[1] then
    FTaxonomies := FTaxonomies + [btIoc];
  if cklTaxonomy.Checked[2] then
    FTaxonomies := FTaxonomies + [btCbro];

  ModalResult:= mrOK;
end;

procedure TdlgDestTaxon.FormCreate(Sender: TObject);
begin
  FTaxonomies := [];
  FTaxon := 0;
end;

procedure TdlgDestTaxon.FormShow(Sender: TObject);
begin
  eDestinationTaxon.Enabled := FTaxonomyAction <> taSplit;
  lblDestinationTaxon.Enabled := eDestinationTaxon.Enabled;
end;

procedure TdlgDestTaxon.eDestinationTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eDestinationTaxon, True, FTaxon);
end;

initialization
  {$I udlg_desttaxon.lrs}

end.

