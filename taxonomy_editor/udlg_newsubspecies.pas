unit udlg_newsubspecies;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, CheckLst, Buttons,
  atshapelinebgra, BCPanel, lib_taxa;

type

  { TdlgNewSubspecies }

  TdlgNewSubspecies = class(TForm)
    cklTaxonomy: TCheckListBox;
    eEpythet: TEdit;
    lblApplyTo: TLabel;
    lineBottom: TShapeLineBGRA;
    pApplyTo: TBCPanel;
    pBottom: TPanel;
    pContent: TPanel;
    sbApply: TBitBtn;
    sbClose: TButton;
    procedure eEpythetKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure sbApplyClick(Sender: TObject);
  private
    FTaxonomies: TBirdTaxonomies;
    FEpythet: String;
    function ValidateFields: Boolean;
  public
    property Taxonomies: TBirdTaxonomies read FTaxonomies write FTaxonomies;
    property Epythet: String read FEpythet write FEpythet;
  end;

var
  dlgNewSubspecies: TdlgNewSubspecies;

implementation

{ TdlgNewSubspecies }

procedure TdlgNewSubspecies.eEpythetKeyPress(Sender: TObject; var Key: char);
begin
  { <ENTER/RETURN> key }
  if (Key = #13) then
  begin
    sbApplyClick(nil);
    Key := #0;
  end;

  FormKeyPress(Sender, Key);
end;

procedure TdlgNewSubspecies.FormCreate(Sender: TObject);
begin
  FTaxonomies := [];
  FEpythet := EmptyStr;
end;

procedure TdlgNewSubspecies.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel;
  end;
end;

procedure TdlgNewSubspecies.sbApplyClick(Sender: TObject);
begin
  if not ValidateFields then
    Exit;

  if cklTaxonomy.Checked[0] then
    FTaxonomies := FTaxonomies + [btClements];
  if cklTaxonomy.Checked[1] then
    FTaxonomies := FTaxonomies + [btIoc];
  //if cklTaxonomy.Checked[2] then
  //  FTaxonomies := FTaxonomies + [btCbro];

  FEpythet := eEpythet.Text;

  ModalResult:= mrOK;
end;

function TdlgNewSubspecies.ValidateFields: Boolean;
begin
  Result:= True;

  // Required fields
  if (cklTaxonomy.Checked[0] = False) and (cklTaxonomy.Checked[1] = False) and (cklTaxonomy.Checked[2] = False) then
  begin
    MsgDlg('', 'Select at least one taxonomy to update!', mtInformation);
    Result:= False;
  end;
  if (eEpythet.Text = EmptyStr) then
  begin
    MsgDlg('', 'Inform the epythet for the new subspecies!', mtInformation);
    Result:= False;
  end;
end;

initialization
  {$I udlg_newsubspecies.lrs}

end.

