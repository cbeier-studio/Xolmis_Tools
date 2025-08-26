unit udlg_desttaxon;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, CheckLst, EditBtn, StdCtrls,
  Character, Buttons, atshapelinebgra, BCPanel, utils_global, utils_taxonomy;

type

  { TdlgDestTaxon }

  TdlgDestTaxon = class(TForm)
    iButtons: TImageList;
    lblChangeSuffix: TLabel;
    pChangeSuffix: TBCPanel;
    pDestinationTaxon: TBCPanel;
    eDestinationTaxon: TEditButton;
    lblApplyTo: TLabel;
    lblDestinationTaxon: TLabel;
    lineBottom: TShapeLineBGRA;
    pContent: TPanel;
    pApplyTo: TBCPanel;
    pBottom: TPanel;
    rbSuffixKeep: TRadioButton;
    rbSuffixA: TRadioButton;
    rbSuffixUs: TRadioButton;
    rbSuffixUm: TRadioButton;
    rbSuffixI: TRadioButton;
    sbApplyToMarked: TSpeedButton;
    sbApplyToSelected: TSpeedButton;
    sbClose: TButton;
    sbApply: TBitBtn;
    procedure eDestinationTaxonButtonClick(Sender: TObject);
    procedure eDestinationTaxonKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure rbSuffixKeepClick(Sender: TObject);
    procedure sbApplyClick(Sender: TObject);
    procedure sbApplyToSelectedClick(Sender: TObject);
  private
    FTaxonomyAction: TTaxonomyAction;
    FApplyTo: TApplyChangesTo;
    FChangeSuffix: TChangeSuffix;
    FTaxonomies: TBirdTaxonomies;
    FTaxon: Integer;
    function ValidateFields: Boolean;
  public
    property TaxonomyAction: TTaxonomyAction read FTaxonomyAction write FTaxonomyAction;
    property ApplyTo: TApplyChangesTo read FApplyTo write FApplyTo;
    property ChangeSuffix: TChangeSuffix read FChangeSuffix write FChangeSuffix;
    property Taxonomies: TBirdTaxonomies read FTaxonomies write FTaxonomies;
    property Taxon: Integer read FTaxon write FTaxon;
  end;

var
  dlgDestTaxon: TdlgDestTaxon;

implementation

uses
  utils_dialogs;

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
  //if (cklTaxonomy.Checked[0] = False) and (cklTaxonomy.Checked[1] = False) and (cklTaxonomy.Checked[2] = False) then
  //begin
  //  MsgDlg('', 'Select at least one taxonomy to update!', mtInformation);
  //  Result:= False;
  //end;
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

  //if cklTaxonomy.Checked[0] then
  //  FTaxonomies := FTaxonomies + [btClements];
  //if cklTaxonomy.Checked[1] then
  //  FTaxonomies := FTaxonomies + [btIoc];
  //if cklTaxonomy.Checked[2] then
  //  FTaxonomies := FTaxonomies + [btCbro];

  ModalResult:= mrOK;
end;

procedure TdlgDestTaxon.FormCreate(Sender: TObject);
begin
  FTaxonomies := [];
  FTaxon := 0;
end;

procedure TdlgDestTaxon.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel;
  end;
end;

procedure TdlgDestTaxon.FormShow(Sender: TObject);
begin
  eDestinationTaxon.Enabled := FTaxonomyAction <> taSplit;
  lblDestinationTaxon.Enabled := eDestinationTaxon.Enabled;
end;

procedure TdlgDestTaxon.rbSuffixKeepClick(Sender: TObject);
begin
  if rbSuffixKeep.Checked then
    FChangeSuffix := csKeep
  else
  if rbSuffixA.Checked then
    FChangeSuffix := csA
  else
  if rbSuffixUs.Checked then
    FChangeSuffix := csUs
  else
  if rbSuffixUm.Checked then
    FChangeSuffix := csUm
  else
  if rbSuffixI.Checked then
    FChangeSuffix := csI;
end;

procedure TdlgDestTaxon.eDestinationTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eDestinationTaxon, True, FTaxon);
end;

procedure TdlgDestTaxon.eDestinationTaxonKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfAll], eDestinationTaxon, True, FTaxon, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    eDestinationTaxon.Clear;
    Key := #0;
  end;
  { <ENTER/RETURN> key }
  if (Key = #13) then
  begin
    sbApplyClick(nil);
    Key := #0;
  end;
end;

initialization
  {$I udlg_desttaxon.lrs}

end.

