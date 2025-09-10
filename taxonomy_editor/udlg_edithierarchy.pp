unit udlg_edithierarchy;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons,
  atshapelinebgra, BCPanel, Character, utils_taxonomy;

type

  { TdlgEditHierarchy }

  TdlgEditHierarchy = class(TForm)
    iButtons: TImageList;
    pParentTaxon: TBCPanel;
    pOrder: TBCPanel;
    pFamily: TBCPanel;
    pSubfamily: TBCPanel;
    pGenus: TBCPanel;
    pSpecies: TBCPanel;
    pSubspeciesGroup: TBCPanel;
    pApplyTo: TBCPanel;
    cbClearParentTaxon: TCheckBox;
    cbClearOrder: TCheckBox;
    cbClearFamily: TCheckBox;
    cbClearSubfamily: TCheckBox;
    cbClearGenus: TCheckBox;
    cbClearSpecies: TCheckBox;
    cbClearSubspeciesGroup: TCheckBox;
    eParentTaxon: TEditButton;
    eOrder: TEditButton;
    eFamily: TEditButton;
    eSubfamily: TEditButton;
    eGenus: TEditButton;
    eSpecies: TEditButton;
    eSubspeciesGroup: TEditButton;
    lblDestinationTaxon: TLabel;
    lblDestinationTaxon1: TLabel;
    lblDestinationTaxon2: TLabel;
    lblDestinationTaxon3: TLabel;
    lblDestinationTaxon4: TLabel;
    lblDestinationTaxon5: TLabel;
    lblDestinationTaxon6: TLabel;
    lblApplyTo: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    sbClose: TButton;
    sbApply: TBitBtn;
    sbApplyToSelected: TSpeedButton;
    sbApplyToMarked: TSpeedButton;
    procedure cbClearFamilyChange(Sender: TObject);
    procedure cbClearGenusChange(Sender: TObject);
    procedure cbClearOrderChange(Sender: TObject);
    procedure cbClearParentTaxonChange(Sender: TObject);
    procedure cbClearSpeciesChange(Sender: TObject);
    procedure cbClearSubfamilyChange(Sender: TObject);
    procedure cbClearSubspeciesGroupChange(Sender: TObject);
    procedure eFamilyButtonClick(Sender: TObject);
    procedure eFamilyKeyPress(Sender: TObject; var Key: char);
    procedure eGenusButtonClick(Sender: TObject);
    procedure eGenusKeyPress(Sender: TObject; var Key: char);
    procedure eOrderButtonClick(Sender: TObject);
    procedure eOrderKeyPress(Sender: TObject; var Key: char);
    procedure eParentTaxonButtonClick(Sender: TObject);
    procedure eParentTaxonKeyPress(Sender: TObject; var Key: char);
    procedure eSpeciesButtonClick(Sender: TObject);
    procedure eSpeciesKeyPress(Sender: TObject; var Key: char);
    procedure eSubfamilyButtonClick(Sender: TObject);
    procedure eSubfamilyKeyPress(Sender: TObject; var Key: char);
    procedure eSubspeciesGroupButtonClick(Sender: TObject);
    procedure eSubspeciesGroupKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbApplyClick(Sender: TObject);
    procedure sbApplyToSelectedClick(Sender: TObject);
  private
    FApplyTo: TApplyChangesTo;
    FCurrent, FParent: Integer;
    FOrder, FFamily, FSubfamily, FGenus, FSpecies, FSubspeciesGroup: Integer;
    function GetClearParentTaxon: Boolean;
    function GetClearOrder: Boolean;
    function GetClearFamily: Boolean;
    function GetClearSubfamily: Boolean;
    function GetClearGenus: Boolean;
    function GetClearSpecies: Boolean;
    function GetClearSubspeciesGroup: Boolean;
    procedure GetValues;
    function ValidateFields: Boolean;
  public
    property ApplyTo: TApplyChangesTo read FApplyTo write FApplyTo;
    property CurrentTaxon: Integer read FCurrent write FCurrent;
    property ParentTaxon: Integer read FParent write FParent;
    property Order: Integer read FOrder write FOrder;
    property Family: Integer read FFamily write FFamily;
    property Subfamily: Integer read FSubfamily write FSubfamily;
    property Genus: Integer read FGenus write FGenus;
    property Species: Integer read FSpecies write FSpecies;
    property SubspeciesGroup: Integer read FSubspeciesGroup write FSubspeciesGroup;

    property ClearParentTaxon: Boolean read GetClearParentTaxon;
    property ClearOrder: Boolean read GetClearOrder;
    property ClearFamily: Boolean read GetClearFamily;
    property ClearSubfamily: Boolean read GetClearSubfamily;
    property ClearGenus: Boolean read GetClearGenus;
    property ClearSpecies: Boolean read GetClearSpecies;
    property ClearSubspeciesGroup: Boolean read GetClearSubspeciesGroup;
  end;

var
  dlgEditHierarchy: TdlgEditHierarchy;

implementation

uses
  data_getvalue, utils_global, utils_dialogs;

{ TdlgEditHierarchy }

procedure TdlgEditHierarchy.cbClearFamilyChange(Sender: TObject);
begin
  if cbClearFamily.Checked then
  begin
    FFamily := 0;
    eFamily.Clear;
  end;
end;

procedure TdlgEditHierarchy.cbClearGenusChange(Sender: TObject);
begin
  if cbClearGenus.Checked then
  begin
    FGenus := 0;
    eGenus.Clear;
  end;
end;

procedure TdlgEditHierarchy.cbClearOrderChange(Sender: TObject);
begin
  if cbClearOrder.Checked then
  begin
    FOrder := 0;
    eOrder.Clear;
  end;
end;

procedure TdlgEditHierarchy.cbClearParentTaxonChange(Sender: TObject);
begin
  if cbClearParentTaxon.Checked then
  begin
    FParent := 0;
    eParentTaxon.Clear;
  end;
end;

procedure TdlgEditHierarchy.cbClearSpeciesChange(Sender: TObject);
begin
  if cbClearSpecies.Checked then
  begin
    FSpecies := 0;
    eSpecies.Clear;
  end;
end;

procedure TdlgEditHierarchy.cbClearSubfamilyChange(Sender: TObject);
begin
  if cbClearSubfamily.Checked then
  begin
    FSubfamily := 0;
    eSubfamily.Clear;
  end;
end;

procedure TdlgEditHierarchy.cbClearSubspeciesGroupChange(Sender: TObject);
begin
  if cbClearSubspeciesGroup.Checked then
  begin
    FSubspeciesGroup := 0;
    eSubspeciesGroup.Clear;
  end;
end;

procedure TdlgEditHierarchy.eFamilyButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfFamilies], eFamily, True, FFamily);
end;

procedure TdlgEditHierarchy.eFamilyKeyPress(Sender: TObject; var Key: char);
begin
  //FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfFamilies], eFamily, True, FFamily, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FFamily := 0;
    eFamily.Clear;
    Key := #0;
  end;
end;

procedure TdlgEditHierarchy.eGenusButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfGenera], eGenus, True, FGenus);
end;

procedure TdlgEditHierarchy.eGenusKeyPress(Sender: TObject; var Key: char);
begin
  //FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfGenera], eGenus, True, FGenus, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FGenus := 0;
    eGenus.Clear;
    Key := #0;
  end;
end;

procedure TdlgEditHierarchy.eOrderButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfOrders], eOrder, True, FOrder);
end;

procedure TdlgEditHierarchy.eOrderKeyPress(Sender: TObject; var Key: char);
begin
  //FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfOrders], eOrder, True, FOrder, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FOrder := 0;
    eOrder.Clear;
    Key := #0;
  end;
end;

procedure TdlgEditHierarchy.eParentTaxonButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfAll], eParentTaxon, True, FParent);
end;

procedure TdlgEditHierarchy.eParentTaxonKeyPress(Sender: TObject; var Key: char);
begin
  //FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfAll], eParentTaxon, True, FParent, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FParent := 0;
    eParentTaxon.Clear;
    Key := #0;
  end;
end;

procedure TdlgEditHierarchy.eSpeciesButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfSpecies], eSpecies, True, FSpecies);
end;

procedure TdlgEditHierarchy.eSpeciesKeyPress(Sender: TObject; var Key: char);
begin
  //FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfSpecies], eSpecies, True, FSpecies, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FSpecies := 0;
    eSpecies.Clear;
    Key := #0;
  end;
end;

procedure TdlgEditHierarchy.eSubfamilyButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfFamilies], eSubfamily, True, FSubfamily);
end;

procedure TdlgEditHierarchy.eSubfamilyKeyPress(Sender: TObject; var Key: char);
begin
  //FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfFamilies], eSubfamily, True, FSubfamily, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FSubfamily := 0;
    eSubfamily.Clear;
    Key := #0;
  end;
end;

procedure TdlgEditHierarchy.eSubspeciesGroupButtonClick(Sender: TObject);
begin
  FindTaxonDlg([tfSubspeciesGroups], eSubspeciesGroup, True, FSubspeciesGroup);
end;

procedure TdlgEditHierarchy.eSubspeciesGroupKeyPress(Sender: TObject; var Key: char);
begin
  //FormKeyPress(Sender, Key);

  { Alphabetic search in numeric field }
  if (IsLetter(Key) or IsNumber(Key) or IsPunctuation(Key) or IsSeparator(Key) or IsSymbol(Key)) then
  begin
    FindTaxonDlg([tfSubspeciesGroups], eSubspeciesGroup, True, FSubspeciesGroup, Key);
    Key := #0;
  end;
  { CLEAR FIELD VALUE = Backspace }
  if (Key = #8) then
  begin
    FSubspeciesGroup := 0;
    eSubspeciesGroup.Clear;
    Key := #0;
  end;
end;

procedure TdlgEditHierarchy.FormShow(Sender: TObject);
begin
  if HaveMarkedTaxa then
    sbApplyToMarked.Down := True;

  GetValues;
end;

function TdlgEditHierarchy.GetClearFamily: Boolean;
begin
  Result := cbClearFamily.Checked;
end;

function TdlgEditHierarchy.GetClearGenus: Boolean;
begin
  Result := cbClearGenus.Checked;
end;

function TdlgEditHierarchy.GetClearOrder: Boolean;
begin
  Result := cbClearOrder.Checked;
end;

function TdlgEditHierarchy.GetClearParentTaxon: Boolean;
begin
  Result := cbClearParentTaxon.Checked;
end;

function TdlgEditHierarchy.GetClearSpecies: Boolean;
begin
  Result := cbClearSpecies.Checked;
end;

function TdlgEditHierarchy.GetClearSubfamily: Boolean;
begin
  Result := cbClearSubfamily.Checked;
end;

function TdlgEditHierarchy.GetClearSubspeciesGroup: Boolean;
begin
  Result := cbClearSubspeciesGroup.Checked;
end;

procedure TdlgEditHierarchy.GetValues;
begin
  if FParent > 0 then
    eParentTaxon.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FParent);
  if FOrder > 0 then
    eOrder.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FOrder);
  if FFamily > 0 then
    eFamily.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FFamily);
  if FSubfamily > 0 then
    eSubfamily.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FSubfamily);
  if FGenus > 0 then
    eGenus.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FGenus);
  if FSpecies > 0 then
    eSpecies.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FSpecies);
  if FSubspeciesGroup > 0 then
    eSubspeciesGroup.Text := GetName('zoo_taxa', 'full_name', 'taxon_id', FSubspeciesGroup);
end;

procedure TdlgEditHierarchy.sbApplyClick(Sender: TObject);
begin
  if not ValidateFields then
  begin
    MsgDlg(rsTitleInformation, 'At least one field must be filled to proceed.', mtInformation);
    Exit;
  end;

  ModalResult := mrOK;
end;

procedure TdlgEditHierarchy.sbApplyToSelectedClick(Sender: TObject);
begin
  if sbApplyToSelected.Down then
    FApplyTo := acSelected
  else
  if sbApplyToMarked.Down then
    FApplyTo := acMarked;
end;

function TdlgEditHierarchy.ValidateFields: Boolean;
begin
  Result := True;

  if ((not cbClearParentTaxon.Checked) and (not cbClearOrder.Checked) and (not cbClearFamily.Checked) and
    (not cbClearSubfamily.Checked) and (not cbClearGenus.Checked) and (not cbClearSpecies.Checked) and
    (not cbClearSubspeciesGroup.Checked)) and
    ((FParent = 0) and (FOrder = 0) and (FFamily = 0) and (FSubfamily = 0) and (FGenus = 0) and
    (FSpecies = 0) and (FSubspeciesGroup = 0)) then
    Result := False;
end;

initialization
  {$I udlg_edithierarchy.lrs}

end.

