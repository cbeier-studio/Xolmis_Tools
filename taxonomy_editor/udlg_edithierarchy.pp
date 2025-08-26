unit udlg_edithierarchy;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons,
  atshapelinebgra, BCPanel, utils_taxonomy;

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
  private
    FApplyTo: TApplyChangesTo;
    FParent: Integer;
    FOrder, FFamily, FSubfamily, FGenus, FSpecies, FSubspeciesGroup: Integer;
  public
    property ApplyTo: TApplyChangesTo read FApplyTo write FApplyTo;
    property ParentTaxon: Integer read FParent write FParent;
    property Order: Integer read FOrder write FOrder;
    property Family: Integer read FFamily write FFamily;
    property Subfamily: Integer read FSubfamily write FSubfamily;
    property Genus: Integer read FGenus write FGenus;
    property Species: Integer read FSpecies write FSpecies;
    property SubspeciesGroup: Integer read FSubspeciesGroup write FSubspeciesGroup;
  end;

var
  dlgEditHierarchy: TdlgEditHierarchy;

implementation

initialization
  {$I udlg_edithierarchy.lrs}

end.

