unit udlg_edithierarchy;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons,
  atshapelinebgra, BCPanel;

type

  { TdlgEditHierarchy }

  TdlgEditHierarchy = class(TForm)
    BCPanel1: TBCPanel;
    BCPanel2: TBCPanel;
    BCPanel3: TBCPanel;
    BCPanel4: TBCPanel;
    BCPanel5: TBCPanel;
    BCPanel6: TBCPanel;
    BCPanel7: TBCPanel;
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
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    sbClose: TButton;
    sbApply: TBitBtn;
  private

  public

  end;

var
  dlgEditHierarchy: TdlgEditHierarchy;

implementation

initialization
  {$I udlg_edithierarchy.lrs}

end.

