unit udlg_desttaxon;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, CheckLst, EditBtn, StdCtrls,
  Buttons, atshapelinebgra, BCPanel;

type

  { TdlgDestTaxon }

  TdlgDestTaxon = class(TForm)
    BCPanel1: TBCPanel;
    cklTaxonomy: TCheckListBox;
    eDestinationTaxon: TEditButton;
    lblDestinationTaxon: TLabel;
    lineBottom: TShapeLineBGRA;
    pBottom: TPanel;
    sbClose: TButton;
    sbApply: TBitBtn;
  private

  public

  end;

var
  dlgDestTaxon: TdlgDestTaxon;

implementation

initialization
  {$I udlg_desttaxon.lrs}

end.

