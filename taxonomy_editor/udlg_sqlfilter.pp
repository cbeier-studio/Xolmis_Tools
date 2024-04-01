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
    SpeedButton1: TSpeedButton;
    SynCompletion: TSynCompletion;
    SynSQLSyn: TSynSQLSyn;
  private

  public

  end;

var
  dlgSqlFilter: TdlgSqlFilter;

implementation

initialization
  {$I udlg_sqlfilter.lrs}

end.

