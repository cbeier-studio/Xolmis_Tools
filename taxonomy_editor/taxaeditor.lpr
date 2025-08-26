program TaxaEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  sdflaz,
  FrameViewer09,
  // Data
  data_types, data_core, data_select, data_crud, data_getvalue, data_validations,
  // Models
  models_base, models_rank, models_taxon,
  // Utils
  utils_global, utils_conversions, utils_dialogs, utils_taxonomy,
  // IO
  io_clements, io_ioc,
  // Forms
  ufrm_TaxaEditor, udlg_desttaxon, udlg_edithierarchy, udlg_sqlfilter,
  udlg_newsubspecies, udm_taxa;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  //Application.Title := 'Xolmis Taxonomies Editor';
  Application.Title := 'Xolmis Taxonomy Editor';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TdmTaxa, dmTaxa);
  Application.CreateForm(TfrmTaxaEditor, frmTaxaEditor);
  Application.Run;
end.

