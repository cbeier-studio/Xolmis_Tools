unit dm_dev;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, SQLDB, SQLDBLib, SQLite3Conn, DB;

type

  { TDMD }

  TDMD = class(TDataModule)
    dsReports: TDataSource;
    dsFields: TDataSource;
    dsTables: TDataSource;
    imgBtns: TImageList;
    OpenDlg: TOpenDialog;
    qFieldsdarwin_core_name: TStringField;
    qFieldsdisplay_name: TStringField;
    qFieldsfield_name: TStringField;
    qFieldsfield_type: TStringField;
    qFieldsfilter_type: TStringField;
    qFieldsinsert_date: TDateTimeField;
    qFieldsinteger_key: TBooleanField;
    qFieldslookup_key: TStringField;
    qFieldslookup_name: TStringField;
    qFieldslookup_result: TStringField;
    qFieldslookup_table: TStringField;
    qFieldsmaximum_value: TFloatField;
    qFieldsminimum_value: TFloatField;
    qFieldssorted_status: TBooleanField;
    qFieldssort_num: TLongintField;
    qFieldstable_name: TStringField;
    qFieldstext_key: TBooleanField;
    qFieldsupdate_date: TDateTimeField;
    qFieldsvalue_list: TMemoField;
    qFieldsvisible_status: TBooleanField;
    qTablesdisplay_name: TStringField;
    qTablesexport_show: TBooleanField;
    qTablesfilter_show: TBooleanField;
    qTablesimport_show: TBooleanField;
    qTablesinsert_date: TDateTimeField;
    qTablestable_name: TStringField;
    qTablesupdate_date: TDateTimeField;
    qTablesvisible_status: TBooleanField;
    sqlCon: TSQLConnector;
    sqlLibLoader: TSQLDBLibraryLoader;
    qTables: TSQLQuery;
    qFields: TSQLQuery;
    qReports: TSQLQuery;
    sqlTrans: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure sqlConAfterConnect(Sender: TObject);
    procedure sqlConBeforeConnect(Sender: TObject);
  private

  public

  end;

var
  DMD: TDMD;

implementation

uses dev_types;

{$R *.lfm}

{ TDMD }

procedure TDMD.DataModuleCreate(Sender: TObject);
begin
  sqlCon.Close();
  sqlCon.DatabaseName := EmptyStr;
  if not Application.Terminated then
  begin
    sqlCon.Open;
  end;
end;

procedure TDMD.DataModuleDestroy(Sender: TObject);
begin
  sqlCon.Close();
end;

procedure TDMD.sqlConAfterConnect(Sender: TObject);
begin
  if not qTables.Active then
    qTables.Open;
  if not qFields.Active then
    qFields.Open;
end;

procedure TDMD.sqlConBeforeConnect(Sender: TObject);
begin
  if sqlCon.DatabaseName = EmptyStr then
  begin
    OpenDlg.InitialDir := AppDataDir;
    if OpenDlg.Execute then
    begin
      sqlCon.DatabaseName := OpenDlg.FileName;
    end
    else
      Application.Terminate;
  end;
end;

end.

