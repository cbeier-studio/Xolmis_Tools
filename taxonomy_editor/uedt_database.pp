unit uedt_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons,
  EditBtn, ComCtrls, atshapelinebgra;

type

  { TedtDatabase }

  TedtDatabase = class(TForm)
    eDBPass: TEditButton;
    eDBFile: TFileNameEdit;
    eDBUser: TEdit;
    iButtons: TImageList;
    lblTitleAuthentication: TLabel;
    lblDBFile: TLabel;
    lblDBPass: TLabel;
    lblDBUser: TLabel;
    lineBottom: TShapeLineBGRA;
    OpenDlg: TOpenDialog;
    pTitleAuthentication: TPanel;
    pMain: TPanel;
    pBottom: TPanel;
    sbCancel: TButton;
    sbSave: TButton;
    procedure eDBFileChange(Sender: TObject);
    procedure eDBPassButtonClick(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure sbCancelClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
  private
    FIsNew: Boolean;
    function IsRequiredFilled: Boolean;
    //procedure ApplyDarkMode;
    procedure GetRecord;
    procedure SetRecord;
  public
    property IsNew: Boolean read FIsNew write FIsNew;
  end;

var
  edtDatabase: TedtDatabase;

implementation

uses
  utils_global, data_types, udm_taxa;

{$R *.lfm}

{ TedtDatabase }

//procedure TedtDatabase.ApplyDarkMode;
//begin
//  eDBFile.Images := DMM.iEditsDark;
//  eDBPass.Images := iButtonsDark;
//end;

procedure TedtDatabase.eDBFileChange(Sender: TObject);
begin
  sbSave.Enabled := IsRequiredFilled;
end;

procedure TedtDatabase.eDBPassButtonClick(Sender: TObject);
begin
  if eDBPass.EchoMode = emNormal then
  begin
    // hide password
    eDBPass.EchoMode := emPassword;
    eDBPass.ImageIndex := 0;
  end
  else
  begin
    // show password
    eDBPass.EchoMode := emNormal;
    eDBPass.ImageIndex := 1;
  end;
end;

procedure TedtDatabase.eNameKeyPress(Sender: TObject; var Key: char);
begin
  FormKeyPress(Sender, Key);

  { <ENTER/RETURN> key }
  if (Key = #13) then
  begin
    SelectNext(Sender as TWinControl, True, True);
    Key := #0;
  end;
end;

procedure TedtDatabase.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  { SAVE = Ctrl + S }
  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0;
    //if not (dsConn.State in [dsInsert, dsEdit]) then
    //  Exit;

    sbSaveClick(nil);
  end;
end;

procedure TedtDatabase.FormKeyPress(Sender: TObject; var Key: char);
begin
  { <ESC> key }
  if (Key = #27) then
  begin
    Key := #0;

    ModalResult := mrCancel; { Cancel insert/edit }
  end;
end;

procedure TedtDatabase.FormShow(Sender: TObject);
begin
  //if IsDarkModeEnabled then
  //  ApplyDarkMode;

  eDBFile.InitialDir := InstallDir;
  eDBFile.DialogTitle := 'Select database file';

  if not FIsNew then
    GetRecord;
end;

procedure TedtDatabase.GetRecord;
begin
  eDBFile.FileName := databaseConnection.Database;
  eDBUser.Text := databaseConnection.UserName;
  eDBPass.Text := databaseConnection.Password;
end;

function TedtDatabase.IsRequiredFilled: Boolean;
begin
  Result := False;

  if (eDBFile.Text <> EmptyStr) then
    Result := True;
end;

procedure TedtDatabase.sbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TedtDatabase.sbSaveClick(Sender: TObject);
begin
  SetRecord;

  edtDatabase.ModalResult := mrOk;
end;

procedure TedtDatabase.SetRecord;
begin
  databaseConnection.Database := eDBFile.FileName;
  databaseConnection.UserName := eDBUser.Text;
  databaseConnection.Password := eDBPass.Text;

  databaseConnection.SaveParams;
end;

end.

