unit frm_settingseditor;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, ComCtrls, ExtCtrls, StdCtrls, SysUtils, Forms, Controls,
  Graphics, Dialogs, ValEdit, fpjson, jsonConf;

type

  { TfrmSettingsEditor }

  TfrmSettingsEditor = class(TForm)
    btnOpen: TBitBtn;
    btnSave: TBitBtn;
    btnClose: TBitBtn;
    iButtons: TImageList;
    OpenDlg: TOpenDialog;
    SectionList: TListBox;
    pTop: TPanel;
    SBar: TStatusBar;
    gridEdit: TValueListEditor;
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SectionListSelectionChange(Sender: TObject; User: boolean);
  private
    FFilename: String;
    ConfigData: TJSONData;
    ConfigObj, SectionObj: TJSONObject;

    procedure LoadFromFile(aFileName: String);
    procedure LoadSectionValues(SectionName: String);
  public
    property Filename: String read FFilename write FFilename;
  end;

var
  frmSettingsEditor: TfrmSettingsEditor;

implementation

{$R *.lfm}

{ TfrmSettingsEditor }

procedure TfrmSettingsEditor.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSettingsEditor.btnOpenClick(Sender: TObject);
begin
  if OpenDlg.Execute then
  begin
    FFilename := OpenDlg.FileName;
    LoadFromFile(FFileName);
  end;
end;

procedure TfrmSettingsEditor.FormDestroy(Sender: TObject);
begin
  if Assigned(ConfigData) then
    ConfigData.Free;
end;

procedure TfrmSettingsEditor.LoadFromFile(aFileName: String);
var
  FileStream: TFileStream;
  i: Integer;
begin
  if (aFileName = EmptyStr) then
    Exit;
  if not FileExists(FFileName) then
    Exit;

  SBar.Panels[0].Text := FFileName;

  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    ConfigData := GetJSON(FileStream);

    SectionList.Items.Clear;

    if Assigned(ConfigData) and (ConfigData is TJSONObject) then
    begin
      ConfigObj := TJSONObject(ConfigData);
      for i := 0 to ConfigObj.Count - 1 do
      begin
        SectionList.Items.Add(ConfigObj.Names[i]);
      end;
    end;

  finally
    FileStream.Free;
  end;
end;

procedure TfrmSettingsEditor.LoadSectionValues(SectionName: String);
var
  i: Integer;
begin
  gridEdit.Strings.Clear;

  if Assigned(ConfigData) and (ConfigData is TJSONObject) then
  begin
    SectionObj := TJSONObject(ConfigData).Find(SectionName) as TJSONObject;
    if Assigned(SectionObj) then
    begin
      for i := 0 to SectionObj.Count - 1 do
      begin
        gridEdit.InsertRow(SectionObj.Names[i], SectionObj.Items[i].AsString, True);
      end;

      gridEdit.AutoAdjustColumns;
    end;
  end;

end;

procedure TfrmSettingsEditor.SectionListSelectionChange(Sender: TObject; User: boolean);
begin
  if Assigned(ConfigData) then
    LoadSectionValues(SectionList.GetSelectedText);
end;

end.

