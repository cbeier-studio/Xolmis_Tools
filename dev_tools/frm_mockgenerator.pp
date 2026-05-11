unit frm_mockgenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  dev_mock_generator;

type

  { TfrmMockGenerator }

  TfrmMockGenerator = class(TForm)
    btnClose: TButton;
    btnGenerate: TButton;
    eConfigFile: TFileNameEdit;
    lblConfig: TLabel;
    MemoLog: TMemo;
    procedure btnCloseClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmMockGenerator: TfrmMockGenerator;

implementation

{$R *.lfm}

{ TfrmMockGenerator }

procedure TfrmMockGenerator.btnGenerateClick(Sender: TObject);
var
  Generator: TMockDataGenerator;
begin
  if Trim(eConfigFile.FileName) = '' then
    raise Exception.Create('Please choose a generator-config.json file.');

  MemoLog.Lines.Clear;
  MemoLog.Lines.Add('Loading configuration...');

  Generator := TMockDataGenerator.Create;
  try
    Generator.LoadConfig(eConfigFile.FileName);
    MemoLog.Lines.Add('Generating mock data...');
    Generator.GenerateAll;
    MemoLog.Lines.Add('Generation finished successfully.');
  finally
    Generator.Free;
  end;
end;

procedure TfrmMockGenerator.FormCreate(Sender: TObject);
begin
  eConfigFile.Filter := 'JSON files (*.json)|*.json|All files (*.*)|*.*';
  // The config file is expected at the project root (same folder as the executable).
  // Schema and repository paths inside the config are resolved relative to the config file.
  // Example: "file": "resources/table-persons.json" resolves from the config's directory.
  eConfigFile.FileName := ExpandFileName('generator-config.json');
end;

procedure TfrmMockGenerator.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
