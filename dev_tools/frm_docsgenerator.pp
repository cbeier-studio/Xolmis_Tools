unit frm_docsgenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Graphics, Dialogs, AsyncProcess, EditBtn, StdCtrls, ComCtrls,
  LazUTF8, Buttons;

type

  { TfrmDocsGenerator }

  TfrmDocsGenerator = class(TForm)
    AsyncProcess1: TAsyncProcess;
    btnBuild: TBitBtn;
    btnServe: TBitBtn;
    btnStop: TBitBtn;
    btnClose: TBitBtn;
    eDocDir: TDirectoryEdit;
    iButtons: TImageList;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    procedure AsyncProcess1ReadData(Sender: TObject);
    procedure AsyncProcess1Terminate(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnServeClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
  private

  public

  end;

var
  frmDocsGenerator: TfrmDocsGenerator;

implementation

{$R *.lfm}

{ TfrmDocsGenerator }

procedure TfrmDocsGenerator.AsyncProcess1ReadData(Sender: TObject);
var
  Buffer: array[0..2047] of byte;
  BytesRead: LongInt;
  Raw: RawByteString;
  S: String;
begin
  with TAsyncProcess(Sender) do
  begin
    // Enquanto houver dados no pipe, leia
    while Output.NumBytesAvailable > 0 do
    begin
      BytesRead := Output.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
      begin
        SetLength(Raw, BytesRead);
        Move(Buffer[0], Raw[1], BytesRead);

        // Converte UTF-8 → encoding interno da aplicação
        S := UTF8ToString(Raw);

        Memo1.Lines.Add(S);
        Memo1.SelStart := Length(Memo1.Text);
      end;
    end;
  end;
end;

procedure TfrmDocsGenerator.AsyncProcess1Terminate(Sender: TObject);
begin
  Memo1.Lines.Add(Format('Process terminated with Exit Code: %d', [ExitCode]));
  // Re-enable a 'Start Build' button if needed
  eDocDir.Enabled := True;
  btnBuild.Enabled := True;
  btnClose.Enabled := True;
  btnServe.Enabled := True;
  btnStop.Enabled := False;
end;

procedure TfrmDocsGenerator.btnBuildClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Starting mkdocs process...');
  eDocDir.Enabled := False;
  btnServe.Enabled := False;
  btnBuild.Enabled := False;
  btnClose.Enabled := False;

  //AsyncProcess1.Environment.Add('PYTHONIOENCODING=UTF-8');
  AsyncProcess1.CurrentDirectory := eDocDir.Directory;
  AsyncProcess1.Executable := 'mkdocs';
  AsyncProcess1.Parameters.Clear;
  AsyncProcess1.Parameters.Add('build');
  AsyncProcess1.Execute;

  btnStop.Enabled := True;
end;

procedure TfrmDocsGenerator.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmDocsGenerator.btnServeClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Starting mkdocs process...');
  eDocDir.Enabled := False;
  btnServe.Enabled := False;
  btnBuild.Enabled := False;
  btnClose.Enabled := False;

  AsyncProcess1.CurrentDirectory := eDocDir.Directory;
  AsyncProcess1.Executable := 'mkdocs';
  AsyncProcess1.Parameters.Clear;
  AsyncProcess1.Parameters.Add('serve');
  AsyncProcess1.Execute;

  btnStop.Enabled := True;
end;

procedure TfrmDocsGenerator.btnStopClick(Sender: TObject);
begin
  if AsyncProcess1.Running then
    AsyncProcess1.Terminate(0);
end;

end.

