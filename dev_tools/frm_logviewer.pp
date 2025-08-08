unit frm_logviewer;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, ComCtrls, ExtCtrls, laz.VirtualTrees, SynEdit, DateUtils, fgl, RegExpr,
  SynHighlighterSQL, SysUtils, Forms, Controls, Graphics, Dialogs, dev_types;

type

  { TfrmLogViewer }

  TfrmLogViewer = class(TForm)
    btnOpen: TBitBtn;
    btnSave: TBitBtn;
    btnClose: TBitBtn;
    iButtons: TImageList;
    hlSQLSyn: TSynSQLSyn;
    OpenDlg: TOpenDialog;
    splitLogMessage: TSplitter;
    vstLog: TLazVirtualStringTree;
    pTop: TPanel;
    SBar: TStatusBar;
    synMessage: TSynEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstLogBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstLogFocusChanged
      (Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstLogFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstLogGetText
      (Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vstLogInitNode
      (Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    LogEntries: array of TLogEntry;
    procedure LoadLogFile(const aFileName: String);
  public

  end;

var
  frmLogViewer: TfrmLogViewer;

implementation

{$R *.lfm}

{ TfrmLogViewer }

procedure TfrmLogViewer.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmLogViewer.btnOpenClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    LoadLogFile(OpenDlg.FileName);
end;

procedure TfrmLogViewer.FormCreate(Sender: TObject);
begin
  vstLog.Clear;
  synMessage.Font.Quality := fqDefault;
  synMessage.Lines.Text := '';
end;

procedure TfrmLogViewer.FormDestroy(Sender: TObject);
begin
  SetLength(LogEntries, 0);
end;

procedure TfrmLogViewer.LoadLogFile(const aFileName: String );
var
  i: Integer;
  Node: PVirtualNode;
  LogPtr: PLogEntry;
  Lines: TStringList;
  Entry: TLogEntry;
  Regex: TRegExpr;
  Line: string;
  DetailBuffer: TStringList;
begin
  if not FileExists(aFileName) then
    Exit;

  SetLength(LogEntries, 0);

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(aFileName);

    Regex := TRegExpr.Create;
    DetailBuffer := TStringList.Create;
    vstLog.Clear;
    vstLog.BeginUpdate;
    try
      Regex.Expression := '^ \[(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{3}) (\w+)\] (.+?)(?: \| (.*))?$';
      i := 0;
      while i < Lines.Count do
      begin
        Line := Lines[i];
        if Regex.Exec(Line) then
        begin
          // Salva detalhes acumulados no último entry
          if (High(LogEntries) > 0) and (DetailBuffer.Count > 0) then
          begin
            LogEntries[High(LogEntries)].Details := Trim(DetailBuffer.Text);
            DetailBuffer.Clear;
          end;

          SetLength(LogEntries, Length(LogEntries) + 1);
          Entry := TLogEntry.Create;
          //New(Entry);
          Entry.LogDate := ISO8601ToDate(Copy(Regex.Match[1], 1, 19));
          Entry.LogLevel := Regex.Match[2];
          Entry.Message := Regex.Match[3];
          Entry.Details := Regex.Match[4]; // pode estar vazio
          LogEntries[High(LogEntries)] := Entry;
          Node := vstLog.AddChild(nil, Entry);
        end
        else
        begin
          // Linha de detalhe adicional
          if High(LogEntries) > 0 then
            DetailBuffer.Add(Line);
        end;
        Inc(i);
      end;

      // Finaliza último detalhe
      if (High(LogEntries) > 0) and (DetailBuffer.Count > 0) then
        LogEntries[High(LogEntries)].Details := Trim(DetailBuffer.Text);
    finally
      vstLog.EndUpdate;
      Regex.Free;
      DetailBuffer.Free;
    end;
  finally
    Lines.Free;
  end;

end;

procedure TfrmLogViewer.vstLogBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  LogPtr: PLogEntry;
begin
  if Column = 1 then
  begin
    LogPtr := Sender.GetNodeData(Node);
    if Assigned(LogPtr) then
    begin
      if LogPtr^.LogLevel = 'Error' then
        TargetCanvas.Brush.Color := $009597E7
      else if LogPtr^.LogLevel = 'Warning' then
        TargetCanvas.Brush.Color := $0065EFFF
      else if LogPtr^.LogLevel = 'Info' then
        TargetCanvas.Brush.Color := $00E4D9B0
      else if LogPtr^.LogLevel = 'Debug' then
        TargetCanvas.Brush.Color := clWhite;

      TargetCanvas.FillRect(CellRect);
    end;
  end;
end;

procedure TfrmLogViewer.vstLogFocusChanged
  (Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLogEntry;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    synMessage.Lines.Text := Data^.Details;
end;

procedure TfrmLogViewer.vstLogFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PLogEntry;
begin
  Data := Sender.GetNodeData(Node);
  Data^.Free;
end;

procedure TfrmLogViewer.vstLogGetText
  (Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PLogEntry;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    case Column of
      0: CellText := DateTimeToStr(Data^.LogDate);
      1: CellText := Data^.LogLevel;
      2: CellText := Data^.Message;
    end;
  end;
end;

procedure TfrmLogViewer.vstLogInitNode
  (Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  vstLog.NodeDataSize := SizeOf(TLogEntry);
end;

end.

