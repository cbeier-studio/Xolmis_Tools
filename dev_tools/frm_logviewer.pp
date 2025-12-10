unit frm_logviewer;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, ComCtrls, ExtCtrls, laz.VirtualTrees, SynEdit, DateUtils, fgl, RegExpr,
  SynHighlighterSQL, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn, dev_types;

type

  { TfrmLogViewer }

  TfrmLogViewer = class(TForm)
    btnOpen: TBitBtn;
    btnSave: TBitBtn;
    btnClose: TBitBtn;
    eSearch: TEditButton;
    iButtons: TImageList;
    hlSQLSyn: TSynSQLSyn;
    OpenDlg: TOpenDialog;
    splitLogMessage: TSplitter;
    togError: TToggleBox;
    togInfo: TToggleBox;
    togWarning: TToggleBox;
    togDebug: TToggleBox;
    vstLog: TLazVirtualStringTree;
    pTop: TPanel;
    SBar: TStatusBar;
    synMessage: TSynEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure eSearchButtonClick(Sender: TObject);
    procedure eSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure togErrorChange(Sender: TObject);
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
    SearchText: String;
    procedure ApplyFilters;
    procedure LoadLogFile(const aFileName: String);
  public

  end;

var
  frmLogViewer: TfrmLogViewer;

implementation

{$R *.lfm}

{ TfrmLogViewer }

procedure TfrmLogViewer.ApplyFilters;
var
  Node: PVirtualNode;
  Data: PLogEntry;
  MatchesType, MatchesText: Boolean;
begin
  vstLog.BeginUpdate;
  try
    Node := vstLog.GetFirst;
    while Assigned(Node) do
    begin
      Data := vstLog.GetNodeData(Node);
      if Assigned(Data) then
      begin
        // Verifica se o tipo da mensagem deve ser exibido
        MatchesType :=
          ((Data^.LogLevel = 'Error')   and togError.Checked) or
          ((Data^.LogLevel = 'Info')    and togInfo.Checked) or
          ((Data^.LogLevel = 'Warning') and togWarning.Checked) or
          ((Data^.LogLevel = 'Debug')   and togDebug.Checked);

        // Filtro por texto
        if SearchText <> '' then
          MatchesText := (Pos(LowerCase(SearchText), LowerCase(Data^.Message)) > 0) or
            (Pos(LowerCase(SearchText), LowerCase(Data^.Details)) > 0)
        else
          MatchesText := True;

        // Se não passar nos filtros, marca como oculto
        if MatchesType and MatchesText then
          vstLog.IsVisible[Node] := True
        else
          vstLog.IsVisible[Node] := False;
      end;
      Node := vstLog.GetNext(Node);
    end;
  finally
    vstLog.EndUpdate;
  end;
end;

procedure TfrmLogViewer.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmLogViewer.btnOpenClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    LoadLogFile(OpenDlg.FileName);
end;

procedure TfrmLogViewer.eSearchButtonClick(Sender: TObject);
begin
  eSearch.Clear;
end;

procedure TfrmLogViewer.eSearchChange(Sender: TObject);
begin
  SearchText := Trim(eSearch.Text);
  ApplyFilters;
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

  togError.Checked := True;
  togInfo.Checked := True;
  togWarning.Checked := True;
  togDebug.Checked := True;

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
          //Entry.Details := Regex.Match[4]; // pode estar vazio
          if Regex.MatchLen[4] > 0 then
            DetailBuffer.Add(Trim(Regex.Match[4]))
          else
            DetailBuffer.Clear;
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

    SBar.Panels[0].Text := aFileName;
  finally
    Lines.Free;
  end;

end;

procedure TfrmLogViewer.togErrorChange(Sender: TObject);
begin
  ApplyFilters;
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

