unit utils_dialogs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Controls, StdCtrls, EditBtn,
  {$IFDEF MSWINDOWS} Windows, DwmApi, Messages,{$ENDIF}
  Forms, Dialogs, utils_taxonomy;

{$IFDEF MSWINDOWS}
type
  TRoundedWindowCornerType = (rcDefault, rcOff, rcOn, rcSmall);

const
  DWMWCP_DEFAULT    = 0; // Let the system decide whether or not to round window corners
  DWMWCP_DONOTROUND = 1; // Never round window corners
  DWMWCP_ROUND      = 2; // Round the corners if appropriate
  DWMWCP_ROUNDSMALL = 3; // Round the corners if appropriate, with a small radius

  DWMWA_WINDOW_CORNER_PREFERENCE = 33; // [set] WINDOW_CORNER_PREFERENCE, Controls the policy that rounds top-level window corners
{$ENDIF}

  function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl;
    UseValid: Boolean; var aResultKey: Integer; const aInitialValue: String = ''): Boolean; overload;
  function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl; aDataset: TDataset;
    aResultKeyField, aResultNameField: String; UseValid: Boolean; SaveOnClose: Boolean;
    const aInitialValue: String = ''): Boolean; overload;

  function MsgDlg(aTitle, aText: String; aType: TMsgDlgType): Boolean;
  {$IFDEF MSWINDOWS}
  procedure SetRoundedCorners(const TheHandle: HWND; const CornerType: TRoundedWindowCornerType);
  {$ENDIF}

implementation

uses
  data_types, utils_global, udm_taxa, udlg_find;

function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl; UseValid: Boolean; var aResultKey: Integer;
  const aInitialValue: String): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  {$IFDEF DEBUG}
  LogDebug('OPEN DIALOG: Find taxon');
  {$ENDIF}
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := tbZooTaxa;
    TaxonFilter := aFiltro;
    //UsarValido := UseValid;
    //GetFormPosition(aControl, WindowPos);
    if Assigned(aControl) then
    begin
      //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
      PControl := aControl.ClientOrigin;
      SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    end
    else
      Position := poScreenCenter;
    InitialValue := aInitialValue;
    if ShowModal = mrOK then
    begin
      aResultKey := dlgFind.KeySelected;
      if Assigned(aControl) then
        if aControl is TEditButton then
        begin
          TEditButton(aControl).Text := dlgFind.NameSelected;
          TEditButton(aControl).Modified := True;
        end
        else
        if aControl is TCustomEdit then
        begin
          TCustomEdit(aControl).Text := dlgFind.NameSelected;
          TCustomEdit(aControl).Modified := True;
        end;
      Result := True;
    end;
  finally
    FreeAndNil(dlgFind);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: Find taxon');
    {$ENDIF}
  end;
end;

function FindTaxonDlg(aFiltro: TTaxonFilters; aControl: TControl; aDataset: TDataset; aResultKeyField,
  aResultNameField: String; UseValid: Boolean; SaveOnClose: Boolean; const aInitialValue: String): Boolean;
var
  PControl: TPoint;
begin
  Result := False;
  {$IFDEF DEBUG}
  LogDebug('OPEN DIALOG: Find taxon');
  {$ENDIF}
  dlgFind := TdlgFind.Create(nil);
  with dlgFind do
  try
    TableType := tbZooTaxa;
    TaxonFilter := aFiltro;
    //UsarValido := UseValid;
    //GetFormPosition(aControl, WindowPos);
    //PControl := aControl.ClientToScreen(Point(aControl.Left, aControl.Top));
    PControl := aControl.ClientOrigin;
    SetDialogPosition(PControl.X, PControl.Y, aControl.Width, aControl.Height);
    InitialValue := aInitialValue;
    Result := ShowModal = mrOK;
    if Result then
    begin
      //CanEdit(aDataSet);
      aDataSet.FieldByName(aResultKeyField).AsInteger := dlgFind.KeySelected;
      aDataSet.FieldByName(aResultNameField).AsString := dlgFind.NameSelected;
      if SaveOnClose then
        aDataSet.Post;
      if aControl is TCustomEdit then
        TCustomEdit(aControl).Modified := True;
    end else
    begin
      if SaveOnClose then
        aDataSet.Cancel;
    end;
  finally
    FreeAndNil(dlgFind);
    {$IFDEF DEBUG}
    LogDebug('CLOSE DIALOG: Find taxon');
    {$ENDIF}
  end;
end;

function MsgDlg(aTitle, aText: String; aType: TMsgDlgType): Boolean;
begin
  Result := False;

  with dmTaxa.TaskDlg do
  begin
    Flags := Flags - [tfShowProgressBar];
    FooterText := EmptyStr;
    FooterIcon := tdiNone;
    Title := aTitle;
    Text := aText;
    case aType of
      mtError:
        begin
          Caption := rsTitleError;
          CommonButtons := [tcbOk];
          MainIcon := tdiError;
          DefaultButton := tcbOk;
        end;
      mtConfirmation:
        begin
          Caption := rsTitleConfirmation;
          CommonButtons := [tcbYes, tcbNo];
          MainIcon := tdiNone;
          DefaultButton := tcbNo;
        end;
      mtInformation:
        begin
          Caption := rsTitleInformation;
          CommonButtons := [tcbOK];
          MainIcon := tdiInformation;
          DefaultButton := tcbOK;
        end;
      mtWarning:
        begin
          Caption := rsTitleCaution;
          CommonButtons := [tcbOK];
          MainIcon := tdiWarning;
          DefaultButton := tcbOK;
        end;
    end;
    if Execute then
    begin
      case ModalResult of
        mrOK, mrYes:
          Result := True;
        mrCancel, mrNo:
          Result := False;
      end;
    end
    else
      Result := False;
  end;
end;

{$IFDEF MSWINDOWS}
procedure SetRoundedCorners(const TheHandle: HWND; const CornerType: TRoundedWindowCornerType);
var
  DWM_WINDOW_CORNER_PREFERENCE: Cardinal;
begin
  if InitDwmLibrary then
  begin
    case CornerType of
      rcOff:
        DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_DONOTROUND;
      rcOn:
        DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_ROUND;
      rcSmall:
        DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_ROUNDSMALL;
    else
      DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_DEFAULT;
    end;
    DwmSetWindowAttribute(TheHandle, DWMWA_WINDOW_CORNER_PREFERENCE,
      @DWM_WINDOW_CORNER_PREFERENCE, sizeof(DWM_WINDOW_CORNER_PREFERENCE));
  end;
end;
{$ENDIF}

end.

