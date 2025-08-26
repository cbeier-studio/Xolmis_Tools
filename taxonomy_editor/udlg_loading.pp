unit udlg_loading;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, BCFluentProgressRing,
  ColorSpeedButton;

type

  { TdlgLoading }

  TdlgLoading = class(TForm)
    btnCancel: TBitBtn;
    iButtons: TImageList;
    ringProgress: TBCFluentProgressRing;
    lblLoading: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BackupProgress(Sender: TObject; Remaining, PageCount: integer);
    procedure ZipperProgress(Sender: TObject; const Pct: Double);
  private
    function GetMin: Integer;
    function GetMax: Integer;
    function GetProgress: Integer;
    function GetShowCancel: Boolean;
    procedure SetMin(aValue: Integer);
    procedure SetMax(aValue: Integer);
    procedure SetProgress(aValue: Integer);
    procedure SetShowCancel(aValue: Boolean);
  public
    procedure UpdateProgress(const aMessage: String; aPercent: Integer; canCancel: Boolean = False);

    property Max: Integer read GetMax write SetMax;
    property Min: Integer read GetMin write SetMin;
    property Progress: Integer read GetProgress write SetProgress;
    property ShowCancel: Boolean read GetShowCancel write SetShowCancel;
  end;

var
  dlgLoading: TdlgLoading;

implementation

uses
  utils_global, utils_dialogs;

{$R *.lfm}

{ TdlgLoading }

procedure TdlgLoading.FormShow(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetRoundedCorners(Self.Handle, rcOn);
  {$ENDIF}
end;

function TdlgLoading.GetMax: Integer;
begin
  Result := ringProgress.MaxValue;
end;

function TdlgLoading.GetMin: Integer;
begin
  Result := ringProgress.MinValue;
end;

function TdlgLoading.GetProgress: Integer;
begin
  Result := ringProgress.Value;
end;

function TdlgLoading.GetShowCancel: Boolean;
begin
  Result := btnCancel.Visible;
end;

procedure TdlgLoading.SetMax(aValue: Integer);
begin
  ringProgress.MaxValue := aValue;
end;

procedure TdlgLoading.SetMin(aValue: Integer);
begin
  ringProgress.MinValue := aValue;
end;

procedure TdlgLoading.SetProgress(aValue: Integer);
begin
  ringProgress.Value := aValue;
  Application.ProcessMessages;
end;

procedure TdlgLoading.SetShowCancel(aValue: Boolean);
begin
  btnCancel.Visible := aValue;
end;

procedure TdlgLoading.BackupProgress(Sender: TObject; Remaining, PageCount: integer);
begin
  ringProgress.Value := 100 * (Remaining - PageCount) div PageCount;
end;

procedure TdlgLoading.btnCancelClick(Sender: TObject);
begin
  Parar := True;
  Hide;
end;

procedure TdlgLoading.UpdateProgress(const aMessage: String; aPercent: Integer; canCancel: Boolean);
begin
  lblLoading.Caption := aMessage;
  btnCancel.Visible := canCancel;
  if aPercent < 0 then
  begin
    ringProgress.Indeterminate := True;
  end
  else
  begin
    ringProgress.Indeterminate := False;
    ringProgress.Value := aPercent;
  end;
  Application.ProcessMessages;
end;

procedure TdlgLoading.ZipperProgress(Sender: TObject; const Pct: Double);
begin
  ringProgress.Value := Round(Pct);
end;

end.

