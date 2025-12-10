unit dev_types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, lcltype, lclintf, {$IFDEF MSWINDOWS} Windows, DwmApi, Messages,{$ENDIF} fgl;

const
  NomeApp: String = 'Xolmis Dev Tools';

type
  TLogEntry = class
    LogDate: TDateTime;
    LogLevel: string; // Ex.: "Info", "Debug"
    Message: string;  // Mensagem principal
    Details: string;  // Detalhes adicionais (como SQL)
  end;
  PLogEntry = ^TLogEntry;

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

var
  stopProcess: Boolean = False;

  //TLogEntries = specialize TFPGList<TLogEntry>;

  { System variables }
  function InstallDir: String;
  function AppDataDir: String;
  function TempDir: String;

  {$IFDEF MSWINDOWS}
  procedure SetRoundedCorners(const TheHandle: HWND; const CornerType: TRoundedWindowCornerType);
  {$ENDIF}

implementation

function InstallDir: String;
var
  s: String;
begin
  s := ExtractFilePath(Application.ExeName);
  s := IncludeTrailingPathDelimiter(s);

  Result := s;
end;

function AppDataDir: String;
var
  s: String;
begin
  s := GetAppConfigDir(False);
  s := IncludeTrailingPathDelimiter(s);

  Result:= s;
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function TempDir: String;
var
  s: String;
begin
  s := ConcatPaths([GetTempDir(False), NomeApp]);
  s := IncludeTrailingPathDelimiter(s);

  Result := s;
  if not DirectoryExists(Result) then
    CreateDir(Result);
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

