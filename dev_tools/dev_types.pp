unit dev_types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, lcltype, lclintf, Windows, fgl;

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

  //TLogEntries = specialize TFPGList<TLogEntry>;

  { System variables }
  function InstallDir: String;
  function AppDataDir: String;
  function TempDir: String;

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

end.

