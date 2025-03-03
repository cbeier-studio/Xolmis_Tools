unit dev_types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, lcltype, lclintf, Windows;

const
  NomeApp: String = 'Xolmis Dev Tools';

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

