unit utils_conversions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, RegExpr;

  function WildcardWords(aText: String; aWildcard: String = '%'): String;
  function WildcardSyllables(aText: String; aWildcard: String = '%'): String;

implementation

function WildcardWords(aText: String; aWildcard: String): String;
var
  i, total: Integer;
  Words: TStringList;
begin
  Result := EmptyStr;

  Words := TStringList.Create; // ('"', ' ', [soStrictDelimiter]);
  try
    total := ExtractStrings([' '], [' '], PAnsiChar(aText), Words);
    if total > 1 then
      for i := 0 to Words.Count - 1 do
        if ExecRegExpr('^#[a-z]+$', Words[i]) then
          Result := Result + ''
        else
          if i = 0 then
            Result := AnsiDequotedStr(Words[i], '"') + aWildcard + ' '
          else
            if i = Words.Count - 1 then
              Result := Result + aWildcard + AnsiDequotedStr(Words[i], '"')
            else
              Result := Result + aWildcard + AnsiDequotedStr(Words[i], '"') + aWildcard + ' ';
    if total = 1 then
      if ExecRegExpr('^#[a-z]+$', aText) then
        Result := ''
      else
        Result := AnsiDequotedStr(aText, '"');
  finally
    FreeAndNil(Words);
  end;
end;

function WildcardSyllables(aText: String; aWildcard: String): String;
var
  i, total: Integer;
  Syllables: TStringList;
begin
  Result := EmptyStr;

  Syllables := TStringList.Create; // ('"', ' ', [soStrictDelimiter]);
  try
    total := ExtractStrings([' ', '+'], [' '], PAnsiChar(aText), Syllables);
    if total > 1 then
      for i := 0 to Syllables.Count - 1 do
        if ExecRegExpr('^#[a-z]+$', Syllables[i]) then
          Result := Result + ''
        else
          if i = 0 then
            Result := AnsiDequotedStr(Syllables[i], '"') + aWildcard
          else
            if i = Syllables.Count - 1 then
              Result := Result + AnsiDequotedStr(Syllables[i], '"')
            else
              Result := Result + AnsiDequotedStr(Syllables[i], '"') + aWildcard;
    if total = 1 then
      if ExecRegExpr('^#[a-z]+$', aText) then
        Result := ''
      else
        Result := AnsiDequotedStr(aText, '"');
  finally
    FreeAndNil(Syllables);
  end;
end;

end.

