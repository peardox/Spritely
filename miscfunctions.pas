unit MiscFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function StripExtension(S: String): String;

implementation

function StripExtension(S: String): String;
var
  I: SizeInt;
begin
  Result := S;
  I := S.IndexOf('.');
  if(I >= 0) then
    Result := S.Remove(I);
end;

end.

