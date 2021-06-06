unit MiscFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, CastleVectors, CastleUtils;

function StripExtension(S: String): String;
function WrapRadians(const AValue: Single): Single;

implementation

function WrapRadians(const AValue: Single): Single;
begin
  Result := (FloatModulo(AValue + Pi, 2 * Pi)) - Pi;
end;

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

