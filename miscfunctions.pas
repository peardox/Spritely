unit MiscFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, CastleVectors, CastleUtils;

function StripExtension(S: String): String;
function WrapRadians(const AValue: Single): Single;
function RadsToFace(const AValue: Single): Cardinal;

implementation

function WrapRadians(const AValue: Single): Single;
begin
  Result := FloatModulo(AValue + Pi, 2 * Pi) - Pi;
end;

function RadsToFace(const AValue: Single): Cardinal;
begin
  Result := Round(FloatModulo(AValue, 2 * Pi) / (Pi / 2)) Mod 4;
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

