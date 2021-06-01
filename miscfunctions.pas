unit MiscFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleVectors;

function StripExtension(S: String): String;
function Vector3(const X: Single): TVector3; overload; inline;

implementation

function Vector3(const X: Single): TVector3;
begin
  Result.Data[0] := X;
  Result.Data[1] := X;
  Result.Data[2] := X;
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

