unit AniTakeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JsonTools, CastleDownload, CastleTimeUtils;

type
  { TAniTake }
  TAniTake = Class(TComponent)
    private
      AnimName: String;
      AnimStart: TFloatTime;
      AnimStop: TFloatTime;
    public
      constructor Create(AOwner: TComponent); override;
      constructor Create(AOwner: TComponent; const AName: String; const AStart: Cardinal; const AStop: Cardinal);
  end;
  PAniTake = ^TAniTake;
  TAniTakeArray = Array of TAniTake;

const
  AniTakeFPS: Cardinal = 30;

function AniTakeFromJson(const Json: TJsonNode): TAniTakeArray;

implementation

constructor TAniTake.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

constructor TAniTake.Create(AOwner: TComponent; const AName: String; const AStart: Cardinal; const AStop: Cardinal);
begin
  Create(AOwner);
  AnimName := AName;
  AnimStart := TFloatTime(AStart / AniTakeFPS);
  AnimStop := TFloatTime((AStop + 1) / AniTakeFPS);
end;

function AniTakeFromJson(const Json: TJsonNode): TAniTakeArray;
var
  Node: TJsonNode;
begin
  Result := nil;
  if (Json.Kind = nkArray) and (Json.Name = 'Animations') then
    begin
//      for Node in Json
    end;
end;

end.

