unit AniTakeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleLog, JsonTools, CastleDownload, CastleTimeUtils;

type
  { TAniTake }
  TAniTake = record
    TakeName: String;
    TakeStart: Integer;
    TakeStop: Integer;
  end;
  PAniTake = ^TAniTake;
  TAniTakeArray = Array of TAniTake;

const
  AniDefaultTakeFPS: Cardinal = 30;

function AniTakeFromJson(const Json: TJsonNode): TAniTakeArray;

implementation

function AniTakeFromJson(const Json: TJsonNode): TAniTakeArray;
var
  Node: TJsonNode;
  Element: TJsonNode;
  Data: TJsonNode;
  Take: TAniTake;
  I: Integer;
begin
  Result := nil;
  for Node in Json do
    begin
      if (Node.Kind = nkArray) and (Node.Name = 'Animations') then
        begin
          WriteLnLog('AnimationCount : ' + IntToStr(Node.Count));
          SetLength(Result, Node.Count);
          for I := 0 to Node.Count - 1 do
            begin
              Element := Node.Child(I);
              Take := default(TAniTake);
              Take.TakeStart := -1; // To trap missing values
              for Data in Element do
                begin
                  case Data.Name of
                    'FromFrame':
                      begin
                        if Data.Kind = nkNumber then
                          Take.TakeStart := Data.AsInteger
                        else
                          raise Exception.Create('FromFrame is not numeric');
                      end;
                    'ToFrame':
                      begin
                        if Data.Kind = nkNumber then
                          Take.TakeStop := Data.AsInteger
                        else
                          raise Exception.Create('ToFrame is not numeric');
                      end;
                    'Action':
                      begin
                        if Data.Kind = nkString then
                          begin
                            Take.TakeName := Data.AsString;
                            if (Take.TakeName = EmptyStr) then
                              raise Exception.Create('Action is blank');
                          end
                        else
                          raise Exception.Create('Action is not a string');
                      end;
                  end;
                end;
              if not(Take.TakeName = EmptyStr) and not(Take.TakeStart = -1) and not(Take.TakeStop = 0) then
                Result[I] := Take
              else
                raise Exception.Create('Partial record found for Take #' + IntToStr(I));
            end;
        end
      else
        WriteLnLog('Node : ' + Node.Name + ' =>' + Node.KindAsString);
    end;
end;

end.

