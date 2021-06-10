unit SpritelyLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleLog, CastleApplicationProperties,
  CastleClassUtils, CastleNotifications;

type
  { Log handler sending logs to server using asynchronous HTTP POST. }
  TLogHandler = class(TComponent)
  strict private
    InsideLogCallback: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LogCallback(const Message: String);
  end;

var
  LogHandler: TLogHandler;

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}

constructor TLogHandler.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TLogHandler.LogCallback(const Message: String);
begin
  if InsideLogCallback then
    Exit;

  InsideLogCallback := True;
  try
    {$ifdef cgeapp}
    WriteLn(stderr, Message);
    {$else}
    with CastleForm do
      begin
        Applog.Lines.Add(Message);
      end;
    {$endif}
  finally
    InsideLogCallback := False;
  end;
end;

end.

