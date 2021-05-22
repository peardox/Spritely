program AniConv;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, JsonTools, AniTxtJson, castle_base
  { you can add units after this };

type

  { AniList }

  AniList = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ AniList }

procedure AniList.DoRun;
var
  FileName: String;
  ErrorMsg: String;
  Json: TJsonNode;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  FileName := 'C:\Assets\3drt\paid\Elf-Males\elfrangers-aniamtions-list.txt';
  WriteLn('Parsing ' + FileName);
  Json := AniTxtToJson(FileName);
  if not(Json = nil) then
    begin
      WriteLn('Saving to ' + FileName + '.json');
      Json.SaveToFile(FileName + '.json', True);
      JSon.Free;
    end
  else
    WriteLn('Errors encountered converting file.');

  // stop program loop
  Terminate;
end;

constructor AniList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor AniList.Destroy;
begin
  inherited Destroy;
end;

procedure AniList.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: AniList;
begin
  Application:=AniList.Create(nil);
  Application.Title:='Animation List Converter';
  Application.Run;
  Application.Free;
end.

