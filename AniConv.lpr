program AniConv;

{$mode objfpc}{$H+}
// {$define workingcopy}

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

{$ifdef workingcopy}
const AList: Array[0..11] of String = (
  'C:\Assets\3drt\paid\chibii-racers-dirt-bikes\_bike_animations.txt',
  'C:\Assets\3drt\paid\Crazy-Rabbits\crazy-rabbits-animations-list.txt',
  'C:\Assets\3drt\paid\Elf-Females\souldblade-elfrangers-aniamtions-list.txt',
  'C:\Assets\3drt\paid\Elf-Males\elfrangers-aniamtions-list.txt',
  'C:\Assets\3drt\paid\Ghouls\animations-list-ghouls.txt',
  'C:\Assets\3drt\paid\Goblins-Undead\goblin_anim.txt',
  'C:\Assets\3drt\paid\Spiders\spider_anim.txt',
  'C:\Assets\3drt\paid\Thief\Thief-animations-list.txt',
  'C:\Assets\3drt\paid\Dragon\dragon_animation_list.txt',
  'C:\Assets\3drt\paid\Dragon-boss\dragonboss_animation_list.txt',
  'C:\Assets\3drt\paid\Elongata\Elong_anim.txt',
  'C:\Assets\3drt\paid\Female-Ninja\Animations.txt');
{$endif}

  { AniList }

procedure AniList.DoRun;
var
  FileName: String;
  ErrorMsg: String;
  Json: TJsonNode;
  {$ifdef workingcopy}
  I: Integer;
  {$endif}
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
  {$ifdef workingcopy}
  for I := 0 to Length(Alist) - 1 do
  begin
  FileName := AList[I];
  {$else}
  FileName := 'C:\Assets\3drt\paid\Elf-Males\elfrangers-aniamtions-list.txt';
  {$endif}
  WriteLn('Parsing ' + FileName);
  Json := AniTxtToJson(FileName);
  if not(Json = nil) then
    begin
      WriteLn('Saving to ' + FileName + '.json');
      WriteLn('');
      Json.SaveToFile(FileName + '.json', True);
      JSon.Free;
    end
  else
    WriteLn('Errors encountered converting file.');
  {$ifdef workingcopy}
  end;
  {$endif}
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

