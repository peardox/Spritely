program SpritelyLaz;

{$mode objfpc}{$H+}
{$ifndef cgeapp}
{$NOTE Lazarus project}
{$else}
{$NOTE CGE project}
{$endif}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, GUIInitialization, castle_components, castle_base, Staging, MiscFunctions
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TCastleForm, CastleForm);
  Application.Run;
end.

