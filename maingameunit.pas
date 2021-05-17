unit MainGameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, CastleUIState,
  {$ifndef cgeapp}
  Forms, Controls, Graphics, Dialogs, CastleControl,
  {$else}
  CastleWindow,
  {$endif}
  CastleControls, CastleColors, CastleUIControls,
  CastleTriangles, CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform,
  CastleViewport, CastleCameras, CastleProjection,
  X3DNodes, X3DFields, X3DTIme,
  CastleImages, CastleGLImages,
  CastleTextureImages, CastleCompositeImage,
  CastleApplicationProperties, CastleLog, CastleTimeUtils, CastleKeysMouse;

type
  { TCastleSceneHelper }

  TCastleSceneHelper = class helper for TCastleScene
  public
     procedure Normalize;
  end;

  { TCastleApp }

  TCastleApp = class(TUIState)
    procedure BeforeRender; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
    procedure Resize; override; // TCastleUserInterface
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override; // TUIState
    function  Motion(const Event: TInputMotion): Boolean; override; // TUIState
    function  Press(const Event: TInputPressRelease): Boolean; override; // TUIState
    function  Release(const Event: TInputPressRelease): Boolean; override; // TUIState
  private
    Viewport: TCastleViewport;
    Scene: TCastleScene;
    LabelFPS: TCastleLabel;
    LabelRender: TCastleLabel;
    LabelSpare: TCastleLabel;
  public
    procedure BootStrap;
    procedure CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const ButtonCode: TNotifyEvent = nil);
    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True; RightAlign: Boolean = False);
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure LoadViewport;
    procedure LoadScene(filename: String);
    procedure ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
  end;

var
  AppTime: Int64;
  PrepDone: Boolean;
  RenderReady: Boolean;
  CastleApp: TCastleApp;

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}

{ TCastleSceneHelper }

{ Normalize - Center the model in a 1x1x1 cube }
procedure TCastleSceneHelper.Normalize;
begin
  if not(RootNode = nil) then
    begin
    if not BoundingBox.IsEmptyOrZero then
      begin
        if BoundingBox.MaxSize > 0 then
          begin
            Center := Vector3(Min(BoundingBox.Data[0].X, BoundingBox.Data[1].X) + (BoundingBox.SizeX / 2),
                              Min(BoundingBox.Data[0].Y, BoundingBox.Data[1].Y) + (BoundingBox.SizeY / 2),
                              Min(BoundingBox.Data[0].Z, BoundingBox.Data[1].Z) + (BoundingBox.SizeZ / 2));
            Scale := Vector3(1 / BoundingBox.MaxSize,
                             1 / BoundingBox.MaxSize,
                             1 / BoundingBox.MaxSize);
            Translation := -Center;
          end;
      end;
    end;
end;

{ TCastleApp }

procedure TCastleApp.BootStrap;
var
  ProcTimer: Int64;
begin
  ProcTimer := CastleGetTickCount64;
  LoadScene('castle-data:/up.glb');
  ProcTimer := CastleGetTickCount64 - ProcTimer;
  WriteLnLog('ProcTimer (LoadScene) = ' + FormatFloat('####0.000', ProcTimer / 1000) + ' seconds');
end;

procedure TCastleApp.CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const ButtonCode: TNotifyEvent = nil);
begin
  objButton := TCastleButton.Create(Application);
  objButton.Caption := ButtonText;
  objButton.Anchor(hpMiddle, 10);
  objButton.Anchor(vpBottom, 10 + (Line * 35));
  objButton.onClick := ButtonCode;
  InsertFront(objButton);
end;

procedure TCastleApp.CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True; RightAlign: Boolean = False);
begin
  objLabel := TCastleLabel.Create(Application);
  objLabel.Padding := 5;
  objLabel.Color := White;
  objLabel.Frame := True;
  objLabel.FrameColor := Black;
  objLabel.Anchor(hpLeft, 10);
  if RightAlign then
    objLabel.Anchor(hpRight, -10)
  else
    objLabel.Anchor(hpLeft, 10);
  if BottomUp then
    objLabel.Anchor(vpBottom, 10 + (Line * 35))
  else
    objLabel.Anchor(vpTop, -(10 + (Line * 35)));
  InsertFront(objLabel);
end;

procedure TCastleApp.ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
var
  Spherical: TVector3;
begin
  Spherical := -ADirection.Normalize;
  Spherical := Spherical * ARadius;
  Viewport.Camera.Up := Vector3(0, 1, 0);
  Viewport.Camera.Direction := ADirection;
  Viewport.Camera.Position  := Spherical;
end;

procedure TCastleApp.LoadViewport;
begin
  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := True;
  Viewport.AutoCamera := False;
  Viewport.Setup2D;
  Viewport.NavigationType := ntNone;
  Viewport.AssignDefaultCamera;
  Viewport.Camera.Orthographic.Width := 2;
  Viewport.Camera.Orthographic.Height := 2;
  Viewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  Viewport.Camera.Orthographic.Scale := 1;
  Viewport.Camera.ProjectionType := ptOrthographic;

  InsertFront(Viewport);

  CreateLabel(LabelSpare, 2);
  CreateLabel(LabelFPS, 1);
  CreateLabel(LabelRender, 0);

  ViewFromRadius(2, Vector3(-1, -1, -1));
end;

procedure TCastleApp.LoadScene(filename: String);
begin
  try
    Scene := TCastleScene.Create(Application);
    Scene.Spatial := [ssDynamicCollisions, ssRendering];
    Scene.Load(filename);
    Scene.Normalize;
    Scene.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
        True,
        Viewport.PrepareParams);
    Viewport.Items.Add(Scene);
    Viewport.Items.MainScene := Scene;
  except
    on E : Exception do
      begin
        WriteLnLog('Oops #1' + LineEnding + E.ClassName + LineEnding + E.Message);
       end;
  end;
end;

procedure TCastleApp.Start;
begin
  inherited;
  LogTextureCache := True;
  WriteLnLog('Start : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  Scene := nil;
  LoadViewport;
  PrepDone := True;
end;

procedure TCastleApp.Stop;
begin
  inherited;
  WriteLnLog('Stop : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleApp.BeforeRender;
begin
  inherited;
  LabelFPS.Caption := 'FPS = ' + FormatFloat('####0.00', Container.Fps.RealFps);
  LabelRender.Caption := 'Render = ' + FormatFloat('####0.00', Container.Fps.OnlyRenderFps);

end;

procedure TCastleApp.Render;
begin
  inherited;

  if PrepDone and GLInitialized and RenderReady then
    begin
      PrepDone := False;
      BootStrap;
    end;
  RenderReady := True;
end;

procedure TCastleApp.Resize;
begin
  inherited;
end;

procedure TCastleApp.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
end;

function TCastleApp.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
end;

function TCastleApp.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

function TCastleApp.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

end.

