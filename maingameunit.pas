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
  CastleImages, CastleGLImages, CastleRectangles,
  CastleTextureImages, CastleCompositeImage, CastleLog,
  CastleApplicationProperties, CastleTimeUtils, CastleKeysMouse,
  multimodel;

type
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
    LabelFPS: TCastleLabel;
    LabelRender: TCastleLabel;
    LabelSpare: TCastleLabel;
  public
    Viewport: TCastleViewport;
    TestModel: TCastleModel;
    CameraRotation: Single;
    CameraElevation: Single;
    iScale: Single;
    ViewMode: Cardinal;
    LabelMode: TCastleLabel;
    procedure BootStrap;
    procedure CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const ButtonCode: TNotifyEvent = nil);
    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True; RightAlign: Boolean = False);
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure LoadViewport;
    procedure LoadModel(filename: String);
    procedure ShowModel(AModel: TCastleModel);
    procedure ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
    procedure ViewFromRadius(const ARadius: Single; const AElevation: Single; const ATheta: Single);
    function  CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isTransparent: Boolean = False): TCastleImage;
  end;

var
  AppTime: Int64;
  PrepDone: Boolean;
  RenderReady: Boolean;
  CastleApp: TCastleApp;
  MaxVP: TVector2Integer;

  const
    SpriteWidth = 64;
    SpriteHeight = 64;
    Margin = 0.1;

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}

{ TCastleApp }

procedure TCastleApp.BootStrap;
var
  ProcTimer: Int64;
begin
  ProcTimer := CastleGetTickCount64;
  LoadModel('castle-data:/Quaternius/RPGCharacters/Wizard.glb');
  ProcTimer := CastleGetTickCount64 - ProcTimer;
  WriteLnLog('ProcTimer (LoadModel) = ' + FormatFloat('####0.000', ProcTimer / 1000) + ' seconds');
  ShowModel(TestModel);
  if TestModel.HasAnimations then
    TestModel.SelectAnimation(TestModel.Actions[0]);
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

procedure TCastleApp.ViewFromRadius(const ARadius: Single; const AElevation: Single; const ATheta: Single);
begin
  ViewFromRadius(ARadius, Vector3(sqrt(ARadius) * Cos(ATheta), AElevation, sqrt(ARadius) * Sin(ATheta)));
end;

procedure TCastleApp.ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
begin
  Viewport.Camera.Up := Vector3(0, 1, 0);
  Viewport.Camera.Direction := ADirection;
  Viewport.Camera.Position  := ARadius * -ADirection.Normalize;
end;

procedure TCastleApp.LoadViewport;
begin
  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := True;
  Viewport.AutoCamera := False;
  Viewport.Setup2D;
  Viewport.BackgroundColor := Vector4(1, 1, 1, 1);
  Viewport.NavigationType := ntNone;
  Viewport.AssignDefaultCamera;
  Viewport.Camera.Orthographic.Width := SpriteWidth;
  Viewport.Camera.Orthographic.Height := SpriteHeight;
  Viewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  Viewport.Camera.Orthographic.Scale := (SpriteHeight / SpriteWidth) / Min(SpriteWidth, SpriteHeight);
  Viewport.Camera.ProjectionType := ptOrthographic;

  InsertFront(Viewport);

  CreateLabel(LabelMode, 0, False);

  CreateLabel(LabelSpare, 2);
  CreateLabel(LabelFPS, 1);
  CreateLabel(LabelRender, 0);
end;

procedure TCastleApp.ShowModel(AModel: TCastleModel);
begin
  Viewport.Items.Add(AModel.Scene);
  Viewport.Items.MainScene := AModel.Scene;
end;

procedure TCastleApp.LoadModel(filename: String);
var
  sc: TVector3;
  sr: Single;
begin
  try
    TestModel := TCastleModel.Create(Application);
    TestModel.Spatial := [ssDynamicCollisions, ssRendering];
    TestModel.Load(filename);
    TestModel.Normalize;
    TestModel.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
        True,
        Viewport.PrepareParams);
    sc := Vector3(0, 0, 0);
    sr := 0;
    TestModel.Scene.BoundingBox.BoundingSphere(sc, sr);
    if not(sr = 0) then
      iScale := 1.0 / sr;
    WriteLnLog('Spehere Center : ' + sc.ToString + ' Sphere Radius : ' + FloatToStr(sr));
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
  CameraRotation := 2 * Pi * (5/8);
  CameraElevation := 0;
  ViewMode := 0;
  LogTextureCache := True;
  TestModel := nil;
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
      {$ifdef cgeapp}
      BootStrap;
      {$else}
      CastleForm.GuiBootStrap;
      {$endif}
    end;
  RenderReady := True;
end;

procedure TCastleApp.Resize;
begin
  inherited;
end;

procedure TCastleApp.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  if ViewMode = 0 then
    ViewFromRadius(2, 0, 2 * pi * (6/8))
  else if ViewMode = 1 then
    begin
      CameraElevation :=  -0.81625;
      ViewFromRadius(2, CameraElevation, CameraRotation);
    end
  else if ViewMode = 2 then
    begin
      CameraElevation :=  -1;
      ViewFromRadius(2, CameraElevation, CameraRotation);
    end
  else if ViewMode = 3 then
    begin
      CameraElevation :=  -2;
      ViewFromRadius(2, CameraElevation, CameraRotation);
    end
  else if ViewMode = 4 then
    begin
      CameraElevation :=  -9999;
      ViewFromRadius(2, CameraElevation, CameraRotation);
    end
  else
    begin
      ViewMode := 0;
      CameraElevation :=  0;
      ViewFromRadius(2, CameraElevation, CameraRotation);
    end;

  if not(TestModel = nil) then
    begin
      if not(TestModel.IsLocked) and not((Max(TestModel.Scene.BoundingBox.SizeZ, Max(TestModel.Scene.BoundingBox.SizeX, TestModel.Scene.BoundingBox.SizeY)) - Min(Viewport.Camera.Orthographic.EffectiveHeight, Viewport.Camera.Orthographic.EffectiveWidth)) < 0.1) then
        begin
          iScale := (1.0 - Margin) * (1 / Max(TestModel.Scene.BoundingBox.SizeZ, Max(TestModel.Scene.BoundingBox.SizeX, TestModel.Scene.BoundingBox.SizeY)));
          TestModel.LockedScale := iScale;
          TestModel.IsLocked := True;
        end;

      TestModel.Scene.Scale := Vector3(iScale, iScale, iScale);
      if(TestModel.CurrentAnimation >= 0) and (TestModel.CurrentAnimation < TestModel.Actions.Count) then
        begin
          if TestModel.IsPaused then
            LabelSpare.Caption := 'Frame : ' + FormatFloat('####0.0000', TestModel.CurrentFrame) + ' / ' + FormatFloat('####0.0000', TestModel.TotalFrames) + ' (Paused)'
          else
            LabelSpare.Caption := 'Frame : ' + FormatFloat('####0.0000', TestModel.CurrentFrame) + ' / ' + FormatFloat('####0.0000', TestModel.TotalFrames);
        end;
      {$ifdef cgeapp}
// Todo : UpdateInfoPanel for App
      {$else}
      CastleForm.UpdateInfoPanel;
      {$endif}
    end;
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

procedure ShowAppMessage(const AMsg: String);
begin
{$ifdef cgeapp}
  WriteLnLog(AMsg);
{$else}
  ShowMessage(AMsg);
{$endif}
end;

function TCastleApp.CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isTransparent: Boolean = False): TCastleImage;
var
  SourceViewport: TCastleViewport;
  GrabScene: TCastleScene;
  ViewportRect: TRectangle;
  Image: TDrawableImage;
begin
  SourceViewport := nil;

  if not(SourceScene = nil) and (TextureWidth > 0) and (TextureHeight > 0) then
    begin
      try
        try
          Image := TDrawableImage.Create(TRGBAlphaImage.Create(TextureWidth, TextureHeight), true, true);
          Image.RenderToImageBegin;

          GrabScene := SourceScene.Clone(nil);

          SourceViewport := TCastleViewport.Create(nil);
          SourceViewport.Width := TextureWidth;
          SourceViewport.Height := TextureHeight;
          if isTransparent then
            SourceViewport.Transparent := True
          else
            SourceViewport.BackgroundColor := Vector4(1,1,1,1);

          SourceViewport.Setup2D;
          SourceViewport.Camera.ProjectionType := ptOrthographic;
          SourceViewport.Camera.Orthographic.Origin := Viewport.Camera.Orthographic.Origin;
          SourceViewport.Camera.Up := Viewport.Camera.Up;
          SourceViewport.Camera.Direction := Viewport.Camera.Direction;
          SourceViewport.Camera.Position  := Viewport.Camera.Position;
          SourceViewport.Camera.Orthographic.Scale := Min(
            Viewport.Camera.Orthographic.EffectiveWidth / TextureWidth,
            Viewport.Camera.Orthographic.EffectiveHeight / TextureHeight);

          WriteLnLog('Scale : ' + FloatToStr(SourceViewport.Camera.Orthographic.Scale));

          SourceViewport.Items := ViewPort.Items;
          ViewportRect := Rectangle(0, 0, TextureWidth, TextureHeight);
            {$ifndef cgeapp}CastleForm.{$endif}Window.Container.RenderControl(SourceViewport,ViewportRect);

          Image.RenderToImageEnd;

          if not False { Application.OpenGLES } then
          begin
            try
              Result := Image.GetContents(TRGBAlphaImage);
            except
              on E : Exception do
                begin
                  ShowAppMessage(E.ClassName + LineEnding + E.Message);
                end;
            end;
          end;

        except
          on E : Exception do
            begin
              ShowAppMessage(E.ClassName + LineEnding + E.Message);
            end;
        end;
      finally
        FreeAndNil(GrabScene);
        FreeAndNil(SourceViewport);
        FreeAndNil(Image);
      end;
    end;
end;

end.

