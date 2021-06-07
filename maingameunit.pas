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
  { TViewMode }
  TViewMode = (
    vmFlat,
    vmTwoToOne,
    vmIsometric,
    vmIsometricX2,
    vmMilitary,
    vmCustom
  );

  { TCastleApp }

  TCastleApp = class(TUIState)
    procedure BeforeRender; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
    procedure Resize; override; // TCastleUserInterface
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override; // TUIState
    function  Press(const Event: TInputPressRelease): Boolean; override; // TUIState
    function  Release(const Event: TInputPressRelease): Boolean; override; // TUIState
  private
    fViewMode: Cardinal;
    fStretchMultiplier: Single;
    LabelFPS: TCastleLabel;
    LabelRender: TCastleLabel;
    LabelSpare: TCastleLabel;
  public
    Viewport: TCastleViewport;
    TestModel: TCastleModel;
    Stage: TCastleScene;
    CameraRotation: Single;
    ModelRotation: Single;
    CameraElevation: Single;
    iScale: Single;
    BoundRadius: Single;
    LabelMode: TCastleLabel;
    ModelRotationCheck: Boolean;
    procedure UpdateModelRotation;
    procedure BootStrap;
    procedure CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const ButtonCode: TNotifyEvent = nil);
    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True; RightAlign: Boolean = False);
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure LoadViewport;
    procedure LoadModel(filename: String);
    procedure ShowModel(AModel: TCastleModel);
    procedure SetStretchMultiplier(const AStretch: Single);
    procedure SetViewMode(const AViewMode: Cardinal);
    procedure ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
    procedure ViewFromRadius(const ARadius: Single; const AElevation: Single; const ATheta: Single);
    function  CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isTransparent: Boolean = False): TCastleImage;
    procedure UpdateScale;

    property  StretchMultiplier: Single read fStretchMultiplier write SetStretchMultiplier default 0;
    property  ViewMode: Cardinal read fViewMode write SetViewMode default 0;
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

procedure TCastleApp.SetStretchMultiplier(const AStretch: Single);
begin
  if not(fStretchMultiplier = AStretch) then
    begin
      fStretchMultiplier := AStretch;
      UpdateScale;
    end;
end;

procedure TCastleApp.SetViewMode(const AViewMode: Cardinal);
begin
  if not(fViewMode = AViewMode) then
    begin
      fViewMode := AViewMode;
      UpdateScale;
    end;
end;

procedure TCastleApp.UpdateScale;
begin
  if not(Viewport = nil) and not(Viewport.Camera = nil) and (Viewport.Camera.ProjectionType = ptOrthographic) then
    begin
      Viewport.Camera.Orthographic.Width := Viewport.EffectiveWidth / StretchMultiplier;
      Viewport.Camera.Orthographic.Height := Viewport.EffectiveHeight;
      iScale := Min(Viewport.EffectiveWidth, Viewport.EffectiveHeight);
      Viewport.Camera.Orthographic.Scale := (2 * BoundRadius) / iScale;
    end
end;

procedure TCastleApp.Resize;
begin
  WriteLnLog('Resize : ' + IntToStr(Container.Width) + ' x ' + IntToStr(Container.Height));
  inherited;
  UpdateScale;
end;

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
  StretchMultiplier := 1;

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
  Viewport.Camera.Orthographic.Scale := 1;
  Viewport.Camera.ProjectionType := ptOrthographic;

  InsertFront(Viewport);

  UpdateScale; // SB

  CreateLabel(LabelMode, 0, False);

  CreateLabel(LabelSpare, 2);
  CreateLabel(LabelFPS, 1);
  CreateLabel(LabelRender, 0);
end;

procedure TCastleApp.ShowModel(AModel: TCastleModel);
begin
  Viewport.Items.Add(Stage);
  Viewport.Items.Add(AModel.Scene);
  Viewport.Items.MainScene := AModel.Scene;
end;

procedure TCastleApp.LoadModel(filename: String);
begin
  try
    if not(TestModel = nil) then
      begin
        FreeAndNil(TestModel);
      end;

    CameraRotation := 2 * Pi * (5/8);
    ModelRotation := 0;
    ModelRotationCheck := False;
    CameraElevation := 0;
    ViewMode := 0;
    BoundRadius := 1.0;
    iScale := 1.0;

    TestModel := TCastleModel.Create(Application);
    TestModel.Spatial := [ssDynamicCollisions, ssRendering];
    TestModel.Load(filename);
    TestModel.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
        True,
        Viewport.PrepareParams);
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

procedure TCastleApp.UpdateModelRotation;
begin
  ModelRotation += 3;
  if ModelRotation < 360 then
    begin
      ViewFromRadius(2, CameraElevation, CameraRotation + (2 * Pi * (ModelRotation / 360)));
//      TestModel.Transform.Rotation := Vector4(0, 1, 0, 2 * Pi * (ModelRotation / 360));
    end
  else
    begin
      ModelRotation := 0;
      ViewFromRadius(2, CameraElevation, CameraRotation);
//      TestModel.Transform.Rotation := Vector4(0, 1, 0, 2 * Pi * (ModelRotation / 360));
      ModelRotationCheck := False;
    end;
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

procedure TCastleApp.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  sc: TVector3;
  sr: Single;
begin
  if ModelRotationCheck then
    begin
      UpdateModelRotation;
    end
  else
    begin
    if ViewMode = 0 then
      begin
        StretchMultiplier := 1;
        Viewport.Camera.Orthographic.Stretch := False;
        ViewFromRadius(2, 0, 2 * pi * (6/8));
      end
    else if ViewMode = 1 then
      begin
        StretchMultiplier := 1;
        Viewport.Camera.Orthographic.Stretch := False;
        CameraElevation :=  -0.81625;
        ViewFromRadius(2, CameraElevation, CameraRotation);
      end
    else if ViewMode = 2 then
      begin
        StretchMultiplier := 1;
        Viewport.Camera.Orthographic.Stretch := False;
        CameraElevation :=  -1;
        ViewFromRadius(2, CameraElevation, CameraRotation);
      end
    else if ViewMode = 3 then
      begin
        StretchMultiplier := 1;
        Viewport.Camera.Orthographic.Stretch := False;
        CameraElevation :=  -2;
        ViewFromRadius(2, CameraElevation, CameraRotation);
      end
    else if ViewMode = 4 then
      begin
        StretchMultiplier := 0.81625;
        Viewport.Camera.Orthographic.Stretch := True;
        CameraElevation :=  -2;
        ViewFromRadius(2, CameraElevation, CameraRotation);
      end
    else if ViewMode = 5 then
      begin
        StretchMultiplier := 1;
        Viewport.Camera.Orthographic.Stretch := False;
        CameraElevation :=  -9999;
        ViewFromRadius(2, CameraElevation, CameraRotation);
      end
    else
      begin
        StretchMultiplier := 1;
        Viewport.Camera.Orthographic.Stretch := False;
        ViewMode := 0;
        CameraElevation :=  0;
        ViewFromRadius(2, CameraElevation, CameraRotation);
      end;

    if not(TestModel = nil) then
      begin
        if not(TestModel.IsLocked) then
          begin
            sc := Vector3(0, 0, 0);
            sr := 0;
            TestModel.Scene.BoundingBox.BoundingSphere(sc, sr);
            if not(sr = 0) then
              BoundRadius := sqrt(sr)
            else
              BoundRadius := 1.0;

            iScale := Min(Viewport.EffectiveWidth, Viewport.EffectiveHeight);
            TestModel.LockedScale := iScale;
            TestModel.IsLocked := True;
          end;

        Viewport.Camera.Orthographic.Scale := (2 * BoundRadius) / iScale;

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
      end;
      CastleForm.UpdateInfoPanel;
      {$endif}
    end;
  inherited;
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

          if Viewport.Camera.Orthographic.Stretch then
            begin
              SourceViewport.Camera.Orthographic.Stretch := True;
              SourceViewport.Camera.Orthographic.Width := SourceViewport.EffectiveWidth / StretchMultiplier;
              SourceViewport.Camera.Orthographic.Height := SourceViewport.EffectiveHeight;
            end;

          SourceViewport.Camera.Orthographic.Scale := (2 * BoundRadius) /
            Min(SourceViewport.EffectiveWidth, SourceViewport.EffectiveHeight);

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

