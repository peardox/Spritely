unit MainGameUnit;

{$mode objfpc}{$H+}
{$define multimodel}

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
  CastleGLUtils, multimodel, staging, Overlays, MiscFunctions;

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
  private
    fViewMode: Cardinal;
    fOverSample: Cardinal;
    fSpriteWidth: Cardinal;
    fSpriteHeight: Cardinal;

    fStretchMultiplier: Single;
    LabelFPS: TCastleLabel;
    LabelRender: TCastleLabel;
    LabelSpare: TCastleLabel;
  public
    VPMax: TVector2Integer;

    FileToLoadList: TStringList;
    Viewport: TCastleViewport;
    IsTransparent: Boolean;
    WorkingModel: TCastleModel;
    Stage: TCastleStage;
    CameraRotation: Single;
    ModelRotation: Single;
    CameraElevation: Single;
    iScale: Single;
    iScaleMultiplier: Single;
    BoundRadius: Single;
    LabelMode: TCastleLabel;
    ModelRotationCheck: Boolean;
    ModelRotationDone: Boolean;
    AppLogLevel: Boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateModelRotation;
    procedure BootStrap;
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure LoadViewport;
//    procedure LoadModel(Sender: TObject);
    procedure LoadModel(filename: String);
    procedure ShowModel(AModel: TCastleModel);
    procedure SetStretchMultiplier(const AStretch: Single);
    procedure SetViewMode(const AViewMode: Cardinal);
    procedure ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
    procedure ViewFromRadius(const ARadius: Single; const AElevation: Single; const ATheta: Single);
    function  CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isSpriteTransparent: Boolean = False): TCastleImage;
    procedure UpdateScale;

    property  StretchMultiplier: Single read fStretchMultiplier write SetStretchMultiplier default 0;
    property  ViewMode: Cardinal read fViewMode write SetViewMode default 0;
    property  OverSample: Cardinal read fOverSample write fOverSample;
    property  SpriteWidth: Cardinal read fSpriteWidth write fSpriteWidth;
    property  SpriteHeight: Cardinal read fSpriteHeight write fSpriteHeight;
end;

var
  AppTime: Int64;
  PrepDone: Boolean;
  RenderReady: Boolean;
  CastleApp: TCastleApp;
  CastleOverlay: TCastleOverlay;
  ModelArray: TCastleModelArray;

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}

{ TCastleApp }

constructor TCastleApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WriteLnLog('TCastleApp Created');
  LogTextureCache := True;
  AppLogLevel := False;
  WorkingModel := nil;
  ModelArray := nil;
  ViewMode := 2;
  OverSample := 8;
  SpriteWidth := 256;
  SpriteHeight := 256;
  StretchMultiplier := 1;
  BoundRadius := 1.0;
  IsTransparent := True;
  VPMax := GLFeatures.MaxViewportDimensions;
  PrepDone := True;

  FileToLoadList := TStringList.Create;
  FileToLoadList.OwnsObjects := False;
  FileToLoadList.Duplicates := dupAccept;

  CastleOverlay := TCastleOverlay.Create({$ifndef cgeapp}CastleForm.{$endif}Window);
  LoadViewport;
end;

destructor TCastleApp.Destroy;
begin
  FileToLoadList.Free;
  inherited;
end;

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
      if not(iScale = 0) then
        Viewport.Camera.Orthographic.Scale := (2 * BoundRadius) / iScale;
      WriteLnLog('UpdateScale : ' + FloatToStr(Viewport.EffectiveWidth) + ' x ' + FloatToStr(Viewport.EffectiveHeight));
    end
  else
    WriteLnLog('Skipped UpdateScale');

end;

procedure TCastleApp.Resize;
var
  DesiredAspect: Single;
  ActualAspect: Single;
begin
  WriteLnLog('Start State Resize');
  inherited;

  DesiredAspect := SpriteWidth / SpriteHeight;
  ActualAspect := StateContainer.Width / StateContainer.Height;

  if DesiredAspect <= ActualAspect then
    begin
      if StateContainer.Width <= (StateContainer.Height / DesiredAspect) then
        begin
          LabelMode.Caption := '1 = Aspect Switch';
          Viewport.Height := StateContainer.Height;
          Viewport.Width := StateContainer.Height * DesiredAspect;
        end
      else
        begin
          LabelMode.Caption := '2 = Aspect Switch';
          Viewport.Height := StateContainer.Height;
          Viewport.Width := StateContainer.Height * DesiredAspect;
        end;
    end
  else
    begin
      if StateContainer.Width <= (StateContainer.Height / DesiredAspect) then
        begin
          LabelMode.Caption := '3 = Aspect Switch';
          Viewport.Width := StateContainer.Width;
          Viewport.Height := StateContainer.Width / DesiredAspect;
        end
      else
        begin
          LabelMode.Caption := '4 = Aspect Switch';
          Viewport.Width := StateContainer.Width;
          Viewport.Height := StateContainer.Width / DesiredAspect;
        end;
    end;

  Viewport.Left := Trunc((StateContainer.Width - Viewport.Width) / 2);
  Viewport.Bottom := Trunc((StateContainer.Height - Viewport.Height) / 2);

  LabelMode.Caption := LabelMode.Caption + ' : Viewport = ' + FloatToStr(Viewport.Width) + ' x ' + FloatToStr(Viewport.Height);

  UpdateScale;
  WriteLnLog('End State Resize : LeftTop = ' + FloatToStr(Viewport.Left) + ' ' + FloatToStr(Viewport.Height) + ' : Viewport = ' + FloatToStr(Viewport.Width) + ' x ' + FloatToStr(Viewport.Height));
end;

procedure TCastleApp.BootStrap;
begin
  SpriteWidth := 1024;
  SpriteHeight := 1024;
  LoadModel('castle-data:/Quaternius/RPGCharacters/Wizard.glb');
  ShowModel(WorkingModel);
  if WorkingModel.HasAnimations then
    WorkingModel.SelectAnimation(WorkingModel.Actions[0]);
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
  WriteLnLog('Start LoadViewport');

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := False;
  Viewport.AutoCamera := False;
  Viewport.Setup2D;
  if isTransparent then
    Viewport.Transparent := True
  else
    Viewport.BackgroundColor := Vector4(1,1,1,1);
  Viewport.NavigationType := ntNone;
  Viewport.AssignDefaultCamera;
  Viewport.Width := StateContainer.Width;
  Viewport.Height := StateContainer.Height;
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
  Viewport.Items.Add(AModel.Scene);
  Viewport.Items.MainScene := AModel.Scene;
end;
{
procedure TCastleApp.LoadModel(Sender: TObject);
begin
  WriteLnLog('LoadModel ' + FileToLoad[0]  + ' at ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000));
  if FileToLoad.Count > 0 then
    LoadModel(FileToLoad.Pop);
  WriteLnLog('LoadMode Done at ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000));

  if FileToLoad.Count = 0 then
    begin
      WriteLnLog('Pop UI');
//      TUIState.Pop(CastleOverlay);
    end;
end;
}
procedure TCastleApp.LoadModel(filename: String);
begin
  try
    if not(WorkingModel = nil) then
      begin
        Stage.Remove(WorkingModel.Scene);
        {$ifdef multimodel}
        {$else}
        FreeAndNil(WorkingModel);
        {$endif}
      end;

    CameraRotation := 2 * Pi * (5/8);
    ModelRotation := 0;
    ModelRotationCheck := False;
    ModelRotationDone := True;
    CameraElevation := 0;
    BoundRadius := 1.0;
    iScale := 1.0;
    iScaleMultiplier := 1.0;
    WorkingModel := TCastleModel.Create(Application);
    SetLength(ModelArray, Length(ModelArray) + 1);
    ModelArray[Length(ModelArray) - 1] := WorkingModel;

    WorkingModel.Load(filename);

    if (Stage = nil) then
      begin
        Stage := TCastleStage.Create(Self);
//        Stage.LoadStage(-1);
//        Stage.LoadStage('castle-data:/ground/Tileable Brick Ground Textures - Set 2/Brick_03.png', -1);
//      Stage.LoadStage('castle-data:/ground/myfreetextures/seamless-wood-planks-4.jpg', -1);
      Stage.LoadStage('castle-data:/ground/myfreetextures/tilesf2.jpg', -1);
//      Stage.LoadStage('castle-data:/ground/grid16.png', 0, 1000, 1000);
//      Stage.LoadStage('castle-data:/ground/myfreetextures/pavers1b2.jpg', -1);
//        Stage.LoadStage('castle-data:/ground/White_Texture.png', -1);
        Stage.Add(WorkingModel.Scene);
        Viewport.Items.Add(Stage);
        Viewport.Items.MainScene := Stage;
    end
    else
      begin
        Stage.Add(WorkingModel.Scene);
      end;
{
    WorkingModel.Spatial := [ssDynamicCollisions, ssRendering];
    WorkingModel.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
        True,
        Viewport.PrepareParams);
}
  except
    on E : Exception do
      begin
        WriteLnLog('Oops #1' + LineEnding + E.ClassName + LineEnding + E.Message);
       end;
  end;
end;

procedure TCastleApp.Start;
begin
  WriteLnLog('Start MainState');
  inherited;
end;

procedure TCastleApp.Stop;
begin
  inherited;
  WriteLnLog('Stop MainState: ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleApp.BeforeRender;
begin
  inherited;
  if AppLogLevel then
    WriteLnLog('Start BeforeRender');
  LabelFPS.Caption := 'FPS = ' + FormatFloat('####0.00', StateContainer.Fps.RealFps);
  LabelRender.Caption := 'Render = ' + FormatFloat('####0.00', StateContainer.Fps.OnlyRenderFps);
end;

procedure TCastleApp.UpdateModelRotation;
begin
  if not ModelRotationCheck then
    ModelRotation += 12
  else
    ModelRotation += 3;
  if ModelRotation < 360 then
    begin
      ViewFromRadius(2, CameraElevation, CameraRotation + (2 * Pi * (ModelRotation / 360)));
    end
  else
    begin
      ModelRotation := 0;
      ViewFromRadius(2, CameraElevation, CameraRotation);
      if not ModelRotationCheck then
        ModelRotationDone := True;
    end;
end;

procedure TCastleApp.Render;
begin
  if AppLogLevel then
    WriteLnLog('Start Render');
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
  if AppLogLevel then
    WriteLnLog('Start Update');
  if ModelRotationCheck or not ModelRotationDone then
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
        CameraElevation :=  -2;
        ViewFromRadius(2, CameraElevation,  2 * pi * (6/8));
      end
    else
      begin
        StretchMultiplier := 1;
        Viewport.Camera.Orthographic.Stretch := False;
        ViewMode := 0;
        CameraElevation :=  0;
        ViewFromRadius(2, CameraElevation, CameraRotation);
      end;

    if not(WorkingModel = nil) then
      begin
        if not(WorkingModel.IsLocked) then
          begin
            sc := Vector3(0, 0, 0);
            sr := 0;
            WorkingModel.Scene.BoundingBox.BoundingSphere(sc, sr);
            if not(sr = 0) then
              BoundRadius := sqrt(sr)
            else
              BoundRadius := 1.0;

            iScale := Min(Viewport.EffectiveWidth, Viewport.EffectiveHeight);
            iScaleMultiplier := 1.0;
            WorkingModel.LockedScale := iScale;
            WorkingModel.IsLocked := True;
          end;

        if not(iScale = 0) then
          Viewport.Camera.Orthographic.Scale := (2 * BoundRadius) / (iScale * iScaleMultiplier);

        if(WorkingModel.CurrentAnimation >= 0) and (WorkingModel.CurrentAnimation < WorkingModel.Actions.Count) then
          begin
            if WorkingModel.IsPaused then
              LabelSpare.Caption := 'Frame : ' + FormatFloat('####0.0000', WorkingModel.CurrentFrame) + ' / ' + FormatFloat('####0.0000', WorkingModel.TotalFrames) + ' (Paused)'
            else
              LabelSpare.Caption := 'Frame : ' + FormatFloat('####0.0000', WorkingModel.CurrentFrame) + ' / ' + FormatFloat('####0.0000', WorkingModel.TotalFrames);
          end;

      end;
      {$ifdef cgeapp}
// Todo : UpdateInfoPanel for App
      {$else}
      CastleForm.UpdateInfoPanel;
      {$endif}
    end;

  inherited;
end;

procedure ShowAppMessage(const AMsg: String);
begin
{$ifdef cgeapp}
  WriteLnLog(AMsg);
{$else}
  ShowMessage(AMsg);
{$endif}
end;

function TCastleApp.CreateSpriteImage(const SourceScene: TCastleScene; const TextureWidth: Cardinal; const TextureHeight: Cardinal; const isSpriteTransparent: Boolean = False): TCastleImage;
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
          if isSpriteTransparent then
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
          (Min(SourceViewport.EffectiveWidth, SourceViewport.EffectiveHeight) * iScaleMultiplier);

//          SourceViewport.Camera.Orthographic.Scale := Viewport.Camera.Orthographic.Scale / (iScale);
//          SourceViewport.Camera.Orthographic.Scale := (2 * BoundRadius) / Min(SourceViewport.EffectiveWidth, SourceViewport.EffectiveHeight);
//          SourceViewport.Camera.Orthographic.Scale := ((2 * BoundRadius) / (iScale * OverSample));

          WriteLnLog(FloatToStr(iScale) + ' vs ' + FloatToStr(WorkingModel.LockedScale));
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

