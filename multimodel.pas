unit multimodel;

{$mode objfpc}{$H+}
{$define logevents}

interface

uses
  Classes, SysUtils, Math,
  CastleColors, CastleTriangles, CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform,
  CastleViewport, CastleCameras, CastleProjection,
  X3DNodes, X3DFields, X3DTIme, X3DLoad, CastleBoxes,
  CastleImages, CastleGLImages, CastleDebugTransform,
  CastleTextureImages, CastleCompositeImage, CastleClassUtils,
  CastleLog, CastleTimeUtils, CastleRectangles, CastleRenderOptions;

type
  { TAnimationInfo }
  TAnimationInfo = record
    AnimStart: TFloatTime;
    AnimEnd: TFloatTime;
    FrameLow: TFloatTime;
    FrameHigh: TFloatTime;
    Hidden: Boolean;
  end;

  { TTimedEvent }
  TTimedEventListener = class(TComponent)
    procedure ReceivedIsActive(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    procedure ReceivedElapsedTime(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  end;

  { TCastleModel }
  TCastleModel = class(TCastleComponent)
  private
    fActions: TStringList;
    fAnimNode: TTimeSensorNode;
    fCurrentAnimation: Integer;
    fIsLooped: Boolean;
    fIsPaused: Boolean;
    fIsPlaying: Boolean;
    fFrameLow: TFloatTime;
    fFrameHigh: TFloatTime;
    fLastFrame: TFloatTime;
    fIsNormalized: Boolean;
    fRootNode: TX3DRootNode;
    fScene: TCastleScene;
    fTransform: TTransformNode;
    fUseFrames: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  getSpatial: TSceneSpatialStructures;
    procedure SetSpatial(const Value: TSceneSpatialStructures);

    property  Actions: TStringList read fActions write fActions;
    property  CurrentAnimation: Integer read fCurrentAnimation write fCurrentAnimation;
    property  AnimNode: TTimeSensorNode read fAnimNode write fAnimNode;
    property  FrameLow: TFloatTime read fFrameLow write fFrameLow;
    property  FrameHigh: TFloatTime read fFrameHigh write fFrameHigh;
    property  LastFrame: TFloatTime read fLastFrame write fLastFrame;
    property  IsLooped: Boolean read fIsLooped write fIsLooped;
    property  IsPaused: Boolean read fIsPaused write fIsPaused;
    property  IsPlaying: Boolean read fIsPlaying write fIsPlaying;
    property  RootNode: TX3DRootNode read fRootNode write fRootNode;
    property  Scene: TCastleScene read fScene write fScene;
    property  Transform: TTransformNode read fTransform write fTransform;
    property  UseFrames: Boolean read fUseFrames write fUseFrames;
    property  Spatial: TSceneSpatialStructures read getSpatial write SetSpatial default [];

    procedure PrepareResources(const Options: TPrepareResourcesOptions;
      const ProgressStep: boolean; const Params: TPrepareParams);
    procedure Load(const ARootNode: TX3DRootNode; const AOwnsRootNode: boolean;
      const AOptions: TSceneLoadOptions = []);
    procedure Load(const AURL: string; const AOptions: TSceneLoadOptions = []);

    procedure AddAnimation(const AAction: String; const ASensor: TTimeSensorNode; const AIsLooped: Boolean = True);
    procedure GoToFrame(AFrame: TFloatTime; const APause: Boolean = True);
    procedure Normalize;
    procedure Pause;
    procedure RemoveAnimation;
    procedure Start;
    procedure Stop;
  end;

implementation

procedure TTimedEventListener.ReceivedIsActive(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  Val: Boolean;
begin
  Val := (Value as TSFBool).Value;
  if not(TCastleModel(Owner).AnimNode = nil) then
    begin
      if TCastleModel(Owner).UseFrames and (TCastleModel(Owner).FrameLow = 0) then
        begin
          TCastleModel(Owner).FrameLow := (380 / 1500) * TCastleModel(Owner).AnimNode.CycleInterval;
          TCastleModel(Owner).FrameHigh := (399 / 1500) * TCastleModel(Owner).AnimNode.CycleInterval;
        end;
    end;
  if not(Val) then
    begin
      if not TCastleModel(Owner).IsPaused then
        begin
          {$ifdef logevents}
          WriteLnLog('ReceivedIsActive : ' + BoolToStr(Val) + ' -> GotoFrame : ' + FloatToStr(TCastleModel(Owner).FrameLow));
          {$endif}
          TCastleModel(Owner).GoToFrame(TCastleModel(Owner).FrameLow, False);
        end
      {$ifndef logevents}
      ;
      {$else}
      else
        WriteLnLog('ReceivedIsActive (Paused) : ' + BoolToStr(Val));
      {$endif}
    end;
  {$ifdef logevents}
  WriteLnLog('ReceivedIsActive : ' + BoolToStr(Val));
  {$endif}
end;

procedure TTimedEventListener.ReceivedElapsedTime(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  Val: Double;
begin
  Val := (Value as TSFTime).Value;
  if Val >= TCastleModel(Owner).FrameHigh then
    begin
      TCastleModel(Owner).AnimNode.Stop;
      {$ifdef logevents}
      WriteLnLog('ReceivedElapsedTime (Stopping) : ' + FloatToStr(Val));
      {$endif}
    end;
end;

function  TCastleModel.getSpatial: TSceneSpatialStructures;
begin
  Result := fScene.Spatial;
end;

procedure TCastleModel.SetSpatial(const Value: TSceneSpatialStructures);
begin
  fScene.Spatial := Value;
end;

procedure TCastleModel.PrepareResources(const Options: TPrepareResourcesOptions;
  const ProgressStep: boolean; const Params: TPrepareParams);
begin
  fScene.PrepareResources(Options, ProgressStep, Params);
end;

procedure TCastleModel.Load(const ARootNode: TX3DRootNode; const AOwnsRootNode: boolean;
  const AOptions: TSceneLoadOptions);
begin
  fScene.Load(ARootNode, AOwnsRootNode, AOptions);
end;

procedure TCastleModel.Load(const AURL: string; const AOptions: TSceneLoadOptions);
begin
  fScene.Load(AURL, AOptions);
end;

procedure TCastleModel.AddAnimation(const AAction: String; const ASensor: TTimeSensorNode; const AIsLooped: Boolean = True);
begin
  fAnimNode := ASensor;
  fIsLooped := AIsLooped;
  FreeAndNil(fActions);
end;

procedure TCastleModel.GoToFrame(AFrame: TFloatTime; const APause: Boolean = True);
begin
  if not(fAnimNode = nil) then
    begin
      if ((AFrame >= 0) and (AFrame <= fAnimNode.CycleInterval)) then
        begin
          fIsPaused := False;
          fAnimNode.Stop;
          if not APause then
            begin
//              Scene.ForceAnimationPose(gAnimNode.X3DName, AFrame, True, True);
              fAnimNode.Start(False, True, AFrame);
              WriteLnLog('Goto (Immediate) : ' + FloatToStr(AFrame) + ' (' + FloatToStr(fAnimNode.ElapsedTimeInCycle) + ')');
            end
          else
            begin
              fIsPaused := True;
              WriteLnLog('Goto (Pause) : ' + FloatToStr(AFrame));
            end;
          LastFrame := AFrame;
        end;
    end;
end;

procedure TCastleModel.Normalize;
begin
  if not(fScene = nil) then
    begin
    if not fScene.BoundingBox.IsEmptyOrZero then
      begin
        if fScene.BoundingBox.MaxSize > 0 then
          begin
            fScene.Center := Vector3(Min(fScene.BoundingBox.Data[0].X, fScene.BoundingBox.Data[1].X) + (fScene.BoundingBox.SizeX / 2),
                              Min(fScene.BoundingBox.Data[0].Y, fScene.BoundingBox.Data[1].Y) + (fScene.BoundingBox.SizeY / 2),
                              Min(fScene.BoundingBox.Data[0].Z, fScene.BoundingBox.Data[1].Z) + (fScene.BoundingBox.SizeZ / 2));
            fScene.Scale := Vector3(1 / fScene.BoundingBox.MaxSize,
                             1 / fScene.BoundingBox.MaxSize,
                             1 / fScene.BoundingBox.MaxSize);
            fScene.Translation := -fScene.Center;
          end;
      end;
    end;
end;

constructor TCastleModel.Create(AOwner: TComponent);
begin
  inherited;
  fActions := nil;
  fAnimNode := nil;
  fIsLooped := False;
  fIsPaused := False;
  fIsPlaying := False;
  fFrameLow := 0;
  fFrameHigh := 0;
  fLastFrame := 0;
  fIsNormalized := False;
  fRootNode := nil;
  fScene := TCastleScene.Create(AOwner);
  fTransform := nil;
  fUseFrames := False;
end;

destructor TCastleModel.Destroy;
begin
  inherited;
end;

procedure TCastleModel.Pause;
begin
  fIsPaused := True;
  fLastFrame := fAnimNode.ElapsedTimeInCycle;
  fAnimNode.Stop;
end;

procedure TCastleModel.RemoveAnimation;
begin
  fIsLooped := False;
  fIsPaused := False;
  fIsPlaying := False;
//  fAction := EmptyStr;
end;

procedure TCastleModel.Start;
begin
  fIsPaused := False;
  fAnimNode.Start(fIsLooped, True);
end;

procedure TCastleModel.Stop;
begin
  fIsPaused := False;
  fAnimNode.Stop;
end;



end.

