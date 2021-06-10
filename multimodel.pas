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
  CastleLog, CastleTimeUtils, CastleRectangles, CastleRenderOptions,
  CastleQuaternions, AniTakeUtils;

type
  { TAnimationInfo }
  PAnimationInfo = ^TAnimationInfo;
  TAnimationInfo = Class(TComponent)
    private
      AnimNode: TTimeSensorNode;
      AnimName: String;
      AnimStart: TFloatTime; // Time the animation starts, may be after AnimLow
      AnimStop: TFloatTime; // Time the animation ends, may be before AnimHigh
      AnimLow: TFloatTime; // Always Zero
      AnimHigh: TFloatTime; // Always CycleInterval
      AnimLast: TFloatTime; // Stores time animation paused at
      IsPaused: Boolean; // Is animation paused
      IsLooped: Boolean;
      IsTakeOne: Boolean;
      IsUserDefined: Boolean;
      IsHidden: Boolean;
      fParentAnim: TAnimationInfo;
      procedure ReceivedIsActive(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
      procedure ReceivedElapsedTime(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    public
      constructor Create(AOwner: TComponent); override;
      constructor Create(AOwner: TComponent; const AName: String; const ASensor: TTimeSensorNode; const AIsLooped: Boolean = True);
      constructor Create(AOwner: TComponent; const ATake: TAniTake; const ASensor: TTimeSensorNode; const AParent: TAnimationInfo; const AIsLooped: Boolean = True; const ATakeFPS: Single = 30);
      property Sensor: TTimeSensorNode read AnimNode write AnimNode;
      property ParentAnim: TAnimationInfo read fParentAnim write fParentAnim;
  end;
  TAnimationInfoArray = Array of TAnimationInfo;

  { TCastleModel }
  TCastleModel = class(TCastleComponent)
  private
    fActions: TStringList;
    fCurrentAnimation: Integer;
    fHasAnimations: Boolean;
    fIsLocked: Boolean;
    fLockedScale: Single;
    fModelName: String;
    fRootNode: TX3DRootNode;
    fScene: TCastleScene;
    fSceneNode:  TX3DRootNode;
    fTransform: TTransformNode;
    fDebug: TDebugTransformBox;
    fBaseRotation: TVector3Single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  GetSpatial: TSceneSpatialStructures;
    procedure SetSpatial(const Value: TSceneSpatialStructures);
    function  GetIsLooped: Boolean;
    procedure SetIsLooped(const Value: Boolean);
    function  GetAnimation(I: Cardinal): TAnimationInfo;

    property  Actions: TStringList read fActions write fActions;
    property  Animations[I: Cardinal]: TAnimationInfo read GetAnimation;
    procedure AddAllAnimations;
    property  Debug: TDebugTransformBox read fDebug write fDebug;
    procedure FreeAllAnimations;
    property  CurrentAnimation: Integer read fCurrentAnimation write fCurrentAnimation;
    property  HasAnimations: Boolean read fHasAnimations write fHasAnimations;
    property  IsLocked: Boolean read fIsLocked write fIsLocked;
    property  LockedScale: Single read fLockedScale write fLockedScale;
    property  ModelName: String read fModelName write fModelName;
    property  BaseRotation: TVector3 read fBaseRotation write fBaseRotation;
    property  RootNode: TX3DRootNode read fRootNode write fRootNode;
    property  Scene: TCastleScene read fScene write fScene;
    property  Transform: TTransformNode read fTransform write fTransform;
    property  Spatial: TSceneSpatialStructures read GetSpatial write SetSpatial default [];
    property  IsLooped: Boolean read GetIsLooped write SetIsLooped default True;

    procedure ResetAnimationState(const IgnoreAffectedBy: TTimeSensorNode = nil);
    procedure PrepareResources(const Options: TPrepareResourcesOptions;
      const ProgressStep: boolean; const Params: TPrepareParams);
    procedure Load(const AURL: string; const AOptions: TSceneLoadOptions = []);

    procedure FreeScene;
    procedure RemoveScene(var AViewport: TCastleViewport);
    procedure AddAnimation(const AAction: String; const ASensor: TTimeSensorNode; const AIsLooped: Boolean = True);
    function  AddAnimation(const ATake: TAniTake; const ASensor: TTimeSensorNode; const AParent: TAnimationInfo; const AIsLooped: Boolean = True; const ATakeFPS: Single = 30): TAnimationInfo;
    function  CurrentFrame: TFloatTime;
    function  TotalFrames: TFloatTime;
    procedure GoToFrame(const AFrame: TFloatTime);
    procedure GoToFrame(const AName: String; const AFrame: TFloatTime);
    function  IsPaused: Boolean;
    procedure Normalize;
    procedure Pause;
    procedure Pause(const AName: String);
    procedure Resume;
    procedure Start;
    procedure Start(const AName: String);
    procedure Stop;
    procedure Stop(const AName: String);
    procedure SelectAnimation(const AName: String; const StartPlaying: Boolean = False);
  end;

implementation

{ TAnimationInfo }

constructor TAnimationInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

constructor TAnimationInfo.Create(AOwner: TComponent; const AName: String; const ASensor: TTimeSensorNode; const AIsLooped: Boolean = True);
begin
  Create(AOwner);
  AnimNode := ASensor;
  AnimName := AName;
  AnimStart := 0;
  AnimStop := ASensor.CycleInterval;
  AnimLow := 0;
  AnimHigh := ASensor.CycleInterval;
  AnimLast := 0;
  IsLooped := AIsLooped;
  IsTakeOne := False;
  ParentAnim := Self;
  IsUserDefined := False;
  IsHidden := False;
  IsPaused := False;
  AnimNode.EventIsActive.AddNotification(@ReceivedIsActive);
  AnimNode.EventElapsedTime.AddNotification(@ReceivedElapsedTime);
end;

constructor TAnimationInfo.Create(AOwner: TComponent; const ATake: TAniTake; const ASensor: TTimeSensorNode; const AParent: TAnimationInfo; const AIsLooped: Boolean = True; const ATakeFPS: Single = 30);
begin
  Create(AOwner);
  AnimNode := ASensor;
  AnimName := ATake.TakeName;
  AnimStart := ATake.TakeStart / ATakeFPS;
  AnimStop := (ATake.TakeStop + 1) / ATakeFPS;
  AnimLow := 0;
  AnimHigh := ASensor.CycleInterval;
  AnimLast := 0;
  IsLooped := AIsLooped;
  IsTakeOne := True;
  ParentAnim := AParent;
  IsUserDefined := True;
  IsHidden := False;
  IsPaused := True;
  WriteLnLog(AnimName + ' - ' + FloatToStr(AnimStart) + ' - ' + FloatToStr(AnimStop));
end;

procedure TAnimationInfo.ReceivedIsActive(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  Val: Boolean;
begin
  Val := (Value as TSFBool).Value;
  if not(Val) then
    begin
      if not IsPaused then
        begin
          {$ifdef logevents}
          WriteLnLog('TAnimationInfo.ReceivedIsActive - ' + BoolToStr(Val) + ' -> Starting : ' + FloatToStr(AnimStart));
          {$endif}
          AnimNode.Start(False, True, AnimStart);
        end
      {$ifndef logevents}
      ;
      {$else}
      else
        WriteLnLog('TAnimationInfo.ReceivedIsActive - ' + BoolToStr(Val) + ' -> Paused');
      {$endif}
    end;
  {$ifdef logevents}
  WriteLnLog('TAnimationInfo.ReceivedIsActive - ' + BoolToStr(Val));
  {$endif}
end;

procedure TAnimationInfo.ReceivedElapsedTime(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  Val: Double;
begin
  Val := (Value as TSFTime).Value;
  if Val >= AnimStop then
    begin
      AnimNode.Stop;
      {$ifdef logevents}
      WriteLnLog('TAnimationInfo.ReceivedElapsedTime - ' + FloatToStr(Val) + ' -> Stopping');
      {$endif}
    end;
end;

{ TCastleModel - Internal Scene Access }

function  TCastleModel.GetSpatial: TSceneSpatialStructures;
begin
  Result := fScene.Spatial;
end;

procedure TCastleModel.SetSpatial(const Value: TSceneSpatialStructures);
begin
  fScene.Spatial := Value;
end;

function  TCastleModel.GetAnimation(I: Cardinal): TAnimationInfo;
begin
  if(I >= 0) and (I < fActions.Count) then
    Result := TAnimationInfo(fActions.Objects[I])
  else
    raise Exception.Create('Animation index out of bounds');
end;

function  TCastleModel.GetIsLooped: Boolean;
var
  ANode: TAnimationInfo;
begin
  if(fCurrentAnimation >= 0) and (fCurrentAnimation < fActions.Count) then
    begin
      ANode := TAnimationInfo(fActions.Objects[fCurrentAnimation]);
      Result := ANode.IsLooped;
    end;
  Result := False;
end;

procedure TCastleModel.SetIsLooped(const Value: Boolean);
var
  ANode: TAnimationInfo;
begin
  if(fCurrentAnimation >= 0) and (fCurrentAnimation < fActions.Count) then
    begin
      ANode := TAnimationInfo(fActions.Objects[fCurrentAnimation]);
      ANode.IsLooped := Value;
    end;
end;

procedure TCastleModel.ResetAnimationState(const IgnoreAffectedBy: TTimeSensorNode = nil);
begin
  if not(fCurrentAnimation = -1) then
    begin
      Stop; // Also resets IsPaused
    end;

  fCurrentAnimation := -1;

  fScene.ResetAnimationState(IgnoreAffectedBy);
end;

procedure TCastleModel.PrepareResources(const Options: TPrepareResourcesOptions;
  const ProgressStep: boolean; const Params: TPrepareParams);
begin
  fScene.PrepareResources(Options, ProgressStep, Params);
end;

procedure TCastleModel.RemoveScene(var AViewport: TCastleViewport);
begin
  if not(fScene = nil) then
    begin
      AViewport.Items.Remove(fScene);
    end;
end;

procedure TCastleModel.FreeScene;
begin
  if not(fScene = nil) then
    begin
      if fScene.OwnsRootNode then
        FreeAndNil(fScene);
    end;
end;

procedure TCastleModel.Load(const AURL: string; const AOptions: TSceneLoadOptions);
begin
  if not(fSceneNode = nil) then
    FreeAndNil(fSceneNode);

  fSceneNode := LoadNode(AURL, True);
  if not(fSceneNode = nil) Then
    begin
      fRootNode := TX3DRootNode.Create;
      fTransform := TTransformNode.Create;

      fTransform.AddChildren(fSceneNode);
      fRootNode.AddChildren(fTransform);

      fScene := TCastleScene.Create(Self);
      fScene.Load(fRootNode, True, AOptions);

      fModelName := AURL;

      AddAllAnimations;
      fDebug := TDebugTransformBox.Create(fScene);
      fDebug.Parent := fScene;
      fDebug.BoxColor := Vector4(0,0,0, 1);
      fDebug.Exists := False;

      Normalize;
    end;
end;

{ TCastleModel }

procedure TCastleModel.AddAllAnimations;
var
  I: Integer;
begin
  if(fScene.AnimationsList.Count > 0) then
    begin
      fScene.ProcessEvents := True;
      fHasAnimations := True;
      fActions := TStringList.Create;
      fActions.Sorted := True;
      fActions.Duplicates := dupError;
      for I := 0 to fScene.AnimationsList.Count - 1 do
        begin
          AddAnimation(fScene.AnimationsList[I], fScene.AnimationTimeSensor(fScene.AnimationsList[I]));
        end;
    end
  else
    fHasAnimations := False;
end;

procedure TCastleModel.FreeAllAnimations;
var
  I: Integer;
  obj: TAnimationInfo;
begin
  if fHasAnimations then
    begin
      if(fActions.Count > 0) then
        begin
          for I := 0 to fActions.Count - 1 do
            begin
              obj := TAnimationInfo(fActions.Objects[I]);
              obj.AnimNode := nil;
              obj.fParentAnim := nil;
              FreeAndNil(obj);
            end;
        end;
    end;
end;

procedure TCastleModel.AddAnimation(const AAction: String; const ASensor: TTimeSensorNode; const AIsLooped: Boolean = True);
var
  ainfo: TAnimationInfo;
begin
  ainfo := TAnimationInfo.Create(Self, AAction, ASensor, IsLooped);
  fActions.AddObject(AAction, ainfo);
end;

function TCastleModel.AddAnimation(const ATake: TAniTake; const ASensor: TTimeSensorNode; const AParent: TAnimationInfo; const AIsLooped: Boolean = True; const ATakeFPS: Single = 30): TAnimationInfo;
var
  ainfo: TAnimationInfo;
begin
  if fActions = nil then
    begin
      fScene.ProcessEvents := True;
      fHasAnimations := True;
      fActions := TStringList.Create;
      fActions.Sorted := True;
      fActions.Duplicates := dupError;
    end;

  ainfo := TAnimationInfo.Create(Self, ATake, ASensor, AParent);
  fActions.AddObject(ATake.TakeName, ainfo);

  Result := ainfo;
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
            fScene.Scale := Vector3(2 / fScene.BoundingBox.MaxSize,
                             2 / fScene.BoundingBox.MaxSize,
                             2 / fScene.BoundingBox.MaxSize);
            fScene.Translation := -fScene.Center;
            if not(fTransform = nil) then
              fTransform.Center := fScene.Center;

          end;
      end;
    end;
end;

constructor TCastleModel.Create(AOwner: TComponent);
begin
  inherited;
  fActions := nil;
  fCurrentAnimation := -1;
  fHasAnimations := False;
  fIsLocked := False;
  fLockedScale := 1.0;
  fModelName := EmptyStr;
  fBaseRotation := Vector3(0, 0, 0);
  fRootNode := nil;
  fTransform := nil;
  fSceneNode := nil;
  fScene := nil;
end;

destructor TCastleModel.Destroy;
begin
  FreeAllAnimations;
  FreeAndNil(fActions);
  fDebug.Exists := False;
  FreeAndNil(fDebug);
  inherited;
end;

procedure TCastleModel.Start(const AName: String);
var
  I: Integer;
  ANode: TAnimationInfo;
begin
  if (fCurrentAnimation = -1) then
    begin
      if fActions.Find(AName, I) then
        begin
          fCurrentAnimation := I;
          Start;
        end;
    end;
end;

procedure TCastleModel.Resume;
var
 ANode: TAnimationInfo;
 PNode: TAnimationInfo;
begin
  if(fCurrentAnimation >= 0) and (fCurrentAnimation < fActions.Count) then
    begin
      ANode := TAnimationInfo(fActions.Objects[fCurrentAnimation]);
      ANode.IsPaused := False;
      if ANode.IsTakeOne then
        begin
          PNode := ANode.ParentAnim;
          if not(PNode = nil) then
            begin
              PNode.IsPaused := False;
              PNode.AnimNode.Start(False, True, PNode.AnimLast);
            end;
        end
      else
        ANode.AnimNode.Start(False, True, ANode.AnimLast);

    end;
end;

procedure TCastleModel.Start;
var
 ANode: TAnimationInfo;
 PNode: TAnimationInfo;
begin
  if(fCurrentAnimation >= 0) and (fCurrentAnimation < fActions.Count) then
    begin
      ANode := TAnimationInfo(fActions.Objects[fCurrentAnimation]);
      ANode.IsPaused := False;
      if ANode.IsTakeOne then
        begin
          PNode := ANode.ParentAnim;
          if not(PNode = nil) then
            begin
              PNode.IsPaused := False;
              PNode.AnimLast := PNode.AnimStart;
              PNode.AnimNode.FakeTime(PNode.AnimLast, False, True, fScene.NextEventTime);
              PNode.AnimNode.Start(False, True, PNode.AnimStart);
            end;
        end
      else
        ANode.AnimNode.Start(False, True, ANode.AnimStart);
    end;
end;

procedure TCastleModel.Stop(const AName: String);
var
  I: Integer;
begin
  if (fCurrentAnimation = -1) then
    begin
      if fActions.Find(AName, I) then
        begin
          fCurrentAnimation := I;
          Stop;
        end;
    end;
end;

procedure TCastleModel.Stop;
var
  ANode: TAnimationInfo;
  PNode: TAnimationInfo;
begin
  if(fCurrentAnimation >= 0) and (fCurrentAnimation < fActions.Count) then
    begin
      WriteLnLog('TCastleModel.Stop - Stopping - ' + IntToStr(fCurrentAnimation));
      ANode := TAnimationInfo(fActions.Objects[fCurrentAnimation]);
      PNode := ANode.ParentAnim;
      if not(PNode = nil) then
        begin
          PNode.AnimNode.Stop;
          PNode.IsPaused := True;
        end
      else
        begin
          ANode.AnimNode.Stop;
          ANode.IsPaused := True;
        end;
    end;
end;

procedure TCastleModel.Pause(const AName: String);
var
  I: Integer;
begin
  if fActions.Find(AName, I) then
    begin
      if (fCurrentAnimation = I) then
        begin
          Pause;
        end;
    end;
end;

procedure TCastleModel.Pause;
var
  ANode: TAnimationInfo;
  PNode: TAnimationInfo;
begin
  if(fCurrentAnimation >= 0) and (fCurrentAnimation < fActions.Count) then
    begin
      ANode := TAnimationInfo(fActions.Objects[fCurrentAnimation]);
      if ANode.IsPaused then
        begin
          WriteLnLog('TCastleModel.Pause - Resuming - ' + IntToStr(fCurrentAnimation));
          Resume;
        end
      else
        begin
          if ANode.IsTakeOne then
            begin
              PNode := ANode.ParentAnim;
              if not(PNode = nil) then
                begin
                  PNode.AnimNode.Stop;
                  PNode.AnimLast := PNode.AnimNode.ElapsedTimeInCycle;
                  PNode.IsPaused := True;
                  ANode.AnimLast := PNode.AnimLast;
                  ANode.IsPaused := True;
                end;
            end
          else
            begin
              ANode.AnimNode.Stop;
              ANode.AnimLast := ANode.AnimNode.ElapsedTimeInCycle;
              ANode.IsPaused := True;
            end;
        end;
    end;
end;

procedure TCastleModel.GoToFrame(const AName: String; const AFrame: TFloatTime);
var
  I: Integer;
begin
  if fActions.Find(AName, I) then
    begin
      if (fCurrentAnimation = I) then
        begin
          GotoFrame(AFrame);
        end;
    end;
end;

procedure TCastleModel.GoToFrame(const AFrame: TFloatTime);
var
 ANode: TAnimationInfo;
 PNode: TAnimationInfo;
begin
  WriteLnLog('TCastleModel.GoToFrame - ' + FloatToStr(AFrame) + ' (' + IntToStr(fCurrentAnimation) + ')');
  if(fCurrentAnimation >= 0) and (fCurrentAnimation < fActions.Count) then
    begin
      ANode := TAnimationInfo(fActions.Objects[fCurrentAnimation]);
      if ANode.IsTakeOne then
        begin
          PNode := ANode.ParentAnim;
          if not(PNode = nil) then
            begin
              PNode.AnimLast := AFrame;
              PNode.AnimNode.FakeTime(ANode.AnimLast, False, True, fScene.NextEventTime);
              ANode.AnimLast := AFrame;
            end;
        end
      else
        begin
          ANode.AnimLast := AFrame;
          ANode.AnimNode.FakeTime(ANode.AnimLast, False, True, fScene.NextEventTime);
        end;
    end;
end;

procedure TCastleModel.SelectAnimation(const AName: String; const StartPlaying: Boolean = False);
var
 ANode: TAnimationInfo;
 PNode: TAnimationInfo;
 I: Integer;
 PrevAnimWasRunning: Boolean;
 PrevAnimWasTPose: Boolean;
begin
  PrevAnimWasTPose := False;
  PrevAnimWasRunning := False;
  if fActions.Find(AName, I) then
    begin
      if(fCurrentAnimation >= 0) and (fCurrentAnimation < fActions.Count) then
        begin
          if not(fCurrentAnimation = I) then
            begin
              ANode := TAnimationInfo(fActions.Objects[fCurrentAnimation]);
              if not(ANode.IsPaused) then
                begin
                  Stop;
                  PrevAnimWasRunning := True;
                end;
            end;
        end
      else
        PrevAnimWasTPose := True;

      WriteLnLog('TCastleModel.SelectAnimation - ' + AName + ' (' + IntToStr(fCurrentAnimation) + ' / ' + IntToStr(I) + ')');
      fCurrentAnimation := I;
      ANode := TAnimationInfo(fActions.Objects[fCurrentAnimation]);
      if ANode.IsTakeOne then
        begin
          PNode := ANode.ParentAnim;
          if not(PNode = nil) then
            begin
              PNode.AnimStart := ANode.AnimStart;
              PNode.AnimStop := ANode.AnimStop;
              PNode.AnimLast := PNode.AnimStart;
              PNode.IsPaused := True;
              ANode.IsPaused := True;
              PNode.AnimNode.FakeTime(PNode.AnimLast, False, True, fScene.NextEventTime);
              Start;
            end;
        end
      else
        begin
        GotoFrame(ANode.AnimStart);
        if PrevAnimWasRunning then
          Start
        else
          begin
            if PrevAnimWasTPose then
              begin
                if StartPlaying then
                  Start
                else
                  ANode.IsPaused := True;
              end
            else
              Stop;
          end;
        end;
    end;
end;

function TCastleModel.CurrentFrame: TFloatTime;
var
 ANode: TAnimationInfo;
begin
  Result := -1;

  if(fCurrentAnimation >= 0) and (fCurrentAnimation < fActions.Count) then
    begin
      ANode := TAnimationInfo(fActions.Objects[fCurrentAnimation]);
      Result := ANode.AnimNode.ElapsedTimeInCycle;
    end;
end;

function TCastleModel.TotalFrames: TFloatTime;
var
 ANode: TAnimationInfo;
begin
  Result := -1;

  if(fCurrentAnimation >= 0) and (fCurrentAnimation < fActions.Count) then
    begin
      ANode := TAnimationInfo(fActions.Objects[fCurrentAnimation]);
      Result := ANode.AnimHigh;
    end;
end;

function  TCastleModel.IsPaused: Boolean;
var
 ANode: TAnimationInfo;
begin
  Result := False;

  if(fCurrentAnimation >= 0) and (fCurrentAnimation < fActions.Count) then
    begin
      ANode := TAnimationInfo(fActions.Objects[fCurrentAnimation]);
      Result := ANode.IsPaused;
    end;
end;

end.

