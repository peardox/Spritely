unit multimodel;

{$mode objfpc}{$H+}
{$define logevents}

interface

uses
  Classes, SysUtils,
  CastleColors, CastleTriangles, CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform,
  CastleViewport, CastleCameras, CastleProjection,
  X3DNodes, X3DFields, X3DTIme, X3DLoad, CastleBoxes,
  CastleImages, CastleGLImages, CastleDebugTransform,
  CastleTextureImages, CastleCompositeImage,
  CastleLog, CastleTimeUtils, CastleRectangles, CastleRenderOptions;

type
  { TTimedEvent }
  TTimedEventListener = class(TComponent)
    procedure ReceivedIsActive(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    procedure ReceivedElapsedTime(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  end;

  { TCastleModel }
  TCastleModel = class(TCastleComponent)
    fAnimNode: TTimeSensorNode;
  private
    fAction: String;
    fIsLooped: Boolean;
    fIsPaused: Boolean;
    fIsPlaying: Boolean;
    fFrameLow: TFloatTime;
    fFrameHigh: TFloatTime;
    fFrameLast: TFloatTime;
    fIsNormalized: Boolean;
    fRootNode: TRoorNode;
    fScene: TCastleScene;
    fTransform: TTransformNode;
  public
    property  Action: String read fAction write fAction;
    property  AnimNode: TTimeSensorNode read fAnimNode write fAnimNode;
    property  FrameLow: TFloatTime read fFrameLow write fFrameLow;
    property  FrameHigh: TFloatTime read fFrameHigh write fFrameHigh;
    property  FrameLast: TFloatTime read fFrameLast write fFrameLast;
    property  IsLooped: Boolean read fIsLooped write fIsLooped;
    property  IsPaused: Boolean read fIsPaused write fIsPaused;
    property  IsPlaying: Boolean read fIsPlaying write fIsPlaying;
    property  RootNode: TRoorNode read fRootNode write fRootNode;
    property  Scene: TCastleScene read fScene write fScene;
    property  Transform: TTransformNode read fTransform write fTransform;
    procedure AddAnimation(const AAction: String; const ASensor: TTimeSensorNode; const AIsLooped: Boolean = True);
    procedure GoToFrame(AFrame: TFloatTime; const APause: Boolean = True);
    procedure Normalize(const AForced: Boolean = False);
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
  if not(CastleApp.gAnimNode = nil) then
    begin
      if CastleApp.gUseFrames and (CastleApp.gFrameLow = 0) then
        begin
          Owner.FrameLow := (380 / 1500) * Owner.AnimNode.CycleInterval;
          Owner.FrameHigh := (399 / 1500) * Owner.AnimNode.CycleInterval;
        end;
    end;
  if not(Val) then
    begin
      if not CastleApp.gIsPaused then
        begin
          {$ifdef logevents}
          WriteLnLog('ReceivedIsActive : ' + BoolToStr(Val) + ' -> GotoFrame : ' + FloatToStr(CastleApp.gFrameLow));
          {$endif}
          CastleApp.GoToFrame(CastleApp.gFrameLow, False);
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
  if Val = 10.395 then
    WriteLnLog('ReceivedElapsedTime (10.395) : ' + FloatToStr(Val));
  if Val >= CastleApp.gFrameHigh then
    begin
      CastleApp.gAnimNode.Stop;
      {$ifdef logevents}
      WriteLnLog('ReceivedElapsedTime (Stopping) : ' + FloatToStr(Val));
      {$endif}
    end;

end;

procedure TCastleModel.AddAnimation(const AAction: String; const ASensor: TTimeSensorNode; const AIsLooped: Boolean = True);
begin
  fAnimNode := ASensor;
  fIsLooped := AIsLooped;
  fAction := AAction;
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
          fLastFrame := AFrame;
        end;
    end;
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
  fAction := EmptyStr;
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

