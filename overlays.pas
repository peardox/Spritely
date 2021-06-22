unit Overlays;

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
  CastleGLUtils, multimodel, staging, MiscFunctions;

type
  { TCastleOverlay }

  TCastleOverlay = class(TUIState)
    constructor Create(AOwner: TComponent); override;
    procedure BeforeRender; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
    procedure Resize; override; // TCastleUserInterface
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override; // TUIState
  private
    Overlay: TCastleRectangleControl;
    Screen: TCastleRectangleControl;
  public
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
  end;

implementation

constructor TCastleOverlay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FullSize := True;

  Overlay := TCastleRectangleControl.Create(AOwner);
  Overlay.FullSize := True;
  Overlay.Color := Vector4(0, 0, 0, 0.25);

  InsertFront(Overlay);
end;

procedure TCastleOverlay.Start;
begin
  inherited;

  Screen := TCastleRectangleControl.Create(Overlay);
  Screen.HorizontalAnchorParent := hpMiddle;
  Screen.HorizontalAnchorSelf := hpMiddle;
  Screen.VerticalAnchorParent := vpMiddle;
  Screen.VerticalAnchorSelf := vpMiddle;
  Screen.Width := Trunc(StateContainer.Width * 0.75);
  Screen.Height := Trunc(StateContainer.Height * 0.75);
  Screen.Color := HexToColor('C8C8C8');

  InsertFront(Screen);
end;

procedure TCastleOverlay.Stop;
begin
end;

procedure TCastleOverlay.BeforeRender;
begin
end;

procedure TCastleOverlay.Render;
begin
end;

procedure TCastleOverlay.Resize;
begin
  inherited;

  Screen.Width := Trunc(StateContainer.Width * 0.75);
  Screen.Height := Trunc(StateContainer.Height * 0.75);
end;

procedure TCastleOverlay.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
end;


end.

