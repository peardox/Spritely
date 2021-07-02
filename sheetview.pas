unit SheetView;

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
  CastleGLUtils, multimodel, staging, Overlays, MiscFunctions;

type
  { TSheetViewer }

  TSheetViewer = class(TUIState)
    procedure BeforeRender; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
    procedure Resize; override; // TCastleUserInterface
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override; // TUIState
  private
    fSheetWidth: Cardinal;
    fSheetHeight: Cardinal;
    fControlWidth: Cardinal;
    VPBackImage: TCastleImageControl;
    MainControl: TCastleUserInterface;
    ControlPanel: TControlPanel;
 public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure LoadSheetViewer;

    property  ControlWidth: Cardinal read fControlWidth write fControlWidth;
    property  SheetWidth: Cardinal read fSheetWidth write fSheetWidth;
    property  SheetHeight: Cardinal read fSheetHeight write fSheetHeight;
  end;

var
  SheetViewer: TSheetViewer;

implementation
{$ifdef cgeapp}
uses AppInitialization, MainGameUnit;
{$else}
uses GUIInitialization, MainGameUnit;
{$endif}

constructor TSheetViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SheetWidth := 4096;
  SheetHeight := 4096;
  ControlWidth := 300;
  LoadSheetViewer;
end;

destructor TSheetViewer.Destroy;
begin
  inherited;
end;


procedure TSheetViewer.Start;
begin
  inherited;
end;

procedure TSheetViewer.Stop;
begin
  inherited;
end;

procedure TSheetViewer.BeforeRender;
begin
  inherited;
end;

procedure TSheetViewer.Resize;
var
  DesiredAspect: Single;
  ActualAspect: Single;
  ViewWidth: Single;
begin
  inherited;

  ViewWidth := StateContainer.Width - ControlWidth;
  DesiredAspect := SheetWidth / SheetHeight;
  ActualAspect := ViewWidth / StateContainer.Height;

  if DesiredAspect <= ActualAspect then
    begin
      if ViewWidth <= (StateContainer.Height / DesiredAspect) then
        begin
          MainControl.Height := StateContainer.Height;
          MainControl.Width := StateContainer.Height * DesiredAspect;
        end
      else
        begin
          MainControl.Height := StateContainer.Height;
          MainControl.Width := StateContainer.Height * DesiredAspect;
        end;
    end
  else
    begin
      if ViewWidth <= (StateContainer.Height / DesiredAspect) then
        begin
          MainControl.Width := ViewWidth;
          MainControl.Height := ViewWidth / DesiredAspect;
        end
      else
        begin
          MainControl.Width := ViewWidth;
          MainControl.Height := ViewWidth / DesiredAspect;
        end;
    end;

  MainControl.Left := Trunc((ViewWidth - MainControl.Width) / 2);
  MainControl.Bottom := Trunc((StateContainer.Height - MainControl.Height) / 2);

  if (TUIState.CurrentTop = SheetViewer) then
    begin
      VPBackImage.Image :=  MakeTransparentLayerRectGrid(SheetWidth, SheetHeight, Trunc(MainControl.Width), Trunc(MainControl.Height), (CastleApp.SpriteWidth), (CastleApp.SpriteHeight));
      VPBackImage.Left := MainControl.Left;
      VPBackImage.Bottom := MainControl.Bottom;
      VPBackImage.Width := MainControl.Width;
      VPBackImage.Height := MainControl.Height;
  end;

end;

procedure TSheetViewer.Render;
begin
  inherited;
end;

procedure TSheetViewer.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
end;


procedure TSheetViewer.LoadSheetViewer;
begin
  MainControl := TCastleUserInterface.Create(Self);
  MainControl.Height := Height;
  MainControl.Width := Width - ControlWidth;
  Self.InsertFront(MainControl);

  VPBackImage := TCastleImageControl.Create(MainControl);
  VPBackImage.OwnsImage := True;
  VPBackImage.Stretch := False;

  InsertFront(VPBackImage);

  ControlPanel := TControlPanel.Create(Self, ControlWidth, StateContainer.Height);
end;

end.

