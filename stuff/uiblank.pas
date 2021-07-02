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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure LoadSheetViewer;
  end;

var
  SheetViewer: TSheetViewer;

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}

constructor TSheetViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  LoadViewport;
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
begin
  inherited;
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
end;

end.

