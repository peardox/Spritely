unit ControlPanel;

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
  X3DNodes, X3DFields, X3DTIme, CastleNotifications, CastleFilesUtils,
  CastleImages, CastleGLImages, CastleRectangles, CastleQuaternions,
  CastleTextureImages, CastleCompositeImage, CastleLog,
  CastleApplicationProperties, CastleTimeUtils, CastleKeysMouse,
  CastleGLUtils, multimodel, staging, MiscFunctions, ControlPanelControls;

type
  { TControlPanel }

  TControlPanel = class(TCastleUserInterface)
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; const AWidth: Single);
    procedure Resize; override;
  protected
    TopSectionHeight: Single;
    TopSection: TCastleUserInterface;
    BottomSection: TCastleUserInterface;
    CtlGrabSCreenBtn: TCastleButton;
    CtlViewModeBtn: TCastleButton;
    procedure DoGrabSCreen(Sender: TObject);
    procedure DoChangeViewMode(Sender: TObject);
  end;

implementation

uses MainGameUnit;

{ TControlPanel }

constructor TControlPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

constructor TControlPanel.Create(AOwner: TComponent; const AWidth: Single);
var
  BtnWidth: Single;
  BtnHeight: Single;
  BtnFontScale: Single;
  BtnImageScale: Single;
  BtnMargin: Single;
  BtnRow: Cardinal;

  procedure PlaceButton(var obj: TCastleButton; const BtnCol: Integer);
  begin
    obj.Left := (BtnCol * (BtnWidth  + BtnMargin)) + BtnMargin;
    obj.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
    obj.Width := BtnWidth;
    obj.Height := BtnHeight;
    obj.FontScale := BtnFontScale;
    obj.ImageScale := BtnImageScale;
  end;
begin
  Create(AOwner);

  BtnRow := 0;
  BtnMargin := 10;
  {$if defined(ANDROID)}
  BtnHeight := 90;
  BtnFontScale := 0.8;
  {$elseif defined(CASTLE_IOS)}
  BtnHeight := 90;
  BtnFontScale := 0.8;
  {$else}
  BtnHeight := 40;
  BtnFontScale := 0.8;
  {$endif}
  BtnWidth := BtnHeight;
  BtnImageScale := (BtnHeight / 512) * BtnFontScale;
  TopSectionHeight := BtnHeight + (2 * BtnMargin);

  Width := AWidth;
  Height := ParentRect.Height;

  Anchor(hpRight);
  Anchor(vpTop);
  BorderColor := White;
  Border.AllSides :=1;

  TUIState(AOwner).InsertFront(Self);

  TopSection := TCastleUserInterface.Create(Self);
  TopSection.Height := TopSectionHeight;
  TopSection.Width := Width;
  Self.InsertFront(TopSection);

  BottomSection := TCastleUserInterface.Create(Self);
  BottomSection.Height := Height - TopSectionHeight;
  BottomSection.Width := Width;
  Self.InsertFront(BottomSection);

  TopSection.CreateButton(CtlGrabSCreenBtn, '', @DoGrabSCreen, 'castle-data:/icons/camera.png');
  PlaceButton(CtlGrabSCreenBtn, 0);
  TopSection.CreateButton(CtlViewModeBtn, '', @DoChangeViewMode, 'castle-data:/icons/pencil.png');
  PlaceButton(CtlViewModeBtn, 1);

end;

procedure TControlPanel.Resize;
begin
  inherited;

  Height := ParentRect.Height;

  TopSection.Height := TopSectionHeight;
  TopSection.Width := Width;
  TopSection.BorderColor := Red;
  TopSection.Border.AllSides :=1;
  TopSection.Bottom := Height - TopSectionHeight;

  BottomSection.Height := Height - TopSectionHeight;
  BottomSection.Width := Width;
  BottomSection.BorderColor := Green;
  BottomSection.Border.AllSides :=1;

  WriteLnLog('BottomSection.Height = ' + FloatTOStr(BottomSection.Height));
end;

procedure TControlPanel.DoGrabSCreen(Sender: TObject);
var
  Sprite: TCastleImage;
  SName: String;
begin
  with Parent as TCastleApp do
    begin
      if not (WorkingModel.Scene = nil) then
        begin
          Sprite := CastleApp.CreateSpriteImage(CastleApp.WorkingModel.Scene, SpriteWidth * OverSample, SpriteHeight * OverSample, CastleApp.UseTransparency);
          if not(Sprite = nil) then
            begin
              if (OverSample > 1) then
                begin
                  Sprite.Resize(SpriteWidth, SpriteHeight, riLanczos); // Mitchel);
                end;
              SName := FileNameAutoInc('grab_%4.4d.png');
              SaveImage(Sprite, SName);
              FreeAndNil(Sprite);
            end;
        end;
    end;
end;

procedure TControlPanel.DoChangeViewMode(Sender: TObject);
begin
  with Parent as TCastleApp do
    begin
      ViewMode := ViewMode + 1;
    end;
end;

end.

