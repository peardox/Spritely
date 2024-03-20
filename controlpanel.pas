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
  CastleTextureImages, CastleLog,
  CastleApplicationProperties, CastleTimeUtils, CastleKeysMouse,
  CastleGLUtils, multimodel, staging, MiscFunctions, ControlPanelControls,
  CastlePageControl, SpriteControlPanel;

type
  { TControlPanel }

  TControlPanel = class(TCastleUserInterface)
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; const AWidth: Single);
    procedure Resize; override;
  private
    SpritePanel: TSpriteControlPanel;
    TopSectionHeight: Single;
    TopSection: TCastleUserInterface;
    CtlGrabSCreenBtn: TCastleButton;
    CtlViewModeBtn: TCastleButton;
    CtlCenterBtn: TCastleButton;
    CtlResetBtn: TCastleButton;
    procedure DoGrabScreen(Sender: TObject);
    procedure DoChangeViewMode(Sender: TObject);
    procedure DoDebug(Sender: TObject);
    procedure DoReset(Sender: TObject);
  public
    TabTest: TCastlePageControl;
    PanelWidth: Single;
  end;

implementation

{$ifdef cgeapp}
uses MainGameUnit;
{$else}
uses GUIInitialization, MainGameUnit;
{$endif}

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
  BtnImageScale := (BtnHeight / 64) * BtnFontScale;
  TopSectionHeight := BtnHeight + (2 * BtnMargin);

  Width := AWidth - 2;
  Height := ParentRect.Height - 2;

  PanelWidth := AWidth - 2;
  Anchor(hpRight);
  Anchor(vpTop);
  BorderColor := White;
  Border.AllSides := 1;

  TUIState(AOwner).InsertFront(Self);

  TopSection := TCastleUserInterface.Create(Self);
  TopSection.Height := TopSectionHeight;
  TopSection.Width := Width;
  TopSection.HorizontalAnchorParent := hpRight;
  TopSection.HorizontalAnchorSelf := hpRight;
  TopSection.HorizontalAnchorDelta := 0;
  TopSection.VerticalAnchorParent := vpTop;
  TopSection.VerticalAnchorSelf := vpTop;
  TopSection.VerticalAnchorDelta := 0;
  Self.InsertFront(TopSection);

  TabTest := TCastlePageControl.Create(Self);

  TabTest.Height := Height - TopSectionHeight;
  TabTest.Width := Width;
  TabTest.HorizontalAnchorParent := hpLeft;
  TabTest.HorizontalAnchorSelf := hpLeft;
  TabTest.HorizontalAnchorDelta := 0;
  TabTest.VerticalAnchorParent := vpTop;
  TabTest.VerticalAnchorSelf := vpTop;
  TabTest.VerticalAnchorDelta :=  -TopSectionHeight;
//  TabTest.Border.AllSides := 1;
//  TabTest.BorderColor := Blue;

  Self.InsertFront(TabTest);

  SpritePanel := TSpriteControlPanel.Create(AOwner, AWidth, TabTest);

  TabTest.AddTab('Orientation', 'castle-data:/icons/orientation.png', SpritePanel);
  TabTest.AddTab('Position', 'castle-data:/icons/position.png');
  TabTest.AddTab('Define Frames', 'castle-data:/icons/record.png');

  TopSection.CreateButton(CtlGrabSCreenBtn, '', @DoGrabScreen, 'castle-data:/icons/b_camera.png');
  PlaceButton(CtlGrabScreenBtn, 0);
  TopSection.CreateButton(CtlViewModeBtn, '', @DoChangeViewMode, 'castle-data:/icons/b_view.png');
  PlaceButton(CtlViewModeBtn, 1);
  TopSection.CreateButton(CtlCenterBtn, '', @DoDebug, 'castle-data:/icons/b_debug.png');
  PlaceButton(CtlCenterBtn, 2);
  TopSection.CreateButton(CtlResetBtn, '', @DoReset, 'castle-data:/icons/b_settings.png');
  PlaceButton(CtlResetBtn, 3);

end;

procedure TControlPanel.Resize;
begin
  inherited;

  Height := ParentRect.Height - 2;

  TopSection.Height := TopSectionHeight;
  TopSection.Width := PanelWidth - Border.Left - Border.Right;
//  TopSection.Border.AllSides := 1;
//  TopSection.BorderColor := Red;

  TabTest.Height := Height - TopSectionHeight;
  TabTest.Width := PanelWidth - Border.Left - Border.Right;

  TabTest.ExtResize;
  SpritePanel.ExtResize;

  WriteLnLog('Height : ' + FloatToStr(Height));
  WriteLnLog('TopSection.Height : ' + FloatToStr(TopSection.Height));
  WriteLnLog('TabTest.Height : ' + FloatToStr(TabTest.Height));
end;

procedure TControlPanel.DoGrabScreen(Sender: TObject);
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
      ViewMode := ViewMode + 1;
end;

procedure TControlPanel.DoDebug(Sender: TObject);
begin
  with Parent as TCastleApp do
    begin
      WorkingModel.Debug.Exists := not WorkingModel.Debug.Exists;
      {$ifndef cgeapp}
      CastleForm.DebugBoxMenu.Checked := WorkingModel.Debug.Exists;
      {$endif}
    end;
end;

procedure TControlPanel.DoReset(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.Normalize;
end;

end.
                                                                               *
