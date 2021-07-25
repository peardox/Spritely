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
    TabSectionHeight: Single;
    TabSection: TCastleUserInterface;
    TabGroup: TCastleHorizontalGroup;
    TabVGroup: TCastleVerticalGroup;
    Tabs: Array [0..9] of TCastleRectangleControl;
    TabImages: Array [0..9] of TCastleImageControl;
    TabCurrent: TCastleRectangleControl;
    TabCaption: TCastleLabel;
    TabCaptionHeight : Single;
    BottomSection: TCastleUserInterface;
    CtlGrabSCreenBtn: TCastleButton;
    CtlViewModeBtn: TCastleButton;
    CtlCenterBtn: TCastleButton;
    CtlResetBtn: TCastleButton;
    procedure DoGrabScreen(Sender: TObject);
    procedure DoChangeViewMode(Sender: TObject);
    procedure DoCenter(Sender: TObject);
    procedure DoReset(Sender: TObject);
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
  ImgMargin: Single;

  procedure PlaceButton(var obj: TCastleButton; const BtnCol: Integer);
  begin
    obj.Left := (BtnCol * (BtnWidth  + BtnMargin)) + BtnMargin;
    obj.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
    obj.Width := BtnWidth;
    obj.Height := BtnHeight;
    obj.FontScale := BtnFontScale;
    obj.ImageScale := BtnImageScale;
  end;

  procedure CreateTab(ObjTab: TCastleRectangleControl;
    ObjTabImage: TCastleImageControl; TabColor: TCastleColor; AURL: String);
  begin
    ObjTab := TCastleRectangleControl.Create(TabGroup);
    ObjTab.Color := TabColor;
    ObjTab.Height := TabSectionHeight;
    ObjTab.Width := TabSectionHeight;
    TabGroup.InsertFront(ObjTab);

    ObjTabImage := TCastleImageControl.Create(ObjTab);
    ObjTabImage.Height := TabSectionHeight - ImgMargin;
    ObjTabImage.Width := TabSectionHeight - ImgMargin;
    ObjTabImage.Left := (ImgMargin / 2) - ObjTab.Border.AllSides;
    ObjTabImage.Bottom := (ImgMargin / 2) - ObjTab.Border.AllSides;
    ObjTabImage.URL := AURL;
    ObjTabImage.Stretch := True;
    ObjTab.InsertFront(ObjTabImage);

  end;

begin
  Create(AOwner);

  ImgMargin := 8;

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
  TabSectionHeight := 40;
  TabCaptionHeight := 24;

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

  TabSection := TCastleUserInterface.Create(Self);
  TabSection.Height := TabSectionHeight;
  TabSection.Width := Width;
  Self.InsertFront(TabSection);

  TabVGroup := TCastleVerticalGroup.Create(TabSection);
  TabVGroup.Height := TabSectionHeight + TabCaptionHeight;
  TabVGroup.Width := Width;
  TabSection.InsertFront(TabVGroup);

  TabGroup := TCastleHorizontalGroup.Create(TabVGroup);
  TabGroup.Height := TabSectionHeight;
  TabGroup.Width := Width;
  TabVGroup.InsertFront(TabGroup);

  TabCurrent := TCastleRectangleControl.Create(TabVGroup);
  TabCurrent.Color := White;
  TabCurrent.Height := TabCaptionHeight;
  TabCurrent.Width := Width;
  TabVGroup.InsertFront(TabCurrent);


  BottomSection := TCastleUserInterface.Create(Self);
  BottomSection.Height := Height - TopSectionHeight - TabSectionHeight - TabCaptionHeight;
  BottomSection.Width := Width;
  Self.InsertFront(BottomSection);

  TopSection.CreateButton(CtlGrabSCreenBtn, '', @DoGrabScreen, 'castle-data:/icons/camera.png');
  PlaceButton(CtlGrabScreenBtn, 0);
  TopSection.CreateButton(CtlViewModeBtn, '', @DoChangeViewMode, 'castle-data:/icons/pencil.png');
  PlaceButton(CtlViewModeBtn, 1);
  TopSection.CreateButton(CtlCenterBtn, '', @DoCenter, 'castle-data:/icons/plus.png');
  PlaceButton(CtlCenterBtn, 2);
  TopSection.CreateButton(CtlResetBtn, '', @DoReset, 'castle-data:/icons/minus.png');
  PlaceButton(CtlResetBtn, 3);

  CreateTab(Tabs[0], TabImages[0], White, 'castle-data:/flaticon/orientation.png');
  CreateTab(Tabs[1], TabImages[1], Vector4(0.8,0.8,0.8,1.0), 'castle-data:/flaticon/position.png');
  CreateTab(Tabs[2], TabImages[2], Vector4(0.8,0.8,0.8,1.0), 'castle-data:/flaticon/play.png');

  TabCaption := TCastleLabel.Create(TabCurrent);
  TabCaption.Caption := 'Orientation';
  TabCaption.Color := Black;
  TabCaption.PaddingHorizontal := 4;
  TabCurrent.InsertFront(TabCaption);

end;

procedure TControlPanel.Resize;
begin
  inherited;

  Height := ParentRect.Height;

  TopSection.Height := TopSectionHeight;
  TopSection.Width := Width;
//  TopSection.BorderColor := Red;
//  TopSection.Border.AllSides :=1;
  TopSection.Bottom := Height - TopSectionHeight;

  TabSection.Height := TabSectionHeight + TabCaptionHeight;
  TabSection.Width := Width;
//  TabSection.BorderColor := Blue;
//  TabSection.Border.AllSides :=1;
  TabSection.Bottom := Height - TopSectionHeight - TabSectionHeight - TabCaptionHeight;

  BottomSection.Height := Height - TopSectionHeight - TabSectionHeight - TabCaptionHeight;
  BottomSection.Width := Width;
//  BottomSection.BorderColor := Green;
//  BottomSection.Border.AllSides :=1;

  WriteLnLog('BottomSection.Height = ' + FloatTOStr(BottomSection.Height));
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

procedure TControlPanel.DoCenter(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.Transform.Center := WorkingModel.Transform.Center + Vector3(0, -0.1, 0);
end;

procedure TControlPanel.DoReset(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.Normalize;
end;

end.
                                                                               *
