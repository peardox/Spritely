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
  private
  protected
    TopSectionHeight: Single;
    TopSection: TCastleUserInterface;
    TabSectionHeight: Single;
    TabSection: TCastleUserInterface;
    TabHGroup: TCastleHorizontalGroup;
    TabVGroup: TCastleVerticalGroup;
    Tabs: Array [0..9] of TCastleRectangleControl;
    TabImages: Array [0..9] of TCastleImageControl;
    TabIndex: Array [0..9] of Integer;
    TabActiveColor: TCastleColor;
    TabHoverColor: TCastleColor;
    TabInActiveColor: TCastleColor;
    TabCurrent: TCastleRectangleControl;
    TabActive: TCastleRectangleControl;
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
    procedure DoMouseEnterTab(const Sender: TCastleUserInterface);
    procedure DoMouseLeaveTab(const Sender: TCastleUserInterface);
  public
    ImgMargin: Single;
  end;

implementation

uses MainGameUnit;

{ TControlPanel }

constructor TControlPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TControlPanel.DoMouseEnterTab(const Sender: TCastleUserInterface);
begin
  if not(TabActive = Sender) then
    begin
      with Sender as TCastleRectangleControl do
        begin
          TCastleRectangleControl(Sender).Color := Vector4(0.9, 0.9, 0.9, 1.0);
          TCastleRectangleControl(Sender).Left := (TControlPanel(Parent).ImgMargin / 2) - Sender.Border.AllSides + 1;
          TCastleRectangleControl(Sender).Bottom := (TControlPanel(Parent).ImgMargin / 2) - Sender.Border.AllSides + 1;
        end;
    end;
end;

procedure TControlPanel.DoMouseLeaveTab(const Sender: TCastleUserInterface);
begin
  if not(TabActive = Sender) then
    begin
      TCastleRectangleControl(Sender).Color := Vector4(0.8, 0.8, 0.8, 1.0);
      TCastleRectangleControl(Sender).Left := (TControlPanel(Parent).ImgMargin / 2) - Sender.Border.AllSides;
      TCastleRectangleControl(Sender).Bottom := (TControlPanel(Parent).ImgMargin / 2) - Sender.Border.AllSides;
    end
  else
    begin
      TCastleRectangleControl(Sender).Color := Vector4(1.0, 1.0, 1.0, 1.0);
      TCastleRectangleControl(Sender).Left := (TControlPanel(Parent).ImgMargin / 2) - Sender.Border.AllSides;
      TCastleRectangleControl(Sender).Bottom := (TControlPanel(Parent).ImgMargin / 2) - Sender.Border.AllSides;
    end;
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

  procedure CreateTab(var ObjTab: TCastleRectangleControl;
    var ObjTabImage: TCastleImageControl; TabColor: TCastleColor; AURL: String);
  begin
    ObjTab := TCastleRectangleControl.Create(TabHGroup);
    ObjTab.Color := TabColor;
    ObjTab.Height := TabSectionHeight;
    ObjTab.Width := TabSectionHeight;
    ObjTab.OnInternalMouseEnter := @DoMouseEnterTab;
    ObjTab.OnInternalMouseLeave := @DoMouseLeaveTab;
    TabHGroup.InsertFront(ObjTab);

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
  TabActiveColor := Vector4(1.0, 1.0, 1.0, 1.0);
  TabInActiveColor := Vector4(0.8, 0.8, 0.8, 1.0);

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
  Border.AllSides := 1;

  TUIState(AOwner).InsertFront(Self);

  TopSection := TCastleUserInterface.Create(Self);
  TopSection.Height := TopSectionHeight;
  TopSection.Width := Width;
  Self.InsertFront(TopSection);

  TabSection := TCastleUserInterface.Create(Self);
  TabSection.Height := TabSectionHeight + TabCaptionHeight;
  TabSection.Width := Width;
  Self.InsertFront(TabSection);

  TabVGroup := TCastleVerticalGroup.Create(TabSection);
  TabVGroup.Height := TabSectionHeight + TabCaptionHeight;
  TabVGroup.Width := Width;
  TabSection.InsertFront(TabVGroup);

  TabHGroup := TCastleHorizontalGroup.Create(TabVGroup);
  TabHGroup.Height := TabSectionHeight;
  TabHGroup.Width := Width;
  TabVGroup.InsertFront(TabHGroup);

  TabCurrent := TCastleRectangleControl.Create(TabVGroup);
  TabCurrent.Color := TabActiveColor;
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

  CreateTab(Tabs[0], TabImages[0], TabActiveColor, 'castle-data:/icons/orientation.png');
  CreateTab(Tabs[1], TabImages[1], TabInActiveColor, 'castle-data:/icons/position.png');
  CreateTab(Tabs[2], TabImages[2], TabInActiveColor, 'castle-data:/icons/record.png');

  TabCaption := TCastleLabel.Create(TabCurrent);
  TabCaption.Caption := 'Orientation';
  TabCaption.Color := Black;
  TabCaption.PaddingHorizontal := 4;
  TabCurrent.InsertFront(TabCaption);

  TabActive := Tabs[0];
end;

procedure TControlPanel.Resize;
begin
  inherited;

  Height := ParentRect.Height;

  TopSection.Height := TopSectionHeight;
  TopSection.Width := Width;
  TopSection.Bottom := Height - TopSectionHeight;

  TabSection.Height := TabSectionHeight + TabCaptionHeight;
  TabSection.Width := Width;
  TabSection.Bottom := Height - TopSectionHeight - TabSectionHeight - TabCaptionHeight;

  BottomSection.Height := Height - TopSectionHeight - TabSectionHeight - TabCaptionHeight;
  BottomSection.Width := Width;
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
