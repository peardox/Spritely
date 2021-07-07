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
  X3DNodes, X3DFields, X3DTIme, CastleNotifications,
  CastleImages, CastleGLImages, CastleRectangles, CastleQuaternions,
  CastleTextureImages, CastleCompositeImage, CastleLog,
  CastleApplicationProperties, CastleTimeUtils, CastleKeysMouse,
  CastleGLUtils, multimodel, staging, MiscFunctions;

type
  TControlPanel = class(TCastleUserInterface)
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; const AWidth: Single; const AHeight: Single);
    procedure Resize; override;
  protected
    TopSection: TCastleUserInterface;
    BottomSection: TCastleUserInterface;
  end;

  TSpriteControlPanel = class(TControlPanel)
  private
    ABtn: TCastleButton;
    BBtn: TCastleButton;
    CBtn: TCastleButton;
    DBtn: TCastleButton;
    EBtn: TCastleButton;
    FBtn: TCastleButton;
    GBtn: TCastleButton;
    HBtn: TCastleButton;
    UseModelSpots: Boolean;
    procedure UseModelSpotsClick(Sender: TObject);
    procedure UpdateView;
    procedure DoRotateXPlus(Sender: TObject);
    procedure DoRotateXMinus(Sender: TObject);
    procedure DoRotateYPlus(Sender: TObject);
    procedure DoRotateYMinus(Sender: TObject);
    procedure DoRotateZPlus(Sender: TObject);
    procedure DoRotateZMinus(Sender: TObject);
    procedure DoZoomIn(Sender: TObject);
    procedure DoZoomOut(Sender: TObject);
  public
    procedure LoadOrentationLayout;
  end;

  { TCastleOverlay }

  TCastleOverlay = class(TUIState)
    constructor Create(AOwner: TComponent); override;
    procedure BeforeRender; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
    procedure Resize; override; // TCastleUserInterface
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override; // TUIState
  private
    stateNotifications: TCastleNotifications;
    Overlay: TCastleRectangleControl;
    Screen: TCastleRectangleControl;
  public
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure AddNote(const AMsg: String);
  end;

implementation

uses MainGameUnit;

constructor TCastleOverlay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FullSize := True;

  Overlay := TCastleRectangleControl.Create(AOwner);
  Overlay.FullSize := True;
  Overlay.Color := Vector4(0, 0, 0, 0.25);

  InsertFront(Overlay);

  Screen := TCastleRectangleControl.Create(Overlay);
  Screen.HorizontalAnchorParent := hpMiddle;
  Screen.HorizontalAnchorSelf := hpMiddle;
  Screen.VerticalAnchorParent := vpMiddle;
  Screen.VerticalAnchorSelf := vpMiddle;
  Screen.Width := Trunc(StateContainer.Width * 0.75);
  Screen.Height := Trunc(StateContainer.Height * 0.75);
  Screen.Color := HexToColor('C8C8C8');

  Overlay.InsertFront(Screen);

  stateNotifications := TCastleNotifications.Create(Screen);
  stateNotifications.MaxMessages := 12;
  stateNotifications.Anchor(hpLeft, 10);
  stateNotifications.Anchor(vpBottom, 10);

  Screen.InsertFront(stateNotifications);
end;

procedure TCastleOverlay.AddNote(const AMsg: String);
begin
  stateNotifications.Show(AMsg);
end;

procedure TCastleOverlay.Start;
begin
  inherited;
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

procedure TControlPanel.Resize;
begin
  inherited;

  TopSection.Height := 80;
  TopSection.Width := Screen.Width;

  BottomSection.Height := Screen.Height - 80;
  BottomSection.Width := Screen.Width;

  WriteLnLog('BottomSection.Height = ' + FloatTOStr(BottomSection.Height));
end;

procedure TCastleOverlay.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
end;

constructor TControlPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{
  AutoSizeToChildren := False;
  AutoSizeWidth := False;
  AutoSizeHeight := False;
}
end;

constructor TControlPanel.Create(AOwner: TComponent; const AWidth: Single; const AHeight: Single);
begin
  Create(AOwner);

  Width := AWidth;
  Height := AHeight;
  Anchor(hpRight);
  Anchor(vpTop);
  BorderColor := White;
  TUIState(AOwner).InsertFront(Self);

  TopSection := TCastleUserInterface.Create(Self);
  TopSection.Height := 80;
  TopSection.Width := Width;
  Self.InsertFront(TopSection);

  BottomSection := TCastleUserInterface.Create(Self);
  BottomSection.Height := Height - 80;
  BottomSection.Width := Width;
  Self.InsertFront(BottomSection);
end;

procedure TSpriteControlPanel.LoadOrentationLayout;
var
  BtnWidth: Single;
  BtnHeight: Single;
  BtnFontScale: Single;
  BtnImageScale: Single;
begin
  BtnWidth := (Width - 30) / 2;
  BtnHeight := BtnWidth * 1 / 3;
  BtnFontScale := 0.75;
  BtnImageScale := 0.075;

  BottomSection.CreateButton(ABtn, 'Rot Z-', @DoRotateZMinus);
  ABtn.Left := 10;
  ABtn.Bottom := 10;
  ABtn.AutoSize := False;
  ABtn.Width := BtnWidth;
  ABtn.Height := BtnHeight;
  ABtn.FontScale := BtnFontScale;
  ABtn.Image.URL := 'castle-data:/icons/rotate/grab_0005.png';
  ABtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(BBtn, 'Rot Z+', @DoRotateZPlus);
  BBtn.Left := BtnWidth + 20;
  BBtn.Bottom := 10;
  BBtn.AutoSize := False;
  BBtn.Width := BtnWidth;
  BBtn.Height := BtnHeight;
  BBtn.FontScale := BtnFontScale;
  BBtn.Image.URL := 'castle-data:/icons/rotate/grab_0004.png';
  BBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(CBtn, 'Rot Y-', @DoRotateYMinus);
  CBtn.Left := 10;
  CBtn.Bottom := 10 + BtnHeight + 10;
  CBtn.AutoSize := False;
  CBtn.Width := BtnWidth;
  CBtn.Height := BtnHeight;
  CBtn.FontScale := BtnFontScale;
  CBtn.Image.URL := 'castle-data:/icons/rotate/grab_0000.png';
  CBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(DBtn, 'Rot Y+', @DoRotateYPlus);
  DBtn.Left := BtnWidth + 20;
  DBtn.Bottom := 10 + BtnHeight + 10;
  DBtn.AutoSize := False;
  DBtn.Width := BtnWidth;
  DBtn.Height := BtnHeight;
  DBtn.FontScale := BtnFontScale;
  DBtn.Image.URL := 'castle-data:/icons/rotate/grab_0001.png';
  DBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(EBtn, 'Rot X-', @DoRotateXMinus);
  EBtn.Left := 10;
  EBtn.Bottom := 10 + 2 * (BtnHeight + 10);
  EBtn.AutoSize := False;
  EBtn.Width := BtnWidth;
  EBtn.Height := BtnHeight;
  EBtn.FontScale := BtnFontScale;
  EBtn.Image.URL := 'castle-data:/icons/rotate/grab_0003.png';
  EBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(FBtn, 'Rot X+', @DoRotateXPlus);
  FBtn.Left := BtnWidth + 20;
  FBtn.Bottom := 10 + 2 * (BtnHeight + 10);
  FBtn.AutoSize := False;
  FBtn.Width := BtnWidth;
  FBtn.Height := BtnHeight;
  FBtn.FontScale := BtnFontScale;
  FBtn.Image.URL := 'castle-data:/icons/rotate/grab_0002.png';
  FBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(GBtn, 'Zoom Out', @DoZoomOut);
  GBtn.Left := 10;
  GBtn.Bottom := 10 + 3 * (BtnHeight + 10);
  GBtn.AutoSize := False;
  GBtn.Width := BtnWidth;
  GBtn.Height := BtnHeight;
  GBtn.FontScale := BtnFontScale;
  GBtn.Image.URL := 'castle-data:/icons/rotate/grab_0006.png';
  GBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(HBtn, 'Zoom In', @DoZoomIn);
  HBtn.Left := BtnWidth + 20;
  HBtn.Bottom := 10 + 3 * (BtnHeight + 10);
  HBtn.AutoSize := False;
  HBtn.Width := BtnWidth;
  HBtn.Height := BtnHeight;
  HBtn.FontScale := BtnFontScale;
  HBtn.Image.URL := 'castle-data:/icons/rotate/grab_0007.png';
  HBtn.ImageScale := BtnImageScale;
end;

procedure TSpriteControlPanel.DoZoomIn(Sender: TObject);
begin
  CastleApp.iScaleMultiplier := CastleApp.iScaleMultiplier + (CastleApp.iScaleMultiplier * 0.1);
end;

procedure TSpriteControlPanel.DoZoomOut(Sender: TObject);
begin
  CastleApp.iScaleMultiplier := CastleApp.iScaleMultiplier - (CastleApp.iScaleMultiplier * 0.1);
end;

procedure TSpriteControlPanel.UpdateView;
var
  Q: TQuaternion;
begin
  With CastleApp do
    begin
    Q := QuatFromAxisAngle(Vector4(0, 0, 0, 0));
    Q := Q * QuatFromAxisAngle(Vector4(1, 0, 0, WorkingModel.BaseRotation.X));
    Q := Q * QuatFromAxisAngle(Vector4(0, 1, 0, WorkingModel.BaseRotation.Y));
    Q := Q * QuatFromAxisAngle(Vector4(0, 0, 1, WorkingModel.BaseRotation.Z));

    WorkingModel.Transform.Rotation := Q.ToAxisAngle;
    end;
end;

procedure TSpriteControlPanel.DoRotateXPlus(Sender: TObject);
begin
  With CastleApp do
    WorkingModel.BaseRotation.X := WorkingModel.BaseRotation.X + ((2 * Pi) / DirectionCount);
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateXMinus(Sender: TObject);
begin
  With CastleApp do
    WorkingModel.BaseRotation.X := WorkingModel.BaseRotation.X - ((2 * Pi) / DirectionCount);
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateYPlus(Sender: TObject);
begin
  With CastleApp do
    WorkingModel.BaseRotation.Y := WorkingModel.BaseRotation.Y + ((2 * Pi) / DirectionCount);
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateYMinus(Sender: TObject);
begin
  With CastleApp do
    WorkingModel.BaseRotation.Y := WorkingModel.BaseRotation.Y - ((2 * Pi) / DirectionCount);
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateZPlus(Sender: TObject);
begin
  With CastleApp do
    WorkingModel.BaseRotation.Z := WorkingModel.BaseRotation.Z + ((2 * Pi) / DirectionCount);
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateZMinus(Sender: TObject);
begin
  With CastleApp do
    WorkingModel.BaseRotation.Z := WorkingModel.BaseRotation.Z - ((2 * Pi) / DirectionCount);
  UpdateView;
end;

procedure TSpriteControlPanel.UseModelSpotsClick(Sender: TObject);
var
  i: Integer;
begin
  With CastleApp do
    begin
      WorkingModel.BaseRotation.X := WorkingModel.BaseRotation.X + ((2 * Pi) / DirectionCount);
      if not(WorkingModel = nil) then
        begin
          if UseModelSpots then
            begin
              UseModelSpots := False;
              for i := 0 to High(WorkingModel.SpotNode) do
                begin
                WorkingModel.SpotNode[i].IsOn := True;
//                WorkingModel.SpotNode[i].Color := Vector3(46/255, 19/255, 19/255);
                end;
            end
          else
            begin
              UseModelSpots := True;
              for i := 0 to High(WorkingModel.SpotNode) do
                WorkingModel.SpotNode[i].IsOn := False;
            end;
        end;
    end;
end;

end.

