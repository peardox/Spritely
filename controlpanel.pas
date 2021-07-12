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
  X3DNodes, X3DFields, X3DTIme, CastleNotifications,
  CastleImages, CastleGLImages, CastleRectangles, CastleQuaternions,
  CastleTextureImages, CastleCompositeImage, CastleLog,
  CastleApplicationProperties, CastleTimeUtils, CastleKeysMouse,
  CastleGLUtils, multimodel, staging, MiscFunctions;

type
  { TControlPanel }

  TControlPanel = class(TCastleUserInterface)
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; const AWidth: Single);
    procedure Resize; override;
  protected
    TopSection: TCastleUserInterface;
    BottomSection: TCastleUserInterface;
  end;

  { TSpriteControlPanel }

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

implementation

uses MainGameUnit;

{ TControlPanel }

constructor TControlPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

constructor TControlPanel.Create(AOwner: TComponent; const AWidth: Single);
begin
  Create(AOwner);

  Width := AWidth;
  Height := ParentRect.Height;

  Anchor(hpRight);
  Anchor(vpTop);
  BorderColor := White;
  Border.AllSides :=1;

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

procedure TControlPanel.Resize;
begin
  inherited;

  Height := ParentRect.Height;

  TopSection.Height := 80;
  TopSection.Width := Width;
  TopSection.BorderColor := Red;
  TopSection.Border.AllSides :=1;
  TopSection.Bottom := Height - 80;

  BottomSection.Height := Height - 80;
  BottomSection.Width := Width;
  BottomSection.BorderColor := Green;
  BottomSection.Border.AllSides :=1;

  WriteLnLog('BottomSection.Height = ' + FloatTOStr(BottomSection.Height));
end;

{ TSpriteControlPanel }

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
  ABtn.Image.URL := 'castle-data:/icons/zminus.png';
  ABtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(BBtn, 'Rot Z+', @DoRotateZPlus);
  BBtn.Left := BtnWidth + 20;
  BBtn.Bottom := 10;
  BBtn.AutoSize := False;
  BBtn.Width := BtnWidth;
  BBtn.Height := BtnHeight;
  BBtn.FontScale := BtnFontScale;
  BBtn.Image.URL := 'castle-data:/icons/zplus.png';
  BBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(CBtn, 'Rot Y-', @DoRotateYMinus);
  CBtn.Left := 10;
  CBtn.Bottom := 10 + BtnHeight + 10;
  CBtn.AutoSize := False;
  CBtn.Width := BtnWidth;
  CBtn.Height := BtnHeight;
  CBtn.FontScale := BtnFontScale;
  CBtn.Image.URL := 'castle-data:/icons/yminus.png';
  CBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(DBtn, 'Rot Y+', @DoRotateYPlus);
  DBtn.Left := BtnWidth + 20;
  DBtn.Bottom := 10 + BtnHeight + 10;
  DBtn.AutoSize := False;
  DBtn.Width := BtnWidth;
  DBtn.Height := BtnHeight;
  DBtn.FontScale := BtnFontScale;
  DBtn.Image.URL := 'castle-data:/icons/yplus.png';
  DBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(EBtn, 'Rot X-', @DoRotateXMinus);
  EBtn.Left := 10;
  EBtn.Bottom := 10 + 2 * (BtnHeight + 10);
  EBtn.AutoSize := False;
  EBtn.Width := BtnWidth;
  EBtn.Height := BtnHeight;
  EBtn.FontScale := BtnFontScale;
  EBtn.Image.URL := 'castle-data:/icons/xminus.png';
  EBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(FBtn, 'Rot X+', @DoRotateXPlus);
  FBtn.Left := BtnWidth + 20;
  FBtn.Bottom := 10 + 2 * (BtnHeight + 10);
  FBtn.AutoSize := False;
  FBtn.Width := BtnWidth;
  FBtn.Height := BtnHeight;
  FBtn.FontScale := BtnFontScale;
  FBtn.Image.URL := 'castle-data:/icons/xplus.png';
  FBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(GBtn, 'Zoom Out', @DoZoomOut);
  GBtn.Left := 10;
  GBtn.Bottom := 10 + 3 * (BtnHeight + 10);
  GBtn.AutoSize := False;
  GBtn.Width := BtnWidth;
  GBtn.Height := BtnHeight;
  GBtn.FontScale := BtnFontScale;
  GBtn.Image.URL := 'castle-data:/icons/zoomout.png';
  GBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(HBtn, 'Zoom In', @DoZoomIn);
  HBtn.Left := BtnWidth + 20;
  HBtn.Bottom := 10 + 3 * (BtnHeight + 10);
  HBtn.AutoSize := False;
  HBtn.Width := BtnWidth;
  HBtn.Height := BtnHeight;
  HBtn.FontScale := BtnFontScale;
  HBtn.Image.URL := 'castle-data:/icons/zoomin.png';
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

