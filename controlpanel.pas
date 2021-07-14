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
  CastleGLUtils, multimodel, staging, MiscFunctions, SpriteControls;

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
    IBtn: TCastleButton;
    JBtn: TCastleButton;
    KBtn: TCastleButton;
    LBtn: TCastleButton;

    AChk: TCastleCheckbox;
    BChk: TCastleCheckbox;

    CtlDirections: TCastleIntegerSpinEdit;
    CtlFrames: TCastleIntegerSpinEdit;
    CtlSpriteHeight: TCastleIntegerSpinEdit;
    CtlSpriteWidth: TCastleIntegerSpinEdit;

    procedure UseModelSpotsClick(Sender: TObject);
    procedure UseTransparencyChange(Sender: TObject);
    procedure UpdateView;
    procedure DoRotateXPlus(Sender: TObject);
    procedure DoRotateXMinus(Sender: TObject);
    procedure DoRotateYPlus(Sender: TObject);
    procedure DoRotateYMinus(Sender: TObject);
    procedure DoRotateZPlus(Sender: TObject);
    procedure DoRotateZMinus(Sender: TObject);
    procedure DoZoomIn(Sender: TObject);
    procedure DoZoomOut(Sender: TObject);
    procedure DoSpriteHeightChange(Sender: TObject);
    procedure DoSpriteWidthChange(Sender: TObject);
    procedure DoDirectionsChange(Sender: TObject);
    procedure DoFramesChange(Sender: TObject);
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
  BtnMargin: Single;
  BtnRow: Cardinal;
begin
  WriteLnLog('Start LoadOrentationLayout');

  BtnRow := 0;
  BtnMargin := 10;
  BtnWidth := (Width - (3 * BtnMargin)) / 2;
  {$if defined(ANDROID)}
  BtnHeight := 60;
  {$elseif defined(CASTLE_IOS)}
  BtnHeight := 60;
  {$else}
  BtnHeight := 30;
  {$end}
  BtnFontScale := 0.8;
  BtnImageScale := (BtnHeight / 512) * BtnFontScale;

  BottomSection.CreateButton(ABtn, 'Rot Z-', @DoRotateZMinus);
  ABtn.Left := BtnMargin;
  ABtn.Bottom := BtnRow + BtnMargin;
  ABtn.AutoSize := False;
  ABtn.Width := BtnWidth;
  ABtn.Height := BtnHeight;
  ABtn.FontScale := BtnFontScale;
  ABtn.Image.URL := 'castle-data:/icons/zminus.png';
  ABtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(BBtn, 'Rot Z+', @DoRotateZPlus);
  BBtn.Left := BtnWidth + (2 * BtnMargin);
  BBtn.Bottom := BtnRow + BtnMargin;
  BBtn.AutoSize := False;
  BBtn.Width := BtnWidth;
  BBtn.Height := BtnHeight;
  BBtn.FontScale := BtnFontScale;
  BBtn.Image.URL := 'castle-data:/icons/zplus.png';
  BBtn.ImageScale := BtnImageScale;

  Inc(BtnRow);

  BottomSection.CreateButton(CBtn, 'Rot Y-', @DoRotateYMinus);
  CBtn.Left := BtnMargin;
  CBtn.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  CBtn.AutoSize := False;
  CBtn.Width := BtnWidth;
  CBtn.Height := BtnHeight;
  CBtn.FontScale := BtnFontScale;
  CBtn.Image.URL := 'castle-data:/icons/yminus.png';
  CBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(DBtn, 'Rot Y+', @DoRotateYPlus);
  DBtn.Left := BtnWidth + 20;
  DBtn.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  DBtn.AutoSize := False;
  DBtn.Width := BtnWidth;
  DBtn.Height := BtnHeight;
  DBtn.FontScale := BtnFontScale;
  DBtn.Image.URL := 'castle-data:/icons/yplus.png';
  DBtn.ImageScale := BtnImageScale;

  Inc(BtnRow);

  BottomSection.CreateButton(EBtn, 'Rot X-', @DoRotateXMinus);
  EBtn.Left := BtnMargin;
  EBtn.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  EBtn.AutoSize := False;
  EBtn.Width := BtnWidth;
  EBtn.Height := BtnHeight;
  EBtn.FontScale := BtnFontScale;
  EBtn.Image.URL := 'castle-data:/icons/xminus.png';
  EBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(FBtn, 'Rot X+', @DoRotateXPlus);
  FBtn.Left := BtnWidth + 20;
  FBtn.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  FBtn.AutoSize := False;
  FBtn.Width := BtnWidth;
  FBtn.Height := BtnHeight;
  FBtn.FontScale := BtnFontScale;
  FBtn.Image.URL := 'castle-data:/icons/xplus.png';
  FBtn.ImageScale := BtnImageScale;

  Inc(BtnRow);

  BottomSection.CreateButton(GBtn, 'Zoom Out', @DoZoomOut);
  GBtn.Left := BtnMargin;
  GBtn.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  GBtn.AutoSize := False;
  GBtn.Width := BtnWidth;
  GBtn.Height := BtnHeight;
  GBtn.FontScale := BtnFontScale;
  GBtn.Image.URL := 'castle-data:/icons/zoomout.png';
  GBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(HBtn, 'Zoom In', @DoZoomIn);
  HBtn.Left := BtnWidth + 20;
  HBtn.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  HBtn.AutoSize := False;
  HBtn.Width := BtnWidth;
  HBtn.Height := BtnHeight;
  HBtn.FontScale := BtnFontScale;
  HBtn.Image.URL := 'castle-data:/icons/zoomin.png';
  HBtn.ImageScale := BtnImageScale;

  Inc(BtnRow);

  BottomSection.CreateButton(IBtn, 'Move Up', @DoZoomOut);
  IBtn.Left := BtnMargin;
  IBtn.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  IBtn.AutoSize := False;
  IBtn.Width := BtnWidth;
  IBtn.Height := BtnHeight;
  IBtn.FontScale := BtnFontScale;
  IBtn.Image.URL := 'castle-data:/icons/up.png';
  IBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(JBtn, 'Move Down', @DoZoomIn);
  JBtn.Left := BtnWidth + 20;
  JBtn.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  JBtn.AutoSize := False;
  JBtn.Width := BtnWidth;
  JBtn.Height := BtnHeight;
  JBtn.FontScale := BtnFontScale;
  JBtn.Image.URL := 'castle-data:/icons/down.png';
  JBtn.ImageScale := BtnImageScale;

  Inc(BtnRow);

  BottomSection.CreateButton(KBtn, 'Move Left', @DoZoomOut);
  KBtn.Left := BtnMargin;
  KBtn.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  KBtn.AutoSize := False;
  KBtn.Width := BtnWidth;
  KBtn.Height := BtnHeight;
  KBtn.FontScale := BtnFontScale;
  KBtn.Image.URL := 'castle-data:/icons/left.png';
  KBtn.ImageScale := BtnImageScale;

  BottomSection.CreateButton(LBtn, 'Move Right', @DoZoomIn);
  LBtn.Left := BtnWidth + 20;
  LBtn.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  LBtn.AutoSize := False;
  LBtn.Width := BtnWidth;
  LBtn.Height := BtnHeight;
  LBtn.FontScale := BtnFontScale;
  LBtn.Image.URL := 'castle-data:/icons/right.png';
  LBtn.ImageScale := BtnImageScale;

  Inc(BtnRow);

  BottomSection.CreateCheckbox(AChk, 'Transparent', @UseTransparencyChange);
  AChk.Left := BtnMargin;
  AChk.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  AChk.AutoSize := False;
  AChk.Width := BtnWidth;
  AChk.Height := BtnHeight;
  AChk.CheckboxColor := White;
  AChk.TextColor := White;

  Inc(BtnRow);

  BottomSection.CreateCheckbox(BChk, 'Local Lights', @UseModelSpotsClick);
  BChk.Left := BtnMargin;
  BChk.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  BChk.AutoSize := False;
  BChk.Width := BtnWidth;
  BChk.Height := BtnHeight;
  BChk.CheckboxColor := White;
  BChk.TextColor := White;

  Inc(BtnRow);

  CtlDirections := TCastleIntegerSpinEdit.Create(BottomSection, 'Directions', (BtnWidth * 2) + 10, BtnHeight);
  CtlDirections.Left := BtnMargin;
  CtlDirections.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  CtlDirections.Width := BtnWidth;
  CtlDirections.Height := BtnHeight;

  Inc(BtnRow);

  CtlFrames := TCastleIntegerSpinEdit.Create(BottomSection, 'Frames', (BtnWidth * 2) + 10, BtnHeight);
  CtlFrames.Left := BtnMargin;
  CtlFrames.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  CtlFrames.Width := BtnWidth;
  CtlFrames.Height := BtnHeight;

  Inc(BtnRow);

  CtlSpriteHeight := TCastleIntegerSpinEdit.Create(BottomSection, 'Height', (BtnWidth * 2) + 10, BtnHeight);
  CtlSpriteHeight.Left := BtnMargin;
  CtlSpriteHeight.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  CtlSpriteHeight.Width := BtnWidth;
  CtlSpriteHeight.Height := BtnHeight;

  Inc(BtnRow);

  CtlSpriteWidth := TCastleIntegerSpinEdit.Create(BottomSection, 'Width', (BtnWidth * 2) + 10, BtnHeight);
  CtlSpriteWidth.Left := BtnMargin;
  CtlSpriteWidth.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
  CtlSpriteWidth.Width := BtnWidth;
  CtlSpriteWidth.Height := BtnHeight;

  Inc(BtnRow);

  with Parent as TCastleApp do
    begin
      WriteLnLog('Setting CP Defaults');
      AChk.Checked := UseTransparency;
      BChk.Checked := UseModelSpots;
      CtlDirections.Min := 1;
      CtlDirections.Max := 64;
      CtlDirections.Value := DirectionCount;
      CtlDirections.OnChange := @DoDirectionsChange;
      CtlFrames.Min := 1;
      CtlFrames.Max := 64;
      CtlFrames.Value := FrameCount;
      CtlFrames.OnChange := @DoFramesChange;
      CtlSpriteHeight.Min := 16;
      CtlSpriteHeight.Max := 2048;
      CtlSpriteHeight.Value := SpriteHeight;
      CtlSpriteHeight.StepSize := 16;
      CtlSpriteHeight.OnChange := @DoSpriteHeightChange;
      CtlSpriteWidth.Min := 16;
      CtlSpriteWidth.Max := 2048;
      CtlSpriteWidth.Value := SpriteWidth;
      CtlSpriteWidth.StepSize := 16;
      CtlSpriteWidth.OnChange := @DoSpriteWidthChange;
    end;
end;

procedure TSpriteControlPanel.DoZoomIn(Sender: TObject);
begin
  with Parent as TCastleApp do
    iScaleMultiplier := iScaleMultiplier + (iScaleMultiplier * 0.1);
end;

procedure TSpriteControlPanel.DoZoomOut(Sender: TObject);
begin
  with Parent as TCastleApp do
    iScaleMultiplier := iScaleMultiplier - (iScaleMultiplier * 0.1);
end;

procedure TSpriteControlPanel.UpdateView;
var
  Q: TQuaternion;
begin
  with Parent as TCastleApp do
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
  with Parent as TCastleApp do
    WorkingModel.BaseRotation.X := WorkingModel.BaseRotation.X + ((2 * Pi) / DirectionCount);
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateXMinus(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.BaseRotation.X := WorkingModel.BaseRotation.X - ((2 * Pi) / DirectionCount);
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateYPlus(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.BaseRotation.Y := WorkingModel.BaseRotation.Y + ((2 * Pi) / DirectionCount);
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateYMinus(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.BaseRotation.Y := WorkingModel.BaseRotation.Y - ((2 * Pi) / DirectionCount);
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateZPlus(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.BaseRotation.Z := WorkingModel.BaseRotation.Z + ((2 * Pi) / DirectionCount);
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateZMinus(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.BaseRotation.Z := WorkingModel.BaseRotation.Z - ((2 * Pi) / DirectionCount);
  UpdateView;
end;

procedure TSpriteControlPanel.UseModelSpotsClick(Sender: TObject);
var
  i: Integer;
begin
  with Parent as TCastleApp do
    begin
      UseModelSpots := BChk.Checked;

      WorkingModel.BaseRotation.X := WorkingModel.BaseRotation.X + ((2 * Pi) / DirectionCount);
      if not(WorkingModel = nil) then
        begin
          if UseModelSpots then
            begin
              UseModelSpots := False;
              for i := 0 to High(WorkingModel.SpotNode) do
                begin
                WorkingModel.SpotNode[i].IsOn := True;
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

procedure TSpriteControlPanel.DoSpriteWidthChange(Sender: TObject);
begin
  with Parent as TCastleApp do
    begin
      SpriteWidth := CtlSpriteWidth.Value;
      Resize;
    end;
end;

procedure TSpriteControlPanel.DoSpriteHeightChange(Sender: TObject);
begin
  with Parent as TCastleApp do
    begin
      SpriteHeight := CtlSpriteHeight.Value;
      Resize;
    end;
end;

procedure TSpriteControlPanel.DoDirectionsChange(Sender: TObject);
begin
  with Parent as TCastleApp do
    DirectionCount := CtlDirections.Value;
end;

procedure TSpriteControlPanel.DoFramesChange(Sender: TObject);
begin
  with Parent as TCastleApp do
    FrameCount := CtlFrames.Value;
end;

procedure TSpriteControlPanel.UseTransparencyChange(Sender: TObject);
begin
  with Parent as TCastleApp do
    begin
      UseTransparency := AChk.Checked;

      if UseTransparency then
        Stage.ShowGround(False)
      else
        Stage.ShowGround(True);
    end;
end;


end.

