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
    CtlRotZMinusBtn: TCastleButton;
    CtlRotZPlusBtn: TCastleButton;
    CtlRotYMinusBtn: TCastleButton;
    CtlRotYPlusBtn: TCastleButton;
    CtlRotXMinusBtn: TCastleButton;
    CtlRotXPlusBtn: TCastleButton;
    CtlZoomOutBtn: TCastleButton;
    CtlZoomInBtn: TCastleButton;
    CtlMoveUpBtn: TCastleButton;
    CtlMoveDownBtn: TCastleButton;
    CtlMoveRightBtn: TCastleButton;
    CtlMoveLeftBtn: TCastleButton;
    CtlCameraLeftBtn: TCastleButton;
    CtlCameraRightBtn: TCastleButton;
    CtlMoveBackBtn: TCastleButton;
    CtlMoveFwdBtn: TCastleButton;

    CtlTransparencyChk: TCastleCheckbox;
    CtlLocalLightsChk: TCastleCheckbox;

    CtlDirectionsISE: TCastleIntegerSpinEdit;
    CtlFramesISE: TCastleIntegerSpinEdit;
    CtlSpriteHeightISE: TCastleIntegerSpinEdit;
    CtlSpriteWidthISE: TCastleIntegerSpinEdit;

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
    procedure DoMoveUp(Sender: TObject);
    procedure DoMoveDown(Sender: TObject);
    procedure DoMoveRight(Sender: TObject);
    procedure DoMoveLeft(Sender: TObject);
    procedure DoMoveFwd(Sender: TObject);
    procedure DoMoveBack(Sender: TObject);
    procedure DoCamLeft(Sender: TObject);
    procedure DoCamRight(Sender: TObject);
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

  procedure PlaceButton(var obj: TCastleButton; const BtnCol: Integer);
  begin
    obj.Left := (BtnCol * (BtnWidth  + BtnMargin)) + BtnMargin;
    obj.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
    obj.Width := BtnWidth;
    obj.Height := BtnHeight;
    obj.FontScale := BtnFontScale;
    obj.ImageScale := BtnImageScale;
  end;

  procedure PlaceCheckbox(var obj: TCastleCheckbox);
  begin
    obj.Left := BtnMargin;
    obj.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
    obj.Width := BtnWidth;
    obj.Height := BtnHeight;
    obj.CheckboxColor := White;
    obj.TextColor := White;
  end;

  procedure PlaceISE(var obj: TCastleIntegerSpinEdit);
  begin
    obj.Left := BtnMargin;
    obj.Bottom := (BtnRow * (BtnHeight + BtnMargin)) + BtnMargin;
    obj.Width := BtnWidth;
    obj.Height := BtnHeight;
  end;

begin
  WriteLnLog('Start LoadOrentationLayout');

  BtnRow := 0;
  BtnMargin := 10;
  BtnWidth := (Width - (3 * BtnMargin)) / 2;
  {$if defined(ANDROID)}
  BtnHeight := 90;
  BtnFontScale := 0.8;
  {$elseif defined(CASTLE_IOS)}
  BtnHeight := 90;
  BtnFontScale := 0.8;
  {$else}
  BtnHeight := 30;
  BtnFontScale := 0.8;
  {$endif}
  BtnImageScale := (BtnHeight / 512) * BtnFontScale;

  BottomSection.CreateButton(CtlRotZMinusBtn, 'Rot Z-', @DoRotateZMinus, 'castle-data:/icons/zminus.png');
  PlaceButton(CtlRotZMinusBtn, 0);
  BottomSection.CreateButton(CtlRotZPlusBtn, 'Rot Z+', @DoRotateZPlus, 'castle-data:/icons/zplus.png');
  PlaceButton(CtlRotZPlusBtn, 1);
  Inc(BtnRow);

  BottomSection.CreateButton(CtlRotYMinusBtn, 'Rot Y-', @DoRotateYMinus, 'castle-data:/icons/yminus.png');
  PlaceButton(CtlRotYMinusBtn, 0);
  BottomSection.CreateButton(CtlRotYPlusBtn, 'Rot Y+', @DoRotateYPlus, 'castle-data:/icons/yplus.png');
  PlaceButton(CtlRotYPlusBtn, 1);
  Inc(BtnRow);

  BottomSection.CreateButton(CtlRotXMinusBtn, 'Rot X-', @DoRotateXMinus, 'castle-data:/icons/xminus.png');
  PlaceButton(CtlRotXMinusBtn, 0);
  BottomSection.CreateButton(CtlRotXPlusBtn, 'Rot X+', @DoRotateXPlus, 'castle-data:/icons/xplus.png');
  PlaceButton(CtlRotXPlusBtn, 1);
  Inc(BtnRow);

  BottomSection.CreateButton(CtlZoomOutBtn, 'Zoom Out', @DoZoomOut, 'castle-data:/icons/zoomout.png');
  PlaceButton(CtlZoomOutBtn, 0);
  BottomSection.CreateButton(CtlZoomInBtn, 'Zoom In', @DoZoomIn, 'castle-data:/icons/zoomin.png');
  PlaceButton(CtlZoomInBtn, 1);
  Inc(BtnRow);

  BottomSection.CreateButton(CtlMoveUpBtn, 'Move Up', @DoMoveUp, 'castle-data:/icons/up.png');
  PlaceButton(CtlMoveUpBtn, 0);
  BottomSection.CreateButton(CtlMoveDownBtn, 'Move Down', @DoMoveDown, 'castle-data:/icons/down.png');
  PlaceButton(CtlMoveDownBtn, 1);
  Inc(BtnRow);

  BottomSection.CreateButton(CtlMoveLeftBtn, 'Move Left', @DoMoveLeft, 'castle-data:/icons/left.png');
  PlaceButton(CtlMoveLeftBtn, 0);
  BottomSection.CreateButton(CtlMoveRightBtn, 'Move Right', @DoMoveRight, 'castle-data:/icons/right.png');
  PlaceButton(CtlMoveRightBtn, 1);
  Inc(BtnRow);

  BottomSection.CreateButton(CtlMoveFwdBtn, 'Move Fwd', @DoMoveFwd, 'castle-data:/icons/forward.png');
  PlaceButton(CtlMoveFwdBtn, 0);
  BottomSection.CreateButton(CtlMoveBackBtn, 'Move Back', @DoMoveBack, 'castle-data:/icons/back.png');
  PlaceButton(CtlMoveBackBtn, 1);
  Inc(BtnRow);

  BottomSection.CreateButton(CtlCameraLeftBtn, 'Cam Rot-', @DoCamLeft, 'castle-data:/icons/camera.png');
  PlaceButton(CtlCameraLeftBtn, 0);
  BottomSection.CreateButton(CtlCameraRightBtn, 'Cam Rot+', @DoCamRight, 'castle-data:/icons/camera.png');
  PlaceButton(CtlCameraRightBtn, 1);
  Inc(BtnRow);

  BottomSection.CreateCheckbox(CtlTransparencyChk, 'Transparent', @UseTransparencyChange);
  PlaceCheckbox(CtlTransparencyChk);
  Inc(BtnRow);

  BottomSection.CreateCheckbox(CtlLocalLightsChk, 'Local Lights', @UseModelSpotsClick);
  PlaceCheckbox(CtlLocalLightsChk);
  Inc(BtnRow);

  CtlDirectionsISE := TCastleIntegerSpinEdit.Create(BottomSection, 'Directions', (BtnWidth * 2) + 10, BtnHeight);
  PlaceISE(CtlDirectionsISE);
  Inc(BtnRow);

  CtlFramesISE := TCastleIntegerSpinEdit.Create(BottomSection, 'Frames', (BtnWidth * 2) + 10, BtnHeight);
  PlaceISE(CtlFramesISE);
  Inc(BtnRow);

  CtlSpriteHeightISE := TCastleIntegerSpinEdit.Create(BottomSection, 'Height', (BtnWidth * 2) + 10, BtnHeight);
  PlaceISE(CtlSpriteHeightISE);
  Inc(BtnRow);

  CtlSpriteWidthISE := TCastleIntegerSpinEdit.Create(BottomSection, 'Width', (BtnWidth * 2) + 10, BtnHeight);
  PlaceISE(CtlSpriteWidthISE);
  Inc(BtnRow);

  with Parent as TCastleApp do
    begin
      WriteLnLog('Setting CP Defaults');
      CtlTransparencyChk.Checked := UseTransparency;
      CtlLocalLightsChk.Checked := UseModelSpots;
      CtlDirectionsISE.Min := 1;
      CtlDirectionsISE.Max := 64;
      CtlDirectionsISE.Value := DirectionCount;
      CtlDirectionsISE.OnChange := @DoDirectionsChange;
      CtlFramesISE.Min := 1;
      CtlFramesISE.Max := 64;
      CtlFramesISE.Value := FrameCount;
      CtlFramesISE.OnChange := @DoFramesChange;
      CtlSpriteHeightISE.Min := 16;
      CtlSpriteHeightISE.Max := 2048;
      CtlSpriteHeightISE.Value := SpriteHeight;
      CtlSpriteHeightISE.StepSize := 16;
      CtlSpriteHeightISE.OnChange := @DoSpriteHeightChange;
      CtlSpriteWidthISE.Min := 16;
      CtlSpriteWidthISE.Max := 2048;
      CtlSpriteWidthISE.Value := SpriteWidth;
      CtlSpriteWidthISE.StepSize := 16;
      CtlSpriteWidthISE.OnChange := @DoSpriteWidthChange;
    end;
end;

procedure TSpriteControlPanel.DoMoveUp(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.Scene.Translation.Y := WorkingModel.Scene.Translation.Y + 0.1;
end;

procedure TSpriteControlPanel.DoMoveDown(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.Scene.Translation.Y := WorkingModel.Scene.Translation.Y - 0.1;
end;

procedure TSpriteControlPanel.DoMoveRight(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.Scene.Translation.X := WorkingModel.Scene.Translation.X + 0.1;
end;

procedure TSpriteControlPanel.DoMoveLeft(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.Scene.Translation.X := WorkingModel.Scene.Translation.X - 0.1;
end;

procedure TSpriteControlPanel.DoMoveFwd(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.Scene.Translation.Z := WorkingModel.Scene.Translation.Z + 0.1;
end;

procedure TSpriteControlPanel.DoMoveBack(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.Scene.Translation.Z := WorkingModel.Scene.Translation.Z - 0.1;
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

procedure TSpriteControlPanel.DoCamLeft(Sender: TObject);
begin
  with Parent as TCastleApp do
    CameraRotation := CameraRotation - (Pi / 8);
end;

procedure TSpriteControlPanel.DoCamRight(Sender: TObject);
begin
  with Parent as TCastleApp do
    CameraRotation := CameraRotation + (Pi / 8);
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
    WorkingModel.BaseRotation.X := WrapRadians(WorkingModel.BaseRotation.X + ((2 * Pi) / DirectionCount));
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateXMinus(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.BaseRotation.X := WrapRadians(WorkingModel.BaseRotation.X - ((2 * Pi) / DirectionCount));
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateYPlus(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.BaseRotation.Y := WrapRadians(WorkingModel.BaseRotation.Y + ((2 * Pi) / DirectionCount));
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateYMinus(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.BaseRotation.Y := WrapRadians(WorkingModel.BaseRotation.Y - ((2 * Pi) / DirectionCount));
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateZPlus(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.BaseRotation.Z := WrapRadians(WorkingModel.BaseRotation.Z + ((2 * Pi) / DirectionCount));
  UpdateView;
end;

procedure TSpriteControlPanel.DoRotateZMinus(Sender: TObject);
begin
  with Parent as TCastleApp do
    WorkingModel.BaseRotation.Z := WrapRadians(WorkingModel.BaseRotation.Z - ((2 * Pi) / DirectionCount));
  UpdateView;
end;

procedure TSpriteControlPanel.UseModelSpotsClick(Sender: TObject);
var
  i: Integer;
begin
  with Parent as TCastleApp do
    begin
      UseModelSpots := CtlLocalLightsChk.Checked;

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
      SpriteWidth := CtlSpriteWidthISE.Value;
      Resize;
    end;
end;

procedure TSpriteControlPanel.DoSpriteHeightChange(Sender: TObject);
begin
  with Parent as TCastleApp do
    begin
      SpriteHeight := CtlSpriteHeightISE.Value;
      Resize;
    end;
end;

procedure TSpriteControlPanel.DoDirectionsChange(Sender: TObject);
begin
  with Parent as TCastleApp do
    DirectionCount := CtlDirectionsISE.Value;
end;

procedure TSpriteControlPanel.DoFramesChange(Sender: TObject);
begin
  with Parent as TCastleApp do
    FrameCount := CtlFramesISE.Value;
end;

procedure TSpriteControlPanel.UseTransparencyChange(Sender: TObject);
begin
  with Parent as TCastleApp do
    begin
      UseTransparency := CtlTransparencyChk.Checked;

      if UseTransparency then
        Stage.ShowGround(False)
      else
        Stage.ShowGround(True);
    end;
end;


end.

