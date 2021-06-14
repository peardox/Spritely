unit GUIInitialization;

{$mode objfpc}{$H+}
// {$define disableMap}

// FPS = 23.98, 24, 25, 29.97, 30, 50, 59.94, 60, Custom
// DG - 838383, LG B2B2B2
interface

uses
  Classes, SysUtils, Math, CastleUIState, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Menus, Spin, Arrow, CastleControl, MainGameUnit,
  CastleControls, CastleColors, CastleUIControls, CastleTriangles, CastleShapes,
  CastleVectors, CastleSceneCore, CastleScene, CastleTransform, CastleViewport,
  CastleCameras, X3DNodes, X3DFields, X3DTIme, CastleImages, CastleGLImages,
  CastleFilesUtils, CastleURIUtils, MiscFunctions, CastleLCLUtils,
  CastleDialogs, CastleApplicationProperties, CastleLog, CastleTimeUtils,
  CastleKeysMouse, JsonTools, AniTxtJson, AniTakeUtils, Types,
  CastleQuaternions, SpritelyLog, staging, multimodel, ExpandPanels,
  CastleGLShaders, usplashabout, uPoweredby;

type
  { TCastleForm }

  TCastleForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CastleOpenDialog1: TCastleOpenDialog;
    ComboBox1: TComboBox;
    FloatSpinEdit1: TFloatSpinEdit;
    Label1: TLabel;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    AppLog: TMemo;
    MenuItem1: TMenuItem;
    FileOpenMenu: TMenuItem;
    DebugBoxMenu: TMenuItem;
    CreateSpriteMenu: TMenuItem;
    ExitMenu: TMenuItem;
    MyRollOut1: TMyRollOut;
    MyRollOut2: TMyRollOut;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PanelLeft: TPanel;
    Panel3: TPanel;
    PanelLeftBottom: TPanel;
    Panel5: TPanel;
    PanelRight: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SplashAbout1: TSplashAbout;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TrackBar1: TTrackBar;
    TreeView1: TTreeView;
    Window: TCastleControlBase;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure FileOpenMenuClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DebugBoxMenuClick(Sender: TObject);
    procedure CreateSpriteMenuClick(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure WindowClose(Sender: TObject);
    procedure WindowMotion(Sender: TObject; const Event: TInputMotion);
    procedure WindowOpen(Sender: TObject);

    procedure AddInfo(const AName: String; const AValue: Integer);
    procedure AddInfo(const AName: String; const AValue: Single);
    procedure AddInfo(const AName: String; const AValue: String);
    procedure UpdateInfo(const AName: String; const AValue: Integer);
    procedure UpdateInfo(const AName: String; const AValue: Single);
    procedure UpdateInfo(const AName: String; const AValue: String);
    procedure WindowPress(Sender: TObject; const Event: TInputPressRelease);

    procedure MapAnims(const modelNode: TTreeNode; const AnimNode: TAnimationInfo);
    function  Pos2DTo3D(const AXpos: Single; const AYpos: Single): String;
    procedure AddInfoPanel;
    procedure UpdateInfoPanel;
    procedure LoadGuiModel(const AModel: String);
  private
    Tracking: Boolean;
    ModeOrientation: Boolean;
  public
    procedure GuiBootStrap;
  end;

var
  CastleForm: TCastleForm;
  FSPrefix: String;
  ModelFile: String;
  MapFile: String;

const
  InfoFloatFormat: String = '###0.0000';

implementation
{$R *.lfm}

procedure TCastleForm.FormCreate(Sender: TObject);
begin
  {$if defined(windows)}
  FSPrefix := 'C:\';
  {$endif}
  {$if defined(linux)}
  FSPrefix := HomePath;
  {$endif}
  {$if defined(darwin)}
  FSPrefix := HomePath;
  {$endif}

{$ifdef disableMap}
  MapFile := '';
  ModelFile := 'castle-data:/oblique.glb';
//  ModelFile := 'castle-data:/up.glb';
//  ModelFile := 'castle-data:/up131.glb';
//  ModelFile := 'castle-data:/Quaternius/RPGCharacters/Wizard.glb';
//  ModelFile := 'castle-data:/isoroom/scene.gltf';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + 'JoseDiaz' + PathDelim + 'cave' + PathDelim + 'cavewoman.gltf' + PathDelim + 'scene.gltf';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + '3DRT-Medieval-Houses' + PathDelim + 'gltf' + PathDelim + 'house-02-01.glb';
//  ModelFile := FSPrefix  + 'Assets' + PathDelim + 'Sketchfab' + PathDelim + 'crocodile_with_animation' + PathDelim + 'crock-up.glb';
{$else}
  MapFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elf-Males' + PathDelim + 'elfrangers-aniamtions-list.txt';
  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elf-Males' + PathDelim + 'FBX 2013' + PathDelim + 'Elf-03.glb';
//  MapFile := FSPrefix + 'Assets' + PathDelim + 'JoseDiaz' + PathDelim + 'German_Shepherd_new' + PathDelim + 'German Shepherd Animation Ranges.txt';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + 'JoseDiaz' + PathDelim + 'german_shepherd' + PathDelim + 'scene.gltf';
//  MapFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'chibii-racers-dirt-bikes' + PathDelim + '_bike_animations.txt';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'chibii-racers-dirt-bikes' + PathDelim + 'gitf' + PathDelim + 'dirt_bike01.gltf';
//  MapFile := FSPrefix  + 'Assets' + PathDelim + '3drt' + PathDelim + 'gltf' + PathDelim + 'Thief' + PathDelim + 'Thief-animations-list.txt';
//  ModelFile := FSPrefix  + 'Assets' + PathDelim + '3drt' + PathDelim + 'gltf' + PathDelim + 'Thief' + PathDelim + 'thief_torch.glb';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + 'TurboSquid' + PathDelim + 'Wyvern' + PathDelim + 'GreenDragon.glb';
{$endif}
  LogShaders := true;

  LogHandler := TLogHandler.Create(Application);

  ApplicationProperties.ApplicationName := 'Spritely';
  ApplicationProperties.Caption := 'Spritely';
  ApplicationProperties.Version := '0.1';
  ApplicationProperties.LimitFPS := 30;
  ApplicationProperties.OnLog.Add(@LogHandler.LogCallback);

  InitializeLog;

  SplashAbout1.ShowSplash;

  PageControl1.ActivePage := TabSheet1;
  TabSheet3.TabVisible := True;

  ModeOrientation := True;
  Applog.Clear;

  {$ifdef darwin}
//  WindowState := wsFullScreen;
  {$endif}
  WriteLnLog('FormCreate : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$ifdef darwin}
//  WindowState := wsFullScreen;
  {$endif}
  AppTime := CastleGetTickCount64;
  PrepDone := False;
  KeyPreview := True;
  Caption := 'Spritely';
  Tracking := False;
  Trackbar1.Position := 20;
  Trackbar1.Max := 100;
  Button1.Caption := 'Create Sprite';
  Button2.Caption := 'Change ViewMode';
  Listview1.Visible := False;
  Splitter2.Visible := False;
end;

procedure TCastleForm.DebugBoxMenuClick(Sender: TObject);
begin
  with CastleApp.TestModel.Debug do
    begin
      Exists := not Exists;
      DebugBoxMenu.Checked := Exists;
      Listview1.Visible := Exists;
      Splitter2.Visible := Exists;
      // TabSheet3.TabVisible := Exists;
    end;
end;

procedure TCastleForm.CreateSpriteMenuClick(Sender: TObject);
begin
  Button1Click(Sender);
end;

procedure TCastleForm.ListView1Click(Sender: TObject);
begin
  ActiveControl := Window;
end;

procedure TCastleForm.TrackBar1Change(Sender: TObject);
var
  ProcTimer: Int64;
begin
  if Tracking then
    begin
    ProcTimer := CastleGetTickCount64;
    CastleApp.Stage.ChangeTextureCoordinates(CastleApp.Stage.GroundModelRoot, Trackbar1.Position);
//    CastleApp.Stage.ChangeTexture(CastleApp.Stage.GroundModelRoot, 'castle-data:/ground/myfreetextures/pavers1b2.jpg');

//    CastleApp.CameraRotation := (2 * Pi) * (Trackbar1.Position / 100000);
      ProcTimer := CastleGetTickCount64 - ProcTimer;
      WriteLnLog('ProcTimer = ' + FormatFloat('####0.000', ProcTimer / 1000) + ' seconds');

    end;
end;

procedure TCastleForm.FormDestroy(Sender: TObject);
begin
//  CastleApp.TestModel.RemoveScene(CastleApp.Viewport);
//  FreeAndNil(GroundModelRoot);
//  FreeAndNil(GroundNode);
//  FreeAndNil(LightNode);
//  FreeAndNil(CastleApp.Stage);
  WriteLnLog('FormDestroy : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleForm.TreeView1Click(Sender: TObject);
var
  Node: TTreeNode;
  NodeParent: TTreeNode;
begin
  Node := Treeview1.Selected;
  if (Node = nil) then // Nothing to do
    exit;

  NodeParent := Node.Parent;
  if (NodeParent = nil) then // User clicked on a Model
    begin
      CastleApp.TestModel.ResetAnimationState;
      Exit;
    end;

  if (NodeParent.Parent = nil) then  // User clicked on an Animation
    begin // A proper animation node
      CastleApp.TestModel.BaseRotation := Vector3(0, 0, 0);
      CastleApp.TestModel.SelectAnimation(Node.Text);
    end
  else
    begin // A TakeOne Node
      CastleApp.TestModel.BaseRotation := Vector3(0, 0, 0);
      CastleApp.TestModel.SelectAnimation(Node.Text);
    end;

  ActiveControl := Window;
end;

procedure TCastleForm.TreeView1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Node: TTreeNode;
  AnimNode: TAnimationInfo;
begin
  Node := TreeView1.GetNodeAt(MousePos.X, MousePos.Y);
  if not Assigned (Node) then
    begin
      WriteLnLog('Bad GetNodeAt');
      Exit;
    end;
  if not(Node.Data = nil) then
    begin
      if(TObject(Node.Data).ClassName = 'TAnimationInfo') then
        begin
          AnimNode := TAnimationInfo(Node.Data);
          if not(Node.Data = nil) then
            MapAnims(Node, AnimNode);
        end;
    end;
  ActiveControl := Window;
end;

procedure TCastleForm.MapAnims(const modelNode: TTreeNode; const AnimNode: TAnimationInfo);
var
  Json: TJsonNode;
  Anis: TAniTakeArray;
  Model: TCastleModel;
  NewAnimNode: TAnimationInfo;
  I: Integer;
begin
  if (modelNode.Data = nil) then
    begin
      WriteLnLog('Bad node : ' + modelNode.Text);
      Exit;
    end;
  if not(TObject(modelNode.parent.Data).ClassName = 'TCastleModel') then
    begin
      WriteLnLog('Not a model : ' + TObject(modelNode.Data).ClassName);
      Exit;
    end;
//  if not(RegularFileExists(URIToFilenameSafe(MapFile))) then
  if not(URIFileExists(MapFile)) then
    Exit;

  Model := TCastleModel(modelNode.parent.Data);

  Json := AniTxtToJson(MapFile);
  if not(Json = nil) then
    begin
      WriteLnLog('Parsed : ' + MapFile);
      Anis := AniTakeFromJson(Json);
      for I := 0 to Length(Anis) - 1 do
        begin
          with CastleApp do
            begin
              WriteLnLog('Adding Action : #' + IntToStr(I) + ' - ' + Anis[I].TakeName);
              NewAnimNode := Model.AddAnimation(Anis[I], AnimNode.Sensor, AnimNode.ParentAnim);
              Treeview1.Items.AddChildObject(modelNode, Anis[I].TakeName, NewAnimNode);
            end;
          WriteLnLog('Action : #' + IntToStr(I) +
            ' = ' + Anis[I].TakeName +
            ' : Start = ' + IntToStr(Anis[I].TakeStart) +
            ' : Stop = ' + IntToStr(Anis[I].TakeStop));
        end;
      SetLength(Anis, 0);
      Json.Free;
      modelNode.Expand(False);
    end;
end;

procedure TCastleForm.GuiBootStrap;
begin
  LoadGuiModel(ModelFile);
end;

procedure TCastleForm.LoadGuiModel(const AModel: String);
var
  I: Integer;
  modelNode: TTreeNode;
begin
  with CastleApp do
    begin
      if not(URIFileExists(AModel)) then
        Exit;

      LoadModel(URIToFilenameSafe(AModel));

      if not(TestModel = nil) then
        begin
          modelNode := Treeview1.Items.AddObject(nil, StripExtension(ExtractURIName(TestModel.ModelName)), TestModel);
          if TestModel.HasAnimations then
            begin
            for I := 0 to TestModel.Actions.Count - 1 do
              begin
                Treeview1.Items.AddChildObject(modelNode, TestModel.Actions[I], TestModel.Animations[I]);
              end;
            modelNode.Expand(False);
            end;
          modelNode.Selected := True;
          TestModel.ResetAnimationState;
        end;
      Tracking := True;
    end;
  AddInfoPanel;
end;

procedure TCastleForm.Button1Click(Sender: TObject);
var
  Sprite: TCastleImage;
  SName: String;
begin
  Button1.Enabled := False;
  with CastleApp do
    begin
      if not (TestModel.Scene = nil) then
        begin
          Sprite := CastleApp.CreateSpriteImage(CastleApp.TestModel.Scene, SpriteWidth * OverSample, SpriteHeight * OverSample);
          if not(Sprite = nil) then
            begin
              if (OverSample > 1) then
                begin
                  Sprite.Resize(SpriteWidth, SpriteHeight, riLanczos); // Mitchel);
                end;
              SName := FileNameAutoInc('grab_%4.4d.jpg');
              SaveImage(Sprite, SName);
              FreeAndNil(Sprite);
            end;
        end;
    end;
  Button1.Enabled := True;
  ActiveControl := Window;
end;

procedure TCastleForm.Button2Click(Sender: TObject);
begin
  with CastleApp do
    begin
      Stage.ChangeTexture(CastleApp.Stage.GroundModelRoot, 'castle-data:/ground/myfreetextures/pavers1b2.jpg');

//      ViewMode := ViewMode + 1;
      //  if not(CastleApp.TestModel.CurrentAnimation = -1) then
      //    begin
      //      CastleApp.TestModel.Pause;
      //    end;
    end;
  ActiveControl := Window;
end;

procedure TCastleForm.ExitMenuClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TCastleForm.FileOpenMenuClick(Sender: TObject);
begin
  CastleOpenDialog1.Filter := '3D Models|*.gltf;*.glb;*.obj;';
  if CastleOpenDialog1.Execute then
    begin
      LoadGuiModel(URIToFilenameSafe(CastleOpenDialog1.Filename));
      Caption := 'Spritely : ' + CastleOpenDialog1.Filename;
    end;
end;

procedure TCastleForm.WindowOpen(Sender: TObject);
begin
  WriteLnLog('WindowOpen : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  RenderReady := False;
  TCastleControlBase.MainControl := Window;
  CastleApp := TCastleApp.Create(Application);
  TUIState.Current := CastleApp;
  Window.Container.UIScaling := usNone;
end;

procedure TCastleForm.WindowClose(Sender: TObject);
begin
  WriteLnLog('WindowClose : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleForm.WindowMotion(Sender: TObject; const Event: TInputMotion);
begin
  UpdateInfo('Mouse', Pos2DTo3D(Event.Position.X, Event.Position.Y));
end;

procedure TCastleForm.AddInfo(const AName: String; const AValue: Integer);
begin
  AddInfo(AName, IntToStr(AValue));
end;

procedure TCastleForm.AddInfo(const AName: String; const AValue: Single);
begin
  AddInfo(AName, FormatFloat(InfoFloatFormat, AValue));
end;

procedure TCastleForm.AddInfo(const AName: String; const AValue: String);
var
  vNewItem: TListItem;
begin
  vNewItem := ListView1.Items.Add;
  vNewItem.Caption := AName;
  vNewItem.SubItems.Add(AValue);
end;

procedure TCastleForm.UpdateInfo(const AName: String; const AValue: Integer);
begin
  UpdateInfo(AName, IntToStr(AValue));
end;

procedure TCastleForm.UpdateInfo(const AName: String; const AValue: Single);
begin
  UpdateInfo(AName, FormatFloat(InfoFloatFormat, AValue));
end;

procedure TCastleForm.UpdateInfo(const AName: String; const AValue: String);
var
  idx: Integer;
begin
  for idx := 0 to ListView1.Items.Count -1 do
    begin
      if ListView1.Items[idx].Caption = AName then
        ListView1.Items[idx].SubItems[0] := AValue;
    end;
end;

procedure TCastleForm.WindowPress(Sender: TObject;
  const Event: TInputPressRelease);
var
  Q: TQuaternion;
const
  OrientationStepSize = 4;
begin
  ActiveControl := Window;
  if Event.Key = keySpace then
    begin
      if not(CastleApp.TestModel.CurrentAnimation = -1) then
        begin
          CastleApp.TestModel.Pause;
        end;
    end;

  if ModeOrientation then
    begin
      with CastleApp do
        begin
          if not(TestModel = nil) then
            begin
              if Event.Key = keyR then
                begin
                  ModelRotationCheck := not ModelRotationCheck;
                  ModelRotationDone := False;
                end;
              if Event.Key = keyNumpadPlus then
                CastleApp.iScaleMultiplier := CastleApp.iScaleMultiplier + (CastleApp.iScaleMultiplier * 0.1);
              if Event.Key = keyNumpadMinus then
                CastleApp.iScaleMultiplier := CastleApp.iScaleMultiplier - (CastleApp.iScaleMultiplier * 0.1);
              if Event.Key = keyPageUp then
                TestModel.BaseRotation.Z := TestModel.BaseRotation.Z + (Pi / OrientationStepSize);
              if Event.Key = keyPageDown then
                TestModel.BaseRotation.Z := TestModel.BaseRotation.Z - (Pi / OrientationStepSize);
              if Event.Key = keyArrowDown then
                TestModel.BaseRotation.X := TestModel.BaseRotation.X + (Pi / OrientationStepSize);
              if Event.Key = keyArrowUp then
                TestModel.BaseRotation.X := TestModel.BaseRotation.X - (Pi / OrientationStepSize);
              if Event.Key = keyArrowRight then
                TestModel.BaseRotation.Y := TestModel.BaseRotation.Y + (Pi / OrientationStepSize);
              if Event.Key = keyArrowLeft then
                TestModel.BaseRotation.Y := TestModel.BaseRotation.Y - (Pi / OrientationStepSize);
              Q := QuatFromAxisAngle(Vector4(0, 0, 0, 0));
              Q := Q * QuatFromAxisAngle(Vector4(1, 0, 0, TestModel.BaseRotation.X));
              Q := Q * QuatFromAxisAngle(Vector4(0, 1, 0, TestModel.BaseRotation.Y));
              Q := Q * QuatFromAxisAngle(Vector4(0, 0, 1, TestModel.BaseRotation.Z));

              TestModel.Transform.Rotation := Q.ToAxisAngle;
              LabelMode.Caption := 'Orientation : X = ' +
                IntToStr(RadsToFace(TestModel.BaseRotation.X)) + ', Y = ' +
                IntToStr(RadsToFace(TestModel.BaseRotation.Y)) + ', Z = ' +
                IntToStr(RadsToFace(TestModel.BaseRotation.Z));
            end;
        end;
    end;
end;

function TCastleForm.Pos2DTo3D(const AXpos: Single; const AYpos: Single): String;
var
  res: String;
  PlanePosition: TVector3;
begin
  res := 'Unknown';
  if CastleApp.Viewport.PositionToCameraPlane(Vector2(AXpos, AYpos), True, 0, PlanePosition) then
    begin
      res := PlanePosition.ToString;
    end;

  Result := res;
end;

procedure TCastleForm.AddInfoPanel;
begin
  with CastleApp do
    begin
      if not(TestModel = nil) then
        begin
          AddInfo('Mouse', '');
          AddInfo('Radius', TestModel.Scene.BoundingBox.Radius2D(2).ToString);
          AddInfo('Window Width', Window.Width);
          AddInfo('Window Height', Window.Height);
          AddInfo('Projection (Y Axis)', CastleApp.CameraElevation);
          AddInfo('Ortho Width', Viewport.Camera.Orthographic.Width);
          AddInfo('Ortho Height', Viewport.Camera.Orthographic.Height);
          AddInfo('Ortho Effective Width', Viewport.Camera.Orthographic.EffectiveWidth);
          AddInfo('Ortho Effective Height', Viewport.Camera.Orthographic.EffectiveHeight);
          AddInfo('Ortho Scale', Viewport.Camera.Orthographic.Scale);
          AddInfo('BBox 0', TestModel.Scene.BoundingBox.Data[0].ToString);
          AddInfo('BBox 1', TestModel.Scene.BoundingBox.Data[1].ToString);
          AddInfo('Translation', TestModel.Scene.Translation.ToString);
          AddInfo('Center', TestModel.Scene.Center.ToString);
          AddInfo('Rotation', TestModel.Scene.Rotation.ToString);
          AddInfo('iScale', CastleApp.iScale.ToString);
          AddInfo('iScaleMultiplier', CastleApp.iScaleMultiplier.ToString);
          AddInfo('BoundRadius', CastleApp.BoundRadius.ToString);
          AddInfo('Pos A', '');
          AddInfo('Pos B', '');
          AddInfo('Size', TestModel.Scene.BoundingBox.Size.ToString);
          AddInfo('Max Viewport', VPMax.ToString);
          AddInfo('Translation', TestModel.Transform.Translation.ToString);
          AddInfo('Center', TestModel.Transform.Center.ToString);
          AddInfo('Rotation', TestModel.Transform.Rotation.ToString);
        end;
    end;
end;

procedure TCastleForm.UpdateInfoPanel;
begin
  with CastleApp do
    begin
      if not(TestModel = nil) then
        begin
          UpdateInfo('Radius', TestModel.Scene.BoundingBox.Radius2D(2).ToString);
          UpdateInfo('Window Width', Window.Width);
          UpdateInfo('Window Height', Window.Height);
          UpdateInfo('Ortho Width', Viewport.Camera.Orthographic.Width);
          UpdateInfo('Ortho Height', Viewport.Camera.Orthographic.Height);
          UpdateInfo('Ortho Effective Width', Viewport.Camera.Orthographic.EffectiveWidth);
          UpdateInfo('Ortho Effective Height', Viewport.Camera.Orthographic.EffectiveHeight);
          UpdateInfo('Ortho Scale', Viewport.Camera.Orthographic.Scale);
          UpdateInfo('BBox 0', TestModel.Scene.BoundingBox.Data[0].ToString);
          UpdateInfo('BBox 1', TestModel.Scene.BoundingBox.Data[1].ToString);
          UpdateInfo('Translation', TestModel.Scene.Translation.ToString);
          UpdateInfo('Center', TestModel.Scene.Center.ToString);
          UpdateInfo('Rotation', TestModel.Scene.Rotation.ToString);
          UpdateInfo('iScale', CastleApp.iScale.ToString);
          UpdateInfo('iScaleMultiplier', CastleApp.iScaleMultiplier.ToString);
          UpdateInfo('BoundRadius', CastleApp.BoundRadius.ToString);
          UpdateInfo('Pos A', Pos2DTo3D(0, 0));
          UpdateInfo('Pos B', Pos2DTo3D(Window.Width, Window.Height));
          UpdateInfo('Size', TestModel.Scene.BoundingBox.Size.ToString);
          UpdateInfo('Translation', TestModel.Transform.Translation.ToString);
          UpdateInfo('Center', TestModel.Transform.Center.ToString);
          UpdateInfo('Rotation', TestModel.Transform.Rotation.ToString);
        end;
    end;
end;

end.

