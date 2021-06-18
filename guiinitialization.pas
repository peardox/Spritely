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
  CastleQuaternions, SpritelyLog, staging, multimodel, ExpandPanels, BGRAKnob,
  BCLabel, ECSwitch, ECSlider, ECSpinCtrls, CastleGLShaders, X3DLoad;

type
  { TCastleForm }

  TCastleForm = class(TForm)
    BGRAKnob1: TBGRAKnob;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CastleOpenDialog1: TCastleOpenDialog;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    GroundHeightSlider: TECSlider;
    GroundScaleSlider: TECSlider;
    ECSwitch1: TECSwitch;
    GroundHeightEdit: TFloatSpinEdit;
    GroundScaleEdit: TFloatSpinEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LightLabel: TLabel;
    ListView1: TListView;
    ListView2: TListView;
    MainMenu1: TMainMenu;
    AppLog: TMemo;
    MenuItem1: TMenuItem;
    FileOpenMenu: TMenuItem;
    DebugBoxMenu: TMenuItem;
    CreateSpriteMenu: TMenuItem;
    ExitMenu: TMenuItem;
    SelectDirectoryMenuItem: TMenuItem;
    MyRollOut1: TMyRollOut;
    MyRollOut2: TMyRollOut;
    MyRollOut3: TMyRollOut;
    PageControl1: TPageControl;
    Panel1: TPanel;
    GPPanel1: TPanel;
    Panel4: TPanel;
    GPPanel2: TPanel;
    Panel7: TPanel;
    PanelLeft: TPanel;
    Panel3: TPanel;
    PanelLeftBottom: TPanel;
    Panel5: TPanel;
    PanelRight: TPanel;
    PopupMenu1: TPopupMenu;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TreeView1: TTreeView;
    Window: TCastleControlBase;
    procedure BGRAKnob1ValueChanged(Sender: TObject; Value: single);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure GroundHeightSliderChange(Sender: TObject);
    procedure GroundScaleSliderChange(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure FileOpenMenuClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DebugBoxMenuClick(Sender: TObject);
    procedure CreateSpriteMenuClick(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure SelectDirectoryMenuItemClick(Sender: TObject);
    procedure TabSheet3Show(Sender: TObject);
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
    procedure FocusViewport;
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
  MapFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elongata' + PathDelim + 'Elong_anim.txt';
  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elongata' + PathDelim + 'gltf' + PathDelim + 'ElongataGreen.glb';
//  MapFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elf-Males' + PathDelim + 'elfrangers-aniamtions-list.txt';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elf-Males' + PathDelim + 'FBX 2013' + PathDelim + 'Elf-03.glb';
//  MapFile := FSPrefix + 'Assets' + PathDelim + 'JoseDiaz' + PathDelim + 'German_Shepherd_new' + PathDelim + 'German Shepherd Animation Ranges.txt';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + 'JoseDiaz' + PathDelim + 'german_shepherd' + PathDelim + 'scene.gltf';
//  MapFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'chibii-racers-dirt-bikes' + PathDelim + '_bike_animations.txt';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'chibii-racers-dirt-bikes' + PathDelim + 'gitf' + PathDelim + 'dirt_bike01.gltf';
//  MapFile := FSPrefix  + 'Assets' + PathDelim + '3drt' + PathDelim + 'gltf' + PathDelim + 'Thief' + PathDelim + 'Thief-animations-list.txt';
//  ModelFile := FSPrefix  + 'Assets' + PathDelim + '3drt' + PathDelim + 'gltf' + PathDelim + 'Thief' + PathDelim + 'thief_torch.glb';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + 'TurboSquid' + PathDelim + 'Wyvern' + PathDelim + 'GreenDragon.glb';
{$endif}
//  LogShaders := true;

  LogHandler := TLogHandler.Create(Application);

  ApplicationProperties.ApplicationName := 'Spritely';
  ApplicationProperties.Caption := 'Spritely';
  ApplicationProperties.Version := '0.1';
  ApplicationProperties.LimitFPS := 30;
  ApplicationProperties.OnLog.Add(@LogHandler.LogCallback);

  InitializeLog;
{
  ExpandPanels1.AddPanel(MyRollOut1);
  ExpandPanels1.AddPanel(MyRollOut2);
  ExpandPanels1.AddPanel(MyRollOut3);
}
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
  Button1.Caption := 'Create Sprite';
  Button2.Caption := 'Change ViewMode';
  Listview1.Visible := False;
  Splitter2.Visible := False;
end;

procedure TCastleForm.DebugBoxMenuClick(Sender: TObject);
begin
  with CastleApp.WorkingModel.Debug do
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
  FocusViewport;
end;

procedure TCastleForm.SelectDirectoryMenuItemClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    begin
      // AddGuiModel(URIToFilenameSafe(CastleOpenDialog1.Filename));
      // Caption := 'Spritely : ' + CastleOpenDialog1.Filename;
    end;
end;

procedure TCastleForm.TabSheet3Show(Sender: TObject);
begin
  AppLog.VertScrollBar.Position := 99999;
end;

procedure TCastleForm.FormDestroy(Sender: TObject);
begin
  WriteLnLog('FormDestroy : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleForm.TreeView1Click(Sender: TObject);
var
  Node: TTreeNode;
  NodeParent: TTreeNode;
  ClickedModel: TCastleModel;
begin
  Node := Treeview1.Selected;
  if (Node = nil) then // Nothing to do
    exit;

  NodeParent := Node.Parent;
  if (NodeParent = nil) then // User clicked on a Model
    begin
      if TObject(Node.Data).ClassName = 'TCastleModel' then
        begin
          ClickedModel := TCastleModel(Node.Data);
          With CastleApp do
            begin
              if not(ClickedModel = WorkingModel) then
                begin
                  Stage.Remove(WorkingModel.Scene);
                  WorkingModel := ClickedModel;
                  Stage.Add(WorkingModel.Scene);
                end;
              WorkingModel.ResetAnimationState;
            end;
        end;
      Exit;
    end;

  if (NodeParent.Parent = nil) then  // User clicked on an Animation
    begin // A proper animation node
//      CastleApp.WorkingModel.BaseRotation := Vector3(0, 0, 0);
//      CastleApp.WorkingModel.SelectAnimation(Node.Text);
    end
  else
    begin // A TakeOne Node
//      CastleApp.WorkingModel.BaseRotation := Vector3(0, 0, 0);
//      CastleApp.WorkingModel.SelectAnimation(Node.Text);
    end;

    FocusViewport;
end;

procedure TCastleForm.FocusViewport;
begin
  if PageControl1.ActivePage = TabSheet1 then
    ActiveControl := Window;
end;

procedure TCastleForm.TreeView1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Node: TTreeNode;
  AnimNode: TAnimationInfo;
  ModelNode: TCastleModel;
begin
  Node := TreeView1.GetNodeAt(MousePos.X, MousePos.Y);
  if not Assigned (Node) then
    begin
      WriteLnLog('Bad GetNodeAt');
      Exit;
    end;
  if not(Node.Data = nil) then
    begin
      WriteLnLog('TreeView node references ' + TObject(Node.Data).ClassName);

      if(TObject(Node.Data).ClassName = 'TAnimationInfo') then
        begin
          AnimNode := TAnimationInfo(Node.Data);
          if not(AnimNode.IsMapped) then
            MapAnims(Node, AnimNode);
        end
      else if(TObject(Node.Data).ClassName = 'TCastleModel') then
        begin
          ModelNode := TCastleModel(Node.Data);
          WriteLnLog('Context Clicked ' + ModelNode.ModelName);
        end;
    end;
  FocusViewport;
end;

procedure TCastleForm.MapAnims(const modelNode: TTreeNode; const AnimNode: TAnimationInfo);
var
  Json: TJsonNode;
  Anis: TAniTakeArray;
  Model: TCastleModel;
  NewAnimNode: TAnimationInfo;
  I: Integer;
  dupe: Integer;
  NewName: String;
  DupeSuffix: Integer;
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
              // Rename Dupes
              NewName := Anis[I].TakeName;
              DupeSuffix := 1;
              if Model.Actions.Find(NewName, dupe) then
                begin
                  while Model.Actions.Find(NewName + '_' + IntToStr(DupeSuffix), dupe) do
                    begin
                      Inc(DupeSuffix);
                    end;
                  NewName := NewName + '_' + IntToStr(DupeSuffix);
                  WriteLnLog('Dupe : Replace : ' + Anis[I].TakeName + ' with ' + NewName);
                  Anis[I].TakeName := NewName;
                end;
              NewAnimNode := Model.AddAnimation(Anis[I], AnimNode.Sensor, AnimNode.ParentAnim);
              Treeview1.Items.AddChildObject(modelNode, Anis[I].TakeName, NewAnimNode);
            end;
        end;
      SetLength(Anis, 0);
      Json.Free;
      AnimNode.IsMapped := True;
      modelNode.Expand(False);
    end;
end;

procedure TCastleForm.GuiBootStrap;
begin
//  Treeview1.Items.Clear;
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

      if not(WorkingModel = nil) then
        begin
          modelNode := Treeview1.Items.AddObject(nil, StripExtension(ExtractURIName(WorkingModel.ModelName)), WorkingModel);
          if WorkingModel.HasAnimations then
            begin
            for I := 0 to WorkingModel.Actions.Count - 1 do
              begin
                Treeview1.Items.AddChildObject(modelNode, WorkingModel.Actions[I], WorkingModel.Animations[I]);
              end;
            modelNode.Expand(False);
            end;
          modelNode.Selected := True;
          WorkingModel.ResetAnimationState;
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
      if not (WorkingModel.Scene = nil) then
        begin
          Sprite := CastleApp.CreateSpriteImage(CastleApp.WorkingModel.Scene, SpriteWidth * OverSample, SpriteHeight * OverSample);
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
  FocusViewport;
end;

procedure TCastleForm.BGRAKnob1ValueChanged(Sender: TObject; Value: single);
var
  ATheta: Single;
  ARadius: Single;
  AElevation: Single;
  ADir: TVector3;
begin
  LightLabel.Caption := FormatFloat('###0.0000', BGRAKnob1.Value);
  ARadius := 1;
  ATheta := DegToRad(BGRAKnob1.Value);
  AElevation := -12;
  ADir := Vector3(sqrt(ARadius) * Cos(ATheta),
                  AElevation,
                  sqrt(ARadius) * Sin(ATheta));

//  CastleApp.Stage.LightNode.Direction := ADir;
{
  CastleApp.Stage.LightNode.DefaultShadowMap := TGeneratedShadowMapNode.Create;
  CastleApp.Stage.LightNode.DefaultShadowMap.Update := upAlways;
  CastleApp.Stage.LightNode.DefaultShadowMap.Size := 4096;
  CastleApp.Stage.LightNode.ShadowVolumesMain := False;
  CastleApp.Stage.LightNode.ShadowVolumes := False;
}
  CastleApp.LabelMode.Caption := 'Light Direction : ' + ADir.ToString;

end;

procedure TCastleForm.Button2Click(Sender: TObject);
begin
  with CastleApp do
    begin
//      SaveNode(CastleApp.Stage.RootNode, 'testscene.x3dv');
      // Stage.ChangeTexture(CastleApp.Stage.GroundModelRoot, 'castle-data:/ground/myfreetextures/pavers1b2.jpg');

      ViewMode := ViewMode + 1;
      //  if not(CastleApp.WorkingModel.CurrentAnimation = -1) then
      //    begin
      //      CastleApp.WorkingModel.Pause;
      //    end;
    end;
  FocusViewport;
end;

procedure TCastleForm.GroundHeightSliderChange(Sender: TObject);
begin
  GroundHeightEdit.Value := GroundHeightSlider.Position;
  CastleApp.WorkingModel.Transform.Translation := Vector3(0, GroundHeightSlider.Position, 0);
end;

procedure TCastleForm.GroundScaleSliderChange(Sender: TObject);
begin
  GroundScaleEdit.Value := GroundScaleSlider.Position;
  CastleApp.Stage.ChangeTextureCoordinates(CastleApp.Stage.GroundModelRoot, GroundScaleSlider.Position);
end;

procedure TCastleForm.ExitMenuClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TCastleForm.FileOpenMenuClick(Sender: TObject);
var
  i: Integer;
begin
  CastleOpenDialog1.Filter := '3D Models|*.gltf;*.glb;*.obj;*.x3d;*.x3dv';
  if CastleOpenDialog1.Execute then
    begin
      if CastleOpenDialog1.Files.Count = 1 then
        begin
          WriteLnLog(CastleOpenDialog1.Files[0]);
          LoadGuiModel(URIToFilenameSafe(CastleOpenDialog1.Filename));
          Caption := 'Spritely : ' + CastleOpenDialog1.Filename;
        end
      else
        begin
          for i := 0 to CastleOpenDialog1.Files.Count - 1 do
            begin
              WriteLnLog(CastleOpenDialog1.Files[i]);
              LoadGuiModel(URIToFilenameSafe(CastleOpenDialog1.Files[i]));
            end;
        end;
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
  FocusViewport;
  if Event.Key = keySpace then
    begin
      if not(CastleApp.WorkingModel.CurrentAnimation = -1) then
        begin
          CastleApp.WorkingModel.Pause;
        end;
    end;

  if ModeOrientation then
    begin
      with CastleApp do
        begin
          if not(WorkingModel = nil) then
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
                WorkingModel.BaseRotation.Z := WorkingModel.BaseRotation.Z + (Pi / OrientationStepSize);
              if Event.Key = keyPageDown then
                WorkingModel.BaseRotation.Z := WorkingModel.BaseRotation.Z - (Pi / OrientationStepSize);
              if Event.Key = keyArrowDown then
                WorkingModel.BaseRotation.X := WorkingModel.BaseRotation.X + (Pi / OrientationStepSize);
              if Event.Key = keyArrowUp then
                WorkingModel.BaseRotation.X := WorkingModel.BaseRotation.X - (Pi / OrientationStepSize);
              if Event.Key = keyArrowRight then
                WorkingModel.BaseRotation.Y := WorkingModel.BaseRotation.Y + (Pi / OrientationStepSize);
              if Event.Key = keyArrowLeft then
                WorkingModel.BaseRotation.Y := WorkingModel.BaseRotation.Y - (Pi / OrientationStepSize);
              Q := QuatFromAxisAngle(Vector4(0, 0, 0, 0));
              Q := Q * QuatFromAxisAngle(Vector4(1, 0, 0, WorkingModel.BaseRotation.X));
              Q := Q * QuatFromAxisAngle(Vector4(0, 1, 0, WorkingModel.BaseRotation.Y));
              Q := Q * QuatFromAxisAngle(Vector4(0, 0, 1, WorkingModel.BaseRotation.Z));

              WorkingModel.Transform.Rotation := Q.ToAxisAngle;
              LabelMode.Caption := 'Orientation : X = ' +
                IntToStr(RadsToFace(WorkingModel.BaseRotation.X)) + ', Y = ' +
                IntToStr(RadsToFace(WorkingModel.BaseRotation.Y)) + ', Z = ' +
                IntToStr(RadsToFace(WorkingModel.BaseRotation.Z));
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
      if not(WorkingModel = nil) then
        begin
          AddInfo('Mouse', '');
          AddInfo('Radius', WorkingModel.Scene.BoundingBox.Radius2D(2).ToString);
          AddInfo('Window Width', Window.Width);
          AddInfo('Window Height', Window.Height);
          AddInfo('Projection (Y Axis)', CastleApp.CameraElevation);
          AddInfo('Ortho Width', Viewport.Camera.Orthographic.Width);
          AddInfo('Ortho Height', Viewport.Camera.Orthographic.Height);
          AddInfo('Ortho Effective Width', Viewport.Camera.Orthographic.EffectiveWidth);
          AddInfo('Ortho Effective Height', Viewport.Camera.Orthographic.EffectiveHeight);
          AddInfo('Ortho Scale', Viewport.Camera.Orthographic.Scale);
          AddInfo('BBox 0', WorkingModel.Scene.BoundingBox.Data[0].ToString);
          AddInfo('BBox 1', WorkingModel.Scene.BoundingBox.Data[1].ToString);
          AddInfo('Translation', WorkingModel.Scene.Translation.ToString);
          AddInfo('Center', WorkingModel.Scene.Center.ToString);
          AddInfo('Rotation', WorkingModel.Scene.Rotation.ToString);
          AddInfo('iScale', CastleApp.iScale.ToString);
          AddInfo('iScaleMultiplier', CastleApp.iScaleMultiplier.ToString);
          AddInfo('BoundRadius', CastleApp.BoundRadius.ToString);
          AddInfo('Pos A', '');
          AddInfo('Pos B', '');
          AddInfo('Size', WorkingModel.Scene.BoundingBox.Size.ToString);
          AddInfo('Max Viewport', VPMax.ToString);
          AddInfo('Translation', WorkingModel.Transform.Translation.ToString);
          AddInfo('Center', WorkingModel.Transform.Center.ToString);
          AddInfo('Rotation', WorkingModel.Transform.Rotation.ToString);
        end;
    end;
end;

procedure TCastleForm.UpdateInfoPanel;
begin
  with CastleApp do
    begin
      if not(WorkingModel = nil) then
        begin
          UpdateInfo('Radius', WorkingModel.Scene.BoundingBox.Radius2D(2).ToString);
          UpdateInfo('Window Width', Window.Width);
          UpdateInfo('Window Height', Window.Height);
          UpdateInfo('Ortho Width', Viewport.Camera.Orthographic.Width);
          UpdateInfo('Ortho Height', Viewport.Camera.Orthographic.Height);
          UpdateInfo('Ortho Effective Width', Viewport.Camera.Orthographic.EffectiveWidth);
          UpdateInfo('Ortho Effective Height', Viewport.Camera.Orthographic.EffectiveHeight);
          UpdateInfo('Ortho Scale', Viewport.Camera.Orthographic.Scale);
          UpdateInfo('BBox 0', WorkingModel.Scene.BoundingBox.Data[0].ToString);
          UpdateInfo('BBox 1', WorkingModel.Scene.BoundingBox.Data[1].ToString);
          UpdateInfo('Translation', WorkingModel.Scene.Translation.ToString);
          UpdateInfo('Center', WorkingModel.Scene.Center.ToString);
          UpdateInfo('Rotation', WorkingModel.Scene.Rotation.ToString);
          UpdateInfo('iScale', CastleApp.iScale.ToString);
          UpdateInfo('iScaleMultiplier', CastleApp.iScaleMultiplier.ToString);
          UpdateInfo('BoundRadius', CastleApp.BoundRadius.ToString);
          UpdateInfo('Pos A', Pos2DTo3D(0, 0));
          UpdateInfo('Pos B', Pos2DTo3D(Window.Width, Window.Height));
          UpdateInfo('Size', WorkingModel.Scene.BoundingBox.Size.ToString);
          UpdateInfo('Translation', WorkingModel.Transform.Translation.ToString);
          UpdateInfo('Center', WorkingModel.Transform.Center.ToString);
          UpdateInfo('Rotation', WorkingModel.Transform.Rotation.ToString);
        end;
    end;
end;

end.

