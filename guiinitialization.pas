unit GUIInitialization;

{$mode objfpc}{$H+}
// {$define pausebtn}
{$define disableMap}

// FPS = 23.98, 24, 25, 29.97, 30, 50, 59.94, 60, Custom
interface

uses
  Classes, SysUtils, Math, CastleUIState, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Menus, CastleControl, MainGameUnit,
  CastleControls, CastleColors, CastleUIControls, CastleTriangles, CastleShapes,
  CastleVectors, CastleSceneCore, CastleScene, CastleTransform, CastleViewport,
  CastleCameras, X3DNodes, X3DFields, X3DTIme, CastleImages, CastleGLImages,
  CastleFilesUtils, CastleURIUtils, MiscFunctions, CastleGLUtils,
  CastleLCLUtils, CastleApplicationProperties, CastleLog, CastleTimeUtils,
  CastleKeysMouse, JsonTools, AniTxtJson, AniTakeUtils, Types, multimodel;

type
  { TCastleForm }

  TCastleForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    FileOpenMenu: TMenuItem;
    DebugBoxMenu: TMenuItem;
    CreateSpriteMenu: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TrackBar1: TTrackBar;
    TreeView1: TTreeView;
    Window: TCastleControlBase;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DebugBoxMenuClick(Sender: TObject);
    procedure CreateSpriteMenuClick(Sender: TObject);
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
  private
    Tracking: Boolean;
  public
    procedure GuiBootStrap;
    procedure MapAnims(const modelNode: TTreeNode; const AnimNode: TAnimationInfo);
    function  Pos2DTo3D(const AXpos: Single; const AYpos: Single): String;
    procedure AddInfoPanel;
    procedure UpdateInfoPanel;
  end;

var
  CastleForm: TCastleForm;
  gYAngle: Single;
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

//  ModelFile := 'castle-data:/Quaternius/RPGCharacters/Wizard.glb';
//  ModelFile := 'castle-data:/up.glb';
//  ModelFile := 'castle-data:/oblique.glb';
//  ModelFile := 'castle-data:/up311.glb';
//  ModelFile := 'castle-data:/up131.glb';
//  ModelFile := 'castle-data:/up113.glb';
//  ModelFile := 'castle-data:/tavern/scene.gltf';
//  MapFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elf-Males' + PathDelim + 'elfrangers-aniamtions-list.txt';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elf-Males' + PathDelim + 'FBX 2013' + PathDelim + 'Elf-03.glb';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'chibii-racers-dirt-bikes' + PathDelim + 'gitf' + PathDelim + 'dirt_bike01.gltf';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + 'TurboSquid' + PathDelim + 'Wyvern' + PathDelim + 'GreenDragon.glb';
  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + '3DRT-Medieval-Houses' + PathDelim + 'gltf' + PathDelim + 'house-02-01.glb';
//  ModelFile := FSPrefix  + 'Assets' + PathDelim + 'ZerinLabs' + PathDelim + 'Retro-Gothic-EnviroKit' + PathDelim + 'glb' + PathDelim + 'deco_cathedral_table.glb';
//  ModelFile := FSPrefix  + 'Assets' + PathDelim + '3drt' + PathDelim + 'gltf' + PathDelim + 'Thief' + PathDelim + 'thief_torch.glb';
  InitializeLog;
  {$ifdef darwin}
//  WindowState := wsFullScreen;
  {$endif}
  WriteLnLog('FormCreate : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$ifdef darwin}
  WindowState := wsFullScreen;
  {$endif}
  AppTime := CastleGetTickCount64;
  PrepDone := False;
  gYAngle := 2;
  Caption := 'Spritely';
  Tracking := False;
  Trackbar1.Max := 30000;
  {$ifdef pausebtn}
  Button1.Caption := 'Pause / Play';
  {$else}
  Button1.Caption := 'Create Sprite';
  {$endif}
  Button2.Caption := 'Scale 1.0'; // 'Pause / Play'; // 'Split Take 001';
end;

procedure TCastleForm.DebugBoxMenuClick(Sender: TObject);
begin
  with CastleApp.TestModel.Debug do
    begin
      Exists := not Exists;
      DebugBoxMenu.Checked := Exists;
    end;
end;

procedure TCastleForm.CreateSpriteMenuClick(Sender: TObject);
begin
  {$ifndef pausebtn}
  Button1.Enabled := False;
  {$endif}
  Button1Click(Sender);
end;

procedure TCastleForm.TrackBar1Change(Sender: TObject);
begin
  if Tracking then
    begin
      CastleApp.iScale := Trackbar1.Position / 10000;
    end;
end;

procedure TCastleForm.FormDestroy(Sender: TObject);
begin
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
    begin
      CastleApp.TestModel.SelectAnimation(Node.Text);
    end;
end;

procedure TCastleForm.TreeView1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Node: TTreeNode;
  AnimNode: TAnimationInfo;
begin
  {$ifdef disableMap}
  Exit;
  {$endif}
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
          // Node.Visible := False;
          MapAnims(Node.Parent, AnimNode);
        end;
    end;

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
  if not(TObject(modelNode.Data).ClassName = 'TCastleModel') then
    begin
      WriteLnLog('Not a model : ' + TObject(modelNode.Data).ClassName);
      Exit;
    end;

  Model := TCastleModel(modelNode.Data);

  Json := AniTxtToJson(MapFile);
  if not(Json = nil) then
    begin
      WriteLnLog('Parsed : ' + MapFile);
      Anis := AniTakeFromJson(Json);
      for I := 0 to Length(Anis) - 1 do
        begin
          with CastleApp do
            begin
              NewAnimNode := Model.AddAnimation(Anis[I], AnimNode.Sensor);
              Treeview1.Items.AddChildObject(modelNode, Anis[I].TakeName, NewAnimNode);
            end;
          WriteLnLog('Action : #' + IntToStr(I) +
            ' = ' + Anis[I].TakeName +
            ' : Start = ' + IntToStr(Anis[I].TakeStart) +
            ' : Stop = ' + IntToStr(Anis[I].TakeStop));
        end;
      SetLength(Anis, 0);
      Json.Free;
    end;
end;

procedure TCastleForm.GuiBootStrap;
var
  I: Integer;
  modelNode: TTreeNode;
begin
  with CastleApp do
    begin
      LoadModel(URIToFilenameSafe(ModelFile));
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
          ShowModel(TestModel);
          TestModel.ResetAnimationState;
        end;
      Trackbar1.Position := Trunc(TestModel.Scene.Scale.X * 10000);
      Tracking := True;
    end;
  AddInfoPanel;
end;

procedure TCastleForm.Button1Click(Sender: TObject);
var
  Sprite: TCastleImage;
  SName: String;
begin
{$ifdef pausebtn}
  if not(CastleApp.TestModel.CurrentAnimation = -1) then
    begin
      CastleApp.TestModel.Pause;
    end;
  Exit;
{$else}
  Button1.Enabled := False;
  if not (CastleApp.TestModel.Scene = nil) then
    begin
      Sprite := CastleApp.CreateSpriteImage(CastleApp.TestModel.Scene, 8192, 8192);
      if not(Sprite = nil) then
        begin
          SName := FileNameAutoInc('grab_%4.4d.jpg');
          SaveImage(Sprite, SName);
          FreeAndNil(Sprite);
        end;
    end;
  Button1.Enabled := True;
{$endif}
end;

procedure TCastleForm.Button2Click(Sender: TObject);
begin
  with CastleApp do
    begin
      if not((Max(TestModel.Scene.BoundingBox.SizeX, TestModel.Scene.BoundingBox.SizeY) - Min(Viewport.Camera.Orthographic.EffectiveHeight, Viewport.Camera.Orthographic.EffectiveWidth)) < 0.1) then
        begin
          iScale := 1 / Max(TestModel.Scene.BoundingBox.SizeX, TestModel.Scene.BoundingBox.SizeY);
          TestModel.LockedScale := iScale;
        end;
      //  if not(CastleApp.TestModel.CurrentAnimation = -1) then
      //    begin
      //      CastleApp.TestModel.Pause;
      //    end;
    end;
end;

procedure TCastleForm.WindowOpen(Sender: TObject);
begin
  WriteLnLog('WindowOpen : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  RenderReady := False;
  MaxVP := GLFeatures.MaxViewportDimensions;
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
      AddInfo('Mouse', '');
      AddInfo('Radius', TestModel.Scene.BoundingBox.Radius2D(2).ToString);
      AddInfo('Window Width', Window.Width);
      AddInfo('Window Height', Window.Height);
      AddInfo('Projection (Y Axis)', gYAngle);
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
      AddInfo('3D Scale', TestModel.Scene.Scale.ToString);
      AddInfo('Pos A', '');
      AddInfo('Pos B', '');
      AddInfo('Size', TestModel.Scene.BoundingBox.Size.ToString);
      AddInfo('Max Viewport', MaxVP.ToString);
    end;
end;

procedure TCastleForm.UpdateInfoPanel;
begin
  with CastleApp do
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
      UpdateInfo('3D Scale', TestModel.Scene.Scale.ToString);
      UpdateInfo('Pos A', Pos2DTo3D(0, 0));
      UpdateInfo('Pos B', Pos2DTo3D(Window.Width, Window.Height));
      UpdateInfo('Size', TestModel.Scene.BoundingBox.Size.ToString);
      end;
end;

end.

