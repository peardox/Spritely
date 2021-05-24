unit GUIInitialization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, CastleUIState, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, CastleControl, MainGameUnit, CastleControls,
  CastleColors, CastleUIControls, CastleTriangles, CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform, CastleViewport, CastleCameras,
  X3DNodes, X3DFields, X3DTIme, CastleImages, CastleGLImages, CastleFilesUtils,
  CastleURIUtils, MiscFunctions, CastleGLUtils,
  CastleApplicationProperties, CastleLog, CastleTimeUtils, CastleKeysMouse;

type
  { TCastleForm }

  TCastleForm = class(TForm)
    Button1: TButton;
    ListView1: TListView;
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
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure WindowClose(Sender: TObject);
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
    function  Pos2DTo3D(const AXpos: Single; const AYpos: Single): String;
    procedure AddInfoPanel;
    procedure UpdateInfoPanel;
  end;

var
  CastleForm: TCastleForm;
  gYAngle: Single;

const
  {$if defined(windows)}
  FSPrefix = 'C:';
  {$endif}
  {$if defined(linux)}
  FSPrefix = 'file:/home/simon';
  {$endif}
  {$if defined(darwin)}
  FSPrefix = 'file:/Users/simon';
  {$endif}

  InfoFloatFormat: String = '###0.0000';
//  MapFile: String = FSPrefix + PathDelim + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elf-Males' + PathDelim + 'elfrangers-aniamtions-list.txt';
//  ModelFile: String = 'castle-data:/Quaternius/RPGCharacters/Wizard.glb';
  ModelFile: String = 'castle-data:/up.glb';
//  ModelFile: String = 'castle-data:/tavern/scene.gltf';
//  ModelFile: String = FSPrefix + PathDelim + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elf-Males' + PathDelim + 'FBX 2013' + PathDelim + 'Elf-03.glb';
//  ModelFile: String = FSPrefix + PathDelim + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'chibii-racers-dirt-bikes' + PathDelim + 'gitf' + PathDelim + 'dirt_bike01.gltf';
//  ModelFile: String = FSPrefix + PathDelim + 'Assets' + PathDelim + 'TurboSquid' + PathDelim + 'Wyvern' + PathDelim + 'GreenDragon.glb';

implementation
{$R *.lfm}

procedure TCastleForm.FormCreate(Sender: TObject);
begin
  InitializeLog;
  {$ifdef darwin}
  WindowState := wsFullScreen;
  {$endif}
  WriteLnLog('FormCreate : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$ifdef darwin}
  WindowState := wsFullScreen;
  {$endif}
  AppTime := CastleGetTickCount64;
  PrepDone := False;
  gYAngle := 2;
  Caption := 'Spritely GUI';
  Tracking := False;
  Trackbar1.Max := 30000;
end;

procedure TCastleForm.TrackBar1Change(Sender: TObject);
var
  NewScale: Single;
begin
  if Tracking then
    begin
      NewScale := Trackbar1.Position / 10000;
      CastleApp.TestModel.Scene.Scale := Vector3(NewScale, NewScale, NewScale);
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

procedure TCastleForm.GuiBootStrap;
var
  I: Integer;
  model: TTreeNode;
  anim: TTreeNode;
begin
  with CastleApp do
    begin
      LoadModel(ModelFile);
      if not(TestModel = nil) then
        begin
          model := Treeview1.Items.AddObject(nil, StripExtension(ExtractURIName(TestModel.ModelName)), TestModel);
          if TestModel.HasAnimations then
            begin
            for I := 0 to TestModel.Actions.Count - 1 do
              begin
                anim := Treeview1.Items.AddChild(model, TestModel.Actions[I]);
//                if I = 0 then
//                  anim.Visible := False;
              end;
            model.Expand(False);
            end;
          model.Selected := True;
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
{
  if not(CastleApp.TestModel.CurrentAnimation = -1) then
    begin
      CastleApp.TestModel.Pause;
    end;
  Exit;
}
  if not (CastleApp.TestModel.Scene = nil) then
    begin
      Sprite := CastleApp.CreateSpriteImage(CastleApp.TestModel.Scene, 1536, 1536);
      if not(Sprite = nil) then
        begin
          SName := FileNameAutoInc('grab_%4.4d.jpg');
          SaveImage(Sprite, SName);
//          infoNotifications.Show('Saved : ' + SName);
          FreeAndNil(Sprite);
        end;
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
      AddInfo('Max Viewport', MaxVP.ToString);
    end;
end;

procedure TCastleForm.UpdateInfoPanel;
begin
  with CastleApp do
    begin
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
      end;
end;

end.

