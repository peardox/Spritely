unit GUIInitialization;
{
  Buglist

  Right Click Action
  Badly Formatted Anitxt
}
{$mode objfpc}{$H+}
{$define disableMap}
// {$define remotefile}

// FPS = 23.98, 24, 25, 29.97, 30, 50, 59.94, 60, Custom
interface

uses
  Classes, SysUtils, Math, CastleUIState, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Menus, Spin, Arrow,
  CastleControl, MainGameUnit,
  CastleControls, CastleColors, CastleUIControls, CastleTriangles, CastleShapes,
  CastleVectors, CastleSceneCore, CastleScene, CastleTransform, CastleViewport,
  CastleCameras, X3DNodes, X3DFields, X3DTIme, CastleImages, CastleGLImages,
  CastleFilesUtils, CastleURIUtils, MiscFunctions, CastleLCLUtils,
  CastleDialogs, CastleApplicationProperties, CastleLog, CastleTimeUtils,
  CastleKeysMouse, JsonTools, AniTxtJson, AniTakeUtils, Types,
  CastleQuaternions, SpritelyLog, staging, multimodel,
  ECSlider, CastleGLShaders, X3DLoad,
  SheetView, Overlays, RGBAlphaImageHelp;

type
  { TCastleForm }

  TCastleForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CastleOpenDialog1: TCastleOpenDialog;
    LogPanel: TPanel;
    RenderPanel: TPanel;
    TabControl1: TTabControl;
    ComboBox1: TComboBox;
    ECSlider1: TECSlider;
    ImageList1: TImageList;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    AppLog: TMemo;
    MenuItem1: TMenuItem;
    FileOpenMenu: TMenuItem;
    DebugBoxMenu: TMenuItem;
    CreateSpriteMenu: TMenuItem;
    ExitMenu: TMenuItem;
    SelectDirectoryMenuItem: TMenuItem;
    Panel1: TPanel;
    PanelLeft: TPanel;
    Panel3: TPanel;
    PanelLeftBottom: TPanel;
    Panel5: TPanel;
    PopupMenu1: TPopupMenu;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    TreeView1: TTreeView;
    Window: TCastleControlBase;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ECSlider1Change(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure FileOpenMenuClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DebugBoxMenuClick(Sender: TObject);
    procedure CreateSpriteMenuClick(Sender: TObject);
    procedure RenderPanelResize(Sender: TObject);
    procedure SelectDirectoryMenuItemClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure TreeView1EditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
    procedure WindowClose(Sender: TObject);
    procedure WindowMotion(Sender: TObject; const Event: TInputMotion);
    procedure WindowOpen(Sender: TObject);

    procedure WindowPress(Sender: TObject; const Event: TInputPressRelease);

    procedure MapAnims(const modelNode: TTreeNode; const AnimNode: TAnimationInfo);
    function  Pos2DTo3D(const AXpos: Single; const AYpos: Single): String;
    procedure LoadGuiModel(const AModel: String; const isRemote: Boolean = False);
    function  SyncModelFromNode(const Node: Pointer): TCastleModel;
    function  IdentifyNode(const Node: TTreeNode): Cardinal;
  private
    Tracking: Boolean;
    ModeOrientation: Boolean;
  public
    procedure GuiBootStrap;
    procedure AddModelToTree(const AModel: TCastleModel; const doExpand: Boolean);
  end;

var
  CastleForm: TCastleForm;
  ScanModelDir: String;
  FSPrefix: String;
  ModelFile: String;
  MapFile: String;
  CallCounter: Integer;

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

  ScanModelDir := FSPrefix + '3DModels' + PathDelim + 'Kenney' + PathDelim + 'Pirate Kit';
{$ifdef disableMap}
  MapFile := '';
{$ifdef remotefile}
  ModelFile := 'https://spritely.co.uk/3DModels/Quaternius/RPG Characters - Nov 2020/Cleric.glb';
{$else}
//  ModelFile := 'castle-data:/oblique.glb';
//  ModelFile := 'castle-data:/up.glb';
//  ModelFile := 'castle-data:/up131.glb';
//  ModelFile := 'castle-data:/Quaternius/Cute Animated Monsters - Aug 2020/Demon.glb';
//  ModelFile := 'castle-data:/Quaternius/RPGCharacters/Cleric.glb';
//  ModelFile := 'castle-data:/Quaternius/RPGCharacters/Monk.glb';
//  ModelFile := 'castle-data:/Quaternius/RPGCharacters/Ranger.glb';
//  ModelFile := 'castle-data:/Quaternius/RPGCharacters/Rogue.glb';
  ModelFile := 'castle-data:/Quaternius/RPGCharacters/Warrior.glb';
//  ModelFile := 'castle-data:/Quaternius/RPGCharacters/Wizard.glb';
//  ModelFile := 'castle-data:/Quaternius/Mechs/Stan.glb';
//  ModelFile := 'castle-data:/isoroom/scene.gltf';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + 'simple_classic_crate' + PathDelim + 'scene.gltf';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + 'JoseDiaz' + PathDelim + 'cavewoman' + PathDelim + 'cavewoman.gltf' + PathDelim + 'scene.gltf';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + '3DRT-Medieval-Houses' + PathDelim + 'gltf' + PathDelim + 'house-02-01.glb';
//  ModelFile := FSPrefix  + 'Assets' + PathDelim + 'Sketchfab' + PathDelim + 'crocodile_with_animation' + PathDelim + 'crock-up.glb';
//  ModelFile := FSPrefix  + 'Assets' + PathDelim + 'Sketchfab' + PathDelim + 'generic_cliff_2_mobile_rhe' + PathDelim + 'scene.gltf';
{$endif}
{$else}
//  MapFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elongata' + PathDelim + 'Elong_anim.txt';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elongata' + PathDelim + 'gltf' + PathDelim + 'ElongataGreen.glb';
//  MapFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elf-Males' + PathDelim + 'elfrangers-aniamtions-list.txt';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Elf-Males' + PathDelim + 'FBX 2013' + PathDelim + 'Elf-03.glb';
//  MapFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Goblins-Undead' + PathDelim + 'goblin_anim.txt';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Goblins-Undead' + PathDelim + 'gltf' + PathDelim + 'Goblin1b-Axe.glb';
//  MapFile := FSPrefix + 'Assets' + PathDelim + 'JoseDiaz' + PathDelim + 'dog' + PathDelim + 'German_Shepherd_new' + PathDelim + 'German Shepherd Animation Ranges.txt';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + 'JoseDiaz' + PathDelim + 'dog' + PathDelim + 'german_shepherd' + PathDelim + 'scene.gltf';
  MapFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'chibii-racers-dirt-bikes' + PathDelim + '_bike_animations.txt';
  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'chibii-racers-dirt-bikes' + PathDelim + 'gitf' + PathDelim + 'dirt_bike01.gltf';
//  MapFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Spiders' + PathDelim + 'spider_anim.txt';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Spiders' + PathDelim + 'gltf' + PathDelim + 'spider1.glb';
//  MapFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Dragon-boss' + PathDelim + 'dragonboss_animation_list.txt';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + '3drt' + PathDelim + 'paid' + PathDelim + 'Dragon-boss' + PathDelim + 'DragonBoss.glb';
//  MapFile := FSPrefix  + 'Assets' + PathDelim + '3drt' + PathDelim + 'gltf' + PathDelim + 'Thief' + PathDelim + 'Thief-animations-list.txt';
//  ModelFile := FSPrefix  + 'Assets' + PathDelim + '3drt' + PathDelim + 'gltf' + PathDelim + 'Thief' + PathDelim + 'thief_torch.glb';
//  ModelFile := FSPrefix + 'Assets' + PathDelim + 'TurboSquid' + PathDelim + 'Wyvern' + PathDelim + 'GreenDragon.glb';
{$endif}
//  LogShaders := true;

  LogHandler := TLogHandler.Create(Application);

  ApplicationProperties.ApplicationName := 'Spritely';
  ApplicationProperties.Caption := 'Spritely';
  ApplicationProperties.Version := '0.1';
//  ApplicationProperties.LimitFPS := 30;
  ApplicationProperties.OnLog.Add(@LogHandler.LogCallback);

  InitializeLog;

  TabControl1.TabIndex := 0;
  RenderPanel.Visible := True;
  LogPanel.Visible := False;

  ModeOrientation := True;
  Applog.Clear;

  WriteLnLog('FormCreate : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  AppTime := CastleGetTickCount64;
  PrepDone := False;
  Caption := 'Spritely';
  Tracking := False;
  Button1.Caption := 'Create Sprite';
  Button2.Caption := 'Change ViewMode';
end;

procedure TCastleForm.DebugBoxMenuClick(Sender: TObject);
begin
  with CastleApp.WorkingModel.Debug do
    begin
      Exists := not Exists;
      DebugBoxMenu.Checked := Exists;
    end;
end;

procedure TCastleForm.CreateSpriteMenuClick(Sender: TObject);
begin
  Button1Click(Sender);
end;

procedure TCastleForm.RenderPanelResize(Sender: TObject);
begin
  if not(CastleApp = nil) then
    begin
      RenderPanel.Constraints.MinHeight := 240;
      RenderPanel.Constraints.MinWidth := CastleApp.ControlWidth + CastleApp.MinSpritePanelWidth;
    end;
end;

procedure TCastleForm.SelectDirectoryMenuItemClick(Sender: TObject);
var
  ProcTimer: Int64;
  cnt: Cardinal;
  ProcMsg: String;
  ScanDir: String;
begin
  WriteLnLog('ApplicationConfig = ' + ApplicationConfig(''));
  WriteLnLog('ApplicationData = ' + ApplicationData(''));

  cnt := 0;
  ProcTimer := CastleGetTickCount64;
  CallCounter := 0;

  SelectDirectoryDialog1.InitialDir := ScanModelDir;
  if SelectDirectoryDialog1.Execute then
    begin
      ScanDir := SelectDirectoryDialog1.FileName;
      WriteLnLog('Scanning Path : ' + URIToFilenameSafe(ScanDir));
      try
//        cnt := FindFiles(ScanDir, '*', True, @FindFilesFunction, @FoundUUIDList, []);
      except
        on E : Exception do
          begin
            WriteLnLog('Something went wrong in SelectSpriteDirectoryClick' + LineEnding + E.ClassName + LineEnding + E.Message);
          end;
      end;
    end;

  ProcTimer := CastleGetTickCount64 - ProcTimer;
  ProcMsg := 'Scan of ' + IntToStr(cnt) + ' took ' + FormatFloat('####0.000000', ProcTimer / 1000) + ' seconds';
  WriteLnLog(ProcMsg);

//  if SelectDirectoryDialog1.Execute then
//    begin
      // FileToLoadList.Add();
      // AddGuiModel(URIToFilenameSafe(CastleOpenDialog1.Filename));
      // Caption := 'Spritely : ' + CastleOpenDialog1.Filename;
//    end;
end;

procedure TCastleForm.TabControl1Change(Sender: TObject);
begin
  RenderPanel.Visible := False;
  LogPanel.Visible := False;

  if TabControl1.TabIndex = 0 then
    begin
      TUIState.Current := CastleApp;
      RenderPanel.Visible := True;
      ActiveControl := Window;
    end
  else if TabControl1.TabIndex = 1 then
    begin
      TUIState.Current := SheetViewer;
      RenderPanel.Visible := True;
      ActiveControl := Window;
    end
  else if TabControl1.TabIndex = 2 then
    begin
      LogPanel.Visible := True;
      AppLog.VertScrollBar.Position := 99999;
    end;

end;

procedure TCastleForm.FormDestroy(Sender: TObject);
begin
  WriteLnLog('FormDestroy : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

function TCastleForm.SyncModelFromNode(const Node: Pointer): TCastleModel;
var
  ClickedModel: TCastleModel;
begin
  Result := nil;

  ClickedModel := TCastleModel(Node);
  With CastleApp do
    begin
      if not(ClickedModel = WorkingModel) then
        begin
          Stage.Remove(WorkingModel.Scene);
          WorkingModel := ClickedModel;
          Stage.Add(WorkingModel.Scene);
        end;
      Result := ClickedModel;
    end;
end;

function TCastleForm.IdentifyNode(const Node: TTreeNode): Cardinal;
var
  NodeLevel: Cardinal;
  ParentNode: TTreeNode;
begin
  NodeLevel:= 0;
  if not(Node.Parent = nil) then
    begin
      ParentNode := Node.Parent;
      while not(ParentNode = nil) do
        begin
          Inc(NodeLevel);
          if NodeLevel > 20 then
            begin
              raise Exception.Create('NodeIdentify too deep : ' + IntToStr(NodeLevel));
            end;
          ParentNode := ParentNode.Parent;
        end;
    end;
  Result := NodeLevel;
end;

procedure TCastleForm.TreeView1Click(Sender: TObject);
var
  Node: TTreeNode;
  NodeParent: TTreeNode;
begin
  Node := Treeview1.Selected;
  if (Node = nil) then // Nothing to do
    exit;

  WriteLnLog('*** Node Level : ' + IntToStr(IdentifyNode(Node)));

  NodeParent := Node.Parent;
  if (NodeParent = nil) then // User clicked on a root node
    begin
      if (TObject(Node.Data).ClassName = 'TCastleModel') then
        begin
          WriteLnLog('Branch Level 1');
          With CastleApp do
            begin
              WorkingModel := SyncModelFromNode(Node.Data);
              WorkingModel.ResetAnimationState;
            end;
        end;
    end
  else if (NodeParent.Parent = nil) then // User clicked on 2nd level
      begin
        if ((TObject(Node.Data).ClassName = 'TAnimationInfo') and
            (TObject(NodeParent.Data).ClassName = 'TCastleModel')) then
          begin
            WriteLnLog('Branch Level 2');
            With CastleApp do
              begin
                WorkingModel := SyncModelFromNode(NodeParent.Data);
                WorkingModel.BaseRotation := Vector3(0, 0, 0);
                WorkingModel.SelectAnimation(Node.Text);
              end;
          end
      end
  else if (NodeParent.Parent.Parent = nil) then // User clicked on 3rd level
    begin
      if ((TObject(Node.Data).ClassName = 'TAnimationInfo') and
               (TObject(NodeParent.Data).ClassName = 'TAnimationInfo') and
               (TObject(NodeParent.Parent.Data).ClassName = 'TCastleModel')) then
        begin
          WriteLnLog('Branch Level 3');
          With CastleApp do
            begin
              WorkingModel := SyncModelFromNode(NodeParent.Parent.Data);
              WorkingModel.BaseRotation := Vector3(0, 0, 0);
              WorkingModel.SelectAnimation(Node.Text);
            end;
        end;
    end;
end;

procedure TCastleForm.TreeView1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Node: TTreeNode;
  AnimNode: TAnimationInfo;
  ModelNode: TCastleModel;
begin
//  Exit;

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
          if not(AnimNode.IsMapped) and (TObject(Node.Parent.Data).ClassName = 'TCastleModel') then
            begin
              CastleApp.WorkingModel := SyncModelFromNode(Node.Parent.Data);
              MapAnims(Node, AnimNode);
            end;
        end
      else if(TObject(Node.Data).ClassName = 'TCastleModel') then
        begin
          ModelNode := TCastleModel(Node.Data);
          WriteLnLog('Context Clicked ' + ModelNode.ModelName);
        end;
    end;
end;

procedure TCastleForm.TreeView1EditingEnd(Sender: TObject; Node: TTreeNode;
  Cancel: Boolean);
begin
  if not(Cancel) then
    begin
      WriteLnLog('Commited TreeView1EditingEnd : ' + Node.Text)
    end;
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
  newNode: TTreeNode;
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
              newNode := Treeview1.Items.AddChildObject(modelNode, Anis[I].TakeName, NewAnimNode);
              newNode.ImageIndex := 4;
              newNode.SelectedIndex := 4;
            end;
        end;
      SetLength(Anis, 0);
      Json.Free;
      AnimNode.IsMapped := True;
      modelNode.Expand(False);
      modelNode.ImageIndex := 1;
      modelNode.SelectedIndex := 1;
    end;
end;

procedure TCastleForm.GuiBootStrap;
begin
//  Treeview1.Items.Clear;
  LoadGuiModel(ModelFile{$ifdef remotefile}, True{$endif});
end;

procedure TCastleForm.AddModelToTree(const AModel: TCastleModel; const doExpand: Boolean);
var
  I: Integer;
  modelNode: TTreeNode;
  newNode: TTreeNode;
begin
  if not(AModel = nil) then
    begin
      modelNode := Treeview1.Items.AddObject(nil, StripExtension(ExtractURIName(AModel.ModelName)), AModel);
      if AModel.HasAnimations then
        begin
        for I := 0 to AModel.Actions.Count - 1 do
          begin
            newNode := Treeview1.Items.AddChildObject(modelNode, AModel.Actions[I], AModel.Animations[I]);
            newNode.ImageIndex := 4;
            newNode.SelectedIndex := 4;
          end;
        if doExpand then
          modelNode.Expand(False);
        modelNode.ImageIndex := 2;
        modelNode.SelectedIndex := 2;
        end
      else
        begin
          modelNode.ImageIndex := 3;
          modelNode.SelectedIndex := 3;
        end;
      modelNode.Selected := True;
      AModel.ResetAnimationState;
    end;
end;

procedure TCastleForm.LoadGuiModel(const AModel: String; const isRemote: Boolean = False);
begin
  if not(isRemote) and not(URIFileExists(AModel)) then
    begin
      WriteLnLog('Tried to open non-existant file ' + AModel);
      Exit;
    end;

  if not(TUIState.CurrentTop = CastleOverlay) then
    begin
      TUIState.Push(CastleOverlay);
      CastleOverlay.AddNote('Adding Models...');
      WriteLnLog('Push UI');
    end;

  with CastleApp do
    begin
      if FileToLoadList.Count = 0 then
        begin
          if not(isRemote) then
            FileToLoadList.Add(URIToFilenameSafe(AModel))
          else
            begin
            FileToLoadList.Add(HTTPEncode(AModel));
            WriteLnLog('REQ : ' + AModel);
            WriteLnLog('URL : ' + HTTPEncode(AModel));
            end;
          WaitForRenderAndCall(@LoadModel);
        end
      else
        FileToLoadList.Add(URIToFilenameSafe(AModel));
//      LoadModel(URIToFilenameSafe(AModel));
      Tracking := True;
    end;
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
  Button1.Enabled := True;
end;

procedure TCastleForm.Button2Click(Sender: TObject);
begin
  with CastleApp do
    begin
      ViewMode := ViewMode + 1;
    end;
end;

procedure TCastleForm.ECSlider1Change(Sender: TObject);
begin
  CastleApp.WorkingModel.Scene.TimePlayingSpeed := ECSlider1.Position;
end;

procedure TCastleForm.FormResize(Sender: TObject);
begin
  PanelLeftBottom.Height := ECSlider1.Height + Button1.Height + Button2.Height;
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
  CastleApp := TCastleApp.Create(Window);
  SheetViewer := TSheetViewer.Create(Window);
  TUIState.Current := CastleApp;
  Window.Container.UIScaling := usNone;
  CastleApp.Resize;
end;

procedure TCastleForm.WindowClose(Sender: TObject);
begin
  TUIState.Current := nil;
  CastleApp.Free;
  WriteLnLog('WindowClose : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleForm.WindowMotion(Sender: TObject; const Event: TInputMotion);
begin
  if not(ActiveControl = Window) then
    Window.SetFocus;
end;

procedure TCastleForm.WindowPress(Sender: TObject;
  const Event: TInputPressRelease);
begin
  if Event.Key = keySpace then
    begin
      if not(CastleApp.WorkingModel.CurrentAnimation = -1) then
        begin
          CastleApp.WorkingModel.Pause;
        end;
    end;

  if Event.Key = keyR then
    begin
      if not(CastleApp.WorkingModel = nil) then
        begin
          CastleApp.ModelRotationCheck := not CastleApp.ModelRotationCheck;
          CastleApp.ModelRotationDone := False;
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

end.

