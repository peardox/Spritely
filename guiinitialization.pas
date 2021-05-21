unit GUIInitialization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, CastleUIState, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, CastleControl, MainGameUnit, CastleControls,
  CastleColors, CastleUIControls, CastleTriangles, CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform, CastleViewport, CastleCameras,
  X3DNodes, X3DFields, X3DTIme, CastleImages, CastleGLImages, CastleFilesUtils,
  CastleURIUtils, MiscFunctions,
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
    TrackBar1: TTrackBar;
    TreeView1: TTreeView;
    Window: TCastleControlBase;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure WindowClose(Sender: TObject);
    procedure WindowOpen(Sender: TObject);
  public
    procedure GuiBootStrap;
  end;

var
  CastleForm: TCastleForm;

implementation
{$R *.lfm}

procedure TCastleForm.FormCreate(Sender: TObject);
begin
  InitializeLog;
  WriteLnLog('FormCreate : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  {$ifdef darwin}
  WindowState := wsFullScreen;
  {$endif}
  AppTime := CastleGetTickCount64;
  PrepDone := False;
  Caption := 'Spritely GUI';
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
begin
  with CastleApp do
    begin
      LoadModel('C:\src\Spritely\data\Quaternius\RPGCharacters\Wizard.glb');
      // LoadModel('castle-data:/Quaternius/RPGCharacters/Wizard.glb');
      if not(TestModel = nil) then
        begin
          model := Treeview1.Items.Add(nil, StripExtension(ExtractURIName(TestModel.ModelName)));
          for I := 0 to TestModel.Actions.Count - 1 do
            begin
              Treeview1.Items.AddChild(model, TestModel.Actions[I]);
            end;
          ShowModel(TestModel);
//          TestModel.SelectAnimation('Run');
//          TestModel.Start('Run');
          TestModel.ResetAnimationState;

          model.Expand(False);
        end;
    end;
end;

procedure TCastleForm.Button1Click(Sender: TObject);
var
  Sprite: TCastleImage;
  SName: String;
begin
  if not(CastleApp.TestModel.CurrentAnimation = -1) then
    begin
{
      if CastleApp.TestModel.IsPaused then
        CastleApp.TestModel.Start
      else
}
      CastleApp.TestModel.Pause;
    end;
  Exit;

  if not (CastleApp.TestModel.Scene = nil) then
    begin
      Sprite := CastleApp.CreateSpriteImage(CastleApp.TestModel.Scene, 1024, 1024);
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
  TCastleControlBase.MainControl := Window;
  CastleApp := TCastleApp.Create(Application);
  TUIState.Current := CastleApp;
  Window.Container.UIScaling := usNone;
end;

procedure TCastleForm.WindowClose(Sender: TObject);
begin
  WriteLnLog('WindowClose : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

end.

