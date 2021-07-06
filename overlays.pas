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
  CastleImages, CastleGLImages, CastleRectangles,
  CastleTextureImages, CastleCompositeImage, CastleLog,
  CastleApplicationProperties, CastleTimeUtils, CastleKeysMouse,
  CastleGLUtils, multimodel, staging, MiscFunctions;

type
  TControlPanel = class(TCastleUserInterface)
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; const AWidth: Single; const AHeight: Single);
  private
    TopSection: TCastleUserInterface;
    BottomSection: TCastleUserInterface;
    ABtn: TCastleButton;
    BBtn: TCastleButton;
    CBtn: TCastleButton;
    UseModelSpots: Boolean;
    procedure UseModelSpotsClick(Sender: TObject);
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
var
  BtnWidth: Single;
begin
  Create(AOwner);

  Width := AWidth;
  Height := AHeight;
  Anchor(hpRight);
  Anchor(vpTop);
  BorderColor := White;
  TUIState(AOwner).InsertFront(Self);

  TopSection := TCastleUserInterface.Create(Self);
  TopSection.Height := Height - 80;
  TopSection.Width := Width;
  Self.InsertFront(TopSection);

  BottomSection := TCastleUserInterface.Create(Self);
  BottomSection.Height := 80;
  BottomSection.Width := Width;
  Self.InsertFront(BottomSection);

  BtnWidth := Width / 3;

  BottomSection.CreateButton(ABtn, 'Hello', nil);
  ABtn.Left := 0;
  ABtn.Bottom := 0;
  ABtn.AutoSize := False;
  ABtn.Width := BtnWidth;
  ABtn.Height := BtnWidth;

  BottomSection.CreateButton(BBtn, 'Hello', nil);
  BBtn.Left := BtnWidth;
  BBtn.Bottom := 0;
  BBtn.AutoSize := False;
  BBtn.Width := BtnWidth;
  BBtn.Height := BtnWidth;

  BottomSection.CreateButton(CBtn, 'Hello', @UseModelSpotsClick);
  CBtn.Left := (BtnWidth * 2);
  CBtn.Bottom := 0;
  CBtn.AutoSize := False;
  CBtn.Width := BtnWidth;
  CBtn.Height := BtnWidth;
end;

procedure TControlPanel.UseModelSpotsClick(Sender: TObject);
var
  i: Integer;
begin
  With CastleApp do
    begin
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

