unit SpriteControls;

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
  TCastleIntegerSpinEdit = class(TCastleUserInterface)
    SpinRect: TCastleRectangleControl;
    SpinImage: TCastleImageControl;
    SpinGroup: TCastleHorizontalGroup;
    SpinLabel: TCastleLabel;
    SpinPlus: TCastleButton;
    SpinNumber: TCastleIntegerEdit;
    SpinMinus: TCastleButton;
  protected
    procedure FocusOnMe(const Sender: TInputListener; const Event: TInputMotion; var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; ACaption: String; const AWidth: Single; const AHeight: Single);
//    procedure Resize; override;
  end;

{
  TCastleFloatSpinEdit = class(TCastleIntegerSpinEdit)
    SpinNumber: TCastleFloatEdit;
  end;
}

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}


constructor TCastleIntegerSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoSizeToChildren := True;

  SpinRect := TCastleRectangleControl.Create(Self);
  SpinRect.AutoSizeToChildren := True;
  SpinRect.Color := Red;
  Self.InsertFront(SpinRect);

  SpinImage := TCastleImageControl.Create(SpinRect);
  SpinImage.AutoSizeToChildren := True;
  SpinRect.InsertFront(SpinImage);

  SpinGroup := TCastleHorizontalGroup.Create(SpinImage);
  SpinGroup.AutoSizeToChildren := True;
  SpinImage.InsertFront(SpinGroup);

  SpinLabel := TCastleLabel.Create(SpinGroup);

  SpinMinus := TCastleButton.Create(SpinGroup);
  SpinMinus.Caption := '-';

  SpinNumber := TCastleIntegerEdit.Create(SpinGroup);

  SpinPlus := TCastleButton.Create(SpinGroup);
  SpinPlus.Caption := '+';

  SpinGroup.InsertFront(SpinLabel);
  SpinGroup.InsertFront(SpinMinus);
  SpinGroup.InsertFront(SpinNumber);
  SpinGroup.InsertFront(SpinPlus);

  TCastleUserInterface(AOwner).InsertFront(Self);

end;

constructor TCastleIntegerSpinEdit.Create(AOwner: TComponent; ACaption: String; const AWidth: Single; const AHeight: Single);
begin
  Create(AOwner);

  SpinGroup.AutoSizeToChildren := False;
  SpinGroup.Height := AHeight;
  SpinGroup.Width := AWidth;

  SpinNumber.Height := AHeight;
  SpinNumber.Width := AHeight * 2;
  SpinNumber.Alignment := hpRight;
  SpinNumber.Enabled := True;
  SpinNumber.OnMotion := @FocusOnMe;

  SpinLabel.AutoSize := False;
  SpinLabel.Caption := ' ' + ACaption;
  SpinLabel.Height := AHeight;
  SpinLabel.VerticalAlignment := vpMiddle;

  SpinPlus.AutoSize := False;
  SpinPlus.Width := AHeight;
  SpinPlus.Height := AHeight;

  SpinMinus.AutoSize := False;
  SpinMinus.Width := AHeight;
  SpinMinus.Height := AHeight;

  SpinLabel.Height := AHeight;
  SpinLabel.Width := AWidth - SpinPlus.Width - SpinNumber.Width - SpinMinus.Width;
//  SpinMinus.Left := SpinLabel.Width;

end;

procedure TCastleIntegerSpinEdit.FocusOnMe(const Sender: TInputListener; const Event: TInputMotion; var Handled: Boolean);
begin
{$ifndef cgeapp}
  With CastleForm do
    ActiveControl := Window;
{$endif}
end;
end.

