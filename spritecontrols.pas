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
    function  GetSpinValue: Integer;
    procedure SetSpinValue(AValue: Integer);
    function  GetSpinMinValue: Integer;
    procedure SetSpinMinValue(AValue: Integer);
    function  GetSpinMaxValue: Integer;
    procedure SetSpinMaxValue(AValue: Integer);
    property  Value: Integer read GetSpinValue write SetSpinValue;
    property  Min: Integer read GetSpinMinValue write SetSpinMinValue;
    property  Max: Integer read GetSpinMaxValue write SetSpinMaxValue;
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

function  TCastleIntegerSpinEdit.GetSpinValue: Integer;
begin
  Result := SpinNumber.Value;
end;

procedure TCastleIntegerSpinEdit.SetSpinValue(AValue: Integer);
begin
  if not(AValue = SpinNumber.Value) then
    SpinNumber.Value := AValue;
end;

function  TCastleIntegerSpinEdit.GetSpinMinValue: Integer;
begin
  Result := SpinNumber.Min;
end;

procedure TCastleIntegerSpinEdit.SetSpinMinValue(AValue: Integer);
begin
  if not(AValue = SpinNumber.Min) then
    SpinNumber.Min := AValue;
end;

function  TCastleIntegerSpinEdit.GetSpinMaxValue: Integer;
begin
  Result := SpinNumber.Max;
end;

procedure TCastleIntegerSpinEdit.SetSpinMaxValue(AValue: Integer);
begin
  if not(AValue = SpinNumber.Max) then
    SpinNumber.Max := AValue;
end;


procedure TCastleIntegerSpinEdit.FocusOnMe(const Sender: TInputListener; const Event: TInputMotion; var Handled: Boolean);
begin
{$ifndef cgeapp}
  With CastleForm do
    ActiveControl := Window;
{$endif}
end;
end.

