unit ControlPanelControls;

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
  CastleUtils, CastleGLUtils, multimodel, staging, MiscFunctions;

type
  TCastleIntegerSpinEdit = class(TCastleUserInterface)
    SpinRect: TCastleRectangleControl;
    SpinImage: TCastleImageControl;
    SpinGroup: TCastleHorizontalGroup;
    SpinLabel: TCastleLabel;
    SpinPlus: TCastleButton;
    SpinNumber: TCastleIntegerEdit;
    SpinMinus: TCastleButton;
  private
    FOnChange: TNotifyEvent;
    SpinStepSize: Cardinal;
    function  GetSpinValue: Integer;
    procedure SetSpinValue(AValue: Integer);
    function  GetSpinMinValue: Integer;
    procedure SetSpinMinValue(AValue: Integer);
    function  GetSpinMaxValue: Integer;
    procedure SetSpinMaxValue(AValue: Integer);
    function  GetSpinStepSize: Cardinal;
    procedure SetSpinStepSize(AValue: Cardinal);
    procedure OnPlusClick(Sender: TObject);
    procedure OnMinusClick(Sender: TObject);
    procedure DoEditedChange(Sender: TObject);
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; ACaption: String; const AWidth: Single; const AHeight: Single);
    property  Value: Integer read GetSpinValue write SetSpinValue default 0;
    property  Min: Integer read GetSpinMinValue write SetSpinMinValue default 0;
    property  Max: Integer read GetSpinMaxValue write SetSpinMaxValue default 1;
    property  StepSize: Cardinal read GetSpinStepSize write SetSpinStepSize default 1;
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

  SpinStepSize := 1;

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
  SpinMinus.onClick := @OnMinusClick;

  SpinNumber := TCastleIntegerEdit.Create(SpinGroup);
  SpinNumber.OnChange := @DoEditedChange;

  SpinPlus := TCastleButton.Create(SpinGroup);
  SpinPlus.Caption := '+';
  SpinPlus.onClick := @OnPlusClick;

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
end;

function  TCastleIntegerSpinEdit.GetSpinValue: Integer;
begin
  Result := SpinNumber.Value;
end;

procedure TCastleIntegerSpinEdit.SetSpinValue(AValue: Integer);
begin
  if not(AValue = SpinNumber.Value) then
    begin
      if (AValue < Min) then
        SpinNumber.Value := Min
      else if (AValue > Max) then
        SpinNumber.Value := Max
      else
        SpinNumber.Value := AValue;
    end;
  if Assigned(OnChange) then
    OnChange(Self);
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

function  TCastleIntegerSpinEdit.GetSpinStepSize: Cardinal;
begin
  Result := SpinStepSize;
end;

procedure TCastleIntegerSpinEdit.SetSpinStepSize(AValue: Cardinal);
begin
  if not(AValue = SpinStepSize) then
    SpinStepSize := AValue;
end;

procedure TCastleIntegerSpinEdit.OnPlusClick(Sender: TObject);
begin
  Value := Value + StepSize;
end;

procedure TCastleIntegerSpinEdit.OnMinusClick(Sender: TObject);
begin
  Value := Value - StepSize;
end;

procedure TCastleIntegerSpinEdit.DoEditedChange(Sender: TObject);
var
  i: Integer;
begin
  i := TCastleIntegerEdit(Sender).Value;
  WriteLnLog('Sender : ' + Sender.ClassName + ' : ' + IntToStr(i));
  Self.Value := TCastleIntegerEdit(Sender).Value;
end;

end.

