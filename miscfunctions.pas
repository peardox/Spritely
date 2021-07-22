unit MiscFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, X3DNodes, CastleImages, CastleVectors,
  CastleUIState, CastleControls, CastleUIControls, CastleColors,
  CastleLog, CastleNotifications, CastleUtils, CastleTimeUtils,
  RGBAlphaImageHelp;

type
  TControlHelper = class helper for TCastleUserInterface
  public
    procedure CreateButton(var obj: TCastleButton; const AText: String; const AProc: TNotifyEvent = nil; const AImage: String = '');
    procedure CreateCheckbox(var obj: TCastleCheckbox; const AText: String; const AProc: TNotifyEvent = nil);
//    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True; RightAlign: Boolean = False);
  end;

  { TUIStateHelper }
  TUIStateHelper = class helper for TUIState
  public
    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True; RightAlign: Boolean = False);
  end;

function StripExtension(S: String): String;
function WrapRadians(const AValue: Single): Single;
function RadsToFace(const AValue: Single): Cardinal;

function CreateSpotLight: TSpotLightNode;
function CreateSpotLight(const APosition: TVector3): TSpotLightNode;
function CreateSpotLight(const APosition: TVector3; const ADirection: TVector3): TSpotLightNode;
function CreatePointLight: TPointLightNode;
function CreateDirectionalLight: TDirectionalLightNode;

function CreateColorPlane(const imWidth: Single = 1.0; const imHeight: Single = 1.0; const LayerDepth: Single = 0): TTransformNode;
function CreateColorPlane(const imWidth: Single; const imHeight: Single; const LayerDepth: Single; const AColor: TVector3): TTransformNode;
function MakeTransparentLayerGrid(const ASpriteWidth: Cardinal; const ASpriteHeight: Cardinal; const AViewWidth: Cardinal; const AViewHeight: Cardinal; const GridSize: Cardinal = 8): TCastleImage;
function MakeTransparentLayerRectGrid(const ASpriteWidth: Cardinal; const ASpriteHeight: Cardinal; const AViewWidth: Cardinal; const AViewHeight: Cardinal; const GridSize: Cardinal = 8; const GridSizeY: Cardinal = 0): TCastleImage;
function HTTPEncode(const AStr: string): string;

implementation
  uses MainGameUnit;

procedure TControlHelper.CreateButton(var obj: TCastleButton; const AText: String; const AProc: TNotifyEvent = nil; const AImage: String = '');
begin
  obj := TCastleButton.Create(Self);
  obj.Caption := AText;
  obj.AutoSize := False;
  obj.HorizontalAnchorDelta := 0;
  obj.VerticalAnchorDelta := 0;
  obj.onClick := AProc;
  if not(AImage = EmptyStr) then
    obj.Image.URL := AImage;
  InsertFront(obj);
end;

procedure TControlHelper.CreateCheckbox(var obj: TCastleCheckbox; const AText: String; const AProc: TNotifyEvent = nil);
begin
  obj := TCastleCheckBox.Create(Self);
  obj.Caption := AText;
  obj.AutoSize := False;
  obj.HorizontalAnchorDelta := 0;
  obj.VerticalAnchorDelta := 0;
  obj.onChange := AProc;
  InsertFront(obj);
end;

procedure TUIStateHelper.CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True; RightAlign: Boolean = False);
begin
  objLabel := TCastleLabel.Create(Self);
  objLabel.Padding := 5;
  objLabel.Color := White;
  objLabel.Frame := True;
  objLabel.FrameColor := Black;
  objLabel.Anchor(hpLeft, 10);
  if RightAlign then
    objLabel.Anchor(hpRight, -10)
  else
    objLabel.Anchor(hpLeft, 10);
  if BottomUp then
    objLabel.Anchor(vpBottom, 10 + (Line * 35))
  else
    objLabel.Anchor(vpTop, -(10 + (Line * 35)));
  InsertFront(objLabel);
end;

function CreateColorPlane(const imWidth: Single = 1.0; const imHeight: Single = 1.0; const LayerDepth: Single = 0): TTransformNode;
begin
  Result := CreateColorPlane(imWidth, imHeight, LayerDepth, Vector3(1, 1, 1));
end;

function CreateColorPlane(const imWidth: Single; const imHeight: Single; const LayerDepth: Single; const AColor: TVector3): TTransformNode;
var
  Shape: TShapeNode;
  Geometry: TIndexedFaceSetNode;
  Coordinate: TCoordinateNode;
  TextureCoordinate: TTextureCoordinateNode;
  MaterialNode: TMaterialNode;
begin
  MaterialNode := TMaterialNode.Create;
  MaterialNode.DiffuseColor := AColor;
  MaterialNode.AmbientIntensity := 1;
  MaterialNode.Shininess := 1;

  { Create Coordinate node (position of quad in 3D) }
  Coordinate := TCoordinateNode.Create;
  Coordinate.SetPoint([
    Vector3(-imWidth / 2, LayerDepth, -imHeight / 2),
    Vector3( imWidth / 2, LayerDepth, -imHeight / 2),
    Vector3( imWidth / 2, LayerDepth,  imHeight / 2),
    Vector3(-imWidth / 2, LayerDepth,  imHeight / 2)
  ]);

  { Create TextureCoordinate node (how the image is mapped onto a surface) }
  TextureCoordinate := TTextureCoordinateNode.Create;
  TextureCoordinate.SetPoint([
    Vector2(0, 0),
    Vector2(1, 0),
    Vector2(1, 1),
    Vector2(0, 1)
  ]);

  { Create Shape and IndexedFaceSet node (mesh with coordinates, texture coordinates) }
  Geometry := TIndexedFaceSetNode.CreateWithShape(Shape);
  Geometry.Coord := Coordinate;
  Geometry.TexCoord := TextureCoordinate;
  Geometry.Solid := false; // to see it from any side
  Geometry.SetCoordIndex([0, 1, 2, 3]);

  { Create Appearance (refers to a texture, connects the Texture to Shape) }
  Shape.Appearance := TAppearanceNode.Create;
  Shape.Appearance.AlphaChannel := acBlending;
  Shape.Appearance.ShadowCaster := false;
  Shape.Appearance.Material := MaterialNode;
  Result := TTransformNode.Create;
  Result.AddChildren(Shape);
end;

function CreateDirectionalLight: TDirectionalLightNode;
var
  Light: TDirectionalLightNode;
begin
  Light := TDirectionalLightNode.Create;
  Light.Direction := Vector3(-0.5, -1.0, -0.5);
  Light.Color := Vector3(1, 1, 1);
  Light.Intensity := 1;
  Light.FdOn.Value := true;
  Light.projectionNear := 1.00;
  Light.projectionFar := 40.00;

  Light.Global := true;
  {
  Light.DefaultShadowMap := TGeneratedShadowMapNode.Create;
  Light.DefaultShadowMap.Update := upAlways;
  Light.DefaultShadowMap.Size := 4096;
  Light.ShadowVolumesMain := False;
  Light.ShadowVolumes := False;

  Light.ProjectionRectangle := FloatRectangle(-8.0, -16.0, 32.0, 32.0).ToX3DVector;
  Light.ProjectionLocation := Vector3(-11.0, 12.0, 1.0);
}
  {$ifndef darwin}
  Light.Shadows := true;
  {$endif}

  Result := Light;
end;

function CreatePointLight: TPointLightNode;
var
  Light: TPointLightNode;
begin
  Light := TPointLightNode.Create;

  Light.Location := Vector3(5.0, 30.0, 30.0);
  Light.Color := Vector3(1, 1, 1);
  Light.FdOn.Value := true;
  Light.Intensity := 1;
  Light.Global := True;
  Light.Radius := -1;

  Result := Light;
end;

function CreateSpotLight: TSpotLightNode;
begin
  Result := CreateSpotLight(Vector3(0.0, 1.0, 3.0));
end;

function CreateSpotLight(const APosition: TVector3): TSpotLightNode;
begin
  Result := CreateSpotLight(APosition, Vector3(0.0, 0.0, -1.0));
end;

function CreateSpotLight(const APosition: TVector3; const ADirection: TVector3): TSpotLightNode;
var
  Light: TSpotLightNode;
begin
  Light := TSpotLightNode.Create;

  Light.Location := APosition;
  Light.Direction := ADirection;
  Light.Color := Vector3(1, 1, 1);
  Light.FdOn.Value := true;
  Light.Intensity := 1;
  Light.Radius := -1;

  Result := Light;
end;


function WrapRadians(const AValue: Single): Single;
begin
  Result := FloatModulo(AValue + Pi, 2 * Pi) - Pi;
end;

function RadsToFace(const AValue: Single): Cardinal;
begin
  Result := Round(FloatModulo(AValue, 2 * Pi) / (Pi / 2)) Mod 4;
end;

function StripExtension(S: String): String;
var
  I: SizeInt;
begin
  Result := S;
  I := S.IndexOf('.');
  if(I >= 0) then
    Result := S.Remove(I);
end;

function HTTPEncode(const AStr: string): string;
const
  NoConversion = ['A'..'Z', 'a'..'z', '0'..'9', '*', '@', '.', '_', '-', ':', '/'];
var
  Sp, Rp: PChar;
begin
  SetLength(Result, Length(AStr) * 3);
  Sp := PChar(AStr);
  Rp := PChar(Result);
  while Sp^ <> #0 do
  begin
    if Sp^ in NoConversion then
      Rp^ := Sp^
//    else if Sp^ = ' ' then
//      Rp^ := '+'
    else
    begin
      FormatBuf(Rp^, 3, '%%%.2x', 6, [Ord(Sp^)]);
      Inc(Rp, 2);
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PChar(Result));
end;

// TCastleImageControl DG - 838383, LG B2B2B2

function MakeTransparentLayerGrid(const ASpriteWidth: Cardinal; const ASpriteHeight: Cardinal; const AViewWidth: Cardinal; const AViewHeight: Cardinal; const GridSize: Cardinal = 8): TCastleImage;
var
  img: TRGBAlphaImage;
  XPos: Cardinal;
  YPos: Cardinal;
  XGrid: Single;
  YGrid: Single;
  Skip: Integer;
  LightGrey: TVector4Byte;
begin
  if((AViewWidth < GridSize) or (AViewHeight < GridSize)) then
    Exit;

  img := TRGBAlphaImage.Create(AViewWidth, AViewHeight);

  LightGrey := Vector4Byte($B2, $B2, $B2, $FF); // Vector4Byte(255, 0, 0, 255);
  img.Clear(HexToColor('838383'));

  XGrid := AViewWidth / ASpriteWidth;
  YGrid := AViewHeight / ASpriteHeight;

  if (((GridSize * XGrid) < 1) or ((GridSize * YGrid) < 1)) then
    begin
      img.Clear(HexToColor('000000'));
    end
  else
    begin
      for YPos := 0 to (ASpriteHeight div GridSize) - 1 do
        begin
          Skip := YPos Mod 2;
          for XPos := 0 to (ASpriteWidth div GridSize) - 1 do
            begin
              if (((Skip + XPos) Mod 2) = 0) then
                begin
                  img.FastFillRect(Trunc(XPos * GridSize * XGrid), Trunc(YPos * GridSize * YGrid),
                    Trunc(((XPos + 1) * GridSize * XGrid)) - 1, Trunc(((YPos + 1) * GridSize * YGrid)) - 1,
                    LightGrey);
                end;
            end;
        end;
    end;

  Result := img;
end;

function MakeTransparentLayerRectGrid(const ASpriteWidth: Cardinal; const ASpriteHeight: Cardinal; const AViewWidth: Cardinal; const AViewHeight: Cardinal; const GridSize: Cardinal = 8; const GridSizeY: Cardinal = 0): TCastleImage;
var
  img: TRGBAlphaImage;
  XPos: Cardinal;
  YPos: Cardinal;
  XGrid: Single;
  YGrid: Single;
  GridWidth: Cardinal;
  GridHeight: Cardinal;
  Skip: Integer;
  LightGrey: TVector4Byte;
begin
  GridWidth := GridSize;

  if GridSizeY = 0 then
    GridHeight := GridSize
  else
    GridHeight := GridSizeY;

  if((AViewWidth < GridWidth) or (AViewHeight < GridHeight)) then
    Exit;

  img := TRGBAlphaImage.Create(AViewWidth, AViewHeight);

  LightGrey := Vector4Byte($B2, $B2, $B2, $FF); // Vector4Byte(255, 0, 0, 255);
  img.Clear(HexToColor('838383'));

  XGrid := AViewWidth / ASpriteWidth;
  YGrid := AViewHeight / ASpriteHeight;

  if (((GridWidth * XGrid) < 1) or ((GridHeight * YGrid) < 1)) then
    begin
      img.Clear(HexToColor('000000'));
    end
  else
    begin
      for YPos := 0 to (ASpriteHeight div GridHeight) - 1 do
        begin
          Skip := YPos Mod 2;
          for XPos := 0 to (ASpriteWidth div GridWidth) - 1 do
            begin
              if (((Skip + XPos) Mod 2) = 0) then
                begin
                  img.FastFillRect(Trunc(XPos * GridWidth * XGrid), Trunc(YPos * GridHeight * YGrid),
                    Trunc(((XPos + 1) * GridWidth * XGrid)) - 1, Trunc(((YPos + 1) * GridHeight * YGrid)) - 1,
                    LightGrey);
                end;
            end;
        end;
    end;

  Result := img;
end;


end.

