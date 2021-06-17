unit MiscFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, X3DNodes, CastleImages, CastleVectors, CastleUtils;

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


implementation

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

  Light.DefaultShadowMap := TGeneratedShadowMapNode.Create;
  Light.DefaultShadowMap.Update := upAlways;
  Light.DefaultShadowMap.Size := 4096;
  Light.ShadowVolumesMain := False;
  Light.ShadowVolumes := False;
{
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

function MakeTransparentLayerGrid(const ACellSize: Cardinal; const AWidth: Cardinal; const AHeight: Cardinal): TRGBImage;
var
  img: TRGBImage;
begin
  img := TRGBImage.Create(AWidth, AHeight);
  img.Clear(Vector4Byte(255, 255, 255, 255));
  img.FillRectangle(0, 0, ACellSize - 1, ACellSize - 1, Vector4(0.75, 0.75, 0.75, 1.00));
  Result := img;
end;


end.

