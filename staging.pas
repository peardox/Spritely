unit Staging;

{$mode objfpc}{$H+}

interface

uses
  Classes, Math, SysUtils, CastleRectangles, CastleLog,
  CastleSceneCore, CastleScene, CastleTransform,
  CastleImages, CastleTriangles, CastleShapes, CastleVectors,
  X3DNodes, X3DFields, X3DTIme, X3DLoad, CastleRenderOptions,
  CastleViewport, CastleCameras, CastleProjection;

function LoadStage(var AScene: TCastleScene; const GroundLevel: Single = 0): TCastleScene;
function LoadStage(var AScene: TCastleScene; const GroundLevel: Single; const GroundColor: TVector3): TCastleScene;
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
  Light.Direction := Vector3( 0.00, -0.98, -0.00);
  Light.Color := Vector3(1, 1, 1);
  Light.Intensity := 1;
  Light.FdOn.Value := true;
  Light.Global := true;
  Light.Shadows := true;

  Light.DefaultShadowMap := TGeneratedShadowMapNode.Create;
  Light.DefaultShadowMap.Update := upAlways;
  Light.DefaultShadowMap.Size := 4096;
  Light.ShadowVolumesMain := False;
  Light.ShadowVolumes := False;
  Light.ProjectionRectangle := FloatRectangle(-5.0, -5.0, 2 * 5.0, 2 * 5.0).ToX3DVector;
  Light.ProjectionLocation := Vector3(1.0, 3.0, 1.0);

  Result := Light;
end;

function CreatePointLight: TPointLightNode;
var
  Light: TPointLightNode;
begin
  Light := TPointLightNode.Create;

  Light.Location := Vector3(-5.0, 100.0, -10.0);
  Light.Color := Vector3(1, 1, 1);
  Light.FdOn.Value := true;
  Light.Intensity := 1;
  Light.Global := true;
  Light.Shadows := true;
  Light.Radius := -1;
  Result := Light;
end;


function LoadStage(var AScene: TCastleScene; const GroundLevel: Single = 0): TCastleScene;
begin
  Result := LoadStage(AScene, GroundLevel, Vector3(1,1,1));
end;

function LoadStage(var AScene: TCastleScene; const GroundLevel: Single; const GroundColor: TVector3): TCastleScene;
var
  NewStage: TCastleScene;
  GroundNode: TTransformNode;
  {$ifdef usepoint}
  Light: TPointLightNode;
  {$else}
  Light: TDirectionalLightNode;
  {$endif}
  Root: TX3DRootNode;
begin
  if AScene = nil then
    begin
      Exit(nil);
    end;

  try
    NewStage := TCastleScene.Create(nil);
    Root := TX3DRootNode.Create;

    AScene.Spatial := [ssDynamicCollisions, ssRendering];
    AScene.ProcessEvents := True;

    GroundNode := CreateColorPlane(5000, 5000, Min(AScene.BoundingBox.Data[0].Y, AScene.BoundingBox.Data[1].Y) + GroundLevel, GroundColor);
    {$ifdef usepoint}
    Light := CreatePointLight;
    {$else}
    Light := CreateDirectionalLight;
    {$endif}

    Light.projectionFar := 240.00;

    Root.AddChildren(Light);
    Root.AddChildren(GroundNode);
    NewStage.Load(Root, True);
    NewStage.Add(AScene);

    NewStage.ReceiveShadowVolumes:=True;
    NewStage.Spatial := [ssDynamicCollisions, ssRendering];
    NewStage.ProcessEvents := True;
    NewStage.RenderOptions.PhongShading := true;
    NewStage.RenderOptions.ShadowSampling := ssSimple;
    NewStage.RenderOptions.BlendingSort := bs2D;

  except
    on E : Exception do
      begin
        WriteLnLog('Oops #1' + LineEnding + E.ClassName + LineEnding + E.Message);
       end;
  end;

  Result := NewStage;
end;

end.

