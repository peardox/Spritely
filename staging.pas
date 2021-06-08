unit Staging;

{$mode objfpc}{$H+}

interface
// "uri" : "Tileable Brick Ground Textures - Set 1/Ground_01_Nrm.png"
// "uri" : "Tileable Brick Ground Textures - Set 1/Ground_01.png"

uses
  Classes, Math, SysUtils, CastleRectangles, CastleLog,
  CastleSceneCore, CastleScene, CastleTransform, CastleURIUtils,
  CastleImages, CastleTriangles, CastleShapes, CastleVectors,
  X3DNodes, X3DFields, X3DTIme, X3DLoad, CastleRenderOptions,
  CastleViewport, CastleCameras, CastleProjection;

type
  TCastleStage = Class(TCastleScene)
  private
    fGroundTransformNode: TTransformNode;
    fLightNode: TDirectionalLightNode;
  public
    procedure LoadStage(const GroundModel: String; const GroundLevel: Single = 0);
    procedure LoadStage(const GroundLevel: Single = 0);
    procedure LoadStage(const GroundLevel: Single; const GroundColor: TVector3);
    procedure LoadStage(const GroundLevel: Single; const GroundColor: TVector3; const GroundModel: String);
    property  GroundTransformNode: TTransformNode read fGroundTransformNode write fGroundTransformNode;
    property  LightNode: TDirectionalLightNode read fLightNode write fLightNode;
  end;

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
  Light.Direction := Vector3( -0.500, -0.500, -0.500);
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
  Light.ProjectionRectangle := FloatRectangle(-20.0, -20.0, 40.0, 40.0).ToX3DVector;
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

procedure TCastleStage.LoadStage(const GroundModel: String; const GroundLevel: Single = 0);
begin
  LoadStage(GroundLevel, Vector3(1,1,1), GroundModel);
end;

procedure TCastleStage.LoadStage(const GroundLevel: Single = 0);
begin
  LoadStage(GroundLevel, Vector3(1,1,1));
end;

procedure TCastleStage.LoadStage(const GroundLevel: Single; const GroundColor: TVector3);
begin
  LoadStage(GroundLevel, Vector3(1,1,1), EmptyStr);
end;


procedure TCastleStage.LoadStage(const GroundLevel: Single; const GroundColor: TVector3; const GroundModel: String);
var
  GroundModelRoot: TX3DRootNode;
  StageRootNode: TX3DRootNode;
begin
  try
    StageRootNode := TX3DRootNode.Create;
    if (GroundModel = EmptyStr) or not(URIFileExists(GroundModel)) then
      fGroundTransformNode := CreateColorPlane(20, 20, GroundLevel, GroundColor)
    else
      begin
        GroundModelRoot := LoadNode(GroundModel);
        fGroundTransformNode := TTransformNode.Create;
        fGroundTransformNode.Translation := Vector3(0, GroundLevel, 0);
        fGroundTransformNode.AddChildren(GroundModelRoot);
      end;
    fGroundTransformNode.X3DName := 'GroundTransformNode';

    fLightNode := CreateDirectionalLight;
    fLightNode.X3DName := 'LightNode';

    fLightNode.projectionFar := 240.00;

    StageRootNode.AddChildren(fLightNode);
    StageRootNode.AddChildren(fGroundTransformNode);
    Load(StageRootNode, True);

    ReceiveShadowVolumes:=True;
    Spatial := [ssDynamicCollisions, ssRendering];
    ProcessEvents := True;
    RenderOptions.PhongShading := true;
    RenderOptions.ShadowSampling := ssSimple;
    RenderOptions.BlendingSort := bs2D;

  except
    on E : Exception do
      begin
        WriteLnLog('Oops #1' + LineEnding + E.ClassName + LineEnding + E.Message);
       end;
  end;
end;

end.

