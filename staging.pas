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
    fGroundModelRoot: TX3DRootNode;
  public
    GroundTexture: String;
    procedure LoadStage(const GroundModel: String; const GroundLevel: Single = 0);
    procedure LoadStage(const GroundLevel: Single = 0);
    procedure LoadStage(const GroundLevel: Single; const GroundColor: TVector3);
    procedure LoadStage(const GroundLevel: Single; const GroundColor: TVector3; const GroundModel: String);
    property  GroundTransformNode: TTransformNode read fGroundTransformNode write fGroundTransformNode;
    procedure ChangeTextureCoordinates(const Model3D: TX3DRootNode; const AScale: Single = 1.0);
    function  ChangeTexture(const ANode: TX3DRootNode; const TextureUrl: String): TVector3Cardinal;
    function  CreateGroundPlane(AFileName: String; const AScale: Single = 1.0): TX3DRootNode;

    property  LightNode: TDirectionalLightNode read fLightNode write fLightNode;
    property  GroundModelRoot: TX3DRootNode read fGroundModelRoot;
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
  Light.Direction := Vector3(-0.5, -1.0, 0.5);
  Light.Color := Vector3(1, 1, 1);
  Light.Intensity := 1;
  Light.FdOn.Value := true;
  Light.Global := true;
  Light.Shadows := true;
  Light.projectionNear := 1.00;
  Light.projectionFar := 40.00;

  Light.DefaultShadowMap := TGeneratedShadowMapNode.Create;
  Light.DefaultShadowMap.Update := upAlways;
  Light.DefaultShadowMap.Size := 4096;
  Light.ShadowVolumesMain := False;
  Light.ShadowVolumes := False;
  Light.ProjectionRectangle := FloatRectangle(-8.0, -16.0, 32.0, 32.0).ToX3DVector;
  Light.ProjectionLocation := Vector3(-11.0, 12.0, 1.0);

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
  StageRootNode: TX3DRootNode;
begin
  try
    StageRootNode := TX3DRootNode.Create;
    if (GroundModel = EmptyStr) or not(URIFileExists(GroundModel)) then
      fGroundTransformNode := CreateColorPlane(20, 20, GroundLevel, GroundColor)
    else
      begin
        fGroundModelRoot := CreateGroundPlane(GroundModel, 5);
        fGroundTransformNode := TTransformNode.Create;
        fGroundTransformNode.Translation := Vector3(0, GroundLevel, 0);
        fGroundTransformNode.Scale := Vector3(20, 1, 20);
        fGroundTransformNode.AddChildren(fGroundModelRoot);
      end;
    fGroundTransformNode.X3DName := 'GroundTransformNode';

    fLightNode := CreateDirectionalLight;
    fLightNode.X3DName := 'LightNode';

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

procedure TCastleStage.ChangeTextureCoordinates(const Model3D: TX3DRootNode; const AScale: Single = 1.0);
var
  TextureCoordinateNode: TTextureCoordinateNode;
  NewCoords: array[0..3] of TVector2;
begin
  if (AScale = 0) then
    begin
      WriteLnLog('Trying to change ground texture scale to zero - ignoring');
      Exit;
    end;

  // Find the texture
  TextureCoordinateNode := Model3D.TryFindNodeByName(TTextureCoordinateNode, 'ObjFrontTextureCoordinates', false) as TTextureCoordinateNode;
  if not (TextureCoordinateNode = nil) then
    begin
      // Set new texture coordinates
      NewCoords[0] := Vector2(AScale, 0);
      NewCoords[1] := Vector2(AScale, AScale);
      NewCoords[2] := Vector2(0, AScale);
      NewCoords[3] := Vector2(0, 0);

      TextureCoordinateNode.SetPoint(NewCoords);
    end
  else
    WriteLnLog('Failed to change ground texture scale to ' + FloatToStr(AScale));
end;

function TCastleStage.ChangeTexture(const ANode: TX3DRootNode; const TextureUrl: String): TVector3Cardinal;
var
  TextureNode: TImageTextureNode;
begin
  Result := TVector3Cardinal.Zero;
  TextureNode := ANode.TryFindNodeByName(TImageTextureNode, 'ObjFrontTexture', false) as TImageTextureNode;
  if not (TextureNode = nil) then
  begin
    TextureNode.SetUrl(TextureUrl);
    if TextureNode.IsTextureImage then
      begin
        Result := TextureNode.TextureImage.Dimensions;
        GroundTexture := TextureUrl;
      end;
  end
  else
    WriteLnLog('Failed to change ground texture to ' + TextureUrl);
end;

function TCastleStage.CreateGroundPlane(AFileName: String; const AScale: Single = 1.0): TX3DRootNode;
var
  PlaneNode: TX3DRootNode;
begin
  PlaneNode := LoadNode('castle-data:/ground/plane.x3dv');
  ChangeTexture(PlaneNode, AFileName);
  ChangeTextureCoordinates(PlaneNode, AScale);
  Result := PlaneNode;
end;

end.

