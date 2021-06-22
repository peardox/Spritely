unit Staging;

{$mode objfpc}{$H+}
{$define pointlight}
// {$define spotlight}
interface
// "uri" : "Tileable Brick Ground Textures - Set 1/Ground_01_Nrm.png"
// "uri" : "Tileable Brick Ground Textures - Set 1/Ground_01.png"

uses
  Classes, Math, SysUtils, CastleRectangles, CastleLog,
  CastleSceneCore, CastleScene, CastleTransform, CastleURIUtils,
  CastleImages, CastleTriangles, CastleShapes, CastleVectors,
  X3DNodes, X3DFields, X3DTIme, X3DLoad, CastleRenderOptions,
  CastleViewport, CastleCameras, CastleProjection, MiscFunctions;

type
  TCastleStage = Class(TCastleScene)
  private
    fGroundTransformNode: TTransformNode;
    {$if defined(spotlight)}
    fLightNode: TSpotLightNode;
    {$elseif defined(pointlight)}
    fLightNode: TPointLightNode;
    {$else}
*    fLightNode: TDirectionalLightNode;
    {$endif}
    fShadowNode: TSpotLightNode;
    fGroundModelRoot: TX3DRootNode;
  public
    GroundTexture: String;
    procedure LoadStage(const GroundModel: String; const GroundLevel: Single = 0; const GroundScale: Single = 5;  const GroundSize: Single = 20);
    procedure LoadStage(const GroundLevel: Single = 0);
    procedure LoadStage(const GroundLevel: Single; const GroundColor: TVector3);
    procedure LoadStage(const GroundLevel: Single; const GroundColor: TVector3; const GroundModel: String; const GroundScale: Single = 5;  const GroundSize: Single = 20);
    property  GroundTransformNode: TTransformNode read fGroundTransformNode write fGroundTransformNode;
    procedure ChangeTextureCoordinates(const Model3D: TX3DRootNode; const AScale: Single = 1.0);
    function  ChangeTexture(const ANode: TX3DRootNode; const TextureUrl: String): TVector3Cardinal;
    function  CreateGroundPlane(AFileName: String; const AScale: Single = 1.0): TX3DRootNode;

    {$if defined(spotlight)}
    property  LightNode: TSpotLightNode read fLightNode write fLightNode;
    {$elseif defined(pointlight)}
    property  LightNode: TPointLightNode read fLightNode write fLightNode;
    {$else}
    property  LightNode: TDirectionalLightNode read fLightNode write fLightNode;
    {$endif}
    property  ShadowNode: TSpotLightNode read fShadowNode write fShadowNode;
    property  GroundModelRoot: TX3DRootNode read fGroundModelRoot;
  end;

implementation

procedure TCastleStage.LoadStage(const GroundModel: String; const GroundLevel: Single = 0; const GroundScale: Single = 5;  const GroundSize: Single = 20);
begin
  LoadStage(GroundLevel, Vector3(1,1,1), GroundModel, GroundScale, GroundSize);
end;

procedure TCastleStage.LoadStage(const GroundLevel: Single = 0);
begin
  LoadStage(GroundLevel, Vector3(1,1,1));
end;

procedure TCastleStage.LoadStage(const GroundLevel: Single; const GroundColor: TVector3);
begin
  LoadStage(GroundLevel, Vector3(1,1,1), EmptyStr);
end;


procedure TCastleStage.LoadStage(const GroundLevel: Single; const GroundColor: TVector3; const GroundModel: String; const GroundScale: Single = 5;  const GroundSize: Single = 20);
var
  StageRootNode: TX3DRootNode;
begin
  try
    StageRootNode := TX3DRootNode.Create;
    if (GroundModel = EmptyStr) or not(URIFileExists(GroundModel)) then
      fGroundTransformNode := CreateColorPlane(GroundSize, GroundSize, GroundLevel, GroundColor)
    else
      begin
        fGroundModelRoot := CreateGroundPlane(GroundModel, GroundScale);
        fGroundTransformNode := TTransformNode.Create;
        fGroundTransformNode.Translation := Vector3(0, GroundLevel, 0);
        fGroundTransformNode.Scale := Vector3(GroundSize, 1, GroundSize);
        fGroundTransformNode.AddChildren(fGroundModelRoot);
      end;
    fGroundTransformNode.X3DName := 'GroundTransformNode';

    {$if defined(spotlight)}
    fLightNode := CreateSpotLight(Vector3(0, 30, 30));
    fLightNode.Shadows := True;
    fLightNode.DefaultShadowMap := TGeneratedShadowMapNode.Create;
    fLightNode.DefaultShadowMap.Update := upAlways;
    fLightNode.DefaultShadowMap.Size := 4096;
    {$elseif defined(pointlight)}
    fLightNode := CreatePointLight;
    {$else}
    fLightNode := CreateDirectionalLight;
    {$endif}
    StageRootNode.AddChildren(fLightNode);
{
    fShadowNode := CreateSpotLight;
    fShadowNode.Shadows := True;
    StageRootNode.AddChildren(fShadowNode);
}
    StageRootNode.AddChildren(fGroundTransformNode);
    Load(StageRootNode, True);

//    ReceiveShadowVolumes:=True;
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

