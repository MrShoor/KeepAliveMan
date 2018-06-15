unit untMain;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$Define NoDCC}
{$Else}
  {$Define DCC}
{$EndIf}

interface

uses
  {$IfDef FPC}
  FileUtil,
  LCLType,
  {$EndIf}
  {$IfDef DCC}
  AppEvnts,
  Messages,
  {$EndIf}
  avRes, avTypes, mutils, avCameraController, avModel, avMesh, avTexLoader, avLights,
  gWorld, gLevel,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus;

const
  SHADERS_FROMRES = False;
  SHADERS_DIR = 'D:\Projects\KeepAliveMan\shaders\!Out';

type
  TOnShadowPassGeometry = procedure (const APointLightMatrices: TPointLightMatrices) of object;

  { TGeometryRenderer }

  TGeometryRenderer = class(TInterfacedObject, IGeometryRenderer)
  private
    FOnShadowPassGeometry: TOnShadowPassGeometry;
    procedure ShadowPassGeometry(const APointLightMatrices: TPointLightMatrices);
    procedure DrawTransparentGeometry();
  public
    constructor Create(const AOnShadowPassGeometry: TOnShadowPassGeometry);
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    {$IfDef DCC}
    ApplicationEvents: TApplicationEvents;
    {$EndIf}
    {$IfDef FPC}
    ApplicationProperties: TApplicationProperties;
    {$EndIf}
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  private
    FLastStepTime: Int64;

    FMain: TavMainRender;
    FFBO : TavFrameBuffer;

    FWorld: TGWorld;
    FLevel: TGameLevel;

    FProgPointShadows: TavProgram;
    FModelsCollection: TavModelCollection;
    FModels: IavModelInstanceArr;
    FModelSphere: IavModelInstance;

    FRendererIntf: IGeometryRenderer;

    FLightRenderer : TavLightRenderer;

    FMapIrradiance: TavTexture;
    FMapRadiance  : TavTexture;
    FHammersleyPts: TVec4Arr;

    procedure ShadowPassGeometry(const APointLightMatrices: TPointLightMatrices);
  public
    {$IfDef FPC}
    procedure EraseBackground(DC: HDC); override;
    {$EndIf}
    {$IfDef DCC}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    {$EndIf}
    procedure ProcessInput;
    procedure RenderScene;
    procedure DrawFrame;
  end;

var
  frmMain: TfrmMain;

implementation

{ TGeometryRenderer }

procedure TGeometryRenderer.ShadowPassGeometry(const APointLightMatrices: TPointLightMatrices);
begin
  FOnShadowPassGeometry(APointLightMatrices);
end;

procedure TGeometryRenderer.DrawTransparentGeometry;
begin

end;

constructor TGeometryRenderer.Create(const AOnShadowPassGeometry: TOnShadowPassGeometry);
begin
  FOnShadowPassGeometry := AOnShadowPassGeometry;
end;

{$IfnDef NoDCC}
  {$R *.dfm}
{$EndIf}

{$IfDef FPC}
  {$R *.lfm}
{$EndIf}

{ TfrmMain }

procedure TfrmMain.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
var currTime: Int64;
begin
  Done := False;
  currTime := FMain.Time64;
  while FLastStepTime + WORLD_TIMESTEP <= currTime do
  begin
    Inc(FLastStepTime, WORLD_TIMESTEP);
    ProcessInput;
    FWorld.UpdateStep;

    FMain.InvalidateWindow;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FRendererIntf := TGeometryRenderer.Create({$IfDef FPC}@{$EndIf}ShadowPassGeometry);

  FMain := TavMainRender.Create(nil);
  FMain.Projection.NearPlane := 0.5;
  FMain.Projection.FarPlane := 100;
  FMain.Camera.Eye := Vec(10,10,-10);

  FFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f], [True, False]);

  FWorld := TGWorld.Create(FMain);
  FLevel := TGameLevel0.Create(FWorld);
  FProgPointShadows := TavProgram.Create(FMain);
  FProgPointShadows.Load('point_shadows', SHADERS_FROMRES, SHADERS_DIR);

  FModelsCollection := TavModelCollection.Create(FMain);
  FModels := FModelsCollection.ObtainModels(avMesh.LoadInstancesFromFile('models\test1.avm'));
  for i := 0 to FModels.Count - 1 do
    if FModels[i].ModelName = 'Icosphere' then
    begin
      FModelSphere := FModels[i];
      FModels.DeleteWithSwap(i);
      Break;
    end;

  FLightRenderer := TavLightRenderer.Create(FMain);
  with FLightRenderer.AddPointLight() do
  begin
    Pos := Vec(0,0,0);
    Radius := 15;
    Color := Vec(1,1,1);
  end;

  FMapIrradiance := TavTexture.Create(FMain);
  FMapIrradiance.TargetFormat := TTextureFormat.RGBA16f;
  FMapIrradiance.TexData := LoadTexture(ExtractFilePath(ParamStr(0))+'\EnvMaps\Campus_irradiance.dds');
  FMapRadiance   := TavTexture.Create(FMain);
  FMapRadiance.TargetFormat := TTextureFormat.RGBA16f;
  FMapRadiance.TexData := LoadTexture(ExtractFilePath(ParamStr(0))+'\EnvMaps\Campus_radiance.dds');

  FHammersleyPts := GenerateHammersleyPts(64);

  with TavCameraController.Create(FMain) do
  begin
    MouseBtn_Move := 1;
    MouseBtn_Rotate := 2;
    MovePlane := Plane(0,1,0,0);
    CanMove := True;
    CanRotate := True;
  end;

  FLastStepTime := FMain.Time64;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLevel);
  FreeAndNil(FWorld);
  FreeAndNil(FMain);
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FWorld.Res.ProgramMesh.Invalidate();
  FLightRenderer.InvalidateShaders();
  FMain.InvalidateWindow;
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  DrawFrame;
end;

procedure TfrmMain.ShadowPassGeometry(const APointLightMatrices: TPointLightMatrices);
begin
  if FMain.ActiveProgram <> FProgPointShadows then
    FProgPointShadows.Select();
end;

{$IfDef FPC}
procedure TfrmMain.EraseBackground(DC: HDC);
begin
//  inherited EraseBackground(DC);
end;
{$EndIf}

procedure TfrmMain.ProcessInput;
begin

end;

procedure TfrmMain.RenderScene;
begin
  FMain.States.DepthTest := True;

  FLightRenderer.Render(FRendererIntf);

  FFBO.SetFrameRectFromWindow();
  FFBO.Select();

  FFBO.Clear(0, Vec(0,0,0,0));
  FFBO.ClearDS(1);

  FProgModels.Select();
  FProgModels.SetUniform('uRadiance', FMapRadiance, Sampler_Linear);
  FProgModels.SetUniform('uIrradiance', FMapIrradiance, Sampler_Linear);
  FProgModels.SetUniform('uHammersleyPts', FHammersleyPts);
  FProgModels.SetUniform('uSamplesCount', Length(FHammersleyPts)*1.0);

  FProgModels.SetUniform('depthRange', FMain.Projection.DepthRange);
  FProgModels.SetUniform('planesNearFar', Vec(FMain.Projection.NearPlane, FMain.Projection.FarPlane));
  FProgModels.SetUniform('light_headBufferSize', FLightRenderer.LightsHeadBuffer.Size*1.0);
  FProgModels.SetUniform('light_headBuffer', FLightRenderer.LightsHeadBuffer, Sampler_NoFilter);
  FProgModels.SetUniform('light_linkedList', FLightRenderer.LightsLinkedList);
  FProgModels.SetUniform('light_list', FLightRenderer.LightsList);

  FProgModels.SetUniform('debugLines', 0.0);
  FModelsCollection.Select;
  FMain.States.Wireframe := False;
  FMain.States.CullMode := cmBack;
  FModelsCollection.Draw(FModels);

  FMain.States.Wireframe := True;
  FMain.States.CullMode := cmNone;
  FProgModels.SetUniform('debugLines', 1.0);
  FModelsCollection.Draw(FModelSphere);

  FFBO.BlitToWindow();
end;

{$IfDef DCC}
procedure TfrmMain.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;
{$EndIf}

procedure TfrmMain.DrawFrame;
begin
  if FMain = nil then Exit;

  if not FMain.Inited3D then
  begin
    FMain.Window := Handle;
    FMain.Init3D(apiDX11);
  end;
  if not FMain.Inited3D then Exit;

  if FMain.Bind then
  try
    RenderScene;
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

end.

