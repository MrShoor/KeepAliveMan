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
  avRes, avTypes, mutils, avCameraController, avModel, avMesh, avTexLoader,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus;

const
  SHADERS_FROMRES = False;
  SHADERS_DIR = 'C:\MyProj\KeepAliveMan\shaders\!Out';

type
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
    FMain: TavMainRender;
    FFBO : TavFrameBuffer;

    FProgModels: TavProgram;
    FModelsCollection: TavModelCollection;
    FModels: IavModelInstanceArr;

    FMapIrradiance: TavTexture;
    FMapRadiance  : TavTexture;
    FHammersleyPts: TVec4Arr;
  public
    {$IfDef FPC}
    procedure EraseBackground(DC: HDC); override;
    {$EndIf}
    {$IfDef DCC}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    {$EndIf}
    procedure RenderScene;
    procedure DrawFrame;
  end;

var
  frmMain: TfrmMain;

implementation

{$IfnDef NoDCC}
  {$R *.dfm}
{$EndIf}

{$IfDef FPC}
  {$R *.lfm}
{$EndIf}

{ TfrmMain }

procedure TfrmMain.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  Done := False;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMain := TavMainRender.Create(nil);
  FMain.Projection.NearPlane := 0.1;
  FMain.Projection.FarPlane := 1000;
  FMain.Camera.Eye := Vec(10,10,-10);

  FFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f], [True, False]);

  FProgModels := TavProgram.Create(FMain);
  FProgModels.Load('avMesh', SHADERS_FROMRES, SHADERS_DIR);

  FModelsCollection := TavModelCollection.Create(FMain);
  FModels := FModelsCollection.ObtainModels(avMesh.LoadInstancesFromFile('models\test1.avm'));

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
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FProgModels.Invalidate();
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  DrawFrame;
end;

{$IfDef FPC}
procedure TfrmMain.EraseBackground(DC: HDC);
begin
//  inherited EraseBackground(DC);
end;
{$EndIf}

procedure TfrmMain.RenderScene;
begin
  FMain.States.DepthTest := True;

  FFBO.SetFrameRectFromWindow();
  FFBO.Select();

  FFBO.Clear(0, Vec(0,0,0,0));
  FFBO.ClearDS(1);

  FProgModels.Select();
  FProgModels.SetUniform('uRadiance', FMapRadiance, Sampler_Linear);
  FProgModels.SetUniform('uIrradiance', FMapIrradiance, Sampler_Linear);
  FProgModels.SetUniform('uHammersleyPts', FHammersleyPts);
  FProgModels.SetUniform('uSamplesCount', Length(FHammersleyPts)*1.0);
  FModelsCollection.Select;
  FModelsCollection.Draw(FModels);

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

