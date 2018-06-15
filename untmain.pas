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
  gWorld, gLevel,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus;

const
  SHADERS_FROMRES = False;
  SHADERS_DIR = 'D:\Projects\KeepAliveMan\shaders\!Out';

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
    FLastStepTime: Int64;

    FMain: TavMainRender;
    FFBO : TavFrameBuffer;

    FWorld: TGWorld;
    FLevel: TGameLevel;

    FModelsCollection: TavModelCollection;
    FModels: IavModelInstanceArr;
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
begin
  FMain := TavMainRender.Create(nil);
  FMain.Projection.NearPlane := 0.1;
  FMain.Projection.FarPlane := 1000;
  FMain.Camera.Eye := Vec(10,10,-10);

  FFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f], [True, False]);

  FWorld := TGWorld.Create(FMain);
  FLevel := TGameLevel0.Create(FWorld);

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

procedure TfrmMain.ProcessInput;
begin

end;

procedure TfrmMain.RenderScene;
begin
  FMain.States.DepthTest := True;

  FFBO.SetFrameRectFromWindow();
  FFBO.Select();

  FFBO.Clear(0, Vec(0,0,0,0));
  FFBO.ClearDS(1);

  FWorld.Draw;

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

