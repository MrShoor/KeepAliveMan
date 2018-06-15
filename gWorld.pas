unit gWorld;

interface

uses
  SysUtils,
  avBase, avRes, avTexLoader, avTypes, IntfUtils, avContnrs, mutils, gTypes, avModel, avMesh;

const
  WORLD_TIMESTEP = 4;

type
  IModelManager = interface
    procedure DropCache;
    function GetInstances(const APrefabFile: string): IavMeshInstances;
    function GetInstance(const APrefabFile: string; const ASourceInstance: string): IavMeshInstance;
    function BuildInstances(const APrefabFile: string; ASourceInstances: array of string): IavMeshInstanceArray;
    function BuildInstance(const APrefabFile: string; ASourceInstance: string): IavMeshInstance;
  end;

  { TModelManager }

  TModelManager = class (TInterfacedObject, IModelManager)
  private type
    TPrefabsHash = {$IfDef FPC}specialize{$EndIf} THashMap<string, IavMeshInstances>;
    IPrefabsHash = {$IfDef FPC}specialize{$EndIf} IHashMap<string, IavMeshInstances>;
  private
    FTexManager: ITextureManager;
    FPrefabs: IPrefabsHash;

    FGlobalCounter: Integer;
    function NextID: Integer;
    function ObtainPrefabs(const AFileName: string): IavMeshInstances;
  public
    procedure DropCache;
    function GetInstances(const APrefabFile: string): IavMeshInstances;
    function GetInstance(const APrefabFile: string; const ASourceInstance: string): IavMeshInstance;
    function BuildInstances(const APrefabFile: string; ASourceInstances: array of string): IavMeshInstanceArray;
    function BuildInstance(const APrefabFile: string; ASourceInstance: string): IavMeshInstance;

    procedure AfterConstruction; override;
  end;

  TGWorld = class;

  { TGObject }

  TGObject = class (TWeakedObject)
  strict private
    FWorld: TGWorld;
    FStateID: Integer;
  protected
    property World: TGWorld read FWorld;
  public
    procedure BumpStateID;
    constructor Create(const AWorld: TGWorld); virtual;
    destructor Destroy; override;
  end;
  IGObjectArr = {$IfDef FPC}specialize{$EndIf}IArray<TGObject>;
  TGObjectArr = {$IfDef FPC}specialize{$EndIf}TArray<TGObject>;
  IGObjectSet = {$IfDef FPC}specialize{$EndIf}IHashSet<TGObject>;
  TGObjectSet = {$IfDef FPC}specialize{$EndIf}THashSet<TGObject>;

  { TGBody }

  TGBody = class (TGObject)
  private
    FPos : TVec3;
    FRot : Single;
    FSize: Single;
    procedure SetPos(const AValue: TVec3);
    procedure SetRot(const AValue: Single);
    procedure SetSize(const AValue: Single);

    procedure ReinsertToTree; inline;
  protected
    FInSetup: Boolean;
  public
    property Pos : TVec3  read FPos  write SetPos;
    property Rot : Single read FRot  write SetRot;
    property Size: Single read FSize write SetSize;

    function Transform(): TMat4;

    function GetBox(): TAABB;

    procedure SetupDone; virtual;

    procedure DrawModels(); virtual;
    procedure UpdateStep; virtual;

    constructor Create(const AWorld: TGWorld); override;
    destructor Destroy; override;
  end;
  IGBodyArr = {$IfDef FPC}specialize{$EndIf}IArray<TGBody>;
  TGBodyArr = {$IfDef FPC}specialize{$EndIf}TArray<TGBody>;

  { TGBodyWithRes }

  TGBodyWithRes = class (TGBody)
  private
    FRes: TgResource;
    procedure SetRes(const AValue: TgResource);
  protected
    FModel: IavModelInstance;
    FModelSrcTransform: TMat4;
    procedure DoUpdateRes; virtual;
  public
    property Res: TgResource read FRes write SetRes;

    procedure DrawModels(); override;
  end;

  { TGWorldRes }

  TGWorldRes = class (TavMainRenderChild)
  const
      SHADERS_FROMRES = False;
      SHADERS_DIR = 'D:\Projects\KeepAliveMan\shaders\!Out';
  private
    FPrefabs: IModelManager;
    FModels: TavModelCollection;
  private
    FMapIrradiance: TavTexture;
    FMapRadiance  : TavTexture;
    FHammersleyPts: TVec4Arr;
    FProgramMesh: TavProgram;
  public
    procedure PreapreForRenderMeshes;

    property Prefabs: IModelManager read FPrefabs;
    property Models: TavModelCollection read FModels;
    property ProgramMesh: TavProgram read FProgramMesh;
    procedure AfterConstruction; override;
  end;

  { TGWorld }

  TGWorld = class (TWeakedObject)
  private type
    TTreeBodies = {$IfDef FPC}specialize{$EndIf}TLooseOctTree<TGBody>;
    ITreeBodies = {$IfDef FPC}specialize{$EndIf}ILooseOctTree<TGBody>;
    ITreeBodiesNode = {$IfDef FPC}specialize{$EndIf}IBase_LooseTreeNode<TGBody, TAABB>;
  private type
    TAreaIterator = class(TInterfacedObject, ILooseNodeCallBackIterator)
    private
      FOutArr: IGBodyArr;
      FArea  : TRectF;
      procedure OnEnumNode(const ASender: IInterface; const ANode: IInterface; var EnumChilds: Boolean);
    public
      constructor Create(const AArea: TRectF; const AOutArray: IGBodyArr);
    end;
  private
    FTime: Int64;

    FRes: TGWorldRes;

    FGObjects: IGObjectSet;

    FBodies: ITreeBodies;
    procedure FixCollisions(const ABody: TGBody);
    procedure AddOrUpdateAtTree(const ABody: TGBody);
  public
    property Res: TGWorldRes read FRes;

    function GetWorldTime: Int64;

    function GetBodiesAtArea(const AArea: TRectF): IGBodyArr; overload;
    function GetBodiesAtArea(const APt: TVec2; const ARad: Single): IGBodyArr; overload;

    procedure Draw;
    procedure UpdateStep;

    constructor Create(const AMainRenderer: TavMainRender);
    destructor Destroy; override;
  end;

implementation

{ TModelManager }

function TModelManager.NextID: Integer;
begin
  Inc(FGlobalCounter);
  Result := FGlobalCounter;
end;

function TModelManager.ObtainPrefabs(const AFileName: string): IavMeshInstances;
var meshes: IavMeshes;
begin
  if FPrefabs.TryGetValue(AFileName, Result) then Exit;
  if FileExists(AFileName) then
    avMesh.LoadFromFile(AFileName, meshes, Result, FTexManager)
  else
    Exit(nil);
  FPrefabs.Add(AFileName, Result);
end;

procedure TModelManager.DropCache;
begin
  FPrefabs.Clear;
  FTexManager.DropCache;
end;

function TModelManager.GetInstances(const APrefabFile: string): IavMeshInstances;
begin
  Result := ObtainPrefabs(APrefabFile);
end;

function TModelManager.GetInstance(const APrefabFile: string; const ASourceInstance: string): IavMeshInstance;
var Instances: IavMeshInstances;
begin
  Instances := ObtainPrefabs(APrefabFile);
  if not Instances.TryGetValue(ASourceInstance, Result) then
    Result := nil;
end;

function TModelManager.BuildInstances(const APrefabFile: string; ASourceInstances: array of string): IavMeshInstanceArray;
var Instances: IavMeshInstances;
    I: Integer;
    Inst, NewInst: IavMeshInstance;
begin
  Instances := ObtainPrefabs(APrefabFile);
  Result := TavMeshInstanceArray.Create;

  for i := 0 to Length(ASourceInstances) - 1 do
  begin
      if Instances.TryGetValue(ASourceInstances[i], Inst) then
      begin
        NewInst := Inst.Clone(Inst.Name + IntToStr(NextID));
        Result.Add(NewInst);
      end;
  end;
end;

function TModelManager.BuildInstance(const APrefabFile: string; ASourceInstance: string): IavMeshInstance;
var Instances: IavMeshInstances;
begin
  Instances := ObtainPrefabs(APrefabFile);
  if Instances = nil then Exit(nil);
  if Instances.TryGetValue(ASourceInstance, Result) then
    Result := Result.Clone(Result.Name + IntToStr(NextID))
  else
    Result := nil;
end;

procedure TModelManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FTexManager := Create_ITextureManager;
  FPrefabs := TPrefabsHash.Create;
  FGlobalCounter := 0;
end;

{ TGWorldRes }

procedure TGWorldRes.PreapreForRenderMeshes;
begin
  FProgramMesh.Select();
  FProgramMesh.SetUniform('uRadiance', FMapRadiance, Sampler_Linear);
  FProgramMesh.SetUniform('uIrradiance', FMapIrradiance, Sampler_Linear);
  FProgramMesh.SetUniform('uHammersleyPts', FHammersleyPts);
  FProgramMesh.SetUniform('uSamplesCount', Length(FHammersleyPts)*1.0);
  FModels.Select();
end;

procedure TGWorldRes.AfterConstruction;
begin
  inherited AfterConstruction;
  FPrefabs := TModelManager.Create;
  FModels := TavModelCollection.Create(Self);

  FProgramMesh := TavProgram.Create(Self);
  FProgramMesh.Load('avMesh', SHADERS_FROMRES, SHADERS_DIR);

  FMapIrradiance := TavTexture.Create(Main);
  FMapIrradiance.TargetFormat := TTextureFormat.RGBA16f;
  FMapIrradiance.TexData := LoadTexture(ExtractFilePath(ParamStr(0))+'\EnvMaps\Campus_irradiance.dds');
  FMapRadiance   := TavTexture.Create(Main);
  FMapRadiance.TargetFormat := TTextureFormat.RGBA16f;
  FMapRadiance.TexData := LoadTexture(ExtractFilePath(ParamStr(0))+'\EnvMaps\Campus_radiance.dds');

  FHammersleyPts := GenerateHammersleyPts(64);
end;

{ TGBodyWithRes }

procedure TGBodyWithRes.SetRes(const AValue: TgResource);
begin
  if FRes.Equal(AValue) then Exit;
  FRes := AValue;
  DoUpdateRes;
end;

procedure TGBodyWithRes.DoUpdateRes;
var
  i: Integer;
begin
  FModel := nil;
  if FRes.modelFileName <> '' then
  begin
    FModel := World.Res.Models.AddFromMeshInstance(World.Res.Prefabs.GetInstance(FRes.modelFileName, FRes.modelInst));
    FModelSrcTransform := FModel.Mesh.Transform;
  end;
end;

procedure TGBodyWithRes.DrawModels;
var
  i: Integer;
begin
  inherited DrawModels();
  FModel.Mesh.Transform := FModelSrcTransform * Transform();
  World.Res.Models.Draw(FModel);
end;

{ TGWorld.TAreaIterator }

procedure TGWorld.TAreaIterator.OnEnumNode(const ASender: IInterface; const ANode: IInterface; var EnumChilds: Boolean);
var octNode: ITreeBodiesNode absolute ANode;
    octTree: ITreeBodies absolute ASender;
    box: TAABB;
    rct: TRectF;
    i: Integer;
begin
  box := octTree.AABB(ANode);
  rct := RectF(box.min.xy, box.max.xy);
  EnumChilds := Intersect(rct, FArea);
  for i := 0 to octNode.ItemsCount() - 1 do
  begin
    box := octNode.Item(i).GetBox();
    rct := RectF(box.min.xy, box.max.xy);
    if Intersect(rct, FArea) then
      FOutArr.Add(octNode.Item(i));
  end;
end;

constructor TGWorld.TAreaIterator.Create(const AArea: TRectF; const AOutArray: IGBodyArr);
begin
  FArea := AArea;
  FOutArr := AOutArray;
end;

{ TGBody }

procedure TGBody.SetPos(const AValue: TVec3);
begin
  if FPos = AValue then Exit;
  FPos := AValue;
  ReinsertToTree;
end;

procedure TGBody.SetRot(const AValue: Single);
begin
  if FRot = AValue then Exit;
  FRot := AValue;
  ReinsertToTree;
end;

procedure TGBody.SetSize(const AValue: Single);
begin
  if FSize = AValue then Exit;
  FSize := AValue;
  ReinsertToTree;
end;

procedure TGBody.ReinsertToTree;
begin
  World.AddOrUpdateAtTree(Self);
end;

function TGBody.Transform: TMat4;
var m: TMat2;
begin
  m := Mat2(FRot);
  Result.OX := Vec(m.Row[0].x, 0, m.Row[0].y);
  Result.OY := Vec(0, 1, 0);
  Result.OZ := Vec(m.Row[1].x, 0, m.Row[1].y);
  Result.Pos := Pos;
  Result.Col[3] := Vec(0,0,0,1);
end;

function TGBody.GetBox: TAABB;
begin
  Result.min.xy := Pos.xy - Vec(Size*0.5, Size*0.5);
  Result.min.z := 1;
  Result.max.xy := Pos.xy + Vec(Size*0.5, Size*0.5);
  Result.max.z := 2;
end;

procedure TGBody.SetupDone;
begin
  FInSetup := False;
  ReinsertToTree;
end;

procedure TGBody.DrawModels;
begin

end;

procedure TGBody.UpdateStep;
begin

end;

constructor TGBody.Create(const AWorld: TGWorld);
begin
  inherited Create(AWorld);
  FInSetup := True;
end;

destructor TGBody.Destroy;
begin
  World.FBodies.Delete(Self);
  inherited Destroy;
end;

{ TGObject }

procedure TGObject.BumpStateID;
begin
  Inc(FStateID);
end;

constructor TGObject.Create(const AWorld: TGWorld);
begin
  Assert(AWorld <> nil);
  FWorld := AWorld;
  FWorld.FGObjects.Add(Self);
end;

destructor TGObject.Destroy;
begin
  inherited;
  FWorld.FGObjects.Delete(Self);
end;

{ TGWorld }

procedure TGWorld.FixCollisions(const ABody: TGBody);
begin
  //todo
end;

procedure TGWorld.AddOrUpdateAtTree(const ABody: TGBody);
begin
  FBodies.Delete(ABody);
  FBodies.Add(ABody, ABody.GetBox);
end;

function TGWorld.GetWorldTime: Int64;
begin
  Result := FTime;
end;

function TGWorld.GetBodiesAtArea(const AArea: TRectF): IGBodyArr;
var ci: ILooseNodeCallBackIterator;
begin
  Result := TGBodyArr.Create();
  ci := TAreaIterator.Create(AArea, Result);
  FBodies.EnumNodes(ci);
end;

function TGWorld.GetBodiesAtArea(const APt: TVec2; const ARad: Single): IGBodyArr;
begin
  Result := GetBodiesAtArea(RectF(APt - Vec(ARad, ARad), APt + Vec(ARad, ARad)));
end;

procedure TGWorld.Draw;
var obj: TGObject;
begin
  Res.PreapreForRenderMeshes;
  FGObjects.Reset;
  while FGObjects.Next(obj) do
    if (obj is TGBody) then TGBody(obj).DrawModels();
end;

procedure TGWorld.UpdateStep;
var obj: TGObject;
begin
  Inc(FTime, WORLD_TIMESTEP);
  FGObjects.Reset;
  while FGObjects.Next(obj) do
    if (obj is TGBody) then TGBody(obj).UpdateStep;
end;

constructor TGWorld.Create(const AMainRenderer: TavMainRender);
begin
  FRes := TGWorldRes.Create(AMainRenderer);

  FGObjects := TGObjectSet.Create;
  FBodies := TTreeBodies.Create(Vec(0.1, 0.1, 100));
end;

destructor TGWorld.Destroy;
var objs: IGObjectArr;
    i: Integer;
begin
  objs := FGObjects.ToIArray();
  for i := 0 to objs.Count - 1 do
    objs[i].Free;
  inherited;
end;

end.
