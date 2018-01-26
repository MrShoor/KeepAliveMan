unit gWorld;

interface

uses
  avBase, IntfUtils, avContnrs, mutils;

type
  TGWorld = class;

  TGObject = class (TWeakedObject)
  private
    FWorld: TGWorld;
  public
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

    function GetBox(): TAABB;

    procedure SetupDone; virtual;

    constructor Create(const AWorld: TGWorld); override;
    destructor Destroy; override;
  end;
  IGBodyArr = {$IfDef FPC}specialize{$EndIf}IArray<TGBody>;
  TGBodyArr = {$IfDef FPC}specialize{$EndIf}TArray<TGBody>;

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
    FGObjects: IGObjectSet;

    FBodies: ITreeBodies;
    procedure AddOrUpdateAtTree(const ABody: TGBody);
  public
    function GetBodiesAtArea(const AArea: TRectF): IGBodyArr; overload;
    function GetBodiesAtArea(const APt: TVec2; const ARad: Single): IGBodyArr; overload;

    constructor Create();
    destructor Destroy; override;
  end;

implementation

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
  FWorld.AddOrUpdateAtTree(Self);
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

constructor TGBody.Create(const AWorld: TGWorld);
begin
  inherited Create(AWorld);
  FInSetup := True;
end;

destructor TGBody.Destroy;
begin
  FWorld.FBodies.Delete(Self);
  inherited Destroy;
end;

{ TGObject }

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

procedure TGWorld.AddOrUpdateAtTree(const ABody: TGBody);
begin
  FBodies.Delete(ABody);
  FBodies.Add(ABody, ABody.GetBox);
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

constructor TGWorld.Create;
begin
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
