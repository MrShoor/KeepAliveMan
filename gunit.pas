unit gUnit;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, gWorld, mutils, avMesh, avContnrs, gTypes,
  intfUtils;

type
  TCharacterState = (csNone,
                     csMove,
                     csWalk,
                     csJump,
                     csAttack,
                     csDefence,
                     csStunImpact,
                     csDead,
                     csPicking,
                     csEating,
                     csInteractiveAction);

  { TAttackInfo }

  TAttackInfo = packed record
    Duration       : Integer;  //msec
    DamageTime     : TVec2i;   //x - start time of damage, y - end time of
    AnimationOffset: TVec3Arr;
    AnimationName  : string;
    //WeaponInfo     : IGameObjectInfo;
    //function WeaponType: TWeaponType;
  end;

  TBlockInfo = packed record
    SetupTime   : Integer;
    DamageReduce: Single;
    EDReduce    : Single;
  end;

  TJumpInfo = packed record
    AnimationOffset: TVec3Arr;
    Duration: Single;
  end;

  { TAttackState }

  TAttackState = packed record
    StartTime    : Int64;
    CurrentTime  : Int64;
    AttackIndex  : Integer;
    AttackTarget : TVec3;
    //WeaponID     : TGameObjectID;
    //DamagedUnits : array of Int64;
    //function Damaged(const ID: Int64): Boolean;
    //function AddDamaged(const ID: Int64): Boolean;
  end;

  TBlockState = packed record
    StartTime  : Int64;
    CurrentTime: Int64;
    BlockActive: Boolean;
  end;

  TMoveState = packed record
    AtTargetMode : Boolean;
    MoveVector   : TVec2;
  end;

  TStunImpactType = (sitBlock, sitLow, sitHigh);

  TImpactState = packed record
    StunType: TStunImpactType;
    StopTime: Int64;
  end;

  TDeadState = packed record
    DeadTime : Int64;
    AnimIndex: Integer;
  end;

  TPickingState = packed record
    PickingObject: Int64;
    StartTime: Int64;
  end;

  TEatingState = packed record
    StartTime : Int64;
//    InvKind   : TInventoryKind;
    InvSlot   : Integer;
  end;

  TJumpState = packed record
    StartTime  : Int64;
    Direction  : TVec2;
    JumpStyle  : Integer;
    CurrentTime: Int64;
  end;

  { TGameUnit }

  TGameUnit = class (TGBodyWithRes)
  private
    FLastUpdateTime: Int64;
    FState: TCharacterState;
    FTargetAngle: Single;

    FMoveState   : TMoveState;
    FAttackState : TAttackState;
    FBlockState  : TBlockState;
    FImpactState : TImpactState;
    FDeadState   : TDeadState;
    FPickingState: TPickingState;
    FEatingState : TEatingState;
    FJumpState   : TJumpState;

    FAttackActive: TAttackInfo;
    FBlockActive : TBlockInfo;
    FJumpActive  : TJumpInfo;

    FMaxED: Integer;
    FMaxHP: Integer;
    FMaxST: Integer;

    FHP: Integer;
    FProtectState: Single;
    FST: Integer;
    FED: Integer;

    FViewDistance : Single;
    FViewAngle    : Single;

    function GetLookDir: TVec2;
    function GetState: TCharacterState;
    procedure SetLookDir(const AValue: TVec2);
    procedure SetState(const Value: TCharacterState);
    procedure SetProtectState(const AValue: Single);
  protected
    Freezed : Boolean;
    FAnimation: IavAnimationController;

    procedure GetCurrentWeaponDamage(out HPDamage, EDDamage: Integer);

    function ClampMoveSpeed(const V: TVec2; const IsWalk: Boolean): TVec2;
    function GetMaxMoveSpeed(IsWalk: Boolean): Single; virtual; abstract;
    function GetMaxRotSpeed(): Single; virtual; abstract;
    function GetBlockInfo(): TBlockInfo; virtual; abstract;
    //function GetAttackInfo(const AttackIndex: Integer; const AWeapon: TGameObjectID): TAttackInfo; virtual; abstract;
    function GetJumpInfo(const AJumpStyle: Integer): TJumpInfo; virtual; abstract;
    //function ActiveWeapon: TGameObjectID; virtual; abstract;

    function ReduceDamage(const ADamage: Integer; const ADir: TVec3): Integer; virtual;
    function ReduceEDDamage(const AEDDamage: Integer; const ADir: TVec3): Integer; virtual;

    procedure ProcessAttack(); virtual; abstract;

    //function TryPickToInventory(const AItem: TItemObject): Boolean; virtual;
    //function CanBeEated(const AInvKind: TInventoryKind; const ASlot: Integer): Boolean;
    //procedure ApplyFood(const AFood: IGameObjectInfo; const Quantity: Integer); overload; virtual;
    //procedure ApplyFood(const AFood: TGameObjectID; const Quantity: Integer); overload;

    //procedure SetAngle(AValue: Single); override;

    procedure DoUpdateRes; override;
    procedure UpdateStates(ASender: TObject); virtual;
  public
    procedure UpdateStep; override;

    procedure RecieveDamage(const AFromID: Int64; ADamage, AEDDamage: Integer; const ADir: TVec3);
    //function ClampAngleForUpdateStep(const ATargetAngle: Single): Single;

    property State: TCharacterState read GetState write SetState;

    property MaxHP : Integer read FMaxHP write FMaxHP;
    property MaxST : Integer read FMaxST write FMaxST;
    property MaxED : Integer read FMaxED write FMaxED;

    property HP : Integer read FHP write FHP;
    property ST : Integer read FST write FST;
    property ED : Integer read FED write FED;

    property ViewDistance : Single read FViewDistance write FViewDistance;
    property ViewAngle    : Single read FViewAngle    write FViewAngle;

    property LookDir : TVec2 read GetLookDir write SetLookDir;

    property BlockState  : TBlockState  read FBlockState;
    property MoveState   : TMoveState   read FMoveState;
    property AttackState : TAttackState read FAttackState;
    property ProtectState: Single       read FProtectState write SetProtectState;
    property ImpactState : TImpactState read FImpactState;
    property DeadState   : TDeadState   read FDeadState;
    property JumpState   : TJumpState   read FJumpState;

    property AttackInfo: TAttackInfo read FAttackActive;
    property BlockInfo : TBlockInfo read FBlockActive;

    //state change actions
    procedure SetMove(const V: TVec2; const ARelative: Boolean);
    procedure StopMove();
    procedure SetTargetAngle(const ATargetAngle: Single);
    procedure SetAttack(const AttackType: Integer; const AAttackTarget: TVec3);
    procedure SetBlock;
    procedure SetJump(const ADirection: TVec2; const AJumpStyle: Integer);
    procedure ReleaseBlock;
    //function  TryPick(const AItem: TItemObject): Boolean;
    //function  TryEat(const AInvKind: TInventoryKind; const ASlotIndex: Integer): Boolean;
    //procedure SetDeath; //todo

    function See(const APos: TVec3): Boolean; overload;
    //function See(const AObj: TGameObject): Boolean; overload;
    //function GetVisibleObjectIDs(): IGameObjectIDsSet;
    //function GetVisibleObjects(): IGameObjectsSet;

    //function GetInventory(const AKind: TInventoryKind): IUnitInventory; virtual;

    constructor Create(const AWorld: TGWorld); override;
  end;

  { TGamePlayer }

  TGamePlayer = class (TGameUnit)
  protected
    function GetMaxMoveSpeed(IsWalk: Boolean): Single; override;
    function GetMaxRotSpeed(): Single; override;
    function GetBlockInfo(): TBlockInfo; override;
    //function GetAttackInfo(const AttackIndex: Integer; const AWeapon: TGameObjectID): TAttackInfo; override;
    function GetJumpInfo(const AJumpStyle: Integer): TJumpInfo; override;
    //function ActiveWeapon: TGameObjectID; override;

    procedure ProcessAttack(); override;

    //function TryPickToInventory(const AItem: TItemObject): Boolean; override;
  protected
    //FBag : IUnitInventory;
    //FWear: IUnitInventory;
  public
    //function GetInventory(const AKind: TInventoryKind): IUnitInventory; override;
    procedure AfterConstruction; override;
  end;

function ExtractAnimationOffsets(const AMesh: IavMeshInstance; const AnimationName, BoneName: string): TVec3Arr;

implementation

uses Math, TypInfo;

function ExtractAnimationOffsets(const AMesh: IavMeshInstance; const AnimationName, BoneName: string): TVec3Arr;
var arm  : IavArmature;
    anim : IavAnimation;
    bone : IavBone;
    armBoneIndex: Integer;
    animBoneIndex: Integer;
    boneHead: TVec3;
    m: TMat4;
    i: Integer;
begin
  Result := nil;
  if AMesh = nil then Exit;
  if AMesh.Pose = nil then Exit;
  arm := AMesh.Pose.Armature;
  if arm = nil then Exit;
  anim := arm.FindAnim(AnimationName);
  if anim = nil then Exit;
  armBoneIndex := arm.IndexOfBone(BoneName);
  if armBoneIndex < 0 then Exit;
  animBoneIndex := anim.GetLocalBoneIndex(armBoneIndex);
  if animBoneIndex < 0 then Exit;
  boneHead := arm.Bone[armBoneIndex].Head;

  SetLength(Result, anim.FrameCount);
  for i := 0 to anim.FrameCount - 1 do
  begin
    anim.GetBoneTransform(animBoneIndex, i, armBoneIndex, m);
    m := m * arm.Transform;
    Result[i] := boneHead * m;
  end;
end;

{ TAttackInfo }

//function TAttackInfo.WeaponType: TWeaponType;
//begin
//  Result := wtSword;
//  if WeaponInfo = nil then Exit;
//  if itWeapon in WeaponInfo.Tags then Result := WeaponInfo.TagParams.Weapon.Kind;
//  if itWeapon2H in WeaponInfo.Tags then Result := WeaponInfo.TagParams.Weapon2H.Kind;
//end;

{ TAttackState }

//function TAttackState.Damaged(const ID: Int64): Boolean;
//var i: Integer;
//begin
//  Result := False;
//  for i := 0 to Length(DamagedUnits) - 1 do
//    if DamagedUnits[i] = ID then Exit(True);
//end;

//function TAttackState.AddDamaged(const ID: Int64): Boolean;
//begin
//  if Damaged(ID) then Exit(False);
//  SetLength(DamagedUnits, Length(DamagedUnits)+1);
//  DamagedUnits[High(DamagedUnits)] := ID;
//  Result := True;
//end;

{ TGamePlayer }

function TGamePlayer.GetMaxMoveSpeed(IsWalk: Boolean): Single;
const SPEED = 8;
begin
  Result := 3*SPEED / 1000;
end;

function TGamePlayer.GetMaxRotSpeed: Single;
begin
  Result := 20 * Pi / 1000;
end;

function TGamePlayer.GetBlockInfo: TBlockInfo;
begin
  Result.SetupTime := 400;
  Result.EDReduce  := 0.9;
  Result.DamageReduce := 0.9;
end;

//function TGamePlayer.GetAttackInfo(const AttackIndex: Integer; const AWeapon: TGameObjectID): TAttackInfo;
//const WeaponPrefix: array [TWeaponType] of string = ('sword_', 'bow_');
//var wInfo: IGameObjectInfo;
//    weaponType: TWeaponType;
//    Mesh: IavMeshInstance;
//begin
//  Result.Duration := 1052;
//  Result.DamageTime := Vec(495, 693);
//
//  weaponType := wtSword;
//  if World.GameObjectManager.TryGetGameObjectInfo(AWeapon, wInfo) then
//  begin
//    if itWeapon in wInfo.Tags then
//      weaponType := wInfo.TagParams.Weapon.Kind;
//    if itWeapon2H in wInfo.Tags then
//      weaponType := wInfo.TagParams.Weapon2H.Kind;
//  end;
//  if weaponType = wtBow then
//    Result.DamageTime := Vec(600, 600);
//  Result.AnimationName := WeaponPrefix[weaponType]+'attack'+IntToStr(AttackIndex);
//  Result.WeaponInfo := wInfo;
//
//  Result.AnimationOffset := nil;
//  if GameObjectInfo <> nil then
//  begin
//    Mesh := World.ModelManager.GetInstance(GameObjectInfo.ModelFile, GameObjectInfo.ModelInst);
//    if Mesh <> nil then
//      Result.AnimationOffset := ExtractAnimationOffsets(Mesh, Result.AnimationName, 'Hips');
//  end;
//
//  if Length(Result.AnimationOffset) > 0 then
//    Result.Duration := Length(Result.AnimationOffset)*30;
//end;

function TGamePlayer.GetJumpInfo(const AJumpStyle: Integer): TJumpInfo;
var Mesh: IavMeshInstance;
begin
  Result.AnimationOffset := nil;
  Result.Duration := 1000;
    //World.Res.Models.GetInterface();
  Mesh := World.Res.Prefabs.GetInstance(Res.modelFileName, Res.modelInst);
  if Mesh <> nil then
    Result.AnimationOffset := ExtractAnimationOffsets(Mesh, 'roll'+IntToStr(AJumpStyle), 'Hips');
end;

//function TGamePlayer.ActiveWeapon: TGameObjectID;
//begin
//  Result := FWear.Item(Integer(wsRightHand)).GOID;
//end;

procedure TGamePlayer.ProcessAttack;
//const AttackDist = 1.5;
//var objs: IGameObjectsArr;
//    dir : TVec3;
//    dirLenSqr: Single;
//    i: Integer;
//    EDDamage, HPDamage: Integer;
begin
  //objs := World.GetObjects(Position, AttackDist);
  //if objs = nil then Exit;
  //for i := 0 to objs.Count - 1 do
  //begin
  //  if objs[i] = self then Continue;
  //  if not (objs[i] is TGameUnit) then Continue;
  //  dir := objs[i].Position - Position;
  //  dirLenSqr := LenSqr(dir);
  //  if dirLenSqr > sqr(AttackDist + Size*0.5 + objs[i].Size*0.5) then Continue;
  //  dir := dir / sqrt(dirLenSqr);
  //  if dot(LookDir, dir.xy) < 0.3 then Continue;
  //  if FAttackState.AddDamaged(objs[i].ID) then
  //  begin
  //    GetCurrentWeaponDamage(HPDamage, EDDamage);
  //    TGameUnit(objs[i]).RecieveDamage(ID, HPDamage, EDDamage, dir);
  //  end;
  //end;
end;

//function TGamePlayer.TryPickToInventory(const AItem: TItemObject): Boolean;
//var i: Integer;
//    q, oldq: Integer;
//    bestSlot: Integer;
//begin
//  Result := False;
//  oldq := AItem.Quantity;
//  q := oldq;
//  while q > 0 do
//  begin
//    bestSlot := FindSlotFor(AItem.GOID, GetInventory(ikBag));
//    if bestSlot >= 0 then
//    begin
//      q := AItem.Quantity;
//      FBag.TryAdd(AItem.GOID, bestSlot, q);
//      AItem.Quantity := q;
//    end
//    else
//      Break;
//  end;
//  Result := AItem.Quantity <> oldq;
//  //for i := 0 to FBag.Count - 1 do
//  //  if FBag.TryAdd(AItem.GOID, i, q) then Exit(True);
//end;

//function TGamePlayer.GetInventory(const AKind: TInventoryKind): IUnitInventory;
//begin
//  Result := inherited GetInventory(AKind);
//  case AKind of
//    ikBag : Result := FBag;
//    ikCharacter : Result := FWear;
//  end;
//end;

procedure TGamePlayer.AfterConstruction;
begin
  inherited AfterConstruction;
  //FBag  := TUnitInventoryBag.Create(World.GameObjectManager);
  //FWear := TUnitInventoryWear.Create(World.GameObjectManager);
end;

{ TGameUnit }

function TGameUnit.GetLookDir: TVec2;
begin
  Result.x := cos(Rot);
  Result.y := sin(Rot);
end;

function TGameUnit.GetState: TCharacterState;
begin
  Result := FState;
end;

procedure TGameUnit.SetLookDir(const AValue: TVec2);
begin
  if LenSqr(AValue) = 0 then Exit;
  Rot := arctan2(AValue.y, AValue.x);
end;

procedure TGameUnit.SetProtectState(const AValue: Single);
begin
  if FProtectState=AValue then Exit;
  FProtectState:=AValue;
end;

procedure TGameUnit.GetCurrentWeaponDamage(out HPDamage, EDDamage: Integer);
//var gInfo: IGameObjectInfo;
begin
  //HPDamage := 1;
  //EDDamage := 1;
  //if World.GameObjectManager.TryGetGameObjectInfo(ActiveWeapon, gInfo) then
  //begin
  //  if itWeapon in gInfo.Tags then
  //  begin
  //    HPDamage := gInfo.TagParams.Weapon.Damage;
  //    EDDamage := gInfo.TagParams.Weapon.EDDamage;
  //  end;
  //  if itWeapon2H in gInfo.Tags then
  //  begin
  //    HPDamage := gInfo.TagParams.Weapon2H.Damage;
  //    EDDamage := gInfo.TagParams.Weapon2H.EDDamage;
  //  end;
  //end;
end;

procedure TGameUnit.SetState(const Value: TCharacterState);
begin
  if FState = Value then Exit;
  FState := Value;
//  BumpStateID;
end;

function TGameUnit.ClampMoveSpeed(const V: TVec2; const IsWalk: Boolean): TVec2;
begin
  if LenSqr(V) = 0 then Exit(V);
  Result := SetLen(V, GetMaxMoveSpeed(IsWalk));
end;

function TGameUnit.ReduceDamage(const ADamage: Integer; const ADir: TVec3): Integer;
var NewDmg: Single;
    k: Single;
begin
  NewDmg := ADamage;

  if State = csDefence then
  begin
    k := -Dot(LookDir, ADir.xy);
    if k > 0.2 then
    begin
      NewDmg := NewDmg * 0.2;
    end;
  end;

  if State = csStunImpact then
  begin
    case FImpactState.StunType of
      sitBlock : NewDmg := NewDmg * 1.1;
      sitHigh  : NewDmg := NewDmg * 1.3;
      sitLow   : NewDmg := NewDmg * 1.2;
    end;
  end;

  Result := Round(NewDmg);
end;

function TGameUnit.ReduceEDDamage(const AEDDamage: Integer; const ADir: TVec3): Integer;
begin
  Result := ReduceDamage(AEDDamage, ADir);
end;

procedure TGameUnit.DoUpdateRes;
begin
  inherited DoUpdateRes;
  if FModel <> nil then
  begin
    FAnimation := Create_IavAnimationController(FModel.Mesh.Pose, World.GetWorldTime);
    FAnimation.AnimationStart('idle0');
  end
  else
    FAnimation := nil;
end;

procedure TGameUnit.RecieveDamage(const AFromID: Int64; ADamage, AEDDamage: Integer; const ADir: TVec3);
begin
  //if State = csDead then Exit;
  //
  //ADamage := ReduceDamage(ADamage, ADir);
  //World.GameEvents.UnitDamaged(Self, ADamage);
  //
  //FHP := Max(0, FHP - ADamage);
  //if FHP = 0 then
  //begin
  //  State := csDead;
  //  FVelocity := Vec(0,0,0);
  //  FDeadState.AnimIndex := Random(2);
  //  FDeadState.DeadTime := World.GetWorldTime;
  //  Exit;
  //end;
  //
  //AEDDamage := ReduceEDDamage(AEDDamage, ADir);
  //FED := FED - AEDDamage;
  //if FED <= 0 then
  //begin
  //  if State = csDefence then
  //  begin
  //    FImpactState.StunType := sitBlock;
  //    FImpactState.StopTime := World.GetWorldTime + 400;
  //  end
  //  else
  //    if (State = csAttack) or (FED < -(FMaxED div 10)) then
  //    begin
  //      FImpactState.StunType := sitHigh;
  //      FImpactState.StopTime := World.GetWorldTime + 600;
  //    end
  //    else
  //    begin
  //      FImpactState.StunType := sitLow;
  //      FImpactState.StopTime := World.GetWorldTime + 600;
  //    end;
  //  State := csStunImpact;
  //  FED := 0;
  //end;
  //
  //BumpStateID;
end;

//function TGameUnit.TryPickToInventory(const AItem: TItemObject): Boolean;
//begin
//  Result := False;
//end;

//function TGameUnit.CanBeEated(const AInvKind: TInventoryKind; const ASlot: Integer): Boolean;
//var inv: IUnitInventory;
//    gInfo: IGameObjectInfo;
//begin
//  Result := False;
//  inv := GetInventory(AInvKind);
//  if inv = nil then Exit;
//  if ASlot < 0 then Exit;
//  if ASlot >= inv.Count then Exit;
//  if not World.GameObjectManager.TryGetGameObjectInfo(inv.Item(ASlot).GOID, gInfo) then Exit;
//  if not (itFood in gInfo.Tags) then Exit;
//  if inv.Item(ASlot).Quantity = 0 then Exit;
//  Result := True;
//end;

//procedure TGameUnit.ApplyFood(const AFood: IGameObjectInfo; const Quantity: Integer);
//begin
//  Inc(FHP, 20);
//  if FHP > FMaxHP then FHP := FMaxHP;
//end;
//
//procedure TGameUnit.ApplyFood(const AFood: TGameObjectID; const Quantity: Integer);
//var gInfo: IGameObjectInfo;
//begin
//  if not World.GameObjectManager.TryGetGameObjectInfo(AFood, gInfo) then Exit;
//  ApplyFood(gInfo, Quantity);
//end;

//procedure TGameUnit.SetAngle(AValue: Single);
//var tmp1, tmp2: Integer;
//begin
//  if FAngle = AValue then Exit;
//  tmp1 := Round(frac(FAngle/(2*Pi)) * 64);
//  FAngle := AValue;
//  tmp2 := Round(frac(FAngle/(2*Pi)) * 64);
//  if tmp1 <> tmp2 then BumpStateID;
//end;

//procedure TGameUnit.ApplyVelocity;
//var ClampedVel: TVec3;
//begin
//  //inherited ApplyVelocity;
//
//  ClampedVel := Velocity;
//  ClampedVel.xy := ClampMoveSpeed(ClampedVel.xy, False);
//  case FState of
//    csMove, csWalk:
//    begin
//      if FMoveState.AtTargetMode then
//      begin
//        if LenSqr(Position.xy - FMoveState.MoveVector) < Sqr(Max(0.1, Size*0.5)) then
//        begin
//          StopMove();
//          Exit;
//        end
//        else
//          FVelocity.xy := FMoveState.MoveVector - Position.xy;
//      end;
//    end;
//  else
//
//  end;
//  Position := Position + ClampedVel;
//end;

//function TGameUnit.ClampAngleForUpdateStep(const ATargetAngle: Single): Single;
//var dAngle: Single;
//    allowRotate: Boolean;
//begin
//  dAngle := NormalizeAngle(ATargetAngle - Angle);
//  if dAngle > PI then dAngle := dAngle - 2*PI;
//
//  allowRotate := State in [csNone, csMove, csWalk, csDefence];
//  if State = csAttack then
//    allowRotate := FAttackActive.WeaponType = wtBow;
//
//  if allowRotate then
//    dAngle := min(Abs(dAngle), GetMaxRotSpeed() ) * Sign(dAngle)
//  else
//    dAngle := 0;
//
//  Result := Angle + dAngle;
//end;

procedure TGameUnit.SetMove(const V: TVec2; const ARelative: Boolean);
begin
  if not (State in [csNone, csMove, csWalk, csPicking, csInteractiveAction]) then Exit;
  if FMoveState.AtTargetMode <> (not ARelative) then BumpStateID;
  if FMoveState.MoveVector <> V then BumpStateID;

  FMoveState.AtTargetMode := not ARelative;
  FMoveState.MoveVector := V;

  State := csMove;
end;

procedure TGameUnit.StopMove;
begin
  if State = csMove then State := csNone;
end;

procedure TGameUnit.SetTargetAngle(const ATargetAngle: Single);
begin
  FTargetAngle := ATargetAngle;
end;

procedure TGameUnit.UpdateStates(ASender: TObject);

  //function GetFriendList(): TInt64Arr;
  //begin
  //  SetLength(Result, 1);
  //  Result[0] := Self.ID;
  //end;

  function RunByDirection: string;
  type
    TMoveAnimation = record
      dir: TVec2;
      name: String;
    end;
  const
    MoveAnimations: array [0..7] of TMoveAnimation =
      (
        (dir: (x: 1; y:  0); name: 'run_forw'),
        (dir: (x:-1; y:  0); name: 'run_back'),
        (dir: (x: 0; y:  1); name: 'run_left'),
        (dir: (x: 0; y: -1); name: 'run_right'),
        (dir: (x: -0.7071; y:  0.7071); name: 'run_back_left'),
        (dir: (x: -0.7071; y: -0.7071); name: 'run_back_right'),
        (dir: (x:  0.7071; y:  0.7071); name: 'run_forw_left'),
        (dir: (x:  0.7071; y: -0.7071); name: 'run_forw_right')
      );
  var i: Integer;
      dir: TVec2;
      maxDot, currDot: Single;
      maxDotIndex: Integer;
  begin
    dir := FMoveState.MoveVector;
    if FMoveState.AtTargetMode then dir := dir - FMoveState.MoveVector;
    dir := Rotate(dir, -Rot);
    maxDot := -10;
    maxDotIndex := -1;
    for i := 0 to 3 do
    begin
      currDot := Dot(dir, MoveAnimations[i].dir);
      if maxDot < currDot then
      begin
        maxDot := currDot;
        maxDotIndex := i;
      end;
    end;
    if maxDotIndex >= 0 then
      Result := MoveAnimations[maxDotIndex].name
    else
      Result := 'idle0';
  end;

  procedure UpdateOffsetsFromAnimations(const NewTime: Int64);

    procedure UpdateOffsets(const AStartTime, ACurrentTime: Integer; const AOffsets: TVec3Arr);
    const AnimSpeed = 30;
    var OldFrameIdx, NewFrameIdx: Integer;
        dv: TVec3;
    begin
      OldFrameIdx := (ACurrentTime) * AnimSpeed div 1000;
      NewFrameIdx := (NewTime-AStartTime) * AnimSpeed div 1000;

      OldFrameIdx := Max(0, OldFrameIdx);
      NewFrameIdx := Max(0, NewFrameIdx);
      OldFrameIdx := Min(OldFrameIdx, Length(AOffsets)-1);
      NewFrameIdx := Min(NewFrameIdx, Length(AOffsets)-1);
      if NewFrameIdx > OldFrameIdx then
      begin
        dv := AOffsets[NewFrameIdx] - AOffsets[OldFrameIdx];
        dv := Quat(Vec(0,1,0), Rot) * dv;
        Pos := Pos + Vec(dv.x, 0, dv.z);
      end;
    end;

  begin
    if (State = csAttack) and (Length(FAttackActive.AnimationOffset)>0) then
      UpdateOffsets(FAttackState.StartTime, FAttackState.CurrentTime, FAttackActive.AnimationOffset);
    if (State = csJump) and (Length(FJumpActive.AnimationOffset) > 0) then
      UpdateOffsets(FJumpState.StartTime, FJumpState.CurrentTime, FJumpActive.AnimationOffset);
  end;

var curtime, endtime, dtime: Int64;
    tn: Single;
    movedir: TVec2;
    vel: TVec3;
    item: TGObject;
    q: Integer;
    //bullet: TBulletObject;
    EDDamage, HPDamage: Integer;
begin
  if Freezed then Exit;

  curtime := World.GetWorldTime;
  FAnimation.SetTime(curtime);

  dtime := curtime - FLastUpdateTime;
  if dtime <= 0 then Exit;
  try
    inherited;

    UpdateOffsetsFromAnimations(curtime);

    case State of
      csNone   :
        begin
          FAnimation.AnimationStartAndStopOther(['idle0']);
        end;
      csMove   :
        begin
          FAnimation.AnimationStartAndStopOther([RunByDirection()]);
        end;
      csWalk   : ;
      csJump   :
        begin
          FJumpState.CurrentTime := curtime - FJumpState.StartTime;
          if FJumpState.CurrentTime >= FJumpActive.Duration then
            State := csNone;
        end;
      csAttack :
        begin
          endtime := FAttackState.StartTime + FAttackActive.Duration;
          if endtime < curtime then
          begin
            //FAttackState.DamagedUnits := nil;
            State := csNone;
          end
          else
          begin
            //case FAttackActive.WeaponType of
            //  wtBow :
            //    begin
            //      if (FAttackState.CurrentTime < FAttackActive.DamageTime.x) and
            //         (curtime - FAttackState.StartTime >= FAttackActive.DamageTime.x) then
            //      begin
            //        {$IfDef ServerSide}
            //        bullet := World.CreateGameObject(TBulletObject) as TBulletObject;
            //        bullet.GOID := 10;
            //        bullet.FriendList := GetFriendList;
            //        bullet.Position := Position;
            //        bullet.Angle := Angle;
            //        bullet.LifeTime := 1000;
            //        vel.xy := VecSinCos(bullet.Angle)*0.15;
            //        vel.z := Len(vel.xy) / Len(FAttackState.AttackTarget.xy - Position.xy);
            //        vel.z := vel.z * (FAttackState.AttackTarget.z - Position.z);
            //        bullet.Velocity := vel;
            //        GetCurrentWeaponDamage(HPDamage, EDDamage);
            //        bullet.EDDamage := EDDamage;
            //        bullet.HPDamage := HPDamage;
            //        bullet.CreatorID := ID;
            //        bullet.BumpStateID;
            //        //WriteLn('Create bullet ', bullet.ID);
            //        {$EndIf}
            //      end;
            //      FAttackState.CurrentTime := curtime - FAttackState.StartTime;
            //    end;
            //  wtSword:
            //    begin
            //      FAttackState.CurrentTime := curtime - FAttackState.StartTime;
            //      if (FAttackState.CurrentTime >= FAttackActive.DamageTime.x) and
            //         (FAttackState.CurrentTime <= FAttackActive.DamageTime.y) then
            //      begin
            //        ProcessAttack();
            //      end;
            //    end;
            //end;
            FAttackState.CurrentTime := curtime - FAttackState.StartTime;
          end;
        end;
      csDefence:
        begin
          FBlockState.CurrentTime := curtime;
          FBlockState.BlockActive := FBlockState.StartTime + FBlockActive.SetupTime < curtime;
        end;
      csStunImpact:
        begin
          if FImpactState.StopTime <= curtime then
          begin
            case FImpactState.StunType of
              sitBlock:
                begin
                  FBlockState.CurrentTime := curtime;
                  FBlockState.BlockActive := True;
                  State := csDefence;
                end;
              sitHigh, sitLow:
                State := csNone;
            end;
          end;
        end;
      csPicking:
        begin
          //item := World.FindObject(FPickingState.PickingObject);
          //if (item = nil) or (not (item is TItemObject)) then
          //  State := csNone
          //else
          //  if FPickingState.StartTime + 500 <= curtime then
          //  begin
          //    if TryPickToInventory(TItemObject(item)) then
          //    begin
          //      {$IfDef ServerSide}
          //      if TItemObject(item).Quantity = 0 then
          //        World.DestroyGameObject(item);
          //      {$EndIf}
          //      World.GameEvents.InventoryChanged(Self, ikBag);
          //    end;
          //    State := csNone;
          //  end;
        end;
      csEating :
        begin
          //if FEatingState.StartTime + 500 <= curtime then
          //begin
          //  if CanBeEated(FEatingState.InvKind, FEatingState.InvSlot) then
          //  begin
          //    q := 1;
          //    ApplyFood(GetInventory(FEatingState.InvKind).Item(FEatingState.InvSlot).GOID, q);
          //    GetInventory(FEatingState.InvKind).TryDel(FEatingState.InvSlot, q);
          //    World.GameEvents.InventoryChanged(Self, FEatingState.InvKind);
          //  end;
          //  State := csNone;
          //end;
        end;
    end;
    FED := Min(FMaxED, FED + 1);
  finally
    FLastUpdateTime := curtime;
  end;
end;

procedure TGameUnit.UpdateStep;
begin
  inherited UpdateStep;
  UpdateStates(Self);
end;

//function TGameUnit.GetInventory(const AKind: TInventoryKind): IUnitInventory;
//begin
//  Result := nil;
//end;

constructor TGameUnit.Create(const AWorld: TGWorld);
begin
  inherited Create(AWorld);
  ViewDistance := 15;
  //ViewAngle    := 0.13;
  ViewAngle    := 0.5;

  FMaxED := 1000;
  FMaxHP := 100;
  FMaxST := 100;

  FHP := FMaxHP;
  FST := FMaxST;
  FED := FMaxED;
end;

procedure TGameUnit.SetAttack(const AttackType: Integer; const AAttackTarget: TVec3);
begin
  if not (State in [csNone, csMove, csWalk, csDefence, csPicking, csInteractiveAction]) then Exit;

  FAttackState.StartTime := World.GetWorldTime;
  FAttackState.CurrentTime := FAttackState.StartTime;
  FAttackState.AttackIndex := AttackType;
  FAttackState.AttackTarget := AAttackTarget;
  //FAttackState.WeaponID := ActiveWeapon;

  //FAttackActive := GetAttackInfo(AttackType, FAttackState.WeaponID);

  State := csAttack;
end;

procedure TGameUnit.SetBlock;
begin
  if not (State in [csNone, csMove, csWalk, csDefence, csPicking, csInteractiveAction]) then Exit;
  FBlockActive := GetBlockInfo();
  State := csDefence;
end;

procedure TGameUnit.SetJump(const ADirection: TVec2; const AJumpStyle: Integer);
begin
  if not (State in [csNone, csMove, csWalk, csDefence, csPicking, csInteractiveAction]) then Exit;
  if LenSqr(ADirection) = 0 then Exit;
  Rot := arctan2(ADirection.y, ADirection.x);
  FJumpState.StartTime := World.GetWorldTime;
  FJumpState.JumpStyle := AJumpStyle;
  FJumpState.Direction := ADirection;
  FJumpActive := GetJumpInfo(AJumpStyle);
  State := csJump;
end;

procedure TGameUnit.ReleaseBlock;
begin
  if State = csDefence then State := csNone;
end;

//function TGameUnit.TryPick(const AItem: TItemObject): Boolean;
//begin
//  Result := False;
//  if not (State in [csNone, csMove, csWalk]) then Exit;
//  FVelocity.xy := Vec(0, 0);
//  if DistanceTo(AItem) < PICK_DISTANCE then
//  begin
//    FPickingState.PickingObject := AItem.ID;
//    FPickingState.StartTime := World.GetWorldTime;
//    Result := True;
//    State := csPicking;
//  end;
//end;
//
//function TGameUnit.TryEat(const AInvKind: TInventoryKind; const ASlotIndex: Integer): Boolean;
//begin
//  Result := False;
//  if not (State in [csNone, csMove, csWalk]) then Exit;
//  FVelocity.xy := Vec(0, 0);
//  CanBeEated(AInvKind, ASlotIndex);
//  FEatingState.StartTime := World.GetWorldTime;
//  FEatingState.InvKind := AInvKind;
//  FEatingState.InvSlot := ASlotIndex;
//  State := csEating;
//end;

function TGameUnit.See(const APos: TVec3): Boolean;
var
  dir: TVec3;
  distSqr: Single;
begin
  Result := False;
  dir := APos - Pos;

  distSqr := LenSqr(dir.xy);
  if distSqr > sqr(ViewDistance) then Exit;
  {
  if distSqr <> 0 then
  begin
    if distSqr > Size*8 then
    begin
      dir := dir / sqrt(distSqr);
      if ViewAngle > dot(dir, normalize(LookDir)) then Exit;
    end;
  end;
  }
  Result := True;
end;

//function TGameUnit.See(const AObj: TGameObject): Boolean;
//begin
//  if AObj = nil then Exit(False);
//  Result := See(AObj.Position);
//end;

//function TGameUnit.GetVisibleObjectIDs: IGameObjectIDsSet;
//var objs: IGameObjectsArr;
//    obj : TGameObject;
//    i: Integer;
//begin
//  Result := TGameObjectIDsSet.Create();
//  objs := World.GetAllObjects(); //todo optimize
//  for i := 0 to objs.Count - 1 do
//  begin
//    obj := objs[i];
//    if See(obj) then Result.Add(obj.ID);
//  end;
//end;
//
//function TGameUnit.GetVisibleObjects: IGameObjectsSet;
//var objs: IGameObjectsArr;
//    obj : TGameObject;
//    i: Integer;
//begin
//  Result := TGameObjectsSet.Create();
//  objs := World.GetAllObjects(); //todo optimize
//  for i := 0 to objs.Count - 1 do
//  begin
//    obj := objs[i];
//    if See(obj) then Result.Add(obj);
//  end;
//end;

end.


