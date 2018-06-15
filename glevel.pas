unit gLevel;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, gWorld, gUnit, gTypes;

type

  { TGameLevel }

  TGameLevel = class
  private
    FWorld: TGWorld;
  protected
    FPlayer: TGameUnit;
  public
    property World: TGWorld read FWorld;
    property Player: TGameUnit read FPlayer;

    constructor Create(const AWorld: TGWorld); virtual;
  end;

  { TGameLevel0 }

  TGameLevel0 = class (TGameLevel)
  private
  public
    procedure AfterConstruction; override;
  end;

implementation

{ TGameLevel0 }

procedure TGameLevel0.AfterConstruction;
begin
  inherited AfterConstruction;
end;

{ TGameLevel }

constructor TGameLevel.Create(const AWorld: TGWorld);
var res: TgResource;
begin
  FWorld := AWorld;

  FPlayer := TGamePlayer.Create(FWorld);

  res.modelFileName := 'Character\char.avm';
  res.modelInst := 'Maria_J_J_Ong';
  FPlayer.Res := res;
end;

end.

