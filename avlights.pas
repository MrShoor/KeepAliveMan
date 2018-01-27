unit avLights;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avBase, avRes, avTypes, avContnrs, mutils;

type
  IGeometryRenderer = interface
    procedure DrawGeometry();
    procedure DrawTransparentGeometry();
  end;

  TavLightRenderer = class;

  { TavLightSource }

  TavLightSource = class(TavObject)
  private
  protected
    function LightRenderer: TavLightRenderer; inline;
    function CanRegister(target: TavObject): boolean; override;
    procedure InvalidateLight;
  public
    constructor Create(AParent: TavObject); override;
    destructor Destroy; override;
  end;
  IavLightArr = {$IfDef FPC}specialize{$EndIf}IArray<TavLightSource>;
  TavLightArr = {$IfDef FPC}specialize{$EndIf}TArray<TavLightSource>;
  IavLightSet = {$IfDef FPC}specialize{$EndIf}IHashSet<TavLightSource>;
  TavLightSet = {$IfDef FPC}specialize{$EndIf}THashSet<TavLightSource>;

  { TavPointLight }

  TavPointLight = class(TavLightSource)
  private
    FColor: TVec3;
    FPos: TVec3;
    FRadius: Single;
    procedure SetColor(const AValue: TVec3);
    procedure SetPos(const AValue: TVec3);
    procedure SetRadius(const AValue: Single);
  public
    property Pos   : TVec3  read FPos    write SetPos;
    property Radius: Single read FRadius write SetRadius;
    property Color : TVec3  read FColor  write SetColor;
  end;

  { TavLightRenderer }

  TavLightRenderer = class (TavMainRenderChild)
  private
    FLights: IavLightSet;
    FInvalidLights: IavLightSet;

    procedure ValidateLights;
  public
    function AddPointLight(): TavPointLight;

    function  LightsCount: Integer;
    procedure Reset;
    function  Next(out ALight: TavLightSource): Boolean;

    procedure Render(const ARenderer: IGeometryRenderer);

    procedure AfterConstruction; override;
  end;

implementation

{ TavPointLight }

procedure TavPointLight.SetPos(const AValue: TVec3);
begin
  if FPos = AValue then Exit;
  FPos := AValue;
  InvalidateLight;
end;

procedure TavPointLight.SetColor(const AValue: TVec3);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
  InvalidateLight;
end;

procedure TavPointLight.SetRadius(const AValue: Single);
begin
  if FRadius = AValue then Exit;
  FRadius := AValue;
  InvalidateLight;
end;

{ TavLightSource }

function TavLightSource.LightRenderer: TavLightRenderer;
begin
  Result := TavLightRenderer(Parent);
end;

function TavLightSource.CanRegister(target: TavObject): boolean;
begin
  Result := target is TavLightRenderer;
end;

procedure TavLightSource.InvalidateLight;
begin
  LightRenderer.FInvalidLights.Add(Self);
end;

constructor TavLightSource.Create(AParent: TavObject);
begin
  inherited Create(AParent);
  LightRenderer.FLights.Add(Self);
end;

destructor TavLightSource.Destroy;
begin
  inherited Destroy;
  LightRenderer.FLights.Delete(Self);
end;

{ TavLightRenderer }

procedure TavLightRenderer.ValidateLights;
begin
  FInvalidLights.Clear;
end;

function TavLightRenderer.AddPointLight: TavPointLight;
begin
  Result := TavPointLight.Create(Self);
end;

function TavLightRenderer.LightsCount: Integer;
begin
  Result := FLights.Count;
end;

procedure TavLightRenderer.Reset;
begin
  FLights.Reset;
end;

function TavLightRenderer.Next(out ALight: TavLightSource): Boolean;
begin
  Result := FLights.Next(ALight);
end;

procedure TavLightRenderer.Render(const ARenderer: IGeometryRenderer);
begin

end;

procedure TavLightRenderer.AfterConstruction;
begin
  inherited AfterConstruction;
  FLights := TavLightSet.Create();
  FInvalidLights := TavLightSet.Create();
end;

end.

