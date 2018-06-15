unit gTypes;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils;

type

  { TgResource }

  TgResource = packed record
    modelFileName: string;
    modelInst: string;
    function Equal(const ARes: TgResource): boolean;
  end;

implementation

{ TgResource }

function TgResource.Equal(const ARes: TgResource): boolean;
begin
  Result := (modelFileName = ARes.modelFileName) and (modelInst = ARes.modelInst);
end;

end.

