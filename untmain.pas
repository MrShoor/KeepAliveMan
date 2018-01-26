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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus;

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
  private

  public
    {$IfDef FPC}
    procedure EraseBackground(DC: HDC); override;
    {$EndIf}
    {$IfDef DCC}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    {$EndIf}
    procedure RenderScene;
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

{$IfDef FPC}
procedure TfrmMain.EraseBackground(DC: HDC);
begin
//  inherited EraseBackground(DC);
end;
{$EndIf}

{$IfDef DCC}
procedure TfrmMain.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;
{$EndIf}

procedure TfrmMain.RenderScene;
begin

end;

end.

