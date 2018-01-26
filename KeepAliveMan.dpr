program KeepAliveMan;

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {frmMain},
  gWorld in 'gWorld.pas',
  gRenderCommon in 'gRenderCommon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
