program Samples;

uses
  Vcl.Forms,
  Samples.Main in 'src\Samples.Main.pas' {FrmMain},
  RESTRequest4D.Request in '..\..\src\core\RESTRequest4D.Request.pas',
  RESTRequest4D.Request.Intf in '..\..\src\interfaces\RESTRequest4D.Request.Intf.pas',
  RESTRequest4D.Response.Intf in '..\..\src\interfaces\RESTRequest4D.Response.Intf.pas',
  RESTRequest4D.Response in '..\..\src\core\RESTRequest4D.Response.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
