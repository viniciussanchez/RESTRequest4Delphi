program Samples;

uses
  Vcl.Forms,
  Samples.Main in 'src\Samples.Main.pas' {FrmMain},
  RESTRequest4D.Request.Authentication in '..\src\core\RESTRequest4D.Request.Authentication.pas',
  RESTRequest4D.Request.Body in '..\src\core\RESTRequest4D.Request.Body.pas',
  RESTRequest4D.Request.Params in '..\src\core\RESTRequest4D.Request.Params.pas',
  RESTRequest4D.Request in '..\src\core\RESTRequest4D.Request.pas',
  RESTRequest4D.Request.Authentication.Intf in '..\src\interfaces\RESTRequest4D.Request.Authentication.Intf.pas',
  RESTRequest4D.Request.Body.Intf in '..\src\interfaces\RESTRequest4D.Request.Body.Intf.pas',
  RESTRequest4D.Request.Intf in '..\src\interfaces\RESTRequest4D.Request.Intf.pas',
  RESTRequest4D.Request.Params.Intf in '..\src\interfaces\RESTRequest4D.Request.Params.Intf.pas',
  RESTRequest4D.Request.Headers.Intf in '..\src\interfaces\RESTRequest4D.Request.Headers.Intf.pas',
  RESTRequest4D.Request.Headers in '..\src\core\RESTRequest4D.Request.Headers.pas',
  RESTRequest4D.Request.Response.Intf in '..\src\interfaces\RESTRequest4D.Request.Response.Intf.pas',
  RESTRequest4D.Request.Response in '..\src\core\RESTRequest4D.Request.Response.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
