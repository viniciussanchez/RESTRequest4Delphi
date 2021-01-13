program Samples;

uses
  Vcl.Forms,
  Samples.Main in 'src\Samples.Main.pas' {FrmMain},
  RESTRequest4D in '..\..\src\RESTRequest4D.pas',
  RESTRequest4D.Request.Client in '..\..\src\RESTRequest4D.Request.Client.pas',
  RESTRequest4D.Request.Contract in '..\..\src\RESTRequest4D.Request.Contract.pas',
  RESTRequest4D.Request.Indy in '..\..\src\RESTRequest4D.Request.Indy.pas',
  RESTRequest4D.Request.NetHTTP in '..\..\src\RESTRequest4D.Request.NetHTTP.pas',
  RESTRequest4D.Response.Client in '..\..\src\RESTRequest4D.Response.Client.pas',
  RESTRequest4D.Response.Contract in '..\..\src\RESTRequest4D.Response.Contract.pas',
  RESTRequest4D.Response.Indy in '..\..\src\RESTRequest4D.Response.Indy.pas',
  RESTRequest4D.Response.NetHTTP in '..\..\src\RESTRequest4D.Response.NetHTTP.pas',
  RESTRequest4D.Utils in '..\..\src\RESTRequest4D.Utils.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
