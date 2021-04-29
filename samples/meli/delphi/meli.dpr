program meli;

uses
  Vcl.Forms,
  MainView in 'src\MainView.pas' {frmMainView},
  MeLi.Consts in 'src\MeLi.Consts.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMainView, frmMainView);
  Application.Run;
end.
