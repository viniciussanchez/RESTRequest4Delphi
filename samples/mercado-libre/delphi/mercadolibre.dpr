program mercadolibre;

uses
  Vcl.Forms,
  Mercado.Libre.View in 'src\Mercado.Libre.View.pas' {FrmMercadoLibre},
  Mercado.Libre.Consts in 'src\Mercado.Libre.Consts.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMercadoLibre, FrmMercadoLibre);
  Application.Run;
end.
