unit Mercado.Libre.View;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls;

type
  TFrmMercadoLibre = class(TForm)
    btnGetUserInfo: TButton;
    memoResponse: TMemo;
    editUserID: TEdit;
    editAccess_Token: TEdit;
    procedure btnGetUserInfoClick(Sender: TObject);
  end;

var
  FrmMercadoLibre: TFrmMercadoLibre;

implementation

{$R *.dfm}

uses REST.Types, RESTRequest4D, Mercado.Libre.Consts;

procedure TFrmMercadoLibre.btnGetUserInfoClick(Sender: TObject);
var
  LResponse: RESTRequest4D.IResponse;
begin
  if Trim(editUserID.Text) = EmptyStr then
    Exit;
  if Trim(editAccess_Token.Text) = EmptyStr then
    Exit;

  LResponse := RESTRequest4D.TRequest.New
    .BaseURL(ML_APIBASE)
    .Resource(ML_GET_USR_INF_REGEX)
    .AddUrlSegment('cust_id', editUserID.Text)
    .Token('Bearer ' + editAccess_Token.Text)
    .Accept(REST.Types.CONTENTTYPE_APPLICATION_JSON)
    .RaiseExceptionOn500(True)
    .Get;

  memoResponse.Text := LResponse.Content;
end;

end.
