unit MainView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmMainView = class(TForm)
    btnGetUserInfo: TButton;
    memoResponse: TMemo;
    editUserID: TEdit;
    editAccess_Token: TEdit;
    procedure btnGetUserInfoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMainView: TfrmMainView;

implementation

{$R *.dfm}

uses
  REST.Types,
  RESTRequest4D,
  MeLi.Consts;

procedure TfrmMainView.btnGetUserInfoClick(Sender: TObject);
var
  LResponse: RESTRequest4D.IResponse;
begin
  if Trim(editUserID.Text) = EmptyStr then
    Exit;
  if Trim(editAccess_Token.Text) = EmptyStr then
    Exit;

  LResponse := RESTRequest4D.TRequest.New
    .BaseURL(ML_APIBASE)
    .Resource(ML_GET_USR_INF)
    .AddUrlSegment('cust_id',editUserID.Text)
    .Token('Bearer '+editAccess_Token.Text)
    .Accept(REST.Types.CONTENTTYPE_APPLICATION_JSON)
    .RaiseExceptionOn500(True)
    .Get;

  memoResponse.Text := LResponse.Content;

end;

end.
