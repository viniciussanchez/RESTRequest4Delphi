unit RESTRequest4D.Request.Authentication;

interface

uses RESTRequest4D.Request.Authentication.Intf, REST.Client, REST.Authenticator.Basic;

type
  TRequestAuthentication = class(TInterfacedObject, IRequestAuthentication)
  private
    FHTTPBasicAuthenticator: THTTPBasicAuthenticator;
    function Password(const APassword: string): IRequestAuthentication;
    function Username(const AUser: string): IRequestAuthentication;
  public
    constructor Create(const ARESTClient: TRESTClient);
    destructor Destroy; override;
  end;

implementation

uses System.SysUtils;

{ TRequestAuthentication }

constructor TRequestAuthentication.Create(const ARESTClient: TRESTClient);
begin
  FHTTPBasicAuthenticator := THTTPBasicAuthenticator.Create(nil);
  ARESTClient.Authenticator := FHTTPBasicAuthenticator;
end;

destructor TRequestAuthentication.Destroy;
begin
  FreeAndNil(FHTTPBasicAuthenticator);
  inherited;
end;

function TRequestAuthentication.Password(const APassword: string): IRequestAuthentication;
begin
  Result := Self;
  FHTTPBasicAuthenticator.Password := APassword;
end;

function TRequestAuthentication.Username(const AUser: string): IRequestAuthentication;
begin
  Result := Self;
  FHTTPBasicAuthenticator.Username := AUser;
end;

end.
