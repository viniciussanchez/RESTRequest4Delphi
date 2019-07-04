unit RESTRequest4D.Request.Authentication;

interface

uses RESTRequest4D.Request.Authentication.Intf, REST.Client, REST.Authenticator.Basic;

type
  TRequestAuthentication = class(TInterfacedObject, IRequestAuthentication)
  private
    FRESTClient: TRESTClient;
    FHTTPBasicAuthenticator: THTTPBasicAuthenticator;
    function Clear: IRequestAuthentication;
    function SetPassword(const APassword: string): IRequestAuthentication;
    function SetUsername(const AUser: string): IRequestAuthentication;
    function GetPassword: string;
    function GetUsername: string;
  public
    constructor Create(const ARESTClient: TRESTClient);
    destructor Destroy; override;
  end;

implementation

uses System.SysUtils;

{ TRequestAuthentication }

function TRequestAuthentication.Clear: IRequestAuthentication;
begin
  FRESTClient.Authenticator := nil;
  FHTTPBasicAuthenticator.Password := EmptyStr;
  FHTTPBasicAuthenticator.Username := EmptyStr;
end;

constructor TRequestAuthentication.Create(const ARESTClient: TRESTClient);
begin
  FHTTPBasicAuthenticator := THTTPBasicAuthenticator.Create(nil);
  FRESTClient := ARESTClient;
end;

destructor TRequestAuthentication.Destroy;
begin
  FreeAndNil(FHTTPBasicAuthenticator);
  inherited;
end;

function TRequestAuthentication.GetPassword: string;
begin
  Result := FHTTPBasicAuthenticator.Password;
end;

function TRequestAuthentication.SetPassword(const APassword: string): IRequestAuthentication;
begin
  Result := Self;
  if not Assigned(FRESTClient.Authenticator) then
    FRESTClient.Authenticator := FHTTPBasicAuthenticator;
  FHTTPBasicAuthenticator.Password := APassword;
end;

function TRequestAuthentication.SetUsername(const AUser: string): IRequestAuthentication;
begin
  Result := Self;
  if not Assigned(FRESTClient.Authenticator) then
    FRESTClient.Authenticator := FHTTPBasicAuthenticator;
  FHTTPBasicAuthenticator.Username := AUser;
end;

function TRequestAuthentication.GetUsername: string;
begin
  Result := FHTTPBasicAuthenticator.Username;
end;

end.
