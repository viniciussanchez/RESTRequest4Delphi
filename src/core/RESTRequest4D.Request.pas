unit RESTRequest4D.Request;

interface

uses RESTRequest4D.Request.Intf, StatusCode.Types, Data.DB, REST.Client, REST.Response.Adapter, RESTRequest4D.Request.Params.Intf,
  RESTRequest4D.Request.Body.Intf, RESTRequest4D.Request.Authentication.Intf, REST.Types;

type
  TRequest = class(TInterfacedObject, IRequest)
  private
    FBody: IRequestBody;
    FParams: IRequestParams;
    FAuthentication: IRequestAuthentication;
    FRESTRequest: TRESTRequest;
    FRESTResponseDataSetAdapter: TRESTResponseDataSetAdapter;
    FRESTResponse: TRESTResponse;
    FRESTClient: TRESTClient;
    function SetDataSetAdapter(const ADataSet: TDataSet): IRequest;
    function SetBaseURL(const ABaseURL: string = ''): IRequest;
    function SetResource(const AResource: string = ''): IRequest;
    function SetResourceSuffix(const AResourceSuffix: string = ''): IRequest;
    function SetMethod(const AMethod: TRESTRequestMethod = rmGET): IRequest;
    function Execute(const AWaitMessage: string): TStatusCode; overload;
    function Execute: TStatusCode; overload;
    function Body: IRequestBody;
    function Params: IRequestParams;
    function Authentication: IRequestAuthentication;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses RESTRequest4D.Request.Body, RESTRequest4D.Request.Params, System.SysUtils, VCL.Wait, RESTRequest4D.Request.Authentication;

{ TRequest }

function TRequest.Authentication: IRequestAuthentication;
begin
  if not Assigned(FAuthentication) then
    FAuthentication := TRequestAuthentication.Create(FRESTClient);
  Result := FAuthentication;
end;

function TRequest.Body: IRequestBody;
begin
  Result := FBody;
end;

constructor TRequest.Create;
begin
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTResponse.ContentType := 'application/json';

  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.AcceptCharset := 'utf-8, *;q=0.8';
  FRESTClient.Accept := 'application/json, text/plain; q=0.9, text/html;q=0.8,';

  FRESTRequest := TRESTRequest.Create(nil);
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;

  FBody := TRequestBody.Create(FRESTRequest);
  FParams := TRequestParams.Create(FRESTRequest);
end;

destructor TRequest.Destroy;
begin
  FBody := nil;
  FAuthentication := nil;
  FParams := nil;
  if Assigned(FRESTResponseDataSetAdapter) then
    FreeAndNil(FRESTResponseDataSetAdapter);
  FreeAndNil(FRESTRequest);
  FreeAndNil(FRESTClient);
  FreeAndNil(FRESTResponse);
  inherited;
end;

function TRequest.Execute: TStatusCode;
begin
  FRESTRequest.Execute;
  Result := TStatusCode(FRESTResponse.StatusCode);
end;

function TRequest.Execute(const AWaitMessage: string): TStatusCode;
begin
  TWait.Create(AWaitMessage).Start(
    procedure
    begin
      FRESTRequest.Execute;
    end);
  Result := TStatusCode(FRESTResponse.StatusCode);
end;

function TRequest.Params: IRequestParams;
begin
  Result := FParams;
end;

function TRequest.SetBaseURL(const ABaseURL: string = ''): IRequest;
begin
  Result := Self;
  FRESTClient.BaseURL := ABaseURL;
end;

function TRequest.SetDataSetAdapter(const ADataSet: TDataSet): IRequest;
begin
  Result := Self;
  if not Assigned(ADataSet) then
    Exit;
  if not Assigned(FRESTResponseDataSetAdapter) then
  begin
    FRESTResponseDataSetAdapter := TRESTResponseDataSetAdapter.Create(nil);
    FRESTResponseDataSetAdapter.Response := FRESTResponse;
  end;
  FRESTResponseDataSetAdapter.Dataset := ADataSet;
end;

function TRequest.SetMethod(const AMethod: TRESTRequestMethod): IRequest;
begin
  Result := Self;
  FRESTRequest.Method := AMethod;
end;

function TRequest.SetResource(const AResource: string = ''): IRequest;
begin
  Result := Self;
  FRESTRequest.Resource := AResource;
end;

function TRequest.SetResourceSuffix(const AResourceSuffix: string = ''): IRequest;
begin
  Result := Self;
  FRESTRequest.ResourceSuffix := AResourceSuffix;
end;

end.
