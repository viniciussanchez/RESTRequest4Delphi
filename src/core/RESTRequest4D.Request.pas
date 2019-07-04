unit RESTRequest4D.Request;

interface

uses RESTRequest4D.Request.Intf, Data.DB, REST.Client, REST.Response.Adapter, RESTRequest4D.Request.Params.Intf, REST.Types,
  RESTRequest4D.Request.Body.Intf, RESTRequest4D.Request.Authentication.Intf;

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
    procedure DoJoinComponents;
    function SetDataSetAdapter(const ADataSet: TDataSet): IRequest;
    function SetBaseURL(const ABaseURL: string = ''): IRequest;
    function SetResource(const AResource: string = ''): IRequest;
    function SetResourceSuffix(const AResourceSuffix: string = ''): IRequest;
    function SetMethod(const AMethod: TRESTRequestMethod = rmGET): IRequest;
    function GetFullRequestURL(const AIncludeParams: Boolean = True): string;
    function GetMethod: TRESTRequestMethod;
    function GetResourceSuffix: string;
    function GetResource: string;
    function GetBaseURL: string;
    function GetDataSetAdapter: TDataSet;
    function Execute: Integer;
    function Body: IRequestBody;
    function Params: IRequestParams;
    function Authentication: IRequestAuthentication;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses RESTRequest4D.Request.Body, RESTRequest4D.Request.Params, System.SysUtils, RESTRequest4D.Request.Authentication;

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
  FRESTClient := TRESTClient.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);

  FBody := TRequestBody.Create(FRESTRequest);
  FParams := TRequestParams.Create(FRESTRequest);

  DoJoinComponents;
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

procedure TRequest.DoJoinComponents;
begin
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
end;

function TRequest.Execute: Integer;
begin
  FRESTRequest.Execute;
  Result := FRESTResponse.StatusCode;
end;

function TRequest.GetBaseURL: string;
begin
  Result := FRESTClient.BaseURL;
end;

function TRequest.GetDataSetAdapter: TDataSet;
begin
  Result := nil;
  if Assigned(FRESTResponseDataSetAdapter) then
    Result := FRESTResponseDataSetAdapter.Dataset;
end;

function TRequest.GetFullRequestURL(const AIncludeParams: Boolean): string;
begin
  Result := FRESTRequest.GetFullRequestURL(AIncludeParams);
end;

function TRequest.GetMethod: TRESTRequestMethod;
begin
  Result := FRESTRequest.Method;
end;

function TRequest.GetResource: string;
begin
  Result := FRESTRequest.Resource;
end;

function TRequest.GetResourceSuffix: string;
begin
  Result := FRESTRequest.ResourceSuffix;
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
