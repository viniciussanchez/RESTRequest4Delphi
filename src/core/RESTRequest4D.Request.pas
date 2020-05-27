unit RESTRequest4D.Request;

interface

uses RESTRequest4D.Request.Intf, Data.DB, REST.Client, REST.Response.Adapter, RESTRequest4D.Request.Params.Intf, REST.Types,
  RESTRequest4D.Request.Body.Intf, RESTRequest4D.Request.Authentication.Intf, System.SysUtils, RESTRequest4D.Request.Headers.Intf,
  RESTRequest4D.Request.Response.Intf, System.JSON;

type
  IRequestAuthentication = RESTRequest4D.Request.Authentication.Intf.IRequestAuthentication;
  IRequestBody = RESTRequest4D.Request.Body.Intf.IRequestBody;
  IRequestHeaders = RESTRequest4D.Request.Headers.Intf.IRequestHeaders;
  IRequest = RESTRequest4D.Request.Intf.IRequest;
  IRequestParams = RESTRequest4D.Request.Params.Intf.IRequestParams;
  IRequestResponse = RESTRequest4D.Request.Response.Intf.IRequestResponse;

  TRequest = class(TInterfacedObject, IRequest)
  private
    FBody: IRequestBody;
    FParams: IRequestParams;
    FResponse: IRequestResponse;
    FHeaders: IRequestHeaders;
    FAuthentication: IRequestAuthentication;
    FRESTRequest: TRESTRequest;
    FDataSetAdapter: TDataSet;
    FRESTResponse: TRESTResponse;
    FRESTClient: TRESTClient;
    FToken: string;
    procedure DoJoinComponents;
    procedure DoAfterExecute(Sender: TCustomRESTRequest);
    procedure ActiveCachedUpdates(const ADataSet: TDataSet; const AActive: Boolean = True);
    function GetAcceptEncoding: string;
    function SetAcceptEncoding(const AAcceptEncoding: string): IRequest;
    function GetAcceptCharset: string;
    function SetAcceptCharset(const AAcceptCharset: string): IRequest;
    function GetAccept: string;
    function SetAccept(const AAccept: string): IRequest;
    function SetDataSetAdapter(const ADataSet: TDataSet): IRequest;
    function SetBaseURL(const ABaseURL: string = ''): IRequest;
    function SetResource(const AResource: string = ''): IRequest;
    function SetResourceSuffix(const AResourceSuffix: string = ''): IRequest;
    function SetMethod(const AMethod: TRESTRequestMethod = rmGET): IRequest;
    function SetRaiseExceptionOn500(const ARaiseException: Boolean = True): IRequest;
    function SetToken(const AToken: string): IRequest;
    function GetRaiseExceptionOn500: Boolean;
    function GetFullRequestURL(const AIncludeParams: Boolean = True): string;
    function GetTimeout: Integer;
    function SetTimeout(const ATimeout: Integer): IRequest;
    function GetMethod: TRESTRequestMethod;
    function GetResourceSuffix: string;
    function GetResource: string;
    function GetBaseURL: string;
    function GetDataSetAdapter: TDataSet;
    function GetToken: string;
    function Get: IRequest;
    function Post: IRequest;
    function Put: IRequest;
    function Delete: IRequest;
    function Execute: Integer;
    function Body: IRequestBody;
    function ClearBody: IRequest;
    function AddBody(const AContent: string; const AContentType: TRESTContentType = ctNone): IRequest; overload;
    function AddBody(const AContent: TJSONObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TJSONArray; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TObject; const AOwns: Boolean = True): IRequest; overload;
    function Headers: IRequestHeaders;
    function Response: IRequestResponse;
    function Params: IRequestParams;
    function Authentication: IRequestAuthentication;
    function ExecuteAsync(ACompletionHandler: TProc = nil; ASynchronized: Boolean = True; AFreeThread: Boolean = True; ACompletionHandlerWithError: TProc<TObject> = nil): TRESTExecutionThread;
  public
    class function New: IRequest;
    constructor Create(const ABaseURL: string; const AToken: string = ''); overload;
    constructor Create(const AMethod: TRESTRequestMethod = rmGET; const ABaseURL: string = ''; const AToken: string = ''); overload;
    destructor Destroy; override;
  end;

implementation

uses RESTRequest4D.Request.Body, RESTRequest4D.Request.Params, RESTRequest4D.Request.Authentication, DataSet.Serialize,
  RESTRequest4D.Request.Headers, System.Generics.Collections, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  RESTRequest4D.Request.Response;

{ TRequest }

procedure TRequest.ActiveCachedUpdates(const ADataSet: TDataSet; const AActive: Boolean = True);
var
  LDataSet: TDataSet;
  LDataSetDetails: TList<TDataSet>;
begin
  LDataSetDetails := TList<TDataSet>.Create;
  try
    if ADataSet is TFDMemTable then
    begin
      if not AActive then
        TFDMemTable(ADataSet).Close;
      TFDMemTable(ADataSet).CachedUpdates := AActive;
      if AActive and (not TFDMemTable(ADataSet).Active) and (TFDMemTable(ADataSet).FieldCount > 0) then
        TFDMemTable(ADataSet).Open;
    end;
    ADataSet.GetDetailDataSets(LDataSetDetails);
    for LDataSet in LDataSetDetails do
      ActiveCachedUpdates(LDataSet, AActive);
  finally
    LDataSetDetails.Free;
  end;
end;

function TRequest.AddBody(const AContent: string; const AContentType: TRESTContentType): IRequest;
begin
  Result := Self;
  Self.Body.Add(AContent, AContentType);
end;

function TRequest.AddBody(const AContent: TJSONObject; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  Self.Body.Add(AContent, AOwns);
end;

function TRequest.AddBody(const AContent: TJSONArray; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  Self.Body.Add(AContent, AOwns);
end;

function TRequest.AddBody(const AContent: TObject; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  Self.Body.Add(AContent, AOwns);
end;

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

constructor TRequest.Create(const ABaseURL, AToken: string);
begin
  Create(rmGET, ABaseURL, AToken);
end;

function TRequest.ClearBody: IRequest;
begin
  Result := Self;
  Self.Body.Clear;
end;

constructor TRequest.Create(const AMethod: TRESTRequestMethod = rmGET; const ABaseURL: string = ''; const AToken: string = '');
begin
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTClient := TRESTClient.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);

  FBody := TRequestBody.Create(FRESTRequest);
  FParams := TRequestParams.Create(FRESTRequest);
  FHeaders := TRequestHeaders.Create(FRESTRequest);
  FResponse := TRequestResponse.Create(FRESTResponse);

  FRESTRequest.OnAfterExecute := DoAfterExecute;
  DoJoinComponents;

  FRESTRequest.Method := AMethod;
  FRESTClient.RaiseExceptionOn500 := False;
  FRESTClient.BaseURL := ABaseURL;
  FToken := AToken;
end;

function TRequest.Delete: IRequest;
begin
  Result := Self;
  Self.SetMethod(TRESTRequestMethod.rmDELETE);
  Self.Execute;
end;

destructor TRequest.Destroy;
begin
  FBody := nil;
  FAuthentication := nil;
  FParams := nil;
  FHeaders := nil;
  FreeAndNil(FRESTRequest);
  FreeAndNil(FRESTClient);
  FreeAndNil(FRESTResponse);
  inherited;
end;

procedure TRequest.DoAfterExecute(Sender: TCustomRESTRequest);
begin
  if not Assigned(FDataSetAdapter) then
    Exit;  
  ActiveCachedUpdates(FDataSetAdapter, False);
  FDataSetAdapter.LoadFromJSON(FRESTResponse.Content);
  ActiveCachedUpdates(FDataSetAdapter);
end;

procedure TRequest.DoJoinComponents;
begin
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
end;

function TRequest.Execute: Integer;
begin
  if not FToken.Trim.IsEmpty then
    FHeaders.Add('Authorization', FToken, [poDoNotEncode]);
  FRESTRequest.Execute;
  Result := FRESTResponse.StatusCode;
end;

function TRequest.ExecuteAsync(ACompletionHandler: TProc; ASynchronized, AFreeThread: Boolean;
  ACompletionHandlerWithError: TProc<TObject>): TRESTExecutionThread;
begin
  if not FToken.Trim.IsEmpty then
    FHeaders.Add('Authorization', FToken, [poDoNotEncode]);
  Result := FRESTRequest.ExecuteAsync(ACompletionHandler, ASynchronized, AFreeThread, ACompletionHandlerWithError);
end;

function TRequest.Get: IRequest;
begin
  Result := Self;
  Self.SetMethod(TRESTRequestMethod.rmGET);
  Self.Execute;
end;

function TRequest.GetAccept: string;
begin
  Result := FRESTRequest.Accept;
end;

function TRequest.GetAcceptCharset: string;
begin
  Result := FRESTRequest.AcceptCharset;
end;

function TRequest.GetAcceptEncoding: string;
begin
  Result := FRESTRequest.AcceptEncoding;
end;

function TRequest.GetBaseURL: string;
begin
  Result := FRESTClient.BaseURL;
end;

function TRequest.GetDataSetAdapter: TDataSet;
begin
  Result := FDataSetAdapter;
end;

function TRequest.GetFullRequestURL(const AIncludeParams: Boolean): string;
begin
  Result := FRESTRequest.GetFullRequestURL(AIncludeParams);
end;

function TRequest.GetMethod: TRESTRequestMethod;
begin
  Result := FRESTRequest.Method;
end;

function TRequest.GetRaiseExceptionOn500: Boolean;
begin
  Result := FRESTClient.RaiseExceptionOn500;
end;

function TRequest.GetResource: string;
begin
  Result := FRESTRequest.Resource;
end;

function TRequest.GetResourceSuffix: string;
begin
  Result := FRESTRequest.ResourceSuffix;
end;

function TRequest.GetTimeout: Integer;
begin
  Result := FRESTRequest.Timeout;
end;

function TRequest.GetToken: string;
begin
  Result := FToken;
end;

function TRequest.Headers: IRequestHeaders;
begin
  Result := FHeaders;
end;

class function TRequest.New: IRequest;
begin
  Result := TRequest.Create;
end;

function TRequest.Params: IRequestParams;
begin
  Result := FParams;
end;

function TRequest.Post: IRequest;
begin
  Result := Self;
  Self.SetMethod(TRESTRequestMethod.rmPOST);
  Self.Execute;
end;

function TRequest.Put: IRequest;
begin
  Result := Self;
  Self.SetMethod(TRESTRequestMethod.rmPUT);
  Self.Execute;
end;

function TRequest.Response: IRequestResponse;
begin
  Result := FResponse;
end;

function TRequest.SetAccept(const AAccept: string): IRequest;
const
  REQUEST_DEFAULT_ACCEPT =
    CONTENTTYPE_APPLICATION_JSON + ', ' +
    CONTENTTYPE_TEXT_PLAIN + '; q=0.9, ' +
    CONTENTTYPE_TEXT_HTML + ';q=0.8,';
begin
  Result := Self;
  FRESTRequest.Accept := REQUEST_DEFAULT_ACCEPT;
  if not AAccept.Trim.IsEmpty then
    FRESTRequest.Accept := AAccept;
end;

function TRequest.SetAcceptCharset(const AAcceptCharset: string): IRequest;
const
  REQUEST_DEFAULT_ACCEPT_CHARSET = 'utf-8, *;q=0.8';
begin
  Result := Self;
  FRESTRequest.AcceptCharset := REQUEST_DEFAULT_ACCEPT_CHARSET;
  if not AAcceptCharset.Trim.IsEmpty then
    FRESTRequest.AcceptCharset := AAcceptCharset;
end;

function TRequest.SetAcceptEncoding(const AAcceptEncoding: string): IRequest;
begin
  Result := Self;
  FRESTRequest.AcceptEncoding := AAcceptEncoding;
end;

function TRequest.SetBaseURL(const ABaseURL: string = ''): IRequest;
begin
  Result := Self;
  FRESTClient.BaseURL := ABaseURL;
end;

function TRequest.SetDataSetAdapter(const ADataSet: TDataSet): IRequest;
begin
  Result := Self;
  FDataSetAdapter := ADataSet;
end;

function TRequest.SetMethod(const AMethod: TRESTRequestMethod): IRequest;
begin
  Result := Self;
  FRESTRequest.Method := AMethod;
  if AMethod = TRESTRequestMethod.rmGET then
    Self.FBody.Clear;
end;

function TRequest.SetRaiseExceptionOn500(const ARaiseException: Boolean = True): IRequest;
begin
  Result := Self;
  FRESTClient.RaiseExceptionOn500 := ARaiseException;
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

function TRequest.SetTimeout(const ATimeout: Integer): IRequest;
begin
  Result := Self;
  FRESTRequest.Timeout := ATimeout;
end;

function TRequest.SetToken(const AToken: string): IRequest;
begin
  Result := Self;
  FToken := AToken;
end;

end.
