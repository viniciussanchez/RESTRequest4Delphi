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
    function AcceptEncoding: string; overload;
    function AcceptEncoding(const AAcceptEncoding: string): IRequest; overload;
    function AcceptCharset: string; overload;
    function AcceptCharset(const AAcceptCharset: string): IRequest; overload;
    function Accept: string; overload;
    function Accept(const AAccept: string): IRequest; overload;
    function DataSetAdapter(const ADataSet: TDataSet): IRequest; overload;
    function DataSetAdapter: TDataSet; overload;
    function BaseURL(const ABaseURL: string): IRequest; overload;
    function BaseURL: string; overload;
    function Resource(const AResource: string): IRequest; overload;
    function Resource: string; overload;
    function ResourceSuffix(const AResourceSuffix: string): IRequest; overload;
    function ResourceSuffix: string; overload;
    function Method(const AMethod: TRESTRequestMethod): IRequest; overload;
    function Method: TRESTRequestMethod; overload;
    function Token(const AToken: string): IRequest;
    function Timeout(const ATimeout: Integer): IRequest; overload;
    function Timeout: Integer; overload;
    function RaiseExceptionOn500(const ARaiseException: Boolean): IRequest; overload;
    function RaiseExceptionOn500: Boolean; overload;
    function FullRequestURL(const AIncludeParams: Boolean = True): string;
    function Get: IRequestResponse;
    function Post: IRequestResponse;
    function Put: IRequestResponse;
    function Delete: IRequestResponse;
    function Execute: Integer;
    function ClearBody: IRequest;
    function AddBody(const AContent: string; const AContentType: TRESTContentType = ctNone): IRequest; overload;
    function AddBody(const AContent: TJSONObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TJSONArray; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TObject; const AOwns: Boolean = True): IRequest; overload;
    function ClearHeaders: IRequest;
    function AddHeader(const AName, AValue: string; const AOptions: TRESTRequestParameterOptions = []): IRequest;
    function Params: IRequestParams;
    function BasicAuthentication(const AUsername, APassword: string): IRequest;
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
  FBody.Add(AContent, AContentType);
end;

function TRequest.AddBody(const AContent: TJSONObject; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  FBody.Add(AContent, AOwns);
end;

function TRequest.AddBody(const AContent: TJSONArray; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  FBody.Add(AContent, AOwns);
end;

function TRequest.AddBody(const AContent: TObject; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  FBody.Add(AContent, AOwns);
end;

function TRequest.AddHeader(const AName, AValue: string; const AOptions: TRESTRequestParameterOptions): IRequest;
begin
  Result := Self;
  FHeaders.Add(AName, AValue, AOptions);
end;

function TRequest.BasicAuthentication(const AUsername, APassword: string): IRequest;
begin
  Result := Self;
  if not Assigned(FAuthentication) then
    FAuthentication := TRequestAuthentication.Create(FRESTClient);
  FAuthentication.Username(AUsername).Password(APassword);
end;

constructor TRequest.Create(const ABaseURL, AToken: string);
begin
  Create(rmGET, ABaseURL, AToken);
end;

function TRequest.ClearBody: IRequest;
begin
  Result := Self;
  FBody.Clear;
end;

function TRequest.ClearHeaders: IRequest;
begin
  Result := Self;
  FHeaders.Clear;
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

function TRequest.Delete: IRequestResponse;
begin
  Result := FResponse;
  Self.Method(TRESTRequestMethod.rmDELETE);
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

function TRequest.Get: IRequestResponse;
begin
  Result := FResponse;
  Self.Method(TRESTRequestMethod.rmGET);
  Self.Execute;
end;

function TRequest.Accept: string;
begin
  Result := FRESTRequest.Accept;
end;

function TRequest.AcceptCharset: string;
begin
  Result := FRESTRequest.AcceptCharset;
end;

function TRequest.AcceptEncoding: string;
begin
  Result := FRESTRequest.AcceptEncoding;
end;

function TRequest.BaseURL: string;
begin
  Result := FRESTClient.BaseURL;
end;

function TRequest.DataSetAdapter: TDataSet;
begin
  Result := FDataSetAdapter;
end;

function TRequest.FullRequestURL(const AIncludeParams: Boolean): string;
begin
  Result := FRESTRequest.GetFullRequestURL(AIncludeParams);
end;

function TRequest.Method: TRESTRequestMethod;
begin
  Result := FRESTRequest.Method;
end;

function TRequest.RaiseExceptionOn500: Boolean;
begin
  Result := FRESTClient.RaiseExceptionOn500;
end;

function TRequest.Resource: string;
begin
  Result := FRESTRequest.Resource;
end;

function TRequest.ResourceSuffix: string;
begin
  Result := FRESTRequest.ResourceSuffix;
end;

function TRequest.Timeout: Integer;
begin
  Result := FRESTRequest.Timeout;
end;

class function TRequest.New: IRequest;
begin
  Result := TRequest.Create;
end;

function TRequest.Params: IRequestParams;
begin
  Result := FParams;
end;

function TRequest.Post: IRequestResponse;
begin
  Result := FResponse;
  Self.Method(TRESTRequestMethod.rmPOST);
  Self.Execute;
end;

function TRequest.Put: IRequestResponse;
begin
  Result := FResponse;
  Self.Method(TRESTRequestMethod.rmPUT);
  Self.Execute;
end;

function TRequest.Accept(const AAccept: string): IRequest;
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

function TRequest.AcceptCharset(const AAcceptCharset: string): IRequest;
const
  REQUEST_DEFAULT_ACCEPT_CHARSET = 'utf-8, *;q=0.8';
begin
  Result := Self;
  FRESTRequest.AcceptCharset := REQUEST_DEFAULT_ACCEPT_CHARSET;
  if not AAcceptCharset.Trim.IsEmpty then
    FRESTRequest.AcceptCharset := AAcceptCharset;
end;

function TRequest.AcceptEncoding(const AAcceptEncoding: string): IRequest;
begin
  Result := Self;
  FRESTRequest.AcceptEncoding := AAcceptEncoding;
end;

function TRequest.BaseURL(const ABaseURL: string): IRequest;
begin
  Result := Self;
  FRESTClient.BaseURL := ABaseURL;
end;

function TRequest.DataSetAdapter(const ADataSet: TDataSet): IRequest;
begin
  Result := Self;
  FDataSetAdapter := ADataSet;
end;

function TRequest.Method(const AMethod: TRESTRequestMethod): IRequest;
begin
  Result := Self;
  FRESTRequest.Method := AMethod;
  if AMethod = TRESTRequestMethod.rmGET then
    Self.FBody.Clear;
end;

function TRequest.RaiseExceptionOn500(const ARaiseException: Boolean): IRequest;
begin
  Result := Self;
  FRESTClient.RaiseExceptionOn500 := ARaiseException;
end;

function TRequest.Resource(const AResource: string): IRequest;
begin
  Result := Self;
  FRESTRequest.Resource := AResource;
end;

function TRequest.ResourceSuffix(const AResourceSuffix: string): IRequest;
begin
  Result := Self;
  FRESTRequest.ResourceSuffix := AResourceSuffix;
end;

function TRequest.Timeout(const ATimeout: Integer): IRequest;
begin
  Result := Self;
  FRESTRequest.Timeout := ATimeout;
end;

function TRequest.Token(const AToken: string): IRequest;
begin
  Result := Self;
  FToken := AToken;
end;

end.
