unit RESTRequest4D.Request;

interface

uses RESTRequest4D.Request.Intf, Data.DB, REST.Client, REST.Response.Adapter, RESTRequest4D.Request.Params.Intf, REST.Types,
  RESTRequest4D.Request.Body.Intf, RESTRequest4D.Request.Authentication.Intf, System.SysUtils, RESTRequest4D.Request.Headers.Intf;

type
  TRequest = class(TInterfacedObject, IRequest)
  private
    FBody: IRequestBody;
    FParams: IRequestParams;
    FHeaders: IRequestHeaders;
    FAuthentication: IRequestAuthentication;
    FRESTRequest: TRESTRequest;
    FDataSetAdapter: TDataSet;
    FRESTResponse: TRESTResponse;
    FRESTClient: TRESTClient;
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
    function GetRaiseExceptionOn500: Boolean;
    function GetFullRequestURL(const AIncludeParams: Boolean = True): string;
    function GetTimeout: Integer;
    function SetTimeout(const ATimeout: Integer): IRequest;
    function GetMethod: TRESTRequestMethod;
    function GetResourceSuffix: string;
    function GetResource: string;
    function GetBaseURL: string;
    function GetDataSetAdapter: TDataSet;
    function GetStatusCode: Integer;
    function Execute: Integer;
    function Body: IRequestBody;
    function Headers: IRequestHeaders;
    function Params: IRequestParams;
    function Authentication: IRequestAuthentication;
    function ExecuteAsync(ACompletionHandler: TProc = nil; ASynchronized: Boolean = True; AFreeThread: Boolean = True;
      ACompletionHandlerWithError: TProc<TObject> = nil): TRESTExecutionThread;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses RESTRequest4D.Request.Body, RESTRequest4D.Request.Params, RESTRequest4D.Request.Authentication, DataSet.Serialize.Helper,
  RESTRequest4D.Request.Headers, System.Generics.Collections, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

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
      if AActive and not TFDMemTable(ADataSet).Active then
        TFDMemTable(ADataSet).Open;
    end;
    ADataSet.GetDetailDataSets(LDataSetDetails);
    for LDataSet in LDataSetDetails do
      ActiveCachedUpdates(LDataSet, AActive);
  finally
    LDataSetDetails.Free;
  end;
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

constructor TRequest.Create;
begin
  inherited;
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTClient := TRESTClient.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);

  FBody := TRequestBody.Create(FRESTRequest);
  FParams := TRequestParams.Create(FRESTRequest);
  FHeaders := TRequestHeaders.Create(FRESTRequest);

  FRESTRequest.OnAfterExecute := DoAfterExecute;
  DoJoinComponents;
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
  FRESTRequest.Execute;
  Result := FRESTResponse.StatusCode;
end;

function TRequest.ExecuteAsync(ACompletionHandler: TProc; ASynchronized, AFreeThread: Boolean;
  ACompletionHandlerWithError: TProc<TObject>): TRESTExecutionThread;
begin
  Result := FRESTRequest.ExecuteAsync(ACompletionHandler, ASynchronized, AFreeThread, ACompletionHandlerWithError);
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

function TRequest.GetStatusCode: Integer;
begin
  Result := FRESTRequest.Response.StatusCode;
end;

function TRequest.GetTimeout: Integer;
begin
  Result := FRESTRequest.Timeout;
end;

function TRequest.Headers: IRequestHeaders;
begin
  Result := FHeaders;
end;

function TRequest.Params: IRequestParams;
begin
  Result := FParams;
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
  FRESTRequest.Timeout := ATimeout;
end;

end.
