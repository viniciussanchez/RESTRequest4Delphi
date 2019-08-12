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
    function ClearDataSet(const ADataSet: TDataSet): TRequest;
    function ActiveCachedUpdates(const ADataSet: TDataSet; const AActive: Boolean = True): TRequest;
    function SetDataSetAdapter(const ADataSet: TDataSet): IRequest;
    function SetBaseURL(const ABaseURL: string = ''): IRequest;
    function SetResource(const AResource: string = ''): IRequest;
    function SetResourceSuffix(const AResourceSuffix: string = ''): IRequest;
    function SetMethod(const AMethod: TRESTRequestMethod = rmGET): IRequest;
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
  RESTRequest4D.Request.Headers, System.Generics.Collections, FireDAC.Comp.DataSet;

{ TRequest }

function TRequest.ActiveCachedUpdates(const ADataSet: TDataSet; const AActive: Boolean): TRequest;
begin
  Result := Self;
  if ADataSet is TFDDataSet then
  begin
    TFDDataSet(ADataSet).CancelUpdates;
    TFDDataSet(ADataSet).CachedUpdates := AActive;
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

function TRequest.ClearDataSet(const ADataSet: TDataSet): TRequest;
var
  LHandlerControlsEnabled: Boolean;
begin
  Result := Self;
  LHandlerControlsEnabled := not ADataSet.ControlsDisabled;
  if LHandlerControlsEnabled then
    ADataSet.DisableControls;
  try
    ADataSet.First;
    while not ADataSet.Eof do
      ADataSet.Delete;
  finally
    if LHandlerControlsEnabled then
      ADataSet.EnableControls;
  end;
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
var
  LDataSetChild: TDataSet;
  LDataSetsChilds: TList<TDataSet>;
begin
  LDataSetsChilds := nil;
  if not Assigned(FDataSetAdapter) then
    Exit;
  if not FDataSetAdapter.Active then
    FDataSetAdapter.Open;
  ClearDataSet(FDataSetAdapter);
  FDataSetAdapter.GetDetailDataSets(LDataSetsChilds);
  try
    for LDataSetChild in LDataSetsChilds do
      ClearDataSet(LDataSetChild).ActiveCachedUpdates(LDataSetChild, False);
    FDataSetAdapter.LoadFromJSON(FRESTResponse.Content);
  finally
    if Assigned(LDataSetsChilds) then
    begin
      for LDataSetChild in LDataSetsChilds do
        ActiveCachedUpdates(LDataSetChild);
      LDataSetsChilds.Free;
    end;
  end;
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
