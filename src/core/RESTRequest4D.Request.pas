unit RESTRequest4D.Request;

interface

uses RESTRequest4D.Request.Intf, Data.DB, REST.Client, REST.Response.Adapter, REST.Types, System.SysUtils, System.Classes,
  RESTRequest4D.Response.Intf, System.JSON, REST.Authenticator.Basic
  {$if CompilerVersion <= 32.0} ,IPPeerClient {$endif};

type
  IRequest = RESTRequest4D.Request.Intf.IRequest;
  IResponse = RESTRequest4D.Response.Intf.IResponse;

  TRequest = class(TInterfacedObject, IRequest)
  private
    FParams: TStrings;
    FResponse: IResponse;
    FHeaders: TStrings;
    FHTTPBasicAuthenticator: THTTPBasicAuthenticator;
    FRESTRequest: TRESTRequest;
    FDataSetAdapter: TDataSet;
    FRESTResponse: TRESTResponse;
    FRESTClient: TRESTClient;
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
    function Timeout(const ATimeout: Integer): IRequest; overload;
    function Timeout: Integer; overload;
    function RaiseExceptionOn500(const ARaiseException: Boolean): IRequest; overload;
    function RaiseExceptionOn500: Boolean; overload;
    function FullRequestURL(const AIncludeParams: Boolean = True): string;
    function Token(const AToken: string): IRequest;
    function BasicAuthentication(const AUsername, APassword: string): IRequest;
    function Get: IResponse;
    function Post: IResponse;
    function Put: IResponse;
    function Delete: IResponse;
    function Patch: IResponse;
    function ClearBody: IRequest;
    function AddBody(const AContent: string; const AContentType: TRESTContentType = ctAPPLICATION_JSON): IRequest; overload;
    function AddBody(const AContent: TJSONObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TJSONArray; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TObject; const AOwns: Boolean = True): IRequest; overload;
    function ClearHeaders: IRequest;
    function AddHeader(const AName, AValue: string; const AOptions: TRESTRequestParameterOptions = []): IRequest;
    function ClearParams: IRequest;
    function UserAgent(const AName: string): IRequest;
    {$IF COMPILERVERSION < 33}
      function AddParam(const AName, AValue: string; const AKind: TRESTRequestParameterKind = TRESTRequestParameterKind.pkGETorPOST): IRequest;
    {$ELSE}
      function AddParam(const AName, AValue: string; const AKind: TRESTRequestParameterKind = TRESTRequestParameterKind.pkQUERY): IRequest;
      function AddFile(const AName: string; const AValue: TStream): IRequest;
    {$ENDIF}
    function AddText(const AName: string; const AValue: string; const AContentType: TRESTContentType = TRESTContentType.ctAPPLICATION_JSON): IRequest;
  public
    constructor Create;
    class function New: IRequest;
    destructor Destroy; override;
  end;

implementation

uses DataSet.Serialize, System.Generics.Collections, FireDAC.Comp.DataSet, FireDAC.Comp.Client, RESTRequest4D.Response;

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
  if not AContent.Trim.IsEmpty then
    {$IF COMPILERVERSION <= 29}
      FRESTRequest.AddBody(AContent, AContentType);
    {$ELSE}
      FRESTRequest.Body.Add(AContent, AContentType);
    {$ENDIF}
end;

function TRequest.AddBody(const AContent: TJSONObject; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  if Assigned(AContent) then
  begin
    {$IF COMPILERVERSION <= 29}
      FRESTRequest.AddBody(AContent);
    {$ELSE}
      FRESTRequest.Body.Add(AContent);
    {$ENDIF}
    if AOwns then
      AContent.Free;
  end;
end;

function TRequest.AddBody(const AContent: TJSONArray; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  if Assigned(AContent) then
  begin
    Self.AddBody(AContent.ToString);
    if AOwns then
      AContent.Free;
  end;
end;

function TRequest.AddBody(const AContent: TObject; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  if Assigned(AContent) then
  begin
    {$IF COMPILERVERSION <= 29}
      FRESTRequest.AddBody(AContent);
    {$ELSE}
      FRESTRequest.Body.Add(AContent);
    {$ENDIF}
    if AOwns then
      AContent.Free;
  end;
end;

{$IF COMPILERVERSION >= 33}
function TRequest.AddFile(const AName: string; const AValue: TStream): IRequest;
begin
  Result := Self;
  if not Assigned(AValue) then
    Exit;
  with FRESTRequest.Params.AddItem do
  begin
    Name := AName;
    SetStream(AValue);
    Value := AValue.ToString;
    Kind := TRESTRequestParameterKind.pkFILE;
    ContentType := TRESTContentType.ctAPPLICATION_OCTET_STREAM;
  end;
end;
{$ENDIF}

function TRequest.AddHeader(const AName, AValue: string; const AOptions: TRESTRequestParameterOptions): IRequest;
begin
  Result := Self;
  if (not AName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
  begin
    if (FHeaders.IndexOf(AName) < 0) then
      FHeaders.Add(AName);
    FRESTRequest.Params.AddHeader(AName, AValue);
    FRESTRequest.Params.ParameterByName(AName).Options := AOptions;
  end;
end;

function TRequest.AddParam(const AName, AValue: string; const AKind: TRESTRequestParameterKind): IRequest;
begin
  Result := Self;
  if (not AName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
  begin
    FParams.Add(AName);
    FRESTRequest.AddParameter(AName, AValue, AKind);
  end;
end;

function TRequest.AddText(const AName, AValue: string; const AContentType: TRESTContentType): IRequest;
begin
  Result := Self;
  with FRESTRequest.Params.AddItem do
  begin
    Name := AName;
    Value := AValue;
    Kind := TRESTRequestParameterKind.pkREQUESTBODY;
    ContentType := AContentType;
  end;
end;

function TRequest.BasicAuthentication(const AUsername, APassword: string): IRequest;
begin
  Result := Self;
  if not Assigned(FHTTPBasicAuthenticator) then
  begin
    FHTTPBasicAuthenticator := THTTPBasicAuthenticator.Create(nil);
    FRESTClient.Authenticator := FHTTPBasicAuthenticator;
  end;
  FHTTPBasicAuthenticator.Username := AUsername;
  FHTTPBasicAuthenticator.Password := APassword;
end;

function TRequest.ClearBody: IRequest;
begin
  Result := Self;
  FRESTRequest.ClearBody;
end;

function TRequest.ClearHeaders: IRequest;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to Pred(FHeaders.Count) do
    FRESTRequest.Params.Delete(FRESTRequest.Params.ParameterByName(FHeaders[I]));
end;

function TRequest.ClearParams: IRequest;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to Pred(FParams.Count) do
    FRESTRequest.Params.Delete(FRESTRequest.Params.ParameterByName(FParams[I]));
end;

function TRequest.UserAgent(const AName: string): IRequest;
begin
  Result := Self;  
  if not AName.Trim.IsEmpty then
    FRESTRequest.Client.UserAgent := AName;
end;

constructor TRequest.Create;
begin
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTClient := TRESTClient.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);

  FParams := TStringList.Create;
  FHeaders := TStringList.Create;
  FResponse := TResponse.Create(FRESTResponse);

  FRESTRequest.OnAfterExecute := DoAfterExecute;
  DoJoinComponents;

  FRESTClient.RaiseExceptionOn500 := False;
end;

function TRequest.Delete: IResponse;
begin
  Result := FResponse;
  FRESTRequest.Method := TRESTRequestMethod.rmDELETE;
  FRESTRequest.Execute;
end;

destructor TRequest.Destroy;
begin
  if Assigned(FHTTPBasicAuthenticator) then
    FreeAndNil(FHTTPBasicAuthenticator);
  FreeAndNil(FParams);
  FreeAndNil(FHeaders);
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

function TRequest.Get: IResponse;
begin
  Result := FResponse;
  FRESTRequest.Method := TRESTRequestMethod.rmGET;
  FRESTRequest.Execute;
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

function TRequest.Patch: IResponse;
begin
  Result := FResponse;
  FRESTRequest.Method := TRESTRequestMethod.rmPATCH;
  FRESTRequest.Execute;
end;

function TRequest.Post: IResponse;
begin
  Result := FResponse;
  FRESTRequest.Method := TRESTRequestMethod.rmPOST;
  FRESTRequest.Execute;
end;

function TRequest.Put: IResponse;
begin
  Result := FResponse;
  FRESTRequest.Method := TRESTRequestMethod.rmPUT;
  FRESTRequest.Execute;
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
  Self.AddHeader('Authorization', AToken, [poDoNotEncode]);
end;

end.
