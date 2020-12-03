unit RESTRequest4D.Request.Client;

interface

uses RESTRequest4D.Request.Contract, Data.DB, REST.Client, REST.Response.Adapter, REST.Types, System.SysUtils, System.Classes,
  RESTRequest4D.Response.Contract, System.JSON, REST.Authenticator.Basic
  {$if CompilerVersion <= 32.0} ,IPPeerClient {$endif};

type
  TRequestClient = class(TInterfacedObject, IRequest)
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
    function RaiseExceptionOn500: Boolean; overload;
    function RaiseExceptionOn500(const ARaiseException: Boolean): IRequest; overload;
    function FullRequestURL(const AIncludeParams: Boolean = True): string;
    function Token(const AToken: string): IRequest;
    function BasicAuthentication(const AUsername, APassword: string): IRequest;
    function Get: IResponse;
    function Post: IResponse;
    function Put: IResponse;
    function Delete: IResponse;
    function Patch: IResponse;
    function ClearBody: IRequest;
    function AddBody(const AContent: string): IRequest; overload;
    function AddBody(const AContent: TJSONObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TJSONArray; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TStream; const AOwns: Boolean = True): IRequest; overload;
    function ClearHeaders: IRequest;
    function AddHeader(const AName, AValue: string): IRequest;
    function ClearParams: IRequest;
    function ContentType(const AContentType: string): IRequest;
    function UserAgent(const AName: string): IRequest;
    function AddCookies(const ACookies: TStrings): IRequest;
    function AddParam(const AName, AValue: string): IRequest;
    {$IF COMPILERVERSION >= 33}
      function AddFile(const AName: string; const AValue: TStream): IRequest;
    {$ENDIF}
    function AddText(const AName: string; const AValue: string; const AContentType: TRESTContentType = TRESTContentType.ctAPPLICATION_JSON): IRequest;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses DataSet.Serialize, System.Generics.Collections, FireDAC.Comp.DataSet, FireDAC.Comp.Client, RESTRequest4D.Response.Client;

procedure TRequestClient.ActiveCachedUpdates(const ADataSet: TDataSet; const AActive: Boolean = True);
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

function TRequestClient.AddBody(const AContent: string): IRequest;
begin
  Result := Self;
  if AContent.Trim.IsEmpty then
    Exit;
  {$IF COMPILERVERSION <= 29}
    FRESTRequest.AddBody(AContent);
  {$ELSE}
    FRESTRequest.Body.Add(AContent);
  {$ENDIF}
end;

function TRequestClient.AddBody(const AContent: TJSONObject; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  if not Assigned(AContent) then
    Exit;
  {$IF COMPILERVERSION <= 29}
    FRESTRequest.AddBody(AContent);
  {$ELSE}
    FRESTRequest.Body.Add(AContent);
  {$ENDIF}
  if AOwns then
    AContent.Free;
end;

function TRequestClient.AddBody(const AContent: TJSONArray; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  if not Assigned(AContent) then
    Exit;
  Self.AddBody(AContent.ToString);
  if AOwns then
    AContent.Free;
end;

function TRequestClient.AddBody(const AContent: TObject; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  if not Assigned(AContent) then
    Exit;
  {$IF COMPILERVERSION <= 29}
    FRESTRequest.AddBody(AContent);
  {$ELSE}
    FRESTRequest.Body.Add(AContent);
  {$ENDIF}
  if AOwns then
    AContent.Free;
end;

{$IF COMPILERVERSION >= 33}
function TRequestClient.AddFile(const AName: string; const AValue: TStream): IRequest;
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

function TRequestClient.AddHeader(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  if FHeaders.IndexOf(AName) < 0 then
    FHeaders.Add(AName);
  FRESTRequest.Params.AddHeader(AName, AValue);
end;

function TRequestClient.AddParam(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  FParams.Add(AName);
  FRESTRequest.AddParameter(AName, AValue, {$IF COMPILERVERSION < 33}TRESTRequestParameterKind.pkGETorPOST{$ELSE}TRESTRequestParameterKind.pkQUERY{$ENDIF});
end;

function TRequestClient.AddText(const AName, AValue: string; const AContentType: TRESTContentType): IRequest;
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

function TRequestClient.BasicAuthentication(const AUsername, APassword: string): IRequest;
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

function TRequestClient.ClearBody: IRequest;
begin
  Result := Self;
  FRESTRequest.ClearBody;
end;

function TRequestClient.ClearHeaders: IRequest;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to Pred(FHeaders.Count) do
    FRESTRequest.Params.Delete(FRESTRequest.Params.ParameterByName(FHeaders[I]));
end;

function TRequestClient.ClearParams: IRequest;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to Pred(FParams.Count) do
    FRESTRequest.Params.Delete(FRESTRequest.Params.ParameterByName(FParams[I]));
end;

function TRequestClient.ContentType(const AContentType: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Content-Type', AContentType);
end;

function TRequestClient.UserAgent(const AName: string): IRequest;
begin
  Result := Self;  
  if not AName.Trim.IsEmpty then
    FRESTRequest.Client.UserAgent := AName;
end;

constructor TRequestClient.Create;
begin
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTClient := TRESTClient.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);

  FParams := TStringList.Create;
  FHeaders := TStringList.Create;
  FResponse := TResponseClient.Create(FRESTResponse);

  FRESTRequest.OnAfterExecute := DoAfterExecute;
  DoJoinComponents;

  FRESTClient.RaiseExceptionOn500 := False;
  Self.ContentType('application/json');
end;

function TRequestClient.Delete: IResponse;
begin
  Result := FResponse;
  FRESTRequest.Method := TRESTRequestMethod.rmDELETE;
  FRESTRequest.Execute;
end;

destructor TRequestClient.Destroy;
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

procedure TRequestClient.DoAfterExecute(Sender: TCustomRESTRequest);
begin
  if not Assigned(FDataSetAdapter) then
    Exit;  
  ActiveCachedUpdates(FDataSetAdapter, False);
  FDataSetAdapter.LoadFromJSON(FRESTResponse.Content);
  ActiveCachedUpdates(FDataSetAdapter);
end;

procedure TRequestClient.DoJoinComponents;
begin
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
end;

function TRequestClient.Get: IResponse;
begin
  Result := FResponse;
  FRESTRequest.Method := TRESTRequestMethod.rmGET;
  FRESTRequest.Execute;
end;

function TRequestClient.Accept: string;
begin
  Result := FRESTRequest.Accept;
end;

function TRequestClient.AcceptCharset: string;
begin
  Result := FRESTRequest.AcceptCharset;
end;

function TRequestClient.AcceptEncoding: string;
begin
  Result := FRESTRequest.AcceptEncoding;
end;

function TRequestClient.BaseURL: string;
begin
  Result := FRESTClient.BaseURL;
end;

function TRequestClient.DataSetAdapter: TDataSet;
begin
  Result := FDataSetAdapter;
end;

function TRequestClient.FullRequestURL(const AIncludeParams: Boolean): string;
begin
  Result := FRESTRequest.GetFullRequestURL(AIncludeParams);
end;

function TRequestClient.Resource: string;
begin
  Result := FRESTRequest.Resource;
end;

function TRequestClient.ResourceSuffix: string;
begin
  Result := FRESTRequest.ResourceSuffix;
end;

function TRequestClient.Timeout: Integer;
begin
  Result := FRESTRequest.Timeout;
end;

function TRequestClient.Patch: IResponse;
begin
  Result := FResponse;
  FRESTRequest.Method := TRESTRequestMethod.rmPATCH;
  FRESTRequest.Execute;
end;

function TRequestClient.Post: IResponse;
begin
  Result := FResponse;
  FRESTRequest.Method := TRESTRequestMethod.rmPOST;
  FRESTRequest.Execute;
end;

function TRequestClient.Put: IResponse;
begin
  Result := FResponse;
  FRESTRequest.Method := TRESTRequestMethod.rmPUT;
  FRESTRequest.Execute;
end;

function TRequestClient.Accept(const AAccept: string): IRequest;
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

function TRequestClient.AcceptCharset(const AAcceptCharset: string): IRequest;
const
  REQUEST_DEFAULT_ACCEPT_CHARSET = 'utf-8, *;q=0.8';
begin
  Result := Self;
  FRESTRequest.AcceptCharset := REQUEST_DEFAULT_ACCEPT_CHARSET;
  if not AAcceptCharset.Trim.IsEmpty then
    FRESTRequest.AcceptCharset := AAcceptCharset;
end;

function TRequestClient.AcceptEncoding(const AAcceptEncoding: string): IRequest;
begin
  Result := Self;
  FRESTRequest.AcceptEncoding := AAcceptEncoding;
end;

function TRequestClient.BaseURL(const ABaseURL: string): IRequest;
begin
  Result := Self;
  FRESTClient.BaseURL := ABaseURL;
end;

function TRequestClient.DataSetAdapter(const ADataSet: TDataSet): IRequest;
begin
  Result := Self;
  FDataSetAdapter := ADataSet;
end;

function TRequestClient.RaiseExceptionOn500: Boolean;
begin
  Result := FRESTClient.RaiseExceptionOn500;
end;

function TRequestClient.RaiseExceptionOn500(const ARaiseException: Boolean): IRequest;
begin
  Result := Self;
  FRESTClient.RaiseExceptionOn500 := ARaiseException;
end;

function TRequestClient.Resource(const AResource: string): IRequest;
begin
  Result := Self;
  FRESTRequest.Resource := AResource;
end;

function TRequestClient.ResourceSuffix(const AResourceSuffix: string): IRequest;
begin
  Result := Self;
  FRESTRequest.ResourceSuffix := AResourceSuffix;
end;

function TRequestClient.Timeout(const ATimeout: Integer): IRequest;
begin
  Result := Self;
  FRESTRequest.Timeout := ATimeout;
end;

function TRequestClient.Token(const AToken: string): IRequest;
const
  AUTHORIZATION = 'Authorization';
begin
  Result := Self;
  if AToken.Trim.IsEmpty then
    Exit;
  if FHeaders.IndexOf(AUTHORIZATION) < 0 then
    FHeaders.Add(AUTHORIZATION);
  FRESTRequest.Params.AddHeader(AUTHORIZATION, AToken);
  FRESTRequest.Params.ParameterByName(AUTHORIZATION).Options := [poDoNotEncode];
end;

function TRequestClient.AddBody(const AContent: TStream; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  {$IF COMPILERVERSION <= 29}
    FRESTRequest.AddBody(AContent, TRESTContentType.ctAPPLICATION_OCTET_STREAM);
  {$ELSE}
    FRESTRequest.Body.Add(AContent, TRESTContentType.ctAPPLICATION_OCTET_STREAM);
  {$ENDIF}
  if AOwns then
    AContent.Free;
end;

function TRequestClient.AddCookies(const ACookies: TStrings): IRequest;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to Pred(ACookies.Count) do
    FRESTRequest.AddParameter(ACookies.Names[I], ACookies.Values[ACookies.Names[I]], TRESTRequestParameterKind.pkCOOKIE);
end;

end.
