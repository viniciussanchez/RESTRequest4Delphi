unit RESTRequest4D.Request.Client;

interface

uses RESTRequest4D.Request.Contract, Data.DB, REST.Client, REST.Response.Adapter, REST.Types, System.SysUtils, System.Classes,
  RESTRequest4D.Response.Contract, REST.Authenticator.Basic,
  {$IF COMPILERVERSION >= 33.0}System.Net.HttpClient,{$ENDIF}
  {$IF COMPILERVERSION >= 36.0}System.NetEncoding,{$ENDIF}
  System.JSON, RESTRequest4D.Request.Adapter.Contract;

type
  TRequestClient = class(TInterfacedObject, IRequest)
  private
    FParams: TStrings;
    FResponse: IResponse;
    FHeaders: TStrings;
    FHTTPBasicAuthenticator: THTTPBasicAuthenticator;
    FRESTRequest: TRESTRequest;
    FAdapters: TArray<IRequestAdapter>;
    FRESTResponse: TRESTResponse;
    FRESTClient: TRESTClient;
    FRetries: Integer;
    FOnBeforeExecute: TRR4DCallbackOnBeforeExecute;
    FOnAfterExecute: TRR4DCallbackOnAfterExecute;
    procedure ExecuteRequest;
    procedure DoJoinComponents;
    function PrepareUrlSegments(const AValue: string): string;
    function AcceptEncoding: string; overload;
    function AcceptEncoding(const AAcceptEncoding: string): IRequest; overload;
    function AcceptCharset: string; overload;
    function AcceptCharset(const AAcceptCharset: string): IRequest; overload;
    function Accept: string; overload;
    function Accept(const AAccept: string): IRequest; overload;
    function Adapters(const AAdapter: IRequestAdapter): IRequest; overload;
    function Adapters(const AAdapters: TArray<IRequestAdapter>): IRequest; overload;
    function Adapters: TArray<IRequestAdapter>; overload;
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
    function TokenBearer(const AToken: string): IRequest;
    function BasicAuthentication(const AUsername, APassword: string): IRequest;
    function Retry(const ARetries: Integer): IRequest;
    function OnBeforeExecute(const AOnBeforeExecute: TRR4DCallbackOnBeforeExecute): IRequest;
    function OnAfterExecute(const AOnAfterExecute: TRR4DCallbackOnAfterExecute): IRequest;
    function Get: IResponse;
    function Post: IResponse;
    function Put: IResponse;
    function Delete: IResponse;
    function Patch: IResponse;
    function ClearBody: IRequest;
    function AddParam(const AName, AValue: string; const AKind: TRESTRequestParameterKind = {$IF COMPILERVERSION < 33}TRESTRequestParameterKind.pkGETorPOST{$ELSE}TRESTRequestParameterKind.pkQUERY{$ENDIF}; const AOptions: TRESTRequestParameterOptions = []): IRequest;
    function AddBody(const AContent: string; const AContentType: TRESTContentType = ctAPPLICATION_JSON): IRequest; overload;
    function AddBody(const AContent: TJSONObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TJSONArray; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TStream; const AOwns: Boolean = True): IRequest; overload;
    function FallbackCharsetEncoding(const AFallbackCharsetEncoding: string): IRequest;
    function AddUrlSegment(const AName, AValue: string): IRequest;
    function SynchronizedEvents(const AValue: Boolean): IRequest;
    function ClearHeaders: IRequest;
    function AddHeader(const AName, AValue: string; const AOptions: TRESTRequestParameterOptions = []): IRequest;
    function ClearParams: IRequest;
    function ContentType(const AContentType: string): IRequest; overload;
    function ContentType: string; overload;
    function UserAgent(const AName: string): IRequest;
    function AddCookies(const ACookies: TStrings): IRequest;
    function AddCookie(const ACookieName, ACookieValue: string): IRequest;
    function AddField(const AFieldName: string; const AValue: string): IRequest; overload;
    function AddFile(const AFieldName: string; const AFileName: string; const AContentType: TRESTContentType = TRESTContentType.ctNone): IRequest; overload;
    function AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string = ''; const AContentType: TRESTContentType = TRESTContentType.ctNone): IRequest; overload;
    function Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
    function DeactivateProxy: IRequest;
  protected
    procedure DoAfterExecute(Sender: TCustomRESTRequest); virtual;
    procedure DoBeforeExecute(Sender: TCustomRESTRequest); virtual;
    procedure DoHTTPProtocolError(Sender: TCustomRESTRequest); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses System.Generics.Collections, RESTRequest4D.Response.Client;

function TRequestClient.AddBody(const AContent: string; const AContentType: TRESTContentType): IRequest;
begin
  Result := Self;
  if AContent.Trim.IsEmpty then
    Exit;
  {$IF COMPILERVERSION <= 29}
    FRESTRequest.AddBody(AContent, AContentType);
  {$ELSE}
    FRESTRequest.Body.Add(AContent, AContentType);
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
  begin
    {$IFDEF MSWINDOWS}
      AContent.Free;
    {$ELSE}
      {$IF COMPILERVERSION <= 35}
        AContent.DisposeOf;
      {$ELSE}
        AContent.Free;
      {$ENDIF}
    {$ENDIF}
  end;
end;

function TRequestClient.AddBody(const AContent: TJSONArray; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  if not Assigned(AContent) then
    Exit;
  Self.AddBody(AContent.ToString);
  if AOwns then
  begin
    {$IFDEF MSWINDOWS}
      AContent.Free;
    {$ELSE}
      {$IF COMPILERVERSION <= 35}
        AContent.DisposeOf;
      {$ELSE}
        AContent.Free;
      {$ENDIF}
    {$ENDIF}
  end;
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
  begin
    {$IFDEF MSWINDOWS}
      AContent.Free;
    {$ELSE}
      {$IF COMPILERVERSION <= 35}
        AContent.DisposeOf;
      {$ELSE}
        AContent.Free;
      {$ENDIF}
    {$ENDIF}
  end;
end;

function TRequestClient.AddField(const AFieldName: string; const AValue: string): IRequest;
begin
  Result := Self;
  FRESTRequest.Params.AddItem(AFieldName, AValue);
end;

function TRequestClient.AddFile(const AFieldName: string; const AFileName: string; const AContentType: TRESTContentType): IRequest;
begin
  Result := Self;
  {$IF COMPILERVERSION >= 32.0}
    if not FileExists(AFileName) then
      Exit;
    FRESTRequest.AddFile(AFieldName, AFileName, AContentType);
  {$ELSE}
    raise Exception.Create('Method not implemented for your Delphi version. Try changing the engine or submitting a pull request.');
  {$ENDIF}
end;

function TRequestClient.AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string; const AContentType: TRESTContentType): IRequest;
{$IF COMPILERVERSION >= 33.0}
var
  lFileName: string;
{$ENDIF}
begin
  Result := Self;
  {$IF COMPILERVERSION >= 33.0}
    if not Assigned(AValue) then
      Exit;
    lFileName := Trim(AFileName);
    if (lFileName = EmptyStr) then
      lFileName := AFieldName;
    AValue.Position := 0;
    with FRESTRequest.Params.AddItem do
    begin
      name := AFieldName;
      SetStream(AValue);
      Value := lFileName;
      Kind := TRESTRequestParameterKind.pkFILE;
      ContentType := AContentType;
    end;
  {$ELSE}
    raise Exception.Create('Method not implemented for your Delphi version. Try changing the engine or submitting a pull request.');
  {$ENDIF}
end;

function TRequestClient.AddHeader(const AName, AValue: string; const AOptions: TRESTRequestParameterOptions): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  if FHeaders.IndexOf(AName) < 0 then
    FHeaders.Add(AName);
  FRESTRequest.Params.AddHeader(AName, AValue);
  FRESTRequest.Params.ParameterByName(AName).Options := AOptions;
end;

function TRequestClient.AddParam(const AName, AValue: string; const AKind: TRESTRequestParameterKind = {$IF COMPILERVERSION < 33}TRESTRequestParameterKind.pkGETorPOST{$ELSE}TRESTRequestParameterKind.pkQUERY{$ENDIF}; const AOptions: TRESTRequestParameterOptions = []): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  FParams.Add(AName);
{$IF COMPILERVERSION >= 36}
  FRESTRequest.AddParameter(AName, TNetEncoding.URL.Encode(AValue), AKind, [TRESTRequestParameterOption.poDoNotEncode]);
{$ELSE}
  FRESTRequest.AddParameter(AName, AValue, AKind);
{$ENDIF}
end;

function TRequestClient.AddUrlSegment(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  if not FRESTRequest.Params.ContainsParameter(AName) then
  begin
    if (not ResourceSuffix.Trim.IsEmpty) and (not ResourceSuffix.EndsWith('/')) then
      ResourceSuffix(ResourceSuffix + '/');
    ResourceSuffix(ResourceSuffix + '{' + AName + '}');
  end;
  FRESTRequest.Params.AddUrlSegment(AName, AValue);
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

function TRequestClient.ContentType: string;
begin
  if Assigned(FRESTRequest.Params.ParameterByName('Content-Type')) then
    Result := FRESTRequest.Params.ParameterByName('Content-Type').Value;
end;

function TRequestClient.ContentType(const AContentType: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Content-Type', AContentType, [poDoNotEncode]);
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
  FRESTClient.SynchronizedEvents := False;
  {$IF COMPILERVERSION >= 33}
    FRESTClient.SecureProtocols := [THTTPSecureProtocol.SSL3, THTTPSecureProtocol.TLS1, THTTPSecureProtocol.TLS11, THTTPSecureProtocol.TLS12];
  {$ENDIF}
  FRESTRequest := TRESTRequest.Create(nil);
  FRESTRequest.SynchronizedEvents := False;
  FParams := TStringList.Create;
  FHeaders := TStringList.Create;
  FResponse := TResponseClient.Create(FRESTResponse);
  FRESTRequest.OnAfterExecute := DoAfterExecute;
  FRESTRequest.OnHTTPProtocolError := DoHTTPProtocolError;
  DoJoinComponents;
  FRESTClient.RaiseExceptionOn500 := False;
  FRetries := 0;
end;

function TRequestClient.DeactivateProxy: IRequest;
begin
  Result := Self;
  FRESTClient.ProxyPassword := EmptyStr;
  FRESTClient.ProxyServer := EmptyStr;
  FRESTClient.ProxyUsername := EmptyStr;
  FRESTClient.ProxyPort := 0;
end;

function TRequestClient.Delete: IResponse;
begin
  Result := FResponse;
  DoBeforeExecute(FRESTRequest);
  FRESTRequest.Method := TRESTRequestMethod.rmDELETE;
  ExecuteRequest;
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
var
  LAdapter: IRequestAdapter;
begin
  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self, FResponse);
  for LAdapter in FAdapters do
    LAdapter.Execute(FRESTResponse.Content);
end;

procedure TRequestClient.DoBeforeExecute(Sender: TCustomRESTRequest);
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);
end;

procedure TRequestClient.DoHTTPProtocolError(Sender: TCustomRESTRequest);
begin
  // virtual method
end;

procedure TRequestClient.DoJoinComponents;
begin
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
end;

procedure TRequestClient.ExecuteRequest;
var
  LAttempts: Integer;
begin
  LAttempts := FRetries + 1;
  while LAttempts > 0 do
  begin
    try
      FRESTRequest.Execute;
      LAttempts := 0;
    except
      begin
        LAttempts := LAttempts - 1;
        if LAttempts = 0 then
          raise;
      end;
    end;
  end;
end;

function TRequestClient.Get: IResponse;
begin
  Result := FResponse;
  DoBeforeExecute(FRESTRequest);
  FRESTRequest.Method := TRESTRequestMethod.rmGET;
  ExecuteRequest;
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

function TRequestClient.FallbackCharsetEncoding(const AFallbackCharsetEncoding: string): IRequest;
begin
  Result := Self;
  FRESTClient.FallbackCharsetEncoding := AFallbackCharsetEncoding;
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

function TRequestClient.Retry(const ARetries: Integer): IRequest;
begin
  Result := Self;
  FRetries := ARetries;
end;

function TRequestClient.OnBeforeExecute(const AOnBeforeExecute: TRR4DCallbackOnBeforeExecute): IRequest;
begin
  Result := Self;
  FOnBeforeExecute := AOnBeforeExecute;
end;

function TRequestClient.OnAfterExecute(const AOnAfterExecute: TRR4DCallbackOnAfterExecute): IRequest;
begin
  Result := Self;
  FOnAfterExecute := AOnAfterExecute;
end;

function TRequestClient.SynchronizedEvents(const AValue: Boolean): IRequest;
begin
  FRESTClient.SynchronizedEvents := AValue;
  FRESTRequest.SynchronizedEvents := AValue;
end;

function TRequestClient.Timeout: Integer;
begin
  {$IF COMPILERVERSION <= 33}
    Result := FRESTRequest.Timeout;
  {$ELSE}
    Result := FRESTRequest.ConnectTimeout;
  {$ENDIF}
end;

function TRequestClient.Patch: IResponse;
begin
  Result := FResponse;
  DoBeforeExecute(FRESTRequest);
  FRESTRequest.Method := TRESTRequestMethod.rmPATCH;
  ExecuteRequest;
end;

function TRequestClient.Post: IResponse;
begin
  Result := FResponse;
  DoBeforeExecute(FRESTRequest);
  FRESTRequest.Method := TRESTRequestMethod.rmPOST;
  ExecuteRequest;
end;

function TRequestClient.PrepareUrlSegments(const AValue: string): string;
var
  LSplitedPath: TArray<string>;
  LPart: string;
  LPreparedUrl: string;
begin
  LSplitedPath := AValue.Split(['/', '?', '=', '&']);
  LPreparedUrl := AValue;
  for LPart in LSplitedPath do
  begin
    if LPart.StartsWith(':') then
      LPreparedUrl := StringReplace(LPreparedUrl, LPart, Format('{%s}', [LPart.TrimLeft([':'])]), []);
  end;
  Result := LPreparedUrl;
end;

function TRequestClient.Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
begin
  Result := Self;
  FRESTClient.ProxyPassword := APassword;
  FRESTClient.ProxyServer := AServer;
  FRESTClient.ProxyUsername := AUsername;
  FRESTClient.ProxyPort := APort;
end;

function TRequestClient.Put: IResponse;
begin
  Result := FResponse;
  DoBeforeExecute(FRESTRequest);
  FRESTRequest.Method := TRESTRequestMethod.rmPUT;
  ExecuteRequest;
end;

function TRequestClient.Accept(const AAccept: string): IRequest;
const
  REQUEST_DEFAULT_ACCEPT = CONTENTTYPE_APPLICATION_JSON + ', ' + CONTENTTYPE_TEXT_PLAIN + '; q=0.9, ' + CONTENTTYPE_TEXT_HTML + ';q=0.8,';
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
  FRESTClient.BaseURL := PrepareUrlSegments(ABaseURL);
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
  FRESTRequest.Resource := PrepareUrlSegments(AResource);
end;

function TRequestClient.ResourceSuffix(const AResourceSuffix: string): IRequest;
begin
  Result := Self;
  FRESTRequest.ResourceSuffix := PrepareUrlSegments(AResourceSuffix);
end;

function TRequestClient.Timeout(const ATimeout: Integer): IRequest;
begin
  Result := Self;
  {$IF COMPILERVERSION <= 33}
    FRESTRequest.Timeout := ATimeout;
  {$ELSE}
    FRESTRequest.ConnectTimeout := ATimeout;
    FRESTRequest.ReadTimeout := ATimeout;
  {$ENDIF}
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

function TRequestClient.TokenBearer(const AToken: string): IRequest;
begin
  Result := Self;
  if AToken.Trim.IsEmpty then
    Exit;
  Self.Token('Bearer ' + AToken);
end;

function TRequestClient.Adapters(const AAdapter: IRequestAdapter): IRequest;
begin
  Result := Adapters([AAdapter]);
end;

function TRequestClient.Adapters(const AAdapters: TArray<IRequestAdapter>): IRequest;
begin
  FAdapters := AAdapters;
  Result := Self;
end;

function TRequestClient.Adapters: TArray<IRequestAdapter>;
begin
  Result := FAdapters;
end;

function TRequestClient.AddBody(const AContent: TStream; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  if not Assigned(AContent) then
    Exit;
  {$IF COMPILERVERSION <= 29}
    FRESTRequest.AddBody(AContent, TRESTContentType.ctAPPLICATION_OCTET_STREAM);
  {$ELSE}
    FRESTRequest.Body.Add(AContent, TRESTContentType.ctAPPLICATION_OCTET_STREAM);
  {$ENDIF}
  if AOwns then
  begin
    {$IFDEF MSWINDOWS}
      AContent.Free;
    {$ELSE}
      {$IF COMPILERVERSION <= 35}
        AContent.DisposeOf;
      {$ELSE}
        AContent.Free;
      {$ENDIF}
    {$ENDIF}
  end;
end;

function TRequestClient.AddCookies(const ACookies: TStrings): IRequest;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to Pred(ACookies.Count) do
    FRESTRequest.AddParameter(ACookies.Names[I], ACookies.Values[ACookies.Names[I]], TRESTRequestParameterKind.pkCOOKIE);
end;

function TRequestClient.AddCookie(const ACookieName, ACookieValue: string): IRequest;
begin
  Result := Self;
  FRESTRequest.AddParameter(ACookieName, ACookieValue, TRESTRequestParameterKind.pkCOOKIE);
end;

end.
