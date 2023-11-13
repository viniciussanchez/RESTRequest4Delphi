unit RESTRequest4D.Request.NetHTTP;

interface

uses System.Net.Mime, System.Net.HttpClientComponent, System.Net.HttpClient, RESTRequest4D.Request.Contract, System.Classes,
  System.JSON, System.SysUtils, REST.Json, IdURI, System.NetEncoding, RESTRequest4D.Utils, System.Net.URLClient,
  RESTRequest4D.Response.NetHTTP, RESTRequest4D.Request.Adapter.Contract, RESTRequest4D.Response.Contract;

type
  TRequestNetHTTP = class(TInterfacedObject, IRequest)
  private
    FUseMultipartFormData: Boolean;
    FMultipartFormData: TMultipartFormData;
    FParams: TStrings;
    FUrlSegments: TStrings;
    FNetHTTPClient: TNetHTTPClient;
    FBaseURL: string;
    FResource: string;
    FResourceSuffix: string;
    FAdapters: TArray<IRequestAdapter>;
    FResponse: IResponse;
    FStreamSend: TStream;
    FStreamResult: TStringStream;
    FRetries: Integer;
    FOnBeforeExecute: TRR4DCallbackOnBeforeExecute;
    FOnAfterExecute: TRR4DCallbackOnAfterExecute;
    function ExecuteRequest(const AMethod: TMethodRequest): IHTTPResponse;
    function AcceptEncoding: string; overload;
    function AcceptEncoding(const AAcceptEncoding: string): IRequest; overload;
    function AcceptCharset: string; overload;
    function AcceptCharset(const AAcceptCharset: string): IRequest; overload;
    function Accept: string; overload;
    function Accept(const AAccept: string): IRequest; overload;
    function Timeout: Integer; overload;
    function Timeout(const ATimeout: Integer): IRequest; overload;
    function Adapters(const AAdapter: IRequestAdapter): IRequest; overload;
    function Adapters(const AAdapters: TArray<IRequestAdapter>): IRequest; overload;
    function Adapters: TArray<IRequestAdapter>; overload;
    function BaseURL(const ABaseURL: string): IRequest; overload;
    function BaseURL: string; overload;
    function Resource(const AResource: string): IRequest; overload;
    function RaiseExceptionOn500: Boolean; overload;
    function RaiseExceptionOn500(const ARaiseException: Boolean): IRequest; overload;
    function Resource: string; overload;
    function ResourceSuffix(const AResourceSuffix: string): IRequest; overload;
    function ResourceSuffix: string; overload;
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
    function FullRequestURL(const AIncludeParams: Boolean = True): string;
    function ClearBody: IRequest;
    function AddBody(const AContent: string): IRequest; overload;
    function AddBody(const AContent: TJSONObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TJSONArray; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TStream; const AOwns: Boolean = True): IRequest; overload;
    function AddUrlSegment(const AName, AValue: string): IRequest;
    function ClearHeaders: IRequest;
    function AddHeader(const AName, AValue: string): IRequest;
    function ClearParams: IRequest;
    function ContentType(const AContentType: string): IRequest; overload;
    function ContentType: string; overload;
    function UserAgent(const AName: string): IRequest;
    function AddCookies(const ACookies: TStrings): IRequest;
    function AddCookie(const ACookieName, ACookieValue: string): IRequest;
    function AddParam(const AName, AValue: string): IRequest;
    function AddField(const AFieldName: string; const AValue: string): IRequest; overload;
    function AddFile(const AFieldName: string; const AFileName: string; const AContentType: string = ''): IRequest; overload;
    function AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string = ''; const AContentType: string = ''): IRequest; overload;
    function Asynchronous(const AValue: Boolean): IRequest;
    function SynchronizedEvents(const AValue: Boolean): IRequest;
    function MakeURL(const AIncludeParams: Boolean = True): string;
    function Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
    function DeactivateProxy: IRequest;
  protected
    procedure DoAfterExecute(const Sender: TObject; const AResponse: IHTTPResponse); virtual;
    procedure DoBeforeExecute(const Sender: TNetHTTPClient); virtual;
    procedure DoHTTPProtocolError(const Sender: TObject; const AError: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

function TRequestNetHTTP.Accept(const AAccept: string): IRequest;
begin
  Result := Self;
  FNetHTTPClient.Accept := AAccept;
end;

function TRequestNetHTTP.Accept: string;
begin
  Result := FNetHTTPClient.Accept;
end;

function TRequestNetHTTP.AcceptCharset(const AAcceptCharset: string): IRequest;
begin
  Result := Self;
  FNetHTTPClient.AcceptCharSet := AAcceptCharset;
end;

function TRequestNetHTTP.AcceptCharset: string;
begin
  Result := FNetHTTPClient.AcceptCharSet;
end;

function TRequestNetHTTP.AcceptEncoding(const AAcceptEncoding: string): IRequest;
begin
  Result := Self;
  FNetHTTPClient.AcceptEncoding := AAcceptEncoding;
end;

function TRequestNetHTTP.AcceptEncoding: string;
begin
  Result := FNetHTTPClient.AcceptEncoding;
end;

function TRequestNetHTTP.AddBody(const AContent: string): IRequest;
begin
  Result := Self;
  if not Assigned(FStreamSend) then
    FStreamSend := TStringStream.Create(AContent, TEncoding.UTF8)
  else
    TStringStream(FStreamSend).WriteString(AContent);
  FStreamSend.Position := 0;
end;

function TRequestNetHTTP.AddBody(const AContent: TStream; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  try
    if not Assigned(FStreamSend) then
      FStreamSend := TStringStream.Create;
    TStringStream(FStreamSend).CopyFrom(AContent, AContent.Size);
    FStreamSend.Position := 0;
  finally
    if AOwns then
    begin
      {$IFDEF MSWINDOWS}
        AContent.Free;
      {$ELSE}
        AContent.DisposeOf;
      {$ENDIF}
    end;
  end;
end;

function TRequestNetHTTP.AddBody(const AContent: TJSONObject; const AOwns: Boolean): IRequest;
begin
  Result := Self.AddBody(AContent.ToJSON);
  if AOwns then
  begin
    {$IFDEF MSWINDOWS}
      AContent.Free;
    {$ELSE}
      AContent.DisposeOf;
    {$ENDIF}
  end;
end;

function TRequestNetHTTP.AddBody(const AContent: TObject; const AOwns: Boolean): IRequest;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJson.ObjectToJsonObject(AContent);
  try
    Result := Self.AddBody(LJSONObject, False);
  finally
    LJSONObject.Free;
    if AOwns then
    begin
      {$IFDEF MSWINDOWS}
        AContent.Free;
      {$ELSE}
        AContent.DisposeOf;
      {$ENDIF}
    end;
  end;
end;

function TRequestNetHTTP.AddBody(const AContent: TJSONArray; const AOwns: Boolean): IRequest;
begin
  Result := Self.AddBody(AContent.ToJSON);
  if AOwns then
  begin
    {$IFDEF MSWINDOWS}
      AContent.Free;
    {$ELSE}
      AContent.DisposeOf;
    {$ENDIF}
  end;
end;

function TRequestNetHTTP.AddCookies(const ACookies: TStrings): IRequest;
var
  LURI: TIdURI;
begin
  Result := Self;
  LURI := TIdURI.Create(MakeURL(False));
  try
    if not Assigned(FNetHTTPClient.CookieManager) then
      FNetHTTPClient.CookieManager := TCookieManager.Create;
    FNetHTTPClient.CookieManager.AddServerCookie(ACookies.Text, LURI.URI);
  finally
    {$IFDEF MSWINDOWS}
      ACookies.Free;
    {$ELSE}
      ACookies.DisposeOf;
    {$ENDIF}
    LURI.Free;
  end;
end;

function TRequestNetHTTP.AddCookie(const ACookieName, ACookieValue: string): IRequest;
var
  cookies: TStringList;
begin
  cookies := TStringList.Create;
  try
    cookies.AddPair(ACookieName, ACookieValue);
    Result := AddCookies(cookies);
  finally
    cookies.Free;
  end;
end;

function TRequestNetHTTP.AddField(const AFieldName: string; const AValue: string): IRequest;
begin
  Result := Self;
  FMultipartFormData.AddField(AFieldName, AValue);
  FUseMultipartFormData := True;
end;

function TRequestNetHTTP.AddFile(const AFieldName: string; const AFileName: string; const AContentType: string): IRequest;
begin
  Result := Self;
  if not FileExists(AFileName) then
    Exit;
  {$IF COMPILERVERSION >= 33.0}
    FMultipartFormData.AddFile(AFieldName, AFileName, AContentType);
  {$ELSE}
    FMultipartFormData.AddFile(AFieldName, AFileName);
  {$ENDIF}
  FUseMultipartFormData := True;
end;

function TRequestNetHTTP.AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string; const AContentType: string): IRequest;
{$IF COMPILERVERSION >= 33.0}
var
  lFileName: string;
{$ENDIF}
begin
  Result := Self;
  if not Assigned(AValue) then
    Exit;
  {$IF COMPILERVERSION >= 33.0}
    lFileName := Trim(AFileName);
    if (lFileName = EmptyStr) then
      lFileName := AFieldName;
    AValue.Position := 0;
    FMultipartFormData.AddStream(AFieldName, AValue, lFileName, AContentType);
    FUseMultipartFormData := True;
  {$ENDIF}
end;

function TRequestNetHTTP.AddHeader(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  {$IF COMPILERVERSION >= 34}
    FNetHTTPClient.CustHeaders.Add(AName, AValue);
  {$ELSE}
    {TODO -oAll -cCustoms Headers : Add headers with NetHTTPClient in versions below of 10.4 Sydney}
  {$ENDIF}
end;

function TRequestNetHTTP.AddParam(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if (not AName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
    FParams.Add(AName + '=' + AValue);
end;

function TRequestNetHTTP.AddUrlSegment(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  if not FullRequestURL(False).Contains(AName) then
  begin
    if (not ResourceSuffix.Trim.IsEmpty) and (not ResourceSuffix.EndsWith('/')) then
      ResourceSuffix(ResourceSuffix + '/');
    ResourceSuffix(ResourceSuffix + '{' + AName + '}');
  end;
  if FUrlSegments.IndexOf(AName) < 0 then
    FUrlSegments.Add(Format('%s=%s', [AName, AValue]));
end;

function TRequestNetHTTP.Asynchronous(const AValue: Boolean): IRequest;
begin
  Result := Self;
  FNetHTTPClient.Asynchronous := AValue;
end;

function TRequestNetHTTP.SynchronizedEvents(const AValue: Boolean): IRequest;
begin
  Result := Self;
  FNetHTTPClient.SynchronizeEvents := AValue;
end;

function TRequestNetHTTP.BaseURL(const ABaseURL: string): IRequest;
begin
  Result := Self;
  FBaseURL := ABaseURL;
end;

function TRequestNetHTTP.BaseURL: string;
begin
  Result := FBaseURL;
end;

function TRequestNetHTTP.BasicAuthentication(const AUsername, APassword: string): IRequest;
begin
  Result := Self.AddHeader('Authorization', 'Basic ' + TNetEncoding.Base64.Encode(AUsername + ':' + APassword));
end;

function TRequestNetHTTP.ClearBody: IRequest;
begin
  Result := Self;
  if Assigned(FStreamSend) then
    FreeAndNil(FStreamSend);
end;

function TRequestNetHTTP.ClearHeaders: IRequest;
{$IF COMPILERVERSION >= 34}
var
  LURLHeaders:TNetHeader;
{$ENDIF}
begin
  Result := Self;
  {$IF COMPILERVERSION >= 34}
    for LURLHeaders in FNetHTTPClient.CustHeaders do
      FNetHTTPClient.CustHeaders.Delete(LURLHeaders.Name);
  {$ELSE}
    {TODO -oAll -cCustom Headers : Clear headers with NetHTTPClient in versions below of 10.4 Sydney}
  {$ENDIF}
end;

function TRequestNetHTTP.ClearParams: IRequest;
begin
  Result := Self;
  FParams.Clear;
end;

function TRequestNetHTTP.ContentType: string;
begin
  Result := FNetHTTPClient.CustHeaders.Value['Content-Type'];
end;

function TRequestNetHTTP.ContentType(const AContentType: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Content-Type', AContentType);
end;

constructor TRequestNetHTTP.Create;
begin
  FNetHTTPClient := TNetHTTPClient.Create(nil);;
  FNetHTTPClient.ResponseTimeout := 900000;
  FNetHTTPClient.ConnectionTimeout := 900000;
  FNetHTTPClient.UserAgent := 'User-Agent:Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.96 Safari/537.36';
  FNetHTTPClient.AcceptCharSet := 'utf-8';
  FNetHTTPClient.AcceptEncoding := 'utf-8';
  FNetHTTPClient.HandleRedirects := True;
  FNetHTTPClient.ContentType := 'application/json';
  FNetHTTPClient.OnRequestError := DoHTTPProtocolError;
  FNetHTTPClient.OnRequestCompleted := DoAfterExecute;
  FNetHTTPClient.Asynchronous := False;
  FNetHTTPClient.SynchronizeEvents := True;

  FParams := TStringList.Create;
  FUrlSegments := TStringList.Create;

  FResponse := TResponseNetHTTP.Create;
  TResponseNetHTTP(FResponse).SetContent(FStreamResult);

  FStreamResult := TStringStream.Create;
  Self.ContentType('application/json');
  FRetries := 0;

  FMultipartFormData := TMultipartFormData.Create;
  FUseMultipartFormData := False;
end;

function TRequestNetHTTP.DeactivateProxy: IRequest;
begin
  Result := Self;
  FNetHTTPClient.ProxySettings := TProxySettings.Create(EmptyStr, 0);
end;

function TRequestNetHTTP.Delete: IResponse;
begin
  Result := FResponse;
  DoBeforeExecute(FNetHTTPClient);
  TResponseNetHTTP(FResponse).SetHTTPResponse(ExecuteRequest(mrDELETE));
end;

destructor TRequestNetHTTP.Destroy;
begin
  if Assigned(FNetHTTPClient) then
    FNetHTTPClient.Free;
  if Assigned(FParams) then
    FParams.Free;
  if Assigned(FUrlSegments) then
    FUrlSegments.Free;
  if Assigned(FStreamSend) then
    FStreamSend.Free;
  if Assigned(FStreamResult) then
    FStreamResult.Free;
  if Assigned(FMultipartFormData) then
    FMultipartFormData.Free;
  inherited;
end;

function TRequestNetHTTP.Adapters(const AAdapter: IRequestAdapter): IRequest;
begin
  Result := Adapters([AAdapter]);
end;

function TRequestNetHTTP.Adapters(const AAdapters: TArray<IRequestAdapter>): IRequest;
begin
  FAdapters := AAdapters;
  Result := Self;
end;

function TRequestNetHTTP.Adapters: TArray<IRequestAdapter>;
begin
  Result := FAdapters;
end;

procedure TRequestNetHTTP.DoAfterExecute(const Sender: TObject; const AResponse: IHTTPResponse);
var
  LAdapter: IRequestAdapter;
begin
  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self, FResponse);
  for LAdapter in FAdapters do
    LAdapter.Execute(FResponse.Content);
end;

procedure TRequestNetHTTP.DoBeforeExecute(const Sender: TNetHTTPClient);
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);
end;

procedure TRequestNetHTTP.DoHTTPProtocolError(const Sender: TObject; const AError: string);
begin
  // virtual method
end;

function TRequestNetHTTP.ExecuteRequest(const AMethod: TMethodRequest): IHTTPResponse;
var
  LAttempts: Integer;
begin
  LAttempts := FRetries + 1;
  while LAttempts > 0 do
  begin
    try
      case AMethod of
        mrGET:
          Result := FNetHTTPClient.Get(TIdURI.URLEncode(MakeURL), FStreamResult);
        mrPOST:
        begin
          if (Assigned(FMultipartFormData.Stream) and (FUseMultipartFormData = True)) then
          begin
            FNetHTTPClient.ContentType := EmptyStr;
            FUseMultipartFormData := False;
            Result := FNetHTTPClient.Post(TIdURI.URLEncode(MakeURL), FMultipartFormData, FStreamResult);
          end
          else
            Result := FNetHTTPClient.Post(TIdURI.URLEncode(MakeURL), FStreamSend, FStreamResult);
        end;
        mrPUT:
        begin
          if (Assigned(FMultipartFormData.Stream) and (FUseMultipartFormData = True)) then
          begin
            FNetHTTPClient.ContentType := EmptyStr;
            FUseMultipartFormData := False;
            {$IF COMPILERVERSION >= 33.0}
              Result := FNetHTTPClient.Put(TIdURI.URLEncode(MakeURL), FMultipartFormData, FStreamResult);
            {$ENDIF}
          end
          else
            Result := FNetHTTPClient.Put(TIdURI.URLEncode(MakeURL), FStreamSend, FStreamResult);
        end;
        mrPATCH:
          Result := FNetHTTPClient.Patch(TIdURI.URLEncode(MakeURL), FStreamSend, FStreamResult);
        mrDELETE:
          Result := FNetHTTPClient.Delete(TIdURI.URLEncode(MakeURL), FStreamResult);
      end;
      if Assigned(Result) then
        LAttempts := 0
      else
        LAttempts := LAttempts - 1;
    except
      begin
        LAttempts := LAttempts - 1;
        if LAttempts = 0 then
          raise;
      end;
    end;
  end;
end;

function TRequestNetHTTP.FullRequestURL(const AIncludeParams: Boolean): string;
begin
  Result := Self.MakeURL(AIncludeParams);
end;

function TRequestNetHTTP.Get: IResponse;
begin
  Result := FResponse;
  DoBeforeExecute(FNetHTTPClient);
  TResponseNetHTTP(FResponse).SetHTTPResponse(ExecuteRequest(mrGET));
end;

function TRequestNetHTTP.MakeURL(const AIncludeParams: Boolean): string;
var
  I: Integer;
begin
  Result := FBaseURL;
  if not FResource.Trim.IsEmpty then
  begin
    if not Result.EndsWith('/') then
      Result := Result + '/';
    Result := Result + FResource;
  end;
  if not FResourceSuffix.Trim.IsEmpty then
  begin
    if not Result.EndsWith('/') then
      Result := Result + '/';
    Result := Result + FResourceSuffix;
  end;
  if FUrlSegments.Count > 0 then
  begin
    for I := 0 to Pred(FUrlSegments.Count) do
    begin
      Result := StringReplace(Result, Format('{%s}', [FUrlSegments.Names[I]]), FUrlSegments.ValueFromIndex[I], [rfReplaceAll, rfIgnoreCase]);
      Result := StringReplace(Result, Format(':%s', [FUrlSegments.Names[I]]), FUrlSegments.ValueFromIndex[I], [rfReplaceAll, rfIgnoreCase]);
    end;
  end;
  if not AIncludeParams then
    Exit;
  if FParams.Count > 0 then
  begin
    Result := Result + '?';
    for I := 0 to Pred(FParams.Count) do
    begin
      if I > 0 then
        Result := Result + '&';
      Result := Result + FParams.Strings[I];
    end;
  end;
end;

function TRequestNetHTTP.Patch: IResponse;
begin
  Result := FResponse;
  DoBeforeExecute(FNetHTTPClient);
  TResponseNetHTTP(FResponse).SetHTTPResponse(ExecuteRequest(mrPATCH));
end;

function TRequestNetHTTP.Post: IResponse;
begin
  Result := FResponse;
  DoBeforeExecute(FNetHTTPClient);
  TResponseNetHTTP(FResponse).SetHTTPResponse(ExecuteRequest(mrPOST));
end;

function TRequestNetHTTP.Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
begin
  Result := Self;
  FNetHTTPClient.ProxySettings := TProxySettings.Create(AServer, APort, AUsername, APassword);
end;

function TRequestNetHTTP.Put: IResponse;
begin
  Result := FResponse;
  DoBeforeExecute(FNetHTTPClient);
  TResponseNetHTTP(FResponse).SetHTTPResponse(ExecuteRequest(mrPUT));
end;

function TRequestNetHTTP.RaiseExceptionOn500(const ARaiseException: Boolean): IRequest;
begin
  raise Exception.Create('Not implemented');
end;

function TRequestNetHTTP.RaiseExceptionOn500: Boolean;
begin
  raise Exception.Create('Not implemented');
end;

function TRequestNetHTTP.Resource: string;
begin
  Result := FResource;
end;

function TRequestNetHTTP.Resource(const AResource: string): IRequest;
begin
  Result := Self;
  FResource := AResource;
end;

function TRequestNetHTTP.ResourceSuffix(const AResourceSuffix: string): IRequest;
begin
  Result := Self;
  FResourceSuffix := AResourceSuffix;
end;

function TRequestNetHTTP.ResourceSuffix: string;
begin
  Result := FResourceSuffix;
end;

function TRequestNetHTTP.Retry(const ARetries: Integer): IRequest;
begin
  Result := Self;
  FRetries := ARetries;
end;

function TRequestNetHTTP.OnBeforeExecute(const AOnBeforeExecute: TRR4DCallbackOnBeforeExecute): IRequest;
begin
  Result := Self;
  FOnBeforeExecute := AOnBeforeExecute;
end;

function TRequestNetHTTP.OnAfterExecute(const AOnAfterExecute: TRR4DCallbackOnAfterExecute): IRequest;
begin
  Result := Self;
  FOnAfterExecute := AOnAfterExecute;
end;

function TRequestNetHTTP.Timeout: Integer;
begin
  Result := FNetHTTPClient.ResponseTimeout;
end;

function TRequestNetHTTP.Timeout(const ATimeout: Integer): IRequest;
begin
  Result := Self;
  FNetHTTPClient.ResponseTimeout := ATimeout;
  FNetHTTPClient.ConnectionTimeout := ATimeout;
end;

function TRequestNetHTTP.Token(const AToken: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Authorization', AToken);
end;

function TRequestNetHTTP.TokenBearer(const AToken: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Authorization', 'Bearer ' + AToken);
end;

function TRequestNetHTTP.UserAgent(const AName: string): IRequest;
begin
  Result := Self;
  FNetHTTPClient.UserAgent := AName;
end;

end.
