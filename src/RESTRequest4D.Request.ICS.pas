unit RESTRequest4D.Request.ICS;

interface

uses RESTRequest4D.Request.Contract, RESTRequest4D.Response.Contract, OverbyteIcsLogger, OverbyteIcsSSLEAY, OverbyteIcsWndControl,
  OverbyteIcsHttpProt, OverbyteIcsUrl, OverbyteIcsWSocket, RESTRequest4D.Utils, OverbyteIcsSslHttpRest, NetEncoding,
  RESTRequest4D.Request.Adapter.Contract, Data.DB, System.Classes, System.JSON;

type
  TRequestICS = class(TInterfacedObject, IRequest)
  private
    FSslHttpRest: TSslHttpRest;
    FBaseURL: string;
    FResource: string;
    FResourceSuffix: string;
    FResponse: IResponse;
    FBodyRaw: TStringList;
    FRetries: Integer;
    FUrlSegments: TStrings;
    FAdapters: TArray<IRequestAdapter>;
    FOnBeforeExecute: TRR4DCallbackOnBeforeExecute;
    FOnAfterExecute: TRR4DCallbackOnAfterExecute;
    function OnBeforeExecute(const AOnBeforeExecute: TRR4DCallbackOnBeforeExecute): IRequest;
    function OnAfterExecute(const AOnAfterExecute: TRR4DCallbackOnAfterExecute): IRequest;
    function Adapters(const AAdapter: IRequestAdapter): IRequest; overload;
    function Adapters(const AAdapters: TArray<IRequestAdapter>): IRequest; overload;
    function Adapters: TArray<IRequestAdapter>; overload;
    procedure ExecuteRequest(const AMethod: TMethodRequest);
    function AcceptEncoding: string; overload;
    function AcceptEncoding(const AAcceptEncoding: string): IRequest; overload;
    function AcceptCharset: string; overload;
    function AcceptCharset(const AAcceptCharset: string): IRequest; overload;
    function Accept: string; overload;
    function Accept(const AAccept: string): IRequest; overload;
    function Timeout: Integer; overload;
    function Timeout(const ATimeout: Integer): IRequest; overload;
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
    function Get: IResponse;
    function Post: IResponse;
    function Put: IResponse;
    function Delete: IResponse;
    function Patch: IResponse;
    function FullRequestURL(const AIncludeParams: Boolean = True): string;
    function ClearBody: IRequest;
    function AddParam(const AName, AValue: string): IRequest;
    function AddBody(const AContent: string): IRequest; overload;
    function AddHeader(const AName, AValue: string): IRequest;
    function AddBody(const AContent: TJSONObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TJSONArray; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TStream; const AOwns: Boolean = True): IRequest; overload;
    function AddUrlSegment(const AName, AValue: string): IRequest;
    function ClearHeaders: IRequest;
    function ClearParams: IRequest;
    function UserAgent(const AName: string): IRequest;
    function ContentType(const AContentType: string): IRequest; overload;
    function ContentType: string; overload;
    function AddCookies(const ACookies: TStrings): IRequest;
    function AddCookie(const ACookieName, ACookieValue: string): IRequest;
    function AddFile(const AFileName: string; UploadStrat: THttpUploadStrat): IRequest; overload;
    function AddField(const AFieldName: string; const AValue: string): IRequest; overload;
    function Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
    function DeactivateProxy: IRequest;
    function CertFile(const APath: string): IRequest;
    function KeyFile(const APath: string): IRequest;
    function MakeURL(const AIncludeParams: Boolean = True): string;
  protected
    procedure DoBeforeExecute; virtual;
    procedure DoAfterExecute; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses RESTRequest4D.Response.ICS, System.SysUtils, REST.JSON;

function TRequestICS.OnBeforeExecute(const AOnBeforeExecute: TRR4DCallbackOnBeforeExecute): IRequest;
begin
  Result := Self;
  FOnBeforeExecute := AOnBeforeExecute;
end;

function TRequestICS.OnAfterExecute(const AOnAfterExecute: TRR4DCallbackOnAfterExecute): IRequest;
begin
  Result := Self;
  FOnAfterExecute := AOnAfterExecute;
end;

function TRequestICS.Adapters: TArray<IRequestAdapter>;
begin
  Result := FAdapters;
end;

function TRequestICS.Adapters(const AAdapters: TArray<IRequestAdapter>): IRequest;
begin
  FAdapters := AAdapters;
  Result := Self;
end;

function TRequestICS.Adapters(const AAdapter: IRequestAdapter): IRequest;
begin
  Result := Adapters([AAdapter]);
end;

function TRequestICS.AddBody(const AContent: TStream; const AOwns: Boolean): IRequest;
begin
  try
    FBodyRaw.LoadFromStream(AContent);
  finally
    if AOwns then
      AContent.Free;
  end;
end;

function TRequestICS.AddCookie(const ACookieName, ACookieValue: string): IRequest;
begin
  raise Exception.Create('Not implemented');
end;

function TRequestICS.AddCookies(const ACookies: TStrings): IRequest;
begin
  raise Exception.Create('Not implemented');
end;

function TRequestICS.AddField(const AFieldName, AValue: string): IRequest;
begin
  Result := Self;
  FSslHttpRest.RestParams.AddItem(AFieldName, AValue);
  FSslHttpRest.RestParams.PContent := PContUrlencoded;
end;

function TRequestICS.AddFile(const AFileName: string; UploadStrat: THttpUploadStrat): IRequest;
begin
  Result := Self;
  FSslHttpRest.HttpUploadFile := AFileName;
  FSslHttpRest.HttpUploadStrat := UploadStrat;
end;

function TRequestICS.RaiseExceptionOn500: Boolean;
begin
  raise Exception.Create('Not implemented');
end;

function TRequestICS.RaiseExceptionOn500(const ARaiseException: Boolean): IRequest;
begin
  raise Exception.Create('Not implemented');
end;

function TRequestICS.Patch: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrPATCH);
end;

function TRequestICS.Put: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrPUT);
end;

function TRequestICS.Post: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrPOST);
end;

function TRequestICS.Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
begin
  Result := Self;
  FSslHttpRest.Proxy := AServer;
  FSslHttpRest.ProxyUsername := AUsername;
  FSslHttpRest.ProxyPassword := APassword;
  FSslHttpRest.ProxyPort := IntToStr(APort);
end;

function TRequestICS.Get: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrGET);
end;

function TRequestICS.KeyFile(const APath: string): IRequest;
begin
  Result := Self;
  FSslHttpRest.SslCliCert.PrivateKeyLoadFromPemFile(APath);
end;

function TRequestICS.DeactivateProxy: IRequest;
begin
  FSslHttpRest.ProxyURL := '';
  FSslHttpRest.ProxyUsername := '';
  FSslHttpRest.ProxyPassword := '';
  FSslHttpRest.ProxyPort := '';
end;

function TRequestICS.Delete: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrDELETE);
end;

function TRequestICS.AddBody(const AContent: string): IRequest;
begin
  Result := Self;
  FBodyRaw.Clear;
  Self.ContentType('application/json; charset=UTF-8');
  FSslHttpRest.RestParams.PContent := PContBodyJson;
  FBodyRaw.Add(AContent);
end;

function TRequestICS.AddHeader(const AName, AValue: string): IRequest;
begin
  Result := Self;
  FSslHttpRest.ExtraHeaders.Add(Format('%s: %s', [AName, AValue]));
end;

function TRequestICS.Token(const AToken: string): IRequest;
begin
  Result := Self;
  FSslHttpRest.ServerAuth := httpAuthToken;
  FSslHttpRest.AuthBearerToken := AToken;
end;

function TRequestICS.TokenBearer(const AToken: string): IRequest;
begin
  Result := Self;
  FSslHttpRest.ServerAuth := httpAuthBearer;
  FSslHttpRest.AuthBearerToken := AToken;
end;

function TRequestICS.FullRequestURL(const AIncludeParams: Boolean): string;
begin
  Result := Self.MakeURL(AIncludeParams);
end;

function TRequestICS.MakeURL(const AIncludeParams: Boolean): string;
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
      Result := StringReplace(Result, Format('{%s}', [FUrlSegments.Names[I]]), FUrlSegments.ValueFromIndex[I],
        [rfReplaceAll, rfIgnoreCase]);
      Result := StringReplace(Result, Format(':%s', [FUrlSegments.Names[I]]), FUrlSegments.ValueFromIndex[I],
        [rfReplaceAll, rfIgnoreCase]);
    end;
  end;
  if not AIncludeParams then
    Exit;
end;

function TRequestICS.AddParam(const AName, AValue: string): IRequest;
begin
  Result := Self;
  FSslHttpRest.RestParams.AddItem(AName, AValue);
end;

function TRequestICS.AddUrlSegment(const AName, AValue: string): IRequest;
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

function TRequestICS.ClearParams: IRequest;
begin
  Result := Self;
  FSslHttpRest.RestParams.Clear;
end;

function TRequestICS.ContentType: string;
begin
  Result := FSslHttpRest.ContentTypePost;
end;

function TRequestICS.ContentType(const AContentType: string): IRequest;
begin
  Result := Self;
  FSslHttpRest.ContentTypePost := AContentType;
end;

function TRequestICS.AddBody(const AContent: TObject; const AOwns: Boolean): IRequest;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJson.ObjectToJsonObject(AContent);
  try
    Result := Self.AddBody(LJSONObject, False);
  finally
    LJSONObject.Free;
    if AOwns then
      AContent.Free;
  end;
end;

function TRequestICS.CertFile(const APath: string): IRequest;
begin
  Result := Self;
  FSslHttpRest.SslCliCert.LoadFromFile(APath);
end;

function TRequestICS.ClearBody: IRequest;
begin
  Result := Self;
  FBodyRaw.Clear;
end;

function TRequestICS.AddBody(const AContent: TJSONArray; const AOwns: Boolean): IRequest;
begin
  Result := Self.AddBody(AContent.ToJSON);
  if AOwns then
    AContent.Free;
end;

function TRequestICS.AddBody(const AContent: TJSONObject; const AOwns: Boolean): IRequest;
begin
  Result := Self.AddBody(AContent.ToJSON);
  if AOwns then
    AContent.Free;
end;

function TRequestICS.ClearHeaders: IRequest;
begin
  Result := Self;
  FSslHttpRest.ExtraHeaders.Clear;
end;

function TRequestICS.Accept: string;
begin
  Result := FSslHttpRest.Accept;
end;

function TRequestICS.Accept(const AAccept: string): IRequest;
begin
  Result := Self;
  FSslHttpRest.Accept := AAccept;
end;

function TRequestICS.AcceptCharset(const AAcceptCharset: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Accept-Charset', AAcceptCharset);
end;

function TRequestICS.AcceptCharset: string;
begin
  Result := FSslHttpRest.ExtraHeaders.Values['Accept-Charset'];
end;

function TRequestICS.AcceptEncoding(const AAcceptEncoding: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Accept-Encoding', AAcceptEncoding);
end;

function TRequestICS.AcceptEncoding: string;
begin
  Result := FSslHttpRest.ExtraHeaders.Values['Accept-Encoding'];
end;

function TRequestICS.BaseURL: string;
begin
  Result := FBaseURL;
end;

function TRequestICS.BaseURL(const ABaseURL: string): IRequest;
begin
  Result := Self;
  FBaseURL := ABaseURL;
end;

function TRequestICS.BasicAuthentication(const AUsername, APassword: string): IRequest;
begin
  Result := Self;
  FSslHttpRest.Username := AUsername;
  FSslHttpRest.Password := APassword;
  FSslHttpRest.ServerAuth := httpAuthBasic;
end;

constructor TRequestICS.Create;
begin
  FSslHttpRest := TSslHttpRest.Create(nil);
  OverbyteIcsWSocket.LoadSsl;
  FSslHttpRest.DebugLevel := DebugHdr;
  FBodyRaw := TStringList.Create;
  FUrlSegments := TStringList.Create;
  FBodyRaw.LineBreak := '';
  FResponse := TResponseICS.Create(FSslHttpRest);
end;

destructor TRequestICS.Destroy;
begin
  OverbyteIcsWSocket.UnloadSsl;
  FreeAndNil(FSslHttpRest);
  FreeAndNil(FBodyRaw);
  FreeAndNil(FUrlSegments);
  inherited;
end;

procedure TRequestICS.DoAfterExecute;
var
  LAdapter: IRequestAdapter;
begin
  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self, FResponse);
  for LAdapter in FAdapters do
    LAdapter.Execute(FResponse.Content);
end;

procedure TRequestICS.DoBeforeExecute;
begin
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(Self);
end;

procedure TRequestICS.ExecuteRequest(const AMethod: TMethodRequest);
var
  LAttempts: Integer;
begin
  LAttempts := FRetries + 1;
  while LAttempts > 0 do
  begin
    try
      Self.DoBeforeExecute;
      case AMethod of
        mrGET:
          FSslHttpRest.RestRequest(httpGET, (MakeURL), False);
        mrPOST:
          FSslHttpRest.RestRequest(httpPOST, (MakeURL), False, FBodyRaw.Text);
        mrPUT:
          FSslHttpRest.RestRequest(httpPUT, (MakeURL), False, FBodyRaw.Text);
        mrPATCH:
          FSslHttpRest.RestRequest(httpPATCH, (MakeURL), False, FBodyRaw.Text);
        mrDELETE:
          FSslHttpRest.RestRequest(httpDELETE, (MakeURL), False, FBodyRaw.Text);
      end;
      LAttempts := 0;
      Self.DoAfterExecute;
    except
      begin
        LAttempts := LAttempts - 1;
        if LAttempts = 0 then
          raise;
      end;
    end;
  end;
end;

function TRequestICS.Resource: string;
begin
  Result := FResource;
end;

function TRequestICS.Resource(const AResource: string): IRequest;
begin
  Result := Self;
  FResource := AResource;
end;

function TRequestICS.ResourceSuffix(const AResourceSuffix: string): IRequest;
begin
  Result := Self;
  FResourceSuffix := AResourceSuffix;
end;

function TRequestICS.ResourceSuffix: string;
begin
  Result := FResourceSuffix;
end;

function TRequestICS.Retry(const ARetries: Integer): IRequest;
begin
  Result := Self;
  FRetries := ARetries;
end;

function TRequestICS.Timeout: Integer;
begin
  Result := FSslHttpRest.Timeout;
end;

function TRequestICS.Timeout(const ATimeout: Integer): IRequest;
begin
  Result := Self;
  FSslHttpRest.Timeout := ATimeout;
end;

function TRequestICS.UserAgent(const AName: string): IRequest;
begin
  Result := Self;
  FSslHttpRest.Agent := AName;
end;

end.
