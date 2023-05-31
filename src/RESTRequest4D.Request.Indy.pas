unit RESTRequest4D.Request.Indy;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses RESTRequest4D.Request.Contract, RESTRequest4D.Response.Contract, IdHTTP, IdSSLOpenSSL, IdCTypes, IdSSLOpenSSLHeaders,
  RESTRequest4D.Utils, RESTRequest4D.Request.Adapter.Contract, IdMultipartFormData,
  {$IFDEF FPC}
    DB, Classes, fpjson, jsonparser, fpjsonrtti, SysUtils;
  {$ELSE}
    Data.DB, System.Classes, System.JSON, System.SysUtils, REST.Json;
  {$ENDIF}

type
  TRequestIndy = class(TInterfacedObject, IRequest)
  private
    FIdMultiPartFormDataStream: TIdMultiPartFormDataStream;
    FHeaders: TStrings;
    FParams: TStrings;
    FUrlSegments: TStrings;
    FIdHTTP: TIdHTTP;
    FIdSSLIOHandlerSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;
    FBaseURL: string;
    FResource: string;
    FResourceSuffix: string;
    FAdapters: TArray<IRequestAdapter>;
    FResponse: IResponse;
    FStreamSend: TStream;
    FStreamResult: TStringStream;
    FRetries: Integer;
    procedure ExecuteRequest(const AMethod: TMethodRequest);
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
    function ContentType(const AContentType: string): IRequest;
    function UserAgent(const AName: string): IRequest;
    function AddCookies(const ACookies: TStrings): IRequest;
    function AddCookie(const ACookieName, ACookieValue: string): IRequest;
    function AddParam(const AName, AValue: string): IRequest;
    function AddField(const AFieldName: string; const AValue: string): IRequest; overload;
    function AddFile(const AFieldName: string; const AFileName: string; const AContentType: string = ''): IRequest; overload;
    function AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string = ''; const AContentType: string = ''): IRequest; overload;
    function MakeURL(const AIncludeParams: Boolean = True): string;
    function Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
    function DeactivateProxy: IRequest;
    function CertFile(const APath: string): IRequest;
    function KeyFile(const APath: string): IRequest;
    function HTTPOptions(const AHTTPOptions: TIdHTTPOptions): IRequest;
    procedure OnStatusInfoEx(ASender: TObject; const AsslSocket: PSSL; const AWhere, Aret: TIdC_INT; const AType, AMsg: string);
  protected
    procedure DoAfterExecute; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses RESTRequest4D.Response.Indy, IdURI, IdCookieManager;

function TRequestIndy.AddField(const AFieldName: string; const AValue: string): IRequest;
begin
  Result := Self;
  {$IF DEFINED(RR4D_INDY)}
    FIdMultiPartFormDataStream.AddFormField(AFieldName, AValue, EmptyStr, ' ').ContentTransfer:= '8bit';
  {$ENDIF}
end;

function TRequestIndy.AddFile(const AFieldName: string; const AFileName: string; const AContentType: string): IRequest;
begin
  Result := Self;
  if not FileExists(AFileName) then
    Exit;
  {$IF DEFINED(RR4D_INDY)}
    FIdMultiPartFormDataStream.AddFile(AFieldName, AFileName, AContentType);
  {$ENDIF}
end;

function TRequestIndy.AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string; const AContentType: string): IRequest;
var
  lFileName: string;
begin
  Result := Self;
  if not Assigned(AValue) then
    Exit;
  {$IF DEFINED(RR4D_INDY)}
    lFileName := Trim(AFileName);
    if (lFileName = EmptyStr) then
      lFileName := AFieldName;
    AValue.Position := 0;
    FIdMultiPartFormDataStream.AddFormField(AFieldName, AContentType, EmptyStr, AValue, lFileName);
  {$ENDIF}
end;

function TRequestIndy.AddBody(const AContent: TStream; const AOwns: Boolean): IRequest;
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
      {$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
        AContent.Free;
      {$ELSE}
        AContent.DisposeOf;
      {$ENDIF}
    end;
  end;
end;

function TRequestIndy.AddCookies(const ACookies: TStrings): IRequest;
var
  LURI: TIdURI;
begin
  Result := Self;
  LURI := TIdURI.Create(MakeURL(False));
  try
    if not Assigned(FIdHTTP.CookieManager) then
      FIdHTTP.CookieManager := TIdCookieManager.Create(FIdHTTP);
    FIdHTTP.CookieManager.AddServerCookies(ACookies, LURI);
  finally
    {$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
      ACookies.Free;
    {$ELSE}
      ACookies.DisposeOf;
    {$ENDIF}
    LURI.Free;
  end;
end;

function TRequestIndy.AddCookie(const ACookieName, ACookieValue: string): IRequest;
var
  cookies: TStringList;
begin
  cookies := TStringList.Create;
  cookies.AddPair(ACookieName, ACookieValue);
  Result := AddCookies(cookies);
end;

function TRequestIndy.RaiseExceptionOn500: Boolean;
begin
  Result := not (hoNoProtocolErrorException in FIdHTTP.HTTPOptions);
end;

function TRequestIndy.RaiseExceptionOn500(const ARaiseException: Boolean): IRequest;
begin
  Result := Self;
  if ARaiseException then
    FIdHTTP.HTTPOptions := FIdHTTP.HTTPOptions - [hoNoProtocolErrorException]
  else
    FIdHTTP.HTTPOptions := FIdHTTP.HTTPOptions + [hoNoProtocolErrorException];
end;

function TRequestIndy.Adapters(const AAdapter: IRequestAdapter): IRequest;
begin
  Result := Adapters([AAdapter]);
end;

function TRequestIndy.Adapters(const AAdapters: TArray<IRequestAdapter>): IRequest;
begin
  FAdapters := AAdapters;
  Result := Self;
end;

function TRequestIndy.Adapters: TArray<IRequestAdapter>;
begin
  Result := FAdapters;
end;

procedure TRequestIndy.DoAfterExecute;
var
  LAdapter: IRequestAdapter;
begin
  for LAdapter in FAdapters do
    LAdapter.Execute(FResponse.Content);
end;

procedure TRequestIndy.ExecuteRequest(const AMethod: TMethodRequest);
var
  LAttempts: Integer;
begin
  LAttempts := FRetries + 1;
  while LAttempts > 0 do
  begin
    try
      case AMethod of
        mrGET:
          FIdHTTP.Get(TIdURI.URLEncode(MakeURL), FStreamResult);
        mrPOST:
        begin
          if (Assigned(FIdMultiPartFormDataStream) and (FIdMultiPartFormDataStream.Size > 0)) then
            FIdHTTP.Post(TIdURI.URLEncode(MakeURL), FIdMultiPartFormDataStream, FStreamResult)
          else
            FIdHTTP.Post(TIdURI.URLEncode(MakeURL), FStreamSend, FStreamResult);
        end;
        mrPUT:
        begin
          if (Assigned(FIdMultiPartFormDataStream) and (FIdMultiPartFormDataStream.Size > 0)) then
            raise Exception.Create('Method Put not supported multipart/form-data.')
          else
            FIdHTTP.Put(TIdURI.URLEncode(MakeURL), FStreamSend, FStreamResult);
        end;
        mrPATCH:
          FIdHTTP.Patch(TIdURI.URLEncode(MakeURL), FStreamSend, FStreamResult);
        mrDELETE:
          FIdHTTP.Delete(TIdURI.URLEncode(MakeURL), FStreamResult);
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

function TRequestIndy.Patch: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrPATCH);
end;

function TRequestIndy.Put: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrPUT);
end;

function TRequestIndy.Post: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrPOST);
end;

function TRequestIndy.Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
begin
  Result := Self;
  FIdHTTP.ProxyParams.ProxyServer := AServer;
  FIdHTTP.ProxyParams.ProxyPassword := APassword;
  FIdHTTP.ProxyParams.ProxyUsername := AUsername;
  FIdHTTP.ProxyParams.ProxyPort := APort;
end;

function TRequestIndy.Get: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrGET);
end;

function TRequestIndy.HTTPOptions(const AHTTPOptions: TIdHTTPOptions): IRequest;
begin
  Result := Self;
  FIdHTTP.HTTPOptions := AHTTPOptions;
end;

function TRequestIndy.DeactivateProxy: IRequest;
begin
  Result := Self;
  FIdHTTP.ProxyParams.ProxyServer := EmptyStr;
  FIdHTTP.ProxyParams.ProxyPassword := EmptyStr;
  FIdHTTP.ProxyParams.ProxyUsername := EmptyStr;
  FIdHTTP.ProxyParams.ProxyPort := 0;
end;

function TRequestIndy.CertFile(const APath: string): IRequest;
begin
  Result := Self;
  FIdSSLIOHandlerSocketOpenSSL.SSLOptions.CertFile := APath;
end;

function TRequestIndy.KeyFile(const APath: string): IRequest;
begin
  Result := Self;
  FIdSSLIOHandlerSocketOpenSSL.SSLOptions.KeyFile := APath;
end;

function TRequestIndy.Delete: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrDELETE);
end;

function TRequestIndy.AddBody(const AContent: string): IRequest;
begin
  Result := Self;
  if not Assigned(FStreamSend) then
    FStreamSend := TStringStream.Create(AContent, TEncoding.UTF8)
  else
    TStringStream(FStreamSend).WriteString(AContent);
  FStreamSend.Position := 0;
end;

function TRequestIndy.AddHeader(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  if FHeaders.IndexOf(AName) < 0 then
    FHeaders.Add(AName);
  FIdHTTP.Request.CustomHeaders.AddValue(AName, AValue);
end;

function TRequestIndy.Token(const AToken: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Authorization', AToken);
end;

function TRequestIndy.TokenBearer(const AToken: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Authorization', 'Bearer ' + AToken);
end;

function TRequestIndy.FullRequestURL(const AIncludeParams: Boolean): string;
begin
  Result := Self.MakeURL(AIncludeParams);
end;

function TRequestIndy.MakeURL(const AIncludeParams: Boolean): string;
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

procedure TRequestIndy.OnStatusInfoEx(ASender: TObject; const AsslSocket: PSSL; const AWhere, Aret: TIdC_INT; const AType, AMsg: string);
begin
  SSL_set_tlsext_host_name(AsslSocket, FIdHTTP.URL.Host);
end;

function TRequestIndy.AddParam(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if (not AName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
    FParams.Add(AName + '=' + AValue);
end;

function TRequestIndy.AddUrlSegment(const AName, AValue: string): IRequest;
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

function TRequestIndy.ClearParams: IRequest;
begin
  Result := Self;
  FParams.Clear;
end;

function TRequestIndy.ContentType(const AContentType: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Content-Type', AContentType);
end;

function TRequestIndy.AddBody(const AContent: TObject; const AOwns: Boolean): IRequest;
var
  LJSONObject: TJSONObject;
  {$IFDEF FPC}
    LJSONStreamer: TJSONStreamer;
  {$ENDIF}
begin
  {$IFDEF FPC}
    LJSONStreamer := TJSONStreamer.Create(NIL);
    LJSONObject := LJSONStreamer.ObjectToJSON(AContent);
  {$ELSE}
    LJSONObject := TJson.ObjectToJsonObject(AContent);
  {$ENDIF}
  try
    Result := Self.AddBody(LJSONObject, False);
  finally
    {$IFDEF FPC}
      LJSONStreamer.Free;
    {$ENDIF}
    LJSONObject.Free;
    if AOwns then
    begin
      {$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
        AContent.Free;
      {$ELSE}
        AContent.DisposeOf;
      {$ENDIF}
    end;
  end;
end;

function TRequestIndy.ClearBody: IRequest;
begin
  Result := Self;
  if Assigned(FStreamSend) then
    FreeAndNil(FStreamSend);
end;

function TRequestIndy.AddBody(const AContent: TJSONArray; const AOwns: Boolean): IRequest;
begin
  {$IFDEF FPC}
    Result := Self.AddBody(AContent.AsJSON);
  {$ELSE}
    Result := Self.AddBody(AContent.ToJSON);
  {$ENDIF}
  if AOwns then
  begin
    {$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
      AContent.Free;
    {$ELSE}
      AContent.DisposeOf;
    {$ENDIF}
  end;
end;

function TRequestIndy.AddBody(const AContent: TJSONObject; const AOwns: Boolean): IRequest;
begin
  {$IFDEF FPC}
    Result := Self.AddBody(AContent.AsJSON);
  {$ELSE}
    Result := Self.AddBody(AContent.ToJSON);
  {$ENDIF}
  if AOwns then
  begin
    {$IF DEFINED(MSWINDOWS) OR DEFINED(FPC)}
      AContent.Free;
    {$ELSE}
      AContent.DisposeOf;
    {$ENDIF}
  end;
end;

function TRequestIndy.ClearHeaders: IRequest;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to Pred(FHeaders.Count) do
    FIdHTTP.Request.CustomHeaders.Delete(FIdHTTP.Request.CustomHeaders.IndexOfName(FHeaders[I]));
end;

function TRequestIndy.Accept: string;
begin
  Result := FIdHTTP.Request.Accept;
end;

function TRequestIndy.Accept(const AAccept: string): IRequest;
begin
  Result := Self;
  FIdHTTP.Request.Accept := AAccept;
end;

function TRequestIndy.AcceptCharset(const AAcceptCharset: string): IRequest;
begin
  Result := Self;
  FIdHTTP.Request.AcceptCharSet := AAcceptCharset;
end;

function TRequestIndy.AcceptCharset: string;
begin
  Result := FIdHTTP.Request.AcceptCharSet;
end;

function TRequestIndy.AcceptEncoding(const AAcceptEncoding: string): IRequest;
begin
  Result := Self;
  FIdHTTP.Request.AcceptEncoding := AAcceptEncoding;
end;

function TRequestIndy.AcceptEncoding: string;
begin
  Result := FIdHTTP.Request.AcceptEncoding;
end;

function TRequestIndy.BaseURL: string;
begin
  Result := FBaseURL;
end;

function TRequestIndy.BaseURL(const ABaseURL: string): IRequest;
begin
  Result := Self;
  FBaseURL := ABaseURL;
end;

function TRequestIndy.BasicAuthentication(const AUsername, APassword: string): IRequest;
begin
  Result := Self;
  FIdHTTP.Request.BasicAuthentication := True;
  FIdHTTP.Request.Username := AUsername;
  FIdHTTP.Request.Password := APassword;
end;

constructor TRequestIndy.Create;
begin
  FIdHTTP := TIdHTTP.Create(nil);
  FIdHTTP.Request.Connection := 'Keep-Alive';
  FIdHTTP.Request.UserAgent := 'User-Agent:Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.96 Safari/537.36';
  FIdHTTP.HandleRedirects := True;

  FIdSSLIOHandlerSocketOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create;
  FIdHTTP.IOHandler := FIdSSLIOHandlerSocketOpenSSL;
  FIdSSLIOHandlerSocketOpenSSL.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
  FIdSSLIOHandlerSocketOpenSSL.OnStatusInfoEx := Self.OnStatusInfoEx;

  FHeaders := TStringList.Create;
  FResponse := TResponseIndy.Create(FIdHTTP);
  FParams := TStringList.Create;
  FUrlSegments := TStringList.Create;

  FStreamResult := TStringStream.Create;
  Self.ContentType('application/json');
  FRetries := 0;
  FIdMultiPartFormDataStream := TIdMultiPartFormDataStream.Create;
  FIdHTTP.HTTPOptions:= [hoKeepOrigProtocol];
end;

destructor TRequestIndy.Destroy;
begin
  FreeAndNil(FIdSSLIOHandlerSocketOpenSSL);
  FreeAndNil(FIdHTTP);
  if Assigned(FStreamSend) then
    FreeAndNil(FStreamSend);
  FreeAndNil(FHeaders);
  FreeAndNil(FParams);
  FreeAndNil(FUrlSegments);
  FreeAndNil(FStreamResult);
  if Assigned(FIdMultiPartFormDataStream) then
    FreeAndNil(FIdMultiPartFormDataStream);
  inherited;
end;

function TRequestIndy.Resource: string;
begin
  Result := FResource;
end;

function TRequestIndy.Resource(const AResource: string): IRequest;
begin
  Result := Self;
  FResource := AResource;
end;

function TRequestIndy.ResourceSuffix(const AResourceSuffix: string): IRequest;
begin
  Result := Self;
  FResourceSuffix := AResourceSuffix;
end;

function TRequestIndy.ResourceSuffix: string;
begin
  Result := FResourceSuffix;
end;

function TRequestIndy.Retry(const ARetries: Integer): IRequest;
begin
  Result := Self;
  FRetries := ARetries;
end;

function TRequestIndy.Timeout: Integer;
begin
  Result := FIdHTTP.ReadTimeout;
end;

function TRequestIndy.Timeout(const ATimeout: Integer): IRequest;
begin
  Result := Self;
  FIdHTTP.ReadTimeout := ATimeout;
  FIdHTTP.ConnectTimeout := ATimeout;
end;

function TRequestIndy.UserAgent(const AName: string): IRequest;
begin
  Result := Self;
  FIdHTTP.Request.UserAgent := AName;
end;

end.
