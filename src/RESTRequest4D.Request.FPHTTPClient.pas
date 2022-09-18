unit RESTRequest4D.Request.FPHTTPClient;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses 
	Classes, 
	SysUtils,
  DB,
  fpJSON,
  fpjsonrtti,
  FPHTTPClient,
	OpenSSL,
	OpenSSLSockets,

  RESTRequest4D.Utils,
  RESTRequest4D.Request.Contract,
	RESTRequest4D.Response.Contract,
  DataSet.Serialize;

type

  TRequestFPHTTPClient = class(TInterfacedObject, IRequest)
  private
    FHeaders       : TStrings;
    FParams        : TStrings;
    FUrlSegments   : TStrings;
    FFPHTTPClient  : TFPHTTPClient;
    FBaseURL       : String;
    FResource      : String;
    FResourceSuffix: String;
    FResponse      : IResponse;
    FStreamSend    : TStream;
    FRetries       : Integer;
    FDataSetAdapter: TDataSet;

    function AcceptEncoding : String;   overload;
    function AcceptCharset  : String;   overload;
    function Accept         : String;   overload;
    function Timeout        : Integer;  overload;
    function DataSetAdapter : TDataSet; overload;
    function BaseURL        : String;   overload;
    function Resource       : String;   overload;
    function ResourceSuffix : String;   overload;

    function ClearBody      : IRequest;
    function ClearHeaders   : IRequest;
    function ClearParams    : IRequest;
    function DeactivateProxy: IRequest;
    function Get            : IResponse;
    function Post           : IResponse;
    function Put            : IResponse;
    function Delete         : IResponse;
    function Patch          : IResponse;

    function RaiseExceptionOn500: Boolean; overload;

    function AcceptEncoding     (const AAcceptEncoding: string  ): IRequest; overload;
    function AcceptCharset      (const AAcceptCharset : string  ): IRequest; overload;
    function Accept             (const AAccept        : string  ): IRequest; overload;
    function Timeout            (const ATimeout       : Integer ): IRequest; overload;
    function DataSetAdapter     (const ADataSet       : TDataSet): IRequest; overload;
    function BaseURL            (const ABaseURL       : string  ): IRequest; overload;
    function Resource           (const AResource      : string  ): IRequest; overload;
    function ResourceSuffix     (const AResourceSuffix: string  ): IRequest; overload;
    function RaiseExceptionOn500(const ARaiseException: Boolean ): IRequest; overload;
    function TokenBearer        (const AToken         : string  ): IRequest;
    function Token              (const AToken         : string  ): IRequest;
    function Retry              (const ARetries       : Integer ): IRequest;
    function ContentType        (const AContentType   : string  ): IRequest;
    function UserAgent          (const AName          : string  ): IRequest;
    function AddCookies         (const ACookies       : TStrings): IRequest;
    function FullRequestURL     (const AIncludeParams : Boolean = True): string;
    function MakeURL            (const AIncludeParams : Boolean = True): string;

    function AddBody            (const AContent: string): IRequest; overload;
    function AddBody            (const AContent: TStream;     const AOwns: Boolean = False): IRequest; overload;
    function AddBody            (const AContent: TJSONObject; const AOwns: Boolean = True ): IRequest; overload;
    function AddBody            (const AContent: TJSONArray;  const AOwns: Boolean = True ): IRequest; overload;
    function AddBody            (const AContent: TObject;     const AOwns: Boolean = True ): IRequest; overload;

    function AddHeader          (const AName        , AValue      : string ): IRequest;
    function AddParam           (const AName        , AValue      : string ): IRequest;
    function AddUrlSegment      (const AName        , AValue      : string ): IRequest;
    function AddCookie          (const ACookieName  , ACookieValue: string ): IRequest;
    function AddFile            (const AName: string; const AValue: TStream): IRequest;

    function BasicAuthentication(const AUsername, APassword: string): IRequest;
    function Proxy              (const AServer  , APassword, AUsername: string; const APort: Integer): IRequest;

  protected
    procedure DoAfterExecute;
    procedure ExecuteRequest(const AMethod: TMethodRequest);

  public
    constructor Create;
    destructor Destroy; override;

  end;

implementation

uses
  RESTRequest4D.Response.FPHTTPClient;

constructor TRequestFPHTTPClient.Create;
begin
  FFPHTTPClient                := TFPHTTPClient.Create(nil);
  FFPHTTPClient.KeepConnection := false;
  FFPHTTPClient.AllowRedirect  := false;
  FFPHTTPClient.HTTPversion    := '1.1';
  FHeaders                     := TStringList.Create;
  FResponse                    := TResponseFpHTTPClient.Create(FFPHTTPClient);
  FParams                      := TStringList.Create;
  FUrlSegments                 := TStringList.Create;
  FRetries                     := 0;

  Timeout(3000);
  FFPHTTPClient.AddHeader('User-Agent','Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.96 Safari/537.36');
  ContentType('application/json');
end;

destructor TRequestFPHTTPClient.Destroy;
begin
  if Assigned(FStreamSend) then
  begin
    FreeAndNil(FStreamSend);
  end;

  FreeAndNil(FHeaders);
  FreeAndNil(FParams);
  FreeAndNil(FUrlSegments);
  FreeAndNil(FFPHTTPClient);
  inherited Destroy;
end;

function TRequestFPHTTPClient.AcceptEncoding: String;
begin
  Result := FFPHTTPClient.GetHeader('Accept-Encoding');
end;

function TRequestFPHTTPClient.AcceptCharset: String;
begin
  Result := FFPHTTPClient.GetHeader('Accept-Charset');
end;

function TRequestFPHTTPClient.Accept: String;
begin
  Result := FFPHTTPClient.GetHeader('Accept');
end;

function TRequestFPHTTPClient.Timeout: Integer;
begin
  Result := FFPHTTPClient.ConnectTimeout;
end;

function TRequestFPHTTPClient.DataSetAdapter: TDataSet;
begin
  Result := FDataSetAdapter;
end;

function TRequestFPHTTPClient.BaseURL: String;
begin
  Result := FBaseURL;
end;

function TRequestFPHTTPClient.Resource: String;
begin
  Result := FResource;
end;

function TRequestFPHTTPClient.ResourceSuffix: String;
begin
  Result := FResourceSuffix;
end;

function TRequestFPHTTPClient.ClearBody: IRequest;
begin
  Result := Self;

  if Assigned(FStreamSend) then
  begin
    FreeAndNil(FStreamSend);
  end;
end;

function TRequestFPHTTPClient.ClearHeaders: IRequest;
begin
  Result := Self;
  FFPHTTPClient.RequestHeaders.Clear;
end;

function TRequestFPHTTPClient.ClearParams: IRequest;
begin
  Result := Self;
  FParams.Clear;
end;

function TRequestFPHTTPClient.DeactivateProxy: IRequest;
begin
  Result := Self;
  FFPHTTPClient.Proxy.Host     := EmptyStr;
  FFPHTTPClient.Proxy.Password := EmptyStr;
  FFPHTTPClient.Proxy.UserName := EmptyStr;
  FFPHTTPClient.Proxy.Port     := 0;
end;

function TRequestFPHTTPClient.Get: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrGET);
end;

function TRequestFPHTTPClient.Post: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrPOST);
end;

function TRequestFPHTTPClient.Put: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrPUT);
end;

function TRequestFPHTTPClient.Delete: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrDELETE);
end;

function TRequestFPHTTPClient.Patch: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrPATCH);
end;

function TRequestFPHTTPClient.RaiseExceptionOn500: Boolean;
begin
  Result := False;
end;

procedure TRequestFPHTTPClient.DoAfterExecute;
begin
  if not Assigned(FDataSetAdapter) then
  begin
    Exit;
  end;

  FDataSetAdapter.LoadFromJSON(FResponse.Content);
end;

procedure TRequestFPHTTPClient.ExecuteRequest(const AMethod: TMethodRequest);
var
  LAttempts: Integer;
begin
    LAttempts := FRetries + 1;

    while LAttempts > 0 do
    begin
        try
            if AMethod <> mrGET then
            begin
                FFPHTTPClient.RequestBody := FStreamSend;
            end;

            case AMethod of
                mrGET   : FFPHTTPClient.Get   (MakeURL, FResponse.ContentStream);
                mrPOST  : FFPHTTPClient.Post  (MakeURL, FResponse.ContentStream);
                mrPUT   : FFPHTTPClient.Put   (MakeURL, FResponse.ContentStream);
                mrPATCH : FFPHTTPClient.Put   (MakeURL, FResponse.ContentStream);
                mrDELETE: FFPHTTPClient.Delete(MakeURL, FResponse.ContentStream);
            end;

            LAttempts := 0;
            DoAfterExecute;
        except
            LAttempts := LAttempts - 1;
            if LAttempts = 0 then
            begin
              raise;
            end;
        end;
    end;
end;

function TRequestFPHTTPClient.AcceptEncoding(const AAcceptEncoding: string): IRequest;
begin
 Result := Self;
 FFPHTTPClient.AddHeader('Accept-Encoding', AAcceptEncoding);
end;

function TRequestFPHTTPClient.AcceptCharset(const AAcceptCharset: string): IRequest;
begin
 Result := Self;
 FFPHTTPClient.AddHeader('Accept-Charset', AAcceptCharset);
end;

function TRequestFPHTTPClient.Accept(const AAccept: string): IRequest;
begin
 Result := Self;
 FFPHTTPClient.AddHeader('Accept', AAccept);
end;

function TRequestFPHTTPClient.Timeout(const ATimeout: Integer): IRequest;
begin
  Result := Self;
  FFPHTTPClient.ConnectTimeout := ATimeout;
end;

function TRequestFPHTTPClient.DataSetAdapter(const ADataSet: TDataSet): IRequest;
begin
  Result := Self;
  FDataSetAdapter := ADataSet;
end;

function TRequestFPHTTPClient.BaseURL(const ABaseURL: string): IRequest;
begin
  Result   := Self;
  FBaseURL := ABaseURL;
end;

function TRequestFPHTTPClient.Resource(const AResource: string): IRequest;
begin
  Result    := Self;
  FResource := AResource;
end;

function TRequestFPHTTPClient.ResourceSuffix(const AResourceSuffix: string): IRequest;
begin
  Result          := Self;
  FResourceSuffix := AResourceSuffix;
end;

function TRequestFPHTTPClient.RaiseExceptionOn500(const ARaiseException: Boolean): IRequest;
begin
  Result := Self;
  Raise Exception.Create('RaiseExceptionOn500 not implemented!');
end;

function TRequestFPHTTPClient.TokenBearer(const AToken: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Authorization', 'Bearer ' + AToken);
end;

function TRequestFPHTTPClient.Token(const AToken: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Authorization', AToken);
end;

function TRequestFPHTTPClient.Retry(const ARetries: Integer): IRequest;
begin
  Result   := Self;
  FRetries := ARetries;
end;

function TRequestFPHTTPClient.ContentType(const AContentType: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Content-Type', AContentType);
end;

function TRequestFPHTTPClient.UserAgent(const AName: string): IRequest;
begin
  Result := Self;
  FFPHTTPClient.AddHeader('User-Agent', AName);
end;

function TRequestFPHTTPClient.AddCookies(const ACookies: TStrings): IRequest;
var
  I: Integer;
begin
  Result := Self;

  for I := 0 to ACookies.Count - 1 do
  begin
    FFPHTTPClient.Cookies.Add(ACookies.Text[I]);
  end;
end;

function TRequestFPHTTPClient.AddCookie(const ACookieName, ACookieValue: string): IRequest;
begin
  Result := Self;
  Raise Exception.Create('AddCookie not implemented');
end;

function TRequestFPHTTPClient.FullRequestURL(const AIncludeParams: Boolean): string;
begin
  Result := Self.MakeURL(AIncludeParams);
end;

function TRequestFPHTTPClient.MakeURL(const AIncludeParams: Boolean): string;
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

function TRequestFPHTTPClient.AddBody(const AContent: string): IRequest;
begin
  Result := Self;

  if not Assigned(FStreamSend) then
    FStreamSend := TStringStream.Create(AContent, TEncoding.UTF8)
  else
    TStringStream(FStreamSend).WriteString(AContent);

  FStreamSend.Position := 0;
end;

function TRequestFPHTTPClient.AddBody(const AContent: TStream; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  try
    if not Assigned(FStreamSend) then
    begin
      FStreamSend := TStringStream.Create;
    end;

    TStringStream(FStreamSend).CopyFrom(AContent, AContent.Size);
    FStreamSend.Position := 0;
  finally
    if AOwns then
    begin
      AContent.Free;
    end;
  end;
end;

function TRequestFPHTTPClient.AddBody(const AContent: TObject; const AOwns: Boolean): IRequest;
var
  LJSONStreamer: TJSONStreamer;
  LJSONObject  : TJSONObject;
begin
  LJSONStreamer := TJSONStreamer.Create(NIL);
  LJSONObject   := LJSONStreamer.ObjectToJSON(AContent);
  try
    Result := Self.AddBody(LJSONObject, False);
  finally
    LJSONStreamer.Free;

    if AOwns then
    begin
      AContent.Free;
    end;
  end;
end;

function TRequestFPHTTPClient.AddBody(const AContent: TJSONObject; const AOwns: Boolean): IRequest;
begin
  Result := Self.AddBody(AContent.AsJSON);

  if AOwns then
  begin
    AContent.Free;
  end;
end;

function TRequestFPHTTPClient.AddBody(const AContent: TJSONArray; const AOwns: Boolean): IRequest;
begin
  Result := Self.AddBody(AContent.AsJSON);
  if AOwns then
  begin
    AContent.Free;
  end;
end;

function TRequestFPHTTPClient.AddHeader(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  if FHeaders.IndexOf(AName) < 0 then
    FHeaders.Add(AName);

  FFPHTTPClient.AddHeader(AName, AValue);
end;

function TRequestFPHTTPClient.AddParam(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if (not AName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
    FParams.Add(AName + '=' + AValue);
end;

function TRequestFPHTTPClient.AddUrlSegment(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  if FUrlSegments.IndexOf(AName) < 0 then
    FUrlSegments.Add(Format('%s=%s', [AName, AValue]));
end;

function TRequestFPHTTPClient.AddFile(const AName: string; const AValue: TStream): IRequest;
begin
  Result := Self;
  if (AValue <> Nil) and (AValue.Size > 0) then
  begin
    Self.AddHeader('x-filename', AName);
    AValue.Position := 0;
    TStringStream(FResponse.ContentStream).LoadFromStream(AValue);
  end;
end;

function TRequestFPHTTPClient.BasicAuthentication(const AUsername, APassword: string): IRequest;
begin
  Result := Self;
  FFPHTTPClient.UserName := AUsername;
  FFPHTTPClient.Password := APassword;
end;

function TRequestFPHTTPClient.Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
begin
  Result := Self;

  FFPHTTPClient.Proxy.Host     := AServer;
  FFPHTTPClient.Proxy.Password := APassword;
  FFPHTTPClient.Proxy.UserName := AUsername;
  FFPHTTPClient.Proxy.Port     := APort;
end;

end.


