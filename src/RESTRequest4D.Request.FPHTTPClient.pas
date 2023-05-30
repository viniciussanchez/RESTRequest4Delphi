unit RESTRequest4D.Request.FPHTTPClient;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses Classes, SysUtils, DB, RESTRequest4D.Request.Contract, RESTRequest4D.Response.Contract,
  RESTRequest4D.Utils, FPHTTPClient, openssl, opensslsockets, fpjson, fpjsonrtti,
  RESTRequest4D.Request.Adapter.Contract, Generics.Collections;

type
  TFile = class
  private
    FFileStream: TStream;
    FFileName: string;
    FContentType: string;
  public
    constructor Create(const AFileStream: TStream; const AFileName: string; const AContentType: string); overload;
    destructor Destroy; override;
  end;

  TRequestFPHTTPClient = class(TInterfacedObject, IRequest)
  private
    FHeaders: Tstrings;
    FParams: TstringList;
    FFiles: TDictionary<string, TFile>;
    FFields: TDictionary<string, string>;
    FUrlSegments: Tstrings;
    FFPHTTPClient: TFPHTTPClient;
    FBaseURL: string;
    FResource: string;
    FResourceSuffix: string;
    FAdapters: TArray<IRequestAdapter>;
    FResponse: IResponse;
    FStreamSend: TStream;
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
    function AddCookies(const ACookies: Tstrings): IRequest;
    function AddCookie(const ACookieName, ACookieValue: string): IRequest;
    function AddParam(const AName, AValue: string): IRequest;
    function AddField(const AFieldName: string; const AValue: string): IRequest; overload;
    function AddFile(const AFieldName: string; const AFileName: string; const AContentType: string = ''): IRequest; overload;
    function AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string = ''; const AContentType: string = ''): IRequest; overload;
    function MakeURL(const AIncludeParams: Boolean = True): string;
    function Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
    function DeactivateProxy: IRequest;
  protected
    procedure DoAfterExecute(const Sender: TObject; const AResponse: IResponse); virtual;
    procedure DoBeforeExecute(const Sender: TFPHTTPClient); virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses RESTRequest4D.Response.FPHTTPClient;

constructor TFile.Create(const AFileStream: TStream; const AFileName: string; const AContentType: string);
begin
  FFileStream := AFileStream;
  FFileName := AFileName;
  FContentType := AContentType;
end;

destructor TFile.Destroy;
begin
  inherited Destroy;
end;

procedure TRequestFPHTTPClient.ExecuteRequest(const AMethod: TMethodRequest);
var
  LAttempts: Integer;
  LBound, LContent, LFieldName: string;
  LFile: TFile;
  LStream: TStream;
begin
  LAttempts := FRetries + 1;

  while LAttempts > 0 do
  begin
    try
      DoBeforeExecute(FFPHTTPClient);
      LStream := TstringStream.Create('', TEncoding.UTF8);
      try
        if AMethod <> mrGET then
        begin
          if (FFields.Count > 0) or (FFiles.Count > 0) then
          begin
            LBound := IntToHex(Random(MaxInt), 8) + '_rr4d_boundary';
            ContentType('multipart/form-data; boundary=' + LBound);

            for LFieldName in FFields.Keys do
            begin
              LContent := sLineBreak + '--' + LBound + sLineBreak +
                          'Content-Disposition: form-data; name=' + AnsiQuotedStr(LFieldName, '"') + sLineBreak + sLineBreak +
                          FFields.Items[LFieldName]+ sLineBreak + sLineBreak;
              LStream.Write(PAnsiChar(LContent)^, Length(LContent));
            end;

            for LFieldName in FFiles.Keys do
            begin
              LFile := FFiles.Items[LFieldName];

              LContent := sLineBreak + '--' + LBound + sLineBreak +
                          'Content-Disposition: form-data; name=' + AnsiQuotedStr(LFieldName, '"') +';' +
                          sLineBreak + #9'filename=' + AnsiQuotedStr(LFile.FFileName, '"') +
                          sLineBreak + 'Content-Type: '+AnsiQuotedStr(LFile.FContentType, '"') + sLineBreak + sLineBreak;
              LStream.Write(PAnsiChar(LContent)^, Length(LContent));
              LFile.FFileStream.Position := 0;
              LStream.Write(LFile.FFileStream, LFile.FFileStream.Size);
            end;

            LBound := '--' +LBound+ '--' +sLineBreak;
            LStream.Write(PAnsiChar(LBound)^, Length(LBound));
            LStream.Position := 0;
            FFPHTTPClient.RequestBody := LStream;
          end
          else
            FFPHTTPClient.RequestBody := FStreamSend;
        end;

        case AMethod of
          mrGET:
            FFPHTTPClient.Get(MakeURL, FResponse.ContentStream);
          mrPOST:
            FFPHTTPClient.Post(MakeURL, FResponse.ContentStream);
          mrPUT:
            FFPHTTPClient.Put(MakeURL, FResponse.ContentStream);
          mrPATCH:
            FFPHTTPClient.Put(MakeURL, FResponse.ContentStream);
          mrDELETE:
            FFPHTTPClient.Delete(MakeURL, FResponse.ContentStream);
        end;

        LAttempts := 0;
      finally
        if Assigned(LStream) then
          LStream.Free;
      end;

      DoAfterExecute(Self, FResponse);
    except
      LAttempts := LAttempts - 1;
      if LAttempts = 0 then
        raise;
    end;
  end;
end;

function TRequestFPHTTPClient.AcceptEncoding: string;
begin
  Result := FFPHTTPClient.GetHeader('Accept-Encoding');
end;

function TRequestFPHTTPClient.AcceptEncoding(const AAcceptEncoding: string): IRequest;
begin
  Result := Self;
  FFPHTTPClient.AddHeader('Accept-Encoding', AAcceptEncoding);
end;

function TRequestFPHTTPClient.AcceptCharset: string;
begin
  Result := FFPHTTPClient.GetHeader('Accept-Charset');
end;

function TRequestFPHTTPClient.AcceptCharset(const AAcceptCharset: string): IRequest;
begin
  Result := Self;
  FFPHTTPClient.AddHeader('Accept-Charset', AAcceptCharset);
end;

function TRequestFPHTTPClient.Accept: string;
begin
  Result := FFPHTTPClient.GetHeader('Accept');
end;

function TRequestFPHTTPClient.Accept(const AAccept: string): IRequest;
begin
  Result := Self;
  FFPHTTPClient.AddHeader('Accept', AAccept);
end;

function TRequestFPHTTPClient.Timeout: Integer;
begin
  Result := FFPHTTPClient.ConnectTimeout;
end;

function TRequestFPHTTPClient.Timeout(const ATimeout: Integer): IRequest;
begin
  Result := Self;
  FFPHTTPClient.ConnectTimeout := ATimeout;
end;

function TRequestFPHTTPClient.BaseURL(const ABaseURL: string): IRequest;
begin
  Result := Self;
  FBaseURL := ABaseURL;
end;

function TRequestFPHTTPClient.BaseURL: string;
begin
  Result := FBaseURL;
end;

function TRequestFPHTTPClient.Resource(const AResource: string): IRequest;
begin
  Result := Self;
  FResource := AResource;
end;

function TRequestFPHTTPClient.RaiseExceptionOn500: Boolean;
begin
  Result := False;
end;

function TRequestFPHTTPClient.RaiseExceptionOn500(const ARaiseException: Boolean): IRequest;
begin
  raise Exception.Create('Not implemented');
end;

function TRequestFPHTTPClient.Resource: string;
begin
  Result := FResource;
end;

function TRequestFPHTTPClient.ResourceSuffix(const AResourceSuffix: string): IRequest;
begin
  Result := Self;
  FResourceSuffix := AResourceSuffix;
end;

function TRequestFPHTTPClient.ResourceSuffix: string;
begin
  Result := FResourceSuffix;
end;

function TRequestFPHTTPClient.Token(const AToken: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Authorization', AToken);
end;

function TRequestFPHTTPClient.TokenBearer(const AToken: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Authorization', 'Bearer ' + AToken);
end;

function TRequestFPHTTPClient.BasicAuthentication(const AUsername, APassword: string): IRequest;
begin
  Result := Self;
  FFPHTTPClient.UserName := AUsername;
  FFPHTTPClient.Password := APassword;
end;

function TRequestFPHTTPClient.Retry(const ARetries: Integer): IRequest;
begin
  Result := Self;
  FRetries := ARetries;
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

function TRequestFPHTTPClient.FullRequestURL(const AIncludeParams: Boolean): string;
begin
  Result := Self.MakeURL(AIncludeParams);
end;

function TRequestFPHTTPClient.ClearBody: IRequest;
begin
  Result := Self;
  if Assigned(FStreamSend) then
    FreeAndNil(FStreamSend);
end;

function TRequestFPHTTPClient.AddBody(const AContent: string): IRequest;
begin
  Result := Self;
  if not Assigned(FStreamSend) then
    FStreamSend := TstringStream.Create(AContent, TEncoding.UTF8)
  else
    TstringStream(FStreamSend).Writestring(AContent);
  FStreamSend.Position := 0;
end;

function TRequestFPHTTPClient.AddBody(const AContent: TJSONObject; const AOwns: Boolean): IRequest;
begin
  Result := Self.AddBody(AContent.AsJSON);
  if AOwns then
    AContent.Free;
end;

function TRequestFPHTTPClient.AddBody(const AContent: TJSONArray; const AOwns: Boolean): IRequest;
begin
  Result := Self.AddBody(AContent.AsJSON);
  if AOwns then
    AContent.Free;
end;

function TRequestFPHTTPClient.AddBody(const AContent: TObject; const AOwns: Boolean): IRequest;
var
  LJSONStreamer: TJSONStreamer;
  LJSONObject: TJSONObject;
begin
  LJSONStreamer := TJSONStreamer.Create(NIL);
  LJSONObject := LJSONStreamer.ObjectToJSON(AContent);
  try
    Result := Self.AddBody(LJSONObject, False);
  finally
    LJSONStreamer.Free;
    if AOwns then
      AContent.Free;
  end;
end;

function TRequestFPHTTPClient.AddBody(const AContent: TStream; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  try
    if not Assigned(FStreamSend) then
      FStreamSend := TstringStream.Create;
    TstringStream(FStreamSend).CopyFrom(AContent, AContent.Size);
    FStreamSend.Position := 0;
  finally
    if AOwns then
      AContent.Free;
  end;
end;

function TRequestFPHTTPClient.AddUrlSegment(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  if FUrlSegments.IndexOf(AName) < 0 then
    FUrlSegments.Add(Format('%s=%s', [AName, AValue]));
end;

function TRequestFPHTTPClient.ClearHeaders: IRequest;
begin
  Result := Self;
  FFPHTTPClient.RequestHeaders.Clear;
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

function TRequestFPHTTPClient.ClearParams: IRequest;
begin
  Result := Self;
  FParams.Clear;
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

function TRequestFPHTTPClient.AddCookies(const ACookies: Tstrings): IRequest;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to ACookies.Count - 1 do
    FFPHTTPClient.Cookies.Add(ACookies.Text[I]);
end;

function TRequestFPHTTPClient.AddCookie(const ACookieName, ACookieValue: string): IRequest;
var
  LCookies: TstringList;
begin
  LCookies := TstringList.Create;
  try
    LCookies.AddPair(ACookieName, ACookieValue);
    Result := AddCookies(LCookies);
  finally
    LCookies.Free;
  end;
end;

function TRequestFPHTTPClient.AddParam(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if (not AName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
    FParams.Add(AName + '=' + AValue);
end;

function TRequestFPHTTPClient.AddField(const AFieldName: string; const AValue: string): IRequest;
begin
  Result := Self;
  if (not AFieldName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
    FFields.AddOrSetValue(AFieldName, AValue);
end;

function TRequestFPHTTPClient.AddFile(const AFieldName: string; const AFileName: string; const AContentType: string): IRequest;
var
  LStream: TMemoryStream;
begin
  Result := Self;
  if not FileExists(AFileName) then
    Exit;
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(AFileName);
    LStream.Position := 0;
    AddFile(AFieldName, LStream, AFileName, AContentType);
  finally
    LStream.Free;
  end;
end;

function TRequestFPHTTPClient.AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string; const AContentType: string): IRequest;
var
  LFile: TFile;
begin
  Result := Self;
  if not Assigned(AValue) then
    Exit;
  if (AValue <> Nil) and (AValue.Size > 0) then
  begin
    LFile := TFile.Create(AValue, AFileName, AContentType);
    FFiles.AddOrSetValue(AFieldName, LFile);
  end;
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
      Result := stringReplace(Result, Format('{%s}', [FUrlSegments.Names[I]]), FUrlSegments.ValueFromIndex[I], [rfReplaceAll, rfIgnoreCase]);
      Result := stringReplace(Result, Format(':%s', [FUrlSegments.Names[I]]), FUrlSegments.ValueFromIndex[I], [rfReplaceAll, rfIgnoreCase]);
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
      Result := Result + FParams.strings[I];
    end;
  end;
end;

function TRequestFPHTTPClient.Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
begin
  Result := Self;
  FFPHTTPClient.Proxy.Host := AServer;
  FFPHTTPClient.Proxy.Password := APassword;
  FFPHTTPClient.Proxy.UserName := AUsername;
  FFPHTTPClient.Proxy.Port := APort;
end;

function TRequestFPHTTPClient.DeactivateProxy: IRequest;
begin
  Result := Self;
  FFPHTTPClient.Proxy.Host := EmptyStr;
  FFPHTTPClient.Proxy.Password := EmptyStr;
  FFPHTTPClient.Proxy.UserName := EmptyStr;
  FFPHTTPClient.Proxy.Port := 0;
end;

function TRequestFPHTTPClient.Adapters(const AAdapter: IRequestAdapter): IRequest;
begin
  Result := Adapters([AAdapter]);
end;

function TRequestFPHTTPClient.Adapters(const AAdapters: TArray<IRequestAdapter>): IRequest;
begin
  FAdapters := AAdapters;
  Result := Self;
end;

function TRequestFPHTTPClient.Adapters: TArray<IRequestAdapter>;
begin
  Result := FAdapters;
end;

procedure TRequestFPHTTPClient.DoAfterExecute(const Sender: TObject; const AResponse: IResponse);
var
  LAdapter: IRequestAdapter;
begin
  for LAdapter in FAdapters do
    LAdapter.Execute(FResponse.Content);
end;

procedure TRequestFPHTTPClient.DoBeforeExecute(const Sender: TFPHTTPClient);
begin
  // virtual method
end;

constructor TRequestFPHTTPClient.Create;
begin
  FFPHTTPClient := TFPHTTPClient.Create(nil);
  FFPHTTPClient.KeepConnection := True;
  FFPHTTPClient.AllowRedirect := True;
  FFPHTTPClient.RequestHeaders.Clear;
  FFPHTTPClient.ResponseHeaders.Clear;

  FHeaders := TstringList.Create;
  FResponse := TResponseFpHTTPClient.Create(FFPHTTPClient);
  FParams := TstringList.Create;
  FFields := TDictionary<string, string>.Create;;
  FUrlSegments := TstringList.Create;
  FFiles := TDictionary<string, TFile>.Create;

  UserAgent('Mozilla/5.0 (compatible; fpweb)');
end;

destructor TRequestFPHTTPClient.Destroy;
begin
  if Assigned(FStreamSend) then
    FreeAndNil(FStreamSend);
  FreeAndNil(FHeaders);
  FreeAndNil(FParams);
  FreeAndNil(FFields);
  FreeAndNil(FFields);
  FreeAndNil(FUrlSegments);
  FreeAndNil(FFiles);
  FreeAndNil(FFPHTTPClient);
  inherited Destroy;
end;

end.
