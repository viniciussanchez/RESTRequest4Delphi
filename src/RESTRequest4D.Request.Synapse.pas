unit RESTRequest4D.Request.Synapse;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses Classes, SysUtils, DB, RESTRequest4D.Request.Contract, RESTRequest4D.Response.Contract, RESTRequest4D.Utils,
  DataSet.Serialize, httpsend, ssl_openssl, Generics.Collections,
  {$IFDEF FPC}
    fpjson, fpjsonrtti, base64;
  {$ELSE}
    System.Json,
    System.NetEncoding,
    REST.Json;
  {$ENDIF}

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

  TRequestSynapse = class(TInterfacedObject, IRequest)
  private
    FHeaders: Tstrings;
    FParams: TstringList;
    FFiles: TDictionary<string, TFile>;
    FFields: TDictionary<string, string>;
    FUrlSegments: Tstrings;
    FHTTPSend: THTTPSend;
    FBaseURL: string;
    FResource: string;
    FResourceSuffix: string;
    FDataSetAdapter: TDataSet;
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
    function MimeType: string; overload;
    function MimeType(const AMimeType: string): IRequest; overload;
    function Timeout: Integer; overload;
    function Timeout(const ATimeout: Integer): IRequest; overload;
    function DataSetAdapter(const ADataSet: TDataSet): IRequest; overload;
    function DataSetAdapter: TDataSet; overload;
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
    procedure DoBeforeExecute(const Sender: THTTPSend); virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses RESTRequest4D.Response.Synapse;

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

procedure TRequestSynapse.ExecuteRequest(const AMethod: TMethodRequest);
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
      DoBeforeExecute(FHTTPSend);
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
            FHTTPSend.Document.LoadFromStream(LStream);
          end
          else
          begin
            FStreamSend.Position := 0;
            FHTTPSend.Document.LoadFromStream(FStreamSend);
          end;
        end;

        case AMethod of
          mrGET:
            FHTTPSend.HTTPMethod('GET', MakeURL);
          mrPOST:
            FHTTPSend.HTTPMethod('POST', MakeURL);
          mrPUT:
            FHTTPSend.HTTPMethod('PUT', MakeURL);
          mrPATCH:
            FHTTPSend.HTTPMethod('PATCH', MakeURL);
          mrDELETE:
            FHTTPSend.HTTPMethod('DELETE', MakeURL);
        end;

        FHTTPSend.Document.Position := 0;
        FResponse.ContentStream.CopyFrom(FHTTPSend.Document, FHTTPSend.Document.Size);

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

function TRequestSynapse.AcceptEncoding: string;
begin
  Result := FHTTPSend.Headers.Values['Accept-Encoding'];
end;

function TRequestSynapse.AcceptEncoding(const AAcceptEncoding: string): IRequest;
begin
  Result := Self;
  FHTTPSend.Headers.AddPair('Accept-Encoding', AAcceptEncoding);
end;

function TRequestSynapse.AcceptCharset: string;
begin
  Result := FHTTPSend.Headers.Values['Accept-Charset'];
end;

function TRequestSynapse.AcceptCharset(const AAcceptCharset: string): IRequest;
begin
  Result := Self;
  FHTTPSend.Headers.AddPair('Accept-Charset', AAcceptCharset);
end;

function TRequestSynapse.Accept: string;
begin
  Result := FHTTPSend.Headers.Values['Accept'];
end;

function TRequestSynapse.Accept(const AAccept: string): IRequest;
begin
  Result := Self;
  FHTTPSend.Headers.AddPair('Accept', AAccept);
end;

function TRequestSynapse.Timeout: Integer;
begin
  Result := FHTTPSend.Timeout;
end;

function TRequestSynapse.Timeout(const ATimeout: Integer): IRequest;
begin
  Result := Self;
  FHTTPSend.Timeout := ATimeout;
end;

function TRequestSynapse.DataSetAdapter(const ADataSet: TDataSet): IRequest;
begin
  Result := Self;
  FDataSetAdapter := ADataSet;
end;

function TRequestSynapse.DataSetAdapter: TDataSet;
begin
  Result := FDataSetAdapter;
end;

function TRequestSynapse.BaseURL(const ABaseURL: string): IRequest;
begin
  Result := Self;
  FBaseURL := ABaseURL;
end;

function TRequestSynapse.BaseURL: string;
begin
  Result := FBaseURL;
end;

function TRequestSynapse.Resource(const AResource: string): IRequest;
begin
  Result := Self;
  FResource := AResource;
end;

function TRequestSynapse.RaiseExceptionOn500: Boolean;
begin
  Result := False;
end;

function TRequestSynapse.RaiseExceptionOn500(const ARaiseException: Boolean): IRequest;
begin
  raise Exception.Create('Not implemented');
end;

function TRequestSynapse.Resource: string;
begin
  Result := FResource;
end;

function TRequestSynapse.ResourceSuffix(const AResourceSuffix: string): IRequest;
begin
  Result := Self;
  FResourceSuffix := AResourceSuffix;
end;

function TRequestSynapse.ResourceSuffix: string;
begin
  Result := FResourceSuffix;
end;

function TRequestSynapse.Token(const AToken: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Authorization', AToken);
end;

function TRequestSynapse.TokenBearer(const AToken: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Authorization', 'Bearer ' + AToken);
end;

function TRequestSynapse.BasicAuthentication(const AUsername, APassword: string): IRequest;
begin
  Result := Self;
{$IFDEF FPC}
  FHTTPSend.Headers.Add(Format('Authorization: Basic %s', [EncodestringBase64(AUsername+':'+APassword)]));
{$ELSE}
  FHTTPSend.Headers.Add(Format('Authorization: Basic %s', [TNetEncoding.Base64.Encode(AUsername+':'+APassword)]));
{$ENDIF}
end;

function TRequestSynapse.Retry(const ARetries: Integer): IRequest;
begin
  Result := Self;
  FRetries := ARetries;
end;

function TRequestSynapse.Get: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrGET);
end;

function TRequestSynapse.Post: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrPOST);
end;

function TRequestSynapse.Put: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrPUT);
end;

function TRequestSynapse.Delete: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrDELETE);
end;

function TRequestSynapse.Patch: IResponse;
begin
  Result := FResponse;
  ExecuteRequest(mrPATCH);
end;

function TRequestSynapse.FullRequestURL(const AIncludeParams: Boolean): string;
begin
  Result := Self.MakeURL(AIncludeParams);
end;

function TRequestSynapse.ClearBody: IRequest;
begin
  Result := Self;
  FStreamSend.Position := 0;
  FStreamSend.Size := 0;
end;

function TRequestSynapse.AddBody(const AContent: string): IRequest;
begin
  Result := Self;
  TStringStream(FStreamSend).Writestring(AContent);
  FStreamSend.Position := 0;
end;

function TRequestSynapse.AddBody(const AContent: TJSONObject; const AOwns: Boolean): IRequest;
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

function TRequestSynapse.AddBody(const AContent: TJSONArray; const AOwns: Boolean): IRequest;
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

function TRequestSynapse.AddBody(const AContent: TObject; const AOwns: Boolean): IRequest;
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

function TRequestSynapse.AddBody(const AContent: TStream; const AOwns: Boolean): IRequest;
begin
  Result := Self;
  try
    TstringStream(FStreamSend).CopyFrom(AContent, AContent.Size);
    FStreamSend.Position := 0;
  finally
    if AOwns then
      AContent.Free;
  end;
end;

function TRequestSynapse.AddUrlSegment(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  if FUrlSegments.IndexOf(AName) < 0 then
    FUrlSegments.Add(Format('%s=%s', [AName, AValue]));
end;

function TRequestSynapse.ClearHeaders: IRequest;
begin
  Result := Self;
  FHTTPSend.Headers.Clear;
end;

function TRequestSynapse.AddHeader(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if AName.Trim.IsEmpty or AValue.Trim.IsEmpty then
    Exit;
  if FHeaders.IndexOf(AName) < 0 then
    FHeaders.Add(AName);
  FHTTPSend.Headers.Add(AName+': '+ AValue);
end;

function TRequestSynapse.ClearParams: IRequest;
begin
  Result := Self;
  FParams.Clear;
end;

function TRequestSynapse.ContentType(const AContentType: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('Content-Type', AContentType);
end;

function TRequestSynapse.UserAgent(const AName: string): IRequest;
begin
  Result := Self;
  Self.AddHeader('User-Agent', AName);
end;

function TRequestSynapse.AddCookies(const ACookies: Tstrings): IRequest;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to ACookies.Count - 1 do
    FHTTPSend.Cookies.Add(ACookies.Text[I]);
end;

function TRequestSynapse.AddCookie(const ACookieName, ACookieValue: string): IRequest;
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

function TRequestSynapse.AddParam(const AName, AValue: string): IRequest;
begin
  Result := Self;
  if (not AName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
    FParams.Add(AName + '=' + AValue);
end;

function TRequestSynapse.AddField(const AFieldName: string; const AValue: string): IRequest;
begin
  Result := Self;
  if (not AFieldName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
    FFields.AddOrSetValue(AFieldName, AValue);
end;

function TRequestSynapse.AddFile(const AFieldName: string; const AFileName: string; const AContentType: string): IRequest;
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

function TRequestSynapse.AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string; const AContentType: string): IRequest;
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

function TRequestSynapse.MakeURL(const AIncludeParams: Boolean): string;
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

function TRequestSynapse.MimeType(const AMimeType: string): IRequest;
begin
  Result := Self;
  FHTTPSend.MimeType := AMimeType;
end;

function TRequestSynapse.MimeType: string;
begin
  Result := FHTTPSend.MimeType;
end;

function TRequestSynapse.Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
begin
  Result := Self;
  FHTTPSend.ProxyHost := AServer;
  FHTTPSend.ProxyPass := APassword;
  FHTTPSend.ProxyUser := AUsername;
  FHTTPSend.ProxyPort := IntToStr(APort);
end;

function TRequestSynapse.DeactivateProxy: IRequest;
begin
  Result := Self;
  FHTTPSend.ProxyHost := EmptyStr;
  FHTTPSend.ProxyPass := EmptyStr;
  FHTTPSend.ProxyUser := EmptyStr;
  FHTTPSend.ProxyPort := EmptyStr;
end;

procedure TRequestSynapse.DoAfterExecute(const Sender: TObject; const AResponse: IResponse);
begin
  if not Assigned(FDataSetAdapter) then
    Exit;
  FDataSetAdapter.LoadFromJSON(FResponse.Content);
end;

procedure TRequestSynapse.DoBeforeExecute(const Sender: THTTPSend);
begin
  // virtual method
end;

constructor TRequestSynapse.Create;
begin
  FHTTPSend := THTTPSend.Create;
  FHTTPSend.Headers.NameValueSeparator := ':';

  FHeaders := TstringList.Create;
  FResponse := TResponseSynapse.Create(FHTTPSend);
  FParams := TstringList.Create;
  FFields := TDictionary<string, string>.Create;;
  FUrlSegments := TstringList.Create;
  FFiles := TDictionary<string, TFile>.Create;
  UserAgent('Mozilla/5.0 (compatible; fpweb)');

  FStreamSend := TStringStream.Create('', TEncoding.UTF8)
end;

destructor TRequestSynapse.Destroy;
begin
  FreeAndNil(FStreamSend);
  FreeAndNil(FHeaders);
  FreeAndNil(FParams);
  FreeAndNil(FFields);
  FreeAndNil(FFields);
  FreeAndNil(FUrlSegments);
  FreeAndNil(FFiles);
  FreeAndNil(FHTTPSend);
  inherited Destroy;
end;

end.
