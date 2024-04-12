unit RESTRequest4D.Response.NetHTTP;

interface

uses RESTRequest4D.Response.Contract, System.Net.HttpClientComponent, System.Net.HttpClient, System.Net.URLClient,
  System.SysUtils, System.JSON, System.Classes;

type
  TResponseNetHTTP = class(TInterfacedObject, IResponse)
  private
    FJSONValue: TJSONValue;
    FHTTPResponse: IHTTPResponse;
    FContent: TStringStream;
    FHeaders: TStrings;
    function Content: string;
    function ContentLength: Cardinal;
    function ContentType: string;
    function ContentEncoding: string;
    function ContentStream: TStream;
    function StatusCode: Integer;
    function StatusText: string;
    function RawBytes: TBytes;
    function JSONValue: TJSONValue; overload;
    function JSONValue(const AEncoding: TEncoding): TJSONValue; overload;
    function Headers: TStrings;
    function GetCookie(const ACookieName: string): string;
  public
    constructor Create;
    class function New: IResponse;
    procedure SetContent(const AContent: TStringStream);
    procedure SetHTTPResponse(const AHTTPResponse: IHTTPResponse);
    destructor Destroy; override;
  end;

implementation

function TResponseNetHTTP.JSONValue: TJSONValue;
begin
  Result := Self.JSONValue(TEncoding.UTF8);
end;

class function TResponseNetHTTP.New: IResponse;
begin
  Result := TResponseNetHTTP.Create;
end;

function TResponseNetHTTP.Content: string;
begin
  if Assigned(FHTTPResponse) then
    Result := TStringStream(FHTTPResponse.ContentStream).DataString;
end;

function TResponseNetHTTP.ContentEncoding: string;
begin
  if Assigned(FHTTPResponse) then
    Result := FHTTPResponse.ContentEncoding;
end;

function TResponseNetHTTP.ContentLength: Cardinal;
begin
  Result := 0;
  if Assigned(FHTTPResponse) then
    Result := FHTTPResponse.ContentLength
end;

function TResponseNetHTTP.ContentStream: TStream;
begin
  Result := nil;
  if Assigned(FHTTPResponse) then
  begin
    Result := FHTTPResponse.ContentStream;
    Result.Position := 0;
  end;
end;

function TResponseNetHTTP.ContentType: string;
begin
  if Assigned(FHTTPResponse) and FHTTPResponse.ContainsHeader('Content-Type') then
    Result := FHTTPResponse.HeaderValue['Content-Type'];
end;

constructor TResponseNetHTTP.Create;
begin
  FHeaders := TStringList.Create;
end;

destructor TResponseNetHTTP.Destroy;
begin
  FHTTPResponse := nil;
  if Assigned(FHeaders) then
    FHeaders.Free;
  if Assigned(FJSONValue) then
    FJSONValue.Free;
  inherited;
end;

function TResponseNetHTTP.GetCookie(const ACookieName: string): string;
var
  I: Integer;
begin
  for I := 0 to Pred(FHTTPResponse.Cookies.Count) do
  begin
    if Trim(LowerCase(ACookieName)) = Trim(LowerCase(FHTTPResponse.Cookies.Items[I].Name)) then
      Result := FHTTPResponse.Cookies.Items[I].Value;
  end;
end;

function TResponseNetHTTP.Headers: TStrings;
var
  LHeader: TNameValuePair;
begin
  FHeaders.Clear;
  if Assigned(FHTTPResponse) then
    for LHeader in FHTTPResponse.Headers do
      FHeaders.Add(LHeader.Name + '=' + LHeader.Value);
  Result := FHeaders;
end;

function TResponseNetHTTP.JSONValue(const AEncoding: TEncoding): TJSONValue;
var
  LContent: string;
begin
  if not(Assigned(FJSONValue)) then
  begin
    if Assigned(FHTTPResponse) then
      LContent := FHTTPResponse.ContentAsString.Trim;
    if LContent.StartsWith('{') then
      FJSONValue := (TJSONObject.ParseJSONValue(AEncoding.GetBytes(LContent), 0) as TJSONObject)
    else if LContent.StartsWith('[') then
      FJSONValue := (TJSONObject.ParseJSONValue(AEncoding.GetBytes(LContent), 0) as TJSONArray)
    else
      raise Exception.Create('The return content is not a valid JSON value.');
  end;
  Result := FJSONValue;
end;

function TResponseNetHTTP.RawBytes: TBytes;
begin
  if Assigned(FContent) then
    Result := FContent.Bytes;
end;

procedure TResponseNetHTTP.SetContent(const AContent: TStringStream);
begin
  FContent := AContent;
end;

procedure TResponseNetHTTP.SetHTTPResponse(const AHTTPResponse: IHTTPResponse);
begin
  FHTTPResponse := AHTTPResponse;
end;

function TResponseNetHTTP.StatusCode: Integer;
begin
  Result := 0;
  if Assigned(FHTTPResponse) then
    Result := FHTTPResponse.StatusCode;
end;

function TResponseNetHTTP.StatusText: string;
begin
  if Assigned(FHTTPResponse) then
    Result := FHTTPResponse.StatusText;
end;

end.
