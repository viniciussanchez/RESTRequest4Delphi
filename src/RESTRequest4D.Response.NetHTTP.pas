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
    function Content: string;
    function ContentLength: Cardinal;
    function ContentType: string;
    function ContentEncoding: string;
    function ContentStream: TStream;
    function StatusCode: Integer;
    function StatusText: string;
    function RawBytes: TBytes;
    function JSONValue: TJSONValue;
    function Headers: TStrings;
  public
    procedure SetContent(const AContent: TStringStream);
    procedure SetHTTPResponse(const AHTTPResponse: IHTTPResponse);
    destructor Destroy; override;
  end;

implementation

function TResponseNetHTTP.JSONValue: TJSONValue;
var
  LContent: string;
begin
  if not(Assigned(FJSONValue)) then
  begin
    if Assigned(FHTTPResponse) then
      LContent := FHTTPResponse.ContentAsString.Trim;
    if LContent.StartsWith('{') then
      FJSONValue := (TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(LContent), 0) as TJSONObject)
    else if LContent.StartsWith('[') then
      FJSONValue := (TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(LContent), 0) as TJSONArray)
    else
      raise Exception.Create('The return content is not a valid JSON value.');
  end;
  Result := FJSONValue;
end;

function TResponseNetHTTP.Content: string;
begin
  if Assigned(FHTTPResponse) then
    Result := FHTTPResponse.ContentAsString;
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

destructor TResponseNetHTTP.Destroy;
begin
  FHTTPResponse := nil;
  if Assigned(FJSONValue) then
    FJSONValue.Free;
  inherited;
end;

function TResponseNetHTTP.Headers: TStrings;
var
  LHeader: TNameValuePair;
begin
  Result := TStringList.Create;
  if Assigned(FHTTPResponse) then
    for LHeader in FHTTPResponse.Headers do
      Result.Add(LHeader.Name + '=' + LHeader.Value);
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
