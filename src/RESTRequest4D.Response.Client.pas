unit RESTRequest4D.Response.Client;

interface

uses RESTRequest4D.Response.Contract, REST.Client, System.SysUtils, System.JSON, System.Classes;

type
  TResponseClient = class(TInterfacedObject, IResponse)
  private
    FJSONValue: TJSONValue;
    FRESTResponse: TRESTResponse;
    FStreamValue: TMemoryStream;
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
    constructor Create(const ARESTResponse: TRESTResponse);
    destructor Destroy; override;
  end;

implementation

constructor TResponseClient.Create(const ARESTResponse: TRESTResponse);
begin
  FRESTResponse := ARESTResponse;
end;

destructor TResponseClient.Destroy;
begin
  if Assigned(FJSONValue) then
    FJSONValue.Free;
  if Assigned(FStreamValue) then
    FreeAndNil(FStreamValue);
  inherited;
end;

function TResponseClient.GetCookie(const ACookieName: string): string;
{$IF CompilerVersion >= 35.0}
var
  I: Integer;
{$IFEND}
begin
  {$IF CompilerVersion >= 35.0}
  for I := 0 to Pred(FRESTResponse.Cookies.Count) do
  begin
    if Trim(LowerCase(ACookieName)) = Trim(LowerCase(FRESTResponse.Cookies.Items[I].Name)) then
      Result := FRESTResponse.Cookies.Items[I].Value;
  end;
  {$ELSE}
  raise Exception.Create('GetCookie is not implemented for this version of Delphi.');
  {$IFEND}
end;

function TResponseClient.Content: string;
begin
  Result := FRESTResponse.Content;
end;

function TResponseClient.Headers: TStrings;
begin
  Result := FRESTResponse.Headers;
end;

function TResponseClient.ContentEncoding: string;
begin
  Result := FRESTResponse.ContentEncoding;
end;

function TResponseClient.ContentLength: Cardinal;
begin
  Result := FRESTResponse.ContentLength;
end;

function TResponseClient.ContentStream: TStream;
begin
  if Assigned(FStreamValue) then
    FreeAndNil(FStreamValue);
  FStreamValue := TMemoryStream.Create;
  if (Length(FRESTResponse.RawBytes) > 0) then
    FStreamValue.WriteBuffer(FRESTResponse.RawBytes[0], Length(FRESTResponse.RawBytes));
  Result := FStreamValue;
  Result.Position := 0;
end;

function TResponseClient.ContentType: string;
begin
  Result := FRESTResponse.ContentType;
end;

function TResponseClient.JSONValue(const AEncoding: TEncoding): TJSONValue;
var
  LContent: string;
begin
  if not(Assigned(FJSONValue)) then
  begin
    LContent := Content.Trim;
    if LContent.StartsWith('{') then
      FJSONValue := (TJSONObject.ParseJSONValue(AEncoding.GetBytes(LContent), 0) as TJSONObject)
    else if LContent.StartsWith('[') then
      FJSONValue := (TJSONObject.ParseJSONValue(AEncoding.GetBytes(LContent), 0) as TJSONArray)
    else
      raise Exception.Create('The return content is not a valid JSON value.');
  end;
  Result := FJSONValue;
end;

function TResponseClient.JSONValue: TJSONValue;
begin
  Result := Self.JSONValue(TEncoding.UTF8);
end;

function TResponseClient.RawBytes: TBytes;
begin
  Result := FRESTResponse.RawBytes;
end;

function TResponseClient.StatusCode: Integer;
begin
  Result := FRESTResponse.StatusCode;
end;

function TResponseClient.StatusText: string;
begin
  Result := FRESTResponse.StatusText;
end;

end.
