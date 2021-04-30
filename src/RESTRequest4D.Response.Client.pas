unit RESTRequest4D.Response.Client;

interface

uses RESTRequest4D.Response.Contract, REST.Client, System.SysUtils, System.JSON, System.Classes;

type
  TResponseClient = class(TInterfacedObject, IResponse)
  private
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
    function JSONValue: TJSONValue;
    function Headers: TStrings;
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
  if Assigned(FStreamValue) then
    FreeAndNil(FStreamValue);
  inherited;
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

function TResponseClient.JSONValue: TJSONValue;
begin
  Result := FRESTResponse.JSONValue;
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
