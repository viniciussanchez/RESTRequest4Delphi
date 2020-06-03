unit RESTRequest4D.Request.Response;

interface

uses RESTRequest4D.Request.Response.Intf, REST.Client, System.SysUtils, System.JSON;

type
  TRequestResponse = class(TInterfacedObject, IRequestResponse)
  private
    FRESTResponse: TRESTResponse;
    function Content: string;
    function ContentLength: Cardinal;
    function ContentType: string;
    function ContentEncoding: string;
    function StatusCode: Integer;
    function RawBytes: TBytes;
    function JSONValue: TJSONValue;
  public
    constructor Create(const ARESTResponse: TRESTResponse);
  end;

implementation

{ TRequestResponse }

constructor TRequestResponse.Create(const ARESTResponse: TRESTResponse);
begin
  FRESTResponse := ARESTResponse;
end;

function TRequestResponse.Content: string;
begin
  Result := FRESTResponse.Content;
end;

function TRequestResponse.ContentEncoding: string;
begin
  Result := FRESTResponse.ContentEncoding;
end;

function TRequestResponse.ContentLength: Cardinal;
begin
  Result := FRESTResponse.ContentLength;
end;

function TRequestResponse.ContentType: string;
begin
  Result := FRESTResponse.ContentType;
end;

function TRequestResponse.JSONValue: TJSONValue;
begin
  Result := FRESTResponse.JSONValue;
end;

function TRequestResponse.RawBytes: TBytes;
begin
  Result := FRESTResponse.RawBytes;
end;        

function TRequestResponse.StatusCode: Integer;
begin
  Result := FRESTResponse.StatusCode;
end;

end.
