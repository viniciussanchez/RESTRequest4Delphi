unit RESTRequest4D.Response;

interface

uses RESTRequest4D.Response.Intf, REST.Client, System.SysUtils, System.JSON,System.Classes;

type
  TResponse = class(TInterfacedObject, IResponse)
  private
    FRESTResponse: TRESTResponse;
    function Content: string;
    function ContentLength: Cardinal;
    function ContentType: string;
    function ContentEncoding: string;
    function StatusCode: Integer;
    function RawBytes: TBytes;
    function JSONValue: TJSONValue;
    function Headers :TStrings;
  public
    constructor Create(const ARESTResponse: TRESTResponse);
  end;

implementation

constructor TResponse.Create(const ARESTResponse: TRESTResponse);
begin
  FRESTResponse := ARESTResponse;
end;

function TResponse.Content: string;
begin
  Result := FRESTResponse.Content;
end;

function TResponse.Headers: TStrings;
begin
  Result := FRESTResponse.Headers;
   //Content;
end;

function TResponse.ContentEncoding: string;
begin
  Result := FRESTResponse.ContentEncoding;
end;

function TResponse.ContentLength: Cardinal;
begin
  Result := FRESTResponse.ContentLength;
end;

function TResponse.ContentType: string;
begin
  Result := FRESTResponse.ContentType;
end;

function TResponse.JSONValue: TJSONValue;
begin
  Result := FRESTResponse.JSONValue;
end;

function TResponse.RawBytes: TBytes;
begin
  Result := FRESTResponse.RawBytes;
end;        

function TResponse.StatusCode: Integer;
begin
  Result := FRESTResponse.StatusCode;
end;

end.
