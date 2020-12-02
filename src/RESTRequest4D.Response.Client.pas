unit RESTRequest4D.Response.Client;

interface

uses RESTRequest4D.Response.Contract, REST.Client, System.SysUtils, System.JSON, System.Classes;

type
  TResponseClient = class(TInterfacedObject, IResponse)
  private
    FRESTResponse: TRESTResponse;
    function Content: string;
    function ContentLength: Cardinal;
    function ContentType: string;
    function ContentEncoding: string;
    function StatusCode: Integer;
    function RawBytes: TBytes;
    function JSONValue: TJSONValue;
    function Headers: TStrings;
  public
    constructor Create(const ARESTResponse: TRESTResponse);
  end;

implementation

constructor TResponseClient.Create(const ARESTResponse: TRESTResponse);
begin
  FRESTResponse := ARESTResponse;
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

end.
