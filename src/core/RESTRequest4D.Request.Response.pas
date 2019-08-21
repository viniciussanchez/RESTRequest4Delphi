unit RESTRequest4D.Request.Response;

interface

uses RESTRequest4D.Request.Response.Intf, REST.Client;

type
  TRequestResponse = class(TInterfacedObject, IRequestResponse)
  private
    FRESTResponse: TRESTResponse;
    function GetContent: string;
    function GetContentLength: Cardinal;
    function GetContentType: string;
    function GetContentEncoding: string;
    function GetStatusCode: Integer;
  public
    constructor Create(const ARESTResponse: TRESTResponse);
  end;

implementation

{ TRequestResponse }

constructor TRequestResponse.Create(const ARESTResponse: TRESTResponse);
begin
  FRESTResponse := ARESTResponse;
end;

function TRequestResponse.GetContent: string;
begin
  Result := FRESTResponse.Content;
end;

function TRequestResponse.GetContentEncoding: string;
begin
  Result := FRESTResponse.ContentEncoding;
end;

function TRequestResponse.GetContentLength: Cardinal;
begin
  Result := FRESTResponse.ContentLength;
end;

function TRequestResponse.GetContentType: string;
begin
  Result := FRESTResponse.ContentType;
end;

function TRequestResponse.GetStatusCode: Integer;
begin
  Result := FRESTResponse.StatusCode;
end;

end.
