unit RESTRequest4D.Response.Indy;

interface

uses RESTRequest4D.Response.Contract, System.SysUtils, System.JSON, System.Classes, IdHTTP;

type
  TResponseIndy = class(TInterfacedObject, IResponse)
  private
    FIdHTTP: TIdHTTP;
    function Content: string;
    function ContentLength: Cardinal;
    function ContentType: string;
    function ContentEncoding: string;
    function StatusCode: Integer;
    function RawBytes: TBytes;
    function JSONValue: TJSONValue;
    function Headers: TStrings;
  public
    constructor Create(const AIdHTTP: TIdHTTP);
  end;

implementation

function TResponseIndy.JSONValue: TJSONValue;
begin
  raise Exception.Create('Not implemented');
end;

function TResponseIndy.RawBytes: TBytes;
begin
  raise Exception.Create('Not implemented');
end;

function TResponseIndy.Content: string;
begin
  Result := FIdHTTP.ResponseText;
end;

function TResponseIndy.Headers: TStrings;
var
  LHeader: string;
  LHeaders: TArray<string>;
begin
  Result := TStringList.Create;
  LHeaders := FIdHTTP.Response.CustomHeaders.ToStringArray;
  for LHeader in LHeaders do
    Result.Add(LHeader);
end;

function TResponseIndy.ContentEncoding: string;
begin
  Result := FIdHTTP.Response.ContentEncoding;
end;

function TResponseIndy.ContentLength: Cardinal;
begin
  Result := FIdHTTP.Response.ContentLength;
end;

function TResponseIndy.ContentType: string;
begin
  Result := FIdHTTP.Response.ContentType;
end;

constructor TResponseIndy.Create(const AIdHTTP: TIdHTTP);
begin
  FIdHTTP := AIdHTTP;
end;

function TResponseIndy.StatusCode: Integer;
begin
  Result := FIdHTTP.Response.ResponseCode;
end;

end.
