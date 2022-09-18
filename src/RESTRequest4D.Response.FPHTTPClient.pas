unit RESTRequest4D.Response.FPHTTPClient;

{$mode Delphi}

interface

uses
  SysUtils,
  Classes,
  fpJSON,
  jsonparser,
  RESTRequest4D.Response.Contract,
  fphttpclient,
  openssl,
  opensslsockets;

type

  TResponseFpHTTPClient = class(TInterfacedObject, IResponse)
  private
    FFPHTTPClient: TFPHTTPClient;
    FJSONValue   : TJSONData;
    FStreamResult: TStringStream;

    function Content        : String;
    function ContentLength  : Cardinal;
    function ContentType    : String;
    function ContentEncoding: String;
    function ContentStream  : TStream;
    function StatusCode     : Integer;
    function StatusText     : String;
    function RawBytes       : TBytes;
    function Headers        : TStrings;
    function StreamResult   : TStringStream;
    function JSONValue      : TJSONData;

  public
    constructor Create(const AFPHTTPClient: TFPHTTPClient);
    destructor Destroy; override;
  end;

implementation

constructor TResponseFpHTTPClient.Create(const AFPHTTPClient: TFPHTTPClient);
begin
  FFPHTTPClient := AFPHTTPClient;
  FStreamResult := TStringStream.Create;
end;

destructor TResponseFpHTTPClient.Destroy;
begin
  FreeAndNil(FStreamResult);
  if Assigned(FJSONValue) then
  begin
    FJSONValue.Free;
  end;

  inherited;
end;

function TResponseFpHTTPClient.RawBytes: TBytes;
begin
  Result := FStreamResult.Bytes;
end;

function TResponseFpHTTPClient.Content: string;
begin
  Result := FStreamResult.DataString;
end;

function TResponseFpHTTPClient.Headers: TStrings;
begin
  Result := FFPHTTPClient.ResponseHeaders;
end;

function TResponseFpHTTPClient.StreamResult: TStringStream;
begin
  Result := FStreamResult;
end;

function TResponseFpHTTPClient.JSONValue: TJSONData;
var
  LContent   : string;
  LJSONParser : TJSONParser;
begin
  if not(Assigned(FJSONValue)) then
  begin
    LContent    := Content.Trim;
    LJSONParser := TJSONParser.Create(LContent, False);
    try
      if LContent.StartsWith('{') then
        FJSONValue := LJSONParser.Parse as TJSONObject
      else if LContent.StartsWith('[') then
        FJSONValue := LJSONParser.Parse as TJSONArray
      else
        raise Exception.Create('The return content is not a valid JSON value.');
    finally
      LJSONParser.Free;
    end;
  end;
  Result := FJSONValue;

end;

function TResponseFpHTTPClient.ContentEncoding: string;
begin
  Result := FFPHTTPClient.GetHeader('Content-Encoding');
end;

function TResponseFpHTTPClient.ContentLength: Cardinal;
begin
  Result := StrToInt( FFPHTTPClient.GetHeader('Content-Length') );
end;

function TResponseFpHTTPClient.ContentStream: TStream;
begin
  Result := FStreamResult;
  Result.Position := 0;
end;

function TResponseFpHTTPClient.ContentType: string;
begin
  Result := FFPHTTPClient.GetHeader('Content-Type');
end;

function TResponseFpHTTPClient.StatusCode: Integer;
begin
  Result := FFPHTTPClient.ResponseStatusCode;
end;

function TResponseFpHTTPClient.StatusText: string;
begin
  Result := FFPHTTPClient.ResponseStatusText;
end;

end.


