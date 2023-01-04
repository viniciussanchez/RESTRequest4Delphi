unit RESTRequest4D.Response.Synapse;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses Classes, SysUtils, RESTRequest4D.Response.Contract, httpsend, ssl_openssl,
  {$IFDEF FPC}
    fpjson, jsonparser;
  {$ELSE}
    System.Json;
  {$ENDIF}

type
  TResponseSynapse = class(TInterfacedObject, IResponse)
  private
  {$IFDEF FPC}
    FJSONValue: TJSONData;
  {$ELSE}
    FJSONValue: TJSONValue;
  {$ENDIF}
    FHTTPSend: THTTPSend;
    FStreamResult: TStringStream;
    FContent: TStringStream;
    function Content: string;
    function ContentLength: Cardinal;
    function ContentType: string;
    function ContentEncoding: string;
    function ContentStream: TStream;
    function StatusCode: Integer;
    function StatusText: string;
    function RawBytes: TBytes;
    function Headers: TStrings;
  {$IFDEF FPC}
    function JSONValue: TJSONData;
  {$ELSE}
    function JSONValue: TJSONValue;
  {$ENDIF}
  public
    constructor Create(const AHTTPSend: THTTPSend);
    destructor Destroy; override;
  end;

implementation

function TResponseSynapse.Content: string;
begin
  Result := FStreamResult.DataString;
end;

function TResponseSynapse.ContentLength: Cardinal;
begin
  Result := StrToInt64Def(FHTTPSend.Headers.Values['Content-Length'], 0);
end;

function TResponseSynapse.ContentType: string;
begin
  Result := FHTTPSend.Headers.Values['Content-Type'];
end;

function TResponseSynapse.ContentEncoding: string;
begin
  Result := FHTTPSend.Headers.Values['Content-Encoding'];
end;

function TResponseSynapse.ContentStream: TStream;
begin
  Result := FStreamResult;
  Result.Position := 0;
end;

function TResponseSynapse.StatusCode: Integer;
begin
  Result := FHTTPSend.ResultCode;
end;

function TResponseSynapse.StatusText: string;
begin
  Result := FHTTPSend.ResultString;
end;

function TResponseSynapse.RawBytes: TBytes;
begin
  Result := FStreamResult.Bytes;
end;

{$IFDEF FPC}
function TResponseSynapse.JSONValue: TJSONData;
var
  LContent: string;
  LJSONParser: TJSONParser;
begin
  if not(Assigned(FJSONValue)) then
  begin
    LContent := Content.Trim;
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

{$ELSE}

function TResponseSynapse.JSONValue: TJSONValue;
var
  LContent: string;
begin
  if not(Assigned(FJSONValue)) then
  begin
    LContent := Content.Trim;
    if LContent.StartsWith('{') then
      FJSONValue := (TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(LContent), 0) as TJSONObject)
    else if LContent.StartsWith('[') then
      FJSONValue := (TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(LContent), 0) as TJSONArray)
    else
      raise Exception.Create('The return content is not a valid JSON value.');
  end;
  Result := FJSONValue;
end;
{$ENDIF}

function TResponseSynapse.Headers: TStrings;
begin
  Result := FHTTPSend.Headers;
end;

constructor TResponseSynapse.Create(const AHTTPSend: THTTPSend);
begin
  FHTTPSend := AHTTPSend;
  FHTTPSend.KeepAlive := True;
  FHTTPSend.Headers.Clear;
  FStreamResult := TStringStream.Create;
end;

destructor TResponseSynapse.Destroy;
begin
  FreeAndNil(FStreamResult);
  if Assigned(FJSONValue) then
    FJSONValue.Free;
  inherited Destroy;
end;

end.
