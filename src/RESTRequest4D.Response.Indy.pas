unit RESTRequest4D.Response.Indy;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses RESTRequest4D.Response.Contract, RESTRequest4D.Request.Contract, IdHTTP,
  {$IFDEF FPC}
    SysUtils, fpjson, Classes, jsonparser;
  {$ELSE}
    System.SysUtils, System.JSON, System.Classes;
  {$ENDIF}

type
  TResponseIndy = class(TInterfacedObject, IResponse)
  private
    FRequest: IRequest;
    FHeaders: TStrings;
  {$IFDEF FPC}
    FJSONValue: TJSONData;
  {$ELSE}
    FJSONValue: TJSONValue;
  {$ENDIF}
    FIdHTTP: TIdHTTP;
    FStreamResult: TStringStream;
    FContent: string;
    FStatusCode: Integer;
    FStatusText: string;
    FContentType: string;
    FContentEncoding: string;
    FContentLength: Cardinal;
    function Content: string;
    function ContentLength: Cardinal;
    function ContentType: string;
    function ContentEncoding: string;
    function ContentStream: TStream;
    function StatusCode: Integer;
    function StatusText: string;
    function RawBytes: TBytes;
    function Headers: TStrings;
    function GetCookie(const ACookieName: string): string;
  {$IFDEF FPC}
    function JSONValue: TJSONData;
  {$ELSE}
    function JSONValue: TJSONValue; overload;
    function JSONValue(const AEncoding: TEncoding): TJSONValue; overload;
  {$ENDIF}
  public
    constructor Create(const ARequest: IRequest; const AIdHTTP: TIdHTTP; const AStreamResult: TStringStream);
    destructor Destroy; override;
    procedure UpdateResponseData;
  end;

implementation

{$IFDEF FPC}
function TResponseIndy.JSONValue: TJSONData;
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
function TResponseIndy.JSONValue: TJSONValue;
begin
  Result := Self.JSONValue(TEncoding.UTF8);
end;

function TResponseIndy.JSONValue(const AEncoding: TEncoding): TJSONValue;
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
{$ENDIF}

function TResponseIndy.RawBytes: TBytes;
begin
  Result:= nil;
  if Assigned(FStreamResult) then
    Result := FStreamResult.Bytes;
end;

function TResponseIndy.Content: string;
begin
  if FContent.IsEmpty and Assigned(FStreamResult) and (FStreamResult.Size > 0) then
  begin
    try
      FContent := FStreamResult.DataString;
    except
      on E: EEncodingError do
      begin
        try
          FContent := TEncoding.ANSI.GetString(FStreamResult.Bytes, 0, FStreamResult.Size);
        except
          FContent := '';
        end;
      end;
    end;
  end;
  Result := FContent;
end;

function TResponseIndy.Headers: TStrings;
begin
  if not Assigned(FHeaders) then
    FHeaders := TStringList.Create;
  Result := FHeaders;
end;

function TResponseIndy.ContentEncoding: string;
begin
  Result := FContentEncoding;
end;

function TResponseIndy.ContentLength: Cardinal;
begin
  Result := FContentLength;
end;

function TResponseIndy.ContentStream: TStream;
begin
  Result:= nil;
  if Assigned(FStreamResult) then
  begin
    Result := FStreamResult;
    Result.Position := 0;
  end;
end;

function TResponseIndy.ContentType: string;
begin
  Result := FContentType;
end;

constructor TResponseIndy.Create(const ARequest: IRequest; const AIdHTTP: TIdHTTP; const AStreamResult: TStringStream);
begin
  FRequest := ARequest;
  FIdHTTP := AIdHTTP;
  FStreamResult := AStreamResult;
  FStatusCode := 0;
  FStatusText := '';
  FContentType := '';
  FContentEncoding := '';
  FContentLength := 0;
  FContent := '';
end;

destructor TResponseIndy.Destroy;
begin
  if Assigned(FJSONValue) then
    FJSONValue.Free;
  if Assigned(FHeaders) then
    FHeaders.Free;
  inherited;
end;

function TResponseIndy.GetCookie(const ACookieName: string): string;
var
  I: Integer;
begin
  for I := 0 to Pred(FIdHTTP.CookieManager.CookieCollection.Count) do
  begin
    if Trim(LowerCase(FIdHTTP.CookieManager.CookieCollection.Cookies[I].CookieName)) = Trim(LowerCase(ACookieName)) then
      Result := FIdHTTP.CookieManager.CookieCollection.Cookies[I].CookieText;
  end;
end;

function TResponseIndy.StatusCode: Integer;
begin
  Result := FStatusCode;
end;

function TResponseIndy.StatusText: string;
begin
  Result := FStatusText;
end;

procedure TResponseIndy.UpdateResponseData;
var
  I: Integer;
begin
  if Assigned(FIdHTTP) then
  begin
    FStatusCode := FIdHTTP.Response.ResponseCode;
    FStatusText := FIdHTTP.Response.ResponseText;
    FContentType := FIdHTTP.Response.ContentType;
    FContentEncoding := FIdHTTP.Response.ContentEncoding;
    FContentLength := FIdHTTP.Response.ContentLength;

    if not Assigned(FHeaders) then
      FHeaders := TStringList.Create
    else
      FHeaders.Clear;

    for I := 0 to Pred(FIdHTTP.Response.RawHeaders.Count) do
      FHeaders.Values[FIdHTTP.Response.RawHeaders.Names[I]] := FIdHTTP.Response.RawHeaders.Values[FIdHTTP.Response.RawHeaders.Names[I]];
  end;

  FContent := '';
end;

end.
