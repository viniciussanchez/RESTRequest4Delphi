unit RESTRequest4D.Response.Indy;

{$IFDEF FPC} {$mode delphi} {$ENDIF}

interface

uses RESTRequest4D.Response.Contract, IdHTTP
  {$IFDEF FPC}
    ,SysUtils, fpjson, Classes, jsonparser
  {$ELSE}
    ,System.SysUtils, System.JSON, System.Classes
  {$ENDIF}
  ;

type

  { TResponseIndy }

  TResponseIndy = class(TInterfacedObject, IResponse)
  private
    FJSONValue: {$IFDEF FPC} TJSONData {$ELSE} TJSONValue {$ENDIF};
    FIdHTTP: TIdHTTP;
    function Content: string;
    function ContentLength: Cardinal;
    function ContentType: string;
    function ContentEncoding: string;
    function StatusCode: Integer;
    function RawBytes: TBytes;
    function JSONValue: {$IFDEF FPC} TJSONData {$ELSE} TJSONValue {$ENDIF};
    function Headers: TStrings;
  public
    constructor Create(const AIdHTTP: TIdHTTP);
    destructor Destroy; override;
  end;

implementation

function TResponseIndy.JSONValue: {$IFDEF FPC} TJSONData {$ELSE} TJSONValue {$ENDIF};
var
  LContent: string;
  {$IFDEF FPC}
  LJSONParser : TJSONParser;
  {$ENDIF}
begin
  if not(Assigned(FJSONValue)) then
  begin
    LContent := Content.Trim;
    {$IFDEF FPC}
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
    {$ELSE}
    if LContent.StartsWith('{') then
      FJSONValue := (TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(LContent), 0) as TJSONObject)
    else if LContent.StartsWith('[') then
      FJSONValue := (TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(LContent), 0) as TJSONArray)
    else
      raise Exception.Create('The return content is not a valid JSON value.');
    {$ENDIF}
  end;
  Result := FJSONValue;
end;

function TResponseIndy.RawBytes: TBytes;
begin
  Result := TStringStream(FIdHTTP.Response.ContentStream).Bytes;
end;

function TResponseIndy.Content: string;
begin
  Result := TStringStream(FIdHTTP.Response.ContentStream).DataString;
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

destructor TResponseIndy.Destroy;
begin
  if Assigned(FJSONValue) then
    FJSONValue.Free;
  inherited;
end;

function TResponseIndy.StatusCode: Integer;
begin
  Result := FIdHTTP.Response.ResponseCode;
end;

end.
