unit RESTRequest4D.Response.Indy;

interface

uses RESTRequest4D.Response.Contract, System.SysUtils, System.JSON, System.Classes, IdHTTP;

type
  TResponseIndy = class(TInterfacedObject, IResponse)
  private
    FJSONValue: TJSONValue;
    FContent: TStringStream;
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
    procedure SetContent(const AContent: TStringStream);
    constructor Create(const AIdHTTP: TIdHTTP);
    destructor Destroy; override;
  end;

implementation

function TResponseIndy.JSONValue: TJSONValue;
var
  LContent: string;
begin
  if not(Assigned(FJSONValue)) then
  begin
    LContent := FContent.DataString.Trim;
    if LContent.StartsWith('{') then
      FJSONValue := (TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(LContent), 0) as TJSONObject)
    else if LContent.StartsWith('[') then
      FJSONValue := (TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(LContent), 0) as TJSONArray)
    else
      raise Exception.Create('The return content is not a valid JSON value.');
  end;
  Result := FJSONValue;
end;

function TResponseIndy.RawBytes: TBytes;
begin
  Result := FContent.Bytes;
end;

function TResponseIndy.Content: string;
begin
  Result := FContent.DataString;
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

procedure TResponseIndy.SetContent(const AContent: TStringStream);
begin
  FContent := AContent;
end;

function TResponseIndy.StatusCode: Integer;
begin
  Result := FIdHTTP.Response.ResponseCode;
end;

end.
