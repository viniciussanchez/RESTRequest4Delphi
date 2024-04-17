unit RESTRequest4D.Response.ICS;

interface

uses RESTRequest4D.Response.Contract, OverbyteIcsSslHttpRest, OverbyteIcsLogger, System.SysUtils, System.JSON, System.Classes;

type
  TResponseICS = class(TInterfacedObject, IResponse)
  private
    FJSONValue: TJSONValue;
    FSslHttpRest: TSslHttpRest;
    FLogICS: TStringList;
    function Content: string;
    function ContentLength: int64;
    function ContentType: string;
    function ContentEncoding: string;
    function ContentStream: TStream;
    function StatusCode: Integer;
    function StatusText: string;
    function RawBytes: TBytes;
    function GetCookie(const ACookieName: string): string;
    function JSONValue: TJSONValue; overload;
    function JSONValue(const AEncoding: TEncoding): TJSONValue; overload;
    function Headers: TStrings;
    procedure HttpRest1HttpRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
    function ICSLog: string;
  public
    constructor Create(const AFSslHttpRest: TSslHttpRest);
    destructor Destroy; override;
  end;

implementation

function TResponseICS.JSONValue: TJSONValue;
begin
  Result := Self.JSONValue(TEncoding.UTF8);
end;

function TResponseICS.JSONValue(const AEncoding: TEncoding): TJSONValue;
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

function TResponseICS.RawBytes: TBytes;
begin
  Result := TBytesStream(FSslHttpRest.ResponseStream).Bytes;
end;

function TResponseICS.Content: string;
begin
  Result := FSslHttpRest.ResponseRaw;
end;

function TResponseICS.ICSLog: string;
begin
  Result := FLogICS.Text;
end;

function TResponseICS.GetCookie(const ACookieName: string): string;
begin
  raise Exception.Create('Not implemented');
end;

function TResponseICS.Headers: TStrings;
begin
  Result := FSslHttpRest.RcvdHeader;
end;

procedure TResponseICS.HttpRest1HttpRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
  FLogICS.Add(Msg);
end;

function TResponseICS.StatusCode: Integer;
begin
  Result := FSslHttpRest.StatusCode;
end;

function TResponseICS.StatusText: string;
begin
  Result := FSslHttpRest.ReasonPhrase;
end;

function TResponseICS.ContentEncoding: string;
begin
  Result := FSslHttpRest.ContentEncoding;
end;

function TResponseICS.ContentLength: int64;
begin
  Result := FSslHttpRest.RcvdCount;
end;

function TResponseICS.ContentStream: TStream;
begin
  Result := FSslHttpRest.ResponseStream;
  Result.Position := 0;
end;

function TResponseICS.ContentType: string;
begin
  Result := FSslHttpRest.ContentType;
end;

constructor TResponseICS.Create(const AFSslHttpRest: TSslHttpRest);
begin
  FSslHttpRest := AFSslHttpRest;
  FLogICS := TStringList.Create;
  FSslHttpRest.OnHttpRestProg := HttpRest1HttpRestProg;
end;

destructor TResponseICS.Destroy;
begin
  if Assigned(FJSONValue) then
    FreeAndNil(FJSONValue);
  FreeAndNil(FLogICS);
  inherited;
end;

end.
