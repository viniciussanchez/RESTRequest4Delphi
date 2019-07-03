unit RESTRequest4D.Request.Body;

interface

uses RESTRequest4D.Request.Body.Intf, REST.Client, System.JSON;

type
  TRequestBody = class(TInterfacedObject, IRequestBody)
  private
    FRESTRequest: TRESTRequest;
    function Clear: IRequestBody;
    function Add(const AContent: string): IRequestBody; overload;
    function Add(const AContent: TJSONObject; const AOwns: Boolean = True): IRequestBody; overload;
    function Add(const AContent: TObject; const AOwns: Boolean = True): IRequestBody; overload;
  public
    constructor Create(const ARESTRequest: TRESTRequest);
  end;

implementation

uses System.SysUtils;

{ TRequestBody }

function TRequestBody.Add(const AContent: string): IRequestBody;
begin
  Result := Self;
  if not AContent.Trim.IsEmpty then
    FRESTRequest.Body.Add(AContent);
end;

function TRequestBody.Add(const AContent: TJSONObject; const AOwns: Boolean): IRequestBody;
begin
  Result := Self;
  if Assigned(AContent) then
  begin
    FRESTRequest.Body.Add(AContent);
    if AOwns then
      AContent.Free;
  end;
end;

function TRequestBody.Add(const AContent: TObject; const AOwns: Boolean): IRequestBody;
begin
  Result := Self;
  if Assigned(AContent) then
  begin
    FRESTRequest.Body.Add(AContent);
    if AOwns then
      AContent.Free;
  end;
end;

function TRequestBody.Clear: IRequestBody;
begin
  Result := Self;
  FRESTRequest.ClearBody;
end;

constructor TRequestBody.Create(const ARESTRequest: TRESTRequest);
begin
  FRESTRequest := ARESTRequest;
end;

end.
