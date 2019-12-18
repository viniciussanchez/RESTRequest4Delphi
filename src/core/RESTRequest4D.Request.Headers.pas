unit RESTRequest4D.Request.Headers;

interface

uses RESTRequest4D.Request.Headers.Intf, REST.Client, REST.Types, System.Classes;

type
  TRequestHeaders = class(TInterfacedObject, IRequestHeaders)
  private
    FHeaders: TStrings;
    FRESTRequest: TRESTRequest;
    function Clear: IRequestHeaders;
    function Add(const AName, AValue: string; const AOptions: TRESTRequestParameterOptions = []): IRequestHeaders;
  public
    constructor Create(const ARESTRequest: TRESTRequest);
    destructor Destroy; override;
  end;

implementation

uses System.SysUtils;

{ TRequestHeaders }

function TRequestHeaders.Add(const AName, AValue: string; const AOptions: TRESTRequestParameterOptions): IRequestHeaders;
begin
  Result := Self;
  if (not AName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
  begin
    if (FHeaders.IndexOf(AName) < 0) then
      FHeaders.Add(AName);
    FRESTRequest.Params.AddHeader(AName, AValue);
    FRESTRequest.Params.ParameterByName(AName).Options := AOptions;
  end;
end;

function TRequestHeaders.Clear: IRequestHeaders;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to Pred(FHeaders.Count) do
    FRESTRequest.Params.Delete(FRESTRequest.Params.ParameterByName(FHeaders[I]));
end;

constructor TRequestHeaders.Create(const ARESTRequest: TRESTRequest);
begin
  FHeaders := TStringList.Create;
  FRESTRequest := ARESTRequest;
end;

destructor TRequestHeaders.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

end.
