unit RESTRequest4D.Request.Params;

interface

uses RESTRequest4D.Request.Params.Intf, REST.Client, REST.Types;

type
  TRequestParams = class(TInterfacedObject, IRequestParams)
  private
    FRESTRequest: TRESTRequest;
    function Clear: IRequestParams;
    function AddParam(const AName, AValue: string): IRequestParams;
    function AddHeader(const AName, AValue: string; const AOptions: TRESTRequestParameterOptions = []): IRequestParams;
  public
    constructor Create(const ARESTRequest: TRESTRequest);
  end;

implementation

uses System.SysUtils;

{ TRequestParams }

function TRequestParams.AddHeader(const AName, AValue: string; const AOptions: TRESTRequestParameterOptions = []): IRequestParams;
begin
  Result := Self;
  if (not AName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
  begin
    FRESTRequest.Params.AddHeader(AName, AValue);
    FRESTRequest.Params.ParameterByName(AName).Options := AOptions;
  end;
end;

function TRequestParams.AddParam(const AName, AValue: string): IRequestParams;
begin
  Result := Self;
  if (not AName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
    FRESTRequest.AddParameter(AName, AValue);
end;

function TRequestParams.Clear: IRequestParams;
begin
  Result := Self;
  FRESTRequest.Params.Clear;
end;

constructor TRequestParams.Create(const ARESTRequest: TRESTRequest);
begin
  FRESTRequest := ARESTRequest;
end;

end.
