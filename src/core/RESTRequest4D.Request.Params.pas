unit RESTRequest4D.Request.Params;

interface

uses RESTRequest4D.Request.Params.Intf, REST.Client, REST.Types, System.Classes;

type
  TRequestParams = class(TInterfacedObject, IRequestParams)
  private
    FParams: TStrings;
    FRESTRequest: TRESTRequest;
    function Clear: IRequestParams;
    function Add(const AName, AValue: string; const AKind: TRESTRequestParameterKind = TRESTRequestParameterKind.pkQUERY): IRequestParams; overload;
    function Add(const AName: string; const AValue: Currency; const AKind: TRESTRequestParameterKind = TRESTRequestParameterKind.pkQUERY): IRequestParams; overload;
  public
    constructor Create(const ARESTRequest: TRESTRequest);
    destructor Destroy; override;
  end;

implementation

uses System.SysUtils;

{ TRequestParams }

function TRequestParams.Add(const AName, AValue: string; const AKind: TRESTRequestParameterKind): IRequestParams;
begin
  Result := Self;
  if (not AName.Trim.IsEmpty) and (not AValue.Trim.IsEmpty) then
  begin
    FParams.Add(AName);
    FRESTRequest.AddParameter(AName, AValue, AKind);
  end;
end;

function TRequestParams.Add(const AName: string; const AValue: Currency; const AKind: TRESTRequestParameterKind): IRequestParams;
begin
  Result := Self.Add(AName, CurrToStr(AValue), AKind);
end;

function TRequestParams.Clear: IRequestParams;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to Pred(FParams.Count) do
    FRESTRequest.Params.Delete(FRESTRequest.Params.ParameterByName(FParams[I]));
end;

constructor TRequestParams.Create(const ARESTRequest: TRESTRequest);
begin
  inherited Create;
  FParams := TStringList.Create;
  FRESTRequest := ARESTRequest;
end;

destructor TRequestParams.Destroy;
begin
  FParams.Free;
  inherited;
end;

end.
