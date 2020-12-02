unit RESTRequest4D;

interface

uses RESTRequest4D.Request.Contract, RESTRequest4D.Response.Contract;

type
  IRequest = RESTRequest4D.Request.Contract.IRequest;
  IResponse = RESTRequest4D.Response.Contract.IResponse;

  TRequest = class
  public
    class function New: IRequest;
  end;

implementation

uses
{$IF DEFINED(RR4D_INDY)}
  RESTRequest4D.Request.Indy;
{$ELSE}
  RESTRequest4D.Request.Client;
{$ENDIF}

class function TRequest.New: IRequest;
begin
{$IF DEFINED(RR4D_INDY)}
  Result := TRequestIndy.Create;
{$ELSE}
  Result := TRequestClient.Create;
{$ENDIF}
end;

end.
