unit RESTRequest4D;

interface

uses
  {$IF NOT (DEFINED(RR4D_INDY) or DEFINED(FPC) or DEFINED(RR4D_NETHTTP))}
    REST.Types,
  {$ENDIF}
  RESTRequest4D.Request.Contract, RESTRequest4D.Response.Contract;

type
  IRequest = RESTRequest4D.Request.Contract.IRequest;
  IResponse = RESTRequest4D.Response.Contract.IResponse;

  TRequest = class
  public
    class function New: IRequest;
  end;

{$IF NOT (DEFINED(RR4D_INDY) or DEFINED(FPC) or DEFINED(RR4D_NETHTTP))}
const
  poDoNotEncode = REST.Types.poDoNotEncode;
  poTransient = REST.Types.poTransient;
  poAutoCreated = REST.Types.poAutoCreated;
  {$IF COMPILERVERSION >= 33}
    poFlatArray = REST.Types.poFlatArray;
    poPHPArray = REST.Types.poPHPArray;
    poListArray = REST.Types.poListArray;
  {$ENDIF}

  pkCOOKIE = REST.Types.pkCOOKIE;
  pkGETorPOST = REST.Types.pkGETorPOST;
  pkURLSEGMENT = REST.Types.pkURLSEGMENT;
  pkHTTPHEADER = REST.Types.pkHTTPHEADER;
  pkREQUESTBODY = REST.Types.pkREQUESTBODY;
  {$IF COMPILERVERSION >= 32}
    pkFILE = REST.Types.pkFILE;
  {$ENDIF}
  {$IF COMPILERVERSION >= 33}
    pkQUERY = REST.Types.pkQUERY;
  {$ENDIF}
{$ENDIF}

implementation

uses
  {$IF DEFINED(FPC) and (not DEFINED(RR4D_INDY)) and (not DEFINED(RR4D_SYNAPSE))}
    RESTRequest4D.Request.FPHTTPClient;
  {$ELSEIF DEFINED(RR4D_INDY)}
    RESTRequest4D.Request.Indy;
  {$ELSEIF DEFINED(RR4D_NETHTTP)}
    RESTRequest4D.Request.NetHTTP;
  {$ELSEIF DEFINED(RR4D_SYNAPSE)}
    RESTRequest4D.Request.Synapse;
  {$ELSE}
    RESTRequest4D.Request.Client;
  {$ENDIF}

class function TRequest.New: IRequest;
begin
  {$IF DEFINED(FPC) and (not DEFINED(RR4D_INDY)) and (not DEFINED(RR4D_SYNAPSE))}
    Result := TRequestFPHTTPClient.Create;
  {$ELSEIF DEFINED(RR4D_INDY)}
    Result := TRequestIndy.Create;
  {$ELSEIF DEFINED(RR4D_NETHTTP)}
    Result := TRequestNetHTTP.Create;
  {$ELSEIF DEFINED(RR4D_SYNAPSE)}
    Result := TRequestSynapse.Create;
  {$ELSE}
    Result := TRequestClient.Create;
  {$ENDIF}
end;

end.
