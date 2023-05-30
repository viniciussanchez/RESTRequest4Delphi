unit RESTRequest4D.Request.Contract;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses RESTRequest4D.Response.Contract, RESTRequest4D.Request.Adapter.Contract,
  {$IF NOT (DEFINED(RR4D_INDY) or DEFINED(FPC) or DEFINED(RR4D_NETHTTP))}
    REST.Types,
  {$ENDIF}
  {$IFDEF RR4D_INDY}
    IdHTTP,
  {$ENDIF}
  {$IFDEF FPC}
    SysUtils, fpjson, Classes, DB;
  {$ELSE}
    System.SysUtils, System.JSON, System.Classes, Data.DB;
  {$ENDIF}

type
  IRequest = interface
    ['{2C882459-F4C3-4854-8F7A-F68E8F8DE98E}']
    function AcceptEncoding: string; overload;
    function AcceptEncoding(const AAcceptEncoding: string): IRequest; overload;
    function AcceptCharset: string; overload;
    function AcceptCharset(const AAcceptCharset: string): IRequest; overload;
    function Accept: string; overload;
    function Accept(const AAccept: string): IRequest; overload;
	{$IF DEFINED(RR4D_SYNAPSE)}
    function MimeType: string; overload;
    function MimeType(const AMimeType: string): IRequest; overload;
	{$ENDIF}
    function Timeout: Integer; overload;
    function Timeout(const ATimeout: Integer): IRequest; overload;
    function Adapters(const AAdapter: IRequestAdapter): IRequest; overload;
    function Adapters(const AAdapters: TArray<IRequestAdapter>): IRequest; overload;
    function Adapters: TArray<IRequestAdapter>; overload;
    function BaseURL(const ABaseURL: string): IRequest; overload;
    function BaseURL: string; overload;
    function Resource(const AResource: string): IRequest; overload;
    function RaiseExceptionOn500: Boolean; overload;
    function RaiseExceptionOn500(const ARaiseException: Boolean): IRequest; overload;
    function Resource: string; overload;
    function ResourceSuffix(const AResourceSuffix: string): IRequest; overload;
    function ResourceSuffix: string; overload;
    function Token(const AToken: string): IRequest;
    function TokenBearer(const AToken: string): IRequest;
    function BasicAuthentication(const AUsername, APassword: string): IRequest;
    function Retry(const ARetries: Integer): IRequest;
    function Get: IResponse;
    function Post: IResponse;
    function Put: IResponse;
    function Delete: IResponse;
    function Patch: IResponse;
    function FullRequestURL(const AIncludeParams: Boolean = True): string;
    function ClearBody: IRequest;
    {$IF DEFINED(RR4D_NETHTTP)}
    function Asynchronous(const AValue: Boolean): IRequest;
    {$ENDIF}
    {$IF DEFINED(RR4D_INDY) or DEFINED(FPC) or DEFINED(RR4D_SYNAPSE) or DEFINED(RR4D_NETHTTP)}
    function AddParam(const AName, AValue: string): IRequest;
    function AddBody(const AContent: string): IRequest; overload;
    function AddHeader(const AName, AValue: string): IRequest;
    {$ELSE}
    function SynchronizedEvents(const AValue: Boolean): IRequest;
    function AddHeader(const AName, AValue: string; const AOptions: TRESTRequestParameterOptions = []): IRequest;
    function AddParam(const AName, AValue: string; const AKind: TRESTRequestParameterKind = {$IF COMPILERVERSION < 33}TRESTRequestParameterKind.pkGETorPOST{$ELSE}TRESTRequestParameterKind.pkQUERY{$ENDIF}): IRequest;
    function AddBody(const AContent: string; const AContentType: TRESTContentType = ctAPPLICATION_JSON): IRequest; overload;
    function FallbackCharsetEncoding(const AFallbackCharsetEncoding: string): IRequest;
    {$ENDIF}
    function AddBody(const AContent: TJSONObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TJSONArray; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TStream; const AOwns: Boolean = True): IRequest; overload;
    function AddUrlSegment(const AName, AValue: string): IRequest;
    function ClearHeaders: IRequest;
    function ClearParams: IRequest;
    function UserAgent(const AName: string): IRequest;
    function ContentType(const AContentType: string): IRequest;
    function AddCookies(const ACookies: TStrings): IRequest;
    function AddCookie(const ACookieName, ACookieValue: string): IRequest;
    function AddField(const AFieldName: string; const AValue: string): IRequest; overload;
    {$IF DEFINED(RR4D_INDY) or DEFINED(FPC) or DEFINED(RR4D_SYNAPSE) or DEFINED(RR4D_NETHTTP)}
    function AddFile(const AFieldName: string; const AFileName: string; const AContentType: string = ''): IRequest; overload;
    function AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string = ''; const AContentType: string = ''): IRequest; overload;
    {$ELSE}
    function AddFile(const AFieldName: string; const AFileName: string; const AContentType: TRESTContentType = TRESTContentType.ctNone): IRequest; overload;
    function AddFile(const AFieldName: string; const AValue: TStream; const AFileName: string = ''; const AContentType: TRESTContentType = TRESTContentType.ctNone): IRequest; overload;
    {$ENDIF}
    function Proxy(const AServer, APassword, AUsername: string; const APort: Integer): IRequest;
    function DeactivateProxy: IRequest;
    {$IF DEFINED(RR4D_INDY)}
    function CertFile(const APath: string): IRequest;
    function KeyFile(const APath: string): IRequest;
    function HTTPOptions(const AHTTPOptions: TIdHTTPOptions): IRequest;
    {$ENDIF}
  end;

implementation

end.
