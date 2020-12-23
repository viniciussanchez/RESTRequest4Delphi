unit RESTRequest4D.Request.Contract;

interface

uses RESTRequest4D.Response.Contract,
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
    function Timeout: Integer; overload;
    function Timeout(const ATimeout: Integer): IRequest; overload;
    function DataSetAdapter(const ADataSet: TDataSet): IRequest; overload;
    function DataSetAdapter: TDataSet; overload;
    function BaseURL(const ABaseURL: string): IRequest; overload;
    function BaseURL: string; overload;
    function Resource(const AResource: string): IRequest; overload;
    function RaiseExceptionOn500: Boolean; overload;
    function RaiseExceptionOn500(const ARaiseException: Boolean): IRequest; overload;
    function Resource: string; overload;
    function ResourceSuffix(const AResourceSuffix: string): IRequest; overload;
    function ResourceSuffix: string; overload;
    function Token(const AToken: string): IRequest;
    function BasicAuthentication(const AUsername, APassword: string): IRequest;
    function Get: IResponse;
    function Post: IResponse;
    function Put: IResponse;
    function Delete: IResponse;
    function Patch: IResponse;
    function FullRequestURL(const AIncludeParams: Boolean = True): string;
    function ClearBody: IRequest;
    function AddBody(const AContent: string): IRequest; overload;
    function AddBody(const AContent: TJSONObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TJSONArray; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TObject; const AOwns: Boolean = True): IRequest; overload;
    function AddBody(const AContent: TStream; const AOwns: Boolean = True): IRequest; overload;
    function ClearHeaders: IRequest;
    function AddHeader(const AName, AValue: string): IRequest;
    function ClearParams: IRequest;
    function UserAgent(const AName: string): IRequest;
    function ContentType(const AContentType: string): IRequest;
    function AddCookies(const ACookies: TStrings): IRequest;
    function AddParam(const AName, AValue: string): IRequest;
    function AddFile(const AName: string; const AValue: TStream): IRequest;
  end;

implementation

end.
