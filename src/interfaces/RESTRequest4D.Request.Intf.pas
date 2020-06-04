unit RESTRequest4D.Request.Intf;

interface

uses Data.DB, REST.Client, RESTRequest4D.Request.Params.Intf, REST.Types, System.SysUtils, RESTRequest4D.Request.Response.Intf,
  System.JSON;

type
  /// <summary>
  ///   Interface to represent a request to a given server.
  /// </summary>
  IRequest = interface
    ['{2C882459-F4C3-4854-8F7A-F68E8F8DE98E}']
    /// <summary>
    ///   Get defined accepted encoding.
    /// </summary>
    /// <returns>
    ///   Accepted encoding.
    /// </returns>
    function AcceptEncoding: string; overload;
    /// <summary>
    ///   Specifies the accepted encoding.
    /// </summary>
    /// <param name="AAcceptEncoding">
    ///   Accepted encoding.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    /// <remarks>
    ///   Defaults to empty string, which means "identity encoding".
    ///   To allow for compressed responses set to "gzip, deflate".
    /// </remarks>
    function AcceptEncoding(const AAcceptEncoding: string): IRequest; overload;
    /// <summary>
    ///   Get defined charset that the response is expected to be encoded in.
    /// </summary>
    /// <returns>
    ///   Charset that the response is expected to be encoded in.
    /// </returns>
    function AcceptCharset: string; overload;
    /// <summary>
    ///   Specifies the charset that the response is expected to be encoded in.
    /// </summary>
    /// <param name="AAcceptCharset">
    ///   Charset that the response is expected to be encoded in.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    /// <remarks>
    ///   Defaults to UTF-8.
    /// </remarks>
    function AcceptCharset(const AAcceptCharset: string): IRequest; overload;
    /// <summary>
    ///   Get difined content-type that is accepted for the response.
    /// </summary>
    /// <returns>
    ///   Content-Type that is accepted for the response.
    /// </returns>
    function Accept: string; overload;
    /// <summary>
    ///   <para>
    ///     Specifies the Content-Type that is accepted for the response.
    ///   </para>
    ///   <para>
    ///     Defaults to: application/json,text/plain;q=0.9,text/html;q=0.8
    ///   </para>
    ///   <para>
    ///     We are after JSON, which is why it has the highest quality factor (default 1.0)
    ///   </para>
    /// </summary>
    /// <param name="AAccept">
    ///   Content-Type that is accepted for the response.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function Accept(const AAccept: string): IRequest; overload;
    /// <summary>
    ///   Get defined request timeout.
    /// </summary>
    /// <returns>
    ///   Request timeout.
    /// </returns>
    function Timeout: Integer; overload;
    /// <summary>
    ///   Defines a new timeout for request.
    /// </summary>
    /// <param name="ATimeout">
    ///   Request timeout.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function Timeout(const ATimeout: Integer): IRequest; overload;
    /// <summary>
    ///   Defines a DataSet that will be assigned to an adapter to respond to requests.
    /// </summary>
    /// <param name="ADataSet">
    ///   DataSet that will be adapted.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function DataSetAdapter(const ADataSet: TDataSet): IRequest; overload;
    /// <summary>
    ///   Get defined dataset adapter.
    /// </summary>
    /// <returns>
    ///   Dataset Adapter.
    /// </returns>
    function DataSetAdapter: TDataSet; overload;
    /// <summary>
    ///   Sets the base URL of access to resources to be consumed.
    /// </summary>
    /// <param name="ABaseURL">
    ///   Base URL for access to resources.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function BaseURL(const ABaseURL: string): IRequest; overload;
    /// <summary>
    ///   Get defined base URL.
    /// </summary>
    /// <returns>
    ///   Base URL.
    /// </returns>
    function BaseURL: string; overload;
    /// <summary>
    ///   Sets the resource to be consumed.
    /// </summary>
    /// <param name="AResource">
    ///   Resource name.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function Resource(const AResource: string): IRequest; overload;
    /// <summary>
    ///   Get defined value if the component needs to raise an exception on server status code return 500.
    /// </summary>
    /// <returns>
    ///   Defined raise exception on error 500.
    /// </returns>
    function RaiseExceptionOn500: Boolean; overload;
    /// <summary>
    ///   Defines if the component needs to raise an exception on server status code return 500.
    /// </summary>
    /// <param name="ARaiseException">
    ///   Raise exception.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function RaiseExceptionOn500(const ARaiseException: Boolean): IRequest; overload;
    /// <summary>
    ///   Get defined resource.
    /// </summary>
    /// <returns>
    ///   Resource.
    /// </returns>
    function Resource: string; overload;
    /// <summary>
    ///   Sets the suffix of the resource to be consumed.
    /// </summary>
    /// <param name="AResourceSufix">
    ///   Resource Suffix.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function ResourceSuffix(const AResourceSuffix: string): IRequest; overload;
    /// <summary>
    ///   Get defined resource suffix.
    /// </summary>
    /// <returns>
    ///   Resource suffix.
    /// </returns>
    function ResourceSuffix: string; overload;
    /// <summary>
    ///   Defines an HTTP verb for the request.
    /// </summary>
    /// <param name="AMethod">
    ///   HTTP method.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    /// <remarks>
    ///   Uses REST.Types.
    /// </remarks>
    function Method(const AMethod: TRESTRequestMethod): IRequest; overload;
    /// <summary>
    ///   Get defined method.
    /// </summary>
    /// <returns>
    ///   HTTP method.
    /// </returns>
    /// <remarks>
    ///   Uses REST.Types.
    /// </remarks>
    function Method: TRESTRequestMethod; overload;
    /// <summary>
    ///   Defines an token to the request.
    /// </summary>
    /// <param name="AToken">
    ///   The token value.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function Token(const AToken: string): IRequest;
    /// <summary>
    ///   Calls the Execute function, using the GET method.
    /// </summary>
    /// <returns>
    ///   Returns an instance of the request response interface.
    /// </returns>
    function Get: IRequestResponse;
    /// <summary>
    ///   Calls the Execute function, using the POST method.
    /// </summary>
    /// <returns>
    ///   Returns an instance of the request response interface.
    /// </returns>
    function Post: IRequestResponse;
    /// <summary>
    ///   Calls the Execute function, using the PUT method.
    /// </summary>
    /// <returns>
    ///   Returns an instance of the request response interface.
    /// </returns>
    function Put: IRequestResponse;
    /// <summary>
    ///   Calls the Execute function, using the DELETE method.
    /// </summary>
    /// <returns>
    ///   Returns an instance of the request response interface.
    /// </returns>
    function Delete: IRequestResponse;
    /// <summary>
    ///   Get the full URL.
    /// </summary>
    /// <param name="AIncludeParams">
    ///   Include Added Parameters.
    /// </param>
    /// <returns>
    ///   Full URL.
    /// </returns>
    function FullRequestURL(const AIncludeParams: Boolean = True): string;
    /// <summary>
    ///   Execute the request.
    /// </summary>
    /// <returns>
    ///   Returns the status code of the request.
    /// </returns>
    /// <remarks>
    ///   See more about status code in: https://httpstatuses.com/
    /// </remarks>
    function Execute: Integer;
    /// <summary>
    ///   <para>
    ///     Executes a request asynchronously, i.e. run it in its own thread. There is no automatic serialization o
    ///     property access though, which means that while the execution thread runs, properties of all involved
    ///     TCustomRESTClient and TCustomRESTRequest instances should not be touched from other threads (including the main thread)
    ///     <br/><br/>Using ExecuteAsync is strongly recommended on mobile platforms. iOS (and likely Android) will
    ///     terminate an application if it considers the main thread to be unresponsive, which would be the case if
    ///     there is a running request which takes more than a second or two to return.
    ///   </para>
    ///   <para>
    ///     The idea behind this is that the UI runs in the main thread and mobile devices should respond to user
    ///     interaction basically immediately. Sluggish behaviour (caused by blocking the main thread) is considered
    ///     unacceptable on these small devices.
    ///   </para>
    /// </summary>
    /// <param name="ACompletionHandler">
    ///   An anonymous method that will be run after the execution completed
    /// </param>
    /// <param name="ASynchronized">
    ///   Specifies if ACompletioHandler will be run in the main thread's (True) or execution thread's (False) context
    /// </param>
    /// <param name="AFreeThread">
    ///   If True, then the execution thread will be freed after it completed
    /// </param>
    /// <param name="ACompletionHandlerWithError">
    ///   An anonymous method that will be run if an exception is raised during execution
    /// </param>
    /// <returns>
    ///   Returns a reference to the execution thread. Should only be used if AFreeThread=False, as other wise the
    ///   reference may get invalid unexpectedly.
    /// </returns>
    function ExecuteAsync(ACompletionHandler: TProc = nil; ASynchronized: Boolean = True; AFreeThread: Boolean = True;
      ACompletionHandlerWithError: TProc<TObject> = nil): TRESTExecutionThread;
    /// <summary>
    ///   Removes all content added in the request body.
    /// </summary>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function ClearBody: IRequest;
    /// <summary>
    ///   Adds content to the body of the request.
    /// </summary>
    /// <param name="AContent">
    ///   Content to be added.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function AddBody(const AContent: string; const AContentType: TRESTContentType = ctAPPLICATION_JSON): IRequest; overload;
    /// <summary>
    ///   Adds content to the body of the request.
    /// </summary>
    /// <param name="AContent">
    ///   Content to be added in JSON format.
    /// </param>
    /// <param name="AOwns">
    ///   Indicates who owns JSON.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function AddBody(const AContent: TJSONObject; const AOwns: Boolean = True): IRequest; overload;
    /// <summary>
    ///   Adds content to the body of the request.
    /// </summary>
    /// <param name="AContent">
    ///   Content to be added in JSON array format.
    /// </param>
    /// <param name="AOwns">
    ///   Indicates who owns JSON.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function AddBody(const AContent: TJSONArray; const AOwns: Boolean = True): IRequest; overload;
    /// <summary>
    ///   Adds content to the body of the request.
    /// </summary>
    /// <param name="AContent">
    ///   Object that must be serialized.
    /// </param>
    /// <param name="AOwns">
    ///   Indicates who owns object.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function AddBody(const AContent: TObject; const AOwns: Boolean = True): IRequest; overload;
    /// <summary>
    ///   Removes all added headers.
    /// </summary>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function ClearHeaders: IRequest;
    /// <summary>
    ///   Adds a new header.
    /// </summary>
    /// <param name="AName">
    ///   Name of header.
    /// </param>
    /// <param name="AValue">
    ///   Header value.
    /// </param>
    /// <param name="AOptions">
    ///   Additional options for the header.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    /// <remarks>
    ///   If the parameter already exists, its value will change.
    /// </remarks>
    function AddHeader(const AName, AValue: string; const AOptions: TRESTRequestParameterOptions = []): IRequest;
    /// <summary>
    ///   Allows access to the request parameters.
    /// </summary>
    /// <returns>
    ///   Returns an instance of the request parameter interface.
    /// </returns>
    function Params: IRequestParams;
    /// <summary>
    ///   Set credentials to basic authentication.
    /// </summary>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function BasicAuthentication(const AUsername, APassword: string): IRequest;
  end;

implementation

end.
