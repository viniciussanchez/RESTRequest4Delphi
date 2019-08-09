unit RESTRequest4D.Request.Intf;

interface

uses Data.DB, REST.Client, RESTRequest4D.Request.Body.Intf, RESTRequest4D.Request.Params.Intf, REST.Types, System.SysUtils,
  RESTRequest4D.Request.Authentication.Intf, RESTRequest4D.Request.Headers.Intf;

type
  /// <summary>
  ///   Interface to represent a request to a given server.
  /// </summary>
  IRequest = interface
    ['{2C882459-F4C3-4854-8F7A-F68E8F8DE98E}']
    /// <summary>
    ///   Get defined request timeout.
    /// </summary>
    /// <returns>
    ///   Request timeout.
    /// </returns>
    function GetTimeout: Integer;
    /// <summary>
    ///   Defines a new timeout for request.
    /// </summary>
    /// <param name="ATimeout">
    ///   Request timeout.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function SetTimeout(const ATimeout: Integer): IRequest;
    /// <summary>
    ///   Defines a DataSet that will be assigned to an adapter to respond to requests.
    /// </summary>
    /// <param name="ADataSet">
    ///   DataSet that will be adapted.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function SetDataSetAdapter(const ADataSet: TDataSet): IRequest;
    /// <summary>
    ///   Get defined dataset adapter.
    /// </summary>
    /// <returns>
    ///   Dataset Adapter.
    /// </returns>
    function GetDataSetAdapter: TDataSet;
    /// <summary>
    ///   Sets the base URL of access to resources to be consumed.
    /// </summary>
    /// <param name="ABaseURL">
    ///   Base URL for access to resources.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function SetBaseURL(const ABaseURL: string = ''): IRequest;
    /// <summary>
    ///   Get defined base URL.
    /// </summary>
    /// <returns>
    ///   Base URL.
    /// </returns>
    function GetBaseURL: string;
    /// <summary>
    ///   Sets the resource to be consumed.
    /// </summary>
    /// <param name="AResource">
    ///   Resource name.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function SetResource(const AResource: string = ''): IRequest;
    /// <summary>
    ///   Get defined resource.
    /// </summary>
    /// <returns>
    ///   Resource.
    /// </returns>
    function GetResource: string;
    /// <summary>
    ///   Sets the suffix of the resource to be consumed.
    /// </summary>
    /// <param name="AResourceSufix">
    ///   Resource Suffix.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function SetResourceSuffix(const AResourceSuffix: string = ''): IRequest;
    /// <summary>
    ///   Get defined resource suffix.
    /// </summary>
    /// <returns>
    ///   Resource suffix.
    /// </returns>
    function GetResourceSuffix: string;
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
    function SetMethod(const AMethod: TRESTRequestMethod = rmGET): IRequest;
    /// <summary>
    ///   Get defined method.
    /// </summary>
    /// <returns>
    ///   HTTP method.
    /// </returns>
    /// <remarks>
    ///   Uses REST.Types.
    /// </remarks>
    function GetMethod: TRESTRequestMethod;
    /// <summary>
    ///   Get the full URL.
    /// </summary>
    /// <param name="AIncludeParams">
    ///   Include Added Parameters.
    /// </param>
    /// <returns>
    ///   Full URL.
    /// </returns>
    function GetFullRequestURL(const AIncludeParams: Boolean = True): string;
    /// <summary>
    ///   Get HTTP response status code.
    /// </summary>
    /// <returns>
    ///   Status code.
    /// </returns>
    function GetStatusCode: Integer;
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
    ///   Allows access to the request body.
    /// </summary>
    /// <returns>
    ///   Returns an instance of the request body interface.
    /// </returns>
    function Body: IRequestBody;
    /// <summary>
    ///   Allows access to the request headers.
    /// </summary>
    /// <returns>
    ///   Returns an instance of the request parameter interface.
    /// </returns>
    function Headers: IRequestHeaders;
    /// <summary>
    ///   Allows access to the request parameters.
    /// </summary>
    /// <returns>
    ///   Returns an instance of the request parameter interface.
    /// </returns>
    function Params: IRequestParams;
    /// <summary>
    ///   Allows access to the authentication of the request.
    /// </summary>
    /// <returns>
    ///   Returns an instance of the authentication interface.
    /// </returns>
    function Authentication: IRequestAuthentication;
  end;

implementation

end.
