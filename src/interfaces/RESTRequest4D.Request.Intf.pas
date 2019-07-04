unit RESTRequest4D.Request.Intf;

interface

uses Data.DB, REST.Client, RESTRequest4D.Request.Body.Intf, RESTRequest4D.Request.Params.Intf, REST.Types,
  RESTRequest4D.Request.Authentication.Intf;

type
  /// <summary>
  ///   Interface to represent a request to a given server.
  /// </summary>
  IRequest = interface
    ['{2C882459-F4C3-4854-8F7A-F68E8F8DE98E}']
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
    ///   Defines an HTTP verb for the request.
    /// </summary>
    /// <param name="AMethod">
    ///   HTTP method.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function SetMethod(const AMethod: TRESTRequestMethod = rmGET): IRequest;
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
    ///   Get defined method.
    /// </summary>
    /// <returns>
    ///   HTTP method.
    /// </returns>
    function GetMethod: TRESTRequestMethod;
    /// <summary>
    ///   Get defined resource suffix.
    /// </summary>
    /// <returns>
    ///   Resource suffix.
    /// </returns>
    function GetResourceSuffix: string;
    /// <summary>
    ///   Get defined resource.
    /// </summary>
    /// <returns>
    ///   Resource.
    /// </returns>
    function GetResource: string;
    /// <summary>
    ///   Get defined base URL.
    /// </summary>
    /// <returns>
    ///   Base URL.
    /// </returns>
    function GetBaseURL: string;
    /// <summary>
    ///   Get defined dataset adapter.
    /// </summary>
    /// <returns>
    ///   Dataset Adapter.
    /// </returns>
    function GetDataSetAdapter: TDataSet;
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
    ///   Allows access to the request body.
    /// </summary>
    /// <returns>
    ///   Returns an instance of the request body interface.
    /// </returns>
    function Body: IRequestBody;
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
