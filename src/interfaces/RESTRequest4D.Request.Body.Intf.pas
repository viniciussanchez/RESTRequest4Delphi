unit RESTRequest4D.Request.Body.Intf;

interface

uses System.JSON, REST.Types;

type
  /// <summary>
  ///   Interface to represent the body of a request.
  /// </summary>
  IRequestBody = interface
    ['{D0D60888-6E42-4718-8E2D-1F6A9F10445A}']
    /// <summary>
    ///   Removes all content added in the request body.
    /// </summary>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function Clear: IRequestBody;
    /// <summary>
    ///   Adds content to the body of the request.
    /// </summary>
    /// <param name="AContent">
    ///   Content to be added.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function Add(const AContent: string; const AContentType: TRESTContentType = ctAPPLICATION_JSON): IRequestBody; overload;
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
    function Add(const AContent: TJSONObject; const AOwns: Boolean = True): IRequestBody; overload;
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
    function Add(const AContent: TJSONArray; const AOwns: Boolean = True): IRequestBody; overload;
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
    function Add(const AContent: TObject; const AOwns: Boolean = True): IRequestBody; overload;
  end;

implementation

end.
