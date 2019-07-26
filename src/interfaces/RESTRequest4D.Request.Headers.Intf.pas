unit RESTRequest4D.Request.Headers.Intf;

interface

uses REST.Types;

type
  /// <summary>
  ///   Interface to represent the headers that a request can have.
  /// </summary>
  IRequestHeaders = interface
    ['{619A1A04-A54E-4684-914B-D5AB1EA867A3}']
    /// <summary>
    ///   Removes all added headers.
    /// </summary>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function Clear: IRequestHeaders;
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
    function Add(const AName, AValue: string; const AOptions: TRESTRequestParameterOptions = []): IRequestHeaders;
  end;

implementation

end.
