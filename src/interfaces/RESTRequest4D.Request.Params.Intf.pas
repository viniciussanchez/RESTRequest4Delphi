unit RESTRequest4D.Request.Params.Intf;

interface

uses REST.Types, System.Classes;

type
  /// <summary>
  ///   Interface to represent the parameters that a request can have.
  /// </summary>
  IRequestParams = interface
    ['{CB53222E-B9FD-4DF1-BDF6-4E0EA6E462A5}']
    /// <summary>
    ///   Removes all added parameters.
    /// </summary>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function Clear: IRequestParams;
    /// <summary>
    ///   Adds a new parameter.
    /// </summary>
    /// <param name="AName">
    ///   Name of the parameter.
    /// </param>
    /// <param name="AValue">
    ///   Parameter value.
    /// </param>
    /// <param name="AKind">
    ///   Type of parameter.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    /// <remarks>
    ///   If the parameter already exists, its value will change.
    /// </remarks>
    function Add(const AName, AValue: string; const AKind: TRESTRequestParameterKind = TRESTRequestParameterKind.pkQUERY): IRequestParams; overload;
    /// <summary>
    ///   Adds a new parameter.
    /// </summary>
    /// <param name="AName">
    ///   Name of the parameter.
    /// </param>
    /// <param name="AValue">
    ///   Parameter value.
    /// </param>
    /// <param name="AKind">
    ///   Type of parameter.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    /// <remarks>
    ///   If the parameter already exists, its value will change.
    /// </remarks>
    function Add(const AName: string; const AValue: Currency; const AKind: TRESTRequestParameterKind = TRESTRequestParameterKind.pkQUERY): IRequestParams; overload;
    /// <summary>
    ///   Adds a new parameter.
    /// </summary>
    /// <param name="AName">
    ///   Name of the parameter.
    /// </param>
    /// <param name="AValue">
    ///   Parameter value.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function Add(const AName: string; const AValue: TStream): IRequestParams; overload;
  end;

implementation

end.
