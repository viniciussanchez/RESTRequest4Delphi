unit RESTRequest4D.Request.Authentication.Intf;

interface

type
  /// <summary>
  ///   Interface that represents the authentication of the request.
  /// </summary>
  IRequestAuthentication = interface
    ['{872B5C31-1FD3-4852-9181-CDAB194F9C38}']
    /// <summary>
    ///   Removes authentication data.
    /// </summary>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function Clear: IRequestAuthentication;
    /// <summary>
    ///   Sets the authentication password.
    /// </summary>
    /// <param name="APassword">
    ///   Password to authenticate the request.
    /// </param>
    /// <returns>
    ///   Returns the instance itself following the fluent API pattern.
    /// </returns>
    function SetPassword(const APassword: string): IRequestAuthentication;
    /// <summary>
    ///   Returns the password set for authentication.
    /// </summary>
    /// <returns>
    ///   Password set.
    /// </returns>
    function GetPassword: string;
    /// <summary>
    ///   Sets the authentication user.
    /// </summary>
    /// <param name="AUser">
    ///   User to authenticate the request.
    /// </param>
    /// <returns>
    ///   Retorna a própria instância seguindo o padrão fluent API.
    /// </returns>
    function SetUsername(const AUser: string): IRequestAuthentication;
    /// <summary>
    ///   Returns the user set for authentication.
    /// </summary>
    /// <returns>
    ///   User defined.
    /// </returns>
    function GetUsername: string;
  end;

implementation

end.
