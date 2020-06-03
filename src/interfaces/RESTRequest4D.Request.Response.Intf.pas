unit RESTRequest4D.Request.Response.Intf;

interface

uses System.SysUtils, System.JSON;

type
  /// <summary>
  ///   Interface representing the request response.
  /// </summary>
  IRequestResponse = interface
    ['{A3BB1797-E99E-4C72-8C4A-925825A50C27}']
    /// <summary>
    ///   Get response content.
    /// </summary>
    /// <returns>
    ///   Returns the content of the request.
    /// </returns>
    function Content: string;
    /// <summary>
    ///   Get response content length.
    /// </summary>
    /// <returns>
    ///   Returns the content length of the request.
    /// </returns>
    function ContentLength: Cardinal;
    /// <summary>
    ///   Get response content type.
    /// </summary>
    /// <returns>
    ///   Returns the content type of the request.
    /// </returns>
    function ContentType: string;
    /// <summary>
    ///   Get response content encoding.
    /// </summary>
    /// <returns>
    ///   Returns the content encoding of the request.
    /// </returns>
    function ContentEncoding: string;
    /// <summary>
    ///   Get HTTP response status code.
    /// </summary>
    /// <returns>
    ///   Status code.
    /// </returns>
    function StatusCode: Integer;
    /// <summary>
    ///   Get response raw bytes.
    /// </summary>
    /// <returns>
    ///   Returns TBytes of the response.
    /// </returns>
    function RawBytes: TBytes;
    /// <summary>
    ///   Get response JSON value.
    /// </summary>
    /// <returns>
    ///   Returns TJSONValue of the response.
    /// </returns>
    function JSONValue: TJSONValue;
  end;

implementation

end.
