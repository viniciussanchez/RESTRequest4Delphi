unit RESTRequest4D.Response.Intf;

interface

uses System.SysUtils, System.JSON,System.Classes;

type
  IResponse = interface
    ['{A3BB1797-E99E-4C72-8C4A-925825A50C27}']
    function Content: string;
    function ContentLength: Cardinal;
    function ContentType: string;
    function ContentEncoding: string;
    function StatusCode: Integer;
    function RawBytes: TBytes;
    function JSONValue: TJSONValue;
    function Headers :TStrings;
  end;

implementation

end.
