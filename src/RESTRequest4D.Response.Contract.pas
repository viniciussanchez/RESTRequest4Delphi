unit RESTRequest4D.Response.Contract;

interface

uses
  {$IFDEF FPC}
    SysUtils, Classes, fpjson
  {$ELSE}
    System.SysUtils, System.JSON,System.Classes
  {$ENDIF}
  ;

type
  IResponse = interface
    ['{A3BB1797-E99E-4C72-8C4A-925825A50C27}']
    function Content: string;
    function ContentLength: Cardinal;
    function ContentType: string;
    function ContentEncoding: string;
    function StatusCode: Integer;
    function RawBytes: TBytes;
    function JSONValue: {$IFDEF FPC} TJSONData {$ELSE} TJSONValue {$ENDIF};
    function Headers: TStrings;
  end;

implementation

end.
