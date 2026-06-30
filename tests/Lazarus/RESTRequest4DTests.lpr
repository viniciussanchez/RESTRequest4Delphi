program RESTRequest4DTests;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, consoletestrunner, RESTRequest4D.Test.Request;

var
  Application: TTestRunner;
begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'RESTRequest4Delphi Tests';
  Application.Run;
  Application.Free;
end.
