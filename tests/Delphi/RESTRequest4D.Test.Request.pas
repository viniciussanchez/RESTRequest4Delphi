unit RESTRequest4D.Test.Request;

interface

uses
  DUnitX.TestFramework, System.SysUtils;

type
  [TestFixture]
  TRESTRequest4DTest = class
  public
    [Test]
    procedure TestFluidLifecycleClient;
  end;

implementation

uses
  RESTRequest4D;

procedure TRESTRequest4DTest.TestFluidLifecycleClient;
var
  LResponse: IResponse;
begin
  // Testa chamada fluida onde o request sai de escopo imediatamente após o Get,
  // mas a resposta precisa continuar acessível sem Access Violation.
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('get')
    .Get;

  Assert.IsNotNull(LResponse, 'A resposta não deve ser nula');
  Assert.AreEqual(200, LResponse.StatusCode, 'StatusCode deve ser 200');
  Assert.IsNotEmpty(LResponse.Content, 'Content não deve ser vazio');
end;

initialization
  TDUnitX.RegisterTestFixture(TRESTRequest4DTest);

end.
