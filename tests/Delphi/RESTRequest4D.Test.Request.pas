unit RESTRequest4D.Test.Request;

interface

uses
  DUnitX.TestFramework, System.SysUtils, System.Classes, System.JSON, RESTRequest4D;

type
  [TestFixture]
  TRESTRequest4DTest = class
  public
    [Test]
    procedure TestFluidLifecycle;
    
    [Test]
    procedure TestGetRequest;
    
    [Test]
    procedure TestPostRequestWithJson;
    
    [Test]
    procedure TestAddHeaders;
    
    [Test]
    procedure TestAddParams;
    
    [Test]
    procedure TestAddUrlSegment;
    
    [Test]
    procedure TestRaiseExceptionOn500;
  end;

implementation

{ TRESTRequest4DTest }

procedure TRESTRequest4DTest.TestFluidLifecycle;
var
  LResponse: IResponse;
begin
  // Simula a fluidez onde a interface IRequest temporária é destruída
  // logo após o Get. O IResponse deve continuar acessível e sem Access Violation.
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('get')
    .Get;

  Assert.IsNotNull(LResponse, 'A resposta não deve ser nula');
  Assert.AreEqual(200, LResponse.StatusCode, 'StatusCode deve ser 200');
  Assert.IsNotEmpty(LResponse.Content, 'Content não deve ser vazio');
end;

procedure TRESTRequest4DTest.TestGetRequest;
var
  LResponse: IResponse;
  LJson: TJSONObject;
begin
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('get')
    .Get;

  Assert.AreEqual(200, LResponse.StatusCode);
  Assert.IsNotEmpty(LResponse.Content);
  
  LJson := LResponse.JSONValue as TJSONObject;
  Assert.IsNotNull(LJson, 'JSON retornado não deve ser nulo');
  Assert.IsNotNull(LJson.GetValue('url'), 'JSON deve conter campo URL');
end;

procedure TRESTRequest4DTest.TestPostRequestWithJson;
var
  LResponse: IResponse;
  LBody: TJSONObject;
  LJson: TJSONObject;
  LData: TJSONObject;
begin
  LBody := TJSONObject.Create;
  LBody.AddPair('nome', 'RESTRequest4Delphi');
  LBody.AddPair('status', 'OK');

  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('post')
    .AddBody(LBody)
    .Post;

  Assert.AreEqual(200, LResponse.StatusCode);
  Assert.IsNotEmpty(LResponse.Content);

  LJson := LResponse.JSONValue as TJSONObject;
  Assert.IsNotNull(LJson);
  
  LData := LJson.GetValue('json') as TJSONObject;
  Assert.IsNotNull(LData, 'JSON enviado deve estar no campo json da resposta');
  Assert.AreEqual('RESTRequest4Delphi', LData.GetValue('nome').Value);
end;

procedure TRESTRequest4DTest.TestAddHeaders;
var
  LResponse: IResponse;
  LJson: TJSONObject;
  LHeaders: TJSONObject;
begin
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('headers')
    .AddHeader('X-Test-Library', 'RESTRequest4Delphi')
    .Get;

  Assert.AreEqual(200, LResponse.StatusCode);
  
  LJson := LResponse.JSONValue as TJSONObject;
  LHeaders := LJson.GetValue('headers') as TJSONObject;
  Assert.IsNotNull(LHeaders);
  Assert.AreEqual('RESTRequest4Delphi', LHeaders.GetValue('X-Test-Library').Value);
end;

procedure TRESTRequest4DTest.TestAddParams;
var
  LResponse: IResponse;
  LJson: TJSONObject;
  LArgs: TJSONObject;
begin
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('get')
    .AddParam('search', 'delphi')
    .AddParam('page', '1')
    .Get;

  Assert.AreEqual(200, LResponse.StatusCode);
  
  LJson := LResponse.JSONValue as TJSONObject;
  LArgs := LJson.GetValue('args') as TJSONObject;
  Assert.IsNotNull(LArgs);
  Assert.AreEqual('delphi', LArgs.GetValue('search').Value);
  Assert.AreEqual('1', LArgs.GetValue('page').Value);
end;

procedure TRESTRequest4DTest.TestAddUrlSegment;
var
  LResponse: IResponse;
  LJson: TJSONObject;
begin
  // Substitui :id na URL final por 123
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('anything/:id')
    .AddUrlSegment('id', '123')
    .Get;

  Assert.AreEqual(200, LResponse.StatusCode);
  
  LJson := LResponse.JSONValue as TJSONObject;
  Assert.IsTrue(LJson.GetValue('url').Value.Contains('/anything/123'), 'URL final deve conter o segmento id substituído');
end;

procedure TRESTRequest4DTest.TestRaiseExceptionOn500;
var
  LRequest: IRequest;
begin
  LRequest := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('status/500')
    .RaiseExceptionOn500(True);

  Assert.WillRaise(
    procedure
    begin
      LRequest.Get;
    end,
    Exception
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TRESTRequest4DTest);

end.
