unit RESTRequest4D.Test.Request;

interface

uses
  DUnitX.TestFramework, System.SysUtils, System.Classes, System.JSON, System.SyncObjs, RESTRequest4D, RESTRequest4D.Utils;

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
    procedure TestPutRequest;
    
    [Test]
    procedure TestDeleteRequest;
    
    [Test]
    procedure TestPatchRequest;
    
    [Test]
    procedure TestAddHeaders;
    
    [Test]
    procedure TestAddParams;
    
    [Test]
    procedure TestAddUrlSegment;
    
    [Test]
    procedure TestRaiseExceptionOn500;
    
    [Test]
    procedure TestGetRequestAsync;
    
    [Test]
    procedure TestTokenBearerAuthentication;
    
    [Test]
    procedure TestCookies;
    
    [Test]
    procedure TestMultipartFormData;
    
    [Test]
    procedure TestAddBodyString;
  end;

implementation

{ TRESTRequest4DTest }

procedure TRESTRequest4DTest.TestFluidLifecycle;
var
  LResponse: IResponse;
begin
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

procedure TRESTRequest4DTest.TestPutRequest;
var
  LResponse: IResponse;
  LBody: TJSONObject;
  LJson: TJSONObject;
  LData: TJSONObject;
begin
  LBody := TJSONObject.Create;
  LBody.AddPair('param_put', 'valor_put');

  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('put')
    .AddBody(LBody)
    .Put;

  Assert.AreEqual(200, LResponse.StatusCode);
  LJson := LResponse.JSONValue as TJSONObject;
  LData := LJson.GetValue('json') as TJSONObject;
  Assert.IsNotNull(LData);
  Assert.AreEqual('valor_put', LData.GetValue('param_put').Value);
end;

procedure TRESTRequest4DTest.TestDeleteRequest;
var
  LResponse: IResponse;
begin
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('delete')
    .Delete;

  Assert.AreEqual(200, LResponse.StatusCode);
end;

procedure TRESTRequest4DTest.TestPatchRequest;
var
  LResponse: IResponse;
  LBody: TJSONObject;
  LJson: TJSONObject;
  LData: TJSONObject;
begin
  LBody := TJSONObject.Create;
  LBody.AddPair('param_patch', 'valor_patch');

  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('patch')
    .AddBody(LBody)
    .Patch;

  Assert.AreEqual(200, LResponse.StatusCode);
  LJson := LResponse.JSONValue as TJSONObject;
  LData := LJson.GetValue('json') as TJSONObject;
  Assert.IsNotNull(LData);
  Assert.AreEqual('valor_patch', LData.GetValue('param_patch').Value);
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
  LSuccess: Boolean;
begin
  LRequest := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('status/500')
    .RaiseExceptionOn500(True);

  LSuccess := False;
  try
    LRequest.Get;
  except
    on E: Exception do
      LSuccess := True;
  end;
  Assert.IsTrue(LSuccess, 'Deveria ter disparado uma exceção para o erro 500');
end;

procedure TRESTRequest4DTest.TestGetRequestAsync;
var
  LEvent: TSimpleEvent;
  LRequest: IRequest;
  LResponse: IResponse;
  LTimeout: Cardinal;
begin
  LEvent := TSimpleEvent.Create;
  try
    LResponse := nil;
    LRequest := TRequest.New
      .BaseURL('https://httpbin.org')
      .Resource('get');

    {$IF NOT (DEFINED(RR4D_INDY) or DEFINED(FPC) or DEFINED(RR4D_SYNAPSE) or DEFINED(RR4D_ICS))}
    LRequest.SynchronizedEvents(False);
    {$ENDIF}

    LRequest.GetAsync(
      procedure(const Req: IRequest; const Res: IResponse)
      begin
        LResponse := Res;
        LEvent.SetEvent;
      end
    );

    LTimeout := 0;
    while (LEvent.WaitFor(50) = wrTimeout) and (LTimeout < 10000) do
    begin
      CheckSynchronize(10);
      Inc(LTimeout, 60);
    end;

    if not Assigned(LResponse) then
      Assert.Fail('A resposta do callback assíncrono não deve ser nula. Erro interno: ' + TAsyncRequestThread.LastError);
    if Assigned(LResponse) then
    begin
      Assert.AreEqual(200, LResponse.StatusCode, 'StatusCode deve ser 200');
      Assert.IsNotEmpty(LResponse.Content, 'Content não deve ser vazio');
    end;
  finally
    LEvent.Free;
  end;
end;

procedure TRESTRequest4DTest.TestTokenBearerAuthentication;
var
  LResponse: IResponse;
begin
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('bearer')
    .TokenBearer('token123')
    .Get;

  Assert.AreEqual(200, LResponse.StatusCode);
end;

procedure TRESTRequest4DTest.TestCookies;
var
  LResponse: IResponse;
  LJson: TJSONObject;
  LCookies: TJSONObject;
begin
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('cookies')
    .AddCookie('c1', 'v1')
    .Get;

  Assert.AreEqual(200, LResponse.StatusCode);
  LJson := LResponse.JSONValue as TJSONObject;
  LCookies := LJson.GetValue('cookies') as TJSONObject;
  Assert.IsNotNull(LCookies);
  Assert.AreEqual('v1', LCookies.GetValue('c1').Value);
end;

procedure TRESTRequest4DTest.TestMultipartFormData;
var
  LResponse: IResponse;
  LJson: TJSONObject;
  LForm: TJSONObject;
begin
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('post')
    .AddField('campo1', 'valor1')
    .AddField('campo2', 'valor2')
    .Post;

  Assert.AreEqual(200, LResponse.StatusCode);
  LJson := LResponse.JSONValue as TJSONObject;
  LForm := LJson.GetValue('form') as TJSONObject;
  Assert.IsNotNull(LForm);
  Assert.AreEqual('valor1', LForm.GetValue('campo1').Value);
  Assert.AreEqual('valor2', LForm.GetValue('campo2').Value);
end;

procedure TRESTRequest4DTest.TestAddBodyString;
var
  LResponse: IResponse;
  LJson: TJSONObject;
  LData: string;
begin
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('post')
    .AddBody('Texto Bruto de Teste')
    .Post;

  Assert.AreEqual(200, LResponse.StatusCode);
  LJson := LResponse.JSONValue as TJSONObject;
  LData := LJson.GetValue('data').Value;
  Assert.AreEqual('Texto Bruto de Teste', LData);
end;

initialization
  TDUnitX.RegisterTestFixture(TRESTRequest4DTest);

end.
