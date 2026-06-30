unit RESTRequest4D.Test.Request;

{$mode delphi}

interface

uses
  fpcunit, testregistry, SysUtils, Classes, fpjson, jsonparser, syncobjs, RESTRequest4D, RESTRequest4D.Utils;

type
  TRESTRequest4DTest = class(TTestCase)
  published
    procedure TestFluidLifecycle;
    procedure TestGetRequest;
    procedure TestPostRequestWithJson;
    procedure TestPutRequest;
    procedure TestDeleteRequest;
    procedure TestPatchRequest;
    procedure TestAddHeaders;
    procedure TestAddParams;
    procedure TestAddUrlSegment;
    procedure TestRaiseExceptionOn500;
    procedure TestGetRequestAsync;
    procedure TestTokenBearerAuthentication;
    procedure TestCookies;
    procedure TestMultipartFormData;
    procedure TestAddBodyString;
    procedure TestBenchmarkContentStream;
  end;

implementation

var
  GAsyncResponse: IResponse;
  GAsyncEvent: TSimpleEvent;

procedure AsyncCallback(const Req: IRequest; const Res: IResponse);
begin
  GAsyncResponse := Res;
  GAsyncEvent.SetEvent;
end;

{ TRESTRequest4DTest }

procedure TRESTRequest4DTest.TestFluidLifecycle;
var
  LResponse: IResponse;
begin
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('get')
    .Get;

  AssertNotNull('A resposta não deve ser nula', LResponse);
  AssertEquals('StatusCode deve ser 200', 200, LResponse.StatusCode);
  AssertTrue('Content não deve ser vazio', LResponse.Content <> '');
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

  AssertEquals(200, LResponse.StatusCode);
  AssertTrue(LResponse.Content <> '');
  
  LJson := LResponse.JSONValue as TJSONObject;
  AssertNotNull('JSON retornado não deve ser nulo', LJson);
  AssertNotNull('JSON deve conter campo URL', LJson.Find('url'));
end;

procedure TRESTRequest4DTest.TestPostRequestWithJson;
var
  LResponse: IResponse;
  LBody: TJSONObject;
  LJson: TJSONObject;
  LData: TJSONObject;
begin
  LBody := TJSONObject.Create;
  LBody.Add('nome', 'RESTRequest4Delphi');
  LBody.Add('status', 'OK');

  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('post')
    .AddBody(LBody)
    .Post;

  AssertEquals(200, LResponse.StatusCode);
  AssertTrue(LResponse.Content <> '');

  LJson := LResponse.JSONValue as TJSONObject;
  AssertNotNull(LJson);
  
  LData := LJson.Find('json') as TJSONObject;
  AssertNotNull('JSON enviado deve estar no campo json da resposta', LData);
  AssertEquals('RESTRequest4Delphi', LData.Strings['nome']);
end;

procedure TRESTRequest4DTest.TestPutRequest;
var
  LResponse: IResponse;
  LBody: TJSONObject;
  LJson: TJSONObject;
  LData: TJSONObject;
begin
  LBody := TJSONObject.Create;
  LBody.Add('param_put', 'valor_put');

  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('put')
    .AddBody(LBody)
    .Put;

  AssertEquals(200, LResponse.StatusCode);
  LJson := LResponse.JSONValue as TJSONObject;
  LData := LJson.Find('json') as TJSONObject;
  AssertNotNull(LData);
  AssertEquals('valor_put', LData.Strings['param_put']);
end;

procedure TRESTRequest4DTest.TestDeleteRequest;
var
  LResponse: IResponse;
begin
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('delete')
    .Delete;

  AssertEquals(200, LResponse.StatusCode);
end;

procedure TRESTRequest4DTest.TestPatchRequest;
var
  LResponse: IResponse;
  LBody: TJSONObject;
  LJson: TJSONObject;
  LData: TJSONObject;
begin
  LBody := TJSONObject.Create;
  LBody.Add('param_patch', 'valor_patch');

  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('patch')
    .AddBody(LBody)
    .Patch;

  AssertEquals(200, LResponse.StatusCode);
  LJson := LResponse.JSONValue as TJSONObject;
  LData := LJson.Find('json') as TJSONObject;
  AssertNotNull(LData);
  AssertEquals('valor_patch', LData.Strings['param_patch']);
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

  AssertEquals(200, LResponse.StatusCode);
  
  LJson := LResponse.JSONValue as TJSONObject;
  LHeaders := LJson.Find('headers') as TJSONObject;
  AssertNotNull(LHeaders);
  AssertEquals('RESTRequest4Delphi', LHeaders.Strings['X-Test-Library']);
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

  AssertEquals(200, LResponse.StatusCode);
  
  LJson := LResponse.JSONValue as TJSONObject;
  LArgs := LJson.Find('args') as TJSONObject;
  AssertNotNull(LArgs);
  AssertEquals('delphi', LArgs.Strings['search']);
  AssertEquals('1', LArgs.Strings['page']);
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

  AssertEquals(200, LResponse.StatusCode);
  
  LJson := LResponse.JSONValue as TJSONObject;
  AssertTrue('URL final deve conter o segmento id substituído', Pos('/anything/123', LJson.Strings['url']) > 0);
end;

procedure TRESTRequest4DTest.TestRaiseExceptionOn500;
var
  LRequest: IRequest;
  LSuccess: Boolean;
  LResponse: IResponse;
begin
  LRequest := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('status/500')
    .RaiseExceptionOn500(True);

  LSuccess := False;
  try
    LResponse := LRequest.Get;
    Fail('Não disparou exceção. Status retornado: ' + IntToStr(LResponse.StatusCode));
  except
    on E: Exception do
      LSuccess := True;
  end;
  AssertTrue('Deveria ter disparado uma exceção para o erro 500', LSuccess);
end;

procedure TRESTRequest4DTest.TestGetRequestAsync;
var
  LRequest: IRequest;
  LTimeout: Cardinal;
begin
  GAsyncEvent := TSimpleEvent.Create;
  try
    GAsyncResponse := nil;
    LRequest := TRequest.New
      .BaseURL('https://httpbin.org')
      .Resource('get');

    LRequest.GetAsync(AsyncCallback);

    LTimeout := 0;
    while (GAsyncEvent.WaitFor(50) = wrTimeout) and (LTimeout < 10000) do
    begin
      CheckSynchronize(10);
      Inc(LTimeout, 60);
    end;

    AssertNotNull('A resposta do callback assíncrono não deve ser nula. Erro interno: ' + TAsyncRequestThread.LastError, GAsyncResponse);
    AssertEquals('StatusCode deve ser 200', 200, GAsyncResponse.StatusCode);
    AssertTrue('Content não deve ser vazio', GAsyncResponse.Content <> '');
  finally
    GAsyncEvent.Free;
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

  AssertEquals(200, LResponse.StatusCode);
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

  AssertEquals(200, LResponse.StatusCode);
  LJson := LResponse.JSONValue as TJSONObject;
  LCookies := LJson.Find('cookies') as TJSONObject;
  AssertNotNull(LCookies);
  AssertEquals('v1', LCookies.Strings['c1']);
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

  AssertEquals(200, LResponse.StatusCode);
  LJson := LResponse.JSONValue as TJSONObject;
  AssertNotNull('JSON retornado não deve ser nulo. Conteúdo: ' + LResponse.Content, LJson);
  LForm := LJson.Find('form') as TJSONObject;
  AssertNotNull(LForm);
  AssertNotNull('campo1 nao encontrado no form. JSON: ' + LResponse.Content, LForm.Find('campo1'));
  AssertEquals('valor1', LForm.Strings['campo1']);
  AssertNotNull('campo2 nao encontrado no form. JSON: ' + LResponse.Content, LForm.Find('campo2'));
  AssertEquals('valor2', LForm.Strings['campo2']);
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

  AssertEquals(200, LResponse.StatusCode);
  LJson := LResponse.JSONValue as TJSONObject;
  LData := LJson.Strings['data'];
  AssertEquals('Texto Bruto de Teste', LData);
end;

procedure TRESTRequest4DTest.TestBenchmarkContentStream;
var
  LResponse: IResponse;
  I: Integer;
  LStart: Cardinal;
  LStream: TStream;
  LTime: Cardinal;
  LFile: TStringList;
begin
  LResponse := TRequest.New
    .BaseURL('https://httpbin.org')
    .Resource('bytes/50000')
    .Get;

  AssertEquals(200, LResponse.StatusCode);

  LStart := TThread.GetTickCount;
  for I := 1 to 20000 do
  begin
    LStream := LResponse.ContentStream;
    if LStream.Size <> 50000 then
      Fail('Tamanho incorreto do stream');
  end;
  LTime := TThread.GetTickCount - LStart;

  LFile := TStringList.Create;
  try
    LFile.Add('Tempo gasto para 20.000 acessos ao ContentStream: ' + IntToStr(LTime) + ' ms');
    LFile.SaveToFile('benchmark.txt');
  finally
    LFile.Free;
  end;
end;

initialization
  RegisterTest(TRESTRequest4DTest);

end.
