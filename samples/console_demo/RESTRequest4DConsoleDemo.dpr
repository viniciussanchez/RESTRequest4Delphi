program RESTRequest4DConsoleDemo;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.JSON,
  RESTRequest4D,
  RESTRequest4D.Demo.Telemetry in 'RESTRequest4D.Demo.Telemetry.pas';

var
  LBaseURLJSONPlaceholder: string = 'https://jsonplaceholder.typicode.com';
  LBaseURLHttpBin: string = 'https://httpbin.org';

procedure RunSequentialCRUDFlow;
var
  LBody: TJSONObject;
  LPostId: string;
begin
  WriteLn('Iniciando fluxo CRUD sequencial na API real JSONPlaceholder...');
  
  // 1. GET - Listar todos os posts filtrando por UserId = 1
  WriteLn('-> Listando posts do usuário 1 (GET)...');
  TTelemetry.Measure('Listar Posts (UserId=1)', 'GET', LBaseURLJSONPlaceholder + '/posts?userId=1',
    function: IResponse
    begin
      Result := TRequest.New
        .BaseURL(LBaseURLJSONPlaceholder)
        .Resource('posts')
        .AddParam('userId', '1')
        .Get;
    end
  );

  // 2. GET - Buscar um post específico usando substituição de segmento dinâmico na URL
  WriteLn('-> Obtendo o post ID 1 (GET + URL Segment)...');
  TTelemetry.Measure('Obter Post por ID (URL Segment)', 'GET', LBaseURLJSONPlaceholder + '/posts/1',
    function: IResponse
    begin
      Result := TRequest.New
        .BaseURL(LBaseURLJSONPlaceholder)
        .Resource('posts/:id')
        .AddUrlSegment('id', '1')
        .Get;
    end
  );

  // 3. POST - Criar um novo post enviando corpo em JSON e cabeçalho customizado
  WriteLn('-> Criando um novo post (POST)...');
  LBody := TJSONObject.Create;
  try
    LBody.AddPair('title', 'Artigo de Demonstração');
    LBody.AddPair('body', 'Conteúdo rico de demonstração do componente RESTRequest4Delphi.');
    LBody.AddPair('userId', '1');

    TTelemetry.Measure('Criar Novo Post (JSON Body)', 'POST', LBaseURLJSONPlaceholder + '/posts',
      function: IResponse
      begin
        Result := TRequest.New
          .BaseURL(LBaseURLJSONPlaceholder)
          .Resource('posts')
          .AddHeader('X-Custom-Header', 'DemoConsole')
          .AddBody(LBody)
          .Post;
      end
    );
  finally
    // JSONPlaceholder simula o retorno com ID 101
    LPostId := '101';
  end;

  // 4. PUT - Atualizar o post recém-criado por completo
  WriteLn('-> Atualizando o post criado (PUT)...');
  LBody := TJSONObject.Create;
  try
    LBody.AddPair('id', LPostId);
    LBody.AddPair('title', 'Artigo Atualizado');
    LBody.AddPair('body', 'Conteúdo atualizado do post.');
    LBody.AddPair('userId', '1');

    TTelemetry.Measure('Atualizar Post Completo (PUT)', 'PUT', LBaseURLJSONPlaceholder + '/posts/1',
      function: IResponse
      begin
        Result := TRequest.New
          .BaseURL(LBaseURLJSONPlaceholder)
          .Resource('posts/:id')
          .AddUrlSegment('id', '1')
          .AddBody(LBody)
          .Put;
      end
    );
  finally
  end;

  // 5. DELETE - Excluir o post
  WriteLn('-> Excluindo o post (DELETE)...');
  TTelemetry.Measure('Deletar Post (DELETE)', 'DELETE', LBaseURLJSONPlaceholder + '/posts/1',
    function: IResponse
    begin
      Result := TRequest.New
        .BaseURL(LBaseURLJSONPlaceholder)
        .Resource('posts/:id')
        .AddUrlSegment('id', '1')
        .Delete;
    end
  );
  
  WriteLn('Fluxo CRUD sequencial finalizado.');
  WriteLn('');
end;

procedure RunCookiesAndMultipartFlow;
var
  LUploadStream: TStringStream;
begin
  WriteLn('Iniciando fluxo de Cookies e Upload Multipart no HttpBin...');

  // 1. COOKIES - Gravar e recuperar Cookies
  WriteLn('-> Gravando e validando sessão por Cookies...');
  TTelemetry.Measure('Gravar Cookies (AddCookie)', 'GET', LBaseURLHttpBin + '/cookies/set?session_token=r4d_token_123',
    function: IResponse
    begin
      Result := TRequest.New
        .BaseURL(LBaseURLHttpBin)
        .Resource('cookies/set')
        .AddParam('session_token', 'r4d_token_123')
        .Get;
    end
  );

  // 2. MULTIPART FORM-DATA - Envio de arquivo anexado e campos
  WriteLn('-> Enviando arquivo de texto via Multipart Form-data...');
  LUploadStream := TStringStream.Create('Este e o conteudo de um arquivo texto enviado por streaming real.');
  try
    TTelemetry.Measure('Upload Multipart (AddField)', 'POST', LBaseURLHttpBin + '/post',
      function: IResponse
      begin
        Result := TRequest.New
          .BaseURL(LBaseURLHttpBin)
          .Resource('post')
          .AddField('nome_campo', 'RESTRequest4Delphi')
          .AddFile('arquivo', LUploadStream, 'exemplo_upload.txt')
          .Post;
      end
    );
  finally
    LUploadStream.Free;
  end;
  
  WriteLn('Fluxo de Cookies e Upload finalizado.');
  WriteLn('');
end;

procedure RunConcurrentAsyncFlow;
var
  LEvent1, LEvent2: TSimpleEvent;
  LStart: Cardinal;
  LResponse1, LResponse2: IResponse;
  LRequest1, LRequest2: IRequest;
  LTotalTime: Cardinal;
begin
  WriteLn('Iniciando fluxo assíncrono concorrente paralelo no HttpBin...');
  WriteLn('-> Disparando 2 requisições assíncronas com delay de 1.5s cada em paralelo...');

  LEvent1 := TSimpleEvent.Create;
  LEvent2 := TSimpleEvent.Create;
  try
    LResponse1 := nil;
    LResponse2 := nil;
    LStart := TThread.GetTickCount;

    // Criar e configurar as requisições assíncronas separadamente para evitar deadlocks de VCL
    LRequest1 := TRequest.New
      .BaseURL(LBaseURLHttpBin)
      .Resource('delay/1');
      
    LRequest2 := TRequest.New
      .BaseURL(LBaseURLHttpBin)
      .Resource('delay/1');

    {$IF NOT (DEFINED(RR4D_INDY) or DEFINED(FPC) or DEFINED(RR4D_SYNAPSE) or DEFINED(RR4D_ICS))}
    LRequest1.SynchronizedEvents(False);
    LRequest2.SynchronizedEvents(False);
    {$ENDIF}

    // Disparar a primeira requisição assíncrona
    LRequest1.GetAsync(
      procedure(const Req: IRequest; const Res: IResponse)
      begin
        LResponse1 := Res;
        LEvent1.SetEvent;
      end
    );

    // Disparar a segunda requisição assíncrona
    LRequest2.GetAsync(
      procedure(const Req: IRequest; const Res: IResponse)
      begin
        LResponse2 := Res;
        LEvent2.SetEvent;
      end
    );

    // Aguardar a finalização de ambas concorrentemente chamando CheckSynchronize
    while (LEvent1.WaitFor(10) = wrTimeout) or (LEvent2.WaitFor(10) = wrTimeout) do
    begin
      CheckSynchronize(10);
    end;

    LTotalTime := TThread.GetTickCount - LStart;
    WriteLn(Format('-> Ambas as requisições concorrentes terminaram em %d ms!', [LTotalTime]));

    // Medir os tempos individuais na telemetria
    TTelemetry.Measure('Chamada Assíncrona Paralela 1', 'GET', LBaseURLHttpBin + '/delay/1',
      function: IResponse
      begin
        Result := LResponse1;
      end
    );

    TTelemetry.Measure('Chamada Assíncrona Paralela 2', 'GET', LBaseURLHttpBin + '/delay/1',
      function: IResponse
      begin
        Result := LResponse2;
      end
    );
  finally
    LEvent1.Free;
    LEvent2.Free;
  end;
  
  WriteLn('Fluxo assíncrono concorrente finalizado.');
  WriteLn('');
end;

begin
  try
    WriteLn('======================================================================');
    WriteLn('      DEMONSTRACAO REAL E TELEMETRIA - RESTRequest4Delphi             ');
    WriteLn('======================================================================');
    WriteLn('');

    RunSequentialCRUDFlow;
    RunCookiesAndMultipartFlow;
    RunConcurrentAsyncFlow;

    // Gravar o relatório em Markdown
    WriteLn('Salvando relatório de telemetria em "samples_execution_report.md"...');
    TTelemetry.SaveReport('samples_execution_report.md');
    WriteLn('Sucesso! Demonstração finalizada com exito.');
    
  except
    on E: Exception do
      WriteLn('Erro Fatal na Demonstração: ' + E.ClassName + ': ' + E.Message);
  end;
end.
