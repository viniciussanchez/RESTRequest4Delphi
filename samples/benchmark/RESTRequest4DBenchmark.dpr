program RESTRequest4DBenchmark;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Diagnostics,
  RESTRequest4D;

type
  TBenchmarkMetric = record
    Method: string;
    Url: string;
    LatencyMs: Int64;
    StatusCode: Integer;
    Success: Boolean;
  end;

var
  LMetrics: TArray<TBenchmarkMetric>;
  LEngineName: string;

procedure AddMetric(const AMethod, AUrl: string; const ALatency: Int64; const AStatusCode: Integer; const ASuccess: Boolean);
var
  LIdx: Integer;
begin
  LIdx := Length(LMetrics);
  SetLength(LMetrics, LIdx + 1);
  LMetrics[LIdx].Method := AMethod;
  LMetrics[LIdx].Url := AUrl;
  LMetrics[LIdx].LatencyMs := ALatency;
  LMetrics[LIdx].StatusCode := AStatusCode;
  LMetrics[LIdx].Success := ASuccess;
end;

procedure RunTest(const AMethod, AUrl: string; const AReqFunc: TFunc<IResponse>);
var
  LStopwatch: TStopwatch;
  LResponse: IResponse;
  LStatusCode: Integer;
  LSuccess: Boolean;
begin
  LStatusCode := 0;
  LSuccess := False;
  LStopwatch := TStopwatch.StartNew;
  try
    LResponse := AReqFunc();
    LStopwatch.Stop;
    if Assigned(LResponse) then
    begin
      LStatusCode := LResponse.StatusCode;
      LSuccess := (LStatusCode >= 200) and (LStatusCode < 300);
    end;
  except
    on E: Exception do
    begin
      LStopwatch.Stop;
      // Não propagar exceção, registrar falha
    end;
  end;
  AddMetric(AMethod, AUrl, LStopwatch.ElapsedMilliseconds, LStatusCode, LSuccess);
end;

procedure SaveMetrics(const AFilename: string);
var
  LRoot: TJSONObject;
  LArray: TJSONArray;
  LObj: TJSONObject;
  LMetric: TBenchmarkMetric;
  LFile: TStringList;
begin
  LRoot := TJSONObject.Create;
  LArray := TJSONArray.Create;
  try
    LRoot.AddPair('engine', LEngineName);
    for LMetric in LMetrics do
    begin
      LObj := TJSONObject.Create;
      LObj.AddPair('method', LMetric.Method);
      LObj.AddPair('url', LMetric.Url);
      LObj.AddPair('latency', TJSONNumber.Create(LMetric.LatencyMs));
      LObj.AddPair('statusCode', TJSONNumber.Create(LMetric.StatusCode));
      LObj.AddPair('success', TJSONBool.Create(LMetric.Success));
      LArray.AddElement(LObj);
    end;
    LRoot.AddPair('metrics', LArray);
    
    LFile := TStringList.Create;
    try
      LFile.Text := LRoot.ToJSON;
      LFile.SaveToFile(AFilename);
    finally
      LFile.Free;
    end;
  finally
    LRoot.Free;
  end;
end;

procedure ExecuteBenchmark;
var
  I: Integer;
  LBaseURL: string;
begin
  LBaseURL := 'https://httpbin.org';
  WriteLn('Iniciando benchmark para a engine: ' + LEngineName);

  // Executa 3 iterações de aquecimento (warmup) para inicializar conexões e DNS do socket
  WriteLn('Executando warmup...');
  for I := 1 to 3 do
  begin
    try
      TRequest.New.BaseURL(LBaseURL).Resource('get').Get;
    except
    end;
  end;

  // Executa 5 iterações para cada método e tira a média
  WriteLn('Executando medicoes HTTP...');
  for I := 1 to 5 do
  begin
    WriteLn('-> Iteracao ' + IntToStr(I) + '/5...');
    
    // GET
    RunTest('GET', LBaseURL + '/get',
      function: IResponse
      begin
        Result := TRequest.New.BaseURL(LBaseURL).Resource('get').Get;
      end
    );

    // POST
    RunTest('POST', LBaseURL + '/post',
      function: IResponse
      var
        LBody: TJSONObject;
      begin
        LBody := TJSONObject.Create;
        LBody.AddPair('benchmark', 'RESTRequest4Delphi');
        Result := TRequest.New.BaseURL(LBaseURL).Resource('post').AddBody(LBody).Post;
      end
    );

    // PUT
    RunTest('PUT', LBaseURL + '/put',
      function: IResponse
      var
        LBody: TJSONObject;
      begin
        LBody := TJSONObject.Create;
        LBody.AddPair('benchmark', 'RESTRequest4Delphi');
        Result := TRequest.New.BaseURL(LBaseURL).Resource('put').AddBody(LBody).Put;
      end
    );

    // DELETE
    RunTest('DELETE', LBaseURL + '/delete',
      function: IResponse
      begin
        Result := TRequest.New.BaseURL(LBaseURL).Resource('delete').Delete;
      end
    );

    // PATCH
    RunTest('PATCH', LBaseURL + '/patch',
      function: IResponse
      var
        LBody: TJSONObject;
      begin
        LBody := TJSONObject.Create;
        LBody.AddPair('benchmark', 'RESTRequest4Delphi');
        Result := TRequest.New.BaseURL(LBaseURL).Resource('patch').AddBody(LBody).Patch;
      end
    );
    
    // Pequeno intervalo de 100ms para evitar throttle/rate limit do servidor real httpbin.org
    Sleep(100);
  end;
end;

begin
  try
    if ParamCount < 1 then
    begin
      WriteLn('Uso: RESTRequest4DBenchmark <nome_da_engine>');
      Halt(1);
    end;
    LEngineName := ParamStr(1);
    ExecuteBenchmark;
    SaveMetrics('benchmark_' + LEngineName + '.json');
    WriteLn('Benchmark de ' + LEngineName + ' concluido com sucesso!');
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
