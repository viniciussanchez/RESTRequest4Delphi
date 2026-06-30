unit RESTRequest4D.Demo.Telemetry;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  RESTRequest4D;

type
  TTelemetryItem = record
    StepName: string;
    Method: string;
    URL: string;
    StatusCode: Integer;
    TimeMs: Cardinal;
    ContentLength: Int64;
    Success: Boolean;
    ErrorMessage: string;
  end;

  TTelemetry = class
  private
    class var FItems: TList<TTelemetryItem>;
    class constructor Create;
    class destructor Destroy;
  public
    class function Measure(const AStepName, AMethod, AURL: string; const AFunc: TFunc<IResponse>): IResponse;
    class procedure SaveReport(const AFilename: string);
  end;

implementation

uses
  System.StrUtils;

{ TTelemetry }

class constructor TTelemetry.Create;
begin
  FItems := TList<TTelemetryItem>.Create;
end;

class destructor TTelemetry.Destroy;
begin
  FItems.Free;
end;

class function TTelemetry.Measure(const AStepName, AMethod, AURL: string;
  const AFunc: TFunc<IResponse>): IResponse;
var
  LStart: Cardinal;
  LItem: TTelemetryItem;
begin
  LItem.StepName := AStepName;
  LItem.Method := AMethod;
  LItem.URL := AURL;
  LItem.StatusCode := 0;
  LItem.ContentLength := 0;
  LItem.Success := False;
  LItem.ErrorMessage := '';
  
  LStart := TThread.GetTickCount;
  try
    Result := AFunc();
    LItem.TimeMs := TThread.GetTickCount - LStart;
    
    if Assigned(Result) then
    begin
      LItem.StatusCode := Result.StatusCode;
      LItem.ContentLength := Length(Result.Content);
      LItem.Success := (Result.StatusCode >= 200) and (Result.StatusCode < 300);
      if not LItem.Success then
        LItem.ErrorMessage := 'Retornou status de erro HTTP: ' + IntToStr(Result.StatusCode);
    end
    else
      LItem.ErrorMessage := 'Resposta nula retornada pela API';
  except
    on E: Exception do
    begin
      LItem.TimeMs := TThread.GetTickCount - LStart;
      LItem.ErrorMessage := E.ClassName + ': ' + E.Message;
    end;
  end;
  
  FItems.Add(LItem);
end;

class procedure TTelemetry.SaveReport(const AFilename: string);
var
  LReport: TStringList;
  LItem: TTelemetryItem;
  LTotalTime: Cardinal;
  LSuccessCount: Integer;
  LAvgTime: Double;
  LMaxTime: Cardinal;
begin
  LReport := TStringList.Create;
  try
    LReport.Add('# Relatório de Telemetria de Execução do Exemplo Real');
    LReport.Add('');
    LReport.Add('Este relatório foi gerado automaticamente pelo console de demonstração do **RESTRequest4Delphi** ');
    LReport.Add(Format('em **%s**.', [DateTimeToStr(Now)]));
    LReport.Add('');
    
    // Calcular estatísticas
    LTotalTime := 0;
    LSuccessCount := 0;
    LMaxTime := 0;
    for LItem in FItems do
    begin
      Inc(LTotalTime, LItem.TimeMs);
      if LItem.Success then
        Inc(LSuccessCount);
      if LItem.TimeMs > LMaxTime then
        LMaxTime := LItem.TimeMs;
    end;
    
    if FItems.Count > 0 then
      LAvgTime := LTotalTime / FItems.Count
    else
      LAvgTime := 0;

    LReport.Add('## 📊 Resumo das Estatísticas');
    LReport.Add('');
    LReport.Add(Format('- **Total de Requisições**: %d', [FItems.Count]));
    LReport.Add(Format('- **Taxa de Sucesso**: %d/%d (%.1f%%)', [LSuccessCount, FItems.Count, (LSuccessCount / FItems.Count) * 100]));
    LReport.Add(Format('- **Tempo Total Gasto**: %d ms', [LTotalTime]));
    LReport.Add(Format('- **Tempo Médio por Requisição**: %.2f ms', [LAvgTime]));
    LReport.Add(Format('- **Maior Latência Registrada**: %d ms', [LMaxTime]));
    LReport.Add('');
    
    LReport.Add('## 📋 Detalhamento das Transações');
    LReport.Add('');
    LReport.Add('| Passo / Descrição | Verbo | URL Alvo | Status HTTP | Latência (ms) | Bytes Recebidos | Resultado |');
    LReport.Add('| :--- | :---: | :--- | :---: | :---: | :---: | :--- |');
    
    for LItem in FItems do
    begin
      LReport.Add(Format('| %s | %s | %s | %d | %d | %d | %s |', [
        LItem.StepName,
        LItem.Method,
        LItem.URL,
        LItem.StatusCode,
        LItem.TimeMs,
        LItem.ContentLength,
        IfThen(LItem.Success, '🟢 Sucesso', '🔴 Falha: ' + LItem.ErrorMessage)
      ]));
    end;
    
    LReport.SaveToFile(AFilename);
  finally
    LReport.Free;
  end;
end;

end.
