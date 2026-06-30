unit RESTRequest4D.Utils;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
    Classes, SysUtils,
  {$ELSE}
    System.Classes, System.SysUtils,
  {$ENDIF}
  RESTRequest4D.Request.Contract,
  RESTRequest4D.Response.Contract;

type
  TMethodRequest = (mrGET, mrPOST, mrPUT, mrPATCH, mrDELETE, mrOPTIONS, mrHEAD, mrTRACE);

  TR4DUtils = class
  public
    class function BuildURL(const ABaseURL, AResource, AResourceSuffix: string;
      const AUrlSegments, AParams: TStrings; const AIncludeParams: Boolean = True): string;
  end;

  TAsyncRequestThread = class(TThread)
  private
    FRequest: IRequest;
    FCallback: TRR4DCallbackOnAfterExecute;
    FMethod: string;
    FResponse: IResponse;
    procedure CallCallback;
  protected
    procedure Execute; override;
  public
    constructor Create(const ARequest: IRequest; const AMethod: string; const ACallback: TRR4DCallbackOnAfterExecute);
  end;

implementation

class function TR4DUtils.BuildURL(const ABaseURL, AResource, AResourceSuffix: string;
  const AUrlSegments, AParams: TStrings; const AIncludeParams: Boolean): string;
var
  I: Integer;
  LResource, LResourceSuffix: string;
begin
  Result := ABaseURL.Trim;
  LResource := AResource.Trim;
  LResourceSuffix := AResourceSuffix.Trim;
  
  if not LResource.IsEmpty then
  begin
    if not Result.EndsWith('/') then
      Result := Result + '/';
    if LResource.StartsWith('/') then
      Result := Result + Copy(LResource, 2, MaxInt)
    else
      Result := Result + LResource;
  end;
  
  if not LResourceSuffix.IsEmpty then
  begin
    if not Result.EndsWith('/') then
      Result := Result + '/';
    if LResourceSuffix.StartsWith('/') then
      Result := Result + Copy(LResourceSuffix, 2, MaxInt)
    else
      Result := Result + LResourceSuffix;
  end;
  
  if Assigned(AUrlSegments) and (AUrlSegments.Count > 0) then
  begin
    for I := 0 to Pred(AUrlSegments.Count) do
    begin
      Result := StringReplace(Result, Format('{%s}', [AUrlSegments.Names[I]]), AUrlSegments.ValueFromIndex[I], [rfReplaceAll, rfIgnoreCase]);
      Result := StringReplace(Result, Format(':%s', [AUrlSegments.Names[I]]), AUrlSegments.ValueFromIndex[I], [rfReplaceAll, rfIgnoreCase]);
    end;
  end;
  
  if not AIncludeParams then
    Exit;
    
  if Assigned(AParams) and (AParams.Count > 0) then
  begin
    Result := Result + '?';
    for I := 0 to Pred(AParams.Count) do
    begin
      if I > 0 then
        Result := Result + '&';
      Result := Result + AParams.Strings[I];
    end;
  end;
end;

{ TAsyncRequestThread }

constructor TAsyncRequestThread.Create(const ARequest: IRequest; const AMethod: string; const ACallback: TRR4DCallbackOnAfterExecute);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FRequest := ARequest;
  FMethod := AMethod;
  FCallback := ACallback;
end;

procedure TAsyncRequestThread.Execute;
begin
  try
    if FMethod = 'GET' then
      FResponse := FRequest.Get
    else if FMethod = 'POST' then
      FResponse := FRequest.Post
    else if FMethod = 'PUT' then
      FResponse := FRequest.Put
    else if FMethod = 'DELETE' then
      FResponse := FRequest.Delete
    else if FMethod = 'PATCH' then
      FResponse := FRequest.Patch;
  except
  end;
  
  if Assigned(FCallback) then
    Queue(CallCallback);
end;

procedure TAsyncRequestThread.CallCallback;
begin
  FCallback(FRequest, FResponse);
end;

end.
