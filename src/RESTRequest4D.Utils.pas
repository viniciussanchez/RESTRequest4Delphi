unit RESTRequest4D.Utils;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
    Classes, SysUtils;
  {$ELSE}
    System.Classes, System.SysUtils;
  {$ENDIF}

type
  TMethodRequest = (mrGET, mrPOST, mrPUT, mrPATCH, mrDELETE, mrOPTIONS, mrHEAD, mrTRACE);

  TR4DUtils = class
  public
    class function BuildURL(const ABaseURL, AResource, AResourceSuffix: string;
      const AUrlSegments, AParams: TStrings; const AIncludeParams: Boolean = True): string;
  end;

implementation

class function TR4DUtils.BuildURL(const ABaseURL, AResource, AResourceSuffix: string;
  const AUrlSegments, AParams: TStrings; const AIncludeParams: Boolean): string;
var
  I: Integer;
  LBaseURL, LResource, LResourceSuffix: string;
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

end.
