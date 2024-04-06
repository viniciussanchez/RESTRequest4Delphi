unit RESTRequest4D.Config;

interface

uses
{$IF DEFINED(FPC)}
  SysUtils;
{$ELSE}
  System.SysUtils;
{$ENDIF}

type
  TRESTRequest4DConfig = class
  private
    FParseJSONEncoding: TEncoding;
    class var FInstance: TRESTRequest4DConfig;
  protected
    class function GetDefaultInstance: TRESTRequest4DConfig;
  public
    constructor Create;
    property ParseJSONEncoding: TEncoding read FParseJSONEncoding write FParseJSONEncoding;
    class function GetInstance: TRESTRequest4DConfig;
    class procedure UnInitialize;
  end;

implementation

constructor TRESTRequest4DConfig.Create;
begin
  FParseJSONEncoding := TEncoding.UTF8;
end;

class function TRESTRequest4DConfig.GetDefaultInstance: TRESTRequest4DConfig;
begin
  if not Assigned(FInstance) then
    FInstance := TRESTRequest4DConfig.Create;
  Result := FInstance;
end;

class function TRESTRequest4DConfig.GetInstance: TRESTRequest4DConfig;
begin
  Result := TRESTRequest4DConfig.GetDefaultInstance;
end;

class procedure TRESTRequest4DConfig.UnInitialize;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);
end;

initialization

finalization
  TRESTRequest4DConfig.UnInitialize;

end.
