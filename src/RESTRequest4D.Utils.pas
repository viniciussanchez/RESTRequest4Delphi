unit RESTRequest4D.Utils;

interface

{$IFNDEF FPC}
uses DB;
{$ENDIF}

type
  TMethodRequest = (mrGET, mrPOST, mrPUT, mrPATCH, mrDELETE);

  TRESTRequest4DelphiUtils = class
  public
    {$IFNDEF FPC}
    class procedure ActiveCachedUpdates(const ADataSet: TDataSet; const AActive: Boolean = True);
    {$ENDIF}
  end;

implementation

{$IFNDEF FPC}
uses System.Generics.Collections, FireDAC.Comp.Client;

class procedure TRESTRequest4DelphiUtils.ActiveCachedUpdates(const ADataSet: TDataSet; const AActive: Boolean);
var
  LDataSet: TDataSet;
  LDataSetDetails: TList<TDataSet>;
begin
  LDataSetDetails := TList<TDataSet>.Create;
  try
    if not AActive then
      ADataSet.Close;
    if ADataSet is TFDMemTable then
      TFDMemTable(ADataSet).CachedUpdates := AActive;
    if AActive and (not ADataSet.Active) and (ADataSet.FieldCount > 0) then
      ADataSet.Open;
    ADataSet.GetDetailDataSets(LDataSetDetails);
    for LDataSet in LDataSetDetails do
      ActiveCachedUpdates(LDataSet, AActive);
  finally
    LDataSetDetails.Free;
  end;
end;
{$ENDIF}

end.
