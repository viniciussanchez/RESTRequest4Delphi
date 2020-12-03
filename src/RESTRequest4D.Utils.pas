unit RESTRequest4D.Utils;

interface

uses Data.DB;

type
  TRESTRequest4DelphiUtils = class
  public
    class procedure ActiveCachedUpdates(const ADataSet: TDataSet; const AActive: Boolean = True);
  end;

implementation

uses System.Generics.Collections, FireDAC.Comp.Client;

class procedure TRESTRequest4DelphiUtils.ActiveCachedUpdates(const ADataSet: TDataSet; const AActive: Boolean);
var
  LDataSet: TDataSet;
  LDataSetDetails: TList<TDataSet>;
begin
  LDataSetDetails := TList<TDataSet>.Create;
  try
    if ADataSet is TFDMemTable then
    begin
      if not AActive then
        TFDMemTable(ADataSet).Close;
      TFDMemTable(ADataSet).CachedUpdates := AActive;
      if AActive and (not TFDMemTable(ADataSet).Active) and (TFDMemTable(ADataSet).FieldCount > 0) then
        TFDMemTable(ADataSet).Open;
    end;
    ADataSet.GetDetailDataSets(LDataSetDetails);
    for LDataSet in LDataSetDetails do
      ActiveCachedUpdates(LDataSet, AActive);
  finally
    LDataSetDetails.Free;
  end;
end;

end.
