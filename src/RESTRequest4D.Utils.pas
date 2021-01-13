unit RESTRequest4D.Utils;

{$IFDEF FPC} {$mode delphi} {$ENDIF}

interface

uses DB;

type
  TRESTRequest4DelphiUtils = class
  public
    class procedure ActiveCachedUpdates(const ADataSet: TDataSet; const AActive: Boolean = True);
  end;

implementation

uses
  {$IFDEF FPC}
    Generics.Collections;
  {$ELSE}
    System.Generics.Collections, FireDAC.Comp.Client;
  {$ENDIF}

class procedure TRESTRequest4DelphiUtils.ActiveCachedUpdates(const ADataSet: TDataSet; const AActive: Boolean);
var
  LDataSet: TDataSet;
  LDataSetDetails: TList<TDataSet>;
begin
  LDataSetDetails := TList<TDataSet>.Create;
  try
    {$IFNDEF FPC}
      if ADataSet is TFDMemTable then
      begin
        if not AActive then
          TFDMemTable(ADataSet).Close;
        TFDMemTable(ADataSet).CachedUpdates := AActive;
        if AActive and (not TFDMemTable(ADataSet).Active) and (TFDMemTable(ADataSet).FieldCount > 0) then
          TFDMemTable(ADataSet).Open;
      end;
      ADataSet.GetDetailDataSets(LDataSetDetails);
    {$ENDIF}
    for LDataSet in LDataSetDetails do
      ActiveCachedUpdates(LDataSet, AActive);
  finally
    LDataSetDetails.Free;
  end;
end;

end.
