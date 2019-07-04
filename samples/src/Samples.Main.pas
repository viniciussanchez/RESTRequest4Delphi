unit Samples.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, RESTRequest4D.Request.Intf, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TFrmMain = class(TForm)
    btnSetMethod: TButton;
    btnSetBaseURL: TButton;
    FDMemTable: TFDMemTable;
    btnSetDatasetAdapter: TButton;
    btnGetMethod: TButton;
    btnGetBaseURL: TButton;
    btnGetResource: TButton;
    btnGetResourceSuffix: TButton;
    btnSetResourceSuffix: TButton;
    btnSetResource: TButton;
    btnGetDatasetAdapter: TButton;
    btnGetFullRequestURL: TButton;
    btnClearParams: TButton;
    btnAddParam: TButton;
    Button1: TButton;
    btnClearBody: TButton;
    btnAddBodyWithString: TButton;
    btnAddBodyWithJSONObject: TButton;
    btnAddBodyWithObject: TButton;
    btnJWTAuthorizationToken: TButton;
    btnBasicAuthorization: TButton;
    btnExecuteRequest: TButton;
    btnClearBasicAuthentication: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSetMethodClick(Sender: TObject);
    procedure btnSetBaseURLClick(Sender: TObject);
    procedure btnSetDatasetAdapterClick(Sender: TObject);
    procedure btnSetResourceClick(Sender: TObject);
    procedure btnSetResourceSuffixClick(Sender: TObject);
    procedure btnGetMethodClick(Sender: TObject);
    procedure btnGetBaseURLClick(Sender: TObject);
    procedure btnGetResourceClick(Sender: TObject);
    procedure btnGetResourceSuffixClick(Sender: TObject);
    procedure btnGetDatasetAdapterClick(Sender: TObject);
    procedure btnGetFullRequestURLClick(Sender: TObject);
    procedure btnClearParamsClick(Sender: TObject);
    procedure btnAddParamClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnClearBodyClick(Sender: TObject);
    procedure btnAddBodyWithStringClick(Sender: TObject);
    procedure btnAddBodyWithJSONObjectClick(Sender: TObject);
    procedure btnAddBodyWithObjectClick(Sender: TObject);
    procedure btnExecuteRequestClick(Sender: TObject);
    procedure btnJWTAuthorizationTokenClick(Sender: TObject);
    procedure btnBasicAuthorizationClick(Sender: TObject);
    procedure btnClearBasicAuthenticationClick(Sender: TObject);
  private
    FRequest: IRequest;
    procedure SetRequest(const Value: IRequest);
  public
    property Request: IRequest read FRequest write SetRequest;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses RESTRequest4D.Request, REST.Types, System.JSON;

procedure TFrmMain.btnAddBodyWithJSONObjectClick(Sender: TObject);
begin
  Request.Body.Add(TJSONObject.Create, True);
end;

procedure TFrmMain.btnAddBodyWithObjectClick(Sender: TObject);
begin
  Request.Body.Add(TObject.Create, True);
end;

procedure TFrmMain.btnAddBodyWithStringClick(Sender: TObject);
begin
  Request.Body.Add('Any thing');
end;

procedure TFrmMain.btnAddParamClick(Sender: TObject);
begin
  Request.Params.AddParam('country', 'Brazil');
end;

procedure TFrmMain.btnBasicAuthorizationClick(Sender: TObject);
begin
  Request.Authentication.SetUsername('sample').SetPassword('123');
end;

procedure TFrmMain.btnClearBasicAuthenticationClick(Sender: TObject);
begin
  Request.Authentication.Clear;
end;

procedure TFrmMain.btnClearBodyClick(Sender: TObject);
begin
  Request.Body.Clear;
end;

procedure TFrmMain.btnClearParamsClick(Sender: TObject);
begin
  Request.Params.Clear;
end;

procedure TFrmMain.btnExecuteRequestClick(Sender: TObject);
begin
  Request.Execute;
end;

procedure TFrmMain.btnGetBaseURLClick(Sender: TObject);
begin
  ShowMessage(Request.GetBaseURL);
end;

procedure TFrmMain.btnGetDatasetAdapterClick(Sender: TObject);
var
  MemTable: TFDMemTable;
begin
  MemTable := Request.GetDataSetAdapter as TFDMemTable;
end;

procedure TFrmMain.btnGetFullRequestURLClick(Sender: TObject);
begin
  ShowMessage(Request.GetFullRequestURL(True));
end;

procedure TFrmMain.btnGetMethodClick(Sender: TObject);
var
  Method: TRESTRequestMethod;
begin
  Method := Request.GetMethod;
end;

procedure TFrmMain.btnGetResourceClick(Sender: TObject);
begin
  ShowMessage(Request.GetResource);
end;

procedure TFrmMain.btnGetResourceSuffixClick(Sender: TObject);
begin
  ShowMessage(Request.GetResourceSuffix);
end;

procedure TFrmMain.btnJWTAuthorizationTokenClick(Sender: TObject);
begin
  Request.Params.AddHeader('Authorization', 'JWT Token', [poDoNotEncode]);
end;

procedure TFrmMain.btnSetBaseURLClick(Sender: TObject);
begin
  Request.SetBaseURL('http://localhost:8080/datasnap/rest');
end;

procedure TFrmMain.btnSetDatasetAdapterClick(Sender: TObject);
begin
  Request.SetDataSetAdapter(FDMemTable);
end;

procedure TFrmMain.btnSetMethodClick(Sender: TObject);
begin
  Request.SetMethod(rmGET);
end;

procedure TFrmMain.btnSetResourceClick(Sender: TObject);
begin
  Request.SetResource('servermethods');
end;

procedure TFrmMain.btnSetResourceSuffixClick(Sender: TObject);
begin
  Request.SetResourceSuffix('method');
end;

procedure TFrmMain.Button1Click(Sender: TObject);
begin
  Request.Params.AddHeader('Accept-Encoding', 'gzip');
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  Request := TRequest.Create;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  Request := nil;
end;

procedure TFrmMain.SetRequest(const Value: IRequest);
begin
  FRequest := Value;
end;

end.
