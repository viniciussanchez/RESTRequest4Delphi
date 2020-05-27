unit Samples.Main;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, RESTRequest4D.Request;

type
  TMyCompletionHandlerWithError = TProc<TObject>;

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
    btnGetStatusCode: TButton;
    btnExecuteAsync: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
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
    procedure btnGetStatusCodeClick(Sender: TObject);
    procedure btnExecuteAsyncClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
  private
    FRequest: IRequest;
    procedure SetRequest(const AValue: IRequest);
  public
    property Request: IRequest read FRequest write SetRequest;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses REST.Types, System.JSON;

procedure TFrmMain.btnAddBodyWithJSONObjectClick(Sender: TObject);
begin
  Request.Body.Add(TJSONObject.Create); // or Request.AddBody(TJSONObject.Create);
end;

procedure TFrmMain.btnAddBodyWithObjectClick(Sender: TObject);
begin
  Request.Body.Add(TObject.Create); // or Request.AddBody(TObject.Create);
end;

procedure TFrmMain.btnAddBodyWithStringClick(Sender: TObject);
begin
  Request.Body.Add('Any thing'); // or Request.AddBody('Any thing');
end;

procedure TFrmMain.btnAddParamClick(Sender: TObject);
begin
  Request.Params.Add('country', 'Brazil');
  Request.Params.Add('year', 2019);
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
  Request.Body.Clear; // or Request.ClearBody;
end;

procedure TFrmMain.btnClearParamsClick(Sender: TObject);
begin
  Request.Params.Clear;
end;

procedure TFrmMain.btnExecuteAsyncClick(Sender: TObject);
var
  LMyCompletionHandlerWithError: TMyCompletionHandlerWithError;
begin
  LMyCompletionHandlerWithError := procedure(AObject: TObject)
    begin
      if Assigned(AObject) and (AObject is Exception) then
        raise Exception(AObject); // or whatever you want!
    end;
  Request.ExecuteAsync(nil, True, True, LMyCompletionHandlerWithError);
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
  LMemTable: TFDMemTable;
begin
  LMemTable := Request.GetDataSetAdapter as TFDMemTable;
end;

procedure TFrmMain.btnGetFullRequestURLClick(Sender: TObject);
begin
  ShowMessage(Request.GetFullRequestURL(True));
end;

procedure TFrmMain.btnGetMethodClick(Sender: TObject);
var
  LMethod: TRESTRequestMethod;
begin
  LMethod := Request.GetMethod;
end;

procedure TFrmMain.btnGetResourceClick(Sender: TObject);
begin
  ShowMessage(Request.GetResource);
end;

procedure TFrmMain.btnGetResourceSuffixClick(Sender: TObject);
begin
  ShowMessage(Request.GetResourceSuffix);
end;

procedure TFrmMain.btnGetStatusCodeClick(Sender: TObject);
var
  LStatusCode: Integer;
begin
  LStatusCode := Request.Response.GetStatusCode;
end;

procedure TFrmMain.btnJWTAuthorizationTokenClick(Sender: TObject);
begin
  Request.Headers.Add('Authorization', 'JWT Token', [poDoNotEncode]);
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

procedure TFrmMain.Button10Click(Sender: TObject);
var
  LRaiseException: Boolean;
begin
  LRaiseException := Request.GetRaiseExceptionOn500;
end;

procedure TFrmMain.Button11Click(Sender: TObject);
begin
  Request.SetRaiseExceptionOn500(False);
end;

procedure TFrmMain.Button12Click(Sender: TObject);
var
  LContentEncoding: string;
begin
  LContentEncoding := Request.Response.GetContentEncoding;
end;

procedure TFrmMain.Button13Click(Sender: TObject);
var
  LContentType: string;
begin
  LContentType := Request.Response.GetContentType;
end;

procedure TFrmMain.Button14Click(Sender: TObject);
var
  LContentLength: Cardinal;
begin
  LContentLength := Request.Response.GetContentLength;
end;

procedure TFrmMain.Button15Click(Sender: TObject);
var
  LContent: string;
begin
  LContent := Request.Response.GetContent;
end;

procedure TFrmMain.Button16Click(Sender: TObject);
begin
  Request.SetToken('token');
end;

procedure TFrmMain.Button17Click(Sender: TObject);
var
  LToken: string;
begin
  LToken := Request.GetToken;
end;

procedure TFrmMain.Button1Click(Sender: TObject);
begin
  Request.Headers.Add('Accept-Encoding', 'gzip');
end;

procedure TFrmMain.Button2Click(Sender: TObject);
var
  LTimeout: Integer;
begin
  LTimeout := Request.GetTimeout;
end;

procedure TFrmMain.Button3Click(Sender: TObject);
begin
  Request.SetTimeout(30000);
end;

procedure TFrmMain.Button4Click(Sender: TObject);
var
  LAccept: string;
begin
  LAccept := Request.GetAccept;
end;

procedure TFrmMain.Button5Click(Sender: TObject);
begin
  Request.SetAccept(CONTENTTYPE_APPLICATION_JSON);
end;

procedure TFrmMain.Button6Click(Sender: TObject);
var
  LAcceptCharset: string;
begin
  LAcceptCharset := Request.GetAcceptCharset;
end;

procedure TFrmMain.Button7Click(Sender: TObject);
begin
  Request.SetAcceptCharset('utf-8');
end;

procedure TFrmMain.Button8Click(Sender: TObject);
begin
  Request.SetAcceptEncoding('gzip, deflate');
end;

procedure TFrmMain.Button9Click(Sender: TObject);
var
  LAcceptEncoding: string;
begin
  LAcceptEncoding := Request.GetAcceptEncoding;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  Request := TRequest.Create;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  Request := nil;
end;

procedure TFrmMain.SetRequest(const AValue: IRequest);
begin
  FRequest := AValue;
end;

end.
