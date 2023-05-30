unit Samples.Main;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.Imaging.pngimage, Vcl.Mask;

type
  TMyCompletionHandlerWithError = TProc<TObject>;

  TFrmMain = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    PageControl2: TPageControl;
    TabSheet6: TTabSheet;
    edtBaseURL: TLabeledEdit;
    edtAccept: TLabeledEdit;
    Label1: TLabel;
    mmCustomBody: TMemo;
    btnDELETE: TButton;
    btnPUT: TButton;
    btnPOST: TButton;
    btnGET: TButton;
    mmBody: TMemo;
    lblStatusCode: TLabel;
    Label3: TLabel;
    TabSheet2: TTabSheet;
    Panel7: TPanel;
    Label2: TLabel;
    imgMultipartFormDataStream: TImage;
    Label4: TLabel;
    edtMultipartFormDataText: TEdit;
    Label5: TLabel;
    lblMultipartFormDataFile: TLabel;
    Panel9: TPanel;
    Panel10: TPanel;
    btnMultipartFormDataPost: TButton;
    edtMultipartFormDataBaseURL: TLabeledEdit;
    Panel8: TPanel;
    lblRESTRequest4DelphiComponent: TLabel;
    btnMultipartFormDataPut: TButton;
    FDMemTable1: TFDMemTable;
    procedure btnGETClick(Sender: TObject);
    procedure btnPOSTClick(Sender: TObject);
    procedure btnPUTClick(Sender: TObject);
    procedure btnDELETEClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnMultipartFormDataPostClick(Sender: TObject);
    procedure btnMultipartFormDataPutClick(Sender: TObject);
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses RESTRequest4D;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet1;
  lblMultipartFormDataFile.Caption := (ExtractFilePath(ParamStr(0)) + 'RESTRequest4Delphi.pdf');

{$IF DEFINED(FPC) and (not DEFINED(RR4D_INDY)) and (not DEFINED(RR4D_SYNAPSE))}
  RESTRequest4D.Request.FPHTTPClient;
  lblRESTRequest4DelphiComponent.Caption := 'RESTRequest4Delphi:  RR4D_FPHTTPCLIENT';
{$ELSEIF DEFINED(RR4D_INDY)}
  lblRESTRequest4DelphiComponent.Caption := 'RESTRequest4Delphi:  RR4D_INDY';
{$ELSEIF DEFINED(RR4D_NETHTTP)}
  lblRESTRequest4DelphiComponent.Caption := 'RESTRequest4Delphi:  RR4D_NETHTTP';
{$ELSEIF DEFINED(RR4D_SYNAPSE)}
  lblRESTRequest4DelphiComponent.Caption := 'RESTRequest4Delphi:  RR4D_SYNAPSE';
{$ELSE}
  lblRESTRequest4DelphiComponent.Caption := 'RESTRequest4Delphi:  RR4D_RESTCLIENT';
{$ENDIF}
end;

procedure TFrmMain.btnDELETEClick(Sender: TObject);
var
  LResponse: IResponse;
begin
  LResponse := TRequest.New.BaseURL(edtBaseURL.Text)
    .Accept(edtAccept.Text)
    .Delete;

  mmBody.Lines.Text := LResponse.Content;
  lblStatusCode.Caption := LResponse.StatusCode.ToString;
end;

procedure TFrmMain.btnGETClick(Sender: TObject);
var
  LResponse: IResponse;
begin
  LResponse := TRequest.New.BaseURL(edtBaseURL.Text)
    .Accept(edtAccept.Text)
    .Get;

  mmBody.Lines.Text := LResponse.Content;
  lblStatusCode.Caption := LResponse.StatusCode.ToString;
end;

procedure TFrmMain.btnMultipartFormDataPostClick(Sender: TObject);
var
  LStream: TMemoryStream;
  LResponse: IResponse;
begin
  LStream := TMemoryStream.Create;
  try
    {$IF COMPILERVERSION <= 31.0}
    imgMultipartFormDataStream.Picture.Graphic.SaveToStream(LStream);
    {$ELSE}
    imgMultipartFormDataStream.Picture.SaveToStream(LStream);
    {$ENDIF}

    LResponse := TRequest.New.BaseURL(edtMultipartFormDataBaseURL.Text)
      .AddField('text', edtMultipartFormDataText.Text)
      .AddFile('file', lblMultipartFormDataFile.Caption)
      .AddFile('stream', LStream)
      .Post;
  finally
    LStream.Free;
  end;

  mmBody.Lines.Text := LResponse.Content;
  lblStatusCode.Caption := LResponse.StatusCode.ToString;
end;

procedure TFrmMain.btnMultipartFormDataPutClick(Sender: TObject);
var
  LStream: TMemoryStream;
  LResponse: IResponse;
begin
  LStream := TMemoryStream.Create;
  try
    {$IF COMPILERVERSION <= 31.0}
    imgMultipartFormDataStream.Picture.Graphic.SaveToStream(LStream);
    {$ELSE}
    imgMultipartFormDataStream.Picture.SaveToStream(LStream);
    {$ENDIF}

    LResponse := TRequest.New.BaseURL(edtMultipartFormDataBaseURL.Text)
      .AddField('text', edtMultipartFormDataText.Text)
      .AddFile('file', lblMultipartFormDataFile.Caption)
      .AddFile('stream', LStream)
      .Put;
  finally
    LStream.Free;
  end;

  mmBody.Lines.Text := LResponse.Content;
  lblStatusCode.Caption := LResponse.StatusCode.ToString;
end;

procedure TFrmMain.btnPOSTClick(Sender: TObject);
var
  LResponse: IResponse;
begin
  LResponse := TRequest.New.BaseURL(edtBaseURL.Text)
    .Accept(edtAccept.Text)
    .AddBody(mmCustomBody.Lines.Text)
    .Post;

  mmBody.Lines.Text := LResponse.Content;
  lblStatusCode.Caption := LResponse.StatusCode.ToString;
end;

procedure TFrmMain.btnPUTClick(Sender: TObject);
var
  LResponse: IResponse;
begin
  LResponse := TRequest.New.BaseURL(edtBaseURL.Text)
    .Accept(edtAccept.Text)
    .AddBody(mmCustomBody.Lines.Text)
    .Put;

  mmBody.Lines.Text := LResponse.Content;
  lblStatusCode.Caption := LResponse.StatusCode.ToString;
end;

end.
