unit Samples.Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Imaging.pngimage,
  Vcl.Mask;

type
  TMyCompletionHandlerWithError = TProc<TObject>;

  TFrmMain = class(TForm)
  published
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Panel8: TPanel;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet2: TTabSheet;
    edtBaseURL: TLabeledEdit;
    edtAccept: TLabeledEdit;
    edtMultipartFormDataBaseURL: TLabeledEdit;
    Label1: TLabel;
    lblStatusCode: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblMultipartFormDataFile: TLabel;
    lblRESTRequest4DelphiComponent: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lblMultipartFormDataFileContentType: TLabel;
    Label8: TLabel;
    lblMultipartFormDataStreamContentType: TLabel;
    edtMultipartFormDataText: TEdit;
    edtMultipartFormDataTextContentType: TEdit;
    btnDELETE: TButton;
    btnPUT: TButton;
    btnPOST: TButton;
    btnGET: TButton;
    btnMultipartFormDataPost: TButton;
    btnMultipartFormDataPut: TButton;
    mmCustomBody: TMemo;
    mmBody: TMemo;
    Splitter1: TSplitter;
    FDMemTable1: TFDMemTable;
    Image1: TImage;
    imgMultipartFormDataStream: TImage;
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
{$ELSEIF DEFINED(RR4D_ICS)}
  lblRESTRequest4DelphiComponent.Caption := 'RESTRequest4Delphi:  RR4D_ICS';
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
      {$IF NOT DEFINED(RR4D_SYNAPSE) AND NOT DEFINED (RR4D_ICS)}
        .AddText('text', edtMultipartFormDataText.Text, edtMultipartFormDataTextContentType.Text)
      {$ENDIF}
      {$IF NOT DEFINED(RR4D_ICS)}
        .AddFile('file', lblMultipartFormDataFile.Caption, lblMultipartFormDataFileContentType.Caption)
        .AddFile('stream', LStream)
      {$ENDIF}
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
      {$IF NOT DEFINED(RR4D_SYNAPSE) AND NOT DEFINED (RR4D_ICS)}
        .AddText('text', edtMultipartFormDataText.Text, edtMultipartFormDataTextContentType.Text)
      {$ENDIF}
      {$IF NOT DEFINED(RR4D_ICS)}
        .AddFile('file', lblMultipartFormDataFile.Caption, lblMultipartFormDataFileContentType.Caption)
        .AddFile('stream', LStream)
      {$ENDIF}
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
