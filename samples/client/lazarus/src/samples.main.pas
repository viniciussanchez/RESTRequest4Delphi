unit Samples.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    btnDELETE: TButton;
    btnGET: TButton;
    btnPOST: TButton;
    btnPUT: TButton;
    edtAccept: TLabeledEdit;
    edtBaseURL: TLabeledEdit;
    Image1: TImage;
    Label1: TLabel;
    Label3: TLabel;
    lblStatusCode: TLabel;
    mmBody: TMemo;
    mmCustomBody: TMemo;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet6: TTabSheet;
    procedure btnDELETEClick(Sender: TObject);
    procedure btnGETClick(Sender: TObject);
    procedure btnPOSTClick(Sender: TObject);
    procedure btnPUTClick(Sender: TObject);
  private

  public

  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

uses RESTRequest4D;

{ TFrmMain }

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

