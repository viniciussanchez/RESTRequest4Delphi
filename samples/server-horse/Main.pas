unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Imaging.pngimage, Horse;

type
  TfrmMain = class(TForm)
    pnlLeft: TPanel;
    lblPort: TLabel;
    btnStop: TBitBtn;
    btnStart: TBitBtn;
    edtPort: TEdit;
    bvlDivisao: TBevel;
    Panel1: TPanel;
    Panel7: TPanel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel10: TPanel;
    imgMultipartFormDataStream: TImage;
    lblMultipartFormDataFile: TLabel;
    edtMultipartFormDataText: TEdit;
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblMultipartFormDataFileClick(Sender: TObject);
  private
    { Private declarations }
    procedure Status;
    procedure Start;
    procedure Stop;
    procedure FormData(pReq: THorseRequest; pRes: THorseResponse; pNext: TNextProc);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Vcl.Imaging.jpeg, Winapi.ShellApi, System.JSON, System.Win.ComObj, System.StrUtils, REST.JSON, Web.ReqMulti;

{$R *.dfm}

{ TfrmMain }

procedure ViewImageFromStream(pImage: TImage; pData: TStream);
var
  JPEGImage: TJPEGImage;
begin
  pData.Position := 0;
  JPEGImage := TJPEGImage.Create;
  try
    JPEGImage.LoadFromStream(pData);
    pImage.Picture.Assign(JPEGImage);
  finally
    JPEGImage.Free;
  end;
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  Start;
  Status;
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  Stop;
  Status;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if THorse.IsRunning then
    Stop;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  btnStart.Click;
end;

procedure TfrmMain.lblMultipartFormDataFileClick(Sender: TObject);
begin
  if (lblMultipartFormDataFile.Caption <> EmptyStr) then
    ShellExecute(0, 'open', PChar(lblMultipartFormDataFile.Caption), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmMain.Start;
begin
  THorse.MaxConnections := 100;

  THorse.Get('ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong')
    end);

  THorse.Post('form-data/', FormData);
  THorse.Put('form-data/', FormData);

  THorse.Listen(StrToInt(edtPort.Text));
end;

procedure TfrmMain.Status;
begin
  btnStop.Enabled := THorse.IsRunning;
  btnStart.Enabled := not THorse.IsRunning;
  edtPort.Enabled := not THorse.IsRunning;
end;

procedure TfrmMain.Stop;
begin
  THorse.StopListen;
end;

procedure TfrmMain.FormData(pReq: THorseRequest; pRes: THorseResponse;
  pNext: TNextProc);
var
  lFile: TMemoryStream;
  I: Integer;
begin

  //stream(arrived as file) - pReq.RawWebRequest.Files
  if (pReq.ContentFields.Field('stream').AsStream <> nil) then //Horse version 3.0.2
  begin
    imgMultipartFormDataStream.Picture.Assign(nil);
    {$IF COMPILERVERSION >=32.0}
    imgMultipartFormDataStream.Picture.LoadFromStream(pReq.ContentFields.Field('stream').AsStream); //Horse version 3.0.2
    {$ENDIF}
  end;

  //text
  if pReq.ContentFields.ContainsKey('text') then //Horse version 3.0.2
    edtMultipartFormDataText.Text := pReq.ContentFields.Field('text').AsString; //Horse version 3.0.2

  //file
  for I := 0 to Pred(pReq.RawWebRequest.Files.Count) do
  begin
    if (pReq.RawWebRequest.Files.Items[I].FieldName = 'file') then
    begin
      lFile := TMemoryStream.Create;
      try
        lFile.LoadFromStream(pReq.RawWebRequest.Files.Items[I].Stream);
        lFile.Position := 0;
        lFile.SaveToFile(ExtractFilePath(ParamStr(0)) + pReq.RawWebRequest.Files.Items[I].FileName);
        lblMultipartFormDataFile.Caption := Format('%s%s', [ExtractFilePath(ParamStr(0)), pReq.RawWebRequest.Files.Items[I].FileName]);
      finally
        lFile.Free;
      end;
    end;
  end;

  pRes.Send('Ok').Status(200);
end;

end.
