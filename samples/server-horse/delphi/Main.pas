unit Main;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Imaging.pngimage, Horse;

type
  TFrmMain = class(TForm)
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
    procedure Status;
    procedure Start;
    procedure Stop;
    procedure FormData(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
  end;

var
  FrmMain: TFrmMain;

implementation

uses Vcl.Imaging.jpeg, Winapi.ShellApi, System.JSON, System.Win.ComObj, System.StrUtils, REST.JSON, Web.ReqMulti;

{$R *.dfm}

procedure ViewImageFromStream(pImage: TImage; pData: TStream);
var
  LJPEGImage: TJPEGImage;
begin
  pData.Position := 0;
  LJPEGImage := TJPEGImage.Create;
  try
    LJPEGImage.LoadFromStream(pData);
    pImage.Picture.Assign(LJPEGImage);
  finally
    LJPEGImage.Free;
  end;
end;

procedure TFrmMain.btnStartClick(Sender: TObject);
begin
  Start;
  Status;
end;

procedure TFrmMain.btnStopClick(Sender: TObject);
begin
  Stop;
  Status;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  if THorse.IsRunning then
    Stop;
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  btnStart.Click;
end;

procedure TFrmMain.lblMultipartFormDataFileClick(Sender: TObject);
begin
  if (lblMultipartFormDataFile.Caption <> EmptyStr) then
    ShellExecute(0, 'open', PChar(lblMultipartFormDataFile.Caption), nil, nil, SW_SHOWNORMAL);
end;

procedure TFrmMain.Start;
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

procedure TFrmMain.Status;
begin
  btnStop.Enabled := THorse.IsRunning;
  btnStart.Enabled := not THorse.IsRunning;
  edtPort.Enabled := not THorse.IsRunning;
end;

procedure TFrmMain.Stop;
begin
  THorse.StopListen;
end;

procedure TFrmMain.FormData(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  I: Integer;
  LFile: TMemoryStream;
begin
  if (Req.ContentFields.Field('stream').AsStream <> nil) then
  begin
    imgMultipartFormDataStream.Picture.Assign(nil);
    {$IF COMPILERVERSION >=32.0}
      imgMultipartFormDataStream.Picture.LoadFromStream(Req.ContentFields.Field('stream').AsStream);
    {$ENDIF}
  end;

  if Req.ContentFields.ContainsKey('text') then
    edtMultipartFormDataText.Text := Req.ContentFields.Field('text').AsString;

  for I := 0 to Pred(Req.RawWebRequest.Files.Count) do
  begin
    if (Req.RawWebRequest.Files.Items[I].FieldName = 'file') then
    begin
      LFile := TMemoryStream.Create;
      try
        LFile.LoadFromStream(Req.RawWebRequest.Files.Items[I].Stream);
        LFile.Position := 0;
        LFile.SaveToFile(ExtractFilePath(ParamStr(0)) + Req.RawWebRequest.Files.Items[I].FileName);
        lblMultipartFormDataFile.Caption :=
          Format('%s%s', [ExtractFilePath(ParamStr(0)), Req.RawWebRequest.Files.Items[I].FileName]);
      finally
        LFile.Free;
      end;
    end;
  end;

  Res.Send('Ok').Status(200);
end;

end.
