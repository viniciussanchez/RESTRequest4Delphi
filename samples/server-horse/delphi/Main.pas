unit Main;

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
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Imaging.pngimage,
  Horse;

type
  TFrmMain = class(TForm)
  published
    pnlLeft: TPanel;
    Panel1: TPanel;
    Panel7: TPanel;
    Panel10: TPanel;

    lblPort: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblMultipartFormDataFile: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    lblContentTypeStream: TLabel;
    lblContentTypeText: TLabel;
    lblContentTypeFile: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblFilenameStream: TLabel;
    lblFilenameText: TLabel;
    lblFilenameFile: TLabel;

    btnStop: TBitBtn;
    btnStart: TBitBtn;

    edtPort: TEdit;
    edtMultipartFormDataText: TEdit;

    imgMultipartFormDataStream: TImage;

    bvlDivisao: TBevel;
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
  for I := 0 to Pred(Req.RawWebRequest.Files.Count) do
  begin
    var LContent := Req.RawWebRequest.Files.Items[I].Stream;
    var LContentType := Req.RawWebRequest.Files.Items[I].ContentType;
    var LFieldName := Req.RawWebRequest.Files.Items[I].FieldName;
    var LFileName :=  Req.RawWebRequest.Files.Items[I].FileName;

    if LFieldName.Equals('stream') then
    begin
      imgMultipartFormDataStream.Picture.Assign(nil);
    {$IF COMPILERVERSION >=32.0}
      imgMultipartFormDataStream.Picture.LoadFromStream(LContent);
      lblContentTypeStream.Caption := LContentType;
      lblFilenameStream.Caption := LFileName;
    {$ENDIF}
    end
    else if LFieldName.Equals('text') then
    begin
      var LReader := TStreamReader.Create(LContent, TEncoding.UTF8);
      try
        edtMultipartFormDataText.Text := LReader.ReadToEnd;
        lblContentTypeText.Caption := LContentType;
        lblFilenameText.Caption := LFileName;
      finally
        LReader.Free;
      end;
    end
    else if LFieldName.Equals('file') then
    begin
      LFile := TMemoryStream.Create;
      try
        LFile.LoadFromStream(LContent);
        LFile.Position := 0;
        LFile.SaveToFile(ExtractFilePath(ParamStr(0)) + LFilename);
        lblMultipartFormDataFile.Caption :=
          Format('%s%s', [ExtractFilePath(ParamStr(0)), LFilename]);
        lblContentTypeFile.Caption := LContentType;
        lblFilenameFile.Caption := LFileName;
      finally
        LFile.Free;
      end;
    end;
  end;

  Res.Send('Ok').Status(200);
end;

end.
