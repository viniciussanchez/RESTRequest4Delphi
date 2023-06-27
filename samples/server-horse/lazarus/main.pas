unit Main;

//{$mode objfpc}{$H+}
{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Horse;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    Bevel1: TBevel;
    btnStart: TBitBtn;
    btnStop: TBitBtn;
    edtMultipartFormDataText: TEdit;
    edtPort: TEdit;
    imgMultipartFormDataStream: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblMultipartFormDataFile: TLabel;
    lblPort: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblMultipartFormDataFileClick(Sender: TObject);
  private
    procedure Status;
    procedure Start;
    procedure Stop;
  public
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  Windows, fpjson, HTTPDefs;

{$R *.lfm}

{ TFrmMain }

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

procedure TFrmMain.FormShow(Sender: TObject);
begin
  btnStart.Click;
end;

procedure TFrmMain.lblMultipartFormDataFileClick(Sender: TObject);
begin
  if (lblMultipartFormDataFile.Caption <> EmptyStr) then
    ShellExecute(0, 'open', PChar(lblMultipartFormDataFile.Caption), nil, nil, SW_SHOWNORMAL);
end;

procedure TFrmMain.Status;
begin
  btnStop.Enabled := THorse.IsRunning;
  btnStart.Enabled := not THorse.IsRunning;
  edtPort.Enabled := not THorse.IsRunning;
end;

procedure DoPing(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('pong');
end;

procedure FormData(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  I: Integer;
  LFile: TMemoryStream;
begin
  if (Req.ContentFields.Field('stream').AsStream <> nil) then
  begin
    FrmMain.imgMultipartFormDataStream.Picture.Assign(nil);
  end;

  if Req.ContentFields.ContainsKey('text') then
    FrmMain.edtMultipartFormDataText.Text := Req.ContentFields.Field('text').AsString;

  //for I := 0 to Pred(Req.RawWebRequest.Files.Count) do
  //begin
  //  if (Req.RawWebRequest.Files.Items[I].FieldName = 'file') then
  //  begin
  //    LFile := TMemoryStream.Create;
  //    try
  //      LFile.LoadFromStream(Req.RawWebRequest.Files.Items[I].Stream);
  //      LFile.Position := 0;
  //      LFile.SaveToFile(ExtractFilePath(ParamStr(0)) + Req.RawWebRequest.Files.Items[I].FileName);
  //      lblMultipartFormDataFile.Caption := Format('%s%s', [ExtractFilePath(ParamStr(0)), Req.RawWebRequest.Files.Items[I].FileName]);
  //    finally
  //      LFile.Free;
  //    end;
  //  end;
  //end;

  for I := 0 to Pred(Req.RawWebRequest.Files.Count) do
  begin
    if (Req.RawWebRequest.Files[I].FieldName = 'file') then
    begin
      LFile := TMemoryStream.Create;
      try
        LFile.LoadFromStream(Req.RawWebRequest.Files[I].Stream);
        LFile.Position := 0;
        LFile.SaveToFile(ExtractFilePath(ParamStr(0)) + Req.RawWebRequest.Files[I].FileName);
        FrmMain.lblMultipartFormDataFile.Caption := Format('%s%s', [ExtractFilePath(ParamStr(0)), Req.RawWebRequest.Files[I].FileName]);
      finally
        LFile.Free;
      end;
    end;
  end;

  Res.Send('Ok').Status(200);
end;

procedure TFrmMain.Start;
begin
  //THorse.MaxConnections := 100;

  THorse.Get('ping', DoPing);

  THorse.Post('form-data/', FormData);
  THorse.Put('form-data/', FormData);

  THorse.Listen(StrToInt(edtPort.Text));
end;

procedure TFrmMain.Stop;
begin
  THorse.StopListen;
end;

end.

