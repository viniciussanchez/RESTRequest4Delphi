object FrmMercadoLibre: TFrmMercadoLibre
  Left = 0
  Top = 0
  Caption = 'Mercado Libre - API'
  ClientHeight = 435
  ClientWidth = 626
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnGetUserInfo: TButton
    Left = 32
    Top = 96
    Width = 121
    Height = 25
    Caption = 'Get User Info'
    TabOrder = 0
    OnClick = btnGetUserInfoClick
  end
  object memoResponse: TMemo
    Left = 32
    Top = 127
    Width = 561
    Height = 271
    ReadOnly = True
    TabOrder = 1
  end
  object editUserID: TEdit
    Left = 32
    Top = 32
    Width = 153
    Height = 21
    TabOrder = 2
    TextHint = 'Informe o ID do usu'#225'rio'
  end
  object editAccess_Token: TEdit
    Left = 32
    Top = 59
    Width = 297
    Height = 21
    TabOrder = 3
    TextHint = 'Informe um access_token v'#225'lido'
  end
end
