object FrmMain: TFrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = '.:: Receiving text/files using multipart/form-data ::.'
  ClientHeight = 191
  ClientWidth = 984
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object bvlDivisao: TBevel
    AlignWithMargins = True
    Left = 213
    Top = 3
    Width = 5
    Height = 185
    Align = alLeft
    Shape = bsLeftLine
    ExplicitLeft = 344
    ExplicitTop = 144
    ExplicitHeight = 50
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 210
    Height = 191
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'pnlLeft'
    ShowCaption = False
    TabOrder = 0
    object lblPort: TLabel
      Left = 8
      Top = 19
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object btnStop: TBitBtn
      Left = 104
      Top = 50
      Width = 90
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 0
      OnClick = btnStopClick
    end
    object btnStart: TBitBtn
      Left = 8
      Top = 50
      Width = 90
      Height = 25
      Caption = 'Start'
      TabOrder = 1
      OnClick = btnStartClick
    end
    object edtPort: TEdit
      Left = 38
      Top = 16
      Width = 156
      Height = 21
      NumbersOnly = True
      TabOrder = 2
      Text = '9000'
    end
  end
  object Panel1: TPanel
    Left = 221
    Top = 0
    Width = 763
    Height = 191
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    object Panel7: TPanel
      Left = 0
      Top = 0
      Width = 763
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel7'
      ShowCaption = False
      TabOrder = 0
      object Label2: TLabel
        Left = 10
        Top = 2
        Width = 41
        Height = 15
        Caption = 'Stream'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label4: TLabel
        Left = 197
        Top = 2
        Width = 25
        Height = 15
        Caption = 'Text'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label5: TLabel
        Left = 385
        Top = 2
        Width = 19
        Height = 15
        Caption = 'File'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object Panel10: TPanel
      Left = 0
      Top = 25
      Width = 763
      Height = 166
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Panel10'
      ShowCaption = False
      TabOrder = 1
      object imgMultipartFormDataStream: TImage
        AlignWithMargins = True
        Left = 10
        Top = 2
        Width = 150
        Height = 150
        Margins.Left = 10
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Center = True
        Proportional = True
      end
      object lblMultipartFormDataFile: TLabel
        Left = 385
        Top = 5
        Width = 12
        Height = 13
        Cursor = crHandPoint
        Caption = '...'
        OnClick = lblMultipartFormDataFileClick
      end
      object edtMultipartFormDataText: TEdit
        Left = 197
        Top = 2
        Width = 150
        Height = 21
        TabOrder = 0
      end
    end
  end
end
