object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'REST Request - Samples'
  ClientHeight = 199
  ClientWidth = 861
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnSetMethod: TButton
    Left = 8
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Set method'
    TabOrder = 0
    OnClick = btnSetMethodClick
  end
  object btnSetBaseURL: TButton
    Left = 103
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Set base URL'
    TabOrder = 1
    OnClick = btnSetBaseURLClick
  end
  object btnSetDatasetAdapter: TButton
    Left = 430
    Top = 8
    Width = 131
    Height = 25
    Caption = 'Set dataset adapter'
    TabOrder = 2
    OnClick = btnSetDatasetAdapterClick
  end
  object btnGetMethod: TButton
    Left = 8
    Top = 39
    Width = 89
    Height = 25
    Caption = 'Get method'
    TabOrder = 3
    OnClick = btnGetMethodClick
  end
  object btnGetBaseURL: TButton
    Left = 103
    Top = 39
    Width = 89
    Height = 25
    Caption = 'Get base URL'
    TabOrder = 4
    OnClick = btnGetBaseURLClick
  end
  object btnGetResource: TButton
    Left = 198
    Top = 39
    Width = 89
    Height = 25
    Caption = 'Get resource'
    TabOrder = 5
    OnClick = btnGetResourceClick
  end
  object btnGetResourceSuffix: TButton
    Left = 293
    Top = 39
    Width = 131
    Height = 25
    Caption = 'Get resource suffix'
    TabOrder = 6
    OnClick = btnGetResourceSuffixClick
  end
  object btnSetResourceSuffix: TButton
    Left = 293
    Top = 8
    Width = 131
    Height = 25
    Caption = 'Set resource suffix'
    TabOrder = 7
    OnClick = btnSetResourceSuffixClick
  end
  object btnSetResource: TButton
    Left = 198
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Set resource'
    TabOrder = 8
    OnClick = btnSetResourceClick
  end
  object btnGetDatasetAdapter: TButton
    Left = 430
    Top = 39
    Width = 131
    Height = 25
    Caption = 'Get dataset adapter'
    TabOrder = 9
    OnClick = btnGetDatasetAdapterClick
  end
  object btnGetFullRequestURL: TButton
    Left = 430
    Top = 70
    Width = 131
    Height = 25
    Caption = 'Get full request URL'
    TabOrder = 10
    OnClick = btnGetFullRequestURLClick
  end
  object btnClearParams: TButton
    Left = 8
    Top = 70
    Width = 89
    Height = 25
    Caption = 'Clear params'
    TabOrder = 11
    OnClick = btnClearParamsClick
  end
  object btnAddParam: TButton
    Left = 103
    Top = 70
    Width = 89
    Height = 25
    Caption = 'Add param'
    TabOrder = 12
    OnClick = btnAddParamClick
  end
  object Button1: TButton
    Left = 198
    Top = 70
    Width = 89
    Height = 25
    Caption = 'Add header'
    TabOrder = 13
    OnClick = Button1Click
  end
  object btnClearBody: TButton
    Left = 8
    Top = 101
    Width = 89
    Height = 25
    Caption = 'Clear body'
    TabOrder = 14
    OnClick = btnClearBodyClick
  end
  object btnAddBodyWithString: TButton
    Left = 103
    Top = 101
    Width = 184
    Height = 25
    Caption = 'Add body with string'
    TabOrder = 15
    OnClick = btnAddBodyWithStringClick
  end
  object btnAddBodyWithJSONObject: TButton
    Left = 293
    Top = 101
    Width = 268
    Height = 25
    Caption = 'Add body with JSON Object'
    TabOrder = 16
    OnClick = btnAddBodyWithJSONObjectClick
  end
  object btnAddBodyWithObject: TButton
    Left = 8
    Top = 132
    Width = 279
    Height = 25
    Caption = 'Add body with object'
    TabOrder = 17
    OnClick = btnAddBodyWithObjectClick
  end
  object btnJWTAuthorizationToken: TButton
    Left = 293
    Top = 132
    Width = 268
    Height = 25
    Caption = 'JWT Authorization Token - Sample'
    TabOrder = 18
    OnClick = btnJWTAuthorizationTokenClick
  end
  object btnBasicAuthorization: TButton
    Left = 8
    Top = 163
    Width = 184
    Height = 25
    Caption = 'Basic Authorization - Sample'
    TabOrder = 19
    OnClick = btnBasicAuthorizationClick
  end
  object btnExecuteRequest: TButton
    Left = 430
    Top = 163
    Width = 131
    Height = 25
    Caption = 'Execute request'
    TabOrder = 20
    OnClick = btnExecuteRequestClick
  end
  object btnClearBasicAuthentication: TButton
    Left = 198
    Top = 163
    Width = 89
    Height = 25
    Caption = 'Clear Basic Auth'
    TabOrder = 21
    OnClick = btnClearBasicAuthenticationClick
  end
  object btnGetStatusCode: TButton
    Left = 293
    Top = 70
    Width = 131
    Height = 25
    Caption = 'Get status code'
    TabOrder = 22
    OnClick = btnGetStatusCodeClick
  end
  object btnExecuteAsync: TButton
    Left = 293
    Top = 163
    Width = 131
    Height = 25
    Caption = 'Execute async'
    TabOrder = 23
    OnClick = btnExecuteAsyncClick
  end
  object Button2: TButton
    Left = 567
    Top = 39
    Width = 131
    Height = 25
    Caption = 'Get timeout'
    TabOrder = 24
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 567
    Top = 8
    Width = 131
    Height = 25
    Caption = 'Set timeout'
    TabOrder = 25
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 567
    Top = 101
    Width = 131
    Height = 25
    Caption = 'Get accept'
    TabOrder = 26
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 567
    Top = 70
    Width = 131
    Height = 25
    Caption = 'Set accept'
    TabOrder = 27
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 567
    Top = 163
    Width = 131
    Height = 25
    Caption = 'Get accept charset'
    TabOrder = 28
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 567
    Top = 132
    Width = 131
    Height = 25
    Caption = 'Set accept charset'
    TabOrder = 29
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 704
    Top = 8
    Width = 149
    Height = 25
    Caption = 'Set accept encoding'
    TabOrder = 30
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 704
    Top = 39
    Width = 149
    Height = 25
    Caption = 'Get accept encoding'
    TabOrder = 31
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 704
    Top = 101
    Width = 149
    Height = 25
    Caption = 'Get raise exception on 500'
    TabOrder = 32
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 704
    Top = 70
    Width = 149
    Height = 25
    Caption = 'Set raise exception on 500'
    TabOrder = 33
    OnClick = Button11Click
  end
  object FDMemTable: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 40
    Top = 136
  end
end
