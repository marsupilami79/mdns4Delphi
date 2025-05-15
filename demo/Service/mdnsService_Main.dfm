object FormService: TFormService
  Left = 0
  Top = 0
  Caption = 'mDNS Service'
  ClientHeight = 552
  ClientWidth = 785
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 20
  object Button1: TButton
    Left = 20
    Top = 20
    Width = 94
    Height = 31
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 20
    Top = 59
    Width = 755
    Height = 484
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object IdTCPServer1: TIdTCPServer
    Bindings = <>
    DefaultPort = 12345
    OnExecute = IdTCPServer1Execute
    Left = 180
    Top = 20
  end
end
