object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 16
    Top = 47
    Width = 604
    Height = 387
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object IdTCPServer1: TIdTCPServer
    Bindings = <>
    DefaultPort = 12345
    OnExecute = IdTCPServer1Execute
    Left = 144
    Top = 16
  end
end
