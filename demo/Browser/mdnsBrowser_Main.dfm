object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 532
  ClientWidth = 629
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object ServiceNameLbl: TLabel
    Left = 10
    Top = 10
    Width = 85
    Height = 15
    Caption = 'ServiceNameLbl'
  end
  object ServicesLV: TListView
    Left = 10
    Top = 40
    Width = 400
    Height = 487
    Columns = <>
    TabOrder = 0
  end
  object Button1: TButton
    Left = 424
    Top = 40
    Width = 195
    Height = 55
    Caption = 'browse all types'
    TabOrder = 2
    OnClick = Button1Click
  end
  object ServiceTypeEdt: TEdit
    Left = 424
    Top = 168
    Width = 196
    Height = 28
    TabOrder = 3
    Text = '_https._tcp.local'
  end
  object Button2: TButton
    Left = 424
    Top = 104
    Width = 195
    Height = 55
    Caption = 'browse edit types'
    TabOrder = 1
    OnClick = Button2Click
  end
end
