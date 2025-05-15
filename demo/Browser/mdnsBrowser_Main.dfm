object FormBrowser: TFormBrowser
  Left = 0
  Top = 0
  Caption = 'mDNS Browser'
  ClientHeight = 532
  ClientWidth = 880
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
    Left = 0
    Top = 40
    Width = 584
    Height = 487
    Columns = <
      item
        Caption = 'Name'
        MinWidth = 250
        Width = 350
      end
      item
        Caption = 'IP'
        MinWidth = 200
        Width = 200
      end>
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Button1: TButton
    Left = 793
    Top = 40
    Width = 81
    Height = 32
    Caption = 'get types'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 592
    Top = 72
    Width = 195
    Height = 55
    Caption = 'Browse'
    TabOrder = 1
    OnClick = Button2Click
  end
  object ServiceTypeEdt: TComboBox
    Left = 592
    Top = 40
    Width = 196
    Height = 23
    Sorted = True
    TabOrder = 3
    Text = '_https._tcp.local'
  end
end
