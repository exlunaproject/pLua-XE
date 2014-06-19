object frmMain: TfrmMain
  Left = 291
  Top = 162
  Caption = 'Associative Array Demo'
  ClientHeight = 103
  ClientWidth = 314
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblName: TLabel
    Left = 9
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Name:'
    Color = clBtnFace
    ParentColor = False
  end
  object lblValue: TLabel
    Left = 9
    Top = 48
    Width = 30
    Height = 13
    Caption = 'Value:'
    Color = clBtnFace
    ParentColor = False
  end
  object edName: TEdit
    Left = 15
    Top = 24
    Width = 217
    Height = 23
    TabOrder = 0
  end
  object edValue: TEdit
    Left = 15
    Top = 64
    Width = 217
    Height = 23
    TabOrder = 2
  end
  object btnRead: TButton
    Left = 232
    Top = 22
    Width = 75
    Height = 25
    Caption = 'Read'
    TabOrder = 1
    OnClick = btnReadClick
  end
  object btnWrite: TButton
    Left = 232
    Top = 62
    Width = 75
    Height = 25
    Caption = 'Write'
    TabOrder = 3
    OnClick = btnWriteClick
  end
end
