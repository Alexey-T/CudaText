object Form1: TForm1
  Left = 268
  Top = 199
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 291
  ClientWidth = 603
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 436
    Height = 291
    Align = alLeft
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button1: TButton
    Left = 440
    Top = 120
    Width = 155
    Height = 25
    Caption = 'Load big lib- bad'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 440
    Top = 88
    Width = 155
    Height = 25
    Caption = 'Load small lib- ok'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 440
    Top = 152
    Width = 155
    Height = 25
    Caption = 'Load full lib'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 440
    Top = 240
    Width = 155
    Height = 25
    Caption = 'Analz'
    TabOrder = 4
    OnClick = Button4Click
  end
end
