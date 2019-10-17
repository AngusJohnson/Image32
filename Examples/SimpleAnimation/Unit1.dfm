object Form1: TForm1
  Left = 514
  Top = 310
  Caption = 'Image32: Simple animation'
  ClientHeight = 444
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 324
    Height = 418
    Align = alClient
    TabOrder = 0
    OnDblClick = Panel1DblClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 418
    Width = 324
    Height = 26
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SimpleText = ' Double click to slow animation'
    UseSystemFont = False
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = Exit1Click
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 120
    Top = 152
  end
end
