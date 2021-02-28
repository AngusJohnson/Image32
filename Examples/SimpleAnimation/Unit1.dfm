object Form1: TForm1
  Left = 514
  Top = 310
  Caption = 'Simple animation'
  ClientHeight = 444
  ClientWidth = 261
  Color = clBtnFace
  TransparentColorValue = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDblClick = PnlMainDblClick
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object StatusBar1: TStatusBar
    Left = 0
    Top = 418
    Width = 261
    Height = 26
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SimpleText = ' Double click to slow the animation'
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
end
