object Form1: TForm1
  Left = 514
  Top = 310
  Caption = 'Image32: Vectorize'
  ClientHeight = 439
  ClientWidth = 552
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 552
    Height = 413
    Align = alClient
    TabOrder = 0
    ExplicitTop = -6
    ExplicitWidth = 324
    ExplicitHeight = 418
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 413
    Width = 552
    Height = 26
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open ...'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = Exit1Click
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object mnuShowImage: TMenuItem
        AutoCheck = True
        Caption = 'Show Monochrome &Image'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16457
        OnClick = mnuShowBothClick
      end
      object mnuShowPolygons: TMenuItem
        AutoCheck = True
        Caption = 'Show &Vector Polygons'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16470
        OnClick = mnuShowBothClick
      end
      object mnuShowBoth: TMenuItem
        AutoCheck = True
        Caption = 'Show &Both Image and Polygons'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16450
        OnClick = mnuShowBothClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'PNG Files|*.png'
    Left = 112
    Top = 144
  end
end
