object MainForm: TMainForm
  Left = 514
  Top = 310
  Caption = 'RasterToSVG'
  ClientHeight = 426
  ClientWidth = 627
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 17
  object StatusBar1: TStatusBar
    Left = 0
    Top = 400
    Width = 627
    Height = 26
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
  object pnlDisplayParent: TPanel
    Left = 0
    Top = 0
    Width = 627
    Height = 400
    Align = alClient
    TabOrder = 1
    object pnlTop: TPanel
      Left = 1
      Top = 1
      Width = 625
      Height = 33
      Align = alTop
      TabOrder = 0
      object lblRaster: TLabel
        Left = 20
        Top = 6
        Width = 80
        Height = 17
        Caption = 'Raster Image:'
      end
      object lblSVG: TLabel
        Left = 330
        Top = 6
        Width = 67
        Height = 17
        Caption = 'SVG Image:'
      end
    end
    object pnlOptions: TPanel
      Left = 1
      Top = 365
      Width = 625
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object lblOptions: TLabel
        Left = 20
        Top = 6
        Width = 64
        Height = 17
        Caption = 'pnlOptions'
      end
      object ProgressBar: TProgressBar
        Left = 101
        Top = 8
        Width = 356
        Height = 17
        TabOrder = 0
        Visible = False
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 64
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open ...'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object mnuSaveAs: TMenuItem
        Caption = '&Save As ...'
        Enabled = False
        ShortCut = 16467
        OnClick = mnuSaveAsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Refresh1: TMenuItem
        Caption = '&Refresh'
        ShortCut = 16466
        OnClick = Refresh1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = Exit1Click
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object mnuEraseBackground: TMenuItem
        AutoCheck = True
        Caption = 'Erase Back&ground'
        Checked = True
        ShortCut = 16455
        OnClick = mnuOptionsClick
      end
      object mnuManualPaletteSizes: TMenuItem
        AutoCheck = True
        Caption = 'Manually Set &Palette Sizes'
        OnClick = mnuIniOptionsClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object ChangeSimpleness1: TMenuItem
        Caption = 'Change Simple&ness ...'
        ShortCut = 16462
        OnClick = ChangeSimpleness1Click
      end
      object mnuChangeSmoothness: TMenuItem
        Caption = 'Change S&moothness ...'
        ShortCut = 16461
        OnClick = mnuChangeSmoothnessClick
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object mnuTips: TMenuItem
        Caption = '&Tips'
        ShortCut = 112
        OnClick = mnuTipsClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = '&About ...'
        OnClick = About1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Image Files|*.bmp;*.jpg;*.gif;*.png'
    Left = 112
    Top = 144
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.svg'
    Filter = 'SVG Files|*.svg'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 112
    Top = 200
  end
end
