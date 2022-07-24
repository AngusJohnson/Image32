object Form1: TForm1
  Left = 514
  Top = 310
  Caption = 'Image32: Vectorize'
  ClientHeight = 435
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 16
  object StatusBar1: TStatusBar
    Left = 0
    Top = 409
    Width = 569
    Height = 26
    Panels = <
      item
        Width = 120
      end
      item
        Width = 500
      end>
    ParentFont = True
    UseSystemFont = False
  end
  object pnlSmooth: TPanel
    Left = 512
    Top = 0
    Width = 57
    Height = 409
    Align = alRight
    ParentBackground = False
    TabOrder = 1
    OnEnter = pnlSmoothEnter
    OnExit = pnlSmoothExit
    object lblSmooth: TLabel
      Left = 6
      Top = 6
      Width = 44
      Height = 32
      Alignment = taCenter
      Caption = 'Smooth'#10'Amount'
    end
    object lblSimplify: TLabel
      Left = 5
      Top = 206
      Width = 45
      Height = 32
      Alignment = taCenter
      Caption = 'Simplify'#10'Amount'
    end
    object TrackBar1: TTrackBar
      Left = 15
      Top = 56
      Width = 27
      Height = 118
      Max = 50
      Orientation = trVertical
      PageSize = 0
      Position = 30
      TabOrder = 0
      TickStyle = tsNone
      OnChange = TrackBar1Change
    end
    object TrackBar2: TTrackBar
      Left = 15
      Top = 256
      Width = 27
      Height = 118
      Orientation = trVertical
      PageSize = 0
      Position = 4
      TabOrder = 1
      TickStyle = tsNone
      OnChange = TrackBar1Change
    end
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
      object SaveAs1: TMenuItem
        Caption = '&Save As ...'
        ShortCut = 16467
        OnClick = SaveAs1Click
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
      object mnuShowMonoImage: TMenuItem
        Caption = '&Monochrome Image'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16458
        OnClick = mnuShowSimplifiedClick
      end
      object mnuShowRawPoly: TMenuItem
        Caption = '&Raw Vectors'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16459
        OnClick = mnuShowSimplifiedClick
      end
      object mnuShowSimplifiedSmoothed: TMenuItem
        Caption = '&Smoothed && Simplified '
        Checked = True
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16460
        OnClick = mnuShowSimplifiedClick
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object mnuHighlightVertices: TMenuItem
        AutoCheck = True
        Caption = '&Highlight Vertices'
        GroupIndex = 2
        ShortCut = 16464
        OnClick = mnuHighlightVerticesClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Image Files|*.bmp;*.jpg;*.png'
    Left = 112
    Top = 144
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.png'
    Filter = 'SVG Image|*.svg'
    Left = 112
    Top = 200
  end
end
