object Form1: TForm1
  Left = 514
  Top = 310
  Caption = 'Image32: Vectorize'
  ClientHeight = 870
  ClientWidth = 1138
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -26
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 192
  TextHeight = 31
  object StatusBar1: TStatusBar
    Left = 0
    Top = 818
    Width = 1138
    Height = 52
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <
      item
        Width = 240
      end
      item
        Width = 1000
      end>
    ParentFont = True
    UseSystemFont = False
    ExplicitTop = 817
    ExplicitWidth = 1124
  end
  object pnlSmooth: TPanel
    Left = 1024
    Top = 0
    Width = 114
    Height = 818
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alRight
    ParentBackground = False
    TabOrder = 1
    OnEnter = pnlSmoothEnter
    OnExit = pnlSmoothExit
    ExplicitLeft = 1010
    ExplicitHeight = 817
    object lblSmooth: TLabel
      Left = 12
      Top = 412
      Width = 91
      Height = 62
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Alignment = taCenter
      Caption = 'Smooth'#10'Amount'
    end
    object lblSimplify: TLabel
      Left = 10
      Top = 36
      Width = 91
      Height = 62
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Alignment = taCenter
      Caption = 'Simplify'#10'Amount'
    end
    object TrackBar1: TTrackBar
      Left = 30
      Top = 512
      Width = 54
      Height = 236
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Max = 50
      Orientation = trVertical
      PageSize = 0
      Position = 17
      TabOrder = 1
      ThumbLength = 40
      TickStyle = tsNone
      OnChange = TrackBar1Change
    end
    object TrackBar2: TTrackBar
      Left = 38
      Top = 144
      Width = 54
      Height = 236
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Orientation = trVertical
      PageSize = 0
      Position = 3
      TabOrder = 0
      ThumbLength = 40
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
