object Form1: TForm1
  Left = 514
  Top = 310
  Caption = 'Image32: Vectorize'
  ClientHeight = 436
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
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 16
  object StatusBar1: TStatusBar
    Left = 0
    Top = 410
    Width = 569
    Height = 26
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
  object pnlSmooth: TPanel
    Left = 512
    Top = 0
    Width = 57
    Height = 410
    Align = alRight
    ParentBackground = False
    TabOrder = 1
    OnEnter = pnlSmoothEnter
    OnExit = pnlSmoothExit
    DesignSize = (
      57
      410)
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
      Height = 138
      Anchors = [akLeft, akTop, akBottom]
      Orientation = trVertical
      PageSize = 0
      Position = 3
      TabOrder = 0
      TickStyle = tsNone
      OnChange = TrackBar1Change
    end
    object TrackBar2: TTrackBar
      Left = 15
      Top = 256
      Width = 27
      Height = 138
      Anchors = [akLeft, akTop, akBottom]
      Max = 5
      Orientation = trVertical
      PageSize = 0
      Position = 2
      TabOrder = 1
      TickStyle = tsNone
      OnChange = TrackBar1Change
    end
  end
  object pnlMemo: TPanel
    Left = 0
    Top = 0
    Width = 512
    Height = 410
    Align = alClient
    TabOrder = 2
    Visible = False
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 510
      Height = 41
      Align = alTop
      TabOrder = 0
      object btnCloseMemo: TButton
        Left = 5
        Top = 9
        Width = 75
        Height = 25
        Cancel = True
        Caption = '&Close'
        Default = True
        TabOrder = 0
        OnClick = btnCloseMemoClick
      end
      object rbFlat: TRadioButton
        Left = 186
        Top = 13
        Width = 188
        Height = 17
        Caption = 'Smoothed && &Flattened Paths'
        Checked = True
        TabOrder = 2
        TabStop = True
        OnClick = rbBeziersClick
      end
      object rbRaw: TRadioButton
        Left = 95
        Top = 13
        Width = 85
        Height = 17
        Caption = '&Raw Paths'
        TabOrder = 1
        OnClick = rbBeziersClick
      end
    end
    object Memo1: TMemo
      Left = 1
      Top = 42
      Width = 510
      Height = 367
      Align = alClient
      HideSelection = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
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
        Caption = 'Save &As ...'
        ShortCut = 16449
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
        Caption = 'Monochrome Im&age'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16461
        OnClick = mnuShowSimplifiedClick
      end
      object mnuShowRawPoly: TMenuItem
        Caption = '&Raw Vector Polygons'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16466
        OnClick = mnuShowSimplifiedClick
      end
      object mnuShowSimplifiedSmoothed: TMenuItem
        Caption = 'Smooth&ed && Simplified Polygons'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16467
        OnClick = mnuShowSimplifiedClick
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object mnuShowPolygonCoordinates: TMenuItem
        Caption = 'Show &Polygon Coordinates'
        GroupIndex = 2
        ShortCut = 16464
        OnClick = mnuShowPolygonCoordinatesClick
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
    Filter = 'Image Files|*.bmp;*.jpg;*.png'
    Left = 112
    Top = 200
  end
end
