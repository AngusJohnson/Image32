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
  object pnlMemo: TPanel
    Left = 0
    Top = 0
    Width = 494
    Height = 400
    TabOrder = 1
    Visible = False
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 492
      Height = 41
      Align = alTop
      TabOrder = 0
      object btnCloseMemo: TButton
        Left = 4
        Top = 7
        Width = 75
        Height = 25
        Cancel = True
        Caption = '&Close'
        TabOrder = 0
        OnClick = btnCloseMemoClick
      end
      object rbBeziers: TRadioButton
        Left = 129
        Top = 13
        Width = 113
        Height = 17
        Caption = '&Bezier Paths'
        TabOrder = 1
        OnClick = rbBeziersClick
      end
      object rbFlat: TRadioButton
        Left = 246
        Top = 13
        Width = 113
        Height = 17
        Caption = '&Flat Paths'
        Checked = True
        TabOrder = 2
        TabStop = True
        OnClick = rbBeziersClick
      end
    end
    object Memo1: TMemo
      Left = 1
      Top = 42
      Width = 492
      Height = 357
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
  object pnlDisplayParent: TPanel
    Left = 176
    Top = 72
    Width = 273
    Height = 297
    TabOrder = 2
    object pnlSmooth: TPanel
      Left = 224
      Top = 1
      Width = 48
      Height = 295
      Align = alRight
      ParentBackground = False
      TabOrder = 1
      OnEnter = pnlSmoothEnter
      OnExit = pnlSmoothExit
      DesignSize = (
        48
        295)
      object TrackBar1: TTrackBar
        Left = 6
        Top = 17
        Width = 30
        Height = 260
        Anchors = [akLeft, akTop, akBottom]
        Min = 1
        Orientation = trVertical
        PageSize = 0
        Position = 3
        ShowSelRange = False
        TabOrder = 0
        TickStyle = tsNone
        OnChange = mnuShowBothClick
      end
    end
    object DisplayPanel: TPanel
      Left = 1
      Top = 1
      Width = 223
      Height = 295
      Align = alClient
      BevelInner = bvLowered
      BorderWidth = 16
      TabOrder = 0
      TabStop = True
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
        Checked = True
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16470
        OnClick = mnuShowBothClick
      end
      object mnuShowBoth: TMenuItem
        AutoCheck = True
        Caption = 'Show &Both Image and Polygons'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16450
        OnClick = mnuShowBothClick
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object mnuSmoothCurve: TMenuItem
        AutoCheck = True
        Caption = 'S&mooth Curve'
        Checked = True
        GroupIndex = 1
        ShortCut = 16461
        OnClick = mnuShowBothClick
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object mnuShowPolygonCoordinates: TMenuItem
        Caption = 'Show &Polygon Coordinates'
        GroupIndex = 1
        ShortCut = 16464
        OnClick = mnuShowPolygonCoordinatesClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'PNG Files|*.png'
    Left = 112
    Top = 144
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.png'
    Filter = 'PNG Image File (*.PNG)|*.png'
    Left = 112
    Top = 200
  end
end
