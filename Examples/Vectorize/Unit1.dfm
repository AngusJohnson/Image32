object Form1: TForm1
  Left = 514
  Top = 310
  Caption = 'Image32: Vectorize'
  ClientHeight = 439
  ClientWidth = 552
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
  PixelsPerInch = 96
  TextHeight = 16
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
    Width = 552
    Height = 413
    Align = alClient
    TabOrder = 1
    Visible = False
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 550
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
      object rbBeziers: TRadioButton
        Left = 187
        Top = 14
        Width = 158
        Height = 14
        Caption = 'Smoothed &Bezier Paths'
        TabOrder = 2
        OnClick = rbBeziersClick
      end
      object rbFlat: TRadioButton
        Left = 351
        Top = 13
        Width = 188
        Height = 17
        Caption = 'Smoothed && &Flattened Paths'
        Checked = True
        TabOrder = 3
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
      Width = 550
      Height = 370
      Align = alClient
      HideSelection = False
      ReadOnly = True
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
      Left = 215
      Top = 1
      Width = 57
      Height = 295
      Align = alRight
      ParentBackground = False
      TabOrder = 1
      OnEnter = pnlSmoothEnter
      OnExit = pnlSmoothExit
      DesignSize = (
        57
        295)
      object Label1: TLabel
        Left = 5
        Top = 6
        Width = 50
        Height = 33
        Caption = 'Smooth'#10'Amount'
      end
      object TrackBar1: TTrackBar
        Left = 15
        Top = 46
        Width = 27
        Height = 239
        Anchors = [akLeft, akTop, akBottom]
        Min = 1
        Orientation = trVertical
        PageSize = 0
        Position = 6
        ShowSelRange = False
        TabOrder = 0
        TickStyle = tsNone
        OnChange = TrackBar1Change
      end
    end
    object DisplayPanel: TPanel
      Left = 1
      Top = 1
      Width = 214
      Height = 295
      Align = alClient
      BevelInner = bvLowered
      BorderWidth = 16
      TabOrder = 0
      TabStop = True
      ExplicitWidth = 223
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
        Caption = 'Monochrome Im&age'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16433
        OnClick = mnuShowSimplifiedClick
      end
      object mnuShowRawPoly: TMenuItem
        Caption = '&Raw Vector Polygons'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16434
        OnClick = mnuShowSimplifiedClick
      end
      object mnuShowSimplified: TMenuItem
        Caption = 'Simplified Ve&ctor Polygons'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16435
        OnClick = mnuShowSimplifiedClick
      end
      object mnuShowSmoothedOnly: TMenuItem
        Caption = 'Smoothe&d (unsimplified) Polygons'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16436
        OnClick = mnuShowSimplifiedClick
      end
      object mnuShowSimplifiedSmoothed: TMenuItem
        Caption = 'Smooth&ed && Simplified Polygons'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16437
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
