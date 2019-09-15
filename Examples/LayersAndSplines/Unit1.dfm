object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Image32 - Layers & Splines'
  ClientHeight = 414
  ClientWidth = 458
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
    Width = 458
    Height = 388
    Align = alClient
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnDblClick = Panel1DblClick
    OnMouseDown = Panel1MouseDown
    OnMouseMove = Panel1MouseMove
    OnMouseUp = Panel1MouseUp
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 388
    Width = 458
    Height = 26
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SimpleText = 
      ' Double-click to add a control point. Right click for popup menu' +
      ' options'
    UseSystemFont = False
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      object N1: TMenuItem
        Caption = '-'
      end
      object CopytoClipboard1: TMenuItem
        Caption = '&Copy to Clipboard'
        ShortCut = 16451
        OnClick = CopytoClipboard1Click
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
    object View1: TMenuItem
      Caption = '&View'
      object mnuHideControls: TMenuItem
        AutoCheck = True
        Caption = '&Hide Designer Layers'
        GroupIndex = 1
        OnClick = mnuHideControlsClick
      end
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 232
    Top = 152
    object mnuAddControlPoint: TMenuItem
      Caption = '&Add Control Point'
      OnClick = mnuAddControlPointClick
    end
    object mnuEndPath: TMenuItem
      Caption = 'E&nd Path'
      ShortCut = 16462
      OnClick = mnuEndPathClick
    end
    object mnuClosePath: TMenuItem
      Caption = 'Close Pat&h'
      ShortCut = 16456
      OnClick = mnuClosePathClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object mnuDeleteButton: TMenuItem
      Caption = 'D&elete Button Control'
    end
    object mnuDeleteAllButtonControls: TMenuItem
      Caption = 'Delete All &Button Controls'
      ShortCut = 16450
      OnClick = mnuDeleteAllButtonControlsClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object mnuDeleteLayer: TMenuItem
      Caption = '&Delete Layer'
      ShortCut = 16452
      OnClick = mnuDeleteLayerClick
    end
    object mnuEditLayer: TMenuItem
      Caption = '&Edit Layer'
      ShortCut = 16453
      OnClick = mnuEditLayerClick
    end
  end
end
