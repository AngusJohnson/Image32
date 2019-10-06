object FrmMain: TFrmMain
  Left = 524
  Top = 346
  Caption = 'Image32 - Layers & MixedPaths'
  ClientHeight = 413
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
  object pnlMain: TPanel
    Left = 0
    Top = 41
    Width = 458
    Height = 346
    Align = alClient
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnDblClick = pnlMainDblClick
    OnMouseDown = pnlMainMouseDown
    OnMouseMove = pnlMainMouseMove
    OnMouseUp = pnlMainMouseUp
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 387
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
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 458
    Height = 41
    Align = alTop
    TabOrder = 2
    object Label1: TLabel
      Left = 10
      Top = 13
      Width = 67
      Height = 14
      Caption = 'Curve &Type:'
      FocusControl = cbTypes
    end
    object Label2: TLabel
      Left = 212
      Top = 13
      Width = 63
      Height = 14
      Caption = 'Line &Width:'
      FocusControl = edWidth
    end
    object cbTypes: TComboBox
      Left = 85
      Top = 10
      Width = 113
      Height = 22
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Straight Line'
      OnChange = cbTypesChange
      Items.Strings = (
        'Straight Line'
        'CBezier'
        'QBeier'
        'CSpline'
        'QSpline')
    end
    object edWidth: TEdit
      Left = 283
      Top = 10
      Width = 32
      Height = 22
      TabOrder = 1
      Text = '5'
      OnChange = edWidthChange
    end
    object UpDown1: TUpDown
      Left = 315
      Top = 10
      Width = 16
      Height = 22
      Associate = edWidth
      Position = 5
      TabOrder = 2
    end
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
    Left = 232
    Top = 152
    object mnuMakePolyline: TMenuItem
      Caption = 'Make Poly&line Layer'
      ShortCut = 16460
      OnClick = mnuMakePolylineClick
    end
    object mnuMakePolygon: TMenuItem
      Caption = 'Make &Polygon Layer'
      ShortCut = 16464
      OnClick = mnuMakePolylineClick
    end
    object mnuEditLayer: TMenuItem
      Caption = '&Edit Layer'
      ShortCut = 16453
      OnClick = mnuEditLayerClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object mnuRotateButtons: TMenuItem
      Caption = '&Rotate Buttons'
      ShortCut = 16466
      OnClick = mnuRotateButtonsClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object mnuDeleteButton: TMenuItem
      Caption = 'D&elete Last Button'
      ShortCut = 16452
      OnClick = mnuDeleteButtonClick
    end
    object mnuDeleteAllButtonControls: TMenuItem
      Caption = 'Delete All &Button Controls'
      ShortCut = 16450
      OnClick = mnuDeleteAllButtonControlsClick
    end
    object mnuDeleteLayer: TMenuItem
      Caption = 'Delete &Layer'
      OnClick = mnuDeleteLayerClick
    end
  end
end
