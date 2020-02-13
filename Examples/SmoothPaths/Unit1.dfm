object FrmMain: TFrmMain
  Left = 554
  Top = 394
  Width = 482
  Height = 470
  Caption = 'Image32 - Layers & SmoothPaths'
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
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object pnlMain: TPanel
    Left = 0
    Top = 43
    Width = 466
    Height = 343
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
    Top = 386
    Width = 466
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
    Width = 466
    Height = 43
    Align = alTop
    TabOrder = 2
    object Label2: TLabel
      Left = 8
      Top = 13
      Width = 63
      Height = 14
      Caption = 'Line &Width:'
      FocusControl = edWidth
    end
    object lblPenColor: TLabel
      Left = 317
      Top = 13
      Width = 56
      Height = 14
      Alignment = taRightJustify
      Caption = '&Pen Color:'
    end
    object lblFillColor: TLabel
      Left = 147
      Top = 13
      Width = 47
      Height = 14
      Alignment = taRightJustify
      Caption = '&Fill Color:'
    end
    object Shape1: TShape
      Left = 288
      Top = 12
      Width = 18
      Height = 18
    end
    object edWidth: TEdit
      Left = 77
      Top = 10
      Width = 32
      Height = 22
      TabOrder = 0
      Text = '5'
      OnChange = edWidthChange
    end
    object UpDown1: TUpDown
      Left = 109
      Top = 10
      Width = 16
      Height = 22
      Associate = edWidth
      Min = 1
      Max = 25
      Position = 5
      TabOrder = 3
    end
    object edPenColor: TEdit
      Left = 380
      Top = 10
      Width = 73
      Height = 22
      TabOrder = 2
      OnChange = edPenColorChange
    end
    object edFillColor: TEdit
      Left = 201
      Top = 10
      Width = 73
      Height = 22
      TabOrder = 1
      OnChange = edPenColorChange
    end
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 93
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
    Left = 192
    Top = 93
    object mnuSmoothSym: TMenuItem
      AutoCheck = True
      Caption = 'Smooth - symmetric'
      Checked = True
      GroupIndex = 1
      RadioItem = True
      ShortCut = 16461
      OnClick = mnuSharpNoHdlsClick
    end
    object mnuSmoothAsym: TMenuItem
      AutoCheck = True
      Caption = 'Smooth - asymmetric'
      GroupIndex = 1
      RadioItem = True
      ShortCut = 16449
      OnClick = mnuSharpNoHdlsClick
    end
    object mnuSharpWithHdls: TMenuItem
      AutoCheck = True
      Caption = 'Sharp - with handles'
      GroupIndex = 1
      RadioItem = True
      ShortCut = 16456
      OnClick = mnuSharpNoHdlsClick
    end
    object mnuSharpNoHdls: TMenuItem
      AutoCheck = True
      Caption = 'Sharp - without handles'
      GroupIndex = 1
      RadioItem = True
      ShortCut = 16472
      OnClick = mnuSharpNoHdlsClick
    end
    object N5: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object mnuMakePolyline: TMenuItem
      Caption = 'Convert Button Path to Poly&line Layer '
      GroupIndex = 1
      ShortCut = 16460
      OnClick = mnuMakePolylineClick
    end
    object mnuMakePolygon: TMenuItem
      Caption = 'Convert Button Path to &Polygon Layer '
      GroupIndex = 1
      ShortCut = 16464
      OnClick = mnuMakePolylineClick
    end
    object mnuEditLayer: TMenuItem
      Caption = '&Edit Layer'
      GroupIndex = 1
      ShortCut = 16453
      OnClick = mnuEditLayerClick
    end
    object N3: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object mnuRotateButtons: TMenuItem
      Caption = '&Rotate Buttons'
      GroupIndex = 1
      ShortCut = 16466
      OnClick = mnuRotateButtonsClick
    end
    object N4: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object mnuDeleteButton: TMenuItem
      Caption = 'D&elete Last Button'
      GroupIndex = 1
      ShortCut = 46
      OnClick = mnuDeleteButtonClick
    end
    object mnuDeleteAllButtonControls: TMenuItem
      Caption = 'Delete All &Button Controls'
      GroupIndex = 1
      ShortCut = 16450
      OnClick = mnuDeleteAllButtonControlsClick
    end
    object mnuDeleteLayer: TMenuItem
      Caption = 'Delete &Layer'
      GroupIndex = 1
      ShortCut = 16473
      OnClick = mnuDeleteLayerClick
    end
  end
end
