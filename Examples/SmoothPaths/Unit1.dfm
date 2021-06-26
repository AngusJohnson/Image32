object FrmMain: TFrmMain
  Left = 554
  Top = 394
  Caption = 'SmoothPaths'
  ClientHeight = 406
  ClientWidth = 466
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  OnCreate = FormCreate
  OnDblClick = pnlMainDblClick
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseDown = pnlMainMouseDown
  OnMouseMove = pnlMainMouseMove
  OnMouseUp = pnlMainMouseUp
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object StatusBar1: TStatusBar
    Left = 0
    Top = 380
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
    ParentBackground = False
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 13
      Width = 63
      Height = 14
      Caption = 'Line &Width:'
      FocusControl = edWidth
    end
    object lblPenColor: TLabel
      Left = 140
      Top = 14
      Width = 53
      Height = 14
      Alignment = taRightJustify
      Caption = 'Line&Color:'
    end
    object edWidth: TEdit
      Left = 77
      Top = 10
      Width = 32
      Height = 22
      TabOrder = 0
      Text = '5'
      OnChange = edPenColorChange
    end
    object UpDown1: TUpDown
      Left = 109
      Top = 10
      Width = 16
      Height = 22
      Associate = edWidth
      Min = 1
      Max = 50
      Position = 5
      TabOrder = 2
    end
    object edPenColor: TEdit
      Left = 199
      Top = 10
      Width = 73
      Height = 22
      TabOrder = 1
      Text = '$FF000099'
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
    Left = 192
    Top = 101
    object mnuSmoothSym: TMenuItem
      AutoCheck = True
      Caption = 'Smooth - symmetric'
      Checked = True
      GroupIndex = 1
      RadioItem = True
      ShortCut = 16461
      OnClick = mnuSharpWithHdlsClick
    end
    object mnuSmoothAsym: TMenuItem
      AutoCheck = True
      Caption = 'Smooth - asymmetric'
      GroupIndex = 1
      RadioItem = True
      ShortCut = 16449
      OnClick = mnuSharpWithHdlsClick
    end
    object mnuSharpWithHdls: TMenuItem
      AutoCheck = True
      Caption = 'Sharp - with handles'
      GroupIndex = 1
      RadioItem = True
      ShortCut = 16456
      OnClick = mnuSharpWithHdlsClick
    end
    object mnuSharpNoHdls: TMenuItem
      AutoCheck = True
      Caption = 'Sharp - without handles'
      GroupIndex = 1
      RadioItem = True
      ShortCut = 16472
      OnClick = mnuSharpWithHdlsClick
    end
    object N5: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object mnuRotateButtons: TMenuItem
      Caption = '&Rotate Button'
      GroupIndex = 1
      ShortCut = 16466
      OnClick = mnuRotateButtonsClick
    end
    object N4: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object mnuDeleteLast: TMenuItem
      Caption = 'D&elete Last Button'
      GroupIndex = 1
      ShortCut = 46
      OnClick = mnuDeleteLastClick
    end
    object mnuDeletePath: TMenuItem
      Caption = 'Delete &SmoothPath'
      GroupIndex = 1
      OnClick = mnuDeletePathClick
    end
  end
end
