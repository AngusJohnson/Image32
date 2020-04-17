object FrmMain: TFrmMain
  Left = 554
  Top = 394
  Caption = 'Image, Arrows & Text'
  ClientHeight = 467
  ClientWidth = 473
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
  object StatusBar1: TStatusBar
    Left = 0
    Top = 441
    Width = 473
    Height = 26
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SimpleText = 
      ' Double-click adds a control point. Right click for popup menu o' +
      'ptions'
    UseSystemFont = False
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 473
    Height = 105
    Align = alTop
    TabOrder = 0
    object Label2: TLabel
      Left = 14
      Top = 45
      Width = 63
      Height = 14
      Alignment = taRightJustify
      Caption = 'Line &Width:'
      FocusControl = edWidth
    end
    object lblFillColor: TLabel
      Left = 143
      Top = 45
      Width = 47
      Height = 14
      Alignment = taRightJustify
      Caption = 'Fill &Color:'
      Color = clBtnFace
      FocusControl = edFillColor
      ParentColor = False
      Transparent = True
    end
    object lblPenColor: TLabel
      Left = 317
      Top = 45
      Width = 56
      Height = 14
      Alignment = taRightJustify
      Caption = 'Pe&n Color:'
      FocusControl = edPenColor
    end
    object lblArrowStart: TLabel
      Left = 24
      Top = 75
      Width = 109
      Height = 14
      Alignment = taRightJustify
      Caption = 'Arrow at Line &Start:'
      FocusControl = cbArrowStart
    end
    object lblArrowEnd: TLabel
      Left = 240
      Top = 73
      Width = 103
      Height = 14
      Alignment = taRightJustify
      Caption = 'Arrow at Line &End:'
      FocusControl = cbArrowEnd
    end
    object Label6: TLabel
      Left = 16
      Top = 14
      Width = 101
      Height = 14
      Caption = 'Major Arrow Style:'
    end
    object Shape1: TShape
      Left = 285
      Top = 43
      Width = 20
      Height = 20
    end
    object rbLineArrow: TRadioButton
      Left = 128
      Top = 14
      Width = 52
      Height = 17
      Caption = '&Line'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbLineArrowClick
    end
    object rbPolygonArrow: TRadioButton
      Left = 188
      Top = 14
      Width = 74
      Height = 17
      Caption = '&Polygon'
      TabOrder = 1
      OnClick = rbLineArrowClick
    end
    object edWidth: TEdit
      Left = 85
      Top = 42
      Width = 32
      Height = 22
      TabOrder = 2
      Text = '1'
      OnChange = edWidthChange
    end
    object edFillColor: TEdit
      Left = 195
      Top = 42
      Width = 77
      Height = 22
      CharCase = ecUpperCase
      TabOrder = 3
      Text = '$40FFFF00'
      OnChange = edPenColorChange
    end
    object edPenColor: TEdit
      Left = 380
      Top = 42
      Width = 77
      Height = 22
      CharCase = ecUpperCase
      TabOrder = 4
      Text = '$FF000000'
      OnChange = edPenColorChange
    end
    object cbArrowStart: TComboBox
      Left = 141
      Top = 70
      Width = 80
      Height = 22
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 5
      Text = 'None'
      OnChange = edWidthChange
      Items.Strings = (
        'None'
        'Simple'
        'Fancy'
        'Diamond'
        'Circle'
        'Tail')
    end
    object cbArrowEnd: TComboBox
      Left = 350
      Top = 70
      Width = 80
      Height = 22
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 6
      Text = 'Simple'
      OnChange = edWidthChange
      Items.Strings = (
        'None'
        'Simple'
        'Fancy'
        'Diamond'
        'Circle'
        'Tail')
    end
    object LineWidthUpDown: TUpDown
      Left = 117
      Top = 42
      Width = 16
      Height = 22
      Associate = edWidth
      Min = 1
      Max = 25
      Position = 1
      TabOrder = 7
    end
    object pnlText: TPanel
      Left = 260
      Top = 15
      Width = 197
      Height = 21
      Alignment = taRightJustify
      BevelOuter = bvNone
      Caption = 'Font name; Size'
      TabOrder = 8
      OnDblClick = mnuFontClick
    end
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 125
    object File1: TMenuItem
      Caption = '&File'
      object mnuLoadImage: TMenuItem
        Caption = 'L&oad Image ...'
        ShortCut = 16463
        OnClick = mnuLoadImageClick
      end
      object mnuRemoveImage: TMenuItem
        Caption = 'R&emove Image'
        OnClick = mnuRemoveImageClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object SavetoFile1: TMenuItem
        Caption = '&Save to File ...'
        ShortCut = 16467
        OnClick = SavetoFile1Click
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
    object Edit1: TMenuItem
      Caption = '&Edit'
      object mnuFinishEdit2: TMenuItem
        Caption = 'End Ed&it'
        GroupIndex = 1
        ShortCut = 16465
        OnClick = mnuEndEditClick
      end
      object mnuEditLayer2: TMenuItem
        Caption = '&Edit Layer'
        GroupIndex = 1
        ShortCut = 16453
        OnClick = mnuEditLayerClick
      end
      object mnuDuplicateLayer2: TMenuItem
        Caption = '&Duplicate Layer'
        GroupIndex = 1
        ShortCut = 16452
        OnClick = mnuDuplicateLayerClick
      end
      object mnuDeleteLayer2: TMenuItem
        Caption = 'Delete La&yer'
        GroupIndex = 1
        ShortCut = 16473
        OnClick = mnuDeleteLayerClick
      end
      object N6: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object mnuRotateButtons2: TMenuItem
        Caption = '&Rotate Buttons'
        GroupIndex = 1
        ShortCut = 16466
        OnClick = mnuRotateButtonsClick
      end
      object mnuScaleButtons2: TMenuItem
        Caption = 'Sca&le Buttons'
        GroupIndex = 1
        ShortCut = 16460
        OnClick = mnuRotateButtonsClick
      end
      object N8: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object mnuFont2: TMenuItem
        Caption = '&Font ...'
        GroupIndex = 1
        ShortCut = 16454
        OnClick = mnuFontClick
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object mnuShowDesigner: TMenuItem
        Caption = 'Show &Designer Layers'
        Checked = True
        GroupIndex = 1
        ShortCut = 16452
        OnClick = mnuShowGridClick
      end
      object mnuShowGrid: TMenuItem
        Caption = 'Show &Grid'
        GroupIndex = 1
        ShortCut = 16455
        OnClick = mnuShowGridClick
      end
      object mnuShowHatchedBackground: TMenuItem
        Caption = 'Show &Hatched Background'
        Checked = True
        GroupIndex = 1
        ShortCut = 16456
        OnClick = mnuShowGridClick
      end
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 176
    Top = 125
    object mnuSmoothSym: TMenuItem
      AutoCheck = True
      Caption = 'Smooth - symmetric'
      Checked = True
      GroupIndex = 1
      RadioItem = True
      ShortCut = 16461
      OnClick = mnuTypeChangeClick
    end
    object mnuSmoothAsym: TMenuItem
      AutoCheck = True
      Caption = 'Smooth - asymmetric'
      GroupIndex = 1
      RadioItem = True
      ShortCut = 16449
      OnClick = mnuTypeChangeClick
    end
    object mnuSharpWithHdls: TMenuItem
      AutoCheck = True
      Caption = 'Sharp - with handles'
      GroupIndex = 1
      RadioItem = True
      ShortCut = 16456
      OnClick = mnuTypeChangeClick
    end
    object mnuSharpNoHdls: TMenuItem
      AutoCheck = True
      Caption = 'Sharp - without handles'
      GroupIndex = 1
      RadioItem = True
      ShortCut = 16472
      OnClick = mnuTypeChangeClick
    end
    object N7: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object AddText1: TMenuItem
      Caption = 'Add &Text ...'
      GroupIndex = 1
      ShortCut = 16468
      OnClick = AddText1Click
    end
    object mnuFont: TMenuItem
      Caption = '&Font ...'
      GroupIndex = 1
      ShortCut = 16454
      OnClick = mnuFontClick
    end
    object N5: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object mnuEndEdit: TMenuItem
      Caption = 'End Ed&it'
      GroupIndex = 1
      ShortCut = 16465
      OnClick = mnuEndEditClick
    end
    object mnuEditLayer: TMenuItem
      Caption = '&Edit Layer'
      GroupIndex = 1
      ShortCut = 16453
      OnClick = mnuEditLayerClick
    end
    object mnuDuplicateLayer: TMenuItem
      Caption = '&Duplicate Layer'
      GroupIndex = 1
      ShortCut = 16452
      OnClick = mnuDuplicateLayerClick
    end
    object mnuDeleteLayer: TMenuItem
      Caption = 'Delete La&yer'
      GroupIndex = 1
      ShortCut = 16473
      OnClick = mnuDeleteLayerClick
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
    object mnuScaleButtons: TMenuItem
      Caption = 'Sca&le Buttons'
      GroupIndex = 1
      ShortCut = 16460
      OnClick = mnuRotateButtonsClick
    end
    object N4: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object mnuDeleteButton: TMenuItem
      Caption = 'Delete Last B&utton'
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
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Image Files (*.bmp;*.png;*.jpg)| *.bmp;*.png;*.jpg'
    Left = 104
    Top = 169
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Image Files (*.bmp;*.png;*.jpg)| *.bmp;*.png;*.jpg'
    Left = 184
    Top = 169
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 240
    Top = 169
  end
end
