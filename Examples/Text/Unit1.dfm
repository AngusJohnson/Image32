object Form1: TForm1
  Left = 383
  Top = 304
  Margins.Left = 6
  Margins.Top = 6
  Margins.Right = 6
  Margins.Bottom = 6
  Caption = 'Image32 Text'
  ClientHeight = 916
  ClientWidth = 1283
  Color = clBtnFace
  TransparentColorValue = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -24
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseDown = ImgPanelMouseDown
  OnResize = FormResize
  PixelsPerInch = 192
  TextHeight = 29
  object StatusBar1: TStatusBar
    Left = 0
    Top = 872
    Width = 1283
    Height = 44
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <
      item
        Width = 480
      end
      item
        Width = 400
      end>
    ParentFont = True
    UseSystemFont = False
    ExplicitTop = 774
    ExplicitWidth = 1294
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      object Font1: TMenuItem
        Caption = '&Font ...'
        GroupIndex = 1
        ShortCut = 16454
        OnClick = Font1Click
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object mnuAlignLeft: TMenuItem
        AutoCheck = True
        Caption = 'Align &Left'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16460
        OnClick = mnuAlignJustifiedClick
      end
      object mnuAlignCenter: TMenuItem
        AutoCheck = True
        Caption = 'Align &Center'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16453
        OnClick = mnuAlignJustifiedClick
      end
      object mnuAlignRight: TMenuItem
        AutoCheck = True
        Caption = 'Align &Right'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16466
        OnClick = mnuAlignJustifiedClick
      end
      object mnuAlignJustified: TMenuItem
        AutoCheck = True
        Caption = 'Align Justified'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        ShortCut = 16458
        OnClick = mnuAlignJustifiedClick
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object mnuAlignTop: TMenuItem
        AutoCheck = True
        Caption = 'Align &Top'
        GroupIndex = 2
        RadioItem = True
        ShortCut = 16468
        OnClick = mnuAlignJustifiedClick
      end
      object mnuAlignMiddle: TMenuItem
        AutoCheck = True
        Caption = 'Align &Middle'
        Checked = True
        GroupIndex = 2
        RadioItem = True
        ShortCut = 16461
        OnClick = mnuAlignJustifiedClick
      end
      object mnuAlignBottom: TMenuItem
        AutoCheck = True
        Caption = 'Align &Bottom'
        GroupIndex = 2
        RadioItem = True
        ShortCut = 16450
        OnClick = mnuAlignJustifiedClick
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        GroupIndex = 2
        ShortCut = 27
        OnClick = Exit1Click
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Segoe UI'
    Font.Style = []
    Left = 272
    Top = 80
  end
end
