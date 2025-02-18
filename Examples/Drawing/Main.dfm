object fmMain: TfmMain
  Left = 360
  Top = 91
  Margins.Left = 6
  Margins.Top = 6
  Margins.Right = 6
  Margins.Bottom = 6
  Caption = 'Drawing'
  ClientHeight = 1038
  ClientWidth = 1194
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -30
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 192
  TextHeight = 41
  object pnlTools: TPanel
    Left = 0
    Top = 0
    Width = 202
    Height = 992
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alLeft
    TabOrder = 0
  end
  object pnlMain: TPanel
    Left = 202
    Top = 0
    Width = 992
    Height = 992
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    TabOrder = 1
    object TabControl1: TTabControl
      Left = 1
      Top = 1
      Width = 990
      Height = 48
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Align = alTop
      TabOrder = 0
      Tabs.Strings = (
        '[Untitled]')
      TabIndex = 0
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 992
    Width = 1194
    Height = 46
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <
      item
        Width = 600
      end
      item
        Width = 2000
      end>
  end
  object MainMenu1: TMainMenu
    Left = 32
    Top = 64
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New'
      end
      object File2: TMenuItem
        Caption = '&Open ...'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object N2: TMenuItem
        Caption = '&Save'
      end
      object SaveAs1: TMenuItem
        Caption = 'Save &As ...'
      end
      object SaveAs2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Exit2: TMenuItem
      Caption = '&Edit'
      object mnuUndo: TMenuItem
        Caption = '&Undo'
        ShortCut = 16474
        OnClick = mnuUndoClick
      end
      object mnuRedo: TMenuItem
        Caption = '&Redo'
        ShortCut = 24666
        OnClick = mnuRedoClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuDelSelect: TMenuItem
        Caption = '&Delete Selection'
        ShortCut = 46
        OnClick = mnuDelSelectClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mnuEditForeColor: TMenuItem
        Caption = 'Edit &Fore Color ...'
        ShortCut = 113
        OnClick = mnuEditBackColorClick
      end
      object mnuEditBackColor: TMenuItem
        Caption = 'Edit &Back Color ...'
        ShortCut = 114
        OnClick = mnuEditBackColorClick
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
    end
  end
end
