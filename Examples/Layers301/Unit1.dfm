object MainForm: TMainForm
  Left = 594
  Top = 418
  Caption = 'Layers301'
  ClientHeight = 412
  ClientWidth = 455
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
    Top = 386
    Width = 455
    Height = 26
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = mnuExitClick
      end
    end
    object Action1: TMenuItem
      Caption = '&Action'
      OnClick = PopupMenu1Popup
      object mnuAddRectangle: TMenuItem
        Caption = 'Add &Rectangle'
        ShortCut = 16466
        OnClick = mnuAddRectangleClick
      end
      object mnuAddEllipse: TMenuItem
        Caption = 'Add &Ellipse'
        ShortCut = 16453
        OnClick = mnuAddEllipseClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object mnuDeleteLayer2: TMenuItem
        Caption = '&Delete Layer'
        ShortCut = 46
        OnClick = mnuDeleteLayerClick
      end
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 128
    Top = 152
    object AddEllipse1: TMenuItem
      Caption = 'Add &Ellipse'
      ShortCut = 16453
      OnClick = mnuAddEllipseClick
    end
    object AddRectangle1: TMenuItem
      Caption = 'Add &Rectangle'
      ShortCut = 16466
      OnClick = mnuAddRectangleClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuDeleteLayer: TMenuItem
      Caption = '&Delete Layer'
      ShortCut = 46
      OnClick = mnuDeleteLayerClick
    end
  end
end
