object MainForm: TMainForm
  Left = 392
  Top = 208
  Caption = 'Layers201'
  ClientHeight = 435
  ClientWidth = 416
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 416
    Width = 416
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object MainMenu1: TMainMenu
    Left = 24
    Top = 32
    object File1: TMenuItem
      Caption = '&File'
      object mnuExit: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = mnuExitClick
      end
    end
    object Action1: TMenuItem
      Caption = '&Action'
      object mnuAddImage: TMenuItem
        Caption = 'Add &Image ...'
        ShortCut = 16457
        OnClick = mnuAddImageClick
      end
      object mnuAddText: TMenuItem
        Caption = 'Add &Text'
        ShortCut = 16468
        OnClick = mnuAddTextClick
      end
      object mnuAddArrow: TMenuItem
        Caption = 'Add &Arrow'
        ShortCut = 16449
        OnClick = mnuAddArrowClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuRotate: TMenuItem
        Caption = '&Rotate'
        ShortCut = 16466
        OnClick = mnuRotateClick
      end
      object mnuDelete: TMenuItem
        Caption = '&Delete'
        ShortCut = 46
        OnClick = mnuDeleteClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuSendBackOne: TMenuItem
        Caption = 'Send &Back One'
        ShortCut = 16450
        OnClick = mnuSendBackOneClick
      end
      object mnuBringForwardOne: TMenuItem
        Caption = 'Bring &Forward One'
        ShortCut = 16454
        OnClick = mnuBringForwardOneClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.PNG'
    Filter = 'Image Files|*.png;*.bmp;*.jpg'
    Left = 64
    Top = 32
  end
end
