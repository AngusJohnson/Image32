object MainForm: TMainForm
  Left = 392
  Top = 208
  Caption = 'Layers201'
  ClientHeight = 431
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 412
    Width = 423
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = '  Use <Shift> to Scroll, Ctrl> to Zoom, and <Num> to Scale'
  end
  object MainMenu1: TMainMenu
    Left = 32
    Top = 40
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
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuCloneLayer: TMenuItem
        Caption = '&Clone Layer'
        ShortCut = 16462
        OnClick = mnuCloneLayerClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.PNG'
    Filter = 'Image Files|*.png;*.bmp;*.jpg;*.qoi'
    Left = 88
    Top = 40
  end
  object PopupMenu1: TPopupMenu
    Left = 160
    Top = 40
    object AddImage1: TMenuItem
      Caption = 'Add &Image ...'
      ShortCut = 16457
      OnClick = mnuAddImageClick
    end
    object AddText1: TMenuItem
      Caption = 'Add &Text'
      ShortCut = 16468
      OnClick = mnuAddTextClick
    end
    object AddArrow1: TMenuItem
      Caption = 'Add &Arrow'
      ShortCut = 16449
      OnClick = mnuAddArrowClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Rotate1: TMenuItem
      Caption = '&Rotate'
      ShortCut = 16466
      OnClick = mnuRotateClick
    end
    object Delete1: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = mnuDeleteClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object SendBackOne1: TMenuItem
      Caption = 'Send &Back One'
      ShortCut = 16450
      OnClick = mnuSendBackOneClick
    end
    object BringForwardOne1: TMenuItem
      Caption = 'Bring &Forward One'
      ShortCut = 16454
      OnClick = mnuBringForwardOneClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object CloneLayer1: TMenuItem
      Caption = '&Clone Layer'
      ShortCut = 16462
      OnClick = mnuCloneLayerClick
    end
  end
end
