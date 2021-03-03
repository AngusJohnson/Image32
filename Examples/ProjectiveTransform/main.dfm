object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Projective Transform'
  ClientHeight = 470
  ClientWidth = 824
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
  object StatusBar1: TStatusBar
    Left = 0
    Top = 451
    Width = 824
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 41
    Width = 417
    Height = 410
    Align = alLeft
    ParentBackground = False
    TabOrder = 1
    OnMouseDown = pnlLeftMouseDown
    OnMouseMove = pnlLeftMouseMove
    OnMouseUp = pnlLeftMouseUp
    OnResize = pnlLeftResize
    ExplicitTop = 42
  end
  object pnlRight: TPanel
    Left = 417
    Top = 41
    Width = 407
    Height = 410
    Align = alRight
    TabOrder = 2
    OnResize = pnlLeftResize
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 824
    Height = 41
    Align = alTop
    BevelOuter = bvLowered
    BevelWidth = 2
    TabOrder = 3
    object btnTransform: TButton
      Left = 16
      Top = 9
      Width = 132
      Height = 25
      Caption = 'Start &Transform'
      Enabled = False
      TabOrder = 0
      OnClick = btnTransformClick
    end
    object gbMargins: TGroupBox
      Left = 423
      Top = 4
      Width = 405
      Height = 32
      Caption = 'Margins'
      TabOrder = 1
      Visible = False
      object SpinEdit1: TSpinEdit
        Left = 59
        Top = 6
        Width = 56
        Height = 23
        MaxValue = 800
        MinValue = -800
        TabOrder = 0
        Value = 0
        OnChange = SpinEdit1Change
      end
      object SpinEdit2: TSpinEdit
        Left = 123
        Top = 6
        Width = 56
        Height = 23
        MaxValue = 800
        MinValue = -800
        TabOrder = 1
        Value = 0
        OnChange = SpinEdit1Change
      end
      object SpinEdit3: TSpinEdit
        Left = 186
        Top = 6
        Width = 56
        Height = 23
        MaxValue = 800
        MinValue = -800
        TabOrder = 2
        Value = 0
        OnChange = SpinEdit1Change
      end
      object SpinEdit4: TSpinEdit
        Left = 252
        Top = 6
        Width = 56
        Height = 23
        MaxValue = 800
        MinValue = -800
        TabOrder = 3
        Value = 0
        OnChange = SpinEdit1Change
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 208
    Top = 104
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open ...'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = '&Save ...'
        ShortCut = 16467
        OnClick = Save1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = Exit1Click
      end
    end
    object ransform1: TMenuItem
      Caption = '&Transform'
      object mnuClearTransform: TMenuItem
        Caption = '&Clear'
        OnClick = mnuClearTransformClick
      end
      object mnuStartTransform: TMenuItem
        Caption = '&Start'
        ShortCut = 16468
        OnClick = btnTransformClick
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object mnuHowTo: TMenuItem
        Caption = '&How To'
        ShortCut = 112
        OnClick = mnuHowToClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Image Files|*.bmp;*.png;*.jpg'
    Left = 256
    Top = 184
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Image Files|*.bmp;*.png;*.jpg'
    Left = 264
    Top = 264
  end
end
