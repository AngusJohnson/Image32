object Form1: TForm1
  Left = 244
  Top = 142
  Margins.Left = 6
  Margins.Top = 6
  Margins.Right = 6
  Margins.Bottom = 6
  Caption = 'Projective Transform'
  ClientHeight = 970
  ClientWidth = 1670
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -24
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = Form1Resize
  PixelsPerInch = 192
  TextHeight = 29
  object StatusBar1: TStatusBar
    Left = 0
    Top = 932
    Width = 1670
    Height = 38
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <>
    SimplePanel = True
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 82
    Width = 834
    Height = 850
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alLeft
    ParentBackground = False
    TabOrder = 1
    OnMouseDown = pnlLeftMouseDown
    OnMouseMove = pnlLeftMouseMove
    OnMouseUp = pnlLeftMouseUp
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1670
    Height = 82
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alTop
    BevelOuter = bvLowered
    BevelWidth = 2
    TabOrder = 2
    object btnTransform: TButton
      Left = 32
      Top = 18
      Width = 264
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Start &Transform'
      Enabled = False
      TabOrder = 0
      OnClick = btnTransformClick
    end
    object gbMargins: TGroupBox
      Left = 846
      Top = 6
      Width = 708
      Height = 70
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      TabOrder = 1
      Visible = False
      object Label1: TLabel
        Left = 28
        Top = 18
        Width = 83
        Height = 29
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'Margins'
      end
      object SpinEdit1: TEdit
        Left = 128
        Top = 12
        Width = 66
        Height = 37
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        TabOrder = 0
        Text = '0'
        OnChange = SpinEdit1Change
      end
      object UpDown1: TUpDown
        Left = 194
        Top = 12
        Width = 32
        Height = 37
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Associate = SpinEdit1
        Min = -500
        Max = 1000
        TabOrder = 1
      end
      object SpinEdit2: TEdit
        Left = 256
        Top = 12
        Width = 66
        Height = 37
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        TabOrder = 2
        Text = '0'
        OnChange = SpinEdit1Change
      end
      object UpDown2: TUpDown
        Left = 322
        Top = 12
        Width = 32
        Height = 37
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Associate = SpinEdit2
        Min = -500
        Max = 1000
        TabOrder = 3
      end
      object SpinEdit3: TEdit
        Left = 384
        Top = 12
        Width = 66
        Height = 37
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        TabOrder = 4
        Text = '0'
        OnChange = SpinEdit1Change
      end
      object UpDown3: TUpDown
        Left = 450
        Top = 12
        Width = 32
        Height = 37
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Associate = SpinEdit3
        Min = -500
        Max = 1000
        TabOrder = 5
      end
      object SpinEdit4: TEdit
        Left = 508
        Top = 12
        Width = 66
        Height = 37
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        TabOrder = 6
        Text = '0'
        OnChange = SpinEdit1Change
      end
      object UpDown4: TUpDown
        Left = 574
        Top = 12
        Width = 32
        Height = 37
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Associate = SpinEdit4
        Min = -500
        Max = 1000
        TabOrder = 7
      end
    end
  end
  object pnlRight: TPanel
    Left = 836
    Top = 82
    Width = 834
    Height = 850
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alRight
    ParentBackground = False
    TabOrder = 3
    OnMouseDown = pnlLeftMouseDown
    OnMouseMove = pnlLeftMouseMove
    OnMouseUp = pnlLeftMouseUp
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
