object Form1: TForm1
  Left = 244
  Top = 142
  Caption = 'Projective Transform'
  ClientHeight = 485
  ClientWidth = 835
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
  OnResize = Form1Resize
  PixelsPerInch = 96
  TextHeight = 14
  object StatusBar1: TStatusBar
    Left = 0
    Top = 466
    Width = 835
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 41
    Width = 417
    Height = 425
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
    Width = 835
    Height = 41
    Align = alTop
    BevelOuter = bvLowered
    BevelWidth = 2
    TabOrder = 2
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
      Top = 3
      Width = 354
      Height = 35
      TabOrder = 1
      Visible = False
      object Label1: TLabel
        Left = 14
        Top = 9
        Width = 40
        Height = 14
        Caption = 'Margins'
      end
      object SpinEdit1: TEdit
        Left = 64
        Top = 6
        Width = 33
        Height = 22
        TabOrder = 0
        Text = '0'
        OnChange = SpinEdit1Change
      end
      object UpDown1: TUpDown
        Left = 97
        Top = 6
        Width = 16
        Height = 22
        Associate = SpinEdit1
        Min = -500
        Max = 1000
        TabOrder = 1
      end
      object SpinEdit2: TEdit
        Left = 128
        Top = 6
        Width = 33
        Height = 22
        TabOrder = 2
        Text = '0'
        OnChange = SpinEdit1Change
      end
      object UpDown2: TUpDown
        Left = 161
        Top = 6
        Width = 16
        Height = 22
        Associate = SpinEdit2
        Min = -500
        Max = 1000
        TabOrder = 3
      end
      object SpinEdit3: TEdit
        Left = 192
        Top = 6
        Width = 33
        Height = 22
        TabOrder = 4
        Text = '0'
        OnChange = SpinEdit1Change
      end
      object UpDown3: TUpDown
        Left = 225
        Top = 6
        Width = 16
        Height = 22
        Associate = SpinEdit3
        Min = -500
        Max = 1000
        TabOrder = 5
      end
      object SpinEdit4: TEdit
        Left = 254
        Top = 6
        Width = 33
        Height = 22
        TabOrder = 6
        Text = '0'
        OnChange = SpinEdit1Change
      end
      object UpDown4: TUpDown
        Left = 287
        Top = 6
        Width = 16
        Height = 22
        Associate = SpinEdit4
        Min = -500
        Max = 1000
        TabOrder = 7
      end
    end
  end
  object pnlRight: TPanel
    Left = 418
    Top = 41
    Width = 417
    Height = 425
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
