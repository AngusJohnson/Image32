object Form1: TForm1
  Left = 298
  Top = 205
  Caption = 'Svg101'
  ClientHeight = 428
  ClientWidth = 544
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ImagePanel: TImage32Panel
    Left = 0
    Top = 0
    Width = 544
    Height = 428
    Align = alClient
    BevelInner = bvLowered
    BorderWidth = 16
    Color = clBtnFace
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 0
    TabStop = True
    Touch.InteractiveGestures = [igZoom, igPan, igPressAndTap]
    AutoCenter = True
    FocusedColor = clActiveCaption
    UnFocusedColor = clBtnFace
    Scale = 1.000000000000000000
    ScaleMin = 0.050000000000000000
    ScaleMax = 20.000000000000000000
    ShowScrollButtons = ssbFocused
    AllowScroll = True
    AllowZoom = True
    ExplicitWidth = 709
    ExplicitHeight = 368
  end
  object MainMenu1: TMainMenu
    Left = 144
    Top = 104
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open ...'
        ShortCut = 16463
        OnClick = Open1Click
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
  end
  object OpenDialog1: TOpenDialog
    Filter = 'SVG Files (*.svg)|*.svg'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 256
    Top = 104
  end
end
