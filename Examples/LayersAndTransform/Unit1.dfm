object Form1: TForm1
  Left = 618
  Top = 336
  Width = 386
  Height = 448
  Caption = 'Image32 - Layer and Transform example'
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 370
    Height = 371
    Align = alClient
    TabOrder = 0
    OnMouseDown = Panel1MouseDown
    OnMouseMove = Panel1MouseMove
    OnMouseUp = Panel1MouseUp
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 371
    Width = 370
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SimpleText = 
      '  Right click where you want to add the next spline control poin' +
      't'
    UseSystemFont = False
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      object mnuOpen: TMenuItem
        Caption = '&Open ...'
        ShortCut = 16463
        OnClick = mnuOpenClick
      end
      object mnuSave: TMenuItem
        Caption = '&Save ...'
        ShortCut = 16467
        OnClick = mnuSaveClick
      end
      object N1: TMenuItem
        Caption = '-'
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
        ShortCut = 27
        OnClick = Exit1Click
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object mnuShowAll: TMenuItem
        AutoCheck = True
        Caption = 'Show &All'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        ShortCut = 65
        OnClick = mnuShowControlsClick
      end
      object mnuShowImage: TMenuItem
        AutoCheck = True
        Caption = 'Show &Image'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 73
        OnClick = mnuShowControlsClick
      end
      object mnuShowControls: TMenuItem
        AutoCheck = True
        Caption = 'Show Co&ntrols'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 78
        OnClick = mnuShowControlsClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.bmp'
    Filter = 'Image Files (BMP, PNG, JPG)|*.bmp;*.png;*.jpg'
    Left = 176
    Top = 88
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.bmp'
    Filter = 'Image Files (BMP, PNG, JPG)|*.bmp;*.png;*.jpg'
    Left = 240
    Top = 80
  end
end
