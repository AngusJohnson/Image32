object MainForm: TMainForm
  Left = 202
  Top = 184
  Caption = 'Photo'
  ClientHeight = 432
  ClientWidth = 456
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 456
    Height = 59
    Align = alTop
    TabOrder = 0
    Visible = False
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 413
    Width = 456
    Height = 19
    Color = clBlack
    Panels = <
      item
        Bevel = pbNone
        Style = psOwnerDraw
        Width = 300
      end>
    ParentFont = True
    UseSystemFont = False
    OnDrawPanel = StatusBar1DrawPanel
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 8
    object File1: TMenuItem
      Caption = '&File'
      object mnuOpenFolder: TMenuItem
        Caption = '&Open Folder ...'
        ShortCut = 16463
        OnClick = mnuOpenFolderClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuEscape: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = mnuEscapeClick
      end
    end
    object mnuMainEdit: TMenuItem
      Caption = '&Edit'
      OnClick = mnuMainEditClick
      object mnuEdit2: TMenuItem
        Caption = '&Edit'
        Enabled = False
        ShortCut = 122
        OnClick = mnuEditClick
      end
      object mnuRename2: TMenuItem
        Caption = '&Rename'
        Enabled = False
        ShortCut = 113
        OnClick = mnuRenameClick
      end
      object mnuDelete2: TMenuItem
        Caption = '&Delete'
        Enabled = False
        ShortCut = 46
        OnClick = mnuDeleteClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object mnuBackgroundColor: TMenuItem
        Caption = 'Background &Color ...'
        OnClick = mnuBackgroundColorClick
      end
      object mnuSlideShowInterval: TMenuItem
        Caption = 'Slide Show &Interval ...'
        GroupIndex = 2
        OnClick = mnuSlideShowIntervalClick
      end
      object mnuTransition: TMenuItem
        Caption = 'Slide Show &Transition'
        GroupIndex = 2
        object mnuFast: TMenuItem
          Caption = '&Fast'
          GroupIndex = 1
          RadioItem = True
          OnClick = mnuTransitionClick
        end
        object mnuSimpleFade: TMenuItem
          Caption = '&Simple Fade'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = mnuTransitionClick
        end
        object mnuFadeInWithOut: TMenuItem
          Caption = 'C&oncurrent Fade'
          GroupIndex = 1
          RadioItem = True
          OnClick = mnuTransitionClick
        end
        object mnuFadeOutThenIn: TMenuItem
          Caption = 'Cons&ecutive Fade'
          GroupIndex = 1
          RadioItem = True
          OnClick = mnuTransitionClick
        end
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object mnuSmallPreview: TMenuItem
        Caption = '&Small Thumbnail'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 114
        OnClick = mnuChangeDisplaySizeClick
      end
      object mnuMediumPreview: TMenuItem
        Caption = '&Medium Thumbnail'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        ShortCut = 115
        OnClick = mnuChangeDisplaySizeClick
      end
      object mnuLargePreview: TMenuItem
        Caption = '&Large Thumbnail'
        GroupIndex = 1
        RadioItem = True
        ShortCut = 116
        OnClick = mnuChangeDisplaySizeClick
      end
      object N4: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object mnuLargeImage: TMenuItem
        Caption = '&Large Image'
        GroupIndex = 1
        ShortCut = 118
        OnClick = mnuShowLargeImageInMainformClick
      end
      object mnuFullScreen: TMenuItem
        Caption = 'Full Screen'
        GroupIndex = 2
        ShortCut = 119
        OnClick = mnuFullScreenClick
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object mnuSlideShow: TMenuItem
        Caption = '&Slide Show'
        GroupIndex = 2
        ShortCut = 120
        OnClick = mnuSlideShowClick
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object mnuTips: TMenuItem
        Caption = '&Tips'
        OnClick = mnuTipsClick
      end
      object mnuAbout: TMenuItem
        Caption = '&About'
        ShortCut = 112
        OnClick = mnuAboutClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 144
    Top = 8
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 73
    Top = 10
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      ShortCut = 122
      OnClick = mnuEditClick
    end
    object mnuRename: TMenuItem
      Caption = '&Rename'
      ShortCut = 113
      OnClick = mnuRenameClick
    end
    object mnuDelete: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = mnuDeleteClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object mnuOpenImageFolder: TMenuItem
      Caption = 'Open the &Folder Containing this Image'
      ShortCut = 123
      OnClick = mnuOpenImageFolderClick
    end
  end
  object tmrLoading: TTimer
    Enabled = False
    OnTimer = tmrLoadingTimer
    Left = 120
    Top = 88
  end
  object tmrSlideShow: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = tmrSlideShowTimer
    Left = 177
    Top = 88
  end
end
