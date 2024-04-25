object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Resampling'
  ClientHeight = 889
  ClientWidth = 1141
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -28
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 192
  TextHeight = 34
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 1141
    Height = 851
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    RaggedRight = True
    TabOrder = 0
    Tabs.Strings = (
      'Kernel Resamplers 1'
      'Kernel Resamplers 2'
      'Down Sampling')
    TabIndex = 0
    TabStop = False
    OnChange = TabControl1Change
    ExplicitWidth = 1127
    ExplicitHeight = 850
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 851
    Width = 1141
    Height = 38
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <>
    SimplePanel = True
    SimpleText = '  Ctrl + MouseWheel --> to zoom in or out'
    ExplicitTop = 850
    ExplicitWidth = 1127
  end
end
