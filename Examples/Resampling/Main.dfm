object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Resampling'
  ClientHeight = 1094
  ClientWidth = 1524
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -27
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 192
  TextHeight = 33
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 1524
    Height = 1056
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    RaggedRight = True
    TabOrder = 0
    Tabs.Strings = (
      'Kernel Resamplers'
      'Down Sampling')
    TabIndex = 0
    TabStop = False
    OnChange = TabControl1Change
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 1056
    Width = 1524
    Height = 38
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <>
    SimplePanel = True
    SimpleText = '  Ctrl + MouseWheell --> to zoom in or out'
    ExplicitLeft = 8
    ExplicitTop = 997
    ExplicitWidth = 1508
  end
end