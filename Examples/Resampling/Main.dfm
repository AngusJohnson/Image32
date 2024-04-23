object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Resampling'
  ClientHeight = 1161
  ClientWidth = 1524
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
    Width = 1524
    Height = 1123
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
    ExplicitWidth = 1510
    ExplicitHeight = 1055
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 1123
    Width = 1524
    Height = 38
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <>
    SimplePanel = True
    SimpleText = '  Ctrl + MouseWheel --> to zoom in or out'
  end
end
