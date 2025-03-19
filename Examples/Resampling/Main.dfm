object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Resampling'
  ClientHeight = 930
  ClientWidth = 1141
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -28
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  PixelsPerInch = 192
  TextHeight = 34
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 1141
    Height = 892
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    RaggedRight = True
    TabOrder = 0
    Tabs.Strings = (
      '&1 Resamplers1'
      '&2 Resamplers2'
      '&3 Resamplers3'
      '&4 Resamplers4'
      '&5 Down Sampling')
    TabIndex = 0
    TabStop = False
    OnChange = TabControl1Change
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 892
    Width = 1141
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
