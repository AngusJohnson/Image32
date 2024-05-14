object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Bicubic Interpolation'
  ClientHeight = 773
  ClientWidth = 1159
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
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  PixelsPerInch = 192
  TextHeight = 34
  object StatusBar1: TStatusBar
    Left = 0
    Top = 735
    Width = 1159
    Height = 38
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <>
    SimplePanel = True
    SimpleText = '  Ctrl + MouseWheel --> to zoom in or out'
    ExplicitTop = 734
    ExplicitWidth = 1145
  end
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 1159
    Height = 735
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    TabOrder = 1
    Tabs.Strings = (
      '&1. Close Paths1'
      '&2. Close Paths2'
      '&3. Open Paths')
    TabIndex = 0
    OnChange = TabControl1Change
    ExplicitWidth = 1145
    ExplicitHeight = 734
  end
end
