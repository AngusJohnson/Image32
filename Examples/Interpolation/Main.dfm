object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Bicubic Interpolation'
  ClientHeight = 660
  ClientWidth = 1488
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
  PixelsPerInch = 192
  TextHeight = 34
  object StatusBar1: TStatusBar
    Left = 0
    Top = 622
    Width = 1488
    Height = 38
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <>
    SimplePanel = True
    SimpleText = '  Ctrl + MouseWheel --> to zoom in or out'
    ExplicitTop = 597
    ExplicitWidth = 1474
  end
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 1488
    Height = 622
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
    ExplicitHeight = 598
  end
end
