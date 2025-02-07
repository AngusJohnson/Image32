object Color32DialogForm: TColor32DialogForm
  Left = 391
  Top = 186
  Margins.Left = 6
  Margins.Top = 6
  Margins.Right = 6
  Margins.Bottom = 6
  BorderStyle = bsDialog
  Caption = 'Color32'
  ClientHeight = 670
  ClientWidth = 796
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -30
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 41
  object Label1: TLabel
    Left = 452
    Top = 32
    Width = 58
    Height = 41
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = '&Red:'
    FocusControl = eRed
  end
  object Label2: TLabel
    Left = 452
    Top = 164
    Width = 87
    Height = 41
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = '&Green:'
    FocusControl = eGreen
  end
  object Label3: TLabel
    Left = 452
    Top = 304
    Width = 64
    Height = 41
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = '&Blue:'
    FocusControl = eBlue
  end
  object Label4: TLabel
    Left = 452
    Top = 444
    Width = 83
    Height = 41
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = '&Alpha:'
    FocusControl = eAlpha
  end
  object Label5: TLabel
    Left = 194
    Top = 594
    Width = 18
    Height = 41
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = '#'
  end
  object eHexColor: TEdit
    Left = 216
    Top = 596
    Width = 152
    Height = 41
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -30
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 11
    Text = 'FFFFFFFF'
    OnChange = eHexColorChange
  end
  object tbRed: TTrackBar
    Left = 452
    Top = 94
    Width = 300
    Height = 64
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Max = 255
    PageSize = 16
    Frequency = 16
    Position = 255
    TabOrder = 2
    ThumbLength = 40
    OnChange = tbBlueChange
  end
  object eRed: TEdit
    Left = 670
    Top = 28
    Width = 82
    Height = 49
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    TabOrder = 1
    Text = '255'
    OnChange = eGreenChange
  end
  object tbGreen: TTrackBar
    Left = 452
    Top = 210
    Width = 300
    Height = 64
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Max = 255
    PageSize = 16
    Frequency = 16
    Position = 255
    TabOrder = 4
    ThumbLength = 40
    OnChange = tbBlueChange
  end
  object eGreen: TEdit
    Left = 670
    Top = 160
    Width = 82
    Height = 49
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    TabOrder = 3
    Text = '255'
    OnChange = eGreenChange
  end
  object tbBlue: TTrackBar
    Left = 452
    Top = 366
    Width = 300
    Height = 64
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Max = 255
    PageSize = 16
    Frequency = 16
    Position = 255
    TabOrder = 6
    ThumbLength = 40
    OnChange = tbBlueChange
  end
  object eBlue: TEdit
    Left = 670
    Top = 302
    Width = 82
    Height = 49
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    TabOrder = 5
    Text = '255'
    OnChange = eGreenChange
  end
  object tbAlpha: TTrackBar
    Left = 452
    Top = 508
    Width = 300
    Height = 62
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Max = 255
    PageSize = 16
    Frequency = 16
    Position = 255
    TabOrder = 8
    ThumbLength = 40
    OnChange = tbBlueChange
  end
  object eAlpha: TEdit
    Left = 670
    Top = 442
    Width = 82
    Height = 49
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    TabOrder = 7
    Text = '255'
    OnChange = eGreenChange
  end
  object Bok: TButton
    Left = 584
    Top = 582
    Width = 168
    Height = 66
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 9
  end
  object bCancel: TButton
    Left = 400
    Top = 582
    Width = 166
    Height = 66
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 10
  end
  object tbMain: TTrackBar
    Left = 0
    Top = 498
    Width = 384
    Height = 64
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Max = 255
    PageSize = 16
    Frequency = 16
    TabOrder = 0
    ThumbLength = 40
    OnChange = tbMainChange
  end
end
