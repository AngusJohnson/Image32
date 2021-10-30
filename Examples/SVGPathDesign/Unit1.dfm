object MainForm: TMainForm
  Left = 273
  Top = 177
  Caption = 'SVG Paths'
  ClientHeight = 530
  ClientWidth = 770
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseDown = MainFormMouseDown
  OnMouseMove = MainFormMouseMove
  OnMouseUp = FormMouseUp
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object pnlBottom: TPanel
    Left = 0
    Top = 458
    Width = 770
    Height = 72
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object memo1: TRichEdit
      Left = 0
      Top = 0
      Width = 770
      Height = 72
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
      WordWrap = False
      Zoom = 100
      OnChange = Memo1Change
    end
  end
  object MainMenu1: TMainMenu
    Left = 88
    Top = 56
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open ...'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = mnuExitClick
      end
    end
    object Action1: TMenuItem
      Caption = '&Action'
      object mnuScaleToFit: TMenuItem
        Caption = 'Scale to &Fit Window'
        ShortCut = 16454
        OnClick = mnuScaleToFitClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object AppendSegment1: TMenuItem
        Caption = 'A&ppend Segment'
        object mnuArc2: TMenuItem
          Caption = 'Arc'
          ShortCut = 24641
          OnClick = mnuTClick
        end
        object N10: TMenuItem
          Caption = '-'
        end
        object mnuCubic2: TMenuItem
          Caption = 'Cubic Bezier'
          ShortCut = 24643
          OnClick = mnuTClick
        end
        object mnuSpline2: TMenuItem
          Caption = 'Cubic Spline'
          ShortCut = 24659
          OnClick = mnuTClick
        end
        object N9: TMenuItem
          Caption = '-'
        end
        object mnuQuad2: TMenuItem
          Caption = 'Quad Bezier'
          ShortCut = 24657
          OnClick = mnuTClick
        end
        object mnuTSpline2: TMenuItem
          Caption = 'Quad Spline'
          ShortCut = 24660
          OnClick = mnuTClick
        end
        object N8: TMenuItem
          Caption = '-'
        end
        object mnuLine2: TMenuItem
          Caption = 'Line'
          ShortCut = 24652
          OnClick = mnuTClick
        end
        object MnuHorz2: TMenuItem
          Caption = 'Horz Line'
          ShortCut = 24648
          OnClick = mnuTClick
        end
        object mnuVert2: TMenuItem
          Caption = 'Vert Line'
          ShortCut = 24662
          OnClick = mnuTClick
        end
        object N14: TMenuItem
          Caption = '-'
        end
        object mnuZClose: TMenuItem
          Caption = 'Close Path'
          ShortCut = 24666
          OnClick = mnuTClick
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuReverseArc: TMenuItem
        Caption = 'Reverse &Arc'
        Enabled = False
        ShortCut = 16449
        OnClick = ReverseArcClick
      end
      object mnuRotate2: TMenuItem
        Caption = '&Rotate Arc'
        Enabled = False
        ShortCut = 16466
        OnClick = mnuRotateClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuDeleteSeg: TMenuItem
        Caption = '&Delete End Segment'
        Enabled = False
        ShortCut = 46
        OnClick = mnuDeleteClick
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.png'
    Filter = 'Image Files (BMP, PNG, JPG)|*.bmp;*.png;*.jpg'
    Left = 152
    Top = 56
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 64
    Top = 120
    object N4: TMenuItem
      Caption = 'A&ppend Segment'
      object mnuA: TMenuItem
        Caption = 'Arc'
        ShortCut = 24641
        OnClick = mnuTClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuC: TMenuItem
        Caption = 'Cubic Bezier'
        ShortCut = 24643
        OnClick = mnuTClick
      end
      object mnuS: TMenuItem
        Caption = 'Cubic Spline'
        ShortCut = 24659
        OnClick = mnuTClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mnuQ: TMenuItem
        Caption = 'Quad Bezier'
        ShortCut = 24657
        OnClick = mnuTClick
      end
      object mnuT: TMenuItem
        Caption = 'Quad Spline'
        ShortCut = 24660
        OnClick = mnuTClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object mnuL: TMenuItem
        Caption = 'Line'
        ShortCut = 24652
        OnClick = mnuTClick
      end
      object mnuH: TMenuItem
        Caption = 'Horz Line'
        ShortCut = 24648
        OnClick = mnuTClick
      end
      object mnuV: TMenuItem
        Caption = 'Vert Line'
        ShortCut = 24662
        OnClick = mnuTClick
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object mnuZClose2: TMenuItem
        Caption = 'Close Path'
        ShortCut = 24666
        OnClick = mnuTClick
      end
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object mnuReverseArc2: TMenuItem
      Caption = 'Reverse &Arc'
      Enabled = False
      ShortCut = 16449
      OnClick = ReverseArcClick
    end
    object mnuRotate: TMenuItem
      Caption = '&Rotate Arc'
      Enabled = False
      ShortCut = 16466
      OnClick = mnuRotateClick
    end
    object N13: TMenuItem
      Caption = '-'
    end
    object mnuDeleteSeg2: TMenuItem
      Caption = '&Delete End Segment'
      Enabled = False
      ShortCut = 46
      OnClick = mnuDeleteClick
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.png'
    Filter = 'Image Files ( PNG, JPG, SVG)|*.png;*.jpg;*.svg'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 128
    Top = 144
  end
  object OpenDialog2: TOpenDialog
    Filter = 'SVG Files (*.svg)|*.svg'
    Left = 192
    Top = 120
  end
end
