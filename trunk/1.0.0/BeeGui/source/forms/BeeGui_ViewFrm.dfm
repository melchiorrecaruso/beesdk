object ViewFrm: TViewFrm
  Left = 306
  Top = 202
  Width = 515
  Height = 340
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'View:'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ViewFrm_ViewMemo: TMemo
    Left = 0
    Top = 0
    Width = 507
    Height = 270
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object BtnFont: TBitBtn
    Left = 91
    Top = 280
    Width = 70
    Height = 24
    Anchors = [akLeft, akBottom]
    Caption = '&Font'
    TabOrder = 1
    OnClick = BtnFontClick
  end
  object BtnOk: TBitBtn
    Left = 12
    Top = 280
    Width = 70
    Height = 24
    Anchors = [akLeft, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 2
    NumGlyphs = 2
  end
  object ViewFrm_StatusBar: TStatusBar
    Left = 472
    Top = 289
    Width = 35
    Height = 24
    Align = alNone
    Anchors = [akRight, akBottom]
    Panels = <
      item
        Bevel = pbNone
        Width = 50
      end>
    SimplePanel = False
  end
  object ViewFrm_FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 8
    Top = 8
  end
end
