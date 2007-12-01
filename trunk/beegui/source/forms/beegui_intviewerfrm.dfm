object IntViewerFrm: TIntViewerFrm
  Left = 371
  Top = 256
  Caption = 'Viewer'
  ClientHeight = 507
  ClientWidth = 525
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    525
    507)
  PixelsPerInch = 96
  TextHeight = 13
  object BtnOk: TBitBtn
    Left = 445
    Top = 478
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Default'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
  end
  object IntViewerFrm_Viewer: TMemo
    Left = 0
    Top = 0
    Width = 525
    Height = 470
    Anchors = [akLeft, akTop, akRight, akBottom]
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = False
    TabOrder = 1
  end
  object BtnFont: TBitBtn
    Left = 5
    Top = 478
    Width = 75
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = 'Font'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    OnClick = BtnFontClick
  end
  object BtnSave: TBitBtn
    Left = 86
    Top = 478
    Width = 75
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 3
    OnClick = BtnSaveClick
  end
  object IntViewerFrm_FontDialog: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Default'
    Font.Style = []
    Left = 8
    Top = 8
  end
  object IntViewerFrm_SaveDialog: TSaveDialog
    Filter = 'TXTfiles|*.txt|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 40
    Top = 8
  end
end
