object SelectFrm: TSelectFrm
  Left = 483
  Top = 341
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = '(Un)Select...'
  ClientHeight = 82
  ClientWidth = 277
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SelectFrm_Mask_: TLabel
    Left = 5
    Top = 6
    Width = 57
    Height = 13
    Caption = 'Insert mask:'
    ParentShowHint = False
    ShowHint = False
  end
  object BtnOk: TBitBtn
    Left = 197
    Top = 53
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    NumGlyphs = 2
    Spacing = -1
  end
  object BtnCancel: TBitBtn
    Left = 117
    Top = 53
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    ParentShowHint = False
    ShowHint = False
    TabOrder = 1
    NumGlyphs = 2
    Spacing = -1
  end
  object SelectFrm_Mask: TEdit
    Left = 5
    Top = 23
    Width = 267
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    Text = '*.*'
  end
  object SelectFrm_FrmCaptionCB: TComboBox
    Left = 5
    Top = 54
    Width = 84
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = False
    TabOrder = 3
    Visible = False
    Items.Strings = (
      'Select...'
      'UnSelect...')
  end
end
