object RenameFrm: TRenameFrm
  Left = 371
  Top = 294
  BiDiMode = bdLeftToRight
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Rename'
  ClientHeight = 88
  ClientWidth = 272
  Color = clBtnFace
  Constraints.MaxHeight = 115
  Constraints.MinHeight = 113
  Constraints.MinWidth = 278
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = False
  ParentBiDiMode = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    272
    88)
  PixelsPerInch = 96
  TextHeight = 13
  object RenameFrm_FromLabel: TLabel
    Left = 5
    Top = 10
    Width = 35
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'From:'
    ParentShowHint = False
    ShowHint = False
  end
  object RenameFrm_ToLabel: TLabel
    Left = 5
    Top = 32
    Width = 35
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'To:'
    ParentShowHint = False
    ShowHint = False
  end
  object RenameFrm_From: TLabel
    Left = 50
    Top = 10
    Width = 215
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'ArchiveName'
    ParentShowHint = False
    ShowHint = False
  end
  object RenameFrm_To: TEdit
    Left = 50
    Top = 29
    Width = 217
    Height = 19
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
  end
  object BtnOk: TBitBtn
    Left = 192
    Top = 59
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
  end
  object BtnCancel: TBitBtn
    Left = 111
    Top = 59
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    ParentShowHint = False
    ShowHint = False
    TabOrder = 1
  end
end
