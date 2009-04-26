object ExtractFrm: TExtractFrm
  Left = 432
  Top = 237
  AutoScroll = False
  BiDiMode = bdLeftToRight
  Caption = 'Extraction Path and Options'
  ClientHeight = 417
  ClientWidth = 556
  Color = clBtnFace
  Constraints.MinHeight = 430
  Constraints.MinWidth = 560
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = False
  ParentBiDiMode = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ExtractFrm_ExtrDir_: TLabel
    Left = 8
    Top = 8
    Width = 74
    Height = 13
    Caption = 'Extraction path:'
    ParentShowHint = False
    ShowHint = False
  end
  object ExtractFrm_ExtrOverWrite_: TGroupBox
    Left = 5
    Top = 106
    Width = 255
    Height = 49
    Caption = 'Overwrite mode'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    object ExtractFrm_ExtrOverWrite: TComboBox
      Left = 10
      Top = 18
      Width = 235
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 10
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      Items.Strings = (
        'Skip pre-existing files'
        'Overwrite pre-existing files'
        'Query if Overwrite pre-existing files')
    end
  end
  object ExtractFrm_ExtrOptions: TGroupBox
    Left = 5
    Top = 162
    Width = 255
    Height = 72
    Caption = 'Options'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 3
    object ExtractFrm_ExtrDirName: TCheckBox
      Left = 12
      Top = 20
      Width = 235
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Extract and Recreate the relative pathname '
      Checked = True
      ParentShowHint = False
      ShowHint = False
      State = cbChecked
      TabOrder = 0
    end
    object ExtractFrm_cOption: TCheckBox
      Left = 12
      Top = 42
      Width = 235
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use current archive directory'
      Checked = True
      ParentShowHint = False
      ShowHint = False
      State = cbChecked
      TabOrder = 1
    end
  end
  object ExtractFrm_ExtrDir: TEdit
    Left = 5
    Top = 25
    Width = 546
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
  end
  object BtnOk: TBitBtn
    Left = 476
    Top = 388
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    ParentShowHint = False
    ShowHint = False
    TabOrder = 5
  end
  object BtnCancel: TBitBtn
    Left = 395
    Top = 388
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    ParentShowHint = False
    ShowHint = False
    TabOrder = 4
  end
  object ConfigFrm_PriorityGB: TGroupBox
    Left = 5
    Top = 50
    Width = 255
    Height = 49
    Caption = 'Process priority'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 1
    object ConfigFrm_Priority: TComboBox
      Left = 10
      Top = 18
      Width = 235
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 10
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      Items.Strings = (
        '3 - Real time (unstable)'
        '2 - Higth (unstable)'
        '1 - Normal '
        '0 - Idle')
    end
  end
  object ExtractFrm_Tree: TBeeGui_TreeView
    Left = 269
    Top = 55
    Width = 282
    Height = 324
    Anchors = [akLeft, akTop, akRight, akBottom]
    Indent = 19
    ParentShowHint = False
    ReadOnly = True
    ShowHint = False
    TabOrder = 6
    PathEdit = ExtractFrm_ExtrDir
  end
end
