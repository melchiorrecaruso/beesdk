object ExtractFrm: TExtractFrm
  Left = 432
  Top = 237
  BiDiMode = bdLeftToRight
  Caption = 'Extraction Path and Options'
  ClientHeight = 403
  ClientWidth = 552
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
  OnDestroy = FormDestroy
  DesignSize = (
    552
    403)
  PixelsPerInch = 96
  TextHeight = 13
  object ExtractFrm_FolderLabel: TLabel
    Left = 8
    Top = 8
    Width = 78
    Height = 13
    Caption = 'Extraction path:'
    ParentShowHint = False
    ShowHint = False
  end
  object ExtractFrm_OverWriteGB: TGroupBox
    Left = 5
    Top = 106
    Width = 255
    Height = 49
    Caption = 'Overwrite mode'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    DesignSize = (
      255
      49)
    object ExtractFrm_OverWrite: TComboBox
      Left = 10
      Top = 18
      Width = 235
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 10
      ItemHeight = 13
      ItemIndex = 0
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      Text = 'Query if Overwrite pre-existing files'
      Items.Strings = (
        'Query if Overwrite pre-existing files'
        'Overwrite pre-existing files'
        'Skip pre-existing files')
    end
  end
  object ExtractFrm_OptionsGB: TGroupBox
    Left = 5
    Top = 162
    Width = 255
    Height = 80
    Caption = 'Options'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 3
    DesignSize = (
      255
      80)
    object ExtractFrm_xCommand: TCheckBox
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
      Top = 46
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
  object ExtractFrm_Folder: TEdit
    Left = 5
    Top = 25
    Width = 542
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
  end
  object BtnOk: TBitBtn
    Left = 472
    Top = 374
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    ParentShowHint = False
    ShowHint = False
    TabOrder = 6
  end
  object BtnCancel: TBitBtn
    Left = 391
    Top = 374
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    ParentShowHint = False
    ShowHint = False
    TabOrder = 5
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
    DesignSize = (
      255
      49)
    object General_Priority: TComboBox
      Left = 10
      Top = 18
      Width = 235
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 10
      ItemHeight = 13
      ItemIndex = 1
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      Text = '1 - Normal'
      Items.Strings = (
        '0 - Idle'
        '1 - Normal'
        '2 - Higth (unstable)'
        '3 - Real time (unstable)'
        '')
    end
  end
  object ExtractFrm_FolderTree: TTreeView
    Left = 268
    Top = 55
    Width = 279
    Height = 308
    Anchors = [akLeft, akTop, akRight, akBottom]
    Indent = 19
    TabOrder = 4
  end
end
