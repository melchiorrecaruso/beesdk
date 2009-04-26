object AddFrm: TAddFrm
  Left = 330
  Top = 204
  Anchors = [akLeft, akRight, akBottom]
  AutoScroll = False
  Caption = 'Archive Name and Options'
  ClientHeight = 357
  ClientWidth = 444
  Color = clBtnFace
  Constraints.MinHeight = 384
  Constraints.MinWidth = 452
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BtnOk: TBitBtn
    Left = 364
    Top = 328
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
  end
  object BtnCancel: TBitBtn
    Left = 283
    Top = 328
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
  end
  object AddFrm_Pages: TPageControl
    Left = 5
    Top = 6
    Width = 434
    Height = 316
    ActivePage = AddFrm_Pages_General
    Anchors = [akLeft, akTop, akRight, akBottom]
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    object AddFrm_Pages_General: TTabSheet
      Caption = 'General'
      ParentShowHint = False
      ShowHint = False
      object AddFrm_OptionsGB: TGroupBox
        Left = 208
        Top = 61
        Width = 207
        Height = 162
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Options'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 3
        object AddFrm_rOption: TCheckBox
          Left = 12
          Top = 20
          Width = 185
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Recurse subdirectories'
          Checked = True
          ParentShowHint = False
          ShowHint = False
          State = cbChecked
          TabOrder = 0
        end
        object AddFrm_sOption: TCheckBox
          Left = 12
          Top = 42
          Width = 185
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Create a solid archive'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 1
        end
        object AddFrm_tOption: TCheckBox
          Left = 12
          Top = 64
          Width = 185
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Test files after adding'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 2
        end
        object AddFrm_kOption: TCheckBox
          Left = 12
          Top = 86
          Width = 185
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Crypt files with password'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 3
        end
        object AddFrm_cOption: TCheckBox
          Left = 12
          Top = 130
          Width = 185
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Use current archive directory'
          Checked = True
          ParentShowHint = False
          ShowHint = False
          State = cbChecked
          TabOrder = 4
        end
        object AddFrm_aOption: TCheckBox
          Left = 12
          Top = 108
          Width = 185
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Create Self-extractor archive'
          TabOrder = 5
        end
      end
      object AddFrm_MethodGB: TGroupBox
        Left = 10
        Top = 61
        Width = 185
        Height = 49
        Caption = 'Method'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 2
        object AddFrm_Method: TComboBox
          Left = 10
          Top = 18
          Width = 165
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          Items.Strings = (
            '0 - Store'
            '1 - Low'
            '2 - Good'
            '3 - Maximal')
        end
      end
      object AddFrm_ActionGB: TGroupBox
        Left = 10
        Top = 6
        Width = 406
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Action:'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        object AddFrm_Action: TComboBox
          Left = 10
          Top = 18
          Width = 386
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          BiDiMode = bdLeftToRight
          ItemHeight = 13
          ParentBiDiMode = False
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          Items.Strings = (
            'Update files'
            'Freshen files'
            'Update and freshen files')
        end
      end
      object AddFrm_DictionaryGB: TGroupBox
        Left = 10
        Top = 117
        Width = 185
        Height = 49
        Caption = 'Dictionary size'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 4
        object AddFrm_Dictionary: TComboBox
          Left = 10
          Top = 18
          Width = 165
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 10
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          Items.Strings = (
            '0 - 2 MB'
            '1 - 5 MB'
            '2 - 10 MB'
            '3 - 20 MB'
            '4 - 40 MB'
            '5 - 80 MB'
            '6 - 160 MB'
            '7 - 320 MB'
            '8 - 640 MB'
            '9 - 1280 MB')
        end
      end
      object ConfigFrm_PriorityGB: TGroupBox
        Left = 10
        Top = 174
        Width = 185
        Height = 49
        Caption = 'Process priority'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 1
        object ConfigFrm_Priority: TComboBox
          Left = 10
          Top = 18
          Width = 165
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
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
      object AddFrm_ForceExtGB: TGroupBox
        Left = 10
        Top = 230
        Width = 405
        Height = 49
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Force file extension'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 5
        object AddFrm_eOption: TEdit
          Left = 10
          Top = 18
          Width = 385
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
        end
      end
    end
    object AddFrm_Pages_Files: TTabSheet
      Caption = 'Files'
      ImageIndex = 2
      ParentShowHint = False
      ShowHint = False
      object AddFrm_FilesTitle: TLabel
        Left = 10
        Top = 6
        Width = 231
        Height = 13
        Caption = 'Files and/or Directories to Add and/or to Exclude'
        ParentShowHint = False
        ShowHint = False
      end
      object BtnDir: TSpeedButton
        Left = 340
        Top = 24
        Width = 75
        Height = 23
        Anchors = [akTop, akRight]
        Caption = 'Add folder'
        ParentShowHint = False
        ShowHint = False
        OnClick = AddFrm_PMenu_AddDirClick
      end
      object BtnFile: TSpeedButton
        Left = 340
        Top = 48
        Width = 75
        Height = 23
        Anchors = [akTop, akRight]
        Caption = 'Add file'
        ParentShowHint = False
        ShowHint = False
        OnClick = AddFrm_PMenu_AddFilesClick
      end
      object BtnView: TSpeedButton
        Left = 340
        Top = 80
        Width = 75
        Height = 23
        Anchors = [akTop, akRight]
        Caption = 'View'
        Enabled = False
        ParentShowHint = False
        ShowHint = False
        OnClick = AddFrm_PMenu_ViewClick
      end
      object BtnDelete: TSpeedButton
        Left = 340
        Top = 160
        Width = 75
        Height = 23
        Anchors = [akTop, akRight]
        Caption = 'Delete'
        Enabled = False
        ParentShowHint = False
        ShowHint = False
        OnClick = AddFrm_PMenu_DeleteClick
      end
      object BtnModify: TSpeedButton
        Left = 340
        Top = 128
        Width = 75
        Height = 23
        Anchors = [akTop, akRight]
        Caption = 'Modify'
        Enabled = False
        ParentShowHint = False
        ShowHint = False
        OnClick = AddFrm_PMenu_ModifyClick
      end
      object BtnType: TSpeedButton
        Left = 340
        Top = 104
        Width = 75
        Height = 23
        Anchors = [akTop, akRight]
        Caption = '+  /  -'
        Enabled = False
        ParentShowHint = False
        ShowHint = False
        OnClick = AddFrm_PMenu_TypeClick
      end
      object AddFrm_Root_: TLabel
        Left = 10
        Top = 240
        Width = 98
        Height = 13
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Current root directory'
        ParentShowHint = False
        ShowHint = False
      end
      object AddFrm_Root: TEdit
        Left = 10
        Top = 258
        Width = 315
        Height = 21
        TabStop = False
        Anchors = [akLeft, akRight, akBottom]
        Color = clBtnFace
        OEMConvert = True
        ParentShowHint = False
        ReadOnly = True
        ShowHint = False
        TabOrder = 1
      end
      object BtnUpDown: TUpDown
        Left = 340
        Top = 258
        Width = 75
        Height = 21
        Anchors = [akRight, akBottom]
        Min = -1000
        Max = 1000
        Orientation = udHorizontal
        ParentShowHint = False
        Position = 0
        ShowHint = False
        TabOrder = 0
        Thousands = False
        Wrap = False
        OnClick = BtnUpDownClick
      end
      object AddFrm_FilesList: TBeeGui_ListView_Add
        Left = 10
        Top = 24
        Width = 315
        Height = 208
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Name'
            Width = 290
          end>
        MultiSelect = True
        PopupMenu = AddFrm_PMenu
        SmallImages = AddFrm_FilesImages
        TabOrder = 2
        ViewStyle = vsReport
        OnSelectItem = AddFrm_FilesListSelectItem
        Root = AddFrm_Root
      end
    end
  end
  object AddFrm_MSGs: TComboBox
    Left = 142
    Top = 328
    Width = 123
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
      'Select directory to ADD'
      'Modify mask')
  end
  object AddFrm_PMenu: TPopupMenu
    TrackButton = tbLeftButton
    Left = 8
    Top = 320
    object AddFrm_PMenu_AddDir: TMenuItem
      Caption = 'AddDir'
      ShortCut = 16452
      OnClick = AddFrm_PMenu_AddDirClick
    end
    object AddFrm_PMenu_AddFiles: TMenuItem
      Caption = 'AddFiles'
      ShortCut = 16454
      OnClick = AddFrm_PMenu_AddFilesClick
    end
    object AddFrm_PMenu_N1: TMenuItem
      Caption = '-'
    end
    object AddFrm_PMenu_View: TMenuItem
      Caption = '&View...'
      Enabled = False
      ShortCut = 16470
      OnClick = AddFrm_PMenu_ViewClick
    end
    object AddFrm_PMenu_Type: TMenuItem
      Caption = '+  /  -'
      Enabled = False
      ShortCut = 16472
      OnClick = AddFrm_PMenu_TypeClick
    end
    object AddFrm_PMenu_Modify: TMenuItem
      Caption = 'Modify'
      Enabled = False
      ShortCut = 16461
      OnClick = AddFrm_PMenu_ModifyClick
    end
    object AddFrm_PMenu_N2: TMenuItem
      Caption = '-'
    end
    object AddFrm_PMenu_Delete: TMenuItem
      Caption = '&Delete'
      Enabled = False
      ShortCut = 46
      OnClick = AddFrm_PMenu_DeleteClick
    end
  end
  object AddFrm_FilesImages: TImageList
    Left = 40
    Top = 320
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      FC3FFFFF00000000FC3FFFFF00000000FC3FFFFF00000000FC3FFFFF00000000
      C003C00300000000C003C00300000000C003C00300000000C003C00300000000
      FC3FFFFF00000000FC3FFFFF00000000FC3FFFFF00000000FC3FFFFF00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object AddFrm_OpenDialog: TOpenDialog
    Filter = 'All files|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 72
    Top = 320
  end
  object AddFrm_DragFilesTrg: TDragFilesTrg
    Enabled = True
    Target = AddFrm_FilesList
    OnDrop = AddFrm_DragFilesTrgDrop
    Left = 106
    Top = 322
  end
end
