object AddFrm: TAddFrm
  Left = 330
  Top = 204
  Anchors = [akLeft, akRight, akBottom]
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
  DesignSize = (
    444
    357)
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
    TabOrder = 2
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
  object AddFrm_Messages: TComboBox
    Left = 103
    Top = 328
    Width = 75
    Height = 21
    Style = csDropDownList
    Enabled = False
    ItemHeight = 13
    ParentColor = True
    TabOrder = 0
    Visible = False
    Items.Strings = (
      'Select a folder to add'
      'Modify mask')
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
    TabOrder = 3
    object AddFrm_Pages_General: TTabSheet
      Caption = 'General'
      ParentShowHint = False
      ShowHint = False
      DesignSize = (
        426
        288)
      object AddFrm_OptionsGB: TGroupBox
        Left = 208
        Top = 61
        Width = 207
        Height = 162
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Options'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 5
        DesignSize = (
          207
          162)
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
          TabOrder = 5
        end
        object AddFrm_aOption: TCheckBox
          Left = 12
          Top = 108
          Width = 185
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Create Self-extractor archive'
          TabOrder = 4
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
        TabOrder = 1
        DesignSize = (
          185
          49)
        object AddFrm_Method: TComboBox
          Left = 10
          Top = 18
          Width = 165
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          ItemIndex = 2
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          Text = '2 - Good'
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
        Width = 405
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Action:'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        DesignSize = (
          405
          49)
        object AddFrm_Action: TComboBox
          Left = 10
          Top = 18
          Width = 385
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          BiDiMode = bdLeftToRight
          ItemHeight = 13
          ItemIndex = 0
          ParentBiDiMode = False
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          Text = 'Update and freshen files'
          Items.Strings = (
            'Update and freshen files'
            'Update files'
            'Freshen files')
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
        TabOrder = 2
        DesignSize = (
          185
          49)
        object AddFrm_Dictionary: TComboBox
          Left = 10
          Top = 18
          Width = 165
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 10
          ItemHeight = 13
          ItemIndex = 2
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          Text = '2 - 10 MB'
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
      object AddFrm_PriorityGB: TGroupBox
        Left = 10
        Top = 174
        Width = 185
        Height = 49
        Caption = 'Process priority'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 3
        DesignSize = (
          185
          49)
        object General_Priority: TComboBox
          Left = 10
          Top = 18
          Width = 165
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
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
            '3 - Real time (unstable)')
        end
      end
      object AddFrm_eOptionGB: TGroupBox
        Left = 10
        Top = 230
        Width = 405
        Height = 49
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Force file extension'
        Enabled = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 4
        DesignSize = (
          405
          49)
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
      object AddFrm_ZipCompressionLevelGB: TGroupBox
        Left = 10
        Top = 117
        Width = 185
        Height = 49
        Caption = 'Compression level'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 6
        DesignSize = (
          185
          49)
        object AddFrm_ZipCompressionLevel: TComboBox
          Left = 10
          Top = 18
          Width = 165
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 10
          ItemHeight = 13
          ItemIndex = 2
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          Text = '2 - Normal'
          Items.Strings = (
            '0 - Superfast'
            '1 - Fast'
            '2 - Normal'
            '3 - Maximum')
        end
      end
      object AddFrm_ZipMethodGB: TGroupBox
        Left = 10
        Top = 61
        Width = 185
        Height = 49
        Caption = 'Method'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 7
        DesignSize = (
          185
          49)
        object AddFrm_ZipMethod: TComboBox
          Left = 10
          Top = 18
          Width = 165
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          ItemIndex = 1
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          Text = '1 - Deflate'
          Items.Strings = (
            '0 - Store'
            '1 - Deflate'
            '2 - Bestmethod')
        end
      end
    end
    object AddFrm_Pages_Files: TTabSheet
      Caption = 'Files'
      ImageIndex = 2
      ParentShowHint = False
      ShowHint = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        426
        288)
      object AddFrm_FileListLabel: TLabel
        Left = 10
        Top = 6
        Width = 233
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
      object AddFrm_RootLabel: TLabel
        Left = 10
        Top = 240
        Width = 106
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
        ShowHint = False
        TabOrder = 2
        Thousands = False
        OnClick = BtnUpDownClick
      end
      object AddFrm_FileList: TListView
        Left = 10
        Top = 25
        Width = 315
        Height = 209
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Name'
            Width = 290
          end>
        LargeImages = AddFrm_FileImages
        MultiSelect = True
        ReadOnly = True
        PopupMenu = AddFrm_PMenu
        SmallImages = AddFrm_FileImages
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = AddFrm_FilesListSelectItem
      end
    end
  end
  object AddFrm_PMenu: TPopupMenu
    TrackButton = tbLeftButton
    Left = 71
    Top = 325
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
  object AddFrm_FileImages: TImageList
    Left = 40
    Top = 325
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
    Left = 8
    Top = 325
  end
end
