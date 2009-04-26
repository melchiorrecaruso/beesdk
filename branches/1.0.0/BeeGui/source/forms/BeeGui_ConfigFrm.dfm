object ConfigFrm: TConfigFrm
  Left = 364
  Top = 194
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Configuration'
  ClientHeight = 393
  ClientWidth = 452
  Color = clBtnFace
  Constraints.MaxHeight = 427
  Constraints.MaxWidth = 460
  Constraints.MinHeight = 420
  Constraints.MinWidth = 460
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BtnOk: TBitBtn
    Left = 372
    Top = 364
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
    Left = 291
    Top = 364
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    ParentShowHint = False
    ShowHint = False
    TabOrder = 3
    NumGlyphs = 2
  end
  object ConfigFrm_Tree: TTreeView
    Left = 5
    Top = 10
    Width = 137
    Height = 346
    HideSelection = False
    Indent = 20
    ParentShowHint = False
    ReadOnly = True
    RowSelect = True
    ShowHint = False
    TabOrder = 0
    OnChange = ConfigFrm_TreeChange
    Items.Data = {
      04000000240000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
      0B416464204F7074696F6E73280000000000000000000000FFFFFFFFFFFFFFFF
      00000000000000000F45787472616374204F7074696F6E732700000000000000
      00000000FFFFFFFFFFFFFFFF00000000000000000E536B696E20262042757474
      6F6E73200000000000000000000000FFFFFFFFFFFFFFFF000000000000000007
      47656E6572616C}
  end
  object ConfigFrm_Notebook: TNotebook
    Left = 150
    Top = 5
    Width = 297
    Height = 351
    Constraints.MaxHeight = 351
    Constraints.MaxWidth = 297
    Constraints.MinHeight = 351
    Constraints.MinWidth = 297
    ParentShowHint = False
    ShowHint = False
    TabOrder = 1
    object TPage
      Left = 0
      Top = 0
      Caption = 'Add'
      object ConfigFrm_Notebook_Add: TGroupBox
        Left = 0
        Top = 0
        Width = 297
        Height = 351
        Align = alClient
        Caption = 'Add Options'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        object AddFrm_OptionsGB: TGroupBox
          Left = 10
          Top = 136
          Width = 277
          Height = 167
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Options'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 2
          object AddFrm_rOption: TCheckBox
            Left = 12
            Top = 20
            Width = 253
            Height = 20
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
            Width = 253
            Height = 20
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create a solid archive'
            ParentShowHint = False
            ShowHint = False
            TabOrder = 1
          end
          object AddFrm_tOption: TCheckBox
            Left = 12
            Top = 64
            Width = 253
            Height = 20
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Test files after adding'
            ParentShowHint = False
            ShowHint = False
            TabOrder = 2
          end
          object AddFrm_pOption: TCheckBox
            Left = 12
            Top = 86
            Width = 253
            Height = 20
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Crypt files with password'
            ParentShowHint = False
            ShowHint = False
            TabOrder = 3
          end
          object AddFrm_cOption: TCheckBox
            Left = 12
            Top = 130
            Width = 253
            Height = 20
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
            Width = 253
            Height = 20
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create Self-extractor archive'
            TabOrder = 5
          end
        end
        object AddFrm_MethodGB: TGroupBox
          Left = 10
          Top = 22
          Width = 277
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Method'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          object AddFrm_Method: TComboBox
            Left = 10
            Top = 18
            Width = 257
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 10
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
        object AddFrm_DictionaryGB: TGroupBox
          Left = 10
          Top = 80
          Width = 277
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Dictionary size'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 1
          object AddFrm_Dictionary: TComboBox
            Left = 10
            Top = 18
            Width = 257
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
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Extract'
      object ConfigFrm_Notebook_Extract: TGroupBox
        Left = 0
        Top = 0
        Width = 297
        Height = 351
        Align = alClient
        Caption = 'Extract Options'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        object ExtractFrm_ExtrOverWrite_: TGroupBox
          Left = 10
          Top = 22
          Width = 277
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Overwrite mode'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          object ExtractFrm_ExtrOverWrite: TComboBox
            Left = 10
            Top = 18
            Width = 257
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
          Left = 10
          Top = 80
          Width = 277
          Height = 72
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Options'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 1
          object ExtractFrm_ExtrDirName: TCheckBox
            Left = 12
            Top = 20
            Width = 253
            Height = 20
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Extract and recreate the relative pathname '
            Checked = True
            ParentShowHint = False
            ShowHint = False
            State = cbChecked
            TabOrder = 0
          end
          object ExtractFrm_cOption: TCheckBox
            Left = 12
            Top = 42
            Width = 253
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
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Skin'
      object ConfigFrm_Notebook_Skin: TGroupBox
        Left = 0
        Top = 0
        Width = 297
        Height = 351
        Align = alClient
        Caption = 'Skin and Buttons'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        object ConfigFrm_Btns_SelectLabel: TLabel
          Left = 10
          Top = 130
          Width = 69
          Height = 13
          Caption = 'Select Buttons'
          ParentShowHint = False
          ShowHint = False
        end
        object ConfigFrm_Skin_SelectGB: TGroupBox
          Left = 10
          Top = 22
          Width = 277
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Select Skin'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          object ConfigFrm_Skin_Select_Image: TImage
            Left = 10
            Top = 21
            Width = 256
            Height = 32
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = False
            Transparent = True
          end
          object ConfigFrm_Skin_Select: TBeeGui_ComboBox
            Left = 10
            Top = 65
            Width = 257
            Height = 21
            Style = csDropDownList
            DropDownCount = 10
            ItemHeight = 13
            ParentShowHint = False
            ShowHint = False
            TabOrder = 0
            OnClick = ConfigFrm_Skin_SelectClick
            Mask = 'skins\*.bmp'
          end
        end
        object ConfigFrm_Btns_ListView: TListView
          Left = 10
          Top = 148
          Width = 277
          Height = 192
          Anchors = [akLeft, akTop, akRight, akBottom]
          Columns = <>
          ParentShowHint = False
          ShowHint = False
          SmallImages = ConfigFrm_Btns_Images
          TabOrder = 1
          ViewStyle = vsList
          OnClick = ConfigFrm_Btns_ListViewClick
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'General'
      object ConfigFrm_Notebook_General: TGroupBox
        Left = 0
        Top = 0
        Width = 297
        Height = 351
        Align = alClient
        Caption = 'General'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        object ConfigFrm_General_OptionsGB: TGroupBox
          Left = 10
          Top = 136
          Width = 277
          Height = 121
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Options'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          object ConfigFrm_General_Options_UpAsExit: TCheckBox
            Left = 12
            Top = 20
            Width = 253
            Height = 20
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Enable "Up" button to close the application'
            ParentShowHint = False
            ShowHint = False
            TabOrder = 0
          end
          object ConfigFrm_General_Options_HideMainFrm: TCheckBox
            Left = 12
            Top = 42
            Width = 253
            Height = 20
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Hide main form when add new files'
            TabOrder = 1
          end
          object PasswordFrm_MaskKey: TCheckBox
            Left = 12
            Top = 64
            Width = 253
            Height = 20
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Mask text when insert a password'
            Checked = True
            ParentShowHint = False
            ShowHint = False
            State = cbChecked
            TabOrder = 2
          end
          object ConfigFrm_General_Options_CheckFileType: TCheckBox
            Left = 12
            Top = 88
            Width = 253
            Height = 20
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Check .bee file type association on startup'
            TabOrder = 3
          end
        end
        object ConfigFrm_PriorityGB: TGroupBox
          Left = 10
          Top = 80
          Width = 277
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Process priority'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 1
          object ConfigFrm_Priority: TComboBox
            Left = 10
            Top = 18
            Width = 257
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
        object ConfigFrm_LanguageGB: TGroupBox
          Left = 10
          Top = 22
          Width = 277
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Language'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 2
          object ConfigFrm_Language: TBeeGui_ComboBox
            Left = 10
            Top = 18
            Width = 257
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 10
            ItemHeight = 13
            ParentShowHint = False
            ShowHint = False
            TabOrder = 0
            Mask = 'languages\*.ini'
          end
        end
      end
    end
  end
  object ConfigFrm_Btns_Images: TImageList
    Left = 104
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
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000C001C00100000000
      C001C00100000000CFF9CFF900000000CEF9CFF900000000CC79CFF900000000
      C839CFF900000000C919CFF900000000CB89CFF900000000CFC9CFF900000000
      CFE9CFF900000000CFF9CFF900000000C001C00100000000C001C00100000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
