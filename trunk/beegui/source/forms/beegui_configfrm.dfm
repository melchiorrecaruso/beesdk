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
  Constraints.MinHeight = 418
  Constraints.MinWidth = 458
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    452
    393)
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
      Caption = 'Add4Bee'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ConfigFrm_Notebook_Add4Bee: TGroupBox
        Left = 0
        Top = 0
        Width = 297
        Height = 351
        Align = alClient
        Caption = 'Add Options'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        DesignSize = (
          297
          351)
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
          DesignSize = (
            277
            167)
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
          object AddFrm_kOption: TCheckBox
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
            TabOrder = 5
          end
          object AddFrm_aOption: TCheckBox
            Left = 12
            Top = 108
            Width = 253
            Height = 20
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create Self-extractor archive'
            TabOrder = 4
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
          DesignSize = (
            277
            49)
          object AddFrm_Method: TComboBox
            Left = 10
            Top = 18
            Width = 257
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 10
            ItemHeight = 13
            ItemIndex = 3
            ParentShowHint = False
            ShowHint = False
            TabOrder = 0
            Text = '3 - Maximal'
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
          DesignSize = (
            277
            49)
          object AddFrm_Dictionary: TComboBox
            Left = 10
            Top = 18
            Width = 257
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 10
            ItemHeight = 13
            ItemIndex = 5
            ParentShowHint = False
            ShowHint = False
            TabOrder = 0
            Text = '5 - 80 MB'
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
      Caption = 'Add4Zip'
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Extract'
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        DesignSize = (
          297
          351)
        object ExtractFrm_OverWriteGB: TGroupBox
          Left = 10
          Top = 22
          Width = 277
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Overwrite mode'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          DesignSize = (
            277
            49)
          object ExtractFrm_OverWrite: TComboBox
            Left = 10
            Top = 18
            Width = 257
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
          Left = 10
          Top = 80
          Width = 277
          Height = 72
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Options'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 1
          DesignSize = (
            277
            72)
          object ExtractFrm_xCommand: TCheckBox
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
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ConfigFrm_Notebook_ToolBar: TGroupBox
        Left = 0
        Top = 0
        Width = 297
        Height = 351
        Align = alClient
        Caption = 'Skin and Buttons'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        DesignSize = (
          297
          351)
        object ToolBar_ButtonsLabel: TLabel
          Left = 10
          Top = 130
          Width = 69
          Height = 13
          Caption = 'Select Buttons'
          ParentShowHint = False
          ShowHint = False
        end
        object ToolBar_SkinGB: TGroupBox
          Left = 10
          Top = 22
          Width = 277
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Select Skin'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          DesignSize = (
            277
            99)
          object ToolBar_Skin_Image: TImage
            Left = 10
            Top = 21
            Width = 256
            Height = 32
            Anchors = [akLeft, akTop, akRight]
            ParentShowHint = False
            ShowHint = False
            Transparent = True
          end
          object ToolBar_Skin: TComboBox
            Left = 10
            Top = 65
            Width = 257
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 0
            OnClick = ToolBar_SkinClick
          end
        end
        object ToolBar_Buttons: TListView
          Left = 10
          Top = 148
          Width = 277
          Height = 192
          Anchors = [akLeft, akTop, akRight, akBottom]
          Checkboxes = True
          Columns = <>
          ParentShowHint = False
          ShowHint = False
          TabOrder = 1
          ViewStyle = vsList
          OnClick = ToolBar_ButtonsClick
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'General'
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        DesignSize = (
          297
          351)
        object General_OptionsGB: TGroupBox
          Left = 10
          Top = 80
          Width = 277
          Height = 97
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Options'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 1
          DesignSize = (
            277
            97)
          object General_Options_UpAsExit: TCheckBox
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
          object General_Options_HideMainFrm: TCheckBox
            Left = 12
            Top = 42
            Width = 253
            Height = 20
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Hide main form when add new files'
            TabOrder = 1
          end
          object General_Options_MaskKey: TCheckBox
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
        end
        object General_PriorityGB: TGroupBox
          Left = 10
          Top = 22
          Width = 277
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Process priority'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          DesignSize = (
            277
            49)
          object General_Priority: TComboBox
            Left = 10
            Top = 18
            Width = 257
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
              '3 - Real time (unstable)')
          end
        end
      end
    end
  end
end
