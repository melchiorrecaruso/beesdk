object MainFrm: TMainFrm
  Left = 350
  Top = 263
  Caption = 'BeeGui BETA - Welcome'
  ClientHeight = 517
  ClientWidth = 739
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  Menu = MainFrm_MainMenu
  OldCreateOrder = False
  Position = poDesigned
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MainFrm_Spacer: TBevel
    Left = 0
    Top = 98
    Width = 739
    Height = 3
    Align = alTop
    ParentShowHint = False
    Shape = bsSpacer
    ShowHint = False
    Style = bsRaised
    ExplicitWidth = 782
  end
  object MainFrm_StatusBar: TStatusBar
    Left = 0
    Top = 496
    Width = 739
    Height = 21
    Panels = <
      item
        Width = 500
      end
      item
        Bevel = pbNone
        Style = psOwnerDraw
        Width = 50
      end>
    ParentColor = True
    ParentFont = True
    ParentShowHint = False
    ShowHint = False
    UseSystemFont = False
    OnDrawPanel = MainFrm_StatusBarDrawPanel
    OnResize = MainFrm_StatusBarResize
  end
  object MainFrm_ButtonsPanel: TPanel
    Left = 0
    Top = 0
    Width = 739
    Height = 98
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentColor = True
    ParentShowHint = False
    PopupMenu = MainFrm_PopupMenu_Buttons
    ShowHint = False
    TabOrder = 1
    DesignSize = (
      739
      98)
    object MainFrm_BtnUp: TSpeedButton
      Left = 4
      Top = 70
      Width = 64
      Height = 21
      Caption = 'Up'
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000FFFFFFFFF000000
        0FFFFFFFFF000FFFFFFFFFFFFF000FFFFFFFFFFFFF000FFFFFFFFFF000000000
        FFFFFFFF0000000FFFFFFFFFF00000FFFFFFFFFFFF000FFFFFFFFFFFFFF0FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = False
      Spacing = 3
      OnClick = MainFrm_BtnUpClick
    end
    object MainFrm_BtnAdd: TSpeedButton
      Left = 131
      Top = 3
      Width = 60
      Height = 55
      Caption = 'Add'
      Flat = True
      Layout = blGlyphTop
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = False
      Spacing = 1
      OnClick = MainFrm_MainMenu_Actions_AddClick
    end
    object MainFrm_BtnExtract: TSpeedButton
      Left = 196
      Top = 3
      Width = 60
      Height = 55
      Caption = 'Extract'
      Flat = True
      Layout = blGlyphTop
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = False
      Spacing = 1
      OnClick = MainFrm_MainMenu_Actions_ExtractClick
    end
    object MainFrm_BtnView: TSpeedButton
      Left = 262
      Top = 3
      Width = 60
      Height = 55
      Caption = 'View'
      Flat = True
      Layout = blGlyphTop
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = False
      Spacing = 1
      OnClick = MainFrm_MainMenu_Actions_ViewClick
    end
    object MainFrm_BtnDelete: TSpeedButton
      Left = 328
      Top = 4
      Width = 60
      Height = 55
      Caption = 'Delete'
      Flat = True
      Layout = blGlyphTop
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = False
      Spacing = 1
      OnClick = MainFrm_MainMenu_Actions_DeleteClick
    end
    object MainFrm_BtnTest: TSpeedButton
      Left = 392
      Top = 4
      Width = 60
      Height = 55
      Caption = 'Test'
      Flat = True
      Layout = blGlyphTop
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = False
      Spacing = 1
      OnClick = MainFrm_MainMenu_Actions_TestClick
    end
    object MainFrm_BtnInfo: TSpeedButton
      Left = 456
      Top = 4
      Width = 60
      Height = 55
      Caption = 'Info'
      Flat = True
      Layout = blGlyphTop
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = False
      Spacing = 1
      OnClick = MainFrm_MainMenu_File_InfoClick
    end
    object MainFrm_BtnCheckOut: TSpeedButton
      Left = 520
      Top = 4
      Width = 60
      Height = 55
      Caption = 'CheckOut'
      Flat = True
      Layout = blGlyphTop
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = False
      Spacing = 1
      OnClick = MainFrm_MainMenu_Actions_CheckOutClick
    end
    object MainFrm_BtnHelp: TSpeedButton
      Left = 584
      Top = 4
      Width = 60
      Height = 55
      Caption = 'Help'
      Flat = True
      Layout = blGlyphTop
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = False
      Spacing = 1
      OnClick = MainFrm_MainMenu_Help_F1Click
    end
    object MainFrm_BtnExit: TSpeedButton
      Left = 648
      Top = 4
      Width = 60
      Height = 55
      Caption = 'Exit'
      Flat = True
      Layout = blGlyphTop
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = False
      Spacing = 1
      OnClick = MainFrm_MainMenu_File_ExitClick
    end
    object MainFrm_BtnNew: TSpeedButton
      Left = 2
      Top = 3
      Width = 60
      Height = 55
      Caption = 'New'
      Flat = True
      Layout = blGlyphTop
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = False
      Spacing = 1
      OnClick = MainFrm_MainMenu_File_NewClick
    end
    object MainFrm_BtnOpen: TSpeedButton
      Left = 68
      Top = 4
      Width = 60
      Height = 55
      Caption = 'Open'
      Flat = True
      Layout = blGlyphTop
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = False
      Spacing = 1
      OnClick = MainFrm_MainMenu_File_OpenClick
    end
    object MainFrm_Bevel: TBevel
      Left = 4
      Top = 64
      Width = 731
      Height = 2
      Anchors = [akLeft, akTop, akRight]
      ExplicitWidth = 774
    end
    object MainFrm_BevelFirst: TBevel
      Left = 712
      Top = 4
      Width = 2
      Height = 55
      ParentShowHint = False
      ShowHint = False
      Visible = False
    end
    object MainFrm_BevelSecond: TBevel
      Left = 720
      Top = 4
      Width = 2
      Height = 55
      ParentShowHint = False
      ShowHint = False
      Visible = False
    end
    object MainFrm_BevelThird: TBevel
      Left = 728
      Top = 4
      Width = 2
      Height = 55
      ParentShowHint = False
      ShowHint = False
      Visible = False
    end
    object MainFrm_PathBox: TComboBox
      Left = 74
      Top = 71
      Width = 663
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 10
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      OnClick = MainFrm_PathBoxClick
    end
    object MainFrm_Messages: TComboBox
      Left = 74
      Top = 71
      Width = 183
      Height = 21
      Style = csDropDownList
      Color = clBtnFace
      DropDownCount = 10
      Enabled = False
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = False
      TabOrder = 1
      Visible = False
      Items.Strings = (
        'Select a directory where move the archive'
        'error on moving the archive'
        'Select a directory where copy the archive'
        'error on copying the archive'
        'Rename archive'
        'error on renaming the archive'
        'Do you want really to delete the archive?'
        'error on deleting the archive'
        'Rename file'
        'error on renaming file'
        'Is necessary restart the application. Continue?'
        'error deleting setting file'
        'Do You want to delete files selected?'
        'Actually there is an active process. Terminate this process?'
        'Actually there is an active process.'
        'No files selected.'
        'Items selected: %u'
        'Items: %u'
        'CheckOut directory founded. Delete?'
        'error on create CheckOut directory.'
        'error on set current directory.'
        'Rename directory'
        'error renaming directory'
        'Associate .Bee file type with BeeGui application?'
        'File is modified. Do You want freshen it?'
        'Can'#39#39't load archive infomations')
    end
  end
  object MainFrm_ListView: TListView
    Left = 0
    Top = 101
    Width = 739
    Height = 395
    Align = alClient
    Columns = <>
    PopupMenu = MainFrm_PopupMenu
    TabOrder = 0
    OnClick = MainFrm_ListViewClick
    OnColumnClick = MainFrm_ListViewColumnClick
    OnDblClick = MainFrm_MainMenu_Actions_ViewClick
    OnMouseDown = MainFrm_ListViewMouseDown
    OnMouseMove = MainFrm_ListViewMouseMove
  end
  object MainFrm_ProgressBar: TProgressBar
    Left = 8
    Top = 200
    Width = 92
    Height = 17
    TabOrder = 3
  end
  object MainFrm_OpenDialog: TOpenDialog
    Filter = 
      'Supported archives  (*.bee,*.zip,*.gz,*.tar,*.tgz,*.cab)|*.bee; ' +
      '*.zip;*.gz;*.tar;*.tgz;|Exe file (*.exe)|*.exe|Any file (*.*)|*.' +
      '*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 8
    Top = 168
  end
  object MainFrm_PopupMenu: TPopupMenu
    OnPopup = MainFrm_PopupMenuPopup
    Left = 40
    Top = 136
    object MainFrm_PopupMenu_Open: TMenuItem
      Caption = 'Open...'
      Default = True
      OnClick = MainFrm_MainMenu_Actions_ViewClick
    end
    object MainFrm_PopupMenu_Open_IntViewer: TMenuItem
      Caption = 'Open with internal viewer'
      OnClick = MainFrm_PopupMenu_Open_IntViewerClick
    end
    object MainFrm_PopupMenu_N1: TMenuItem
      Caption = '-'
    end
    object MainFrm_PopupMenu_Extract: TMenuItem
      Caption = 'Extract To...'
      ShortCut = 32837
      OnClick = MainFrm_MainMenu_Actions_ExtractClick
    end
    object MainFrm_PopupMenu_ExtractAll: TMenuItem
      Caption = 'Extract All To...'
      OnClick = MainFrm_MainMenu_Actions_ExtractAllClick
    end
    object MainFrm_PopupMenu_Test: TMenuItem
      Caption = 'Test Selected Files'
      ShortCut = 32838
      OnClick = MainFrm_MainMenu_Actions_TestClick
    end
    object MainFrm_PopupMenu_N2: TMenuItem
      Caption = '-'
    end
    object MainFrm_PopupMenu_Delete: TMenuItem
      Caption = 'Delete Selected Files'
      ShortCut = 32836
      OnClick = MainFrm_MainMenu_Actions_DeleteClick
    end
    object MainFrm_PopupMenu_Rename: TMenuItem
      Caption = 'Rename Selected Files'
      OnClick = MainFrm_MainMenu_Actions_RenameClick
    end
    object MainFrm_PopupMenu_N3: TMenuItem
      Caption = '-'
    end
    object MainFrm_PopupMenu_OrderBy: TMenuItem
      Caption = 'Order by'
      object MainFrm_PopupMenu_OrderBy_Name: TMenuItem
        Caption = 'Name'
        Checked = True
        OnClick = MainFrm_MainMenu_View_OrderBy_NameClick
      end
      object MainFrm_PopupMenu_OrderBy_Size: TMenuItem
        Caption = 'Size'
        OnClick = MainFrm_MainMenu_View_OrderBy_SizeClick
      end
      object MainFrm_PopupMenu_OrderBy_Packed: TMenuItem
        Caption = 'Packed'
        OnClick = MainFrm_MainMenu_View_OrderBy_PackedClick
      end
      object MainFrm_PopupMenu_OrderBy_Ratio: TMenuItem
        Caption = 'Ratio'
        OnClick = MainFrm_MainMenu_View_OrderBy_RatioClick
      end
      object MainFrm_PopupMenu_OrderBy_Type: TMenuItem
        Caption = 'Type'
        OnClick = MainFrm_MainMenu_View_OrderBy_TypeClick
      end
      object MainFrm_PopupMenu_OrderBy_Modified: TMenuItem
        Caption = 'Modified'
        OnClick = MainFrm_MainMenu_View_OrderBy_ModifiedClick
      end
      object MainFrm_PopupMenu_OrderBy_Attributes: TMenuItem
        Caption = 'Attributes'
        OnClick = MainFrm_MainMenu_View_OrderBy_AttributesClick
      end
      object MainFrm_PopupMenu_OrderBy_Method: TMenuItem
        Caption = 'Method'
        OnClick = MainFrm_MainMenu_View_OrderBy_MethodClick
      end
      object MainFrm_PopupMenu_OrderBy_Password: TMenuItem
        Caption = 'Password'
        OnClick = MainFrm_MainMenu_View_OrderBy_PasswordClick
      end
      object MainFrm_PopupMenu_OrderBy_CRC: TMenuItem
        Caption = 'CRC'
        OnClick = MainFrm_MainMenu_View_OrderBy_CRCClick
      end
      object MainFrm_PopupMenu_OrderBy_Path: TMenuItem
        Caption = 'Path'
        OnClick = MainFrm_MainMenu_View_OrderBy_PathClick
      end
      object MainFrm_PopupMenu_OrderBy_Position: TMenuItem
        Caption = 'Position'
        OnClick = MainFrm_MainMenu_View_OrderBy_PositionClick
      end
    end
    object MainFrm_PopupMenu_Details: TMenuItem
      Caption = 'Details'
      object MainFrm_PopupMenu_Details_Name: TMenuItem
        Caption = 'Name'
        Checked = True
        Enabled = False
        OnClick = MainFrm_MainMenu_View_Details_NameClick
      end
      object MainFrm_PopupMenu_Details_Size: TMenuItem
        Caption = 'Size'
        Checked = True
        OnClick = MainFrm_MainMenu_View_Details_SizeClick
      end
      object MainFrm_PopupMenu_Details_Packed: TMenuItem
        Caption = 'Packed'
        Checked = True
        OnClick = MainFrm_MainMenu_View_Details_PackedClick
      end
      object MainFrm_PopupMenu_Details_Ratio: TMenuItem
        Caption = 'Ratio'
        Checked = True
        OnClick = MainFrm_MainMenu_View_Details_RatioClick
      end
      object MainFrm_PopupMenu_Details_Type: TMenuItem
        Caption = 'Type'
        Checked = True
        OnClick = MainFrm_MainMenu_View_Details_TypeClick
      end
      object MainFrm_PopupMenu_Details_Modified: TMenuItem
        Caption = 'Modified'
        Checked = True
        OnClick = MainFrm_MainMenu_View_Details_ModifiedClick
      end
      object MainFrm_PopupMenu_Details_Attributes: TMenuItem
        Caption = 'Attributes'
        Checked = True
        OnClick = MainFrm_MainMenu_View_Details_AttributesClick
      end
      object MainFrm_PopupMenu_Details_Method: TMenuItem
        Caption = 'Method'
        Checked = True
        OnClick = MainFrm_MainMenu_View_Details_MethodClick
      end
      object MainFrm_PopupMenu_Details_Password: TMenuItem
        Caption = 'Password'
        Checked = True
        OnClick = MainFrm_MainMenu_View_Details_PasswordClick
      end
      object MainFrm_PopupMenu_Details_CRC: TMenuItem
        Caption = 'CRC'
        Checked = True
        OnClick = MainFrm_MainMenu_View_Details_CRCClick
      end
      object MainFrm_PopupMenu_Details_Path: TMenuItem
        Caption = 'Path'
        Checked = True
        OnClick = MainFrm_MainMenu_View_Details_PathClick
      end
      object MainFrm_PopupMenu_Details_Position: TMenuItem
        Caption = 'Position'
        Checked = True
        OnClick = MainFrm_MainMenu_View_Details_PositionClick
      end
    end
  end
  object MainFrm_FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 72
    Top = 168
  end
  object MainFrm_MainMenu: TMainMenu
    Left = 8
    Top = 136
    object MainFrm_MainMenu_File: TMenuItem
      Caption = 'File'
      object MainFrm_MainMenu_File_New: TMenuItem
        Caption = 'New...'
        ShortCut = 16462
        OnClick = MainFrm_MainMenu_File_NewClick
      end
      object MainFrm_MainMenu_File_Open: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = MainFrm_MainMenu_File_OpenClick
      end
      object MainFrm_MainMenu_File_Close: TMenuItem
        Caption = 'Close'
        ShortCut = 16451
        OnClick = MainFrm_MainMenu_File_CloseClick
      end
      object MainFrm_MainMenu_File_N1: TMenuItem
        Caption = '-'
      end
      object MainFrm_MainMenu_File_Info: TMenuItem
        Caption = 'Properties...'
        OnClick = MainFrm_MainMenu_File_InfoClick
      end
      object MainFrm_MainMenu_File_N2: TMenuItem
        Caption = '-'
      end
      object MainFrm_MainMenu_File_Move: TMenuItem
        Caption = 'Move...'
        OnClick = MainFrm_MainMenu_File_MoveClick
      end
      object MainFrm_MainMenu_File_Copy: TMenuItem
        Caption = 'Copy...'
        OnClick = MainFrm_MainMenu_File_CopyClick
      end
      object MainFrm_MainMenu_File_Rename: TMenuItem
        Caption = 'Rename...'
        OnClick = MainFrm_MainMenu_File_RenameClick
      end
      object MainFrm_MainMenu_File_Delete: TMenuItem
        Caption = 'Delete'
        OnClick = MainFrm_MainMenu_File_DeleteClick
      end
      object MainFrm_MainMenu_File_N3: TMenuItem
        Caption = '-'
      end
      object MainFrm_MainMenu_File_Exit: TMenuItem
        Caption = 'Exit'
        ShortCut = 32883
        OnClick = MainFrm_MainMenu_File_ExitClick
      end
    end
    object MainFrm_MainMenu_View: TMenuItem
      Caption = 'View'
      object MainFrm_MainMenu_View_ToolBar: TMenuItem
        Caption = 'Toolbars'
        object MainFrm_MainMenu_View_ToolBar_Btns: TMenuItem
          Caption = 'Buttons...'
          OnClick = MainFrm_MainMenu_View_ToolBar_BtnsClick
        end
        object MainFrm_MainMenu_View_ToolBar_AddressBar: TMenuItem
          Caption = 'Addressbar'
          Checked = True
          OnClick = MainFrm_MainMenu_View_ToolBar_AddressBarClick
        end
      end
      object MainFrm_MainMenu_View_StatusBar: TMenuItem
        Caption = 'Statusbar'
        Checked = True
        OnClick = MainFrm_MainMenu_View_StatusBarClick
      end
      object MainFrm_MainMenu_View_N1: TMenuItem
        Caption = '-'
      end
      object MainFrm_MainMenu_View_Icons: TMenuItem
        Caption = 'Large Icons'
        OnClick = MainFrm_MainMenu_View_IconsClick
      end
      object MainFrm_MainMenu_View_SmallIcons: TMenuItem
        Caption = 'Small Icons'
        OnClick = MainFrm_MainMenu_View_SmallIconsClick
      end
      object MainFrm_MainMenu_View_List: TMenuItem
        Caption = 'List'
        OnClick = MainFrm_MainMenu_View_ListClick
      end
      object MainFrm_MainMenu_View_Report: TMenuItem
        Caption = 'Details'
        Checked = True
        OnClick = MainFrm_MainMenu_View_ReportClick
      end
      object MainFrm_MainMenu_View_N2: TMenuItem
        Caption = '-'
      end
      object MainFrm_MainMenu_View_RowSelect: TMenuItem
        Caption = 'Row Select'
        OnClick = MainFrm_MainMenu_View_RowSelectClick
      end
      object MainFrm_MainMenu_View_GridLines: TMenuItem
        Caption = 'Grid Lines'
        OnClick = MainFrm_MainMenu_View_GridLinesClick
      end
      object MainFrm_MainMenu_View_ListMode: TMenuItem
        Caption = 'List Mode'
        ShortCut = 32845
        OnClick = MainFrm_MainMenu_View_ListModeClick
      end
      object MainFrm_MainMenu_View_N3: TMenuItem
        Caption = '-'
      end
      object MainFrm_MainMenu_View_OrderBy: TMenuItem
        Caption = 'Order By'
        object MainFrm_MainMenu_View_OrderBy_Name: TMenuItem
          Caption = 'Name'
          Checked = True
          OnClick = MainFrm_MainMenu_View_OrderBy_NameClick
        end
        object MainFrm_MainMenu_View_OrderBy_Size: TMenuItem
          Caption = 'Size'
          OnClick = MainFrm_MainMenu_View_OrderBy_SizeClick
        end
        object MainFrm_MainMenu_View_OrderBy_Packed: TMenuItem
          Caption = 'Packed'
          OnClick = MainFrm_MainMenu_View_OrderBy_PackedClick
        end
        object MainFrm_MainMenu_View_OrderBy_Ratio: TMenuItem
          Caption = 'Ratio'
          OnClick = MainFrm_MainMenu_View_OrderBy_RatioClick
        end
        object MainFrm_MainMenu_View_OrderBy_Type: TMenuItem
          Caption = 'Type'
          OnClick = MainFrm_MainMenu_View_OrderBy_TypeClick
        end
        object MainFrm_MainMenu_View_OrderBy_Modified: TMenuItem
          Caption = 'Modified'
          OnClick = MainFrm_MainMenu_View_OrderBy_ModifiedClick
        end
        object MainFrm_MainMenu_View_OrderBy_Attributes: TMenuItem
          Caption = 'Attributes'
          OnClick = MainFrm_MainMenu_View_OrderBy_AttributesClick
        end
        object MainFrm_MainMenu_View_OrderBy_Method: TMenuItem
          Caption = 'Method'
          OnClick = MainFrm_MainMenu_View_OrderBy_MethodClick
        end
        object MainFrm_MainMenu_View_OrderBy_Password: TMenuItem
          Caption = 'Password'
          OnClick = MainFrm_MainMenu_View_OrderBy_PasswordClick
        end
        object MainFrm_MainMenu_View_OrderBy_CRC: TMenuItem
          Caption = 'CRC'
          OnClick = MainFrm_MainMenu_View_OrderBy_CRCClick
        end
        object MainFrm_MainMenu_View_OrderBy_Path: TMenuItem
          Caption = 'Path'
          OnClick = MainFrm_MainMenu_View_OrderBy_PathClick
        end
        object MainFrm_MainMenu_View_OrderBy_Position: TMenuItem
          Caption = 'Position'
          OnClick = MainFrm_MainMenu_View_Details_PositionClick
        end
      end
      object MainFrm_MainMenu_View_Details: TMenuItem
        Caption = 'Details'
        object MainFrm_MainMenu_View_Details_Name: TMenuItem
          Caption = 'Name'
          Checked = True
          Enabled = False
          OnClick = MainFrm_MainMenu_View_Details_NameClick
        end
        object MainFrm_MainMenu_View_Details_Size: TMenuItem
          Caption = 'Size'
          Checked = True
          OnClick = MainFrm_MainMenu_View_Details_SizeClick
        end
        object MainFrm_MainMenu_View_Details_Packed: TMenuItem
          Caption = 'Packed'
          Checked = True
          OnClick = MainFrm_MainMenu_View_Details_PackedClick
        end
        object MainFrm_MainMenu_View_Details_Ratio: TMenuItem
          Caption = 'Ratio'
          Checked = True
          OnClick = MainFrm_MainMenu_View_Details_RatioClick
        end
        object MainFrm_MainMenu_View_Details_Type: TMenuItem
          Caption = 'Type'
          Checked = True
          OnClick = MainFrm_MainMenu_View_Details_TypeClick
        end
        object MainFrm_MainMenu_View_Details_Modified: TMenuItem
          Caption = 'Modified'
          Checked = True
          OnClick = MainFrm_MainMenu_View_Details_ModifiedClick
        end
        object MainFrm_MainMenu_View_Details_Attributes: TMenuItem
          Caption = 'Attributes'
          Checked = True
          OnClick = MainFrm_MainMenu_View_Details_AttributesClick
        end
        object MainFrm_MainMenu_View_Details_Method: TMenuItem
          Caption = 'Method'
          Checked = True
          OnClick = MainFrm_MainMenu_View_Details_MethodClick
        end
        object MainFrm_MainMenu_View_Details_Password: TMenuItem
          Caption = 'Password'
          Checked = True
          OnClick = MainFrm_MainMenu_View_Details_PasswordClick
        end
        object MainFrm_MainMenu_View_Details_CRC: TMenuItem
          Caption = 'CRC'
          OnClick = MainFrm_MainMenu_View_Details_CRCClick
        end
        object MainFrm_MainMenu_View_Details_Path: TMenuItem
          Caption = 'Path'
          OnClick = MainFrm_MainMenu_View_Details_PathClick
        end
        object MainFrm_MainMenu_View_Details_Position: TMenuItem
          Caption = 'Position'
          OnClick = MainFrm_MainMenu_View_Details_PositionClick
        end
      end
    end
    object MainFrm_MainMenu_Actions: TMenuItem
      Caption = 'Actions'
      object MainFrm_MainMenu_Actions_Add: TMenuItem
        Caption = 'Add Files and/or Directories...'
        ShortCut = 8257
        OnClick = MainFrm_MainMenu_Actions_AddClick
      end
      object MainFrm_MainMenu_Actions_Delete: TMenuItem
        Caption = 'Delete Selected Files'
        ShortCut = 8260
        OnClick = MainFrm_MainMenu_Actions_DeleteClick
      end
      object MainFrm_MainMenu_Actions_Extract: TMenuItem
        Caption = 'Extract To...'
        ShortCut = 8261
        OnClick = MainFrm_MainMenu_Actions_ExtractClick
      end
      object MainFrm_MainMenu_Actions_ExtractAll: TMenuItem
        Caption = 'Extract All To...'
        OnClick = MainFrm_MainMenu_Actions_ExtractAllClick
      end
      object MainFrm_MainMenu_Actions_Test: TMenuItem
        Caption = 'Test Files'
        ShortCut = 8276
        OnClick = MainFrm_MainMenu_Actions_TestClick
      end
      object MainFrm_MainMenu_Actions_Rename: TMenuItem
        Caption = 'Rename Files...'
        ShortCut = 8274
        OnClick = MainFrm_MainMenu_Actions_RenameClick
      end
      object MainFrm_MainMenu_Actions_View: TMenuItem
        Caption = 'View File...'
        ShortCut = 8278
        OnClick = MainFrm_MainMenu_Actions_ViewClick
      end
      object MainFrm_MainMenu_Actions_N1: TMenuItem
        Caption = '-'
      end
      object MainFrm_MainMenu_Actions_SelectAll: TMenuItem
        Caption = 'Select All'
        ShortCut = 32833
        OnClick = MainFrm_MainMenu_Actions_SelectAllClick
      end
      object MainFrm_MainMenu_Actions_SelectMask: TMenuItem
        Caption = 'Select File Group...'
        OnClick = MainFrm_MainMenu_Actions_SelectMaskClick
      end
      object MainFrm_MainMenu_Actions_UnselectMask: TMenuItem
        Caption = 'Unselect File Group...'
        OnClick = MainFrm_MainMenu_Actions_UnselectMaskClick
      end
      object MainFrm_MainMenu_Actions_Invert: TMenuItem
        Caption = 'Invert Selection'
        ShortCut = 32841
        OnClick = MainFrm_MainMenu_Actions_InvertClick
      end
      object MainFrm_MainMenu_Actions_N2: TMenuItem
        Caption = '-'
      end
      object MainFrm_MainMenu_Actions_CheckOut: TMenuItem
        Caption = 'CheckOut'
        ShortCut = 8271
        OnClick = MainFrm_MainMenu_Actions_CheckOutClick
      end
      object MainFrm_MainMenu_Actions_TestAll: TMenuItem
        Caption = 'Test Archive'
        OnClick = MainFrm_MainMenu_Actions_TestAllClick
      end
    end
    object MainFrm_MainMenu_Options: TMenuItem
      Caption = 'Options'
      object MainFrm_MainMenu_Options_Config: TMenuItem
        Caption = 'Configuration...'
        ShortCut = 32835
        OnClick = MainFrm_MainMenu_Options_ConfigClick
      end
      object MainFrm_MainMenu_Options_Password: TMenuItem
        Caption = 'Set Password...'
        OnClick = MainFrm_MainMenu_Options_PasswordClick
      end
      object MainFrm_MainMenu_Options_N1: TMenuItem
        Caption = '-'
      end
      object MainFrm_MainMenu_Options_SaveOnExit: TMenuItem
        Caption = 'Save Setting on Exit'
        Checked = True
        OnClick = MainFrm_MainMenu_Options_SaveOnExitClick
      end
      object MainFrm_MainMenu_Options_SaveNow: TMenuItem
        Caption = 'Save Setting Now'
        OnClick = MainFrm_MainMenu_Options_SaveNowClick
      end
      object MainFrm_MainMenu_Options_Default: TMenuItem
        Caption = 'Default Setting'
        OnClick = MainFrm_MainMenu_Options_DefaultClick
      end
      object MainFrm_MainMenu_Options_N2: TMenuItem
        Caption = '-'
      end
      object MainFrm_MainMenu_Options_LogReport: TMenuItem
        Caption = 'Create Log Report'
        OnClick = MainFrm_MainMenu_Options_LogReportClick
      end
    end
    object MainFrm_MainMenu_Help: TMenuItem
      Caption = 'Help'
      object MainFrm_MainMenu_Help_F1: TMenuItem
        Caption = 'Help Contents and Index'
        ShortCut = 112
        OnClick = MainFrm_MainMenu_Help_F1Click
      end
      object MainFrm_MainMenu_Help_N1: TMenuItem
        Caption = '-'
      end
      object MainFrm_MainMenu_Help_Internet: TMenuItem
        Caption = 'BeeGui Home Page'
        ShortCut = 32840
        OnClick = MainFrm_MainMenu_Help_InternetClick
      end
      object MainFrm_MainMenu_Help_N2: TMenuItem
        Caption = '-'
      end
      object MainFrm_MainMenu_Help_License: TMenuItem
        Caption = 'License Agreement'
        ShortCut = 32844
        OnClick = MainFrm_MainMenu_Help_LicenseClick
      end
      object MainFrm_MainMenu_Help_About: TMenuItem
        Caption = 'About BeeGui...'
        OnClick = MainFrm_MainMenu_Help_AboutClick
      end
    end
  end
  object MainFrm_SaveDialog: TSaveDialog
    Filter = 
      'Bee file  (*.bee)|*.bee|Zip file (*.zip)|*.zip|Tar file (*.tar)|' +
      '*.tar|Exe file (*.exe)|*.exe|Any file (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 40
    Top = 168
  end
  object MainFrm_PopupMenu_Buttons: TPopupMenu
    Left = 72
    Top = 136
    object MainFrm_PopupMenu_Buttons_New: TMenuItem
      Caption = 'New'
      Checked = True
      OnClick = MainFrm_PopupMenu_Buttons_NewClick
    end
    object MainFrm_PopupMenu_Buttons_Open: TMenuItem
      Caption = 'Open'
      Checked = True
      OnClick = MainFrm_PopupMenu_Buttons_OpenClick
    end
    object MainFrm_PopupMenu_Buttons_N1: TMenuItem
      Caption = 'Spacer'
      Checked = True
      OnClick = MainFrm_PopupMenu_Buttons_N1Click
    end
    object MainFrm_PopupMenu_Buttons_Add: TMenuItem
      Caption = 'Add'
      Checked = True
      OnClick = MainFrm_PopupMenu_Buttons_AddClick
    end
    object MainFrm_PopupMenu_Buttons_Extract: TMenuItem
      Caption = 'Extract'
      Checked = True
      OnClick = MainFrm_PopupMenu_Buttons_ExtractClick
    end
    object MainFrm_PopupMenu_Buttons_View: TMenuItem
      Caption = 'View'
      Checked = True
      OnClick = MainFrm_PopupMenu_Buttons_ViewClick
    end
    object MainFrm_PopupMenu_Buttons_Delete: TMenuItem
      Caption = 'Delete'
      Checked = True
      OnClick = MainFrm_PopupMenu_Buttons_DeleteClick
    end
    object MainFrm_PopupMenu_Buttons_N2: TMenuItem
      Caption = 'Spacer'
      Checked = True
      OnClick = MainFrm_PopupMenu_Buttons_N2Click
    end
    object MainFrm_PopupMenu_Buttons_Test: TMenuItem
      Caption = 'Test'
      Checked = True
      OnClick = MainFrm_PopupMenu_Buttons_TestClick
    end
    object MainFrm_PopupMenu_Buttons_Info: TMenuItem
      Caption = 'Info'
      Checked = True
      OnClick = MainFrm_PopupMenu_Buttons_InfoClick
    end
    object MainFrm_PopupMenu_Buttons_CheckOut: TMenuItem
      Caption = 'CheckOut'
      OnClick = MainFrm_PopupMenu_Buttons_CheckOutClick
    end
    object MainFrm_PopupMenu_Buttons_N3: TMenuItem
      Caption = 'Spacer'
      OnClick = MainFrm_PopupMenu_Buttons_N3Click
    end
    object MainFrm_PopupMenu_Buttons_Help: TMenuItem
      Caption = 'Help'
      OnClick = MainFrm_PopupMenu_Buttons_HelpClick
    end
    object MainFrm_PopupMenu_Buttons_Exit: TMenuItem
      Caption = 'Exit'
      OnClick = MainFrm_PopupMenu_Buttons_ExitClick
    end
  end
end
