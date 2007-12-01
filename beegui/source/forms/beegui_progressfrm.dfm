object ProgressFrm: TProgressFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Progress'
  ClientHeight = 103
  ClientWidth = 389
  Color = clBtnFace
  Constraints.MaxHeight = 130
  Constraints.MinHeight = 130
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    389
    103)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    AlignWithMargins = True
    Left = 3
    Top = 61
    Width = 383
    Height = 2
    Margins.Bottom = 40
    Align = alBottom
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 332
  end
  object ProgressFrm_Msg: TLabel
    AlignWithMargins = True
    Left = 5
    Top = 10
    Width = 379
    Height = 15
    Margins.Left = 5
    Margins.Top = 10
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    ExplicitWidth = 3
    ExplicitHeight = 13
  end
  object BtnPause: TBitBtn
    AlignWithMargins = True
    Left = 226
    Top = 73
    Width = 75
    Height = 23
    Margins.Top = 5
    Anchors = [akRight, akBottom]
    Caption = 'Pause'
    Constraints.MaxHeight = 23
    Constraints.MaxWidth = 75
    Constraints.MinHeight = 23
    Constraints.MinWidth = 75
    TabOrder = 3
    OnClick = BtnPauseClick
  end
  object BtnCancel: TBitBtn
    AlignWithMargins = True
    Left = 307
    Top = 73
    Width = 75
    Height = 23
    Margins.Top = 5
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    Constraints.MaxHeight = 23
    Constraints.MaxWidth = 75
    Constraints.MinHeight = 23
    Constraints.MinWidth = 75
    ModalResult = 1
    TabOrder = 1
    OnClick = BtnCancelClick
  end
  object BtnBackground: TBitBtn
    AlignWithMargins = True
    Left = 5
    Top = 73
    Width = 75
    Height = 23
    Margins.Top = 5
    Anchors = [akLeft, akBottom]
    Caption = 'Background'
    Constraints.MaxHeight = 23
    Constraints.MaxWidth = 75
    Constraints.MinHeight = 23
    Constraints.MinWidth = 75
    TabOrder = 2
    OnClick = BtnBackgroundClick
  end
  object BtnForeGround: TBitBtn
    AlignWithMargins = True
    Left = 5
    Top = 73
    Width = 75
    Height = 23
    Margins.Top = 5
    Anchors = [akLeft, akBottom]
    Caption = 'Foreground'
    Constraints.MaxHeight = 23
    Constraints.MaxWidth = 75
    Constraints.MinHeight = 23
    Constraints.MinWidth = 75
    TabOrder = 4
    Visible = False
    OnClick = BtnForeGroundClick
  end
  object BtnContinue: TBitBtn
    AlignWithMargins = True
    Left = 226
    Top = 73
    Width = 75
    Height = 23
    Margins.Top = 5
    Anchors = [akRight, akBottom]
    Caption = 'Continue'
    Constraints.MaxHeight = 23
    Constraints.MaxWidth = 75
    Constraints.MinHeight = 23
    Constraints.MinWidth = 75
    TabOrder = 0
    Visible = False
    OnClick = BtnContinueClick
  end
  object ProgressFrm_Messages: TComboBox
    Left = 86
    Top = 74
    Width = 70
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    Color = clInactiveBorder
    Enabled = False
    ItemHeight = 13
    TabOrder = 5
    Visible = False
  end
  object ProgressFrm_ProgressBar: TProgressBar
    AlignWithMargins = True
    Left = 5
    Top = 35
    Width = 379
    Height = 18
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    TabOrder = 6
  end
end
