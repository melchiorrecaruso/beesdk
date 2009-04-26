object InfoFrm: TInfoFrm
  Left = 316
  Top = 193
  AutoScroll = False
  Caption = 'Archive properties'
  ClientHeight = 447
  ClientWidth = 359
  Color = clBtnFace
  Constraints.MinHeight = 474
  Constraints.MinWidth = 367
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object InfoFrm_Pages: TPageControl
    Left = 5
    Top = 6
    Width = 349
    Height = 406
    ActivePage = InfoFrm_PageGeneral
    Anchors = [akLeft, akTop, akRight, akBottom]
    ParentShowHint = False
    ShowHint = False
    TabOrder = 1
    object InfoFrm_PageGeneral: TTabSheet
      Caption = 'General'
      ParentShowHint = False
      ShowHint = False
      object InfoFrm_ArchiveName_: TLabel
        Left = 85
        Top = 32
        Width = 135
        Height = 13
        AutoSize = False
        Caption = 'Archive Name'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Version_: TLabel
        Left = 85
        Top = 48
        Width = 135
        Height = 13
        AutoSize = False
        Caption = 'Version'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Files_: TLabel
        Left = 85
        Top = 80
        Width = 135
        Height = 13
        AutoSize = False
        Caption = 'Files'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Packed_: TLabel
        Left = 85
        Top = 112
        Width = 135
        Height = 13
        AutoSize = False
        Caption = 'Files size packed'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Packed: TLabel
        Left = 220
        Top = 112
        Width = 107
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'ERROR'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Ratio_: TLabel
        Left = 85
        Top = 128
        Width = 135
        Height = 13
        AutoSize = False
        Caption = 'Ratio'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Ratio: TLabel
        Left = 220
        Top = 128
        Width = 107
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'ERROR'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Size_: TLabel
        Left = 85
        Top = 96
        Width = 135
        Height = 13
        AutoSize = False
        Caption = 'Files size'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Size: TLabel
        Left = 220
        Top = 96
        Width = 107
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'ERROR'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Files: TLabel
        Left = 220
        Top = 80
        Width = 107
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'ERROR'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Modified_: TLabel
        Left = 85
        Top = 192
        Width = 135
        Height = 13
        AutoSize = False
        Caption = 'Modified'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Modified: TLabel
        Left = 220
        Top = 192
        Width = 107
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'ERROR'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_UpEmpty: TImage
        Left = 14
        Top = 16
        Width = 33
        Height = 11
        AutoSize = True
        ParentShowHint = False
        Picture.Data = {
          07544269746D617082040000424D820400000000000036000000280000002100
          00000B00000001001800000000004C040000C40E0000C40E0000000000000000
          0000C0C0C0FFFFFFE06868E06868E06868E06868E06868E06868E06868E06868
          E06868E06868E06868E06868E06868FFFFFFFFFFFFFFFFFFFF8080FF8080FF80
          80FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF
          FFFFC0C0C000C0C0C0FFFFFFE06868E06868E06868E06868E06868E06868E068
          68E06868E06868E06868FFFFFFFFFFFFFFFFFFFF8080FF8080FF8080FFFFFFFF
          FFFFFFFFFFFF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080
          FF8080FFFFFFC0C0C000C0C0C0FFFFFFE06868E06868E06868E06868E06868E0
          6868E06868FFFFFFFFFFFFFFFFFFFF8080FF8080FF8080FF8080FF8080FF8080
          FF8080FF8080FF8080FFFFFFFFFFFFFFFFFFFF8080FF8080FF8080FF8080FF80
          80FF8080FF8080FFFFFFC0C0C000C0C0C0FFFFFFE06868E06868E06868E06868
          FFFFFFFFFFFFFFFFFFFF8080FF8080FF8080FF8080FF8080FF8080FF8080FF80
          80FF8080FF8080FF8080FF8080FF8080FF8080FF8080FFFFFFFFFFFFFFFFFFFF
          8080FF8080FF8080FF8080FFFFFFC0C0C000C0C0C0FFFFFFE06868FFFFFFFFFF
          FFFFFFFFFF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF
          8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080
          FF8080FFFFFFFFFFFFFFFFFFFF8080FFFFFFC0C0C000C0C0C0FFFFFFFFFFFFFF
          8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080
          FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF80
          80FF8080FF8080FF8080FF8080FF8080FFFFFFFFFFFFC0C0C000C0C0C0C0C0C0
          C0C0C0FFFFFFFFFFFFFFFFFFFF8080FF8080FF8080FF8080FF8080FF8080FF80
          80FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF
          8080FF8080FF8080FF8080FFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C000C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFF8080FF8080FF
          8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080
          FF8080FF8080FFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0FFFFFF
          FFFFFFFFFFFFFF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF80
          80FFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFFF8080FF8080FF8080FFFFFFFF
          FFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFF
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C000}
        ShowHint = False
        Transparent = True
      end
      object InfoFrm_DownFull: TImage
        Left = 14
        Top = 323
        Width = 33
        Height = 11
        Anchors = [akLeft, akBottom]
        AutoSize = True
        ParentShowHint = False
        Picture.Data = {
          07544269746D617082040000424D820400000000000036000000280000002100
          00000B00000001001800000000004C040000C40E0000C40E0000000000000000
          0000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFF7A408CFFFFFF7A408CFFFFFFFF
          FFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0FFFFFFFFFFFFFFFFFF7A408C7A408C7A408C7A408CFFFFFF7A408C
          7A408C7A408C7A408CFFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          FFFFFFFFFFFFFFFFFF7A408C7A408C7A408C7A408C7A408C7A408C7A408CFFFF
          FF7A408C7A408C7A408C7A408C7A408C7A408C7A408CFFFFFFFFFFFFFFFFFFC0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000C0C0C0C0C0C0C0C0C0FFFFFFFFFF
          FFFFFFFF7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A
          408CFFFFFF7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C
          7A408CFFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C000C0C0C0FFFFFFFFFFFF7A
          408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C
          7A408C7A408CFFFFFF7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A40
          8C7A408C7A408C7A408C7A408C7A408CFFFFFFFFFFFFC0C0C000C0C0C0FFFFFF
          823C9664408C64408C64408C7A408C7A408C7A408C7A408C7A408C7A408C7A40
          8C7A408C7A408C7A408CFFFFFF7A408C7A408C7A408C7A408C7A408C7A408C7A
          408C7A408C7A408C7A408C64408C64408C64408C8C42C0FFFFFFC0C0C000C0C0
          C0FFFFFF823C96823C96823C96823C9664408C64408C64408C7A408C7A408C7A
          408C7A408C7A408C7A408C7A408CFFFFFF7A408C7A408C7A408C7A408C7A408C
          7A408C7A408C64408C64408C64408C8C42C08C42C08C42C08C42C0FFFFFFC0C0
          C000C0C0C0FFFFFF823C96823C96823C96823C96823C96823C96823C9664408C
          64408C64408C7A408C7A408C7A408C7A408CFFFFFF7A408C7A408C7A408C7A40
          8C64408C64408C64408C8C42C08C42C08C42C08C42C08C42C08C42C08C42C0FF
          FFFFC0C0C000C0C0C0FFFFFF823C96823C96823C96823C96823C96823C96823C
          96823C96823C96823C9664408C64408C64408C7A408CFFFFFF7A408C64408C64
          408C64408C8C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C0
          8C42C0FFFFFFC0C0C000C0C0C0FFFFFF823C96823C96823C96823C96823C9682
          3C96823C96823C96823C96823C96823C96823C96823C9664408CFFFFFF64408C
          8C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42
          C08C42C08C42C0FFFFFFC0C0C000}
        ShowHint = False
        Transparent = True
      end
      object InfoFrm_Level: TImage
        Left = 14
        Top = 144
        Width = 33
        Height = 11
        AutoSize = True
        ParentShowHint = False
        Picture.Data = {
          07544269746D617082040000424D820400000000000036000000280000002100
          00000B00000001001800000000004C040000C40E0000C40E0000000000000000
          0000C0C0C0FFFFFF823C96823C96823C96823C96823C96823C96823C96823C96
          823C96823C96823C96823C96823C968060A0FFFFFF8060A08C42C08C42C08C42
          C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C0FF
          FFFFC0C0C000C0C0C0FFFFFF823C96823C96823C96823C96823C96823C96823C
          96823C96823C96823C968060A08060A08060A0A060A0FFFFFFA060A08060A080
          60A08060A08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C0
          8C42C0FFFFFFC0C0C000C0C0C0FFFFFF823C96823C96823C96823C96823C9682
          3C96823C968060A08060A08060A0A060A0A060A0A060A0A060A0FFFFFFA060A0
          A060A0A060A0A060A08060A08060A08060A08C42C08C42C08C42C08C42C08C42
          C08C42C08C42C0FFFFFFC0C0C000C0C0C0FFFFFF823C96823C96823C96823C96
          8060A08060A08060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0FFFF
          FFA060A0A060A0A060A0A060A0A060A0A060A0A060A08060A08060A08060A08C
          42C08C42C08C42C08C42C0FFFFFFC0C0C000C0C0C0FFFFFF823C968060A08060
          A08060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A0
          60A0FFFFFFA060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0
          A060A08060A08060A08060A08C42C0FFFFFFC0C0C000C0C0C0FFFFFF8060A0A0
          60A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0
          A060A0A060A0FFFFFFA060A0A060A0A060A0A060A0A060A0A060A0A060A0A060
          A0A060A0A060A0A060A0A060A0A060A08060A0FFFFFFC0C0C000C0C0C0FFFFFF
          E068688060A08060A08060A0A060A0A060A0A060A0A060A0A060A0A060A0A060
          A0A060A0A060A0A060A0FFFFFFA060A0A060A0A060A0A060A0A060A0A060A0A0
          60A0A060A0A060A0A060A08060A08060A08060A0FF8080FFFFFFC0C0C000C0C0
          C0FFFFFFE06868E06868E06868E068688060A08060A08060A0A060A0A060A0A0
          60A0A060A0A060A0A060A0A060A0FFFFFFA060A0A060A0A060A0A060A0A060A0
          A060A0A060A08060A08060A08060A0FF8080FF8080FF8080FF8080FFFFFFC0C0
          C000C0C0C0FFFFFFE06868E06868E06868E06868E06868E06868E068688060A0
          8060A08060A0A060A0A060A0A060A0A060A0FFFFFFA060A0A060A0A060A0A060
          A08060A08060A08060A0FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF
          FFFFC0C0C000C0C0C0FFFFFFE06868E06868E06868E06868E06868E06868E068
          68E06868E06868E068688060A08060A08060A0A060A0FFFFFFA060A08060A080
          60A08060A0FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080
          FF8080FFFFFFC0C0C000C0C0C0FFFFFFE06868E06868E06868E06868E06868E0
          6868E06868E06868E06868E06868E06868E06868E068688060A0FFFFFF8060A0
          FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF80
          80FF8080FF8080FFFFFFC0C0C000}
        ShowHint = False
        Transparent = True
      end
      object InfoFrm_Empty: TImage
        Left = 14
        Top = 64
        Width = 33
        Height = 65
        ParentShowHint = False
        Picture.Data = {
          07544269746D61709A000000424D9A0000000000000036000000280000002100
          000001000000010018000000000064000000C40E0000C40E0000000000000000
          0000C0C0C0FFFFFFE06868E06868E06868E06868E06868E06868E06868E06868
          E06868E06868E06868E06868E06868E06868FFFFFFFF8080FF8080FF8080FF80
          80FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF
          FFFFC0C0C000}
        ShowHint = False
        Stretch = True
        Transparent = True
      end
      object InfoFrm_Full: TImage
        Left = 14
        Top = 192
        Width = 33
        Height = 73
        ParentShowHint = False
        Picture.Data = {
          07544269746D61709A000000424D9A0000000000000036000000280000002100
          000001000000010018000000000064000000C40E0000C40E0000000000000000
          0000C0C0C0FFFFFF823C96823C96823C96823C96823C96823C96823C96823C96
          823C96823C96823C96823C96823C96823C96FFFFFF8C42C08C42C08C42C08C42
          C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C0FF
          FFFFC0C0C000}
        ShowHint = False
        Stretch = True
        Transparent = True
      end
      object InfoFrm_Bevel_1: TBevel
        Left = 85
        Top = 70
        Width = 242
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object InfoFrm_FilesCrypted_: TLabel
        Left = 85
        Top = 160
        Width = 135
        Height = 13
        AutoSize = False
        Caption = 'Files encrypted'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_FilesCrypted: TLabel
        Left = 220
        Top = 160
        Width = 107
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'ERROR'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Bevel2: TBevel
        Left = 85
        Top = 150
        Width = 242
        Height = 2
        Anchors = [akLeft, akTop, akRight]
      end
      object InfoFrm_R: TLabel
        Left = 50
        Top = 100
        Width = 27
        Height = 13
        BiDiMode = bdLeftToRight
        Caption = '41%'
        Color = clBtnFace
        Font.Charset = ANSI_CHARSET
        Font.Color = clPurple
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentBiDiMode = False
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Title: TLabel
        Left = 88
        Top = 8
        Width = 231
        Height = 15
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Archive'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Default'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_Version: TLabel
        Left = 220
        Top = 48
        Width = 107
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'ERROR'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_ArchiveName: TLabel
        Left = 220
        Top = 32
        Width = 107
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'ERROR'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_DownEmpty: TImage
        Left = 14
        Top = 339
        Width = 33
        Height = 16
        Anchors = [akLeft, akBottom]
        AutoSize = True
        ParentShowHint = False
        Picture.Data = {
          07544269746D617076060000424D760600000000000036000000280000002100
          000010000000010018000000000040060000C40E0000C40E0000000000000000
          0000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFF7A408CFFFFFF7A408CFFFFFFFF
          FFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0FFFFFFFFFFFFFFFFFF7A408C7A408C7A408C7A408CFFFFFF7A408C
          7A408C7A408C7A408CFFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          FFFFFFFFFFFFFFFFFF7A408C7A408C7A408C7A408C7A408C7A408C7A408CFFFF
          FF7A408C7A408C7A408C7A408C7A408C7A408C7A408CFFFFFFFFFFFFFFFFFFC0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000C0C0C0C0C0C0C0C0C0FFFFFFFFFF
          FFFFFFFF7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A
          408CFFFFFF7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C
          7A408CFFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C000C0C0C0FFFFFFFFFFFF7A
          408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C7A408C
          7A408C8060A0FFFFFF8060A07A408C7A408C7A408C7A408C7A408C7A408C7A40
          8C7A408C7A408C7A408C7A408C7A408CFFFFFFFFFFFFC0C0C000C0C0C0FFFFFF
          823C9664408C64408C64408C7A408C7A408C7A408C7A408C7A408C7A408C8060
          A08060A08060A0A060A0FFFFFFA060A08060A08060A08060A07A408C7A408C7A
          408C7A408C7A408C7A408C64408C64408C64408C8C42C0FFFFFFC0C0C000C0C0
          C0FFFFFF823C96823C96823C96823C9664408C64408C64408C8060A08060A080
          60A0A060A0A060A0A060A0A060A0FFFFFFA060A0A060A0A060A0A060A08060A0
          8060A08060A064408C64408C64408C8C42C08C42C08C42C08C42C0FFFFFFC0C0
          C000C0C0C0FFFFFF823C96823C96823C96823C968060A08060A08060A0A060A0
          A060A0A060A0A060A0A060A0A060A0A060A0FFFFFFA060A0A060A0A060A0A060
          A0A060A0A060A0A060A08060A08060A08060A08C42C08C42C08C42C08C42C0FF
          FFFFC0C0C000C0C0C0FFFFFF823C968060A08060A08060A0A060A0A060A0A060
          A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0FFFFFFA060A0A060A0A0
          60A0A060A0A060A0A060A0A060A0A060A0A060A0A060A08060A08060A08060A0
          8C42C0FFFFFFC0C0C000C0C0C0FFFFFF8060A0A060A0A060A0A060A0A060A0A0
          60A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0FFFFFFA060A0
          A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060
          A0A060A08060A0FFFFFFC0C0C000C0C0C0FFFFFFE068688060A08060A08060A0
          A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0FFFF
          FFA060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A080
          60A08060A08060A0FF8080FFFFFFC0C0C000C0C0C0FFFFFFE06868E06868E068
          68E068688060A08060A08060A0A060A0A060A0A060A0A060A0A060A0A060A0A0
          60A0FFFFFFA060A0A060A0A060A0A060A0A060A0A060A0A060A08060A08060A0
          8060A0FF8080FF8080FF8080FF8080FFFFFFC0C0C000C0C0C0FFFFFFE06868E0
          6868E06868E06868E06868E06868E068688060A08060A08060A0A060A0A060A0
          A060A0A060A0FFFFFFA060A0A060A0A060A0A060A08060A08060A08060A0FF80
          80FF8080FF8080FF8080FF8080FF8080FF8080FFFFFFC0C0C000C0C0C0FFFFFF
          E06868E06868E06868E06868E06868E06868E06868E06868E06868E068688060
          A08060A08060A0A060A0FFFFFFA060A08060A08060A08060A0FF8080FF8080FF
          8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FFFFFFC0C0C000C0C0
          C0FFFFFFE06868E06868E06868E06868E06868E06868E06868E06868E06868E0
          6868E06868E06868E068688060A0FFFFFF8060A0FF8080FF8080FF8080FF8080
          FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FF8080FFFFFFC0C0
          C000}
        ShowHint = False
        Transparent = True
      end
      object InfoFrm_UpFull: TImage
        Left = 14
        Top = 32
        Width = 33
        Height = 11
        AutoSize = True
        ParentShowHint = False
        Picture.Data = {
          07544269746D617082040000424D820400000000000036000000280000002100
          00000B00000001001800000000004C040000C40E0000C40E0000000000000000
          0000C0C0C0FFFFFF823C96823C96823C96823C96823C96823C96823C96823C96
          823C96823C96823C96823C96823C96FFFFFFFFFFFFFFFFFF8C42C08C42C08C42
          C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C0FF
          FFFFC0C0C000C0C0C0FFFFFF823C96823C96823C96823C96823C96823C96823C
          96823C96823C96823C96FFFFFFFFFFFFFFFFFFA060A0A060A0A060A0FFFFFFFF
          FFFFFFFFFF8C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C08C42C0
          8C42C0FFFFFFC0C0C000C0C0C0FFFFFF823C96823C96823C96823C96823C9682
          3C96823C96FFFFFFFFFFFFFFFFFFA060A0A060A0A060A0A060A0A060A0A060A0
          A060A0A060A0A060A0FFFFFFFFFFFFFFFFFF8C42C08C42C08C42C08C42C08C42
          C08C42C08C42C0FFFFFFC0C0C000C0C0C0FFFFFF823C96823C96823C96823C96
          FFFFFFFFFFFFFFFFFFA060A0A060A0A060A0A060A0A060A0A060A0A060A0A060
          A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0FFFFFFFFFFFFFFFFFF8C
          42C08C42C08C42C08C42C0FFFFFFC0C0C000C0C0C0FFFFFF823C96FFFFFFFFFF
          FFFFFFFFA060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A0
          60A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0
          A060A0FFFFFFFFFFFFFFFFFF8C42C0FFFFFFC0C0C000C0C0C0FFFFFFFFFFFFA0
          60A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0
          A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060
          A0A060A0A060A0A060A0A060A0A060A0FFFFFFFFFFFFC0C0C000C0C0C0C0C0C0
          C0C0C0FFFFFFFFFFFFFFFFFFA060A0A060A0A060A0A060A0A060A0A060A0A060
          A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A0
          60A0A060A0A060A0A060A0FFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C000C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFA060A0A060A0A0
          60A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0
          A060A0A060A0FFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0FFFFFF
          FFFFFFFFFFFFA060A0A060A0A060A0A060A0A060A0A060A0A060A0A060A0A060
          A0FFFFFFFFFFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFFA060A0A060A0A060A0FFFFFFFF
          FFFFFFFFFFC0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0FFFFFFFFFFFFFFFFFF
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C000}
        ShowHint = False
        Transparent = True
      end
      object InfoFrm_ArcSize_: TLabel
        Left = 85
        Top = 176
        Width = 135
        Height = 13
        AutoSize = False
        Caption = 'Archive size'
        ParentShowHint = False
        ShowHint = False
      end
      object InfoFrm_ArcSize: TLabel
        Left = 220
        Top = 176
        Width = 107
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'ERROR'
        ParentShowHint = False
        ShowHint = False
      end
    end
  end
  object BtnOk: TBitBtn
    Left = 279
    Top = 418
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    Spacing = 0
  end
end
