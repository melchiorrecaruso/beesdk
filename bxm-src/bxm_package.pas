{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bxm_package;

interface

uses
  bxm_AddTreeViewMgr, bxm_FolderTreeViewMgr, bxm_IconList, bxm_Process, 
  bxm_AppViewer, bxm_ArchiveFolderBox, bxm_ArchiveListViewMgr, bxm_Consts, 
  bxm_Messages, bxm_SysUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('bxm_AddTreeViewMgr', @bxm_AddTreeViewMgr.Register);
  RegisterUnit('bxm_FolderTreeViewMgr', @bxm_FolderTreeViewMgr.Register);
  RegisterUnit('bxm_IconList', @bxm_IconList.Register);
  RegisterUnit('bxm_Process', @bxm_Process.Register);
  RegisterUnit('bxm_ArchiveFolderBox', @bxm_ArchiveFolderBox.Register);
  RegisterUnit('bxm_ArchiveListViewMgr', @bxm_ArchiveListViewMgr.Register);
end;

initialization
  RegisterPackage('bxm_package', @Register);
end.
