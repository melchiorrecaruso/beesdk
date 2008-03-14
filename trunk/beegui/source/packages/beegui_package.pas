{ Questo file è stato creato automaticamente da Lazarus. Da non modificare!
Questo sorgente viene usato solo per compilare ed installare il pacchetto.
 }

unit beegui_package; 

interface

uses
  BeeGui_AddTreeViewMgr, BeeGui_FolderTreeViewMgr, BeeGui_IconList, 
    BeeGui_ArchiveListViewMgr, BeeGui_ArchiveProcess, BeeGui_ArchiveFolderBox, 
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('BeeGui_AddTreeViewMgr', @BeeGui_AddTreeViewMgr.Register); 
  RegisterUnit('BeeGui_FolderTreeViewMgr', @BeeGui_FolderTreeViewMgr.Register); 
  RegisterUnit('BeeGui_IconList', @BeeGui_IconList.Register); 
  RegisterUnit('BeeGui_ArchiveListViewMgr', @BeeGui_ArchiveListViewMgr.Register
    ); 
  RegisterUnit('BeeGui_ArchiveProcess', @BeeGui_ArchiveProcess.Register); 
  RegisterUnit('BeeGui_ArchiveFolderBox', @BeeGui_ArchiveFolderBox.Register); 
end; 

initialization
  RegisterPackage('beegui_package', @Register); 
end.
