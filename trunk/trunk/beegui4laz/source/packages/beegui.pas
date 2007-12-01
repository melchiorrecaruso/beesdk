{ Questo file è stato creato automaticamente da Lazarus. Da non modificare!
Questo sorgente viene usato solo per compilare ed installare il pacchetto.
 }

unit beegui; 

interface

uses
  BeeGui_AddTreeView, BeeGui_ArchiveTreeView, BeeGui_FolderTreeView, 
    BeeGui_StatusProgressBar, BeeGui_IconList, BeeGui_AppViewer, 
    BeeGui_SysUtils, BeeGui_ArchiveListView, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('BeeGui_AddTreeView', @BeeGui_AddTreeView.Register); 
  RegisterUnit('BeeGui_ArchiveTreeView', @BeeGui_ArchiveTreeView.Register); 
  RegisterUnit('BeeGui_FolderTreeView', @BeeGui_FolderTreeView.Register); 
  RegisterUnit('BeeGui_StatusProgressBar', @BeeGui_StatusProgressBar.Register); 
  RegisterUnit('BeeGui_IconList', @BeeGui_IconList.Register); 
end; 

initialization
  RegisterPackage('beegui', @Register); 
end.
