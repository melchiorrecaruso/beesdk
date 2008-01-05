{ Questo file è stato creato automaticamente da Lazarus. Da non modificare!
Questo sorgente viene usato solo per compilare ed installare il pacchetto.
 }

unit BeeCorePack; 

interface

uses
  BeeCore_IconList, BeeCore_AddTreeViewMgr, BeeCore_FolderTreeViewMgr, 
    Bee_Common, Bee_Interface, Bee_Assembler, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('BeeCore_IconList', @BeeCore_IconList.Register); 
  RegisterUnit('BeeCore_AddTreeViewMgr', @BeeCore_AddTreeViewMgr.Register); 
  RegisterUnit('BeeCore_FolderTreeViewMgr', @BeeCore_FolderTreeViewMgr.Register
    ); 
end; 

initialization
  RegisterPackage('BeeCorePack', @Register); 
end.
