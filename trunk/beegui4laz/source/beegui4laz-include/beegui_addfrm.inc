SessionProperties :=

   'Top;'
 + 'Left;'
 + 'Width;'
 + 'Height;'
 + 'WindowState;';
 
 {$IFDEF WINDOWS}
   AddFrm_FilesTree.BorderWidth := 0;
 {$ELSE}
   AddFrm_FilesTree.BorderWidth := 2;
 {$ENDIF}
