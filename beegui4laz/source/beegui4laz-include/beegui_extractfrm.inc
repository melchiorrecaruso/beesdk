SessionProperties :=

   'Top;'
 + 'Left;'
 + 'Width;'
 + 'Height;'
 + 'WindowState;'
 
 + 'ExtractFrm_FolderEdit.Text';
 
{$IFDEF WINDOWS}
  ExtractFrm_Tree.BorderWidth := 0;
{$ELSE}
  ExtractFrm_Tree.BorderWidth := 2;
{$ENDIF}
