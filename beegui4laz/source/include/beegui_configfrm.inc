SessionProperties :=

   'Top;'
 + 'Left;'
 + 'Width;'
 + 'Height;'
 + 'WindowState;'

 + 'AddFrm_Method.ItemIndex;'
 + 'AddFrm_Dictionary.ItemIndex;'
 + 'ConfigFrm_Priority.ItemIndex;'
 + 'ExtractFrm_Overwrite.ItemIndex;'

 + 'AddFrm_kOption.Checked;'
 + 'AddFrm_rOption.Checked;'
 + 'AddFrm_sOption.Checked;'
 + 'AddFrm_tOption.Checked;'
 + 'ConfigFrm_mkOption.Checked;'
 + 'ExtractFrm_xOption.Checked;';
 
 {$IFDEF WINDOWS}
   ConfigFrm_Tree.BorderWidth := 0;
 {$ELSE}
   ConfigFrm_Tree.BorderWidth := 2;
 {$ENDIF}

