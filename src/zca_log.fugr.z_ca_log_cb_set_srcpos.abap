FUNCTION z_ca_log_cb_set_srcpos.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_SRCPOS) TYPE  ZCA_TT_LOG_SRCPOS
*"----------------------------------------------------------------------
  gt_srcpos = it_srcpos.
  SORT gt_srcpos BY lognr msgnumber.
ENDFUNCTION.
