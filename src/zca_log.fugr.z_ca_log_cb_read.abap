FUNCTION z_ca_log_cb_read.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_S_INFO) TYPE  BAL_S_CBRD
*"  CHANGING
*"     REFERENCE(C_DISPLAY_DATA) TYPE  BAL_S_SHOW
*"     REFERENCE(C_CONTEXT_HEADER) TYPE  BAL_S_CONT
*"     REFERENCE(C_CONTEXT_MESSAGE) TYPE  BAL_S_CONT
*"     REFERENCE(C_FIELD) TYPE  ANY
*"----------------------------------------------------------------------
  "Local data definitions
  DATA:
    ls_srcpos        TYPE zca_log_srcpos.

  IF i_s_info-is_message EQ abap_false.
    RETURN.
  ENDIF.

  IF c_display_data-lognumber CA '$' ##no_text.
    CLEAR ls_srcpos.
  ELSE.
    ls_srcpos-lognr = c_display_data-lognumber.
  ENDIF.

  "Get source position from sorted buffer table
  READ TABLE gt_srcpos INTO ls_srcpos
                       WITH KEY lognr     = ls_srcpos-lognr
                                msgnumber = c_display_data-msgnumber
                                            BINARY SEARCH.
  IF sy-subrc EQ 0.
    CASE i_s_info-ref_field.
      WHEN zif_ca_c_log_techn=>c_fname_class.
        c_field = ls_srcpos-class.

      WHEN zif_ca_c_log_techn=>c_fname_method.
        c_field = ls_srcpos-meth.

      WHEN zif_ca_c_log_techn=>c_fname_line.
        c_field = ls_srcpos-line.
    ENDCASE.
  ENDIF.
ENDFUNCTION.
