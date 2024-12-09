FUNCTION z_ca_log_cb_ucom.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(C_S_USER_COMMAND_DATA) TYPE  BAL_S_CBUC
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  TRY.
      CASE c_s_user_command_data-list_table.
        WHEN zif_ca_c_log_techn=>c_fname_s_excep_srcpos.
          "Structure of error position. Checking selected field is not necessary,
          "because this structure contain only relevant fields to display.
          lcl_ucomm_handler=>display_errpos_in_source_code( c_s_user_command_data ).
      ENDCASE.

    CATCH zcx_ca_error INTO DATA(lx_error).
      MESSAGE lx_error TYPE zcl_ca_log=>c_msgty_e.
  ENDTRY.
ENDFUNCTION.
