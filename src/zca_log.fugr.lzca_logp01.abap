*&---------------------------------------------------------------------*
*&  Include           LZCA_LOGP01
*&---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*     CLASS  lcl_ucomm_handler  IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_ucomm_handler IMPLEMENTATION.

  METHOD display_errpos_in_source_code.
    "-----------------------------------------------------------------*
    "   Display error position in source code
    "-----------------------------------------------------------------*
    "Determine log number to be able to find all necessary values for source position display
    cl_bal_db_search=>get_header_by_key(
                                  EXPORTING
                                    i_client      = sy-mandt
                                    it_log_handle = VALUE #( ( is_uc_data-list_msgh-log_handle ) )
                                  IMPORTING
                                    et_log_header = DATA(lt_balhdr) ).

    "Get source position from sorted buffer table. Source position values are already
    "determined before call of display dialog and passed into this function group.
    DATA(ls_balhdr) = VALUE #( lt_balhdr[ 1 ] OPTIONAL ).
    DATA(ls_srcpos) = VALUE #( gt_srcpos[ lognr     = ls_balhdr-lognumber
                                          msgnumber = is_uc_data-list_msgh-msgnumber ] OPTIONAL ).

    IF ls_srcpos IS INITIAL.
      SELECT SINGLE * INTO  ls_srcpos
                      FROM  zca_log_srcpos
                      WHERE lognr     EQ ls_balhdr-lognumber
                        AND msgnumber EQ is_uc_data-list_msgh-msgnumber.
      IF sy-subrc NE 0.
        SELECT SINGLE * INTO  ls_srcpos
                        FROM  zca_log_srcpos
                        WHERE lognr EQ ls_balhdr-lognumber. "#EC CI_NOORDER
      ENDIF.

      IF ls_srcpos IS NOT INITIAL.
        "Set source position data into buffer
        INSERT ls_srcpos INTO TABLE gt_srcpos.
      ENDIF.
    ENDIF.

    IF ls_srcpos IS INITIAL.
      "Insufficient data to display source position
      RAISE EXCEPTION TYPE zcx_ca_log
        EXPORTING
          textid = zcx_ca_log=>insufficient_data_for_src_pos.
    ENDIF.

    "Display source code - needs only the technical informations
    /iwfnd/cl_sutil_moni=>get_instance( )->show_source( iv_program    = ls_srcpos-prog
                                                        iv_include    = ls_srcpos-incl
                                                        iv_line       = ls_srcpos-line
                                                        iv_new_window = abap_false ).
  ENDMETHOD.                    "display_errpos_in_source_code

ENDCLASS.                    " lcl_ucomm_handler  IMPLEMENTATION
