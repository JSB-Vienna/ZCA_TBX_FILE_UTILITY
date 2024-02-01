"! <p class="shorttext synchronized" lang="en">CA-TBX: File utility for client/PC</p>
"!
"! <p>This class synergize / combine the two techniques of handling application server files and PC files
"! (= presentation server) in one class.</p>
"!
"! <p>If you want to use this class in a report with a selection screen, than please have a look into
"! <strong>class {@link zcl_ca_file_util_selscr_ctlr}</strong> to get more informations on how to use it.</p>
CLASS zcl_ca_file_handler_pc DEFINITION PUBLIC
                                        CREATE PROTECTED
                                        INHERITING FROM zcl_ca_file_handler
                                        GLOBAL FRIENDS zcl_ca_file_handler.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      zif_ca_file_handler~delete REDEFINITION,

      zif_ca_file_handler~read REDEFINITION,

      zif_ca_file_handler~write REDEFINITION.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      constructor
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Download a file to the PC/client</p>
      "!
      "! @parameter iv_file_mode              | <p class="shorttext synchronized" lang="en">Binary or character mode (CVC_FILE_HDLR-&gt;MODE-*)</p>
      "! @parameter iv_check_auth             | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
      "! @parameter iv_write_lf               | <p class="shorttext synchronized" lang="en">X = Add CR/LF at end of CHAR lines</p>
      "! @parameter iv_confirm_overwrite      | <p class="shorttext synchronized" lang="en">X = Confirm overwriting file</p>
      "! @parameter iv_write_field_separator  | <p class="shorttext synchronized" lang="en">X = Separate fields by horizontal tabulator</p>
      "! @parameter iv_trunc_trail_blanks     | <p class="shorttext synchronized" lang="en">X = Truncate trailing blanks at end of CHAR fields</p>
      "! @parameter iv_trunc_trail_blanks_eol | <p class="shorttext synchronized" lang="en">X = Truncate trailing blanks at end of the last column</p>
      "! @parameter iv_file_operation         | <p class="shorttext synchronized" lang="en">File operation type (CVC_FILE_HDLR-&gt;OPERATION-*)</p>
      "! @parameter iv_codepage               | <p class="shorttext synchronized" lang="en">Codepage (can use CODEPAGE as default, see TCP00)</p>
      "! @parameter iv_length                 | <p class="shorttext synchronized" lang="en">Write file of this length</p>
      "! @parameter ev_length                 | <p class="shorttext synchronized" lang="en">Transmitted length</p>
      "! @parameter ct_file                   | <p class="shorttext synchronized" lang="en">File as table</p>
      "! @raising   zcx_ca_file_utility       | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      download
        IMPORTING
          iv_file_mode              TYPE swr_filetype
          iv_check_auth             TYPE abap_bool
          iv_write_lf               TYPE abap_bool  DEFAULT abap_true
          iv_confirm_overwrite      TYPE abap_bool  DEFAULT abap_false
          iv_write_field_separator  TYPE abap_bool  DEFAULT abap_false
          iv_trunc_trail_blanks     TYPE abap_bool  DEFAULT abap_true
          iv_trunc_trail_blanks_eol TYPE abap_bool  DEFAULT abap_true
          iv_file_operation         TYPE dsetactype
          iv_codepage               TYPE cpcodepage OPTIONAL
          iv_length                 TYPE i          OPTIONAL
        EXPORTING
          ev_length                 TYPE i
        CHANGING
          !ct_file                  TYPE STANDARD TABLE
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Upload a file from the PC/client</p>
      "!
      "! @parameter iv_file_mode           | <p class="shorttext synchronized" lang="en">Binary or character mode (CVC_FILE_HDLR-&gt;MODE-*)</p>
      "! @parameter iv_codepage            | <p class="shorttext synchronized" lang="en">Codepage (can use CODEPAGE as default, see TCP00)</p>
      "! @parameter iv_has_field_separator | <p class="shorttext synchronized" lang="en">X = Field are TAB separated, result table needs corresp cols</p>
      "! @parameter et_file                | <p class="shorttext synchronized" lang="en">File as table</p>
      "! @parameter ev_length              | <p class="shorttext synchronized" lang="en">File length</p>
      "! @raising   zcx_ca_file_utility    | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      upload
        IMPORTING
          iv_file_mode           TYPE swr_filetype
          iv_codepage            TYPE cpcodepage OPTIONAL
          iv_has_field_separator TYPE abap_bool  DEFAULT abap_false
        EXPORTING
          et_file                TYPE STANDARD TABLE
          ev_length              TYPE i
        RAISING
          zcx_ca_file_utility.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_ca_file_handler_pc IMPLEMENTATION.

  METHOD constructor.
    "---------------------------------------------------------------------*
    "     Constructor
    "---------------------------------------------------------------------*
    super->constructor( ).
    directory_hdlr = zcl_ca_directory_handler=>get_instance( cvc_file_util->location-pc ).
  ENDMETHOD.                    "Constructor


  METHOD download.
    "---------------------------------------------------------------------*
    "     Download a file to the PC/client
    "---------------------------------------------------------------------*
    CLEAR ev_length.

    cvc_file_util->is_mode_valid( iv_file_mode ).
    cvc_file_util->is_operation_valid( iv_file_operation ).

    DATA(lv_codepage) = CONV abap_encod( iv_codepage ).
    IF lv_codepage EQ '0000'.
      CLEAR lv_codepage.
    ENDIF.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  = directory_hdlr->path_file
        bin_filesize              = iv_length
        filetype                  = SWITCH #( iv_file_mode
                                      WHEN cvc_file_util->mode-binary THEN 'BIN'
                                      WHEN cvc_file_util->mode-text   THEN 'ASC' ) ##no_text
        codepage                  = lv_codepage
        append                    = xsdbool( iv_file_operation EQ cvc_file_util->operation-append )
        write_field_separator     = iv_write_field_separator
        no_auth_check             = xsdbool( iv_check_auth EQ abap_false )
        trunc_trailing_blanks     = iv_trunc_trail_blanks
        trunc_trailing_blanks_eol = iv_trunc_trail_blanks_eol
        write_lf                  = iv_write_lf
        confirm_overwrite         = iv_confirm_overwrite
      IMPORTING
        filelength                = ev_length
      CHANGING
        data_tab                  = ct_file
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                        iv_method   = 'GUI_DOWNLOAD'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-input.
    DATA(lv_lines) = lines( ct_file ).
    ADD lv_lines TO: record_cnt_per_file_operation-transfer,
                     zcl_ca_file_handler=>record_cnt_over_all_operations-transfer.
  ENDMETHOD.                    "download


  METHOD upload.
    "---------------------------------------------------------------------*
    "     Upload a file from the PC/client
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_codepage          TYPE abap_encod.

    CLEAR: ev_length,
           et_file.

    cvc_file_util->is_mode_valid( iv_file_mode ).

    IF iv_codepage IS NOT INITIAL.
      lv_codepage = iv_codepage.
    ELSE.
      lv_codepage = directory_hdlr->codepage.
    ENDIF.
    IF lv_codepage EQ '0000'.
      CLEAR lv_codepage.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = directory_hdlr->path_file
        filetype                = SWITCH #( iv_file_mode
                                    WHEN cvc_file_util->mode-binary THEN 'BIN'
                                    WHEN cvc_file_util->mode-text   THEN 'ASC' ) ##no_text
        has_field_separator     = iv_has_field_separator
        codepage                = lv_codepage
      IMPORTING
        filelength              = ev_length
      CHANGING
        data_tab                = et_file
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                        iv_method   = 'GUI_UPLOAD'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "upload


  METHOD zif_ca_file_handler~delete.
    "---------------------------------------------------------------------*
    "     Delete
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error TYPE REF TO zcx_ca_file_utility,
      lv_rc    TYPE syst_subrc ##needed.

    directory_hdlr->is_path_file_available( iv_path_file ).

    cl_gui_frontend_services=>file_delete(
      EXPORTING
        filename             = directory_hdlr->path_file
      CHANGING
        rc                   = lv_rc
      EXCEPTIONS
        file_delete_failed   = 1
        cntl_error           = 2
        error_no_gui         = 3
        file_not_found       = 4
        access_denied        = 5
        unknown_error        = 6
        not_supported_by_gui = 7
        wrong_parameter      = 8
        OTHERS               = 9 ).
    IF sy-subrc NE 0.
      lx_error = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                iv_method   = 'FILE_DELETE'
                                                iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "zif_ca_file_handler~delete


  METHOD zif_ca_file_handler~read.
    "---------------------------------------------------------------------*
    "     Get file (READ from server / UPLOAD from client PC)
    "---------------------------------------------------------------------*
    super->zif_ca_file_handler~read( iv_path_file = iv_path_file
                                     iv_file_mode = iv_file_mode ).

    upload(
        EXPORTING
          iv_file_mode           = iv_file_mode
          iv_codepage            = iv_codepage
          iv_has_field_separator = iv_has_field_separator
        IMPORTING
          et_file                = et_file
          ev_length              = ev_length ).
  ENDMETHOD.                    "zif_ca_file_handler~read


  METHOD zif_ca_file_handler~write.
    "---------------------------------------------------------------------*
    "     Write file (DOWNLOAD on client/PC)
    "---------------------------------------------------------------------*
    cvc_file_util->is_operation_valid( iv_file_operation ).

    IF iv_file_operation EQ cvc_file_util->operation-input.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>param_invalid
          mv_msgty = zcx_ca_file_utility=>c_msgty_e
          mv_msgv1 = 'IV_FILE_OPERATION'
          mv_msgv2 = CONV #( iv_file_operation ) ##no_text.
    ENDIF.

    cvc_file_util->is_mode_valid( iv_file_mode ).

    directory_hdlr->is_path_file_available( iv_path_file ).

    download(
        EXPORTING
          iv_file_mode              = iv_file_mode
          iv_check_auth             = iv_check_auth
          iv_write_lf               = iv_write_lf
          iv_confirm_overwrite      = iv_confirm_overwrite
          iv_write_field_separator  = iv_write_field_separator
          iv_trunc_trail_blanks     = iv_trunc_trail_blanks
          iv_trunc_trail_blanks_eol = iv_trunc_trail_blanks_eol
          iv_file_operation         = iv_file_operation
          iv_codepage               = iv_codepage
          iv_length                 = iv_length
        IMPORTING
          ev_length                 = ev_length
        CHANGING
          ct_file                   = ct_file ).
  ENDMETHOD.                    "zif_ca_file_handler~write

ENDCLASS.
