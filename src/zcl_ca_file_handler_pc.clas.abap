"! <p class="shorttext synchronized" lang="en">CA-TBX: File utility for client/PC</p>
CLASS zcl_ca_file_handler_pc DEFINITION PUBLIC
                                        CREATE PROTECTED
                                        INHERITING FROM zcl_ca_file_handler
                                        GLOBAL FRIENDS zcl_ca_file_handler.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Download a file to the PC/client</p>
      "!
      "! @parameter check_authority              | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
      "! @parameter add_cr_lf_at_lines_end       | <p class="shorttext synchronized" lang="en">X = Add CR/LF at end of CHAR lines</p>
      "! @parameter confirm_overwriting          | <p class="shorttext synchronized" lang="en">X = Confirm overwriting file</p>
      "! @parameter use_horizont_tab_as_delim    | <p class="shorttext synchronized" lang="en">X = Separate fields by horizontal tabulator</p>
      "! @parameter truncate_trailing_blanks     | <p class="shorttext synchronized" lang="en">X = Truncate trailing blanks at end of CHAR fields</p>
      "! @parameter truncate_trailing_blanks_eol | <p class="shorttext synchronized" lang="en">X = Truncate trailing blanks at end of the last column</p>
      "! @parameter codepage                     | <p class="shorttext synchronized" lang="en">Codepage (can use CODEPAGE as default, see TCP00)</p>
      "! @parameter max_file_length              | <p class="shorttext synchronized" lang="en">Write file of this length</p>
      "! @parameter length_of_file               | <p class="shorttext synchronized" lang="en">Transmitted length</p>
      "! @parameter file                         | <p class="shorttext synchronized" lang="en">File as table</p>
      "! @raising   zcx_ca_file_utility          | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      download
        IMPORTING
          check_authority              TYPE abap_boolean
          add_cr_lf_at_lines_end       TYPE abap_boolean DEFAULT abap_true
          confirm_overwriting          TYPE abap_boolean DEFAULT abap_false
          use_horizont_tab_as_delim    TYPE abap_boolean DEFAULT abap_false
          truncate_trailing_blanks     TYPE abap_boolean DEFAULT abap_true
          truncate_trailing_blanks_eol TYPE abap_boolean DEFAULT abap_true
          codepage                     TYPE cpcodepage OPTIONAL
          max_file_length              TYPE i OPTIONAL
        EXPORTING
          length_of_file               TYPE i
        CHANGING
          file                         TYPE STANDARD TABLE
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Upload a file from the PC/client</p>
      "!
      "! @parameter codepage                    | <p class="shorttext synchronized" lang="en">Codepage (can use CODEPAGE as default, see TCP00)</p>
      "! @parameter fields_are_separated_by_tab | <p class="shorttext synchronized" lang="en">X = Flds are TAB separated, result table needs corresp cols</p>
      "! @parameter read_line_by_line           | <p class="shorttext synchronized" lang="en">X = File is written line-by-line into the internal table</p>
      "! @parameter file                        | <p class="shorttext synchronized" lang="en">File as table</p>
      "! @parameter length_of_file              | <p class="shorttext synchronized" lang="en">File length</p>
      "! @raising   zcx_ca_file_utility         | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      upload
        IMPORTING
          codepage                    TYPE cpcodepage OPTIONAL
          fields_are_separated_by_tab TYPE abap_boolean DEFAULT abap_false
          read_line_by_line           TYPE abap_boolean DEFAULT abap_true
        EXPORTING
          file                        TYPE STANDARD TABLE
          length_of_file              TYPE i
        RAISING
          zcx_ca_file_utility,

      zif_ca_file_handler~delete REDEFINITION,

      zif_ca_file_handler~read REDEFINITION,

      zif_ca_file_handler~write REDEFINITION.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter processing_params   | <p class="shorttext synchronized" lang="en">Parameters where and how the file should be processed</p>
      "! @parameter directory_entry     | <p class="shorttext synchronized" lang="en">CA-TBX: Directory entry details</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      constructor
        IMPORTING
          processing_params TYPE zca_s_file_util_sel_params
          directory_entry   TYPE zca_s_directory_entry OPTIONAL
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
    super->constructor( processing_params = processing_params
                        directory_entry   = directory_entry ).
    directory_hdlr = zcl_ca_directory_handler=>get_instance( cvc_file_util->location-pc ).
  ENDMETHOD.                    "Constructor


  METHOD download.
    "---------------------------------------------------------------------*
    "     Download a file to the PC/client
    "---------------------------------------------------------------------*
    CLEAR length_of_file.

    DATA(_codepage) = CONV abap_encod( codepage ).
    IF _codepage EQ '0000'.
      CLEAR _codepage.
    ENDIF.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  = CONV #( processing_params-path_file )
        bin_filesize              = max_file_length
        filetype                  = SWITCH #( processing_params-mode
                                      WHEN cvc_file_util->mode-binary THEN 'BIN'
                                      WHEN cvc_file_util->mode-text   THEN 'ASC' ) ##no_text
        codepage                  = _codepage
        append                    = xsdbool( processing_params-operation EQ cvc_file_util->operation-append )
        write_field_separator     = use_horizont_tab_as_delim
        no_auth_check             = xsdbool( check_authority EQ abap_false )
        trunc_trailing_blanks     = truncate_trailing_blanks
        trunc_trailing_blanks_eol = truncate_trailing_blanks_eol
        write_lf                  = add_cr_lf_at_lines_end
        confirm_overwrite         = confirm_overwriting
      IMPORTING
        filelength                = file_length
      CHANGING
        data_tab                  = file
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
      DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                        iv_method   = 'GUI_DOWNLOAD'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF _error IS BOUND.
        RAISE EXCEPTION _error.
      ENDIF.
    ENDIF.

    length_of_file = file_length.

    ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-input.
    DATA(_lines) = lines( file ).
    ADD _lines TO: record_cnt_per_file_operation-transfer,
                   zcl_ca_file_handler=>record_cnt_over_all_operations-transfer.
  ENDMETHOD.                    "download


  METHOD upload.
    "---------------------------------------------------------------------*
    "     Upload a file from the PC/client
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      _codepage          TYPE abap_encod.

    CLEAR: length_of_file,
           file.

    IF codepage IS NOT INITIAL.
      _codepage = codepage.
    ELSE.
      _codepage = directory_hdlr->codepage.
    ENDIF.
    IF _codepage EQ '0000'.
      CLEAR _codepage.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = CONV #( processing_params-path_file )
        filetype                = SWITCH #( processing_params-mode
                                    WHEN cvc_file_util->mode-binary THEN 'BIN'
                                    WHEN cvc_file_util->mode-text   THEN 'ASC' ) ##no_text
        has_field_separator     = fields_are_separated_by_tab
        codepage                = _codepage
        read_by_line            = read_line_by_line
      IMPORTING
        filelength              = file_length
      CHANGING
        data_tab                = file
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
      DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                        iv_method   = 'GUI_UPLOAD'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF _error IS BOUND.
        RAISE EXCEPTION _error.
      ENDIF.
    ENDIF.

    length_of_file = file_length.
  ENDMETHOD.                    "upload


  METHOD zif_ca_file_handler~delete.
    "---------------------------------------------------------------------*
    "     Delete
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      _return_code         TYPE syst_subrc ##needed.

    cl_gui_frontend_services=>file_delete(
      EXPORTING
        filename             = CONV #( processing_params-path_file )
      CHANGING
        rc                   = _return_code
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
      DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                iv_method   = 'FILE_DELETE'
                                                iv_subrc    = sy-subrc ) )  ##no_text.
      IF _error IS BOUND.
        RAISE EXCEPTION _error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "zif_ca_file_handler~delete


  METHOD zif_ca_file_handler~read.
    "---------------------------------------------------------------------*
    "     Read entire file from client/PC
    "---------------------------------------------------------------------*
    IF processing_params-operation NE cvc_file_util->operation-input.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>param_invalid
          mv_msgty = zcx_ca_file_utility=>c_msgty_e
          mv_msgv1 = 'PROCESSING_PARAMS-OPERATION'
          mv_msgv2 = CONV #( processing_params-operation ) ##no_text.
    ENDIF.

    upload(
        EXPORTING
          codepage        = codepage
        IMPORTING
          file            = file
          length_of_file  = length_of_file ).
  ENDMETHOD.                    "zif_ca_file_handler~read


  METHOD zif_ca_file_handler~write.
    "---------------------------------------------------------------------*
    "     Write entire file to client/PC
    "---------------------------------------------------------------------*
    IF processing_params-operation EQ cvc_file_util->operation-input.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>param_invalid
          mv_msgty = zcx_ca_file_utility=>c_msgty_e
          mv_msgv1 = 'PROCESSING_PARAMS-OPERATION'
          mv_msgv2 = CONV #( processing_params-operation ) ##no_text.
    ENDIF.

    download(
        EXPORTING
          check_authority              = check_authority
          codepage                     = codepage
          max_file_length              = max_file_length
        IMPORTING
          length_of_file               = length_of_file
        CHANGING
          file                         = file ).
  ENDMETHOD.                    "zif_ca_file_handler~write

ENDCLASS.
