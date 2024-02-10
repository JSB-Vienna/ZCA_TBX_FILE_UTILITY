"! <p class="shorttext synchronized" lang="en">CA-TBX: File utility for application server OR client/PC</p>
"!
"! <p>This class synergize / combine the two techniques of handling application server files and PC files
"! (= presentation server) in one class.</p>
"!
"! <p>If you want to use this class in a report with a selection screen, than please have a look into
"! <strong>class {@link zcl_ca_file_util_selscr_ctlr}</strong> to get more informations on how to use it.</p>
CLASS zcl_ca_file_handler DEFINITION PUBLIC
                                     CREATE PROTECTED
                                     ABSTRACT.

  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_file_handler.

*   l o c a l   t y p e   d e f i n i t i o n
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Counters</p>
      BEGIN OF ty_s_cnt_per_operation,
        read     TYPE i,          " No. of read records
        transfer TYPE i,          " No. of transferred records
      END   OF ty_s_cnt_per_operation,

      "! <p class="shorttext synchronized" lang="en">Dataset counter</p>
      BEGIN OF ty_s_dataset_cnt,
        append TYPE i,
        input  TYPE i,
        output TYPE i,
        update TYPE i,
      END   OF ty_s_dataset_cnt.

*   a l i a s e s
    ALIASES:
*     Attributes
      directory_hdlr            FOR zif_ca_file_handler~directory_hdlr,
      cvc_file_util             FOR zif_ca_file_handler~cvc_file_util,
      processing_params         FOR zif_ca_file_handler~processing_params,
*     Method
      delete                    FOR zif_ca_file_handler~delete,
      read                      FOR zif_ca_file_handler~read,
      set_processing_parameters FOR zif_ca_file_handler~set_processing_parameters,
      write                     FOR zif_ca_file_handler~write.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Record counters over all</p>
      record_cnt_over_all_operations TYPE ty_s_cnt_per_operation READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Dataset counters depending on operation type</p>
      dataset_counter_per_operation  TYPE ty_s_dataset_cnt READ-ONLY.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
*      "! <p class="shorttext synchronized" lang="en">File System Paths</p>
*      mo_path_hdlr    TYPE REF TO cl_fs_path READ-ONLY,
*      "! <p class="shorttext synchronized" lang="en">Constants for File Utility</p>
*      cvc_file_hdlr TYPE REF TO zcl_ca_c_file_utility READ-ONLY,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Counters</p>
      record_cnt_per_file_operation TYPE ty_s_cnt_per_operation READ-ONLY.

**     s i n g l e   v a l u e s
*      "! <p class="shorttext synchronized" lang="en">Current codepage of either the app. server or the client/PC</p>
*      mv_codepage     TYPE cpcodepage READ-ONLY,
*      "! <p class="shorttext synchronized" lang="en">Complete path and file name for dataset access</p>
*      mv_path_file    TYPE string READ-ONLY,
*      "! <p class="shorttext synchronized" lang="en">OS specific path separator</p>
*      mv_path_sep     TYPE zca_d_path_separator READ-ONLY,
*      "! <p class="shorttext synchronized" lang="en">Location: A = server / P = client/PC</p>
*      mv_location     TYPE dxlocation READ-ONLY.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter location            | <p class="shorttext synchronized" lang="en">Location: server or client (CVC_FILE_HDLR-&gt;LOCATION-*)</p>
      "! @parameter result              | <p class="shorttext synchronized" lang="en">CA-TBX: File utility for server OR client/PC</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      get_instance
        IMPORTING
          location      TYPE dxlocation
        RETURNING
          VALUE(result) TYPE REF TO zif_ca_file_handler
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Print simple log with statistics over all activities</p>
      print_log_over_all,

      "! <p class="shorttext synchronized" lang="en">Reset statistics</p>
      reset_statistics.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Get physical path and file name to a logical filename</p>
      "!
      "! <p><strong>Attention!</strong> The parameters {@link .DATA:iv_including_dir} and {@link .DATA:iv_with_file_extension}
      "! exclude each other, means you can either use one or the other with value ABAP_TRUE.</p>
      "!
      "! <p>Some more details about the parameters of this method you can find in the documentation of function
      "! module {@link FUNC:file_get_name} which has the same parameters as the function module
      "! {@link FUNC:file_get_name_and_validate} used in this method.</p>
      "!
      "! @parameter logical_filename    | <p class="shorttext synchronized" lang="en">Logical file name defined in TA FILE</p>
      "! @parameter parameter_1         | <p class="shorttext synchronized" lang="en">Replacement parameter 1</p>
      "! @parameter parameter_2         | <p class="shorttext synchronized" lang="en">Replacement parameter 2</p>
      "! @parameter parameter_3         | <p class="shorttext synchronized" lang="en">Replacement parameter 3</p>
      "! @parameter is_for_local_client | <p class="shorttext synchronized" lang="en">X = Location is the local client/PC</p>
      "! @parameter including_dir       | <p class="shorttext synchronized" lang="en">X = Expecting / return a physical path</p>
      "! @parameter with_file_extension | <p class="shorttext synchronized" lang="en">X = Append internal file extension to file name ..e. g. BIN</p>
      "! @parameter result              | <p class="shorttext synchronized" lang="en">Complete path and file name</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      get_pathfile_from_logical_name
        IMPORTING
          logical_filename    TYPE fileintern
          parameter_1         TYPE text255 DEFAULT space
          parameter_2         TYPE text255 DEFAULT space
          parameter_3         TYPE text255 DEFAULT space
          is_for_local_client TYPE abap_boolean DEFAULT abap_false
          including_dir       TYPE abap_boolean DEFAULT abap_true
          with_file_extension TYPE abap_boolean DEFAULT abap_false
        RETURNING
          VALUE(result)       TYPE string
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Print simple log with statistics</p>
      print_log.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Assemble path and file name, after resolving logical names</p>
      "!
      "! @parameter processing_params   | <p class="shorttext synchronized" lang="en">Parameters where and how the file should be proceeded</p>
      "! @parameter result              | <p class="shorttext synchronized" lang="en">Processing parameters completed by path and file name</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      assemble_path_n_file_name
        IMPORTING
          processing_params TYPE zca_s_file_util_sel_params
        RETURNING
          VALUE(result)     TYPE zca_s_file_util_sel_params
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      constructor
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Check whether a file name is provided</p>
      "!
      "! @parameter processing_params   | <p class="shorttext synchronized" lang="en">Parameters where and how the file should be proceeded</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      is_file_name_provided
        IMPORTING
          processing_params TYPE zca_s_file_util_sel_params
        RAISING
          zcx_ca_file_utility.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_file_handler IMPLEMENTATION.

  METHOD assemble_path_n_file_name.
    "---------------------------------------------------------------------*
    "     Assemble path and file name, after resolving logical names if necessary
    "---------------------------------------------------------------------*
    result = processing_params.

    CASE processing_params-type.
      WHEN cvc_file_util->type-physical.
        "Clean up directory delimiter for a uniform / consistent path + file name handling
        IF result-path IS NOT INITIAL.
          DATA(lv_offset) = strlen( result-path ) - 1.
          IF result-path+lv_offset(1) EQ directory_hdlr->path_separator.
            result-path+lv_offset(1) = space.
          ENDIF.

          result-path_file = result-path.
        ENDIF.

        IF result-file_name IS NOT INITIAL.
          result-path_file = |{ result-path_file }{ directory_hdlr->path_separator }| &
                             |{ result-file_name }|.
        ENDIF.

      WHEN cvc_file_util->type-logical.
        result-path_file = get_pathfile_from_logical_name(
                             logical_filename = CONV #( result-file_name )
*                     USE_PRESENTATION_SERVER = xsdbool( result-location eq cvc_file_util->location-pc )
*                     iv_param_1          = space
*                     iv_param_2          = space
*                     iv_param_3          = space
*                     iv_incl_dir         = abap_true
*                     iv_with_file_ext    = abap_false
*                     iv_use_buffer       = abap_false
                           ).
    ENDCASE.

    directory_hdlr->is_path_file_available( result-path_file ).
  ENDMETHOD.                    "assemble_path_n_file_name


  METHOD constructor.
    "---------------------------------------------------------------------*
    "     Constructor
    "---------------------------------------------------------------------*
    cvc_file_util = zcl_ca_c_file_utility=>get_instance( ).
  ENDMETHOD.                    "Constructor


  METHOD get_instance.
    "---------------------------------------------------------------------*
    "     Get instance
    "---------------------------------------------------------------------*
    DATA(_cvc_file) = zcl_ca_c_file_utility=>get_instance( ).
    _cvc_file->is_location_valid( location ).

    CASE location.
      WHEN _cvc_file->location-pc.
        result ?= NEW zcl_ca_file_handler_pc( ).

      WHEN _cvc_file->location-server.
        result ?= NEW zcl_ca_file_handler_as( ).
    ENDCASE.
  ENDMETHOD.                    "download


  METHOD get_pathfile_from_logical_name.
    "---------------------------------------------------------------------*
    "     Get physical path and file name to a logical filename
    "---------------------------------------------------------------------*
    CALL FUNCTION 'FILE_GET_NAME_AND_VALIDATE'
      EXPORTING
        logical_filename    = logical_filename
        operating_system    = directory_hdlr->operation_system
        parameter_1         = parameter_1
        parameter_2         = parameter_2
        parameter_3         = parameter_3
        including_dir       = abap_true
        with_file_extension = abap_false
        use_buffer          = abap_false
      IMPORTING
        file_name           = result
      EXCEPTIONS
        file_not_found      = 1
        validation_failed   = 2
        incorrect_path      = 3
        OTHERS              = 4.
    IF sy-subrc NE 0.
      DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_function = 'FILE_GET_NAME_AND_VALIDATE'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF _error IS BOUND.
        RAISE EXCEPTION _error.
      ENDIF.
    ENDIF.

    "Create physical filename handler
    directory_hdlr->get_physical_filename_handler( result ).
  ENDMETHOD.                    "get_pathfile_from_logical_name


  METHOD is_file_name_provided.
    "---------------------------------------------------------------------*
    "     Check whether a file name is provided
    "---------------------------------------------------------------------*
    CASE processing_params-type.
      WHEN cvc_file_util->type-physical.
        IF processing_params-path      IS INITIAL OR
           processing_params-file_name IS INITIAL.
          "Please enter a valid path and/or filename
          RAISE EXCEPTION TYPE zcx_ca_file_utility
            MESSAGE ID 'DB6PM' TYPE zcx_ca_file_utility=>c_msgty_e NUMBER '113'.
        ENDIF.

      WHEN cvc_file_util->type-logical.
        IF processing_params-file_name IS INITIAL.
          "No logical filename provided
          RAISE EXCEPTION TYPE zcx_ca_file_utility
            MESSAGE ID 'HR3PRNA' TYPE zcx_ca_file_utility=>c_msgty_e NUMBER '716'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "is_file_name_provided


  METHOD print_log.
    "---------------------------------------------------------------------*
    "     Print statistics as simple list
    "---------------------------------------------------------------------*
    NEW-PAGE LINE-SIZE 132.

    IF record_cnt_per_file_operation-read IS NOT INITIAL.
      WRITE: /   'File name for INPUT (reading) ...................:'(s01),
              52(80) directory_hdlr->path_file.
      IF directory_hdlr->path_file+80 IS NOT INITIAL.
        WRITE /52(80) directory_hdlr->path_file+80.
      ENDIF.
      IF directory_hdlr->path_file+160 IS NOT INITIAL.
        WRITE /52(80) directory_hdlr->path_file+160.
      ENDIF.
      WRITE: /   'Number of read records ..........................:'(s02),
      52 record_cnt_per_file_operation-read NO-ZERO.
      SKIP.
    ENDIF.

    IF record_cnt_per_file_operation-transfer IS NOT INITIAL.
      WRITE: /   'File name for OUTPUT (transfering) ..............:'(s03),
      52(80) directory_hdlr->path_file.
      IF directory_hdlr->path_file+80 IS NOT INITIAL.
        WRITE /52(80) directory_hdlr->path_file+80.
      ENDIF.
      IF directory_hdlr->path_file+160 IS NOT INITIAL.
        WRITE /52(80) directory_hdlr->path_file+160.
      ENDIF.
      WRITE: /   'Number of transferred records ...................:'(s04),
      52 record_cnt_per_file_operation-transfer NO-ZERO.
    ENDIF.
    SKIP 2.
  ENDMETHOD.                    "print_log


  METHOD print_log_over_all.
    "---------------------------------------------------------------------*
    "     Print simple log with statistics over all activities
    "---------------------------------------------------------------------*
    NEW-PAGE LINE-SIZE 132.

    WRITE: /      'Number of opened files for ...'(s07).
    IF zcl_ca_file_handler=>dataset_counter_per_operation-input IS NOT INITIAL.
      WRITE: /   '... action INPUT ................................:'(s08),
              52 zcl_ca_file_handler=>dataset_counter_per_operation-input NO-ZERO.
    ENDIF.
    IF zcl_ca_file_handler=>dataset_counter_per_operation-output IS NOT INITIAL.
      WRITE: /   '... action OUTPUT ...............................:'(s09),
              52 zcl_ca_file_handler=>dataset_counter_per_operation-output NO-ZERO.
    ENDIF.
    IF zcl_ca_file_handler=>dataset_counter_per_operation-append IS NOT INITIAL.
      WRITE: /   '... action APPEND ...............................:'(s10),
              52 zcl_ca_file_handler=>dataset_counter_per_operation-append NO-ZERO.
    ENDIF.
    IF zcl_ca_file_handler=>dataset_counter_per_operation-update IS NOT INITIAL.
      WRITE: /   '... action UPDATE ...............................:'(s11),
              52 zcl_ca_file_handler=>dataset_counter_per_operation-update NO-ZERO.
    ENDIF.
    SKIP.

    IF zcl_ca_file_handler=>record_cnt_over_all_operations-read IS NOT INITIAL.
      WRITE: /   'Number of records read over all .................:'(s05),
              52 zcl_ca_file_handler=>record_cnt_over_all_operations-read NO-ZERO.
      SKIP.
    ENDIF.

    IF zcl_ca_file_handler=>record_cnt_over_all_operations-transfer IS NOT INITIAL.
      WRITE: /   'Number of transferred records over all ..........:'(s06),
              52 zcl_ca_file_handler=>record_cnt_over_all_operations-transfer NO-ZERO.
    ENDIF.
    SKIP 2.
  ENDMETHOD.                    "print_log_over_all


  METHOD reset_statistics.
    "---------------------------------------------------------------------*
    "     Reset statistics
    "---------------------------------------------------------------------*
    CLEAR: zcl_ca_file_handler=>record_cnt_over_all_operations,
           zcl_ca_file_handler=>dataset_counter_per_operation.
  ENDMETHOD.                    "reset_statistics


  METHOD zif_ca_file_handler~delete.
    "---------------------------------------------------------------------*
    "     Delete file
    "---------------------------------------------------------------------*
    "R e d e f i n e d
  ENDMETHOD.                    "zif_ca_file_handler~delete


  METHOD zif_ca_file_handler~read.
    "---------------------------------------------------------------------*
    "     Read entire file
    "---------------------------------------------------------------------*
    "R e d e f i n e d
  ENDMETHOD.                    "zif_ca_file_handler~read


  METHOD zif_ca_file_handler~set_processing_parameters.
    "---------------------------------------------------------------------*
    "     Set parameters where and how the file should be proceeded
    "---------------------------------------------------------------------*
    cvc_file_util->is_location_valid( processing_params-location ).
    cvc_file_util->is_operation_valid( processing_params-operation ).
    cvc_file_util->is_mode_valid( processing_params-mode ).
    cvc_file_util->is_type_valid( processing_params-type ).

    is_file_name_provided( processing_params ).
    me->processing_params = assemble_path_n_file_name( processing_params ).
  ENDMETHOD.                    "zif_ca_file_handler~set_processing_parameters


  METHOD zif_ca_file_handler~write.
    "---------------------------------------------------------------------*
    "     Write entire file
    "---------------------------------------------------------------------*
    "R e d e f i n e d
  ENDMETHOD.                    "zif_ca_file_handler~write

ENDCLASS.
