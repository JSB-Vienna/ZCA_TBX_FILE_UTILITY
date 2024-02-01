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
*     Types
*      ty_s_directory_entry FOR zif_ca_file_handler~directory_hdlr
*     Attributes
      directory_hdlr FOR zif_ca_file_handler~directory_hdlr,
      cvc_file_util  FOR zif_ca_file_handler~cvc_file_util,
*     Method
      delete         FOR zif_ca_file_handler~delete,
      read           FOR zif_ca_file_handler~read,
      write          FOR zif_ca_file_handler~write.

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
      "! @parameter iv_location         | <p class="shorttext synchronized" lang="en">Location: server or client (CVC_FILE_HDLR-&gt;LOCATION-*)</p>
      "! @parameter result              | <p class="shorttext synchronized" lang="en">CA-TBX: File utility for server OR client/PC</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      get_instance
        IMPORTING
          iv_location   TYPE dxlocation
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
      "! @parameter iv_logical_filename | <p class="shorttext synchronized" lang="en">Logical file name defined in TA FILE</p>
      "! @parameter iv_param_1          | <p class="shorttext synchronized" lang="en">Replacement parameter 1</p>
      "! @parameter iv_param_2          | <p class="shorttext synchronized" lang="en">Replacement parameter 2</p>
      "! @parameter iv_param_3          | <p class="shorttext synchronized" lang="en">Replacement parameter 3</p>
      "! @parameter iv_incl_dir         | <p class="shorttext synchronized" lang="en">X = Expecting / return a physical path</p>
      "! @parameter iv_with_file_ext    | <p class="shorttext synchronized" lang="en">X = Append internal file extension to file name ..e. g. BIN</p>
      "! @parameter iv_use_buffer       | <p class="shorttext synchronized" lang="en">X = Use buffer to avoid recurring data access and checks</p>
      "! @parameter rv_path_file        | <p class="shorttext synchronized" lang="en">Complete path and file name</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      get_pathfile_from_logical_name
        IMPORTING
          iv_logical_filename TYPE fileintern
          iv_param_1          TYPE clike DEFAULT space
          iv_param_2          TYPE clike DEFAULT space
          iv_param_3          TYPE clike DEFAULT space
          iv_incl_dir         TYPE abap_bool DEFAULT abap_true
          iv_with_file_ext    TYPE abap_bool DEFAULT abap_false
          iv_use_buffer       TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rv_path_file) TYPE string
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Print simple log with statistics</p>
      print_log.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">OS of location (conform for logical filename)</p>
      mv_opsys    TYPE syst_opsys.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      constructor
        RAISING
          zcx_ca_file_utility.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_file_handler IMPLEMENTATION.

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
    _cvc_file->is_location_valid( iv_location ).

    CASE iv_location.
      WHEN _cvc_file->location-pc.
        result ?= NEW zcl_ca_file_handler_pc( ).

      WHEN _cvc_file->location-server.
        result ?= NEW zcl_ca_file_handler_as( ).
    ENDCASE.
  ENDMETHOD.                    "download


  METHOD zif_ca_file_handler~read.
    "---------------------------------------------------------------------*
    "     Get file (READ from server / UPLOAD from client PC)
    "---------------------------------------------------------------------*
    cvc_file_util->is_mode_valid( iv_file_mode ).
    directory_hdlr->is_path_file_available( iv_path_file ).

    "R e d e f i n e d   f r o m   h e r e
  ENDMETHOD.                    "zif_ca_file_handler~read


  METHOD get_pathfile_from_logical_name.
    "---------------------------------------------------------------------*
    "     Get physical path and file name to a logical filename
    "---------------------------------------------------------------------*
    CALL FUNCTION 'FILE_GET_NAME_AND_VALIDATE'
      EXPORTING
        logical_filename    = iv_logical_filename
        operating_system    = mv_opsys
        parameter_1         = iv_param_1
        parameter_2         = iv_param_2
        parameter_3         = iv_param_3
        including_dir       = iv_incl_dir
        with_file_extension = iv_with_file_ext
        use_buffer          = iv_use_buffer
      IMPORTING
        file_name           = rv_path_file
      EXCEPTIONS
        file_not_found      = 1
        validation_failed   = 2
        incorrect_path      = 3
        OTHERS              = 4.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_function = 'FILE_GET_NAME_AND_VALIDATE'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    "Create physical filename handler
    directory_hdlr->get_physical_filename_handler( rv_path_file ).
  ENDMETHOD.                    "get_pathfile_from_logical_name


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

ENDCLASS.
