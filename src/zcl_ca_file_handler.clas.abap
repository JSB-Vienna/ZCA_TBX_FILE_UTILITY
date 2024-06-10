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
      directory_entry           FOR zif_ca_file_handler~directory_entry,
      directory_hdlr            FOR zif_ca_file_handler~directory_hdlr,
      file_name_handler         FOR zif_ca_file_handler~file_name_handler,
      file_length               FOR zif_ca_file_handler~file_length,
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
      "! @parameter processing_params   | <p class="shorttext synchronized" lang="en">Parameters where and how the file should be processed</p>
      "! @parameter directory_entry     | <p class="shorttext synchronized" lang="en">CA-TBX: Directory entry details</p>
      "! @parameter result              | <p class="shorttext synchronized" lang="en">CA-TBX: File utility for server OR client/PC</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      get_instance
        IMPORTING
          processing_params TYPE zca_s_file_util_sel_params
          directory_entry   TYPE zca_s_directory_entry OPTIONAL
        RETURNING
          VALUE(result)     TYPE REF TO zif_ca_file_handler
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Print simple log with statistics over all activities</p>
      print_log_over_all,

      "! <p class="shorttext synchronized" lang="en">Reset statistics</p>
      reset_statistics.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Print simple log with statistics</p>
      print_log.


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
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Check whether a file name is provided</p>
      "!
      "! @parameter processing_params   | <p class="shorttext synchronized" lang="en">Parameters where and how the file should be processed</p>
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

  METHOD constructor.
    "---------------------------------------------------------------------*
    "     Constructor
    "---------------------------------------------------------------------*
    cvc_file_util       = zcl_ca_c_file_utility=>get_instance( ).
    directory_hdlr      = zcl_ca_directory_handler=>get_instance( processing_params-location ).
    me->directory_entry = directory_entry.
    set_processing_parameters( processing_params ).
  ENDMETHOD.                    "Constructor


  METHOD get_instance.
    "---------------------------------------------------------------------*
    "     Get instance
    "---------------------------------------------------------------------*
    DATA(_cvc_file) = zcl_ca_c_file_utility=>get_instance( ).
    _cvc_file->is_location_valid( processing_params-location ).

    CASE processing_params-location.
      WHEN _cvc_file->location-pc.
        result ?= NEW zcl_ca_file_handler_pc( processing_params = processing_params
                                              directory_entry   = directory_entry ).

      WHEN _cvc_file->location-server.
        result ?= NEW zcl_ca_file_handler_as( processing_params = processing_params
                                              directory_entry   = directory_entry ).
    ENDCASE.
  ENDMETHOD.                    "download


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
    me->processing_params = processing_params.

    directory_hdlr->assemble_path_n_file_name(
                                        IMPORTING
                                          path_file_name_hdlr = file_name_handler
                                        CHANGING
                                          processing_params   = me->processing_params ).
  ENDMETHOD.                    "zif_ca_file_handler~set_processing_parameters


  METHOD zif_ca_file_handler~write.
    "---------------------------------------------------------------------*
    "     Write entire file
    "---------------------------------------------------------------------*
    "R e d e f i n e d
  ENDMETHOD.                    "zif_ca_file_handler~write

ENDCLASS.
