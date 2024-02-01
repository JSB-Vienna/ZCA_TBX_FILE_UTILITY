"! <p class="shorttext synchronized" lang="en">CA-TBX: File utility for application server</p>
"!
"! <p>This class synergize / combine the two techniques of handling application server files and PC files
"! (= presentation server) in one class.</p>
"!
"! <p>If you want to use this class in a report with a selection screen, than please have a look into
"! <strong>class {@link zcl_ca_file_util_selscr_ctlr}</strong> to get more informations on how to use it.</p>
CLASS zcl_ca_file_handler_as DEFINITION PUBLIC
                                        CREATE PROTECTED
                                        INHERITING FROM zcl_ca_file_handler
                                        GLOBAL FRIENDS zcl_ca_file_handler.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Close dataset</p>
      "!
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      close_dataset
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Open dataset</p>
      "!
      "! @parameter iv_path_file        | <p class="shorttext synchronized" lang="en">Physical path and/or file name</p>
      "! @parameter iv_file_mode        | <p class="shorttext synchronized" lang="en">Binary or character mode (CVC_FILE_HDLR-&gt;MODE-*)</p>
      "! @parameter iv_file_operation   | <p class="shorttext synchronized" lang="en">File operation type (CVC_FILE_HDLR-&gt;OPERATION-*)</p>
      "! @parameter iv_codepage         | <p class="shorttext synchronized" lang="en">Codepage (can use CODEPAGE as default, see TCP00)</p>
      "! @parameter iv_check_auth       | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      open_dataset
        IMPORTING
          iv_path_file      TYPE string       OPTIONAL      "is may be already availabe via e. g. GET_LOGICAL_FILENAME
          iv_file_mode      TYPE swr_filetype DEFAULT zcl_ca_c_file_utility=>mode-text
          iv_file_operation TYPE dsetactype   DEFAULT zcl_ca_c_file_utility=>operation-input
          iv_codepage       TYPE cpcodepage   OPTIONAL
          iv_check_auth     TYPE abap_bool    DEFAULT abap_true
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Read data from file and write statistics</p>
      "!
      "! @parameter iv_read_with_max_length | <p class="shorttext synchronized" lang="en">Expected length (lower 0 = complete file)</p>
      "! @parameter es_record               | <p class="shorttext synchronized" lang="en">Record for file</p>
      "! @parameter ev_actual_length        | <p class="shorttext synchronized" lang="en">Actual length of last record</p>
      "! @parameter ev_no_more_record       | <p class="shorttext synchronized" lang="en">X = No more records available (leave loop)</p>
      "! @raising   zcx_ca_file_utility     | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      read_dataset
        IMPORTING
          iv_read_with_max_length TYPE i DEFAULT -1
        EXPORTING
          es_record               TYPE data
          ev_actual_length        TYPE i
          ev_no_more_record       TYPE abap_bool
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Transfer data to file and write statistics</p>
      "!
      "! @parameter is_record           | <p class="shorttext synchronized" lang="en">Record for file</p>
      "! @parameter iv_rec_len          | <p class="shorttext synchronized" lang="en">Length of single record (lower eq 0 = normal behaviour)</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      transfer_dataset
        IMPORTING
          is_record  TYPE simple
          iv_rec_len TYPE i DEFAULT -1
        RAISING
          zcx_ca_file_utility,

      zif_ca_file_handler~delete REDEFINITION,

      zif_ca_file_handler~read REDEFINITION,

      zif_ca_file_handler~write REDEFINITION.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check authority of path and file</p>
      "!
      "! @parameter iv_file_operation   | <p class="shorttext synchronized" lang="en">File operation type (CVC_FILE_HDLR-&gt;OPERATION-*)</p>
      "! @parameter iv_check_auth       | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      authority_check
        IMPORTING
          iv_file_operation TYPE dsetactype
          iv_check_auth     TYPE abap_bool
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Check result of OPEN DATASET and raise exception</p>
      "!
      "! @parameter iv_subrc            | <p class="shorttext synchronized" lang="en">Return code of OPEN DATASET statement</p>
      "! @parameter iv_msg              | <p class="shorttext synchronized" lang="en">Message string of OPEN DATASET statement</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      check_result_of_open_dataset
        IMPORTING
          VALUE(iv_subrc) TYPE syst_subrc
          iv_msg          TYPE bapi_msg
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      constructor
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Determine structure / field length of a record</p>
      "!
      "! @parameter iv_file_mode        | <p class="shorttext synchronized" lang="en">Binary or character mode (CVC_FILE_HDLR-&gt;MODE-*)</p>
      "! @parameter is_record           | <p class="shorttext synchronized" lang="en">Data object to determine length</p>
      "! @parameter rv_rec_len          | <p class="shorttext synchronized" lang="en">Transmitted length</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      get_records_length
        IMPORTING
          iv_file_mode      TYPE swr_filetype
          is_record         TYPE data
        RETURNING
          VALUE(rv_rec_len) TYPE i
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Write file from application server</p>
      "!
      "! @parameter iv_file_mode        | <p class="shorttext synchronized" lang="en">Binary or character mode (CVC_FILE_HDLR-&gt;MODE-*)</p>
      "! @parameter iv_length           | <p class="shorttext synchronized" lang="en">File length</p>
      "! @parameter it_file             | <p class="shorttext synchronized" lang="en">File as table</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      write_file
        IMPORTING
          iv_file_mode TYPE swr_filetype
          iv_length    TYPE i
          it_file      TYPE STANDARD TABLE
        RAISING
          zcx_ca_file_utility
          zcx_ca_param.



* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_file_handler_as IMPLEMENTATION.

  METHOD authority_check.
    "---------------------------------------------------------------------*
    "     Check authority of path and file
    "---------------------------------------------------------------------*
    IF iv_check_auth EQ abap_false.
      RETURN.
    ENDIF.

    cvc_file_util->is_operation_valid( iv_file_operation ).

    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
      EXPORTING
        filename         = CONV fileextern( directory_hdlr->path_file )
        activity         = SWITCH char20(
                              iv_file_operation
                                WHEN cvc_file_util->operation-input
                                  THEN sabc_act_read

                                WHEN cvc_file_util->operation-output OR
                                     cvc_file_util->operation-append OR
                                     cvc_file_util->operation-update
                                  THEN sabc_act_write

                                WHEN cvc_file_util->operation-delete
                                  THEN sabc_act_delete )
      EXCEPTIONS
        no_authority     = 1
        activity_unknown = 2
        OTHERS           = 3.
    CASE sy-subrc.
      WHEN 0.
        "everything is fine

      WHEN 1.
        DATA(_path_file) = CONV bapi_msg( directory_hdlr->path_file ).
        "No authority accessing file '&1&2&3&4'
        RAISE EXCEPTION TYPE zcx_ca_file_utility
          EXPORTING
            textid   = zcx_ca_file_utility=>no_auth_for_path
            mv_msgty = zcx_ca_file_utility=>c_msgty_e
            mv_msgv1 = _path_file(50)
            mv_msgv2 = _path_file+50(50)
            mv_msgv3 = _path_file+100(50)
            mv_msgv4 = _path_file+150(50).

      WHEN OTHERS.
        DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_function = 'AUTHORITY_CHECK_DATASET'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
        IF _error IS BOUND.
          RAISE EXCEPTION _error.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "authority_check


  METHOD check_result_of_open_dataset.
    "-----------------------------------------------------------------*
    "   Check result of OPEN DATASET and raise exception
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _message               TYPE bapi_msg.

    IF iv_subrc EQ 0.
      RETURN.
    ENDIF.

    "Opening the requested file results in an error

    "Either send the message of the OPEN DATASET statement or ...
    IF iv_msg IS NOT INITIAL.
      "To fill SY-MSG++ fields for exception creation
      _message = |{ iv_msg } / => { directory_hdlr->path_file }|.
      MESSAGE e897(s1) WITH _message(50)     _message+50(50)
                            _message+100(50) _message+150(50) INTO _message.
      DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'ZCL_CA_FILE_UTIL'
                                                        iv_method   = 'OPEN'
                                                        iv_subrc    = iv_subrc ) ) ##no_text.
      IF _error IS BOUND.
        RAISE EXCEPTION _error.
      ENDIF.

    ELSE.
      "... the path or file does NOT exist
      "File &1&2&3 could not be opened or does not exist
      _message = directory_hdlr->path_file.
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>open_not_possible
          mv_msgty = zcx_ca_file_utility=>c_msgty_e
          mv_msgv1 = _message(50)
          mv_msgv2 = _message+50(50)
          mv_msgv3 = _message+100(50).
    ENDIF.
  ENDMETHOD.                    "check_result_of_open_dataset


  METHOD close_dataset.
    "---------------------------------------------------------------------*
    "     Close dataset
    "---------------------------------------------------------------------*
    TRY.
        "May implement a locking logic for appl. server files -> further details are here:
        "https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abenfile_interface_locking.htm

        CLOSE DATASET directory_hdlr->path_file.

      CATCH cx_sy_file_access_error INTO DATA(_catched).
        DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = _catched ) )  ##no_text.
        IF _error IS BOUND.
          RAISE EXCEPTION _error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "close_dataset


  METHOD constructor.
    "---------------------------------------------------------------------*
    "     Constructor
    "---------------------------------------------------------------------*
    super->constructor( ).
    directory_hdlr = zcl_ca_directory_handler=>get_instance( cvc_file_util->location-server ).
  ENDMETHOD.                    "Constructor


  METHOD get_records_length.
    "---------------------------------------------------------------------*
    "     Determine structure / field length of a record
    "---------------------------------------------------------------------*
    cvc_file_util->is_mode_valid( iv_file_mode ).

    CASE iv_file_mode.
      WHEN cvc_file_util->mode-binary.
        rv_rec_len = xstrlen( is_record ).

      WHEN cvc_file_util->mode-text.
        rv_rec_len = strlen( is_record ).
    ENDCASE.
  ENDMETHOD.                    "get_records_length


  METHOD open_dataset.
    "---------------------------------------------------------------------*
    "     Open dataset
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      _message       TYPE bapi_msg,
      _x_file_access TYPE REF TO zcx_ca_file_utility.

    TRY.
        IF directory_hdlr->location NE cvc_file_util->location-server.
          "Method &1 is not allowed at the client/PC
          RAISE EXCEPTION TYPE zcx_ca_file_utility
            EXPORTING
              textid   = zcx_ca_file_utility=>not_allowed_for_client
              mv_msgty = zcx_ca_file_utility=>c_msgty_e
              mv_msgv1 = 'OPEN_DATASET' ##no_text.
        ENDIF.

        cvc_file_util->is_mode_valid( iv_file_mode ).
        cvc_file_util->is_operation_valid( iv_file_operation ).

        directory_hdlr->is_path_file_available( iv_path_file ).

        authority_check( iv_file_operation = iv_file_operation
                         iv_check_auth     = iv_check_auth ).

        "May implement a locking logic for appl. server files -> further details are here:
        "https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abenfile_interface_locking.htm

        CASE iv_file_mode.
          WHEN cvc_file_util->mode-binary.
            "Open for  b i n a r y   m o d e
            CASE iv_file_operation.
              WHEN cvc_file_util->operation-input.
                "Read or delete a file
                OPEN DATASET directory_hdlr->path_file FOR INPUT
                                                       IN BINARY MODE
                                                       MESSAGE _message.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = _message ).
                ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-input.

              WHEN cvc_file_util->operation-output.
                "Create a new file
                OPEN DATASET directory_hdlr->path_file FOR OUTPUT
                                                       IN BINARY MODE
                                                       MESSAGE _message.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = _message ).
                ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-output.

              WHEN cvc_file_util->operation-append.
                "Append new records to existing file or create a new file
                OPEN DATASET directory_hdlr->path_file FOR APPENDING
                                                       IN BINARY MODE
                                                       MESSAGE _message.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = _message ).
                ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-append.

              WHEN cvc_file_util->operation-update.
                "Change a existing file
                OPEN DATASET directory_hdlr->path_file FOR APPENDING
                                                       IN BINARY MODE
                                                       MESSAGE _message.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = _message ).
                ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-update.
            ENDCASE.

          WHEN cvc_file_util->mode-text.
            "Open for  t e x t   m o d e
            "Since the encoding of DEFAULT is equal to UTF-8 and NON-UNICODE is nearly
            "nowhere in use anymore. Due to this assumption files will always be opened
            "in UTF-8 mode and the passed codepage must be compatible to this.

            "SAP-Codepage ist eine UTF-8-Codepage?
            IF iv_codepage IS NOT INITIAL AND
               |{ cl_abap_codepage=>sap_to_http( iv_codepage ) CASE = UPPER }| NP 'UTF+8' ##no_text.
              "The passed SAP codepage &1 is no UTF-8 codepage
              RAISE EXCEPTION TYPE zcx_ca_file_utility
                EXPORTING
                  textid   = zcx_ca_file_utility=>no_utf8_codepage
                  mv_msgty = zcx_ca_file_utility=>c_msgty_e
                  mv_msgv1 = CONV #( iv_codepage ).
            ENDIF.

            "Details for text mode files -> https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencodepage_file_guidl.htm
            CASE iv_file_operation.
              WHEN cvc_file_util->operation-input.
                "Read or delete a file
                OPEN DATASET directory_hdlr->path_file FOR INPUT
                                                       IN TEXT MODE ENCODING UTF-8 SKIPPING BYTE-ORDER MARK
                                                       MESSAGE _message.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = _message ).
                ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-input.

              WHEN cvc_file_util->operation-output.
                "Create a new file
                OPEN DATASET directory_hdlr->path_file FOR OUTPUT
                                                       IN TEXT MODE ENCODING UTF-8 WITH BYTE-ORDER MARK
                                                       MESSAGE _message.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = _message ).
                ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-output.

              WHEN cvc_file_util->operation-append.
                "Append new records to existing file or create a new file
                OPEN DATASET directory_hdlr->path_file FOR APPENDING
                                                       IN TEXT MODE ENCODING UTF-8
                                                       MESSAGE _message.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = _message ).
                ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-append.

              WHEN cvc_file_util->operation-update.
                "Change a existing file
                OPEN DATASET directory_hdlr->path_file FOR UPDATE
                                                       IN TEXT MODE ENCODING UTF-8
                                                       MESSAGE _message.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = _message ).
                ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-update.
            ENDCASE.
        ENDCASE.

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_sy_file_access_error
            cx_parameter_invalid INTO DATA(_catched).
        DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = _catched ) )  ##no_text.
        IF _error IS BOUND.
          RAISE EXCEPTION _error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "open_dataset


  METHOD read_dataset.
    "---------------------------------------------------------------------*
    "     Read data from file
    "---------------------------------------------------------------------*
    TRY.
        IF directory_hdlr->location EQ cvc_file_util->location-pc.
          "Method &1 is not allowed at the client/PC
          RAISE EXCEPTION TYPE zcx_ca_file_utility
            EXPORTING
              textid   = zcx_ca_file_utility=>not_allowed_for_client
              mv_msgty = zcx_ca_file_utility=>c_msgty_e
              mv_msgv1 = 'READ_DATASET' ##no_text.
        ENDIF.

        READ DATASET directory_hdlr->path_file INTO es_record
                                               MAXIMUM LENGTH iv_read_with_max_length
                                               ACTUAL  LENGTH ev_actual_length.
        IF sy-subrc NE 0.
          ev_no_more_record = abap_true.
        ELSE.
          ev_no_more_record = abap_false.
        ENDIF.
        "For statistik purposes increase record counters
        ADD 1 TO: record_cnt_per_file_operation-read,
                  zcl_ca_file_handler=>record_cnt_over_all_operations-read.

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_sy_file_access_error INTO DATA(_catched).
        DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = _catched ) )  ##no_text.
        IF _error IS BOUND.
          RAISE EXCEPTION _error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    " read_dataset


  METHOD transfer_dataset.
    "---------------------------------------------------------------------*
    "     Transfer data to file
    "---------------------------------------------------------------------*
    TRY.
*        IF directory_hdlr->location EQ cvc_file_util->location-pc.
*          "Method &1 is not allowed at the client/PC
*          RAISE EXCEPTION TYPE zcx_ca_file_utility
*            EXPORTING
*              textid   = zcx_ca_file_utility=>not_allowed_for_client
*              mv_msgty = zcx_ca_file_utility=>c_msgty_e
*              mv_msgv1 = 'TRANSFER_DATASET' ##no_text.
*        ENDIF.

        IF iv_rec_len LE 0.
          TRANSFER is_record TO directory_hdlr->path_file.
        ELSE.
          TRANSFER is_record TO directory_hdlr->path_file LENGTH iv_rec_len.
        ENDIF.

        ADD 1 TO: record_cnt_per_file_operation-transfer,
                  zcl_ca_file_handler=>record_cnt_over_all_operations-transfer.

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_sy_file_access_error INTO DATA(_catched).
        DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = _catched ) )  ##no_text.
        IF _error IS BOUND.
          RAISE EXCEPTION _error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    " transfer_dataset


  METHOD write_file.
    "---------------------------------------------------------------------*
    "     Write file from application server
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      _record           TYPE REF TO data,
      _record_length    TYPE i,
      _remaining_length TYPE i.

    FIELD-SYMBOLS:
      <_record>       TYPE data,
      <_column_value> TYPE data.

    cvc_file_util->is_mode_valid( iv_file_mode ).

    "Prepare corresponding workarea for transfer
    CREATE DATA _record LIKE LINE OF it_file.
    ASSIGN _record->* TO <_record>.

    "Get length of structure
    _record_length = get_records_length( iv_file_mode = iv_file_mode
                                         is_record    = <_record> ).

    "In binary mode the it is insufficient
    IF iv_file_mode EQ cvc_file_util->mode-binary.
      DATA(_table_columns) = NEW zcl_ca_ddic( iv_data = it_file )->get_component_list( ).

      IF lines( _table_columns ) EQ 1.
        ASSIGN COMPONENT _table_columns[ 1 ]-name OF STRUCTURE <_record> TO <_column_value>.
        _record_length = get_records_length( iv_file_mode = iv_file_mode
                                             is_record    = <_column_value> ).
      ENDIF.
    ENDIF.

    "Write file to server
    _remaining_length = iv_length.
    LOOP AT it_file INTO <_record>.
      IF _remaining_length LT _record_length.
        _record_length = _remaining_length.
      ENDIF.

      IF <_column_value> IS ASSIGNED.
        transfer_dataset( is_record  = <_column_value>
                          iv_rec_len = _record_length ).
      ELSE.
        transfer_dataset( is_record = <_record>
                          iv_rec_len = _record_length ).
      ENDIF.

      _remaining_length = _remaining_length - _record_length.
    ENDLOOP.

    close_dataset( ).
  ENDMETHOD.                    "write_file


  METHOD zif_ca_file_handler~delete.
    "---------------------------------------------------------------------*
    "     Delete
    "---------------------------------------------------------------------*
    TRY.
        directory_hdlr->is_path_file_available( iv_path_file ).

        authority_check( iv_file_operation = cvc_file_util->operation-delete
                         iv_check_auth     = iv_check_auth ).

        DELETE DATASET directory_hdlr->path_file.
        IF sy-subrc NE 0.
          DATA(_path_file_in_char_for_message) = CONV char200( directory_hdlr->path_file ).
          "File &1&2&3 could not be deleted from file system
          RAISE EXCEPTION TYPE zcx_ca_file_utility
            EXPORTING
              textid   = zcx_ca_file_utility=>delete_not_possible
              mv_msgty = zcx_ca_file_utility=>c_msgty_e
              mv_msgv1 = _path_file_in_char_for_message(50)
              mv_msgv2 = _path_file_in_char_for_message+50(50)
              mv_msgv3 = _path_file_in_char_for_message+100(50).
        ENDIF.

      CATCH cx_sy_file_access_error INTO DATA(_catched).
        DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = _catched ) )  ##no_text.
        IF _error IS BOUND.
          RAISE EXCEPTION _error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_file_handler~delete


  METHOD zif_ca_file_handler~read.
    "---------------------------------------------------------------------*
    "     Get file (READ from server / UPLOAD from client PC)
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      _record    TYPE REF TO data.

    FIELD-SYMBOLS:
      <_record>    TYPE data.

    super->read( iv_path_file = iv_path_file
                 iv_file_mode = iv_file_mode ).

    open_dataset( iv_file_mode      = iv_file_mode
                  iv_file_operation = cvc_file_util->operation-input
                  iv_codepage       = iv_codepage
                  iv_check_auth     = iv_check_auth ).

    "Create workarea for outbound table
    CREATE DATA _record LIKE LINE OF et_file.
    ASSIGN _record->* TO <_record>.

    "Read complete file for return
    CLEAR: et_file,
           ev_length.
    DO.
      read_dataset(
              IMPORTING
                es_record    = <_record>
                ev_no_more_record = DATA(lv_no_record)
                ev_actual_length    = DATA(lv_act_length) ).

      "Add actual length to complete length
      ev_length = ev_length + lv_act_length.
      "Append line of file to internal table
      APPEND <_record> TO et_file.

      IF lv_no_record EQ abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    close_dataset( ).
  ENDMETHOD.                    "zif_ca_file_handler~read


  METHOD zif_ca_file_handler~write.
    "---------------------------------------------------------------------*
    "     Write file (TRANSFER to server / DOWNLOAD on client/PC)
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

    TRY.
        open_dataset( iv_path_file      = iv_path_file
                      iv_file_mode      = iv_file_mode
                      iv_file_operation = iv_file_operation
                      iv_codepage       = iv_codepage
                      iv_check_auth     = iv_check_auth ).

        write_file( iv_file_mode = iv_file_mode
                    iv_length    = iv_length
                    it_file      = ct_file ).
        ev_length = iv_length.

      CATCH zcx_ca_param INTO DATA(_catched).
        DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = _catched ) )  ##no_text.
        IF _error IS BOUND.
          RAISE EXCEPTION _error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_file_handler~write

ENDCLASS.
