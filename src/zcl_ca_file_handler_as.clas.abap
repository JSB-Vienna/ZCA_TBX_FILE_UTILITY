"! <p class="shorttext synchronized" lang="en">CA-TBX: File utility for application server</p>
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
      "! @parameter result              | <p class="shorttext synchronized" lang="en">File length</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      close_dataset
        RETURNING
          VALUE(result) TYPE i
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Open dataset</p>
      "!
      "! @parameter codepage            | <p class="shorttext synchronized" lang="en">Codepage (can use CODEPAGE as default, see TCP00)</p>
      "! @parameter check_authority     | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      open_dataset
        IMPORTING
          codepage        TYPE cpcodepage   OPTIONAL
          check_authority TYPE abap_boolean    DEFAULT abap_true
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Read data from file and write statistics</p>
      "!
      "! @parameter max_file_length     | <p class="shorttext synchronized" lang="en">Max. length of the file to read (lower 0 = complete file)</p>
      "! @parameter record              | <p class="shorttext synchronized" lang="en">Record for file</p>
      "! @parameter length_last_record  | <p class="shorttext synchronized" lang="en">Actual length of last record</p>
      "! @parameter no_more_records     | <p class="shorttext synchronized" lang="en">X = No more records available (leave loop)</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      read_dataset
        IMPORTING
          max_file_length    TYPE i DEFAULT -1
        EXPORTING
          record             TYPE data
          length_last_record TYPE i
          no_more_records    TYPE abap_boolean
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Transfer data to file and write statistics</p>
      "!
      "! @parameter record              | <p class="shorttext synchronized" lang="en">Record for file</p>
      "! @parameter max_file_length     | <p class="shorttext synchronized" lang="en">Max. length of the file to write (lower 0 = complete file)</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      transfer_dataset
        IMPORTING
          record          TYPE data
          max_file_length TYPE i DEFAULT -1
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
      "! @parameter check_authority     | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      authority_check
        IMPORTING
          check_authority TYPE abap_boolean
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Check result of OPEN DATASET and raise exception</p>
      "!
      "! @parameter return_code         | <p class="shorttext synchronized" lang="en">Return code of OPEN DATASET statement</p>
      "! @parameter message             | <p class="shorttext synchronized" lang="en">Message string of OPEN DATASET statement</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      check_result_of_open_dataset
        IMPORTING
          VALUE(return_code) TYPE syst_subrc
          message            TYPE bapi_msg
        RAISING
          zcx_ca_file_utility,

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

      "! <p class="shorttext synchronized" lang="en">Determine structure / field length of a record</p>
      "!
      "! @parameter record              | <p class="shorttext synchronized" lang="en">Data object to determine length</p>
      "! @parameter result              | <p class="shorttext synchronized" lang="en">Transmitted length</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      determine_record_length
        IMPORTING
          record        TYPE data
        RETURNING
          VALUE(result) TYPE i
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Write file from application server</p>
      "!
      "! @parameter max_file_length     | <p class="shorttext synchronized" lang="en">Max. length of the file to write (lower 0 = complete file)</p>
      "! @parameter file                | <p class="shorttext synchronized" lang="en">File as table</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      write_file
        IMPORTING
          max_file_length TYPE i
          file            TYPE STANDARD TABLE
        RAISING
          zcx_ca_file_utility
          zcx_ca_param.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Open dataset for binary format</p>
      open_dataset_4_binary_format
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Open dataset for text format</p>
      open_dataset_4_text_format
        RAISING
          zcx_ca_file_utility.

ENDCLASS.



CLASS zcl_ca_file_handler_as IMPLEMENTATION.

  METHOD authority_check.
    "---------------------------------------------------------------------*
    "     Check authority of path and file
    "---------------------------------------------------------------------*
    IF check_authority EQ abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
      EXPORTING
        filename         = CONV fileextern( processing_params-path_file )
        activity         = SWITCH char20(
                              processing_params-operation
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
        DATA(_path_file) = CONV bapi_msg( processing_params-path_file ).
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

    IF return_code EQ 0.
      RETURN.
    ENDIF.

    "Opening the requested file results in an error

    "Either send the message of the OPEN DATASET statement or ...
    IF message IS NOT INITIAL.
      "To fill SY-MSG++ fields for exception creation
      _message = |{ message } / => { processing_params-path_file }|.
      MESSAGE e897(s1) WITH _message(50)     _message+50(50)
                            _message+100(50) _message+150(50) INTO _message.
      DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'ZCL_CA_FILE_UTIL'
                                                        iv_method   = 'OPEN'
                                                        iv_subrc    = return_code ) ) ##no_text.
      IF _error IS BOUND.
        RAISE EXCEPTION _error.
      ENDIF.

    ELSE.
      "... the path or file does NOT exist
      "File &1&2&3 could not be opened or does not exist
      _message = processing_params-path_file.
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
        GET DATASET processing_params-path_file POSITION file_length.
        result = file_length.

        "May implement a locking logic for appl. server files -> further details are here:
        "https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abenfile_interface_locking.htm

        CLOSE DATASET processing_params-path_file.

      CATCH cx_sy_conversion_error
            cx_sy_file_access_error INTO DATA(_catched).
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
    super->constructor( processing_params = processing_params
                        directory_entry   = directory_entry ).
    directory_hdlr = zcl_ca_directory_handler=>get_instance( cvc_file_util->location-server ).
  ENDMETHOD.                    "Constructor


  METHOD determine_record_length.
    "---------------------------------------------------------------------*
    "     Determine structure / field length of a record
    "---------------------------------------------------------------------*
    CASE processing_params-mode.
      WHEN cvc_file_util->mode-binary.
        result = xstrlen( record ).

      WHEN cvc_file_util->mode-text.
        result = strlen( record ).
    ENDCASE.
  ENDMETHOD.                    "get_record_length


  METHOD open_dataset.
    "---------------------------------------------------------------------*
    "     Open dataset
    "---------------------------------------------------------------------*
    TRY.
        authority_check( check_authority ).

        "May implement a locking logic for appl. server files -> further details are here:
        "https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abenfile_interface_locking.htm

        CASE processing_params-mode.
          WHEN cvc_file_util->mode-binary.
            open_dataset_4_binary_format( ).

          WHEN cvc_file_util->mode-text.
            "The encoding of DEFAULT is equal to UTF-8 and NON-UNICODE is nearly nowhere in use anymore. Due
            "to this assumption files will always be opened in UTF-8 mode and the passed code page must be
            "compatible to this.

            "SAP-Codepage ist eine UTF-8-Codepage?
            IF codepage IS NOT INITIAL AND
               |{ cl_abap_codepage=>sap_to_http( codepage ) CASE = UPPER }| NP 'UTF+8' ##no_text.
              "The passed SAP codepage &1 is no UTF-8 codepage
              RAISE EXCEPTION TYPE zcx_ca_file_utility
                EXPORTING
                  textid   = zcx_ca_file_utility=>no_utf8_codepage
                  mv_msgty = zcx_ca_file_utility=>c_msgty_e
                  mv_msgv1 = CONV #( codepage ).
            ENDIF.

            open_dataset_4_text_format( ).
        ENDCASE.

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_error
            cx_sy_file_access_error INTO DATA(_catched).
*            cx_parameter_invalid INTO DATA(_catched).
        DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = _catched ) )  ##no_text.
        IF _error IS BOUND.
          RAISE EXCEPTION _error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "open_dataset


  METHOD open_dataset_4_binary_format.
    "---------------------------------------------------------------------*
    "     Open dataset for binary format
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      _message             TYPE bapi_msg.

    "Open for  b i n a r y   m o d e
    CASE processing_params-operation.
      WHEN cvc_file_util->operation-input.
        "Read or delete a file
        OPEN DATASET processing_params-path_file FOR INPUT
                                                 IN BINARY MODE
                                                 MESSAGE _message.
        check_result_of_open_dataset( return_code = sy-subrc
                                      message     = _message ).
        ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-input.

      WHEN cvc_file_util->operation-output.
        "Create a new file
        OPEN DATASET processing_params-path_file FOR OUTPUT
                                                 IN BINARY MODE
                                                 MESSAGE _message.
        check_result_of_open_dataset( return_code = sy-subrc
                                      message     = _message ).
        ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-output.

      WHEN cvc_file_util->operation-append.
        "Append new records to existing file or create a new file
        OPEN DATASET processing_params-path_file FOR APPENDING
                                                 IN BINARY MODE
                                                 MESSAGE _message.
        check_result_of_open_dataset( return_code = sy-subrc
                                      message     = _message ).
        ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-append.

      WHEN cvc_file_util->operation-update.
        "Change a existing file
        OPEN DATASET processing_params-path_file FOR APPENDING
                                                 IN BINARY MODE
                                                 MESSAGE _message.
        check_result_of_open_dataset( return_code = sy-subrc
                                      message     = _message ).
        ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-update.
    ENDCASE.
  ENDMETHOD.                    "open_dataset_4_binary_format


  METHOD open_dataset_4_text_format.
    "---------------------------------------------------------------------*
    "     Open dataset for text format
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      _message             TYPE bapi_msg.

    "Details for text mode files -> https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencodepage_file_guidl.htm
    CASE processing_params-operation.
      WHEN cvc_file_util->operation-input.
        "Read or delete a file
        OPEN DATASET processing_params-path_file FOR INPUT
                                                 IN TEXT MODE ENCODING UTF-8 SKIPPING BYTE-ORDER MARK
                                                 MESSAGE _message.
        check_result_of_open_dataset( return_code = sy-subrc
                                      message     = _message ).
        ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-input.

      WHEN cvc_file_util->operation-output.
        "Create a new file
        OPEN DATASET processing_params-path_file FOR OUTPUT
                                                 IN TEXT MODE ENCODING UTF-8 WITH BYTE-ORDER MARK
                                                 MESSAGE _message.
        check_result_of_open_dataset( return_code = sy-subrc
                                      message     = _message ).
        ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-output.

      WHEN cvc_file_util->operation-append.
        "Append new records to existing file or create a new file
        OPEN DATASET processing_params-path_file FOR APPENDING
                                                 IN TEXT MODE ENCODING UTF-8
                                                 MESSAGE _message.
        check_result_of_open_dataset( return_code = sy-subrc
                                      message     = _message ).
        ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-append.

      WHEN cvc_file_util->operation-update.
        "Change a existing file
        OPEN DATASET processing_params-path_file FOR UPDATE
                                                 IN TEXT MODE ENCODING UTF-8
                                                 MESSAGE _message.
        check_result_of_open_dataset( return_code = sy-subrc
                                      message     = _message ).
        ADD 1 TO zcl_ca_file_handler=>dataset_counter_per_operation-update.
    ENDCASE.
  ENDMETHOD.                    "open_dataset_4_text_format


  METHOD read_dataset.
    "---------------------------------------------------------------------*
    "     Read data from file
    "---------------------------------------------------------------------*
    TRY.
        READ DATASET processing_params-path_file INTO record
                                                 MAXIMUM LENGTH max_file_length
                                                 ACTUAL  LENGTH length_last_record.
        IF sy-subrc NE 0.
          no_more_records = abap_true.
        ELSE.
          no_more_records = abap_false.
          "For statistik purposes increase record counters
          ADD 1 TO: record_cnt_per_file_operation-read,
                    zcl_ca_file_handler=>record_cnt_over_all_operations-read.
        ENDIF.

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_error
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
        IF max_file_length LE 0.
          TRANSFER record TO processing_params-path_file.
        ELSE.
          TRANSFER record TO processing_params-path_file LENGTH max_file_length.
        ENDIF.

        ADD 1 TO: record_cnt_per_file_operation-transfer,
                  zcl_ca_file_handler=>record_cnt_over_all_operations-transfer.

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_error
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
      _record              TYPE REF TO data.

    FIELD-SYMBOLS:
      <_record>            TYPE data.

    "Prepare corresponding workarea for transfer
    CREATE DATA _record LIKE LINE OF file.
    ASSIGN _record->* TO <_record>.

    "Write data into file on the server
    LOOP AT file INTO <_record>.
      transfer_dataset( record          = <_record>
                        max_file_length = max_file_length ).
    ENDLOOP.
  ENDMETHOD.                    "write_file


  METHOD zif_ca_file_handler~delete.
    "---------------------------------------------------------------------*
    "     Delete file
    "---------------------------------------------------------------------*
    TRY.
        authority_check( check_authority ).

        DELETE DATASET processing_params-path_file.
        IF sy-subrc NE 0.
          DATA(_path_file_in_char_for_message) = CONV char200( processing_params-path_file ).
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
    "     Read entire file
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      _record    TYPE REF TO data.

    FIELD-SYMBOLS:
      <_record>    TYPE data.

    CLEAR: file,
           length_of_file.

    IF processing_params-operation NE cvc_file_util->operation-input.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>param_invalid
          mv_msgty = zcx_ca_file_utility=>c_msgty_e
          mv_msgv1 = 'PROCESSING_PARAMS-OPERATION'
          mv_msgv2 = CONV #( processing_params-operation ) ##no_text.
    ENDIF.

    open_dataset( codepage        = codepage
                  check_authority = check_authority ).

    "Create workarea for outbound file
    CREATE DATA _record LIKE LINE OF file.
    ASSIGN _record->* TO <_record>.

    DO.
      read_dataset(
              IMPORTING
                record          = <_record>
                no_more_records = DATA(_no_more_records) ).

      APPEND <_record> TO file.

      IF _no_more_records EQ abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    length_of_file = close_dataset( ).
  ENDMETHOD.                    "zif_ca_file_handler~read


  METHOD zif_ca_file_handler~write.
    "---------------------------------------------------------------------*
    "     Write entire file
    "---------------------------------------------------------------------*
    CLEAR length_of_file.
    IF processing_params-operation EQ cvc_file_util->operation-input.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>param_invalid
          mv_msgty = zcx_ca_file_utility=>c_msgty_e
          mv_msgv1 = 'PROCESSING_PARAMS-OPERATION'
          mv_msgv2 = CONV #( processing_params-operation ) ##no_text.
    ENDIF.

    TRY.
        open_dataset( codepage        = codepage
                      check_authority = check_authority ).

        write_file( max_file_length = max_file_length
                    file            = file ).

        length_of_file = close_dataset( ).

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
