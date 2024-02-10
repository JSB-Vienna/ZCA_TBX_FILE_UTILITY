"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for file utility</p>
CLASS zcl_ca_c_file_utility DEFINITION PUBLIC
                                       CREATE PROTECTED.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">File location</p>
      BEGIN OF location,
        "! <p class="shorttext synchronized" lang="en">Location: Application server</p>
        server TYPE dxlocation   VALUE 'A' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Location: Client / PC</p>
        pc     TYPE dxlocation   VALUE 'P' ##no_text,
      END OF location,

      "! <p class="shorttext synchronized" lang="en">Path / file type</p>
      BEGIN OF type,
        "! <p class="shorttext synchronized" lang="en">Path / file type: Physically</p>
        physical TYPE dxfiletyp    VALUE 'P',
        "! <p class="shorttext synchronized" lang="en">Path / file type: Logically</p>
        logical  TYPE dxfiletyp    VALUE 'L',
      END OF type,

      "! <p class="shorttext synchronized" lang="en">File modes</p>
      BEGIN OF mode,
        "! <p class="shorttext synchronized" lang="en">File mode: Binary mode, e. g. BIN</p>
        binary TYPE swr_filetype VALUE 'B' ##no_text,
        "! <p class="shorttext synchronized" lang="en">File mode: Text mode, e. g. ASC</p>
        text   TYPE swr_filetype VALUE 'T' ##no_text,
      END OF mode,

      "! <p class="shorttext synchronized" lang="en">File operations</p>
      BEGIN OF operation,
        "! <p class="shorttext synchronized" lang="en">File operation: Append lines to file (server only)</p>
        append TYPE dsetactype   VALUE 'A' ##no_text,
        "! <p class="shorttext synchronized" lang="en">File operation: Delete file</p>
        delete TYPE dsetactype   VALUE 'D' ##no_text,
        "! <p class="shorttext synchronized" lang="en">File operation: Read / upload file</p>
        input  TYPE dsetactype   VALUE 'I' ##no_text,
        "! <p class="shorttext synchronized" lang="en">File operation: Write / download file</p>
        output TYPE dsetactype   VALUE 'O' ##no_text,
        "! <p class="shorttext synchronized" lang="en">File operation: Change file (server only)</p>
        update TYPE dsetactype   VALUE 'U' ##no_text,
      END OF operation,

      "! <p class="shorttext synchronized" lang="en">Sorting order options for file list</p>
      BEGIN OF list_sorting,
        "! <p class="shorttext synchronized" lang="en">Sorting: By TIME descending and name ascending</p>
        by_date_time_changed TYPE char1        VALUE 'T'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Sorting: By FILE_NAME ascending and time descending</p>
        by_file_name         TYPE char1        VALUE 'N'  ##no_text,
      END OF list_sorting,

      "! <p class="shorttext synchronized" lang="en">Content type: Find directories or files</p>
      BEGIN OF content_type,
        "! <p class="shorttext synchronized" lang="en">Value help with directories only</p>
        directories TYPE zca_d_vht_dirs_files VALUE 'D'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Value help with files only</p>
        files       TYPE zca_d_vht_dirs_files VALUE 'F'  ##no_text,
      END OF content_type,

      "! <p class="shorttext synchronized" lang="en">Selection fields for file x</p>
      BEGIN OF selscr_user_command,
        "! <p class="shorttext synchronized" lang="en">User commands for file 0</p>
        location_0_changed  TYPE syst_ucomm VALUE 'LOCATION_0_CHANGED'  ##NO_TEXT,
        type_0_changed      TYPE syst_ucomm VALUE 'TYPE_0_CHANGED'  ##NO_TEXT,
        operation_0_changed TYPE syst_ucomm VALUE 'OPERATION_0_CHANGED'  ##NO_TEXT,
        mode_0_changed      TYPE syst_ucomm VALUE 'MODE_0_CHANGED'  ##NO_TEXT,
        "! <p class="shorttext synchronized" lang="en">User commands for file 1</p>
        location_1_changed  TYPE syst_ucomm VALUE 'LOCATION_1_CHANGED'  ##NO_TEXT,
        type_1_changed      TYPE syst_ucomm VALUE 'TYPE_1_CHANGED'  ##NO_TEXT,
        operation_1_changed TYPE syst_ucomm VALUE 'OPERATION_1_CHANGED'  ##NO_TEXT,
        mode_1_changed      TYPE syst_ucomm VALUE 'MODE_1_CHANGED'  ##NO_TEXT,
        "! <p class="shorttext synchronized" lang="en">User commands for file 2</p>
        location_2_changed  TYPE syst_ucomm VALUE 'LOCATION_2_CHANGED'  ##NO_TEXT,
        type_2_changed      TYPE syst_ucomm VALUE 'TYPE_2_CHANGED'  ##NO_TEXT,
        operation_2_changed TYPE syst_ucomm VALUE 'OPERATION_2_CHANGED'  ##NO_TEXT,
        mode_2_changed      TYPE syst_ucomm VALUE 'MODE_2_CHANGED'  ##NO_TEXT,
        "! <p class="shorttext synchronized" lang="en">User commands for file 3</p>
        location_3_changed  TYPE syst_ucomm VALUE 'LOCATION_3_CHANGED'  ##NO_TEXT,
        type_3_changed      TYPE syst_ucomm VALUE 'TYPE_3_CHANGED'  ##NO_TEXT,
        operation_3_changed TYPE syst_ucomm VALUE 'OPERATION_3_CHANGED'  ##NO_TEXT,
        mode_3_changed      TYPE syst_ucomm VALUE 'MODE_3_CHANGED'  ##NO_TEXT,
        "! <p class="shorttext synchronized" lang="en">User commands for file 4</p>
        location_4_changed  TYPE syst_ucomm VALUE 'LOCATION_4_CHANGED'  ##NO_TEXT,
        type_4_changed      TYPE syst_ucomm VALUE 'TYPE_4_CHANGED'  ##NO_TEXT,
        operation_4_changed TYPE syst_ucomm VALUE 'OPERATION_4_CHANGED'  ##NO_TEXT,
        mode_4_changed      TYPE syst_ucomm VALUE 'MODE_4_CHANGED'  ##NO_TEXT,
        "! <p class="shorttext synchronized" lang="en">User commands for file 5</p>
        location_5_changed  TYPE syst_ucomm VALUE 'LOCATION_5_CHANGED'  ##NO_TEXT,
        type_5_changed      TYPE syst_ucomm VALUE 'TYPE_5_CHANGED'  ##NO_TEXT,
        operation_5_changed TYPE syst_ucomm VALUE 'OPERATION_5_CHANGED'  ##NO_TEXT,
        mode_5_changed      TYPE syst_ucomm VALUE 'MODE_5_CHANGED'  ##NO_TEXT,
        "! <p class="shorttext synchronized" lang="en">User commands for file 6</p>
        location_6_changed  TYPE syst_ucomm VALUE 'LOCATION_6_CHANGED'  ##NO_TEXT,
        type_6_changed      TYPE syst_ucomm VALUE 'TYPE_6_CHANGED'  ##NO_TEXT,
        operation_6_changed TYPE syst_ucomm VALUE 'OPERATION_6_CHANGED'  ##NO_TEXT,
        mode_6_changed      TYPE syst_ucomm VALUE 'MODE_6_CHANGED'  ##NO_TEXT,
        "! <p class="shorttext synchronized" lang="en">User commands for file 7</p>
        location_7_changed  TYPE syst_ucomm VALUE 'LOCATION_7_CHANGED'  ##NO_TEXT,
        type_7_changed      TYPE syst_ucomm VALUE 'TYPE_7_CHANGED'  ##NO_TEXT,
        operation_7_changed TYPE syst_ucomm VALUE 'OPERATION_7_CHANGED'  ##NO_TEXT,
        mode_7_changed      TYPE syst_ucomm VALUE 'MODE_7_CHANGED'  ##NO_TEXT,
        "! <p class="shorttext synchronized" lang="en">User commands for file 8</p>
        location_8_changed  TYPE syst_ucomm VALUE 'LOCATION_8_CHANGED'  ##NO_TEXT,
        type_8_changed      TYPE syst_ucomm VALUE 'TYPE_8_CHANGED'  ##NO_TEXT,
        operation_8_changed TYPE syst_ucomm VALUE 'OPERATION_8_CHANGED'  ##NO_TEXT,
        mode_8_changed      TYPE syst_ucomm VALUE 'MODE_8_CHANGED'  ##NO_TEXT,
        "! <p class="shorttext synchronized" lang="en">User commands for file 9</p>
        location_9_changed  TYPE syst_ucomm VALUE 'LOCATION_9_CHANGED'  ##NO_TEXT,
        type_9_changed      TYPE syst_ucomm VALUE 'TYPE_9_CHANGED'  ##NO_TEXT,
        operation_9_changed TYPE syst_ucomm VALUE 'OPERATION_9_CHANGED'  ##NO_TEXT,
        mode_9_changed      TYPE syst_ucomm VALUE 'MODE_9_CHANGED'  ##NO_TEXT,
      END OF selscr_user_command,

      "! <p class="shorttext synchronized" lang="en">Selection fields for file x</p>
      BEGIN OF selection_field_id,
        "! <p class="shorttext synchronized" lang="en">Use selection fields for file 0</p>
        for_file_0 TYPE num1 VALUE '0'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Use selection fields for file 1</p>
        for_file_1 TYPE num1 VALUE '1'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Use selection fields for file 2</p>
        for_file_2 TYPE num1 VALUE '2'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Use selection fields for file 3</p>
        for_file_3 TYPE num1 VALUE '3'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Use selection fields for file 4</p>
        for_file_4 TYPE num1 VALUE '4'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Use selection fields for file 5</p>
        for_file_5 TYPE num1 VALUE '5'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Use selection fields for file 6</p>
        for_file_6 TYPE num1 VALUE '6'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Use selection fields for file 7</p>
        for_file_7 TYPE num1 VALUE '7'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Use selection fields for file 8</p>
        for_file_8 TYPE num1 VALUE '8'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Use selection fields for file 9</p>
        for_file_9 TYPE num1 VALUE '9'  ##no_text,
      END OF selection_field_id.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_file_utility.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Valid file location passed?</p>
      "!
      "! @parameter location            | <p class="shorttext synchronized" lang="en">File location</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      is_location_valid FINAL
        IMPORTING
          location TYPE dxlocation
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Valid path / file type passed?</p>
      "!
      "! @parameter type                | <p class="shorttext synchronized" lang="en">Path / file type</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      is_type_valid FINAL
        IMPORTING
          type TYPE dxfiletyp
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Valid file mode passed?</p>
      "!
      "! @parameter file_mode           | <p class="shorttext synchronized" lang="en">File mode</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      is_mode_valid FINAL
        IMPORTING
          file_mode TYPE swr_filetype
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Valid file operation passed?</p>
      "!
      "! @parameter operation           | <p class="shorttext synchronized" lang="en">File operation</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      is_operation_valid FINAL
        IMPORTING
          operation TYPE dsetactype
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Valid list_sorting option passed?</p>
      "!
      "! @parameter list_sorting        | <p class="shorttext synchronized" lang="en">List sorting option</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      is_list_sorting_valid FINAL
        IMPORTING
          list_sorting TYPE char1
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Valid content type passed?</p>
      "!
      "! @parameter content_type        | <p class="shorttext synchronized" lang="en">Content type</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      is_content_type_valid FINAL
        IMPORTING
          content_type TYPE zca_d_vht_dirs_files
        RAISING
          zcx_ca_file_utility.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check value against fixed_values</p>
      "!
      "! @parameter value               | <p class="shorttext synchronized" lang="en">Value under test</p>
      "! @parameter param_name          | <p class="shorttext synchronized" lang="en">Name of field/parameter for output in error message</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      check_against_fixed_values
        IMPORTING
          value      TYPE simple
          param_name TYPE csequence
        RAISING
          zcx_ca_file_utility.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_file_utility.

ENDCLASS.



CLASS zcl_ca_c_file_utility IMPLEMENTATION.

  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_file_utility=>singleton_instance IS NOT BOUND.
      zcl_ca_c_file_utility=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_file_utility=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD is_location_valid.
    "-----------------------------------------------------------------*
    "   Valid file location passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = location
                                param_name = 'LOCATION' ) ##no_text.
  ENDMETHOD.                    "is_location_valid


  METHOD is_type_valid.
    "-----------------------------------------------------------------*
    "   Valid path / file type passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = type
                                param_name = 'PATH_TYPE' ) ##no_text.
  ENDMETHOD.                    "is_path_type_valid


  METHOD is_mode_valid.
    "-----------------------------------------------------------------*
    "   Valid file mode passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = file_mode
                                param_name = 'FILE_MODE' ) ##no_text.
  ENDMETHOD.                    "is_mode_valid


  METHOD is_operation_valid.
    "-----------------------------------------------------------------*
    "   Valid file operation passed?
    "-----------------------------------------------------------------*
    "Since the standard domain contains only 4 of 5 possible values,
    "it is not checked via the fixed value approach.
    IF operation CN 'ADIOU' ##no_text.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'OPERATION'
          mv_msgv2 = CONV #( operation ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_operation_valid


  METHOD is_list_sorting_valid.
    "-----------------------------------------------------------------*
    "   Valid list_sorting option passed?
    "-----------------------------------------------------------------*
    IF list_sorting CN 'TN' ##no_text.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'LIST_SORTING'
          mv_msgv2 = CONV #( list_sorting ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_list_sorting_valid


  METHOD is_content_type_valid.
    "-----------------------------------------------------------------*
    "   Valid content type passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = content_type
                                param_name = 'CONTENT_TYPE' ) ##no_text.
  ENDMETHOD.                    "is_value_help_type_valid


  METHOD check_against_fixed_values.
    "-----------------------------------------------------------------*
    "   Check value against fixed_values
    "-----------------------------------------------------------------*
    TRY.
        NEW zcl_ca_ddic( iv_data       = value
                         iv_param_name = param_name )->check_fixed_values( iv_value       = value
                                                                           iv_raise_excep = abap_true ).

      CATCH zcx_ca_param INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_file_utility( lx_catched ).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.                    "check_against_fixed_values

ENDCLASS.
