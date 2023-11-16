*&---------------------------------------------------------------------*
*&  Include           ZCA_FILE_UTIL_SELSCR_FL3
*&---------------------------------------------------------------------*

"!  For details how to use it, please refer to the help of the global class {@link ZCL_CA_FILE_UTILITY_SELSCRCTLR}.
"!  The prepared text elements can also be found there.

*- Specifications for file 3 -----------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK fl3 WITH FRAME TITLE TEXT-fl3.
  PARAMETERS:
    "Location 3
    p_fl3loc TYPE dxlocation     DEFAULT zcl_ca_c_file_utility=>location-server
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND location_3_changed
                                 MODIF ID fl3,
    "Logical or physical name 3
    p_fl3typ TYPE dxfiletyp      DEFAULT zcl_ca_c_file_utility=>path_type-physical
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND pathtype_3_changed
                                 MODIF ID ft3,
    "Path name 3
    p_fl3pth TYPE dxlpath        LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_path_3 ##exists
                                 MODIF ID fp3,
    "File name 3
    p_fl3nam TYPE dxfilename     LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_file_name_3 ##exists
                                 MODIF ID fn3,
    "File operation type 3
    p_fl3op  TYPE dsetactype     DEFAULT zcl_ca_c_file_utility=>operation-input
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND operation_3_changed
                                 MODIF ID fo3,
    "Text or binary mode 3
    p_fl3mod TYPE swr_filetype   DEFAULT zcl_ca_c_file_utility=>mode-binary
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND mode_3_changed
                                 MODIF ID fm3.
SELECTION-SCREEN  END   OF BLOCK fl3.
