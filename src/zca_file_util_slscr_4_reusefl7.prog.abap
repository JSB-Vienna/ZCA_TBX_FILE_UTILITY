*&---------------------------------------------------------------------*
*&  Include           ZCA_FILE_UTIL_SLSCR_4_REUSEFL7
*&---------------------------------------------------------------------*

"!  For details how to use it, please refer to the help of the global class {@link ZCL_CA_FILE_UTIL_SELSCR_CTLR}.
"!  The prepared text elements can also be found there.

*- Specifications for file 7 -----------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK fl7 WITH FRAME TITLE TEXT-fl7.
  PARAMETERS:
    "Location 7
    p_fl7loc TYPE dxlocation     DEFAULT zcl_ca_c_file_utility=>location-server
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND location_7_changed
                                 MODIF ID fl7,
    "Logical or physical name 7
    p_fl7typ TYPE dxfiletyp      DEFAULT zcl_ca_c_file_utility=>type-physical
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND type_7_changed
                                 MODIF ID ft7,
    "Path name 7
    p_fl7pth TYPE dxlpath        LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_path_7 ##exists
                                 MODIF ID fp7,
    "File name 7
    p_fl7nam TYPE dxfilename     LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_file_name_7 ##exists
                                 MODIF ID fn7,
    "File operation type 7
    p_fl7op  TYPE dsetactype     DEFAULT zcl_ca_c_file_utility=>operation-input
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND operation_7_changed
                                 MODIF ID fo7,
    "Text or binary mode 7
    p_fl7mod TYPE swr_filetype   DEFAULT zcl_ca_c_file_utility=>mode-binary
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND mode_7_changed
                                 MODIF ID fm7.
SELECTION-SCREEN  END   OF BLOCK fl7.
