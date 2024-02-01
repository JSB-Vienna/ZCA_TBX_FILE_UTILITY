*&---------------------------------------------------------------------*
*&  Include           ZCA_FILE_UTIL_SLSCR_4_REUSEFL9
*&---------------------------------------------------------------------*

"!  For details how to use it, please refer to the help of the global class {@link ZCL_CA_FILE_UTIL_SELSCR_CTLR}.
"!  The prepared text elements can also be found there.

*- Specifications for file 9 -----------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK fl9 WITH FRAME TITLE TEXT-fl9.
  PARAMETERS:
    "Location 9
    p_fl9loc TYPE dxlocation     DEFAULT zcl_ca_c_file_utility=>location-server
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND location_9_changed
                                 MODIF ID fl9,
    "Logical or physical name 9
    p_fl9typ TYPE dxfiletyp      DEFAULT zcl_ca_c_file_utility=>type-physical
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND type_9_changed
                                 MODIF ID ft9,
    "Path name 9
    p_fl9pth TYPE dxlpath        LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_path_9 ##exists
                                 MODIF ID fp9,
    "File name 9
    p_fl9nam TYPE dxfilename     LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_file_name_9 ##exists
                                 MODIF ID fn9,
    "File operation type 9
    p_fl9op  TYPE dsetactype     DEFAULT zcl_ca_c_file_utility=>operation-input
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND operation_9_changed
                                 MODIF ID fo9,
    "Text or binary mode 9
    p_fl9mod TYPE swr_filetype   DEFAULT zcl_ca_c_file_utility=>mode-binary
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND mode_9_changed
                                 MODIF ID fm9.
SELECTION-SCREEN  END   OF BLOCK fl9.
