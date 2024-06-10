*&---------------------------------------------------------------------*
*&  Include           ZCA_FILE_UTIL_SLSCR_4_REUSEFL8
*&---------------------------------------------------------------------*

"For details how to use it, please refer to the help of the global class ZCL_CA_FILE_UTIL_SELSCR_CTLR.
"The prepared text elements can also be found there.

*- Specifications for file 8 -----------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK fl8 WITH FRAME TITLE TEXT-fl8.
  PARAMETERS:
    "Location 8
    p_fl8loc TYPE dxlocation     DEFAULT zcl_ca_c_file_utility=>location-server
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND location_8_changed
                                 MODIF ID fl8,
    "Logical or physical name 8
    p_fl8typ TYPE dxfiletyp      DEFAULT zcl_ca_c_file_utility=>type-physical
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND type_8_changed
                                 MODIF ID ft8,
    "Path name 8
    p_fl8pth TYPE dxlpath        LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_path_8 ##exists
                                 MODIF ID fp8,
    "File name 8
    p_fl8nam TYPE dxfilename     LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_file_name_8 ##exists
                                 MODIF ID fn8,
    "File operation type 8
    p_fl8op  TYPE dsetactype     DEFAULT zcl_ca_c_file_utility=>operation-input
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND operation_8_changed
                                 MODIF ID fo8,
    "Text or binary mode 8
    p_fl8mod TYPE swr_filetype   DEFAULT zcl_ca_c_file_utility=>mode-binary
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND mode_8_changed
                                 MODIF ID fm8.
SELECTION-SCREEN  END   OF BLOCK fl8.
