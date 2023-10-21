*&---------------------------------------------------------------------*
*&  Include           ZCA_FILE_UTIL_SELSCR_FL6
*&---------------------------------------------------------------------*

"!  For details how to use it, please refer to the help of the global class {@link ZCL_CA_FILE_UTILITY_SELSCRCTLR}.
"!  The prepared text elements can also be found there.

*- Specifications for file 6 -----------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK fl6 WITH FRAME TITLE TEXT-fl6.
  PARAMETERS:
    "Location 6
    p_fl6loc TYPE dxlocation     DEFAULT zcl_ca_c_file_utility=>location-server
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND fl6
                                 MODIF ID fl6,
    "Logical or physical name 6
    p_fl6typ TYPE dxfiletyp      DEFAULT zcl_ca_c_file_utility=>path_type-physical
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND ft6
                                 MODIF ID ft6,
    "Path name 6
    p_fl6pth TYPE dxlpath        LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_fp6 ##exists
                                 MODIF ID fp6,
    "File name 6
    p_fl6nam TYPE dxfilename     LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_fn6 ##exists
                                 MODIF ID fn6,
    "File operation type 6
    p_fl6op  TYPE dsetactype     DEFAULT zcl_ca_c_file_utility=>operation-input
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND fo6
                                 MODIF ID fo6,
    "Text or binary mode 6
    p_fl6mod TYPE swr_filetype   DEFAULT zcl_ca_c_file_utility=>mode-binary
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND fm6
                                 MODIF ID fm6.
SELECTION-SCREEN  END   OF BLOCK fl6.
