*&---------------------------------------------------------------------*
*&  Include           ZCA_FILE_UTIL_SLSCR_4_REUSEFL2
*&---------------------------------------------------------------------*

"!  For details how to use it, please refer to the help of the global class {@link ZCL_CA_FILE_UTIL_SELSCR_CTLR}.
"!  The prepared text elements can also be found there.

*- Specifications for file 2 -----------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK fl2 WITH FRAME TITLE TEXT-fl2.
  PARAMETERS:
    "Location 2
    p_fl2loc TYPE dxlocation     DEFAULT zcl_ca_c_file_utility=>location-server
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND location_2_changed
                                 MODIF ID fl2,
    "Logical or physical name 2
    p_fl2typ TYPE dxfiletyp      DEFAULT zcl_ca_c_file_utility=>type-physical
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND type_2_changed
                                 MODIF ID ft2,
    "Path name 2
    p_fl2pth TYPE dxlpath        LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_path_2 ##exists
                                 MODIF ID fp2,
    "File name 2
    p_fl2nam TYPE dxfilename     LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_file_name_2 ##exists
                                 MODIF ID fn2,
    "File operation type 2
    p_fl2op  TYPE dsetactype     DEFAULT zcl_ca_c_file_utility=>operation-input
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND operation_2_changed
                                 MODIF ID fo2,
    "Text or binary mode 2
    p_fl2mod TYPE swr_filetype   DEFAULT zcl_ca_c_file_utility=>mode-binary
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND mode_2_changed
                                 MODIF ID fm2.
SELECTION-SCREEN  END   OF BLOCK fl2.
