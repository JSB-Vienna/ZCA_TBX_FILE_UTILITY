*&---------------------------------------------------------------------*
*&  Include           ZCA_FILE_UTIL_SLSCR_4_REUSEFL0
*&---------------------------------------------------------------------*

"!  For details how to use it, please refer to the help of the global class {@link ZCL_CA_FILE_UTIL_SELSCR_CTLR}.
"!  The prepared text elements can also be found there.

*- Specifications for file 0 -----------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK fl0 WITH FRAME TITLE TEXT-fl0.
  PARAMETERS:
    "Location 0
    p_fl0loc TYPE dxlocation     DEFAULT zcl_ca_c_file_utility=>location-server
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND location_0_changed
                                 MODIF ID fl0,
    "Logical or physical name 0
    p_fl0typ TYPE dxfiletyp      DEFAULT zcl_ca_c_file_utility=>type-physical
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND type_0_changed
                                 MODIF ID ft0,
    "Path name 0
    p_fl0pth TYPE dxlpath        LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_path_0 ##exists
                                 MODIF ID fp0,
    "File name 0
    p_fl0nam TYPE dxfilename     LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_file_name_0 ##exists
                                 MODIF ID fn0,
    "File operation type 0
    p_fl0op  TYPE dsetactype     DEFAULT zcl_ca_c_file_utility=>operation-input
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND operation_0_changed
                                 MODIF ID fo0,
    "Text or binary mode 0
    p_fl0mod TYPE swr_filetype   DEFAULT zcl_ca_c_file_utility=>mode-binary
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND mode_0_changed
                                 MODIF ID fm0.
SELECTION-SCREEN  END   OF BLOCK fl0.
