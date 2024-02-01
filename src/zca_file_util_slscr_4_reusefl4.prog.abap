*&---------------------------------------------------------------------*
*&  Include           ZCA_FILE_UTIL_SLSCR_4_REUSEFL4
*&---------------------------------------------------------------------*

"!  For details how to use it, please refer to the help of the global class {@link ZCL_CA_FILE_UTIL_SELSCR_CTLR}.
"!  The prepared text elements can also be found there.

*- Specifications for file 4 -----------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK fl4 WITH FRAME TITLE TEXT-fl4.
  PARAMETERS:
    "Location 4
    p_fl4loc TYPE dxlocation     DEFAULT zcl_ca_c_file_utility=>location-server
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND location_4_changed
                                 MODIF ID fl4,
    "Logical or physical name 4
    p_fl4typ TYPE dxfiletyp      DEFAULT zcl_ca_c_file_utility=>type-physical
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND type_4_changed
                                 MODIF ID ft4,
    "Path name 4
    p_fl4pth TYPE dxlpath        LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_path_4 ##exists
                                 MODIF ID fp4,
    "File name 4
    p_fl4nam TYPE dxfilename     LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_file_name_4 ##exists
                                 MODIF ID fn4,
    "File operation type 4
    p_fl4op  TYPE dsetactype     DEFAULT zcl_ca_c_file_utility=>operation-input
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND operation_4_changed
                                 MODIF ID fo4,
    "Text or binary mode 4
    p_fl4mod TYPE swr_filetype   DEFAULT zcl_ca_c_file_utility=>mode-binary
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND mode_4_changed
                                 MODIF ID fm4.
SELECTION-SCREEN  END   OF BLOCK fl4.
