*&---------------------------------------------------------------------*
*&  Include           ZCA_FILE_UTIL_SELSCR_FL5
*&---------------------------------------------------------------------*

"!  For details how to use it, please refer to the help of the global class {@link ZCL_CA_FILE_UTILITY_SELSCRCTLR}.
"!  The prepared text elements can also be found there.

*- Specifications for file 5 -----------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK fl5 WITH FRAME TITLE TEXT-fl5.
  PARAMETERS:
    "Location 5
    p_fl5loc TYPE dxlocation     DEFAULT zcl_ca_c_file_utility=>location-server
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND fl5
                                 MODIF ID fl5,
    "Logical or physical name 5
    p_fl5typ TYPE dxfiletyp      DEFAULT zcl_ca_c_file_utility=>path_type-physical
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND ft5
                                 MODIF ID ft5,
    "Path name 5
    p_fl5pth TYPE dxlpath        LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_fp5 ##exists
                                 MODIF ID fp5,
    "File name 5
    p_fl5nam TYPE dxfilename     LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_fn5 ##exists
                                 MODIF ID fn5,
    "File operation type 5
    p_fl5op  TYPE dsetactype     DEFAULT zcl_ca_c_file_utility=>operation-input
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND fo5
                                 MODIF ID fo5,
    "Text or binary mode 5
    p_fl5mod TYPE swr_filetype   DEFAULT zcl_ca_c_file_utility=>mode-binary
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND fm5
                                 MODIF ID fm5.
SELECTION-SCREEN  END   OF BLOCK fl5.
