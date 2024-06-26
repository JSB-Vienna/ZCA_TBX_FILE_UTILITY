*&---------------------------------------------------------------------*
*&  Include           ZCA_FILE_UTIL_SLSCR_4_REUSEFL1
*&---------------------------------------------------------------------*

"For details how to use it, please refer to the help of the global class ZCL_CA_FILE_UTIL_SELSCR_CTLR.
"The prepared text elements can also be found there.

* s e l e c t i o n   f i e l d s
*- Specifications for file 1 -----------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK fl1 WITH FRAME TITLE TEXT-fl1.
  PARAMETERS:
    "Location 1
    p_fl1loc TYPE dxlocation     DEFAULT zcl_ca_c_file_utility=>location-server
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND location_1_changed
                                 MODIF ID fl1,
    "Logical or physical name 1
    p_fl1typ TYPE dxfiletyp      DEFAULT zcl_ca_c_file_utility=>type-physical
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND type_1_changed
                                 MODIF ID ft1,
    "Path name 1
    p_fl1pth TYPE dxlpath        LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_path_1 ##exists
                                 MODIF ID fp1,
    "File name 1
    p_fl1nam TYPE dxfilename     LOWER CASE        "Don't make it obligatory here because of the functions!
                                 VISIBLE LENGTH 120
                                 MEMORY ID zca_file_name_1 ##exists
                                 MODIF ID fn1,
    "File operation type 1
    p_fl1op  TYPE dsetactype     DEFAULT zcl_ca_c_file_utility=>operation-input
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND operation_1_changed
                                 MODIF ID fo1,
    "Text or binary mode 1
    p_fl1mod TYPE swr_filetype   DEFAULT zcl_ca_c_file_utility=>mode-binary
                                 OBLIGATORY
                                 AS LISTBOX
                                 VISIBLE LENGTH 30
                                 USER-COMMAND mode_1_changed
                                 MODIF ID fm1.
SELECTION-SCREEN  END   OF BLOCK fl1.
