*&---------------------------------------------------------------------*
*&  Include           LZCA_LOGD01
*&---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*     CLASS  lcl_ucomm_handler  DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_ucomm_handler DEFINITION CREATE PUBLIC
                                   FINAL.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Display error position in source code</p>
      "!
      "! @parameter is_uc_data | <p class="shorttext synchronized" lang="en">Application Log: 'USER_COMMAND' callback data</p>
      display_errpos_in_source_code
        IMPORTING
          is_uc_data TYPE bal_s_cbuc.
ENDCLASS.                    " lcl_ucomm_handler  DEFINITION
