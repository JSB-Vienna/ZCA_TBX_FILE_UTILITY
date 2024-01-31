"! <p class="shorttext synchronized" lang="en">Demo object how to use class ZCL_FILE_UTILITY incl. sel. screen</p>
REPORT zca_demo_file_utility.

* t a b l e s   /   s t r u c t u r e s   for selection field definition
TABLES:
  sscrfields.            "Fields on selection screens


* s e l e c t i o n   f i e l d s
*- Scenario selection ------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK sce WITH FRAME TITLE TEXT-sce.
  SELECTION-SCREEN  BEGIN OF LINE.
    PARAMETERS
      "Copy archived file to appl. server or PC
      p_rbcaas       RADIOBUTTON GROUP sce  DEFAULT 'X'
                                            USER-COMMAND sce_sel.
    SELECTION-SCREEN COMMENT 03(70) TEXT-cas.
  SELECTION-SCREEN END OF LINE.


  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS
      "Copy file from local documents folder to appl. server
      p_rbcdas       RADIOBUTTON GROUP sce.
    SELECTION-SCREEN COMMENT 03(70) TEXT-cda.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK sce.



"! <p>Please transfer text elements mentioned in the documentation of the global
"! class {@link ZCL_CA_FILE_UTILITY_SELSCRCTLR} into your report.</p>
INCLUDE zca_demo_file_utilityfl1.



*- Copy archived files of ... ----------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK caf WITH FRAME TITLE TEXT-caf.
  PARAMETERS:
    "Name of Business Object type
    p_typeid TYPE swo_objtyp           MATCHCODE OBJECT h_tojtb
                                       MODIF ID caf,
    "Key of object instance
    p_instid TYPE sibfboriid           MODIF ID caf.
SELECTION-SCREEN END OF BLOCK caf.


"! <p>Please transfer text elements mentioned in the documentation of the global
"! class {@link ZCL_CA_FILE_UTILITY_SELSCRCTLR} into your report.</p>
INCLUDE zca_demo_file_utilityfl2.



"! <p class="shorttext synchronized" lang="en">Demonstrating usage of class ZCL_CA_FILE_UTILITY</p>
CLASS demo_usage_file_utility DEFINITION FINAL
                                         CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Initialization of report data / selections</p>
      constructor,

      "! <p class="shorttext synchronized" lang="en">Control / adjust selection screen fields</p>
      at_sel_screen_output,

      "! <p class="shorttext synchronized" lang="en">Execute value help for parameter P_FL1NAM</p>
      at_sel_screen_on_vr_p_fl1nam,

      "! <p class="shorttext synchronized" lang="en">Execute value help for parameter P_FL1PTH</p>
      at_sel_screen_on_vr_p_fl1pth,

      "! <p class="shorttext synchronized" lang="en">Execute value help for parameter P_FL2NAM</p>
      at_sel_screen_on_vr_p_fl2nam,

      "! <p class="shorttext synchronized" lang="en">Execute value help for parameter P_FL2PTH</p>
      at_sel_screen_on_vr_p_fl2pth,

      "! <p class="shorttext synchronized" lang="en">Execute value help for parameter P_INSTID</p>
      at_sel_screen_on_vr_p_instid,

      "! <p class="shorttext synchronized" lang="en">Check selection values</p>
      at_sel_screen,

      "! <p class="shorttext synchronized" lang="en">Main method, that controls the entire processing</p>
      main.

* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e           FOR  if_xo_const_message~error,
      c_msgty_i           FOR  if_xo_const_message~info,
      c_msgty_s           FOR  if_xo_const_message~success,
      c_msgty_w           FOR  if_xo_const_message~warning.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">File 1 - upper selection fields</p>
      mo_file_1       TYPE REF TO zcl_ca_file_utility_selscrctlr,
      "! <p class="shorttext synchronized" lang="en">File 2 - lower selection fields</p>
      mo_file_2       TYPE REF TO zcl_ca_file_utility_selscrctlr,
      "! <p class="shorttext synchronized" lang="en">Screen field attributes (usage with table SCREEN)</p>
      mo_scr_fld_attr TYPE REF TO zcl_ca_c_screen_field_attr,
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for file utility</p>
      mo_file_options TYPE REF TO zcl_ca_c_file_utility,
      "! <p class="shorttext synchronized" lang="en">BC ArchiveLink + DMS: Content of a business object</p>
      mo_al_cont      TYPE REF TO zcl_ca_archive_content.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Instantiate file 1 and set corresponding default values</p>
      initialize_first_option,

      "! <p class="shorttext synchronized" lang="en">Check selection values for execution</p>
      "!
      "! @raising   zcx_ca_error | <p class="shorttext synchronized" lang="en">Common exception: Abstract SUPER exception + helper methods</p>
      check_input_for_execution
        RAISING
          zcx_ca_error.

ENDCLASS.                     "demo_usage_file_utility  DEFINITION


CLASS demo_usage_file_utility IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    mo_scr_fld_attr = zcl_ca_c_screen_field_attr=>get_instance( ).
    mo_file_options = zcl_ca_c_file_utility=>get_instance( ).

    initialize_first_option( ).
  ENDMETHOD.                    "constructor


  METHOD initialize_first_option.
    "-----------------------------------------------------------------*
    "   Instanciate file 1 and set corresponding default values
    "-----------------------------------------------------------------*
    TRY.
        p_fl1loc = mo_file_options->location-server.
        p_fl1typ = mo_file_options->type-physical.
        p_fl1op  = mo_file_options->operation-output.
        p_fl1mod = mo_file_options->mode-binary.

        mo_file_1 = NEW #( iv_location        = p_fl1loc
                           iv_scrflds_file_no = mo_file_options->selection_fields-for_file_1 ).

        "Create instance for first option only to be able to hide the corresponding
        "block from the selection screen.
        mo_file_2 = NEW #( iv_location        = mo_file_options->location-server
                           iv_scrflds_file_no = mo_file_options->selection_fields-for_file_2 ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "initialize_first_option


  METHOD at_sel_screen_output.
    "-----------------------------------------------------------------*
    "   Control / adjust selection screen fields
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_screen      TYPE screen,
      lv_screen_flag TYPE num1.

    "FLx - Location
    "FTx - Logical or physical name
    "FPx - Path
    "FNx - File name
    "FOx - File operation type
    "FMx - Text or binary mode

    TRY.
        CASE abap_true.
          WHEN p_rbcaas.
            "Copy archived file to appl. server - only fields for the first file are relevant.
            "Parameter IV_USE_SECOND_FILE of method MODIFY_SELECTION_FIELDS has default abap_false,
            "so the second file will be automatically hidden.
            "Hide selection fields 'File mode' and make 'File type' and 'Operation' display only
            mo_file_1->modify_selection_fields( iv_mask_hiding    = 'FM'
                                                iv_mask_disp_only = 'FO' ) ##no_text.

            "Hide the complete block with all fields
            mo_file_2->modify_selection_fields( iv_mask_hiding = 'FL;FT;FP;FN;FO;FM' ) ##no_text.

            lv_screen_flag = mo_scr_fld_attr->switch-on.

          WHEN p_rbcdas.
            "Copy file from local documents folder to appl. server - both files are relevant.
            "Change the location is not allowed -> Hide locaton for both;
            mo_file_1->modify_selection_fields( iv_mask_hiding     = 'FL;FM;FT'
                                                iv_mask_disp_only  = 'FO' ) ##no_text.

            mo_file_2->modify_selection_fields( iv_mask_hiding     = 'FL;FM'
                                                iv_mask_disp_only  = 'FO' ) ##no_text.

            lv_screen_flag = mo_scr_fld_attr->switch-off.
        ENDCASE.

        LOOP AT SCREEN INTO ls_screen.
          "Let fields of first option appear and ...
          CHECK   ls_screen-group1 EQ 'CAF'    OR
                ( ls_screen-group3 EQ 'BLK'   AND
                  ls_screen-name   CS 'CAF' ) ##no_text.
          ls_screen-active = lv_screen_flag.
          MODIFY SCREEN FROM ls_screen.
        ENDLOOP.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "at_sel_screen_output


  METHOD at_sel_screen_on_vr_p_fl1nam.
    "-----------------------------------------------------------------*
    "   Execute value help for parameter P_FL1NAM
    "-----------------------------------------------------------------*
    mo_file_1->f4_browse( ).
  ENDMETHOD.                    "at_sel_screen_on_vr_p_fl1nam


  METHOD at_sel_screen_on_vr_p_fl1pth.
    "-----------------------------------------------------------------*
    "   Execute value help for parameter P_FL1PTH
    "-----------------------------------------------------------------*
    mo_file_1->f4_browse( ).
  ENDMETHOD.                    "at_sel_screen_on_vr_p_fl1pth


  METHOD at_sel_screen_on_vr_p_fl2nam.
    "-----------------------------------------------------------------*
    "   Execute value help for parameter P_FL2NAM
    "-----------------------------------------------------------------*
    mo_file_2->f4_browse( ).
  ENDMETHOD.                    "at_sel_screen_on_vr_p_fl2nam


  METHOD at_sel_screen_on_vr_p_fl2pth.
    "-----------------------------------------------------------------*
    "   Execute value help for parameter P_FL1PTH
    "-----------------------------------------------------------------*
    mo_file_2->f4_browse( ).
  ENDMETHOD.                    "at_sel_screen_on_vr_p_fl2pth


  METHOD at_sel_screen_on_vr_p_instid.
    "-----------------------------------------------------------------*
    "   Execute value help for parameter P_INSTID
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_return TYPE swotreturn,
      lv_typeid TYPE swo_objtyp.

    TRY.
        "Get screen values
        DATA(lo_vh_options) = zcl_ca_c_vh_tool=>get_instance( ).
        DATA(lo_value_help_tool) = NEW zcl_ca_vh_tool( iv_progname  = sy-repid
                                                       iv_screen_no = sy-dynnr
                                                       iv_vh_type   = lo_vh_options->vh_type-internal_table ).

        lo_value_help_tool->register_screen_field( iv_scr_field_name = 'P_TYPEID' ) ##no_text.
        lo_value_help_tool->register_screen_field( iv_scr_field_name = 'P_INSTID' ) ##no_text.
        lo_value_help_tool->read_field_values_from_screen( ).

        lo_value_help_tool->get_screen_field_value(
                                              EXPORTING
                                                iv_scr_field_name   = 'P_TYPEID'
                                              IMPORTING
                                                ev_scr_field_value  = lv_typeid ) ##no_text.

        IF lv_typeid IS INITIAL.
          MESSAGE s001(38) WITH 'No object type entered - function not possible'(e02).
          RETURN.
        ENDIF.

        "Call dialog to enter key in a structured way
        CALL FUNCTION 'SWO_DIALOG_OBJECT_KEY_EDIT'
          EXPORTING
            objtype = lv_typeid
            title   = 'Set key for requested object'(pt1)
            objkey  = p_instid
          IMPORTING
            return  = ls_return
            objkey  = p_instid.
        DATA(lx_error) =
             CAST zcx_ca_param( zcx_ca_error=>create_exception(
                                                 iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                                                 iv_function = 'SWO_DIALOG_OBJECT_KEY_EDIT'
                                                 is_return   = VALUE #( type       = ls_return-errortype
                                                                        id         = ls_return-workarea
                                                                        number     = ls_return-message
                                                                        message_v1 = ls_return-variable1
                                                                        message_v2 = ls_return-variable2
                                                                        message_v3 = ls_return-variable3
                                                                        message_v4 = ls_return-variable4 ) ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "at_sel_screen_on_vr_p_instid


  METHOD at_sel_screen.
    "-----------------------------------------------------------------*
    "   Check selection values
    "-----------------------------------------------------------------*
    TRY.
        CASE sscrfields-ucomm.
          WHEN 'SCE_SEL' ##no_text.
            CASE abap_true.
              WHEN p_rbcaas.
                "Copy archived file to appl. server or PC
                initialize_first_option( ).
                CLEAR p_fl1nam.

              WHEN p_rbcdas.
                "Copy file from local documents folder to appl. server
                mo_file_1 = NEW #( mo_file_options->location-pc ).
                "Set default values for hidden fields
                p_fl1typ = mo_file_options->type-physical.
                p_fl1nam = '%homepath%\documents' ##no_text.
                p_fl1op  = mo_file_options->operation-input.
                p_fl1mod = mo_file_options->mode-binary.

                mo_file_2 = NEW #( mo_file_options->location-server ).
                "Set default values for hidden fields
                p_fl2op  = mo_file_options->operation-output.
                p_fl2mod = mo_file_options->mode-binary.
            ENDCASE.

            IF mo_al_cont IS BOUND.    "only relevant for first option
              mo_al_cont->free( ).
              FREE mo_al_cont.
            ENDIF.

          WHEN 'FL1' ##no_text.
            "Location changed for file 1 -> create a new instance and clear path and file name
            mo_file_1 = NEW #( p_fl1loc ).
            CLEAR: p_fl1pth, p_fl1nam.

          WHEN OTHERS.
            "All of the dropdown fields trigger a command, that's why it is checked
            "here to avoid error messages.
            IF sscrfields-ucomm CP 'F++' ##no_text.
              RETURN.
            ENDIF.

            SET PARAMETER ID 'ZCA_FN1' FIELD p_fl1nam ##exists.

            check_input_for_execution( ).
        ENDCASE.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "at_sel_screen


  METHOD check_input_for_execution.
    "-----------------------------------------------------------------*
    "   Check selection values for execution
    "-----------------------------------------------------------------*
    CASE abap_true.
      WHEN p_rbcdas.
        "Copy file from local documents folder to appl. server
        IF p_fl1nam IS INITIAL OR
           p_fl2nam IS INITIAL.
          "Fill out all required entry fields
          MESSAGE e055(00).
        ENDIF.

      WHEN p_rbcaas.
        "Copy archived file to appl. server
        IF p_fl1nam IS INITIAL.
          "Fill out all required entry fields
          MESSAGE e055(00).
        ENDIF.

        IF p_typeid IS INITIAL OR
           p_instid IS INITIAL.
          MESSAGE e001(38) WITH 'Complete key for ArchiveLink object'(e01).

        ELSE.
          "Check existence
          IF mo_al_cont                   IS NOT BOUND OR
             mo_al_cont->ms_bo_key-typeid NE p_typeid  OR
             mo_al_cont->ms_bo_key-instid NE p_instid.
            mo_al_cont ?= zcl_ca_archive_content=>get_instance( is_lpor = VALUE #( instid = p_instid
                                                                                   typeid = p_typeid
                                                                                   catid  = swfco_objtype_bor ) ).
          ENDIF.

          mo_al_cont->get( ).
          IF mo_al_cont->has_content( ) EQ abap_false.
            "No docuemnts found for &1 &2 &3
            RAISE EXCEPTION TYPE zcx_ca_archive_content
              EXPORTING
                textid   = zcx_ca_archive_content=>no_docs_found
                mv_msgty = c_msgty_w
                mv_msgv1 = CONV #( swfco_objtype_bor )
                mv_msgv2 = CONV #( p_typeid )
                mv_msgv3 = CONV #( p_instid ).
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "check_input_for_execution


  METHOD main.
    "-----------------------------------------------------------------*
    "   Main method, that controls the entire processing
    "-----------------------------------------------------------------*
    "Local data definitions
*    DATA:
*      l...                  TYPE x..

    CASE abap_true.
      WHEN p_rbcaas.
        "Copy archived file to appl. server



      WHEN p_rbcdas.
        "Copy file from local documents folder to appl. server

    ENDCASE.
  ENDMETHOD.                    "main

ENDCLASS.                     "demo_usage_file_utility  IMPLEMENTATION


*---------------------------------------------------------------------*
*     i n i t i a l i z a t i o n
*---------------------------------------------------------------------*
INITIALIZATION.
  DATA(lo_demo_file_utility) = NEW demo_usage_file_utility( ).


*---------------------------------------------------------------------*
*     a t   s e l e c t i o n - s c r e e n   OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  lo_demo_file_utility->at_sel_screen_output( ).


*---------------------------------------------------------------------*
*     a t   s e l e c t i o n - s c r e e n   o n   VALUE-REQUEST
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fl1pth.
  "Value request help for p_fl1pth - Path 1
  lo_demo_file_utility->at_sel_screen_on_vr_p_fl1pth( ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fl1nam.
  "Value request help for p_fl1nam - File name 1
  lo_demo_file_utility->at_sel_screen_on_vr_p_fl1nam( ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fl2pth.
  "Value request help for p_fl2pth - Path 2
  lo_demo_file_utility->at_sel_screen_on_vr_p_fl2pth( ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fl2nam.
  "Value request help for p_fl2nam - File name 2
  lo_demo_file_utility->at_sel_screen_on_vr_p_fl2nam( ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_instid.
  "Value request help for p_instid - Object key
  lo_demo_file_utility->at_sel_screen_on_vr_p_instid( ).


*---------------------------------------------------------------------*
*     a t   s e l e c t i o n - s c r e e n
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  lo_demo_file_utility->at_sel_screen( ).


*---------------------------------------------------------------------*
*     s t a r t - o f - s e l e c t i o n
*---------------------------------------------------------------------*
START-OF-SELECTION.
  lo_demo_file_utility->main( ).
