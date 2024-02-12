"! <p class="shorttext synchronized" lang="en">Demo how to use objects of package ZCA_TBX_FILE_UTILITY</p>
REPORT zca_demo_file_utility.

* t a b l e s   /   s t r u c t u r e s   for selection field definition
TABLES:
  sscrfields.            "Fields on selection screens


* s e l e c t i o n   f i e l d s
*- Scenario selection ------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK sce WITH FRAME TITLE TEXT-sce.
  SELECTION-SCREEN  BEGIN OF LINE.
    PARAMETERS
      "! <p class="shorttext synchronized" lang="en">Scenario: (C)opy (A)rchived file to appl. (S)erver or (P)C</p>
      p_rbcasp       RADIOBUTTON GROUP sce  DEFAULT 'X'
                                            USER-COMMAND scenario_changed.
    SELECTION-SCREEN COMMENT 03(70) TEXT-cas.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS
      "! <p class="shorttext synchronized" lang="en">Scenario: (C)opy file from (P)C to(2) application (S)erver</p>
      p_rbcp2s       RADIOBUTTON GROUP sce.
    SELECTION-SCREEN COMMENT 03(70) TEXT-cda.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS
      "! <p class="shorttext synchronized" lang="en">Scenario: Display a (F)ile (L)ist of selected (DI)rectory</p>
      p_rbfldi       RADIOBUTTON GROUP sce.
    SELECTION-SCREEN COMMENT 03(70) TEXT-cfl.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK sce.



"! <p>Please transfer text elements mentioned in the documentation of the global
"! class {@link zcl_ca_file_util_selscr_ctlr} into your report.</p>
INCLUDE zca_demo_file_utilityfl1.


*- Copy archived PDF file of ... -------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK caf WITH FRAME TITLE TEXT-caf.
  "Look up a connection entry in one of the tables TOA01 - 03. Filter in column
  SELECTION-SCREEN COMMENT /1(79) TEXT-ac1 MODIF ID caf.
  "RESERVE for the document class PDF. Use for the following selection parameters
  SELECTION-SCREEN COMMENT /1(75) TEXT-ac2 MODIF ID caf.
  "the values of the columns SAP_OBJECT and OBJECT_ID.
  SELECTION-SCREEN COMMENT /1(75) TEXT-ac3 MODIF ID caf.
  PARAMETERS:
    "Name of Business Object type
    p_typeid TYPE swo_objtyp           MATCHCODE OBJECT h_tojtb
                                       MODIF ID caf,
    "Key of object instance
    p_instid TYPE sibfboriid           MODIF ID caf.
SELECTION-SCREEN END OF BLOCK caf.


"! <p>Please transfer text elements mentioned in the documentation of the global
"! class {@link zcl_ca_file_util_selscr_ctlr} into your report.</p>
INCLUDE zca_demo_file_utilityfl2.


"! <p class="shorttext synchronized" lang="en">ALV for displaying file list</p>
CLASS alv_file_list DEFINITION INHERITING FROM zcl_ca_salv_wrapper
                               CREATE PUBLIC
                               FINAL.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor
        IMPORTING
          table      TYPE REF TO data
          list_title TYPE lvc_title,

      process REDEFINITION.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      prepare_alv REDEFINITION.

ENDCLASS.                     "alv_file_list  DEFINITION


CLASS alv_file_list IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    super->constructor( ir_table      = table
                        iv_list_title = list_title ).
  ENDMETHOD.                    "constructor


  METHOD prepare_alv.
    "-----------------------------------------------------------------*
    "   Prepare ALV columns
    "-----------------------------------------------------------------*
    LOOP AT mt_cols REFERENCE INTO DATA(_column)
                    WHERE columnname CP 'DIR*'
                       OR columnname CP 'ERR*'
                       OR columnname EQ 'SUBRC'
                       OR columnname EQ 'CONTENT_TYPE'
                       OR columnname CP 'FILE_NAME_*' ##no_text.
      _column->r_column->set_technical( abap_true ).
    ENDLOOP.
  ENDMETHOD.                    "prepare_alv


  METHOD process.
    "-----------------------------------------------------------------*
    "   Process
    "-----------------------------------------------------------------*
    TRY.
        prepare_alv( ).
        mo_salv->display( ).

      CATCH zcx_ca_error
            cx_salv_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE zcx_ca_error=>c_msgty_s DISPLAY LIKE zcx_ca_error=>c_msgty_e.
    ENDTRY.
  ENDMETHOD.                    "process

ENDCLASS.                     "alv_file_list  IMPLEMENTATION





"! <p class="shorttext synchronized" lang="en">Demonstrating the usage of the package ZCA_TBX_FILE_UTILITY</p>
CLASS demo_usage_file_utility DEFINITION FINAL
                                         CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check selection values</p>
      at_sel_screen,

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

      "! <p class="shorttext synchronized" lang="en">Control / adjust selection screen fields</p>
      at_sel_screen_output,

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor,

      "! <p class="shorttext synchronized" lang="en">Initialization of report data / selections</p>
      initialization,

      "! <p class="shorttext synchronized" lang="en">Main method, that controls the entire processing</p>
      main.

* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">File 1 - upper selection fields</p>
      screen_ctlr_file_1 TYPE REF TO zif_ca_file_util_selscr_ctlr,
      "! <p class="shorttext synchronized" lang="en">File 2 - lower selection fields</p>
      screen_ctlr_file_2 TYPE REF TO zif_ca_file_util_selscr_ctlr,
      "! <p class="shorttext synchronized" lang="en">Screen field attributes (usage with table SCREEN)</p>
      cvc_scr_fld_attr   TYPE REF TO zcl_ca_c_screen_field_attr,
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for select options</p>
      cvc_sel_options    TYPE REF TO zcl_ca_c_sel_options,
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for file utility</p>
      cvc_file_util      TYPE REF TO zcl_ca_c_file_utility,
      "! <p class="shorttext synchronized" lang="en">BC ArchiveLink + DMS: Content of a business object</p>
      archive_content    TYPE REF TO zcl_ca_archive_content.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Instantiate files and set values for the first option</p>
      initialize_first_option,

      "! <p class="shorttext synchronized" lang="en">Instantiate files and set values for the second option</p>
      initialize_second_option,

      "! <p class="shorttext synchronized" lang="en">Instantiate files and set values for the third option</p>
      initialize_third_option,

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
    cvc_scr_fld_attr = zcl_ca_c_screen_field_attr=>get_instance( ).
    cvc_sel_options  = zcl_ca_c_sel_options=>get_instance( ).
    cvc_file_util    = zcl_ca_c_file_utility=>get_instance( ).
  ENDMETHOD.                    "constructor


  METHOD initialize_first_option.
    "-----------------------------------------------------------------*
    "   Instantiate for scenario saving an archived file on the PC or AS
    "-----------------------------------------------------------------*
    TRY.
        p_fl1loc = cvc_file_util->location-pc.         "Can be changed
        p_fl1typ = cvc_file_util->type-physical.       "Is hidden
        p_fl1op  = cvc_file_util->operation-output.    "= Writing a file (is hidden)
        p_fl1mod = cvc_file_util->mode-binary.         "Is display only
        CLEAR: p_fl1pth, p_fl1nam.
        SET PARAMETER ID 'ZCA_PATH_1' FIELD p_fl1pth.
        SET PARAMETER ID 'ZCA_FILE_NAME_1' FIELD p_fl1nam.

        screen_ctlr_file_1 = NEW zcl_ca_file_util_selscr_ctlr(
                                               location     = p_fl1loc
                                               sel_field_id = cvc_file_util->selection_field_id-for_file_1 ).

        "Although not needed for this scenario, the second file handler is created too, to be able to hide its
        "selection parameters comfortably and to demonstrate how to hide the corresponding selection screen block.
        screen_ctlr_file_2 = NEW zcl_ca_file_util_selscr_ctlr(
                                               location     = cvc_file_util->location-server
                                               sel_field_id = cvc_file_util->selection_field_id-for_file_2 ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "initialize_first_option


  METHOD initialize_second_option.
    "-----------------------------------------------------------------*
    "   Instantiate for scenario copying file from PC to server
    "-----------------------------------------------------------------*
    TRY.
        p_fl1loc = cvc_file_util->location-pc.         "Is display only - location is fix due to scenario
        screen_ctlr_file_1 = NEW zcl_ca_file_util_selscr_ctlr(
                                               location     = p_fl1loc
                                               sel_field_id = cvc_file_util->selection_field_id-for_file_1 ).
        p_fl1typ = cvc_file_util->type-physical.       "Is hidden
        p_fl1op  = cvc_file_util->operation-input.     "= Reading a file
        p_fl1mod = cvc_file_util->mode-binary.         "Is display only
        CLEAR: p_fl1pth, p_fl1nam.
        SET PARAMETER ID 'ZCA_PATH_1' FIELD p_fl1pth.
        SET PARAMETER ID 'ZCA_FILE_NAME_1' FIELD p_fl1nam.

        p_fl2loc = cvc_file_util->location-server.     "Is display only - location is fix due to scenario
        screen_ctlr_file_2 = NEW zcl_ca_file_util_selscr_ctlr(
                                               location     = p_fl2loc
                                               sel_field_id = cvc_file_util->selection_field_id-for_file_2 ).
        p_fl2typ = cvc_file_util->type-physical.       "Is hidden
        p_fl2op  = cvc_file_util->operation-output.    "= Writing a file (is hidden)
        p_fl2mod = cvc_file_util->mode-binary.         "Is display only
        p_fl2pth = screen_ctlr_file_2->directory_hdlr->resolve_dir_param_2_dir_path( ).  "= DIR_HOME in TA AL11
        CLEAR p_fl2nam.
        SET PARAMETER ID 'ZCA_PATH_2' FIELD p_fl2pth.
        SET PARAMETER ID 'ZCA_FILE_NAME_2' FIELD p_fl2nam.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "initialize_second_option


  METHOD initialize_third_option.
    "-----------------------------------------------------------------*
    "   Instantiate for scenario displaying a file list to a directory
    "-----------------------------------------------------------------*
    TRY.
        p_fl1loc = cvc_file_util->location-pc.         "Can be changed
        p_fl1typ = cvc_file_util->type-physical.       "Is hidden
        p_fl1op  = cvc_file_util->operation-input.     "= Reading a file (is hidden)
        p_fl1mod = cvc_file_util->mode-binary.         "Is hidden
        CLEAR: p_fl1pth, p_fl1nam.
        SET PARAMETER ID 'ZCA_PATH_1' FIELD p_fl1pth.
        SET PARAMETER ID 'ZCA_FILE_NAME_1' FIELD p_fl1nam.

        screen_ctlr_file_1 = NEW zcl_ca_file_util_selscr_ctlr(
                                               location     = p_fl1loc
                                               sel_field_id = cvc_file_util->selection_field_id-for_file_1 ).

        "Although not needed for this scenario, the second file handler is created too, to be able to hide its
        "selection parameters comfortably and to demonstrate how to hide the corresponding selection screen block.
        screen_ctlr_file_2 = NEW zcl_ca_file_util_selscr_ctlr(
                                               location     = cvc_file_util->location-server
                                               sel_field_id = cvc_file_util->selection_field_id-for_file_2 ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "initialize_third_option


  METHOD at_sel_screen_output.
    "-----------------------------------------------------------------*
    "   Control / adjust selection screen fields
    "-----------------------------------------------------------------*
    "FL - (L)ocation
    "FT - (T)ype = logical or physical name
    "FP - (P)ath
    "FN - file (N)ame
    "FO - (O)peration type
    "FM - text or binary (M)ode

    TRY.
        CASE abap_true.
          WHEN p_rbcasp.     "Copy an archived file to the application server or the PC
            "Since the file should be written to either the server OR the PC, the operation type is
            "a must. And since logical path are to much for a demo program the physical path and file
            "type is also clear.
            "Hide selection fields 'File type' and 'Operation' and make 'File mode' display only
            screen_ctlr_file_1->modify_selection_fields( mask_2_hide_sel_params      = 'FT;FO'
                                                         mask_2_set_params_disp_only = 'FM' ) ##no_text.

            "Hide the complete block with all fields of the second file
            screen_ctlr_file_2->modify_selection_fields( use_for_value_help_only = abap_true ) ##no_text.

            "Make the parameters for the archive object visible (again)
            cvc_scr_fld_attr->activate( screen_field_name  = '*CAF*' ) ##no_text.
            cvc_scr_fld_attr->activate( screen_modif_group = 'CAF' ) ##no_text.

          WHEN p_rbcp2s.     "Copy a file from a local folder (= PC) to a folder of the appl. server
            "Change the location is not allowed -> Hide location for both;
            screen_ctlr_file_1->modify_selection_fields( mask_2_hide_sel_params      = 'FL;FM;FT'
                                                         mask_2_set_params_disp_only = 'FO' ) ##no_text.

            screen_ctlr_file_2->modify_selection_fields( mask_2_hide_sel_params      = 'FL;FM;FT'
                                                         mask_2_set_params_disp_only = 'FO' ) ##no_text.

            "Hide in this case the parameters for the archive object
            cvc_scr_fld_attr->deactivate( screen_field_name  = '*CAF*' ) ##no_text.
            cvc_scr_fld_attr->deactivate( screen_modif_group = 'CAF' ) ##no_text.

          WHEN p_rbfldi.     "Display a file list of a selected directory
            screen_ctlr_file_1->modify_selection_fields( mask_2_hide_sel_params = 'FN;FM;FT;FO' ) ##no_text.

            "Hide the complete block with all fields of the second file
            screen_ctlr_file_2->modify_selection_fields( use_for_value_help_only = abap_true ) ##no_text.

            "Hide also in this case the parameters for the archive object
            cvc_scr_fld_attr->deactivate( screen_field_name  = '*CAF*' ) ##no_text.
            cvc_scr_fld_attr->deactivate( screen_modif_group = 'CAF' ) ##no_text.
        ENDCASE.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "at_sel_screen_output


  METHOD at_sel_screen_on_vr_p_fl1nam.
    "-----------------------------------------------------------------*
    "   Execute value help for parameter P_FL1NAM
    "-----------------------------------------------------------------*
    screen_ctlr_file_1->f4_browse( ).
  ENDMETHOD.                    "at_sel_screen_on_vr_p_fl1nam


  METHOD at_sel_screen_on_vr_p_fl1pth.
    "-----------------------------------------------------------------*
    "   Execute value help for parameter P_FL1PTH
    "-----------------------------------------------------------------*
    screen_ctlr_file_1->f4_browse( ).
  ENDMETHOD.                    "at_sel_screen_on_vr_p_fl1pth


  METHOD at_sel_screen_on_vr_p_fl2nam.
    "-----------------------------------------------------------------*
    "   Execute value help for parameter P_FL2NAM
    "-----------------------------------------------------------------*
    screen_ctlr_file_2->f4_browse( ).
  ENDMETHOD.                    "at_sel_screen_on_vr_p_fl2nam


  METHOD at_sel_screen_on_vr_p_fl2pth.
    "-----------------------------------------------------------------*
    "   Execute value help for parameter P_FL1PTH
    "-----------------------------------------------------------------*
    screen_ctlr_file_2->f4_browse( ).
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
          WHEN 'SCENARIO_CHANGED' ##no_text.
            CASE abap_true.
              WHEN p_rbcasp.     "Copy an archived file to the application server or the PC
                initialize_first_option( ).

              WHEN p_rbcp2s.     "Copy a file from a local folder (= PC) to a folder of the appl. server
                initialize_second_option( ).

              WHEN p_rbfldi.     "Display a file list of a selected directory
                initialize_third_option( ).
            ENDCASE.

            IF archive_content IS BOUND.    "only relevant for first option
              archive_content->free( ).
              FREE archive_content.
            ENDIF.

          WHEN cvc_file_util->selscr_user_command-location_1_changed.
            "Location changed for file 1 -> create a new instance and clear path and file name. This is
            "only for the first scenario possible -> see method AT_SEL_SCREEN_OUTPUT.
            screen_ctlr_file_1 = NEW zcl_ca_file_util_selscr_ctlr(
                                                     location = p_fl1loc
                                                     sel_field_id = cvc_file_util->selection_field_id-for_file_1 ).
            CLEAR: p_fl1pth, p_fl1nam.
            SET PARAMETER ID 'ZCA_PATH_1' FIELD p_fl1pth.
            SET PARAMETER ID 'ZCA_FILE_NAME_1' FIELD p_fl1nam.

          WHEN OTHERS.
            "Except the execution command no input checks should be made here to avoid error messages
            IF sscrfields-ucomm NE 'ONLI' ##no_text.
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
      WHEN p_rbcp2s.      "Copy file from local documents folder to appl. server
        IF p_fl1pth IS INITIAL OR
           p_fl1nam IS INITIAL OR
           p_fl2pth IS INITIAL OR
           p_fl2nam IS INITIAL.
          "Fill out all required entry fields
          MESSAGE e055(00).
        ENDIF.

      WHEN p_rbfldi.     "Display a file list of a selected directory
        IF p_fl1pth IS INITIAL.
          "Fill out all required entry fields
          MESSAGE e055(00).
        ENDIF.

      WHEN p_rbcasp.     "Copy archived file to appl. server
        IF p_fl1pth IS INITIAL OR
           p_fl1nam IS INITIAL.
          "Fill out all required entry fields
          MESSAGE e055(00).
        ENDIF.

        IF p_typeid IS INITIAL OR
           p_instid IS INITIAL.
          MESSAGE e001(38) WITH 'Complete key for ArchiveLink object'(e01).

        ELSE.
          "Check existence
          IF archive_content                   IS NOT BOUND OR
             archive_content->ms_bo_key-typeid NE p_typeid  OR
             archive_content->ms_bo_key-instid NE p_instid.
            archive_content ?= zcl_ca_archive_content=>get_instance( is_lpor = VALUE #( instid = p_instid
                                                                                        typeid = p_typeid
                                                                                        catid  = swfco_objtype_bor ) ).
          ENDIF.

          archive_content->get(
                    it_filter_al = VALUE #( ( name    = archive_content->mo_arch_filter->al_filter-doc_class
                                              dsign   = cvc_sel_options->sign-incl
                                              doption = cvc_sel_options->option-eq
                                              dlow    = 'PDF' ) ) ) ##no_text.
          IF archive_content->has_content( ) EQ abap_false.
            "No docuemnts found for &1 &2 &3
            RAISE EXCEPTION TYPE zcx_ca_archive_content
              EXPORTING
                textid   = zcx_ca_archive_content=>no_docs_found
                mv_msgty = zcx_ca_archive_content=>c_msgty_w
                mv_msgv1 = CONV #( p_typeid )
                mv_msgv2 = CONV #( p_instid )
                mv_msgv3 = 'PDF'.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "check_input_for_execution


  METHOD initialization.
    "-----------------------------------------------------------------*
    "   Initialization of report data / selections
    "-----------------------------------------------------------------*
    initialize_first_option( ).
  ENDMETHOD.                    "initialization


  METHOD main.
    "-----------------------------------------------------------------*
    "   Main method, that controls the entire processing
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _binary_document      TYPE solix_tab.

    TRY.
        "HINT !! The method GET_FILE_HANDLER can be used in several ways as demonstrated here in the
        "different branches of this CASE statement.

        CASE abap_true.
          WHEN p_rbcasp.     "Copy an archived file to the application server or the PC
            DATA(_pdf_document) = archive_content->mt_docs[ 1 ].  "Existence of PDFs already check in AT_SEL_SCREEN
            DATA(_pdf_table)    = cl_bcs_convert=>xstring_to_solix( _pdf_document->get_document( ) ).

            "This creates a new instance for the specific file. Furthermore it checks and transfers the values
            "of the selection parameters to it.
            screen_ctlr_file_1->get_file_handler( )->write(
                                                        CHANGING
                                                          file = _pdf_table ).

          WHEN p_rbcp2s.     "Copy a file from a local folder (= PC) to a folder of the appl. server
            screen_ctlr_file_1->get_file_handler( ).
            screen_ctlr_file_1->file_hdlr->read(
                                            IMPORTING
                                              file = _binary_document ).

            DATA(_server_file_hdlr) = screen_ctlr_file_2->get_file_handler( ).
            _server_file_hdlr->write(
                                  CHANGING
                                    file = _binary_document ).

          WHEN p_rbfldi.     "Display a file list of a selected directory
            DATA(_sel_screen_values) = screen_ctlr_file_1->provide_selscreen_param_values( ).
            screen_ctlr_file_1->directory_hdlr->read_content( content_type = cvc_file_util->content_type-both
                                                              path_file    = _sel_screen_values-path_file ).
            DATA(_file_list) = screen_ctlr_file_1->directory_hdlr->content.   "Table must be changeable for ALV
            NEW alv_file_list( table      = REF #( _file_list )
                               list_title = |{ TEXT-lti } { _sel_screen_values-path }| )->process( ).
        ENDCASE.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "main

ENDCLASS.                     "demo_usage_file_utility  IMPLEMENTATION


*---------------------------------------------------------------------*
*     i n i t i a l i z a t i o n
*---------------------------------------------------------------------*
INITIALIZATION.
  DATA(lo_demo_file_utility) = NEW demo_usage_file_utility( ).
  lo_demo_file_utility->initialization( ).


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
