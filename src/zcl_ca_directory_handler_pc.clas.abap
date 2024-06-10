 "! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for presentation client/PC</p>
 CLASS zcl_ca_directory_handler_pc DEFINITION PUBLIC
                                              CREATE PROTECTED
                                              INHERITING FROM zcl_ca_directory_handler
                                              GLOBAL FRIENDS zcl_ca_directory_handler.
*  P U B L I C   S E C T I O N
   PUBLIC SECTION.
*    i n s t a n c e   m e t h o d s
     METHODS:
       zif_ca_directory_handler~resolve_dir_param_2_dir_path REDEFINITION.


*  P R O T E C T E D   S E C T I O N
   PROTECTED SECTION.
*    i n s t a n c e   m e t h o d s
     METHODS:
       "! <p class="shorttext synchronized" lang="en">Constructor</p>
       "!
       "! @parameter sel_screen_ctlr     | <p class="shorttext synchronized" lang="en">Selection screen controller if available</p>
       "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
       constructor
         IMPORTING
           sel_screen_ctlr TYPE REF TO zif_ca_file_util_selscr_ctlr OPTIONAL
         RAISING
           zcx_ca_file_utility,

       determine_location_parameters REDEFINITION,

       get_content_from_location REDEFINITION.


*  P R I V A T E   S E C T I O N
   PRIVATE SECTION.


 ENDCLASS.



 CLASS zcl_ca_directory_handler_pc IMPLEMENTATION.

   METHOD constructor.
     "-----------------------------------------------------------------*
     "   Constructor
     "-----------------------------------------------------------------*
     super->constructor( sel_screen_ctlr ).
     determine_location_parameters( ).
     location = cvc_file_util->location-pc.
   ENDMETHOD.                    "constructor


   METHOD determine_location_parameters.
     "---------------------------------------------------------------------*
     "     Determine several location depending parameters
     "---------------------------------------------------------------------*
     "Local data definitions
     DATA:
       _error       TYPE REF TO zcx_ca_file_utility,
       _encoding    TYPE abap_encod,
       _return_code TYPE syst_subrc.

     cl_gui_frontend_services=>get_platform(
       RECEIVING
         platform             = DATA(_platform)
       EXCEPTIONS
         error_no_gui         = 1
         cntl_error           = 2
         not_supported_by_gui = 3
         OTHERS               = 4 ).
     IF sy-subrc NE 0.
       _error = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                     iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                     iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                     iv_method   = 'GET_PLATFORM'
                                                     iv_subrc    = sy-subrc ) )  ##no_text.
       IF _error IS BOUND.
         RAISE EXCEPTION _error.
       ENDIF.
     ENDIF.

     CASE _platform.
       WHEN cl_gui_frontend_services=>platform_windows95 OR
            cl_gui_frontend_services=>platform_windows98 OR
            cl_gui_frontend_services=>platform_nt351     OR
            cl_gui_frontend_services=>platform_nt40      OR
            cl_gui_frontend_services=>platform_nt50      OR
            cl_gui_frontend_services=>platform_windowsxp.
         operation_system = 'Windows NT' ##no_text.
         path_separator   = '\'.

       WHEN cl_gui_frontend_services=>platform_mac.
         operation_system = 'MC' ##no_text.
         path_separator   = ':'.

       WHEN cl_gui_frontend_services=>platform_os2.
         operation_system = 'PM' ##no_text.
         path_separator   = '\'.

       WHEN cl_gui_frontend_services=>platform_linux   OR
            cl_gui_frontend_services=>platform_hpux    OR
            cl_gui_frontend_services=>platform_tru64   OR
            cl_gui_frontend_services=>platform_aix     OR
            cl_gui_frontend_services=>platform_solaris OR
            cl_gui_frontend_services=>platform_macosx.
         operation_system = 'Linux' ##no_text.
         path_separator   = '/'.

       WHEN OTHERS.
         "Operating system & is not supported
         RAISE EXCEPTION TYPE zcx_ca_file_utility
           EXPORTING
             textid   = zcx_ca_file_utility=>os_not_supported
             mv_msgty = zcx_ca_file_utility=>c_msgty_e
             mv_msgv1 = CONV #( _platform ).
     ENDCASE.

     "Get codepage of client / PC
     cl_gui_frontend_services=>get_saplogon_encoding(
       CHANGING
         file_encoding                 = _encoding
         rc                            = _return_code
       EXCEPTIONS
         cntl_error                    = 1
         error_no_gui                  = 2
         not_supported_by_gui          = 3
         cannot_initialize_globalstate = 4
         OTHERS                        = 5  ).
     IF sy-subrc NE 0.
       _error = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                     iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                     iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                     iv_method   = 'GET_SAPLOGON_ENCODING'
                                                     iv_subrc    = sy-subrc ) )  ##no_text.
       IF _error IS BOUND.
         RAISE EXCEPTION _error.
       ENDIF.
     ENDIF.

     TRY.
         DATA(_codepage) = CONV string( _encoding ).
         IF strlen( _codepage ) EQ 4             AND
            _codepage           CO '0123456789'.
           codepage = _codepage.

         ELSE.
           codepage = cl_abap_codepage=>sap_codepage( _codepage ).
         ENDIF.

       CATCH cx_parameter_invalid INTO DATA(_catched).
         _error = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                     iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                     iv_class    = 'CL_ABAP_CODEPAGE'
                                                     iv_method   = 'SAP_CODEPAGE'
                                                     ix_error    = _catched ) )  ##no_text.
         IF _error IS BOUND.
           RAISE EXCEPTION _error.
         ENDIF.
     ENDTRY.
   ENDMETHOD.                    "determine_location_parameters


   METHOD get_content_from_location.
     "-----------------------------------------------------------------*
     "   Get list of PC directory and prepare
     "-----------------------------------------------------------------*
     "Local data definitions
     DATA:
       _file_list       TYPE rstt_t_files,
       _directory_entry TYPE zca_s_directory_entry,   "ty_s_directory_entry,
       _file_count      TYPE epsfilsiz ##needed.

     cl_gui_frontend_services=>directory_list_files(
       EXPORTING
         directory                   = CONV #( path )
         filter                      = filter
         files_only                  = xsdbool( content_type EQ cvc_file_util->content_type-file )
         directories_only            = xsdbool( content_type EQ cvc_file_util->content_type-directory )
       CHANGING
         file_table                  = _file_list
         count                       = _file_count
       EXCEPTIONS
         cntl_error                  = 1
         directory_list_files_failed = 2
         wrong_parameter             = 3
         error_no_gui                = 4
         not_supported_by_gui        = 5
         OTHERS                      = 6 ).
     IF sy-subrc NE 0.
       DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                         iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                         iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                         iv_method   = 'DIRECTORY_LIST_FILES'
                                                         iv_subrc    = sy-subrc ) ) ##no_text.
       IF _error IS BOUND.
         RAISE EXCEPTION _error.
       ENDIF.
     ENDIF.

     "Map / convert directory entries into result parameter
     LOOP AT _file_list INTO DATA(_file_entry).
       CLEAR _directory_entry.
       _directory_entry-path = path.
       _directory_entry-path_lower_case = to_lower( path ).   "for filtering
       _directory_entry-techn_type     = COND #( WHEN _file_entry-isdir IS INITIAL
                                                   THEN techn_type-file ELSE techn_type-directory ).
       _directory_entry-content_type   = COND #( WHEN _directory_entry-techn_type EQ techn_type-file
                                                   THEN cvc_file_util->content_type-file
                                                   ELSE cvc_file_util->content_type-directory ) ##no_text.
       _directory_entry-file_name      = _file_entry-filename.
       _directory_entry = complete_name_variants_n_ext( _directory_entry ).

       _directory_entry-useable        = xsdbool( _directory_entry-techn_type EQ techn_type-file OR
                                                  _directory_entry-length     GT 0 ).
       _directory_entry-length         = _file_entry-filelength.
       _directory_entry-mod_date       = _file_entry-writedate.
       _directory_entry-mod_time       = _file_entry-writetime.
       APPEND _directory_entry TO content.
     ENDLOOP.
   ENDMETHOD.                    "get_content_from_location


   METHOD zif_ca_directory_handler~resolve_dir_param_2_dir_path.
     "-----------------------------------------------------------------*
     "   Resolve a dir. parameter of the AS into a directory path
     "-----------------------------------------------------------------*
     "Function &1 not allowed in Context &2
     RAISE EXCEPTION TYPE zcx_ca_file_utility
       MESSAGE ID 'SD_EXPRESSION' TYPE 'E' NUMBER '045'
       WITH 'RESOLVE_DIR_PARAM_2_DIR_PATH' 'client/PC' ##no_text.
   ENDMETHOD.                    "zif_ca_directory_handler~resolve_dir_param_2_dir_path

 ENDCLASS.

