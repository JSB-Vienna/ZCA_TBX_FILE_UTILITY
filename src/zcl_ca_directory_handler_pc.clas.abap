 "! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for presentation client/PC</p>
 CLASS zcl_ca_directory_handler_pc DEFINITION PUBLIC
                                              CREATE PROTECTED
                                              INHERITING FROM zcl_ca_directory_handler
                                              GLOBAL FRIENDS zcl_ca_directory_handler.
*  P U B L I C   S E C T I O N
   PUBLIC SECTION.
*    i n s t a n c e   m e t h o d s
     METHODS:
       zif_ca_directory_handler~get_directory_content REDEFINITION.


*  P R O T E C T E D   S E C T I O N
   PROTECTED SECTION.
*    i n s t a n c e   m e t h o d s
     METHODS:
       "! <p class="shorttext synchronized" lang="en">Constructor</p>
       "!
       "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
       constructor
         RAISING
           zcx_ca_file_utility,

       determine_location_parameters REDEFINITION,

       get_content REDEFINITION.


*  P R I V A T E   S E C T I O N
   PRIVATE SECTION.


 ENDCLASS.



 CLASS zcl_ca_directory_handler_pc IMPLEMENTATION.

   METHOD constructor.
     "-----------------------------------------------------------------*
     "   Constructor
     "-----------------------------------------------------------------*
     super->constructor( ).
     location = cvc_file_util->location-pc.
   ENDMETHOD.                    "constructor


   METHOD determine_location_parameters.
     "---------------------------------------------------------------------*
     "     Determine several location depending parameters
     "---------------------------------------------------------------------*
     "Local data definitions
     DATA:
       _error    TYPE REF TO zcx_ca_file_utility,
       _encoding TYPE abap_encod,
       _rc       TYPE syst_subrc.

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
         rc                            = _rc
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


   METHOD get_content.
     "-----------------------------------------------------------------*
     "   Get list of PC directory and prepare for return
     "-----------------------------------------------------------------*
     "Local data definitions
     DATA:
       _file_list       TYPE rstt_t_files,
       _directory_entry TYPE ty_s_directory_entry,
       _file_count      TYPE epsfilsiz ##needed.

     cvc_file_util->is_value_help_type_valid( iv_vh_type ).

     cl_gui_frontend_services=>directory_list_files(
       EXPORTING
         directory                   = iv_path
         filter                      = iv_filter
         files_only                  = xsdbool( iv_vh_type EQ cvc_file_util->value_help-for_files )
         directories_only            = xsdbool( iv_vh_type EQ cvc_file_util->value_help-for_directories )
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
       _directory_entry-dirname  = iv_path.
       _directory_entry-name     = _file_entry-filename.
       _directory_entry-len      = _file_entry-filelength.
       _directory_entry-mod_date = _file_entry-createdate.
       _directory_entry-mod_time = _file_entry-createtime.

       IF _directory_entry-name CA '.'.
         "Get extension
         SPLIT _directory_entry-name AT '.' INTO TABLE DATA(_file_name_splitter) IN CHARACTER MODE.
         READ TABLE _file_name_splitter INTO  DATA(_name_splitter)
                                        INDEX lines( _file_name_splitter ).
         _directory_entry-ext = to_upper( CONV saedoktyp( _name_splitter ) ).
         TRY.
             "Get file name without extension
             DATA(_file_name_length) = find( val  = _directory_entry-name
                                             sub  = '.' && _directory_entry-ext
                                             case = abap_false ).
             IF _file_name_length GE 0.
               _directory_entry-name_wo_ext = _directory_entry-name(_file_name_length).
             ELSE.
               _directory_entry-name_wo_ext = _directory_entry-name.
             ENDIF.

           CATCH cx_sy_strg_par_val.
             _directory_entry-name_wo_ext = _directory_entry-name.
         ENDTRY.
       ENDIF.

       APPEND _directory_entry TO directory_content.
     ENDLOOP.
   ENDMETHOD.                    "get_content


   METHOD zif_ca_directory_handler~get_directory_content.
     "-----------------------------------------------------------------*
     "   Get file OR directory list in the given directory
     "-----------------------------------------------------------------*
     super->get_directory_content( iv_path_file = iv_path_file
                                   iv_filter    = iv_filter
                                   iv_sort      = iv_sort
                                   iv_vh_type   = iv_vh_type ).

     DATA(_path) = path_handler->get_path_name( ).

     get_content( iv_path    = _path
                  iv_filter  = iv_filter
                  iv_vh_type = iv_vh_type ).

     IF directory_content IS INITIAL.
       "No files found in directory &1
       RAISE EXCEPTION TYPE zcx_ca_file_utility
         EXPORTING
           textid   = zcx_ca_file_utility=>no_files_found
           mv_msgty = zcx_ca_file_utility=>c_msgty_s
           mv_msgv1 = CONV #( _path ).
     ENDIF.

     CASE iv_sort.
       WHEN cvc_file_util->list_sorting-by_time.
         SORT directory_content BY mtime DESCENDING name ASCENDING.

            WHEN cvc_file_util->list_sorting-by_name.
         SORT directory_content BY name ASCENDING mtime DESCENDING.
     ENDCASE.
   ENDMETHOD.                    "zif_ca_directory_hdlr~get_directory_content

 ENDCLASS.

