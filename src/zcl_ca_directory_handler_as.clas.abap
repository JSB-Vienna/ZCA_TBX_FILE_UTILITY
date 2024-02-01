 "! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for application server</p>
 CLASS zcl_ca_directory_handler_as DEFINITION PUBLIC
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



 CLASS zcl_ca_directory_handler_as IMPLEMENTATION.

   METHOD constructor.
     "-----------------------------------------------------------------*
     "   Constructor
     "-----------------------------------------------------------------*
     super->constructor( ).
     location = cvc_file_util->location-server.
   ENDMETHOD.                    "constructor


   METHOD determine_location_parameters.
     "---------------------------------------------------------------------*
     "     Determine several location depending parameters
     "---------------------------------------------------------------------*
     operation_system = sy-opsys.

     codepage = cl_abap_codepage=>current( sap_name = abap_true ).

     CALL FUNCTION 'DMC_MDS_GET_PATHSEPARATOR'
       IMPORTING
         ev_path_separator     = path_separator
       EXCEPTIONS
         opsys_not_supported   = 1
         filesys_not_supported = 2
         OTHERS                = 3.
     IF sy-subrc NE 0.
       DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                     iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                     iv_function = 'DMC_MDS_GET_PATHSEPARATOR'
                                                     iv_subrc    = sy-subrc ) )  ##no_text.
       IF _error IS BOUND.
         RAISE EXCEPTION _error.
       ENDIF.
     ENDIF.
   ENDMETHOD.                    "determine_location_parameters


   METHOD get_content.
     "-----------------------------------------------------------------*
     "   This method is a slightly adapted copy of FORM FILL_FILE_LIST of
     "   program RSWATCH0 (= TA AL11)
     "-----------------------------------------------------------------*
     "Local data definitions
     DATA:
       _filter          TYPE RANGE OF string,
       _directory_entry TYPE ty_s_directory_entry,
       _error_count     TYPE p LENGTH 2 DECIMALS 0 VALUE 0,
       _error_id        TYPE ty_s_directory_entry-errno,
       _error_message   TYPE ty_s_directory_entry-errmsg.

     "Directory must be set
     IF iv_path IS INITIAL.
       "Directory name &1 is empty or does not exist
       RAISE EXCEPTION TYPE zcx_ca_file_utility
         EXPORTING
           textid   = zcx_ca_file_utility=>invalid_directory
           mv_msgv1 = space.
     ENDIF.

     cvc_file_util->is_value_help_type_valid( iv_vh_type ).

     DATA(_path) = CONV dirname_al11( iv_path ).

     DATA(_cvc_sel_options) = zcl_ca_c_sel_options=>get_instance( ).
     _filter = VALUE #( sign   = _cvc_sel_options->sign-incl
                        option = _cvc_sel_options->option-cp
                      ( low    = iv_filter )
                      ( low    = |{ iv_filter CASE = LOWER }| )
                      ( low    = |{ iv_filter CASE = UPPER }| ) ).

     "Just to be sure
     CALL 'C_DIR_READ_FINISH' ID 'ERRNO'  FIELD _error_id
                              ID 'ERRMSG' FIELD _error_message. "#EC CI_CCALL

     CALL 'C_DIR_READ_START' ID 'DIR'    FIELD _path
                             ID 'FILE'   FIELD '*'
                             ID 'ERRNO'  FIELD _directory_entry-errno
                             ID 'ERRMSG' FIELD _directory_entry-errmsg. "#EC CI_CCALL
     IF sy-subrc NE 0.
       "& <- CALL &(&,&,..)
       RAISE EXCEPTION TYPE zcx_ca_file_utility
         EXPORTING
           textid   = zcx_ca_file_utility=>call_error
           mv_msgty = zcx_ca_file_utility=>c_msgty_e
           mv_msgv1 = CONV #( _error_message )
           mv_msgv2 = 'C_DIR_READ_START'
           mv_msgv3 = CONV #( _directory_entry-errno )
           mv_msgv4 = CONV #( _directory_entry-errmsg ) ##no_text.
     ENDIF.

     DO.
       CLEAR _directory_entry.
       CALL 'C_DIR_READ_NEXT' ID 'TYPE'   FIELD _directory_entry-type
                              ID 'NAME'   FIELD _directory_entry-name
                              ID 'LEN'    FIELD _directory_entry-len
                              ID 'OWNER'  FIELD _directory_entry-owner
                              ID 'MTIME'  FIELD _directory_entry-mtime
                              ID 'MODE'   FIELD _directory_entry-mode
                              ID 'ERRNO'  FIELD _directory_entry-errno
                              ID 'ERRMSG' FIELD _directory_entry-errmsg. "#EC CI_CCALL

       _directory_entry-dirname = iv_path.
       _directory_entry-subrc   = sy-subrc.

       CASE sy-subrc.
         WHEN 0.
           CLEAR: _directory_entry-errno,
                  _directory_entry-errmsg.
           IF _directory_entry-type(1) CO 'Ff' ##no_text.      "= normal file
             _directory_entry-useable = xsdbool( _directory_entry-name(4) NE 'core' ) ##no_text.
           ELSE.
             " directory, device, fifo, socket,...
             _directory_entry-useable = abap_false.
           ENDIF.

           IF _directory_entry-len EQ 0.
             _directory_entry-useable = abap_false.
           ENDIF.

           "Skip all entries that are not requested
           IF _directory_entry-name NOT IN _filter.
             CONTINUE.
           ENDIF.

           IF ( iv_vh_type            EQ cvc_file_util->value_help-for_directories AND
                _directory_entry-type NE 'directory'      )                         OR

              ( iv_vh_type            EQ cvc_file_util->value_help-for_files       AND
                _directory_entry-type EQ 'directory'      )                         OR

              "Don't offer anything that is no file and no directory
              ( _directory_entry-useable EQ abap_false                             AND
                _directory_entry-type    NE 'directory' ) ##no_text.
             CONTINUE.
           ENDIF.

         WHEN 1.
           "No more slots available.
           EXIT.

         WHEN 5.
           "Only NAME is valid due to internal error.
           CLEAR: _directory_entry-type,  _directory_entry-len,    _directory_entry-owner,  _directory_entry-mtime,
                  _directory_entry-mode,  _directory_entry-errno,  _directory_entry-errmsg.
           _directory_entry-useable = abap_false.

         WHEN OTHERS.                     " SY-SUBRC >= 2
           "possible other return codes (sapaci2.c)
           "3 ... Internal error.
           "4 ... NAME is truncated (Warning only)
           ADD 1 TO _error_count.

           "don't list files with error
           IF _directory_entry-subrc EQ 3.
             CONTINUE.
           ENDIF.
       ENDCASE.

       PERFORM p6_to_date_time_tz IN PROGRAM rstr0400
                                       USING _directory_entry-mtime
                                             _directory_entry-mod_time
                                             _directory_entry-mod_date.

       "Set extension and name without extension
       IF _directory_entry-type CS 'file' ##no_text.
         TRY.
             DATA(_path_handler) = cl_fs_path=>create( name      = path_file && path_separator && _directory_entry-name
                                                       path_kind = cl_fs_path=>path_kind_from_opsys( operation_system ) ).

             _directory_entry-name_wo_ext = _path_handler->get_file_base_name( ).
             _directory_entry-ext         = shift_left( val    = |{ _path_handler->get_file_extension( ) CASE = UPPER }|
                                                        places = 1 ).

           CATCH cx_sy_range_out_of_bounds.
             "File has no extension
             CLEAR _directory_entry-ext.

           CATCH cx_smart_path_syntax INTO DATA(lx_catched).
             _directory_entry-name_wo_ext = lx_catched->get_text( ).
         ENDTRY.
       ENDIF.

       APPEND _directory_entry TO directory_content.
     ENDDO.

     CALL 'C_DIR_READ_FINISH' ID 'ERRNO'  FIELD _error_id
                              ID 'ERRMSG' FIELD _error_message. "#EC CI_CCALL
     IF sy-subrc NE 0.
       WRITE: / 'C_DIR_READ_FINISH', 'SUBRC', sy-subrc ##no_text.
     ENDIF.

     IF _error_count GT 0.
       MESSAGE s217(s1) WITH _error_count.
     ENDIF.
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

     get_content( iv_path    = path_handler->get_path_name( )
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

