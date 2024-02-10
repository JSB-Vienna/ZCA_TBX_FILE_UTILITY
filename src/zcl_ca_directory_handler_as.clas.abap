 "! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for application server</p>
 CLASS zcl_ca_directory_handler_as DEFINITION PUBLIC
                                              CREATE PROTECTED
                                              INHERITING FROM zcl_ca_directory_handler
                                              GLOBAL FRIENDS zcl_ca_directory_handler.
*  P U B L I C   S E C T I O N
   PUBLIC SECTION.
*    i n s t a n c e   m e t h o d s
     METHODS:
       zif_ca_directory_handler~read_content REDEFINITION.


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

       get_content_from_location REDEFINITION.


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


   METHOD get_content_from_location.
     "-----------------------------------------------------------------*
     "   This method is a slightly adapted copy of FORM FILL_FILE_LIST of
     "   program RSWATCH0 (= TA AL11)
     "-----------------------------------------------------------------*
     "Local data definitions
     DATA:
       _filter          TYPE RANGE OF string,
       _directory_entry TYPE ty_s_directory_entry,
       _error_count     TYPE p LENGTH 2 DECIMALS 0 VALUE 0,
       _error_id        TYPE ty_s_directory_entry-error_no,
       _error_message   TYPE ty_s_directory_entry-error_message,
       _mod_time        TYPE c LENGTH 8.      "hh:mm:ss

     "Directory must be set
     IF path IS INITIAL.
       "Directory name &1 is empty or does not exist
       RAISE EXCEPTION TYPE zcx_ca_file_utility
         EXPORTING
           textid   = zcx_ca_file_utility=>invalid_directory
           mv_msgv1 = space.
     ENDIF.

     cvc_file_util->is_content_type_valid( content_type ).

     DATA(_path) = CONV dirname_al11( path ).

     DATA(_cvc_sel_options) = zcl_ca_c_sel_options=>get_instance( ).
     _filter = VALUE #( sign   = _cvc_sel_options->sign-incl
                        option = _cvc_sel_options->option-cp
                      ( low    = filter )
                      ( low    = |{ filter CASE = LOWER }| )
                      ( low    = |{ filter CASE = UPPER }| ) ).

     "Just to be sure
     CALL 'C_DIR_READ_FINISH' ID 'ERRNO'  FIELD _error_id
                              ID 'ERRMSG' FIELD _error_message. "#EC CI_CCALL

     CALL 'C_DIR_READ_START' ID 'DIR'    FIELD _path
                             ID 'FILE'   FIELD '*'
                             ID 'ERRNO'  FIELD _directory_entry-error_no
                             ID 'ERRMSG' FIELD _directory_entry-error_message. "#EC CI_CCALL
     IF sy-subrc NE 0.
       "& <- CALL &(&,&,..)
       RAISE EXCEPTION TYPE zcx_ca_file_utility
         EXPORTING
           textid   = zcx_ca_file_utility=>call_error
           mv_msgty = zcx_ca_file_utility=>c_msgty_e
           mv_msgv1 = CONV #( _error_message )
           mv_msgv2 = 'C_DIR_READ_START'
           mv_msgv3 = CONV #( _directory_entry-error_no )
           mv_msgv4 = CONV #( _directory_entry-error_message ) ##no_text.
     ENDIF.

     DO.
       CLEAR _directory_entry.
       CALL 'C_DIR_READ_NEXT' ID 'TYPE'   FIELD _directory_entry-techn_type
                              ID 'NAME'   FIELD _directory_entry-file_name
                              ID 'LEN'    FIELD _directory_entry-length
                              ID 'OWNER'  FIELD _directory_entry-owner
                              ID 'MTIME'  FIELD _directory_entry-date_time_changed
                              ID 'MODE'   FIELD _directory_entry-protection_mode
                              ID 'ERRNO'  FIELD _directory_entry-error_no
                              ID 'ERRMSG' FIELD _directory_entry-error_message. "#EC CI_CCALL

       _directory_entry-directory_name = path.
       _directory_entry-subrc          = sy-subrc.

       CASE sy-subrc.
         WHEN 0.
           CLEAR: _directory_entry-error_no,
                  _directory_entry-error_message.
           IF _directory_entry-techn_type(1) CO 'Ff' ##no_text.      "= normal file
             _directory_entry-useable = xsdbool( _directory_entry-file_name(4) NE 'core' ) ##no_text.
           ELSE.
             " directory, device, fifo, socket,...
             _directory_entry-useable = abap_false.
           ENDIF.

           IF _directory_entry-length EQ 0.
             _directory_entry-useable = abap_false.
           ENDIF.

           "Skip all entries that are not requested
           IF _directory_entry-file_name NOT IN _filter.
             CONTINUE.
           ENDIF.

           IF ( content_type                EQ cvc_file_util->content_type-directories AND
                _directory_entry-techn_type NE 'directory' )                            OR

              ( content_type                EQ cvc_file_util->content_type-files       AND
                _directory_entry-techn_type EQ 'directory' )                            OR

              "Don't offer anything that is no file and no directory
              ( _directory_entry-useable    EQ abap_false                              AND
                _directory_entry-techn_type NE 'directory' ) ##no_text.
             CONTINUE.
           ENDIF.

         WHEN 1.
           "No more slots available.
           EXIT.

         WHEN 5.
           "Only FILE_NAME is valid due to internal error.
           CLEAR: _directory_entry-techn_type,         _directory_entry-length,           _directory_entry-owner,
                  _directory_entry-date_time_changed,  _directory_entry-protection_mode,  _directory_entry-error_no,
                  _directory_entry-error_message.
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
                                       USING _directory_entry-date_time_changed
                                             _mod_time      "in format hh:mm:ss
                                             _directory_entry-mod_date.
       _directory_entry-mod_time = condense( replace( val = _mod_time  sub = ':'  with = ' ' ) ).

       "Set extension and name without extension
       IF _directory_entry-techn_type CS 'file' ##no_text.
         TRY.
             DATA(_path_handler) = cl_fs_path=>create( name      = path_file && path_separator && _directory_entry-file_name
                                                       path_kind = cl_fs_path=>path_kind_from_opsys( operation_system ) ).

             _directory_entry-file_name_wo_ext = _path_handler->get_file_base_name( ).
             _directory_entry-extension = shift_left( val    = |{ _path_handler->get_file_extension( ) CASE = UPPER }|
                                                      places = 1 ).

           CATCH cx_sy_range_out_of_bounds.
             "File has no extension
             CLEAR _directory_entry-extension.

           CATCH cx_smart_path_syntax INTO DATA(lx_catched).
             _directory_entry-file_name_wo_ext = lx_catched->get_text( ).
         ENDTRY.
       ENDIF.

       APPEND _directory_entry TO content.
     ENDDO.

     CALL 'C_DIR_READ_FINISH' ID 'ERRNO'  FIELD _error_id
                              ID 'ERRMSG' FIELD _error_message. "#EC CI_CCALL
     IF sy-subrc NE 0.
       WRITE: / 'C_DIR_READ_FINISH', 'SUBRC', sy-subrc ##no_text.
     ENDIF.

     IF _error_count GT 0.
       MESSAGE s217(s1) WITH _error_count.
     ENDIF.
   ENDMETHOD.                    "get_content_from_location


   METHOD zif_ca_directory_handler~read_content.
     "-----------------------------------------------------------------*
     "   Get file OR directory list in the given directory
     "-----------------------------------------------------------------*
     super->read_content( path_file    = path_file
                          filter       = filter
                          sort_by      = sort_by
                          content_type = content_type ).

     DATA(_path) = path_handler->get_path_name( ).

     get_content_from_location( path         = path_handler->get_path_name( )
                                filter       = filter
                                content_type = content_type ).

     IF content IS INITIAL.
       "No files found in directory &1
       RAISE EXCEPTION TYPE zcx_ca_file_utility
         EXPORTING
           textid   = zcx_ca_file_utility=>no_files_found
           mv_msgty = zcx_ca_file_utility=>c_msgty_s
           mv_msgv1 = CONV #( _path ).
     ENDIF.

     CASE sort_by.
       WHEN cvc_file_util->list_sorting-by_date_time_changed.
         SORT content BY mod_date mod_time file_name.

       WHEN cvc_file_util->list_sorting-by_file_name.
         SORT content BY file_name.
     ENDCASE.
   ENDMETHOD.                    "zif_ca_directory_hdlr~read_content

 ENDCLASS.

