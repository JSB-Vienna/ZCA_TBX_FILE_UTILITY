 "! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for application server</p>
 CLASS zcl_ca_directory_handler_as DEFINITION PUBLIC
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
*   i n s t a n c e   m e t h o d s
     METHODS:
       "! <p class="shorttext synchronized" lang="en">Convert time stamp of last modification into date and time</p>
       "!
       "! <p>The details about the return codes of C-calls at the end of method are copied from method
       "! {@link cl_hcs_directory_access.METH:get_directory_content}.</p>
       "!
       "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
       finish_reading
         RAISING
           zcx_ca_file_utility,

       "! <p class="shorttext synchronized" lang="en">Convert time stamp of last modification into date and time</p>
       "!
       "! @parameter directory_entry | <p class="shorttext synchronized" lang="en">Actual state of directory entry</p>
       "! @parameter result          | <p class="shorttext synchronized" lang="en">Directory entry with modification date and time</p>
       convert_change_time_stamp
         IMPORTING
           directory_entry TYPE zca_s_directory_entry
         RETURNING
           VALUE(result)   TYPE zca_s_directory_entry,

       "! <p class="shorttext synchronized" lang="en">Convert time stamp of last modification into date and time</p>
       "!
       "! <p>The details about the return codes of C-calls at the end of method are copied from method
       "! {@link cl_hcs_directory_access.METH:get_directory_content}.</p>
       "!
       "! @parameter path_file           | <p class="shorttext synchronized" lang="en">Complete physical path, may incl. file name</p>
       "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
       initialize_reading
         IMPORTING
           path_file TYPE zca_d_path_n_file_name_n_ext
         RAISING
           zcx_ca_file_utility,

       "! <p class="shorttext synchronized" lang="en">Check whether the file or the directory is relevant</p>
       "!
       "! @parameter directory_entry     | <p class="shorttext synchronized" lang="en">Actual state of directory entry</p>
       "! @parameter filter_content_type | <p class="shorttext synchronized" lang="en">Filter for content type</p>
       "! @parameter filter_name         | <p class="shorttext synchronized" lang="en">Filter for directory or file names</p>
       is_file_or_directory_relevant
         IMPORTING
           directory_entry     TYPE zca_s_directory_entry
           filter_content_type TYPE zcl_ca_directory_handler=>ty_ra_filter_techn_type
           filter_name         TYPE ty_ra_filter_name
         RETURNING
           VALUE(result)       TYPE abap_boolean,

       "! <p class="shorttext synchronized" lang="en">Check whether it is a file or directory</p>
       "!
       "! @parameter technical_type | <p class="shorttext synchronized" lang="en">Technical type provided by the application server</p>
       "! @parameter result         | <p class="shorttext synchronized" lang="en">Content type (Dirs or Files, but not BOTH = *)</p>
       map_technical_2_content_type
         IMPORTING
           technical_type TYPE char10
         RETURNING
           VALUE(result)  TYPE zca_d_vht_dirs_files,

       "! <p class="shorttext synchronized" lang="en">Read next entry of the directory</p>
       "!
       "! <p>The details about the return codes of C-calls at the end of method are copied from method
       "! {@link cl_hcs_directory_access.METH:get_directory_content}.</p>
       "!
       "! @parameter path_file           | <p class="shorttext synchronized" lang="en">Complete physical path, may incl. file name</p>
       "! @parameter result              | <p class="shorttext synchronized" lang="en">Next directory entry</p>
       "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
       read_next_directory_entry
         IMPORTING
           path_file     TYPE zca_d_path_n_file_name_n_ext
         RETURNING
           VALUE(result) TYPE zca_s_directory_entry
         RAISING
           zcx_ca_file_utility,

       "! <p class="shorttext synchronized" lang="en">Determines whether an entry is usable</p>
       "!
       "! @parameter directory_entry | <p class="shorttext synchronized" lang="en">Actual state of directory entry</p>
       set_flag_is_usable
         IMPORTING
           directory_entry TYPE zca_s_directory_entry
         RETURNING
           VALUE(result)   TYPE abap_boolean.

 ENDCLASS.



 CLASS zcl_ca_directory_handler_as IMPLEMENTATION.

   METHOD constructor.
     "-----------------------------------------------------------------*
     "   Constructor
     "-----------------------------------------------------------------*
     super->constructor( sel_screen_ctlr ).
     determine_location_parameters( ).
     location = cvc_file_util->location-server.
   ENDMETHOD.                    "constructor


   METHOD convert_change_time_stamp.
     "-----------------------------------------------------------------*
     "   Convert time stamp of last modification into date and time
     "-----------------------------------------------------------------*
     "Local data definitions
     DATA:
       _modification_time TYPE c LENGTH 8.      "hh:mm:ss

     result = directory_entry.
     PERFORM p6_to_date_time_tz IN PROGRAM rstr0400
                                     USING result-mod_timestamp
                                           _modification_time      "in format hh:mm:ss
                                           result-mod_date.
     "Convert changing time into internal format (OCC = 0 = replace each occurrence)
     result-mod_time = condense( replace( val = _modification_time  sub = ':'  with = ' ' occ = 0 ) ).
   ENDMETHOD.                    "convert_change_time_stamp


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


   METHOD finish_reading.
     "-----------------------------------------------------------------*
     "   Close reading pointer
     "-----------------------------------------------------------------*
     "Local data definitions
     DATA:
       _error_no      TYPE cdfmnr,
       _error_message TYPE trmsg_text.

     CALL 'C_DIR_READ_FINISH' ID 'ERRNO'  FIELD _error_no
                              ID 'ERRMSG' FIELD _error_message. "#EC CI_CCALL
     IF sy-subrc NE 0.
       "& <- CALL &(&,&,..)
       RAISE EXCEPTION TYPE zcx_ca_file_utility
         EXPORTING
           textid   = zcx_ca_file_utility=>call_error
           mv_msgty = zcx_ca_file_utility=>c_msgty_e
           mv_msgv1 = 'C_DIR_READ_FINISH'
           mv_msgv2 = CONV #( _error_message )
           mv_msgv3 = CONV #( |{ sy-subrc ZERO = NO }| )
           mv_msgv4 = CONV #( _error_no ) ##no_text.
     ENDIF.
*SAPACDEF( cU("C_DIR_READ_FINISH") , sapac04_dir_read_finish, 0 )
*/*--------------------------------------------------------------------*/
*/**
* * Stop reading a file system directory.
* *
* * ERRNO    C >=3 ? OUT Returns 'errno' of operating system in case
* *                      of error.
* * ERRMSG   C >=1 ? OUT Returns english message text of operating
* *                      system in case of error.
* *
* * SUBRC: 3             Internal error.
* *        4             dir scan has not be exhausted (Warning only).
* */
*/*--------------------------------------------------------------------*/
   ENDMETHOD.                    "finish_reading


   METHOD get_content_from_location.
     "-----------------------------------------------------------------*
     "   This method is a compressed and cleaned version of
     "   FORM FILL_FILE_LIST of program RSWATCH0 (= TA AL11)
     "-----------------------------------------------------------------*
     "Local data definitions
     DATA:
       _skip_count          TYPE i.

     create_filter(
               EXPORTING
                 content_type        = content_type
                 filter              = filter
               IMPORTING
                 filter_name         = DATA(_filter_name)
                 filter_content_type = DATA(_filter_content_type) ).

     TRY.
         initialize_reading( path ).

         DO.
           DATA(_directory_entry) = read_next_directory_entry( path ).

           IF _directory_entry-subrc EQ 1.
             EXIT.     "No more slots available.

           ELSEIF _directory_entry-subrc BETWEEN 2 AND 4.
             _skip_count += 1.
             "An internal error occurred during reading
             IF _directory_entry-subrc EQ 3.
               CONTINUE.
             ENDIF.
           ENDIF.

           "This variable can contain different values and different notations (e. g. 'file, regu' OR 'File'),
           "which is why it is unified. They mapped into a content type, where X are unessential technical entries.
           _directory_entry-techn_type   = to_lower( _directory_entry-techn_type ).
           _directory_entry-content_type = map_technical_2_content_type( _directory_entry-techn_type ).
           _directory_entry              = complete_name_variants_n_ext( _directory_entry ).

           "Skip all entries that are not requested
           IF NOT is_file_or_directory_relevant( directory_entry     = _directory_entry
                                                 filter_content_type = _filter_content_type
                                                 filter_name         = _filter_name ).
             _skip_count += 1.
             CONTINUE.
           ENDIF.

           _directory_entry-useable = set_flag_is_usable( _directory_entry ).
           _directory_entry = convert_change_time_stamp( _directory_entry ).
           APPEND _directory_entry TO content.
         ENDDO.

         finish_reading( ).

         IF _skip_count GT 0.
           MESSAGE s217(s1) WITH _skip_count.
         ENDIF.

       CATCH zcx_ca_file_utility INTO DATA(lx_catched).
         finish_reading( ).             "finish reading after an exception, otherwise the next reading won't work
         RAISE EXCEPTION lx_catched.    "forward exception to consumer of this method
     ENDTRY.
   ENDMETHOD.                    "get_content_from_location


   METHOD initialize_reading.
     "-----------------------------------------------------------------*
     "   Convert time stamp of last modification into date and time
     "-----------------------------------------------------------------*
     "Local data definitions
     DATA:
       _error_no      TYPE cdfmnr,
       _error_message TYPE trmsg_text.

     "Directory must be set
     IF path_file IS INITIAL.
       "Directory name &1 is empty or does not exist
       RAISE EXCEPTION TYPE zcx_ca_file_utility
         EXPORTING
           textid   = zcx_ca_file_utility=>invalid_directory
           mv_msgv1 = space.
     ENDIF.

     CALL 'C_DIR_READ_FINISH' ID 'ERRNO'  FIELD _error_no       "Just to be sure
                              ID 'ERRMSG' FIELD _error_message. "#EC CI_CCALL

*     DATA(_path_file) = CONV dirname_al11( path_file ).
     CALL 'C_DIR_READ_START' ID 'DIR'    FIELD path_file
*                             ID 'FILE'   FIELD '*'
                             ID 'ERRNO'  FIELD _error_no
                             ID 'ERRMSG' FIELD _error_message. "#EC CI_CCALL
     CASE sy-subrc.
       WHEN 1.       "Cannot read that directory
         "Directory name &1 is empty or does not exist
         RAISE EXCEPTION NEW zcx_ca_file_utility( textid   = zcx_ca_file_utility=>invalid_directory
                                                  mv_msgty = zcx_ca_file_utility=>c_msgty_e
                                                  mv_msgv1 = CONV #( |{ path_file ALPHA = OUT }| ) ).

       WHEN 2 OR     "Cannot use that pattern
            3 OR     "Internal error
            4.       "Last dir scan has not be finished
         "& <- CALL &(&,&,..)
         RAISE EXCEPTION NEW zcx_ca_file_utility( textid   = zcx_ca_file_utility=>call_error
                                                  mv_msgty = zcx_ca_file_utility=>c_msgty_e
                                                  mv_msgv1 = 'C_DIR_READ_START'
                                                  mv_msgv2 = CONV #( _error_message )
                                                  mv_msgv3 = CONV #( path_file )
                                                  mv_msgv4 = CONV #( |Error number = { _error_no } / RC = { sy-subrc }| ) ) ##no_text.
     ENDCASE.
*SAPACDEF( cU("C_DIR_READ_START") , sapac02_dir_read_start, 0 )
*/*--------------------------------------------------------------------*/
*/**
* * Read a file system directory.
* * This rountine starts a directory scan. The next routine retrives
* * the directory entries one by one. The third rountine finishs the
* * directory scan.
* *
* * DIR      C >=1 ? IN  Name of directory to list. (Def: current dir)
* * FILE     C >=1 ? IN  Pattern for filename.      (Def: <all>      )
* * GROUP    C >=3 ? IN  This is the group, the searched files belongs
* *                      to. Current range:
* *                      'TMP' : a temporary file
* *                      'TRACE' : a SysLog file
* *                      'OTHER' : anything else (default)
* * ERRNO    C >=3 ? OUT Returns 'errno' of operating system in case
* *                      of error.
* * ERRMSG   C >=1 ? OUT Returns english message text of operating
* *                      system in case of error.
* *
* * SUBRC: 1             Cannot read that directory.
* *        2             Cannot use that pattern.
* *        3             Internal error.
* *        4             Last dir scan has not be finished.
* */
*/*--------------------------------------------------------------------*/
   ENDMETHOD.                    "initialize_reading


   METHOD is_file_or_directory_relevant.
     "-----------------------------------------------------------------*
     "   Check whether the file or the directory is relevant
     "-----------------------------------------------------------------*
     result = abap_false.
     "Skip technical entries like devices, sockets, etc. -> see method MAP_TECHNICAL_2_CONTENT_TYPE
     IF directory_entry-content_type EQ abap_true.
       RETURN.
     ENDIF.

     IF NOT directory_entry-content_type IN filter_content_type.
       RETURN.
     ENDIF.

     DATA(_name_lower_case) =
                  SWITCH dirname_al11( directory_entry-content_type
                    WHEN cvc_file_util->content_type-directory THEN directory_entry-path_lower_case
                    WHEN cvc_file_util->content_type-file      THEN directory_entry-file_name_lower_case ).
     IF _name_lower_case IN filter_name.
       result = abap_true.
     ENDIF.
   ENDMETHOD.                    "is_file_or_directory_relevant


   METHOD map_technical_2_content_type.
     "-----------------------------------------------------------------*
     "   Is mapping the technical type into the content type
     "-----------------------------------------------------------------*
     result = COND #( WHEN technical_type EQ techn_type-directory
                        THEN cvc_file_util->content_type-directory
                      WHEN technical_type CS techn_type-file
                        THEN cvc_file_util->content_type-file
                      ELSE abap_true ).
   ENDMETHOD.                    "map_technical_2_content_type


   METHOD read_next_directory_entry.
     "-----------------------------------------------------------------*
     "   Read next entry of the directory
     "-----------------------------------------------------------------*
     CALL 'C_DIR_READ_NEXT' ID 'TYPE'   FIELD result-techn_type
                            ID 'NAME'   FIELD result-file_name
                            ID 'LEN'    FIELD result-length
                            ID 'OWNER'  FIELD result-owner
                            ID 'MTIME'  FIELD result-mod_timestamp
                            ID 'MODE'   FIELD result-protection_mode
                            ID 'ERRNO'  FIELD result-error_no
                            ID 'ERRMSG' FIELD result-error_message. "#EC CI_CCALL
     result-path            = path_file.
     result-path_lower_case = to_lower( path_file ).      "for filtering
     result-subrc           = sy-subrc.                   "is evaluated in the calling method

     IF sy-subrc EQ 5.
       "Only FILE_NAME is valid due to internal error.
       CLEAR: result-length,         result-owner,            "result-techn_type,
              result-mod_timestamp,  result-protection_mode,  result-error_no,
              result-error_message.
     ENDIF.
*SAPACDEF( cU("C_DIR_READ_NEXT") , sapac03_dir_read_next, 0 )
*/*--------------------------------------------------------------------*/
*/**
* * Read next slot of a file system directory.
* *
* * TYPE     C >=1 ? OUT FILE, DIRECTORY,... Type of entry.
* * NAME     C >=1 ? OUT Name of entry. (Possibly truncated.)
* * LEN      P >=4 ? OUT Length in bytes.
* * OWNER    C >=8 ? OUT Owner of the entry.
* * MTIME    P >=5 ? OUT Last modification date. Seconds since 1970.
* * MODE     C >=9 ? OUT Like "rwx-r-x--x": protection mode.
* * ERRNO    C >=3 ? OUT Returns 'errno' of operating system in case
* *                      of error.
* * ERRMSG   C >=1 ? OUT Returns english message text of operating
* *                      system in case of error.
* *
* * SUBRC: 1             No more slots available.
* *        3             Internal error.
* *        4             NAME is truncated (Warning only).
* */
*/*--------------------------------------------------------------------*/
   ENDMETHOD.                    "read_next_directory_entry


   METHOD set_flag_is_usable.
     "-----------------------------------------------------------------*
     "   Determines whether an entry is usable
     "-----------------------------------------------------------------*
     result = xsdbool( directory_entry-length       NE 0                     OR

                       "Only FILE_NAME is valid due to internal error
                       directory_entry-subrc        NE 5                     OR

                       "Dots only as file name have a specific meaning, but are not usable directories:
                       "One dot: ?
                       "Two dots: Is used to navigate a directory level up
                     ( directory_entry-content_type EQ techn_type-directory AND
                       directory_entry-file_name    CO '.' )                 OR    "= . or ..

                     ( directory_entry-content_type EQ techn_type-file      AND
                       directory_entry-file_name(4) NE 'core' ) ) ##no_text.
   ENDMETHOD.                    "set_flag_is_usable


   METHOD zif_ca_directory_handler~resolve_dir_param_2_dir_path.
     "-----------------------------------------------------------------*
     "   Resolve a dir. parameter of the AS into a directory path
     "-----------------------------------------------------------------*
     DATA(_directory_param) = CONV dirprofilenames( to_upper( directory_param ) ).
     CALL 'C_SAPGPARAM' ID 'NAME'  FIELD _directory_param
                        ID 'VALUE' FIELD result.          "#EC CI_CCALL
     IF sy-subrc NE 0.
       "Directory of parameter &1 does not exist
       RAISE EXCEPTION TYPE zcx_ca_file_utility
         MESSAGE ID 'STC_SC_TASKS' TYPE 'E' NUMBER '258'
         WITH _directory_param.
     ENDIF.
   ENDMETHOD.                    "zif_ca_directory_handler~resolve_dir_param_2_dir_path

 ENDCLASS.

