"! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for applic. server OR client/PC</p>
CLASS zcl_ca_directory_handler DEFINITION PUBLIC
                                          CREATE PROTECTED
                                          ABSTRACT.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_directory_handler.

*   a l i a s e s
    ALIASES:
*     Types
      ty_s_directory_entry          FOR zif_ca_directory_handler~ty_s_directory_entry,
*     Attributes
      cvc_file_util                 FOR zif_ca_directory_handler~cvc_file_util,
      codepage                      FOR zif_ca_directory_handler~codepage,
      content                       FOR zif_ca_directory_handler~content,
      location                      FOR zif_ca_directory_handler~location,
      operation_system              FOR zif_ca_directory_handler~operation_system,
      path_handler                  FOR zif_ca_directory_handler~path_handler,
      path_file                     FOR zif_ca_directory_handler~path_file,
      path_separator                FOR zif_ca_directory_handler~path_separator,
*     Methods
      read_content                  FOR zif_ca_directory_handler~read_content,
      get_physical_filename_handler FOR zif_ca_directory_handler~get_physical_filename_handler,
      is_path_file_available        FOR zif_ca_directory_handler~is_path_file_available,
      resolve_dir_param_2_dir_path  FOR zif_ca_directory_handler~resolve_dir_param_2_dir_path,
      set_physical_path_filename    FOR zif_ca_directory_handler~set_physical_path_filename.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter location            | <p class="shorttext synchronized" lang="en">Location: server or client (CVC_FILE_HDLR-&gt;LOCATION-*)</p>
      "! @parameter result              | <p class="shorttext synchronized" lang="en">CA-TBX: Directory / file listing</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      get_instance
        IMPORTING
          location      TYPE dxlocation
        RETURNING
          VALUE(result) TYPE REF TO zif_ca_directory_handler
        RAISING
          zcx_ca_file_utility.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   t y p e   d e f i n i t i o n s
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Filter</p>
      ty_ra_filter_name       TYPE RANGE OF filename_al11,
      ty_ra_filter_techn_type TYPE RANGE OF char10.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Technical type</p>
      BEGIN OF techn_type,
        file      TYPE c LENGTH 10 VALUE 'file' ##no_text,
        directory TYPE c LENGTH 10 VALUE 'directory' ##no_text,
      END OF techn_type.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      constructor
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Complete file name variants and the extension</p>
      "!
      "! @parameter directory_entry | <p class="shorttext synchronized" lang="en">Actual state of directory entry</p>
      "! @parameter result          | <p class="shorttext synchronized" lang="en">Directory entry completed by name variants and extension</p>
      complete_name_variants_n_ext
        IMPORTING
          directory_entry TYPE ty_s_directory_entry
        RETURNING
          VALUE(result)   TYPE ty_s_directory_entry,

      "! <p class="shorttext synchronized" lang="en">Create type conform filter for shrinking the result</p>
      "!
      "! @parameter content_type        | <p class="shorttext synchronized" lang="en">Content type (Dirs or Files, CVC_FILE_HDLR-&gt;VALUE_HELP-*)</p>
      "! @parameter filter              | <p class="shorttext synchronized" lang="en">Generic path or file name (using *)</p>
      "! @parameter filter_name         | <p class="shorttext synchronized" lang="en">Filter for directory or file names</p>
      "! @parameter filter_content_type | <p class="shorttext synchronized" lang="en">Filter for the technical type</p>
      create_filter
        IMPORTING
          content_type        TYPE zca_d_vht_dirs_files
          filter              TYPE string
        EXPORTING
          filter_name         TYPE ty_ra_filter_name
          filter_content_type TYPE ty_ra_filter_techn_type,

      "! <p class="shorttext synchronized" lang="en">Determine several location depending parameters</p>
      "!
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      determine_location_parameters ABSTRACT
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Get content of directory from location</p>
      "!
      "! <p>The details about the return codes of C-calls at the end of method are copied from method
      "! {@link cl_hcs_directory_access.METH:get_directory_content}. Unfortunately this method provides too
      "! less details (less than AL11), so that it can't be used here.</p>
      "!
      "! @parameter path                | <p class="shorttext synchronized" lang="en">Directory name</p>
      "! @parameter filter              | <p class="shorttext synchronized" lang="en">Generic path or file name (using *)</p>
      "! @parameter content_type        | <p class="shorttext synchronized" lang="en">Content type (Dirs or Files, CVC_FILE_HDLR-&gt;VALUE_HELP-*)</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      get_content_from_location ABSTRACT
        IMPORTING
          path         TYPE string
          filter       TYPE string
          content_type TYPE zca_d_vht_dirs_files
        RAISING
          zcx_ca_file_utility.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_directory_handler IMPLEMENTATION.

  METHOD complete_name_variants_n_ext.
    "-----------------------------------------------------------------*
    "   Complete file name variants and the extension
    "-----------------------------------------------------------------*
    result = directory_entry.
    "Set extension and name without extension
    IF result-content_type NE cvc_file_util->content_type-file.
      RETURN.
    ENDIF.

    TRY.
        DATA(_path_handler) =
                 cl_fs_path=>create( name      = path_file && path_separator && result-file_name
                                     path_kind = cl_fs_path=>path_kind_from_opsys( operation_system ) ).

        result-file_name_lower_case = to_lower( result-file_name ). "for filtering
        result-file_name_wo_ext     = _path_handler->get_file_base_name( ).
        result-extension            = shift_left( val    = to_upper( _path_handler->get_file_extension( ) )
                                                  places = 1 ).

      CATCH cx_sy_range_out_of_bounds.
        "File has no extension
        CLEAR result-extension.

      CATCH cx_smart_path_syntax INTO DATA(lx_catched).
        result-file_name_wo_ext = lx_catched->get_text( ).
    ENDTRY.
  ENDMETHOD.                    "complete_name_variants_n_ext


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    cvc_file_util = zcl_ca_c_file_utility=>get_instance( ).
  ENDMETHOD.                    "constructor


  METHOD create_filter.
    "-----------------------------------------------------------------*
    "   Creating filter for either directories
    "-----------------------------------------------------------------*
    DATA(_cvc_sel_options) = zcl_ca_c_sel_options=>get_instance( ).

    DATA(_filter) = COND #( WHEN filter IS INITIAL THEN '*' ELSE filter ).
    filter_name = VALUE #( ( sign   = _cvc_sel_options->sign-incl
                             option = COND #( WHEN _filter CA '*+' THEN _cvc_sel_options->option-cp
                                                                   ELSE _cvc_sel_options->option-eq )
                             low    = to_lower( _filter ) ) ).

    filter_content_type = VALUE #( ( sign   = _cvc_sel_options->sign-incl
                                     option = COND #( WHEN content_type EQ cvc_file_util->content_type-both
                                                        THEN _cvc_sel_options->option-cp
                                                        ELSE _cvc_sel_options->option-eq )
                                     low    = content_type ) ).
  ENDMETHOD.                    "create_filter


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    DATA(_cvc_file_util) = zcl_ca_c_file_utility=>get_instance( ).
    _cvc_file_util->is_location_valid( location ).

    CASE location.
      WHEN _cvc_file_util->location-pc.
        result ?= NEW zcl_ca_directory_handler_pc( ).

      WHEN _cvc_file_util->location-server.
        result ?= NEW zcl_ca_directory_handler_as( ).
    ENDCASE.
  ENDMETHOD.                    "get_instance


  METHOD zif_ca_directory_handler~get_physical_filename_handler.
    "---------------------------------------------------------------------*
    "     Get handler for physical path and file name
    "---------------------------------------------------------------------*
    TRY.
        is_path_file_available( path_file ).

        IF path_handler IS NOT BOUND.
          path_handler = cl_fs_path=>create( name      = path_file
                                             path_kind = cl_fs_path=>path_kind_from_opsys( operation_system ) ).
        ENDIF.

      CATCH cx_smart_path_syntax INTO DATA(_catched).
        DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = _catched ) )  ##no_text.
        IF _error IS BOUND.
          RAISE EXCEPTION _error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_directory_handler~get_physical_filename_handler


  METHOD zif_ca_directory_handler~is_path_file_available.
    "---------------------------------------------------------------------*
    "     Check if any path and filename is available
    "---------------------------------------------------------------------*
    IF path_file     IS INITIAL AND
       me->path_file IS INITIAL.
      "At least one of the following parameters must be passed: &1 &2 &3 &4
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>at_least_one
          mv_msgty = zcx_ca_file_utility=>c_msgty_e
          mv_msgv1 = 'PATH_FILE'
          mv_msgv2 = 'ME->PATH_FILE' ##no_text.

    ELSEIF path_file     IS NOT INITIAL AND
           me->path_file NE path_file.
      "Set new path
      me->path_file = path_file.
      CLEAR path_handler.
    ENDIF.
  ENDMETHOD.                    "zif_ca_directory_handler~is_path_file_available


  METHOD zif_ca_directory_handler~read_content.
    "-----------------------------------------------------------------*
    "   Get file OR directory list in the given directory
    "-----------------------------------------------------------------*
    cvc_file_util->is_list_sorting_valid( sort_by ).
    cvc_file_util->is_content_type_valid( content_type ).

    get_physical_filename_handler( path_file ).
    CLEAR content.

    DATA(_path) = path_handler->get_path_name( ).

    get_content_from_location( path         = _path
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
        SORT content BY content_type mod_date mod_time file_name.

      WHEN cvc_file_util->list_sorting-by_file_name.
        SORT content BY content_type file_name.
    ENDCASE.
  ENDMETHOD.                    "zif_ca_directory_hdlr~read_content


  METHOD zif_ca_directory_handler~resolve_dir_param_2_dir_path.
    "-----------------------------------------------------------------*
    "   Resolve a dir. parameter of the AS into a directory path
    "-----------------------------------------------------------------*
    "R e d e f i n e d   i n   s u b c l a s s e s
  ENDMETHOD.                    "zif_ca_directory_handler~resolve_dir_param_2_dir_path


  METHOD zif_ca_directory_handler~set_physical_path_filename.
    "---------------------------------------------------------------------*
    "     Set physical path and file name
    "
    "     Use this method after you got the physical_filename_handler
    "     and changed the path with the help of it.
    "---------------------------------------------------------------------*
    me->path_file = path_file.
  ENDMETHOD.                    "zif_ca_directory_handler~set_physical_path_filename

ENDCLASS.

