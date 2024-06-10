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
*     Attributes
      cvc_file_util                  FOR zif_ca_directory_handler~cvc_file_util,
      codepage                       FOR zif_ca_directory_handler~codepage,
      content                        FOR zif_ca_directory_handler~content,
      location                       FOR zif_ca_directory_handler~location,
      operation_system               FOR zif_ca_directory_handler~operation_system,
      path_handler                   FOR zif_ca_directory_handler~path_handler,
      path_file                      FOR zif_ca_directory_handler~path_file,
      path_separator                 FOR zif_ca_directory_handler~path_separator,
*     Methods
      assemble_path_n_file_name      FOR zif_ca_directory_handler~assemble_path_n_file_name,
      get_file_handler_2_dir_entry   FOR zif_ca_directory_handler~get_file_handler_2_dir_entry,
      get_pathfile_from_logical_name FOR zif_ca_directory_handler~get_pathfile_from_logical_name,
      get_physical_filename_handler  FOR zif_ca_directory_handler~get_physical_filename_handler,
      read_content                   FOR zif_ca_directory_handler~read_content,
      resolve_dir_param_2_dir_path   FOR zif_ca_directory_handler~resolve_dir_param_2_dir_path,
      set_selected_path              FOR zif_ca_directory_handler~set_selected_path.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter location            | <p class="shorttext synchronized" lang="en">Location: server or client (CVC_FILE_HDLR-&gt;LOCATION-*)</p>
      "! @parameter sel_screen_ctlr     | <p class="shorttext synchronized" lang="en">Selection screen controller if available</p>
      "! @parameter result              | <p class="shorttext synchronized" lang="en">CA-TBX: Directory / file listing</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      get_instance
        IMPORTING
          location        TYPE dxlocation
          sel_screen_ctlr TYPE REF TO zif_ca_file_util_selscr_ctlr OPTIONAL
        RETURNING
          VALUE(result)   TYPE REF TO zif_ca_directory_handler
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

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Selection screen controller if available</p>
      sel_screen_ctlr TYPE REF TO zif_ca_file_util_selscr_ctlr.

*   i n s t a n c e   m e t h o d s
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

      "! <p class="shorttext synchronized" lang="en">Complete file name variants and the extension</p>
      "!
      "! @parameter directory_entry | <p class="shorttext synchronized" lang="en">Actual state of directory entry</p>
      "! @parameter result          | <p class="shorttext synchronized" lang="en">Directory entry completed by name variants and extension</p>
      complete_name_variants_n_ext
        IMPORTING
          directory_entry TYPE zca_s_directory_entry
        RETURNING
          VALUE(result)   TYPE zca_s_directory_entry,   "ty_s_directory_entry,

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
          path         TYPE zca_d_path_n_file_name_n_ext
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
    me->sel_screen_ctlr = sel_screen_ctlr.
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
        result ?= NEW zcl_ca_directory_handler_pc( sel_screen_ctlr ).

      WHEN _cvc_file_util->location-server.
        result ?= NEW zcl_ca_directory_handler_as( sel_screen_ctlr ).
    ENDCASE.
  ENDMETHOD.                    "get_instance


  METHOD zif_ca_directory_handler~assemble_path_n_file_name.
    "---------------------------------------------------------------------*
    "     Assemble path and file name, after resolving logical names if necessary
    "---------------------------------------------------------------------*
    CASE processing_params-type.
      WHEN cvc_file_util->type-physical.
        "Clean up directory delimiter for a uniform / consistent path + file name handling
        processing_params-path = CONV dirname_al11( replace( val = processing_params-path  sub = '/\'
                                                    with = ' '  occ = -1 ) ).  "-1 = start at the end of the string

        path_file_name_hdlr = get_physical_filename_handler(
                                |{ processing_params-path }{ path_separator }{ processing_params-file_name }| ).
        processing_params-path_file = path_file_name_hdlr->get_path_name( ).

      WHEN cvc_file_util->type-logical.
        processing_params-path_file = get_pathfile_from_logical_name(
                                            logical_filename    = CONV #( processing_params-file_name )
*                                            parameter_1         = space
*                                            parameter_2         = space      "Still not implemented
*                                            parameter_3         = space
                                            is_for_local_client = xsdbool( location EQ cvc_file_util->location-pc )
                                            including_dir       = abap_true
                                            with_file_extension = abap_true ).

        path_file_name_hdlr = get_physical_filename_handler( processing_params-path_file ).
        processing_params-path      = path_file_name_hdlr->get_path_component( ).
        processing_params-file_name = path_file_name_hdlr->get_file_name( ).
    ENDCASE.
  ENDMETHOD.                    "zif_ca_directory_handler~assemble_path_n_file_name


  METHOD zif_ca_directory_handler~get_file_handler_2_dir_entry.
    "---------------------------------------------------------------------*
    "     Get file handler to a directory entry
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      _processing_params   TYPE zca_s_file_util_sel_params.

    IF sel_screen_ctlr IS BOUND.
      _processing_params = sel_screen_ctlr->provide_selscreen_param_values( ).
    ENDIF.

    "This method is consumed for a specific file. Therefore the path and file name have to overwritten
    "due to the possibility that the selection screen contains only a path, because e. g. all PDF of a
    "directory should be processed. And this is why the import parameters overrule the settings of the
    "selection screen, except the location.
    _processing_params-location  = location.
    _processing_params-path      = directory_entry-path.
    _processing_params-file_name = directory_entry-file_name.
    _processing_params-mode      = mode.
    _processing_params-operation = operation.
    _processing_params-type      = type.

    result = zcl_ca_file_handler=>get_instance( processing_params = _processing_params
                                                directory_entry   = directory_entry ).
  ENDMETHOD.                    "zif_ca_directory_handler~get_file_handler_2_dir_entry


  METHOD zif_ca_directory_handler~get_pathfile_from_logical_name.
    "---------------------------------------------------------------------*
    "     Get physical path and file name to a logical filename
    "---------------------------------------------------------------------*
    CALL FUNCTION 'FILE_GET_NAME_AND_VALIDATE'
      EXPORTING
        logical_filename    = logical_filename
        operating_system    = operation_system
        parameter_1         = parameter_1
        parameter_2         = parameter_2
        parameter_3         = parameter_3
        including_dir       = abap_true
        with_file_extension = abap_false
        use_buffer          = abap_false
      IMPORTING
        file_name           = result
      EXCEPTIONS
        file_not_found      = 1
        validation_failed   = 2
        incorrect_path      = 3
        OTHERS              = 4.
    IF sy-subrc NE 0.
      DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_function = 'FILE_GET_NAME_AND_VALIDATE'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF _error IS BOUND.
        RAISE EXCEPTION _error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "zif_ca_directory_handler~get_pathfile_from_logical_name


  METHOD zif_ca_directory_handler~get_physical_filename_handler.
    "---------------------------------------------------------------------*
    "     Get handler for physical or logical path or full file name
    "---------------------------------------------------------------------*
    TRY.
        result = cl_fs_path=>create( name      = path_file
                                     path_kind = cl_fs_path=>path_kind_from_opsys( operation_system ) ).

      CATCH cx_smart_path_syntax INTO DATA(_catched).
        DATA(_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = _catched ) )  ##no_text.
        IF _error IS BOUND.
          RAISE EXCEPTION _error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_directory_handler~get_physical_filename_handler


  METHOD zif_ca_directory_handler~read_content.
    "-----------------------------------------------------------------*
    "   Get file OR directory list in the given directory
    "-----------------------------------------------------------------*
    cvc_file_util->is_list_sorting_valid( sort_by ).
    cvc_file_util->is_content_type_valid( content_type ).

    IF path_file     IS INITIAL AND
       me->path_file IS INITIAL.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION NEW zcx_ca_file_utility( textid   = zcx_ca_file_utility=>at_least_one
                                               mv_msgty = zcx_ca_file_utility=>c_msgty_e
                                               mv_msgv1 = 'PATH_FILE'
                                               mv_msgv2 = 'SPACE' ) ##no_text.
    ENDIF.

    IF path_file IS NOT INITIAL.
      path_handler  = get_physical_filename_handler( path_file ).
      me->path_file = path_handler->get_path_name( ).
    ENDIF.

    IF refresh EQ abap_false AND
       content IS NOT INITIAL.
      RETURN.
    ENDIF.

    CLEAR content.
    get_content_from_location( path         = me->path_file
                               filter       = filter
                               content_type = content_type ).
    IF content IS INITIAL.
      "No files found in directory &1
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>no_files_found
          mv_msgty = zcx_ca_file_utility=>c_msgty_s
          mv_msgv1 = CONV #( me->path_file ).
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


  METHOD zif_ca_directory_handler~set_selected_path.
    "-----------------------------------------------------------------*
    "   Set selected path and path handler
    "-----------------------------------------------------------------*
    IF processing_params IS INITIAL AND
       path_handler      IS NOT BOUND.
      "At least one of the following parameters must be passed: &1 &2 &3 &4
      RAISE EXCEPTION NEW zcx_ca_file_utility( textid   = zcx_ca_file_utility=>at_least_one
                                               mv_msgty = zcx_ca_file_utility=>c_msgty_e
                                               mv_msgv1 = 'PROCESSING_PARAMS'
                                               mv_msgv2 = 'PATH_HANDLER' ) ##no_text.
    ENDIF.

    IF path_handler IS BOUND.
      me->path_handler = path_handler.

    ELSE.
      IF processing_params-path IS INITIAL.
        "Parameter '&1' has invalid value '&2'
        RAISE EXCEPTION NEW zcx_ca_file_utility( textid   = zcx_ca_file_utility=>at_least_one
                                                 mv_msgty = zcx_ca_file_utility=>c_msgty_e
                                                 mv_msgv1 = 'PROCESSING_PARAMS-PATH'
                                                 mv_msgv2 = 'SPACE' ) ##no_text.
      ENDIF.

      me->path_handler = get_physical_filename_handler( CONV #( processing_params-path ) ).
    ENDIF.

    path_file = me->path_handler->get_path_name( ).
  ENDMETHOD.                    "zif_ca_directory_handler~set_selected_path

ENDCLASS.

