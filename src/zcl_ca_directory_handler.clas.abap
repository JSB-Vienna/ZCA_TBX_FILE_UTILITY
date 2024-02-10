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
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      constructor
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Determine several location depending parameters</p>
      "!
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      determine_location_parameters ABSTRACT
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Get content of directory from location</p>
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

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    cvc_file_util = zcl_ca_c_file_utility=>get_instance( ).
  ENDMETHOD.                    "constructor


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
  ENDMETHOD.                    "zif_ca_directory_hdlr~read_content


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

