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
      directory_content             FOR zif_ca_directory_handler~directory_content,
      location                      FOR zif_ca_directory_handler~location,
      operation_system              FOR zif_ca_directory_handler~operation_system,
      path_handler                  FOR zif_ca_directory_handler~path_handler,
      path_file                     FOR zif_ca_directory_handler~path_file,
      path_separator                FOR zif_ca_directory_handler~path_separator,
*     Methods
      get_directory_content         FOR zif_ca_directory_handler~get_directory_content,
      get_physical_filename_handler FOR zif_ca_directory_handler~get_physical_filename_handler,
      is_path_file_available        FOR zif_ca_directory_handler~is_path_file_available,
      set_physical_path_filename    FOR zif_ca_directory_handler~set_physical_path_filename.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter iv_location         | <p class="shorttext synchronized" lang="en">Location: server or client (CVC_FILE_HDLR-&gt;LOCATION-*)</p>
      "! @parameter result              | <p class="shorttext synchronized" lang="en">CA-TBX: Directory / file listing</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      get_instance
        IMPORTING
          iv_location   TYPE dxlocation
        RETURNING
          VALUE(result) TYPE REF TO zif_ca_directory_handler
        RAISING
          zcx_ca_file_utility.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
**   i n s t a n c e   a t t r i b u t e s
*    DATA:
***     o b j e c t   r e f e r e n c e s
**      "! <p class="shorttext synchronized" lang="en">Description</p>
**      mo_...               TYPE REF TO x..
**
***     d a t a   r e f e r e n c e s
**      "! <p class="shorttext synchronized" lang="en">Description</p>
**      mr_...               TYPE REF TO x..
**
***     t a b l e s
**      "! <p class="shorttext synchronized" lang="en">Description</p>
**      mt_...               TYPE x..
**
***     s t r u c t u r e s
**      "! <p class="shorttext synchronized" lang="en">Description</p>
**      ms_...               TYPE x..
*
**     s i n g l e   v a l u e s
*      "! <p class="shorttext synchronized" lang="en">Complete path and file name for dataset access</p>
*      path_file    TYPE string.

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

      "! <p class="shorttext synchronized" lang="en">Get file list of directory</p>
      "!
      "! @parameter iv_path             | <p class="shorttext synchronized" lang="en">Directory name</p>
      "! @parameter iv_filter           | <p class="shorttext synchronized" lang="en">Generic path or file name (using *)</p>
      "! @parameter iv_vh_type          | <p class="shorttext synchronized" lang="en">Value help type (use const CVC_FILE_HDLR-&gt;VALUE_HELP-*)</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      get_content ABSTRACT
        IMPORTING
          iv_path    TYPE string
          iv_filter  TYPE string
          iv_vh_type TYPE zca_d_vht_dirs_files
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
    _cvc_file_util->is_location_valid( iv_location ).

    CASE iv_location.
      WHEN _cvc_file_util->location-pc.
        result ?= NEW zcl_ca_directory_handler_pc( ).

      WHEN _cvc_file_util->location-server.
        result ?= NEW zcl_ca_directory_handler_as( ).
    ENDCASE.
  ENDMETHOD.                    "get_instance


  METHOD zif_ca_directory_handler~get_directory_content.
    "-----------------------------------------------------------------*
    "   Get file OR directory list in the given directory
    "-----------------------------------------------------------------*
    cvc_file_util->is_list_sorting_valid( iv_sort ).
    cvc_file_util->is_value_help_type_valid( iv_vh_type ).

    get_physical_filename_handler( iv_path_file ).
  ENDMETHOD.                    "zif_ca_directory_hdlr~get_directory_content


  METHOD zif_ca_directory_handler~get_physical_filename_handler.
    "---------------------------------------------------------------------*
    "     Get handler for physical path and file name
    "---------------------------------------------------------------------*
    TRY.
        is_path_file_available( iv_path_file ).

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
    IF iv_path_file IS INITIAL AND
       path_file    IS INITIAL.
      "At least one of the following parameters must be passed: &1 &2 &3 &4
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>at_least_one
          mv_msgty = zcx_ca_file_utility=>c_msgty_e
          mv_msgv1 = 'IV_PATH_FILE'
          mv_msgv2 = 'PATH_FILE' ##no_text.

    ELSEIF iv_path_file IS NOT INITIAL AND
           path_file    NE iv_path_file.
      "Set new path
      path_file = iv_path_file.
      CLEAR path_handler.
    ENDIF.
  ENDMETHOD.                    "zif_ca_directory_handler~is_path_file_available


  METHOD zif_ca_directory_handler~set_physical_path_filename.
    "---------------------------------------------------------------------*
    "     Set physical path and file name
    "
    "     Use this method after you got the physical_filename_handler
    "     and changed the path with the help of it.
    "---------------------------------------------------------------------*
    path_file = iv_path_file.
  ENDMETHOD.                    "zif_ca_directory_handler~set_physical_path_filename

ENDCLASS.

