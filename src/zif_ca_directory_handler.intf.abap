"! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for applic. server OR client/PC</p>
INTERFACE zif_ca_directory_handler PUBLIC.
* l o c a l   t y p e   d e f i n i t i o n
  TYPES:
    "! <p class="shorttext synchronized" lang="en">File infos from directory (copied from program RSWATCH0)</p>
    BEGIN OF ty_s_directory_entry,
      directory_name       TYPE dirname_al11,     "is the original path case sensitive
      dir_name_lower_case  TYPE dirname_al11,     "is the original path in lower case
      file_name            TYPE filename_al11,    "is the original file name case sensitive WITH extension
      file_name_lower_case TYPE filename_al11,    "is the original file name in lower case
      file_name_wo_ext     TYPE filename_al11,    "is the original file name case sensitive WITHOUT extension
      extension            TYPE saedoktyp,        "extension in upper case (to find technical settings in SAP)
      techn_type           TYPE char10,
      content_type         TYPE zca_d_vht_dirs_files,
      length(8)            TYPE p DECIMALS 0,     " length in bytes
      useable              TYPE abap_boolean,
      seen                 TYPE abap_boolean,
      changed              TYPE abap_boolean,
      owner                TYPE fileowner_al11,   " owner of the entry
      mod_timestamp(6)     TYPE p DECIMALS 0,     " last mod. date, sec since 1970.
      mod_date             TYPE d,
      mod_time             TYPE t,
      protection_mode      TYPE c LENGTH 9,       " like "rwx-r-x--x": prot. mode
      subrc                TYPE c LENGTH 4,
      error_no             TYPE c LENGTH 3,
      error_message        TYPE c LENGTH 100,
    END   OF ty_s_directory_entry,
    ty_t_directory_content TYPE STANDARD TABLE OF ty_s_directory_entry WITH EMPTY KEY
                                    WITH NON-UNIQUE SORTED KEY name_ext  COMPONENTS file_name extension
                                    WITH NON-UNIQUE SORTED KEY ext_name  COMPONENTS extension file_name
                                    WITH NON-UNIQUE SORTED KEY type_name COMPONENTS content_type file_name.

* i n s t a n c e   a t t r i b u t e s
  DATA:
*   o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for file utility</p>
    cvc_file_util    TYPE REF TO zcl_ca_c_file_utility READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">File System Path handler</p>
    path_handler     TYPE REF TO cl_fs_path READ-ONLY,

**   d a t a   r e f e r e n c e s
*    "! <p class="shorttext synchronized" lang="en">Description</p>
*    mr_...               TYPE REF TO x..

*   t a b l e s
    "! <p class="shorttext synchronized" lang="en">Content of directory / file list</p>
    content          TYPE ty_t_directory_content READ-ONLY,

**   s t r u c t u r e s
*    "! <p class="shorttext synchronized" lang="en">Description</p>
*    ms_...               TYPE x..

*   s i n g l e   v a l u e s
    "! <p class="shorttext synchronized" lang="en">Current codepage of either the app. server or the client/PC</p>
    codepage         TYPE cpcodepage READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">Location: A = server / P = client/PC</p>
    location         TYPE dxlocation READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">OS of location (conform for logical filename)</p>
    operation_system TYPE syst_opsys READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Path, file name and extension</p>
    path_file        TYPE zca_d_path_n_file_name_n_ext,
    "! <p class="shorttext synchronized" lang="en">OS specific path separator</p>
    path_separator   TYPE zca_d_path_separator READ-ONLY.

* i n s t a n c e   m e t h o d s
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Read directory content to the given path</p>
    "!
    "! @parameter content_type        | <p class="shorttext synchronized" lang="en">Content type (Dirs or Files, CVC_FILE_HDLR-&gt;VALUE_HELP-*)</p>
    "! @parameter path_file           | <p class="shorttext synchronized" lang="en">Complete physical path, may incl. file name</p>
    "! @parameter filter              | <p class="shorttext synchronized" lang="en">Generic filename (may end with *)</p>
    "! @parameter sort_by             | <p class="shorttext synchronized" lang="en">Sort file list (use const CVC_FILE_HDLR-&gt;LIST_SORTING-*)</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    read_content
      IMPORTING
        content_type TYPE zca_d_vht_dirs_files
        path_file    TYPE zca_d_path_n_file_name_n_ext OPTIONAL
        filter       TYPE string  DEFAULT '*'
        sort_by      TYPE char1   DEFAULT zcl_ca_c_file_utility=>list_sorting-by_date_time_changed
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Get physical path and file name to a logical filename</p>
    "!
    "! @parameter path_file           | <p class="shorttext synchronized" lang="en">Physical path and/or file name</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    get_physical_filename_handler
      IMPORTING
        path_file TYPE zca_d_path_n_file_name_n_ext OPTIONAL      "is may be already available via e. g. GET_LOGICAL_FILENAME
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Check if any path and filename is available</p>
    "!
    "! @parameter path_file           | <p class="shorttext synchronized" lang="en">Complete physical path and file name</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    is_path_file_available
      IMPORTING
        path_file TYPE zca_d_path_n_file_name_n_ext
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Resolve a dir. parameter of the AS into a directory path</p>
    "!
    "! @parameter directory_param     | <p class="shorttext synchronized" lang="en">Directory parameter as shown in the first column of TA AL11</p>
    "! @parameter result              | <p class="shorttext synchronized" lang="en">Parameter values</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    resolve_dir_param_2_dir_path
      IMPORTING
        directory_param TYPE dirprofilenames DEFAULT 'DIR_HOME'
      RETURNING
        VALUE(result)   TYPE dirname_al11
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Set physical path and file name (see docu in method)</p>
    "!
    "! @parameter path_file | <p class="shorttext synchronized" lang="en">Get handler for physical path and file name</p>
    set_physical_path_filename
      IMPORTING
        path_file TYPE zca_d_path_n_file_name_n_ext.

ENDINTERFACE.
