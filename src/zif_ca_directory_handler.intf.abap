"! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for applic. server OR client/PC</p>
INTERFACE zif_ca_directory_handler PUBLIC.
* l o c a l   t y p e   d e f i n i t i o n
  TYPES:
    "! <p class="shorttext synchronized" lang="en">File infos from directory (copied from program RSWATCH0)</p>
    BEGIN OF ty_s_directory_entry,
      directory_name       TYPE dirname_al11,     " name of directory
      file_name            TYPE filename_al11,    " name of entry
      extension            TYPE saedoktyp,
      file_name_wo_ext     TYPE filename_al11,    " name of entry without ext
      techn_type           TYPE c length 10,      " type of entry
      length(8)            TYPE p DECIMALS 0,     " length in bytes
      owner                TYPE fileowner_al11,   " owner of the entry
      date_time_changed(6) TYPE p DECIMALS 0,     " last mod. date, sec since 1970.
      protection_mode      TYPE c length 9,       " like "rwx-r-x--x": prot. mode
      useable              TYPE abap_boolean,
      subrc                TYPE c length 4,
      error_no             TYPE c length 3,
      error_message        TYPE c length 100,
      mod_date             TYPE d,
      mod_time             TYPE t,
      seen                 TYPE abap_boolean,
      changed              TYPE abap_boolean,
*    status     TYPE abap_boolean,               " not used
    END   OF ty_s_directory_entry,
    ty_t_directory_content TYPE STANDARD TABLE OF ty_s_directory_entry
                                             WITH NON-UNIQUE DEFAULT KEY.

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

    "! <p class="shorttext synchronized" lang="en">Set physical path and file name (see docu in method)</p>
    "!
    "! @parameter path_file | <p class="shorttext synchronized" lang="en">Get handler for physical path and file name</p>
    set_physical_path_filename
      IMPORTING
        path_file TYPE zca_d_path_n_file_name_n_ext.

ENDINTERFACE.
