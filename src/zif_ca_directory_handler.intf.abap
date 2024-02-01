"! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for applic. server OR client/PC</p>
INTERFACE zif_ca_directory_handler PUBLIC.
* l o c a l   t y p e   d e f i n i t i o n
  TYPES:
    "! <p class="shorttext synchronized" lang="en">File infos from directory (copied from program RSWATCH0)</p>
    BEGIN OF ty_s_directory_entry,
      dirname     TYPE dirname_al11,     " name of directory
      name        TYPE filename_al11,    " name of entry
      name_wo_ext TYPE filename_al11,    " name of entry without ext
      ext         TYPE saedoktyp,
      type(10)    TYPE c,                " type of entry.
      len(8)      TYPE p DECIMALS 0,     " length in bytes.
      owner       TYPE fileowner_al11,   " owner of the entry.
      mtime(6)    TYPE p DECIMALS 0,     " last mod. date, sec since 1970.
      mode(9)     TYPE c,                " like "rwx-r-x--x": prot. mode
      useable(1)  TYPE c,
      subrc(4)    TYPE c,
      errno(3)    TYPE c,
      errmsg(40)  TYPE c,
      mod_date    TYPE d,
      mod_time(8) TYPE c,                " hh:mm:ss
      seen(1)     TYPE c,
      changed(1)  TYPE c,
*    status(1)   TYPE c,               " not used
    END   OF ty_s_directory_entry,
    ty_t_directory_content TYPE STANDARD TABLE OF ty_s_directory_entry
                                             WITH NON-UNIQUE DEFAULT KEY.

* i n s t a n c e   a t t r i b u t e s
  DATA:
*   o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for file utility</p>
    cvc_file_util     TYPE REF TO zcl_ca_c_file_utility READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">File System Path handler</p>
    path_handler      TYPE REF TO cl_fs_path READ-ONLY,

**   d a t a   r e f e r e n c e s
*    "! <p class="shorttext synchronized" lang="en">Description</p>
*    mr_...               TYPE REF TO x..

*   t a b l e s
    "! <p class="shorttext synchronized" lang="en">Content of directory / file list</p>
    directory_content TYPE ty_t_directory_content READ-ONLY,

**   s t r u c t u r e s
*    "! <p class="shorttext synchronized" lang="en">Description</p>
*    ms_...               TYPE x..

*   s i n g l e   v a l u e s
    "! <p class="shorttext synchronized" lang="en">Current codepage of either the app. server or the client/PC</p>
    codepage          TYPE cpcodepage READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">Location: A = server / P = client/PC</p>
    location          TYPE dxlocation READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">OS of location (conform for logical filename)</p>
    operation_system  TYPE syst_opsys READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Path, file name and extension</p>
    path_file         TYPE zca_d_path_n_file_name_n_ext,
    "! <p class="shorttext synchronized" lang="en">OS specific path separator</p>
    path_separator    TYPE zca_d_path_separator READ-ONLY.

* i n s t a n c e   m e t h o d s
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Get directory content to the given path</p>
    "!
    "! @parameter iv_vh_type          | <p class="shorttext synchronized" lang="en">Value help for Dirs or Files (CVC_FILE_HDLR-&gt;VALUE_HELP-*)</p>
    "! @parameter iv_path_file        | <p class="shorttext synchronized" lang="en">Complete physical path, may incl. file name</p>
    "! @parameter iv_filter           | <p class="shorttext synchronized" lang="en">Generic filename (may end with *)</p>
    "! @parameter iv_sort             | <p class="shorttext synchronized" lang="en">Sort file list (use const CVC_FILE_HDLR-&gt;LIST_SORTING-*)</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    get_directory_content
      IMPORTING
        iv_vh_type   TYPE zca_d_vht_dirs_files
        iv_path_file TYPE zca_d_path_n_file_name_n_ext OPTIONAL
        iv_filter    TYPE string  DEFAULT '*'
        iv_sort      TYPE char1   DEFAULT zcl_ca_c_file_utility=>list_sorting-by_time
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Get physical path and file name to a logical filename</p>
    "!
    "! @parameter iv_path_file        | <p class="shorttext synchronized" lang="en">Physical path and/or file name</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    get_physical_filename_handler
      IMPORTING
        iv_path_file TYPE zca_d_path_n_file_name_n_ext OPTIONAL      "is may be already available via e. g. GET_LOGICAL_FILENAME
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Check if any path and filename is available</p>
    "!
    "! @parameter iv_path_file        | <p class="shorttext synchronized" lang="en">Complete physical path and file name</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    is_path_file_available
      IMPORTING
        iv_path_file TYPE zca_d_path_n_file_name_n_ext
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Set physical path and file name (see docu in method)</p>
    "!
    "! @parameter iv_path_file | <p class="shorttext synchronized" lang="en">Get handler for physical path and file name</p>
    set_physical_path_filename
      IMPORTING
        iv_path_file TYPE zca_d_path_n_file_name_n_ext.

ENDINTERFACE.
