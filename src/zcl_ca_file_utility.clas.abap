"! <p class="shorttext synchronized" lang="en">CA-TBX: File utility for server + client/PC</p>
"!
"! <p>This class synergize / combine the two techniques of handling application server files and PC files
"! (= presentation server) in one class.</p>
"!
"! <p>If you want to use this class in a report with a selection screen, than please have a look into
"! <strong>class {@link ZCL_CA_FILE_UTILITY_SELSCRCTLR}</strong> to get more informations on how to use it.</p>
CLASS zcl_ca_file_utility DEFINITION PUBLIC
                                     CREATE PUBLIC.

  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message.

*   l o c a l   t y p e   d e f i n i t i o n
    TYPES:
      "! <p class="shorttext synchronized" lang="en">File infos from directory (copied from program RSWATCH0)</p>
      BEGIN OF ty_s_file_list,
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
*        status(1)   TYPE c,               " not used
      END   OF ty_s_file_list,
      ty_t_file_list TYPE STANDARD TABLE OF ty_s_file_list
                                         WITH NON-UNIQUE DEFAULT KEY,

      "! <p class="shorttext synchronized" lang="en">Counters</p>
      BEGIN OF ty_s_cnts,
        read     TYPE i,          " No. of read records
        transfer TYPE i,          " No. of transferred records
      END   OF ty_s_cnts,

      "! <p class="shorttext synchronized" lang="en">Dataset counter</p>
      BEGIN OF ty_s_dataset_cnt,
        append TYPE i,
        input  TYPE i,
        output TYPE i,
        update TYPE i,
      END   OF ty_s_dataset_cnt.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Record counters over all</p>
      ms_cnt_all     TYPE ty_s_cnts READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Dataset counters depending on operation type</p>
      ms_dataset_cnt TYPE ty_s_dataset_cnt READ-ONLY.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">File System Paths</p>
      mo_path_hdlr    TYPE REF TO cl_fs_path READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Constants for File Utility</p>
      mo_file_options TYPE REF TO zcl_ca_c_file_utility READ-ONLY,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Counters</p>
      ms_cnt          TYPE ty_s_cnts READ-ONLY,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Current codepage of either the app. server or the client/PC</p>
      mv_codepage     TYPE cpcodepage READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Complete path and file name for dataset access</p>
      mv_path_file    TYPE string READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">OS specific path separator</p>
      mv_path_sep     TYPE dmc_mds_path_separator READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Location: A = server / P = client/PC</p>
      mv_location     TYPE dxlocation READ-ONLY.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Print simple log with statistics over all activities</p>
      print_log_over_all,

      "! <p class="shorttext synchronized" lang="en">Reset statistics</p>
      reset_statistics.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Close dataset</p>
      "!
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      close_dataset
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter iv_location         | <p class="shorttext synchronized" lang="en">Location: server or client (FILE_OPTIONS->LOCATION-*)</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      constructor
        IMPORTING
          iv_location TYPE dxlocation
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Delete</p>
      "!
      "! @parameter iv_path_file        | <p class="shorttext synchronized" lang="en">Complete physical path and file name</p>
      "! @parameter iv_check_auth       | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      delete
        IMPORTING
          iv_path_file  TYPE string    OPTIONAL      "is may be already availabe via e. g. GET_LOGICAL_FILENAME
          iv_check_auth TYPE abap_bool DEFAULT abap_true
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Get file (READ from server / UPLOAD from client PC)</p>
      "!
      "! @parameter iv_path_file           | <p class="shorttext synchronized" lang="en">Complete physical path and file name</p>
      "! @parameter iv_file_mode           | <p class="shorttext synchronized" lang="en">Binary or character mode (FILE_OPTIONS-&gt;MODE-*)</p>
      "! @parameter iv_codepage            | <p class="shorttext synchronized" lang="en">Codepage (can use MV_CODEPAGE as default, see TCP00)</p>
      "! @parameter iv_has_field_separator | <p class="shorttext synchronized" lang="en">X = Fields are TAB separ. - result table needs corresp. cols</p>
      "! @parameter iv_check_auth          | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
      "! @parameter et_file                | <p class="shorttext synchronized" lang="en">File as table</p>
      "! @parameter ev_length              | <p class="shorttext synchronized" lang="en">File length</p>
      "! @raising   zcx_ca_file_utility    | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      get
        IMPORTING
          iv_path_file           TYPE string       OPTIONAL      "is may be already availabe via e. g. GET_LOGICAL_FILENAME
          iv_file_mode           TYPE swr_filetype DEFAULT zcl_ca_c_file_utility=>mode-binary
          iv_codepage            TYPE cpcodepage   OPTIONAL
          iv_check_auth          TYPE abap_bool    DEFAULT abap_true
          iv_has_field_separator TYPE abap_bool    DEFAULT abap_false
        EXPORTING
          et_file                TYPE STANDARD TABLE
          ev_length              TYPE i
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Get file OR directory list in the given directory</p>
      "!
      "! @parameter iv_path             | <p class="shorttext synchronized" lang="en">Complete physical path, may incl. file name</p>
      "! @parameter iv_filter           | <p class="shorttext synchronized" lang="en">Generic filename (may end with *)</p>
      "! @parameter iv_sort             | <p class="shorttext synchronized" lang="en">Sort file list (use const MO_FILE_OPTIONS->LIST_SORTING-*)</p>
      "! @parameter iv_vh_type          | <p class="shorttext synchronized" lang="en">Value help type (use const MO_FILE_OPTIONS->VALUE_HELP-*)</p>
      "! @parameter rt_file_list        | <p class="shorttext synchronized" lang="en">Content of directory / file list</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      get_file_list
        IMPORTING
          iv_path             TYPE string  OPTIONAL
          iv_filter           TYPE string  DEFAULT '*'
          iv_sort             TYPE char1   DEFAULT zcl_ca_c_file_utility=>list_sorting-by_time
          iv_vh_type          TYPE zca_d_vht_dirs_files DEFAULT zcl_ca_c_file_utility=>value_help-for_directories
        RETURNING
          VALUE(rt_file_list) TYPE ty_t_file_list
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Get physical path and file name to a logical filename</p>
      "!
      "! @parameter iv_logical_filename | <p class="shorttext synchronized" lang="en">Logical file name defined in TA FILE</p>
      "! @parameter iv_param_1          | <p class="shorttext synchronized" lang="en">Replacement parameter 1</p>
      "! @parameter iv_param_2          | <p class="shorttext synchronized" lang="en">Replacement parameter 2</p>
      "! @parameter iv_param_3          | <p class="shorttext synchronized" lang="en">Replacement parameter 3</p>
      "! @parameter iv_incl_dir         | <p class="shorttext synchronized" lang="en">X = Expecting / return a physical path</p>
      "! @parameter iv_with_file_ext    | <p class="shorttext synchronized" lang="en">X = Append internal file extension to file name ..e. g. BIN</p>
      "! @parameter iv_use_buffer       | <p class="shorttext synchronized" lang="en">X = Use buffer to avoid recurring data access and checks</p>
      "! @parameter rv_path_file        | <p class="shorttext synchronized" lang="en">Complete path and file name</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      get_pathfile_from_logical_name
        IMPORTING
          iv_logical_filename TYPE fileintern
          iv_param_1          TYPE clike DEFAULT space
          iv_param_2          TYPE clike DEFAULT space
          iv_param_3          TYPE clike DEFAULT space
          iv_incl_dir         TYPE abap_bool DEFAULT abap_true
          iv_with_file_ext    TYPE abap_bool DEFAULT abap_false
          iv_use_buffer       TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rv_path_file) TYPE string
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Get physical path and file name to a logical filename</p>
      "!
      "! @parameter iv_path_file        | <p class="shorttext synchronized" lang="en">Physical path and/or file name</p>
      "! @parameter ro_path_hdlr        | <p class="shorttext synchronized" lang="en">File System Paths</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      get_physical_filename_handler
        IMPORTING
          iv_path_file        TYPE string       OPTIONAL      "is may be already availabe via e. g. GET_LOGICAL_FILENAME
        RETURNING
          VALUE(ro_path_hdlr) TYPE REF TO cl_fs_path
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Open dataset</p>
      "!
      "! @parameter iv_path_file        | <p class="shorttext synchronized" lang="en">Physical path and/or file name</p>
      "! @parameter iv_file_mode        | <p class="shorttext synchronized" lang="en">Binary or character mode (FILE_OPTIONS->MODE-*)</p>
      "! @parameter iv_file_operation   | <p class="shorttext synchronized" lang="en">File operation type (FILE_OPTIONS->OPERATION-*)</p>
      "! @parameter iv_codepage         | <p class="shorttext synchronized" lang="en">Codepage (can use MV_CODEPAGE as default, see TCP00)</p>
      "! @parameter iv_check_auth       | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      open_dataset
        IMPORTING
          iv_path_file      TYPE string       OPTIONAL      "is may be already availabe via e. g. GET_LOGICAL_FILENAME
          iv_file_mode      TYPE swr_filetype DEFAULT zcl_ca_c_file_utility=>mode-text
          iv_file_operation TYPE dsetactype   DEFAULT zcl_ca_c_file_utility=>operation-input
          iv_codepage       TYPE cpcodepage   OPTIONAL
          iv_check_auth     TYPE abap_bool    DEFAULT abap_true
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Print simple log with statistics</p>
      print_log,

      "! <p class="shorttext synchronized" lang="en">Read data from file and write statistics</p>
      "!
      "! @parameter iv_length           | <p class="shorttext synchronized" lang="en">Expected length (lower 0 = complete file)</p>
      "! @parameter es_record           | <p class="shorttext synchronized" lang="en">Record for file</p>
      "! @parameter ev_length           | <p class="shorttext synchronized" lang="en">Actual length of last record</p>
      "! @parameter ev_no_record        | <p class="shorttext synchronized" lang="en">X = No more records available (leave loop)</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      read_dataset
        IMPORTING
          iv_length    TYPE i DEFAULT -1
        EXPORTING
          es_record    TYPE data
          ev_length    TYPE i
          ev_no_record TYPE abap_bool
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Set physical path and file name (see docu in method)</p>
      "!
      "! @parameter iv_path_file | <p class="shorttext synchronized" lang="en">Get handler for physical path and file name</p>
      set_physical_path_filename
        IMPORTING
          iv_path_file TYPE string,

      "! <p class="shorttext synchronized" lang="en">Transfer data to file and write statistics</p>
      "!
      "! @parameter is_record           | <p class="shorttext synchronized" lang="en">Record for file</p>
      "! @parameter iv_rec_len          | <p class="shorttext synchronized" lang="en">Length of single record (lower eq 0 = normal behaviour)</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      transfer_dataset
        IMPORTING
          is_record  TYPE simple
          iv_rec_len TYPE i DEFAULT -1
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Write file (TRANSFER to server / DOWNLOAD on client/PC)</p>
      "!
      "! @parameter iv_path_file              | <p class="shorttext synchronized" lang="en">Complete physical path and file name</p>
      "! @parameter iv_file_mode              | <p class="shorttext synchronized" lang="en">Binary or character mode (FILE_OPTIONS->MODE-*)</p>
      "! @parameter iv_check_auth             | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
      "! @parameter iv_write_lf               | <p class="shorttext synchronized" lang="en">X = Add CR/LF at end of CHAR lines (PC only)</p>
      "! @parameter iv_confirm_overwrite      | <p class="shorttext synchronized" lang="en">X = Confirm overwriting file (PC only)</p>
      "! @parameter iv_write_field_separator  | <p class="shorttext synchronized" lang="en">X = Separate fields by horizontal tabulator (PC only)</p>
      "! @parameter iv_trunc_trail_blanks     | <p class="shorttext synchronized" lang="en">X = Truncate trailing blanks at end of CHAR fields (PC only)</p>
      "! @parameter iv_trunc_trail_blanks_eol | <p class="shorttext synchronized" lang="en">X = Trunc. trailing blanks at end of the last col. (PC only)</p>
      "! @parameter iv_file_operation         | <p class="shorttext synchronized" lang="en">File operation type (FILE_OPTIONS->OPERATION-*)</p>
      "! @parameter iv_codepage               | <p class="shorttext synchronized" lang="en">Codepage (can use MV_CODEPAGE as default, see TCP00)</p>
      "! @parameter iv_length                 | <p class="shorttext synchronized" lang="en">Write file of this length</p>
      "! @parameter ev_length                 | <p class="shorttext synchronized" lang="en">Transmitted length</p>
      "! @parameter ct_file                   | <p class="shorttext synchronized" lang="en">File as table</p>
      "! @raising   zcx_ca_file_utility       | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      write
        IMPORTING
          iv_path_file              TYPE string       OPTIONAL      "is may be already availabe via e. g. GET_LOGICAL_FILENAME
          iv_file_mode              TYPE swr_filetype DEFAULT zcl_ca_c_file_utility=>mode-binary
          iv_check_auth             TYPE abap_bool    DEFAULT abap_true
          iv_write_lf               TYPE abap_bool    DEFAULT abap_true
          iv_confirm_overwrite      TYPE abap_bool    DEFAULT abap_false
          iv_write_field_separator  TYPE abap_bool    DEFAULT abap_false
          iv_trunc_trail_blanks     TYPE abap_bool    DEFAULT abap_true
          iv_trunc_trail_blanks_eol TYPE abap_bool    DEFAULT abap_true
          iv_file_operation         TYPE dsetactype   DEFAULT zcl_ca_c_file_utility=>operation-output
          iv_codepage               TYPE cpcodepage   OPTIONAL
          iv_length                 TYPE i            OPTIONAL
        EXPORTING
          ev_length                 TYPE i
        CHANGING
          !ct_file                  TYPE STANDARD TABLE
        RAISING
          zcx_ca_file_utility.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e         FOR  if_xo_const_message~error,
      c_msgty_i         FOR  if_xo_const_message~info,
      c_msgty_s         FOR  if_xo_const_message~success,
      c_msgty_w         FOR  if_xo_const_message~warning.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">OS of location (conform for logical filename)</p>
      mv_opsys    TYPE syst_opsys.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check authority of path and file</p>
      "!
      "! @parameter iv_file_operation   | <p class="shorttext synchronized" lang="en">File operation type (FILE_OPTIONS->OPERATION-*)</p>
      "! @parameter iv_check_auth       | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      authority_check
        IMPORTING
          iv_file_operation TYPE dsetactype
          iv_check_auth     TYPE abap_bool
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Check result of OPEN DATASET and raise exception</p>
      "!
      "! @parameter iv_subrc            | <p class="shorttext synchronized" lang="en">Return code of OPEN DATASET statement</p>
      "! @parameter iv_msg              | <p class="shorttext synchronized" lang="en">Message string of OPEN DATASET statement</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      check_result_of_open_dataset
        IMPORTING
          VALUE(iv_subrc) TYPE syst_subrc
          iv_msg          TYPE bapi_msg
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Download a file to the PC/client</p>
      "!
      "! @parameter iv_file_mode              | <p class="shorttext synchronized" lang="en">Binary or character mode (FILE_OPTIONS->MODE-*)</p>
      "! @parameter iv_check_auth             | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
      "! @parameter iv_write_lf               | <p class="shorttext synchronized" lang="en">X = Add CR/LF at end of CHAR lines</p>
      "! @parameter iv_confirm_overwrite      | <p class="shorttext synchronized" lang="en">X = Confirm overwriting file</p>
      "! @parameter iv_write_field_separator  | <p class="shorttext synchronized" lang="en">X = Separate fields by horizontal tabulator</p>
      "! @parameter iv_trunc_trail_blanks     | <p class="shorttext synchronized" lang="en">X = Truncate trailing blanks at end of CHAR fields</p>
      "! @parameter iv_trunc_trail_blanks_eol | <p class="shorttext synchronized" lang="en">X = Truncate trailing blanks at end of the last column</p>
      "! @parameter iv_file_operation         | <p class="shorttext synchronized" lang="en">File operation type (FILE_OPTIONS->OPERATION-*)</p>
      "! @parameter iv_codepage               | <p class="shorttext synchronized" lang="en">Codepage (can use MV_CODEPAGE as default, see TCP00)</p>
      "! @parameter iv_length                 | <p class="shorttext synchronized" lang="en">Write file of this length</p>
      "! @parameter ev_length                 | <p class="shorttext synchronized" lang="en">Transmitted length</p>
      "! @parameter ct_file                   | <p class="shorttext synchronized" lang="en">File as table</p>
      "! @raising   zcx_ca_file_utility       | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      download
        IMPORTING
          iv_file_mode              TYPE swr_filetype
          iv_check_auth             TYPE abap_bool
          iv_write_lf               TYPE abap_bool  DEFAULT abap_true
          iv_confirm_overwrite      TYPE abap_bool  DEFAULT abap_false
          iv_write_field_separator  TYPE abap_bool  DEFAULT abap_false
          iv_trunc_trail_blanks     TYPE abap_bool  DEFAULT abap_true
          iv_trunc_trail_blanks_eol TYPE abap_bool  DEFAULT abap_true
          iv_file_operation         TYPE dsetactype
          iv_codepage               TYPE cpcodepage OPTIONAL
          iv_length                 TYPE i          OPTIONAL
        EXPORTING
          ev_length                 TYPE i
        CHANGING
          !ct_file                  TYPE STANDARD TABLE
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Get file list of directory on application server</p>
      "!
      "! This method is a slightly adapted copy of FORM FILL_FILE_LIST of program RSWATCH0 (AL11)
      "!
      "! @parameter iv_path             | <p class="shorttext synchronized" lang="en">Directory name</p>
      "! @parameter iv_filter           | <p class="shorttext synchronized" lang="en">Generic filename (may end with *)</p>
      "! @parameter iv_vh_type          | <p class="shorttext synchronized" lang="en">Value help type (use const MO_FILE_OPTIONS->VALUE_HELP-*)</p>
      "! @parameter rt_file_list        | <p class="shorttext synchronized" lang="en">Content of directory / file list</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      get_file_list_of_as
        IMPORTING
          iv_path             TYPE string
          iv_filter           TYPE string
          iv_vh_type          TYPE zca_d_vht_dirs_files
        RETURNING
          VALUE(rt_file_list) TYPE ty_t_file_list
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Get file list of directory on PC</p>
      "!
      "! @parameter iv_path             | <p class="shorttext synchronized" lang="en">Directory name</p>
      "! @parameter iv_filter           | <p class="shorttext synchronized" lang="en">Generic filename (may end with *)</p>
      "! @parameter iv_vh_type          | <p class="shorttext synchronized" lang="en">Value help type (use const MO_FILE_OPTIONS->VALUE_HELP-*)</p>
      "! @parameter rt_file_list        | <p class="shorttext synchronized" lang="en">Content of directory / file list</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      get_file_list_of_pc
        IMPORTING
          iv_path             TYPE string
          iv_filter           TYPE string
          iv_vh_type          TYPE zca_d_vht_dirs_files
        RETURNING
          VALUE(rt_file_list) TYPE ty_t_file_list
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Determine structure / field length of a record</p>
      "!
      "! @parameter iv_file_mode        | <p class="shorttext synchronized" lang="en">Binary or character mode (FILE_OPTIONS->MODE-*)</p>
      "! @parameter is_record           | <p class="shorttext synchronized" lang="en">Data object to determine length</p>
      "! @parameter rv_rec_len          | <p class="shorttext synchronized" lang="en">Transmitted length</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      get_records_length
        IMPORTING
          iv_file_mode      TYPE swr_filetype
          is_record         TYPE data
        RETURNING
          VALUE(rv_rec_len) TYPE i
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Check if any path and filename is available</p>
      "!
      "! @parameter iv_path_file        | <p class="shorttext synchronized" lang="en">Complete physical path and file name</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      is_path_file_available
        IMPORTING
          iv_path_file TYPE string
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Upload a file from the PC/client</p>
      "!
      "! @parameter iv_file_mode           | <p class="shorttext synchronized" lang="en">Binary or character mode (FILE_OPTIONS->MODE-*)</p>
      "! @parameter iv_codepage            | <p class="shorttext synchronized" lang="en">Codepage (can use MV_CODEPAGE as default, see TCP00)</p>
      "! @parameter iv_has_field_separator | <p class="shorttext synchronized" lang="en">X = Field are TAB separated, result table needs corresp cols</p>
      "! @parameter et_file                | <p class="shorttext synchronized" lang="en">File as table</p>
      "! @parameter ev_length              | <p class="shorttext synchronized" lang="en">File length</p>
      "! @raising   zcx_ca_file_utility    | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      upload
        IMPORTING
          iv_file_mode           TYPE swr_filetype
          iv_codepage            TYPE cpcodepage OPTIONAL
          iv_has_field_separator TYPE abap_bool  DEFAULT abap_false
        EXPORTING
          et_file                TYPE STANDARD TABLE
          ev_length              TYPE i
        RAISING
          zcx_ca_file_utility,

      "! <p class="shorttext synchronized" lang="en">Write file from application server</p>
      "!
      "! @parameter iv_file_mode        | <p class="shorttext synchronized" lang="en">Binary or character mode (FILE_OPTIONS->MODE-*)</p>
      "! @parameter iv_length           | <p class="shorttext synchronized" lang="en">File length</p>
      "! @parameter it_file             | <p class="shorttext synchronized" lang="en">File as table</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      write_file
        IMPORTING
          iv_file_mode TYPE swr_filetype
          iv_length    TYPE i
          it_file      TYPE STANDARD TABLE
        RAISING
          zcx_ca_file_utility
          zcx_ca_param.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Determine several location depending parameters</p>
      "!
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      determine_location_parameters
        RAISING
          zcx_ca_file_utility.

ENDCLASS.



CLASS ZCL_CA_FILE_UTILITY IMPLEMENTATION.

  METHOD authority_check.
    "---------------------------------------------------------------------*
    "     Check authority of path and file
    "---------------------------------------------------------------------*
    IF iv_check_auth EQ abap_false.
      RETURN.
    ENDIF.

    mo_file_options->is_operation_valid( iv_file_operation ).

    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
      EXPORTING
        filename         = CONV fileextern( mv_path_file )
        activity         = SWITCH char20(
                              iv_file_operation
                                WHEN mo_file_options->operation-input
                                  THEN sabc_act_read

                                WHEN mo_file_options->operation-output OR
                                     mo_file_options->operation-append OR
                                     mo_file_options->operation-update
                                  THEN sabc_act_write

                                WHEN mo_file_options->operation-delete
                                  THEN sabc_act_delete )
      EXCEPTIONS
        no_authority     = 1
        activity_unknown = 2
        OTHERS           = 3.
    CASE sy-subrc.
      WHEN 0.
        "everything is fine

      WHEN 1.
        DATA(lv_path_file) = CONV bapi_msg( mv_path_file ).
        "No authority accessing file '&1&2&3&4'
        RAISE EXCEPTION TYPE zcx_ca_file_utility
          EXPORTING
            textid   = zcx_ca_file_utility=>no_auth_for_path
            mv_msgty = c_msgty_e
            mv_msgv1 = lv_path_file(50)
            mv_msgv2 = lv_path_file+50(50)
            mv_msgv3 = lv_path_file+100(50)
            mv_msgv4 = lv_path_file+150(50).

      WHEN OTHERS.
        DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_function = 'AUTHORITY_CHECK_DATASET'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "authority_check


  METHOD check_result_of_open_dataset.
    "-----------------------------------------------------------------*
    "   Check result of OPEN DATASET and raise exception
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_msg               TYPE bapi_msg.

    IF iv_subrc EQ 0.
      RETURN.
    ENDIF.

    "Opening the requested file results in an error

    "Either send the message of the OPEN DATASET statement or ...
    IF iv_msg IS NOT INITIAL.
      "To fill SY-MSG++ fields for exception creation
      lv_msg = |{ iv_msg } / => { mv_path_file }|.
      MESSAGE e897(s1) WITH lv_msg(50)     lv_msg+50(50)
                            lv_msg+100(50) lv_msg+150(50) INTO lv_msg.
      DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'ZCL_CA_FILE_UTIL'
                                                        iv_method   = 'OPEN'
                                                        iv_subrc    = iv_subrc ) ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.

    ELSE.
      "... the path or file does NOT exist
      "File &1&2&3 could not be opened or does not exist
      lv_msg = mv_path_file.
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>open_not_possible
          mv_msgty = c_msgty_e
          mv_msgv1 = lv_msg(50)
          mv_msgv2 = lv_msg+50(50)
          mv_msgv3 = lv_msg+100(50).
    ENDIF.
  ENDMETHOD.                    "check_result_of_open_dataset


  METHOD close_dataset.
    "---------------------------------------------------------------------*
    "     Close dataset
    "---------------------------------------------------------------------*
    TRY.
        IF mv_location EQ mo_file_options->location-pc.
          "Method &1 is not allowed for presentation server / client/PC
          RAISE EXCEPTION TYPE zcx_ca_file_utility
            EXPORTING
              textid   = zcx_ca_file_utility=>not_allowed_for_client
              mv_msgty = c_msgty_e
              mv_msgv1 = 'CLOSE' ##no_text.
        ENDIF.

        "May implement a locking logic for appl. server files -> further details are here:
        "https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abenfile_interface_locking.htm

        CLOSE DATASET mv_path_file.

      CATCH cx_sy_file_access_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "close_dataset


  METHOD constructor.
    "---------------------------------------------------------------------*
    "     Constructor
    "---------------------------------------------------------------------*
    mo_file_options = zcl_ca_c_file_utility=>get_instance( ).

    mo_file_options->is_location_valid( iv_location ).
    mv_location = iv_location.

    determine_location_parameters( ).
  ENDMETHOD.                    "Constructor


  METHOD delete.
    "---------------------------------------------------------------------*
    "     Delete dataset
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error TYPE REF TO zcx_ca_file_utility,
      lv_rc    TYPE syst_subrc ##needed.

    TRY.
        is_path_file_available( iv_path_file ).

        CASE mv_location.
          WHEN mo_file_options->location-server.
            "A p p l i c a t i o n   s e r v e r
            authority_check( iv_file_operation = mo_file_options->operation-delete
                             iv_check_auth     = iv_check_auth ).

            DELETE DATASET mv_path_file.
            IF sy-subrc NE 0.
              DATA(lv_path_file_msg) = CONV char200( mv_path_file ).
              "File &1&2&3 could not be deleted from file system
              RAISE EXCEPTION TYPE zcx_ca_file_utility
                EXPORTING
                  textid   = zcx_ca_file_utility=>delete_not_possible
                  mv_msgty = c_msgty_e
                  mv_msgv1 = lv_path_file_msg(50)
                  mv_msgv2 = lv_path_file_msg+50(50)
                  mv_msgv3 = lv_path_file_msg+100(50).
            ENDIF.

          WHEN mo_file_options->location-pc.
            "P r e s e n t a t i o n   s e r v e r   /   c l i e n t / P C
            cl_gui_frontend_services=>file_delete(
              EXPORTING
                filename             = mv_path_file
              CHANGING
                rc                   = lv_rc
              EXCEPTIONS
                file_delete_failed   = 1
                cntl_error           = 2
                error_no_gui         = 3
                file_not_found       = 4
                access_denied        = 5
                unknown_error        = 6
                not_supported_by_gui = 7
                wrong_parameter      = 8
                OTHERS               = 9 ).
            IF sy-subrc NE 0.
              lx_error = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                        iv_method   = 'FILE_DELETE'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
              IF lx_error IS BOUND.
                RAISE EXCEPTION lx_error.
              ENDIF.
            ENDIF.
        ENDCASE.

      CATCH cx_sy_file_access_error INTO DATA(lx_catched).
        lx_error = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "delete


  METHOD determine_location_parameters.
    "---------------------------------------------------------------------*
    "     Determine several location depending parameters
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error    TYPE REF TO zcx_ca_file_utility,
      lv_encoding TYPE abap_encod,
      lv_rc       TYPE syst_subrc.

    CASE mv_location.
      WHEN mo_file_options->location-server.
        "A p p l i c a t i o n   s e r v e r
        mv_opsys = sy-opsys.

        mv_codepage = cl_abap_codepage=>current( sap_name = abap_true ).

        CALL FUNCTION 'DMC_MDS_GET_PATHSEPARATOR'
          IMPORTING
            ev_path_separator     = mv_path_sep
          EXCEPTIONS
            opsys_not_supported   = 1
            filesys_not_supported = 2
            OTHERS                = 3.
        IF sy-subrc NE 0.
          lx_error = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_function = 'DMC_MDS_GET_PATHSEPARATOR'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
          IF lx_error IS BOUND.
            RAISE EXCEPTION lx_error.
          ENDIF.
        ENDIF.

      WHEN mo_file_options->location-pc.
        "P r e s e n t a t i o n   s e r v e r   /   c l i e n t / P C
        cl_gui_frontend_services=>get_platform(
          RECEIVING
            platform             = DATA(lv_platform)
          EXCEPTIONS
            error_no_gui         = 1
            cntl_error           = 2
            not_supported_by_gui = 3
            OTHERS               = 4 ).
        IF sy-subrc NE 0.
          lx_error = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                        iv_method   = 'GET_PLATFORM'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
          IF lx_error IS BOUND.
            RAISE EXCEPTION lx_error.
          ENDIF.
        ENDIF.

        CASE lv_platform.
          WHEN cl_gui_frontend_services=>platform_windows95 OR
               cl_gui_frontend_services=>platform_windows98 OR
               cl_gui_frontend_services=>platform_nt351     OR
               cl_gui_frontend_services=>platform_nt40      OR
               cl_gui_frontend_services=>platform_nt50      OR
               cl_gui_frontend_services=>platform_windowsxp.
            mv_opsys    = 'Windows NT' ##no_text.
            mv_path_sep = '\'.

          WHEN cl_gui_frontend_services=>platform_mac.
            mv_opsys    = 'MC' ##no_text.
            mv_path_sep = ':'.

          WHEN cl_gui_frontend_services=>platform_os2.
            mv_opsys    = 'PM' ##no_text.
            mv_path_sep = '\'.

          WHEN cl_gui_frontend_services=>platform_linux   OR
               cl_gui_frontend_services=>platform_hpux    OR
               cl_gui_frontend_services=>platform_tru64   OR
               cl_gui_frontend_services=>platform_aix     OR
               cl_gui_frontend_services=>platform_solaris OR
               cl_gui_frontend_services=>platform_macosx.
            mv_opsys    = 'Linux' ##no_text.
            mv_path_sep = '/'.

          WHEN OTHERS.
            "Operating system & is not supported
            RAISE EXCEPTION TYPE zcx_ca_file_utility
              EXPORTING
                textid   = zcx_ca_file_utility=>os_not_supported
                mv_msgty = c_msgty_e
                mv_msgv1 = CONV #( lv_platform ).
        ENDCASE.

        "Get codepage of client / PC
        cl_gui_frontend_services=>get_saplogon_encoding(
          CHANGING
            file_encoding                 = lv_encoding
            rc                            = lv_rc
          EXCEPTIONS
            cntl_error                    = 1
            error_no_gui                  = 2
            not_supported_by_gui          = 3
            cannot_initialize_globalstate = 4
            OTHERS                        = 5  ).
        IF sy-subrc NE 0.
          lx_error = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                        iv_method   = 'GET_SAPLOGON_ENCODING'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
          IF lx_error IS BOUND.
            RAISE EXCEPTION lx_error.
          ENDIF.
        ENDIF.

        TRY.
            DATA(lv_codepage) = CONV string( lv_encoding ).
            IF strlen( lv_codepage ) EQ 4             AND
               lv_codepage           CO '0123456789'.
              mv_codepage = lv_codepage.

            ELSE.
              mv_codepage = cl_abap_codepage=>sap_codepage( lv_codepage ).
            ENDIF.

          CATCH cx_parameter_invalid INTO DATA(lx_catched).
            lx_error = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'CL_ABAP_CODEPAGE'
                                                        iv_method   = 'SAP_CODEPAGE'
                                                        ix_error    = lx_catched ) )  ##no_text.
            IF lx_error IS BOUND.
              RAISE EXCEPTION lx_error.
            ENDIF.
        ENDTRY.
    ENDCASE.
  ENDMETHOD.                    "determine_location_parameters


  METHOD download.
    "---------------------------------------------------------------------*
    "     Download a file to the PC/client
    "---------------------------------------------------------------------*
    CLEAR ev_length.

    mo_file_options->is_mode_valid( iv_file_mode ).
    mo_file_options->is_operation_valid( iv_file_operation ).

    DATA(lv_codepage) = CONV abap_encod( iv_codepage ).
    IF lv_codepage EQ '0000'.
      CLEAR lv_codepage.
    ENDIF.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  = mv_path_file
        bin_filesize              = iv_length
        filetype                  = SWITCH #( iv_file_mode
                                      WHEN mo_file_options->mode-binary THEN 'BIN'
                                      WHEN mo_file_options->mode-text   THEN 'ASC' ) ##no_text
        codepage                  = lv_codepage
        append                    = xsdbool( iv_file_operation EQ mo_file_options->operation-append )
        write_field_separator     = iv_write_field_separator
        no_auth_check             = xsdbool( iv_check_auth EQ abap_false )
        trunc_trailing_blanks     = iv_trunc_trail_blanks
        trunc_trailing_blanks_eol = iv_trunc_trail_blanks_eol
        write_lf                  = iv_write_lf
        confirm_overwrite         = iv_confirm_overwrite
      IMPORTING
        filelength                = ev_length
      CHANGING
        data_tab                  = ct_file
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                        iv_method   = 'GUI_DOWNLOAD'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "download


  METHOD get.
    "---------------------------------------------------------------------*
    "     Get file (READ from server / UPLOAD from client PC)
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      lr_s_record    TYPE REF TO data.

    FIELD-SYMBOLS:
      <ls_record>    TYPE data.

    mo_file_options->is_mode_valid( iv_file_mode ).

    is_path_file_available( iv_path_file ).

    CASE mv_location.
      WHEN mo_file_options->location-pc.
        "P r e s e n t a t i o n   s e r v e r   /   c l i e n t / P C
        upload(
            EXPORTING
              iv_file_mode           = iv_file_mode
              iv_codepage            = iv_codepage
              iv_has_field_separator = iv_has_field_separator
            IMPORTING
              et_file                = et_file
              ev_length              = ev_length ).

      WHEN mo_file_options->location-server.
        "A p p l i c a t i o n   s e r v e r
        open_dataset( iv_file_mode      = iv_file_mode
                      iv_file_operation = mo_file_options->operation-input
                      iv_codepage       = iv_codepage
                      iv_check_auth     = iv_check_auth ).

        "Create workarea for outbound table
        CREATE DATA lr_s_record LIKE LINE OF et_file.
        ASSIGN lr_s_record->* TO <ls_record>.

        "Read complete file for return
        CLEAR: et_file,
               ev_length.
        DO.
          read_dataset(
                  IMPORTING
                    es_record    = <ls_record>
                    ev_no_record = DATA(lv_no_record)
                    ev_length    = DATA(lv_act_length) ).

          "Add actual length to complete length
          ev_length = ev_length + lv_act_length.
          "Append line of file to internal table
          APPEND <ls_record> TO et_file.

          IF lv_no_record EQ abap_true.
            EXIT.
          ENDIF.
        ENDDO.

        close_dataset( ).
    ENDCASE.
  ENDMETHOD.                    "get


  METHOD get_file_list.
    "-----------------------------------------------------------------*
    "   Get file list of directory
    "-----------------------------------------------------------------*
    mo_file_options->is_list_sorting_valid( iv_sort ).
    mo_file_options->is_value_help_type_valid( iv_vh_type ).

    get_physical_filename_handler( iv_path ).
    DATA(lv_path) = mo_path_hdlr->get_path_name( ).

    CASE mv_location.
      WHEN mo_file_options->location-server.
        rt_file_list = get_file_list_of_as( iv_path    = lv_path
                                            iv_filter  = iv_filter
                                            iv_vh_type = iv_vh_type ).

      WHEN mo_file_options->location-pc.
        rt_file_list = get_file_list_of_pc( iv_path    = lv_path
                                            iv_filter  = iv_filter
                                            iv_vh_type = iv_vh_type ).
    ENDCASE.

    IF rt_file_list IS INITIAL.
      "No files found in directory &1
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>no_files_found
          mv_msgty = c_msgty_s
          mv_msgv1 = CONV #( lv_path ).
    ENDIF.

    CASE iv_sort.
      WHEN mo_file_options->list_sorting-by_time.
        SORT rt_file_list BY mtime DESCENDING name ASCENDING.
      WHEN mo_file_options->list_sorting-by_name.
        SORT rt_file_list BY name ASCENDING mtime DESCENDING.
    ENDCASE.
  ENDMETHOD.                    "get_file_list


  METHOD get_file_list_of_as.
    "-----------------------------------------------------------------*
    "   This method is an adapted copy of FORM FILL_FILE_LIST of
    "   program RSWATCH0 (= TA AL11)
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lra_filter   TYPE RANGE OF string,
      ls_file_list TYPE ty_s_file_list,
      lv_errcnt    TYPE p LENGTH 2 DECIMALS 0 VALUE 0,
      lv_errno     TYPE ty_s_file_list-errno,
      lv_errmsg    TYPE ty_s_file_list-errmsg.

    "Directory must be set
    IF iv_path IS INITIAL.
      "Directory name &1 is empty or does not exist
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>invalid_directory
          mv_msgv1 = space.
    ENDIF.

    mo_file_options->is_value_help_type_valid( iv_vh_type ).

    DATA(lv_path)   = CONV dirname_al11( iv_path ).

    DATA(lo_sel_options) = zcl_ca_c_sel_options=>get_instance( ).
    lra_filter = VALUE #( sign   = lo_sel_options->sign-incl
                          option = lo_sel_options->option-cp
                        ( low    = iv_filter )
                        ( low    = |{ iv_filter CASE = LOWER }| )
                        ( low    = |{ iv_filter CASE = UPPER }| ) ).

    "Just to be sure
    CALL 'C_DIR_READ_FINISH' ID 'ERRNO'  FIELD lv_errno
                             ID 'ERRMSG' FIELD lv_errmsg. "#EC CI_CCALL

    CALL 'C_DIR_READ_START' ID 'DIR'    FIELD lv_path
                            ID 'FILE'   FIELD '*'
                            ID 'ERRNO'  FIELD ls_file_list-errno
                            ID 'ERRMSG' FIELD ls_file_list-errmsg. "#EC CI_CCALL
    IF sy-subrc NE 0.
      "& <- CALL &(&,&,..)
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>call_error
          mv_msgty = c_msgty_e
          mv_msgv1 = CONV #( lv_errmsg )
          mv_msgv2 = 'C_DIR_READ_START'
          mv_msgv3 = CONV #( ls_file_list-errno )
          mv_msgv4 = CONV #( ls_file_list-errmsg ) ##no_text.
    ENDIF.

    DO.
      CLEAR ls_file_list.
      CALL 'C_DIR_READ_NEXT' ID 'TYPE'   FIELD ls_file_list-type
                             ID 'NAME'   FIELD ls_file_list-name
                             ID 'LEN'    FIELD ls_file_list-len
                             ID 'OWNER'  FIELD ls_file_list-owner
                             ID 'MTIME'  FIELD ls_file_list-mtime
                             ID 'MODE'   FIELD ls_file_list-mode
                             ID 'ERRNO'  FIELD ls_file_list-errno
                             ID 'ERRMSG' FIELD ls_file_list-errmsg. "#EC CI_CCALL

      ls_file_list-dirname = iv_path.
      ls_file_list-subrc   = sy-subrc.

      CASE sy-subrc.
        WHEN 0.
          CLEAR: ls_file_list-errno,
                 ls_file_list-errmsg.
          IF ls_file_list-type(1) CO 'Ff' ##no_text.      "= normal file
            ls_file_list-useable = xsdbool( ls_file_list-name(4) NE 'core' ) ##no_text.
          ELSE.
            " directory, device, fifo, socket,...
            ls_file_list-useable = abap_false.
          ENDIF.

          IF ls_file_list-len EQ 0.
            ls_file_list-useable = abap_false.
          ENDIF.

          "Skip all entries that are not requested
          IF ls_file_list-name NOT IN lra_filter.
            CONTINUE.
          ENDIF.

          IF ( iv_vh_type           EQ mo_file_options->value_help-for_directories AND
               ls_file_list-type    NE 'directory'      )                           OR

             ( iv_vh_type           EQ mo_file_options->value_help-for_files       AND
               ls_file_list-type    EQ 'directory'      )                           OR

             "Don't offer anything that is no file and no directory
             ( ls_file_list-useable EQ abap_false                                  AND
               ls_file_list-type    NE 'directory' ) ##no_text.
            CONTINUE.
          ENDIF.

        WHEN 1.
          "No more slots available.
          EXIT.

        WHEN 5.
          "Only NAME is valid due to internal error.
          CLEAR: ls_file_list-type,  ls_file_list-len,    ls_file_list-owner,  ls_file_list-mtime,
                 ls_file_list-mode,  ls_file_list-errno,  ls_file_list-errmsg.
          ls_file_list-useable = abap_false.

        WHEN OTHERS.                     " SY-SUBRC >= 2
          "possible other return codes (sapaci2.c)
          "3 ... Internal error.
          "4 ... NAME is truncated (Warning only)
          ADD 1 TO lv_errcnt.

          "don't list files with error
          IF ls_file_list-subrc EQ 3.
            CONTINUE.
          ENDIF.
      ENDCASE.

      PERFORM p6_to_date_time_tz IN PROGRAM rstr0400
                                      USING ls_file_list-mtime
                                            ls_file_list-mod_time
                                            ls_file_list-mod_date.

      "Set extension and name without extension
      IF ls_file_list-type CS 'file' ##no_text.
        TRY.
            DATA(lo_path_hdlr) = cl_fs_path=>create( name      = mv_path_file && mv_path_sep && ls_file_list-name
                                                     path_kind = cl_fs_path=>path_kind_from_opsys( mv_opsys ) ).

            ls_file_list-name_wo_ext = lo_path_hdlr->get_file_base_name( ).
            ls_file_list-ext         = shift_left( val    = |{ lo_path_hdlr->get_file_extension( ) CASE = UPPER }|
                                                   places = 1 ).

          CATCH cx_sy_range_out_of_bounds.
            "File has no extension
            CLEAR ls_file_list-ext.

          CATCH cx_smart_path_syntax INTO DATA(lx_catched).
            ls_file_list-name_wo_ext = lx_catched->get_text( ).
        ENDTRY.
      ENDIF.

      APPEND ls_file_list TO rt_file_list.
    ENDDO.

    CALL 'C_DIR_READ_FINISH' ID 'ERRNO'  FIELD lv_errno
                             ID 'ERRMSG' FIELD lv_errmsg. "#EC CI_CCALL
    IF sy-subrc NE 0.
      WRITE: / 'C_DIR_READ_FINISH', 'SUBRC', sy-subrc ##no_text.
    ENDIF.

    IF lv_errcnt GT 0.
      MESSAGE s217(s1) WITH lv_errcnt.
    ENDIF.
  ENDMETHOD.                    "get_file_list_of_as


  METHOD get_file_list_of_pc.
    "-----------------------------------------------------------------*
    "   Get list of PC directory and prepare for return
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_flist     TYPE rstt_t_files,
      ls_file_list TYPE ty_s_file_list,
      lv_file_cnt  TYPE epsfilsiz ##needed.

    mo_file_options->is_value_help_type_valid( iv_vh_type ).

    cl_gui_frontend_services=>directory_list_files(
      EXPORTING
        directory                   = iv_path
        filter                      = iv_filter
        files_only                  = xsdbool( iv_vh_type EQ mo_file_options->value_help-for_files )
        directories_only            = xsdbool( iv_vh_type EQ mo_file_options->value_help-for_directories )
      CHANGING
        file_table                  = lt_flist
        count                       = lv_file_cnt
      EXCEPTIONS
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4
        not_supported_by_gui        = 5
        OTHERS                      = 6 ).
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                        iv_method   = 'DIRECTORY_LIST_FILES'
                                                        iv_subrc    = sy-subrc ) ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    "Map / convert directory entries into result parameter
    LOOP AT lt_flist INTO DATA(ls_flist).
      CLEAR ls_file_list.
      ls_file_list-dirname  = iv_path.
      ls_file_list-name     = ls_flist-filename.
      ls_file_list-len      = ls_flist-filelength.
      ls_file_list-mod_date = ls_flist-createdate.
      ls_file_list-mod_time = ls_flist-createtime.

      IF ls_file_list-name CA '.'.
        "Get extension
        SPLIT ls_file_list-name AT '.'
                   INTO TABLE DATA(lt_fn_splitt) IN CHARACTER MODE.
        READ TABLE lt_fn_splitt INTO  DATA(lv_filename)
                                INDEX lines( lt_fn_splitt ).
        ls_file_list-ext = to_upper( CONV saedoktyp( lv_filename ) ).
        TRY.
            "Get file name without extension
            DATA(lv_len) = find( val  = ls_file_list-name
                                 sub  = '.' && ls_file_list-ext
                                 case = abap_false ).
            IF lv_len GE 0.
              ls_file_list-name_wo_ext = ls_file_list-name(lv_len).
            ELSE.
              ls_file_list-name_wo_ext = ls_file_list-name.
            ENDIF.

          CATCH cx_sy_strg_par_val.
            ls_file_list-name_wo_ext = ls_file_list-name.
        ENDTRY.
      ENDIF.

      APPEND ls_file_list TO rt_file_list.
    ENDLOOP.
  ENDMETHOD.                    "get_file_list_of_pc


  METHOD get_pathfile_from_logical_name.
    "---------------------------------------------------------------------*
    "     Get physical path and file name to a logical filename
    "---------------------------------------------------------------------*
    CALL FUNCTION 'FILE_GET_NAME_AND_VALIDATE'
      EXPORTING
        logical_filename    = iv_logical_filename
        operating_system    = mv_opsys
        parameter_1         = iv_param_1
        parameter_2         = iv_param_2
        parameter_3         = iv_param_3
        including_dir       = iv_incl_dir
        with_file_extension = iv_with_file_ext
        use_buffer          = iv_use_buffer
      IMPORTING
        file_name           = rv_path_file
      EXCEPTIONS
        file_not_found      = 1
        validation_failed   = 2
        incorrect_path      = 3
        OTHERS              = 4.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_function = 'FILE_GET_NAME_AND_VALIDATE'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    "Create physical filename handler
    get_physical_filename_handler( rv_path_file ).
  ENDMETHOD.                    "get_pathfile_from_logical_name


  METHOD get_physical_filename_handler.
    "---------------------------------------------------------------------*
    "     Get handler for physical path and file name
    "---------------------------------------------------------------------*
    TRY.
        is_path_file_available( iv_path_file ).

        IF mo_path_hdlr IS NOT BOUND.
          mo_path_hdlr = cl_fs_path=>create( name      = mv_path_file
                                             path_kind = cl_fs_path=>path_kind_from_opsys( mv_opsys ) ).
        ENDIF.

        ro_path_hdlr = mo_path_hdlr.

      CATCH cx_smart_path_syntax INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_physical_filename_handler


  METHOD get_records_length.
    "---------------------------------------------------------------------*
    "     Determine structure / field length of a record
    "---------------------------------------------------------------------*
    mo_file_options->is_mode_valid( iv_file_mode ).

    CASE iv_file_mode.
      WHEN mo_file_options->mode-binary.
        DESCRIBE FIELD is_record LENGTH rv_rec_len IN BYTE MODE.

      WHEN mo_file_options->mode-text.
        DESCRIBE FIELD is_record LENGTH rv_rec_len IN CHARACTER MODE.
    ENDCASE.
  ENDMETHOD.                    "get_records_length


  METHOD is_path_file_available.
    "---------------------------------------------------------------------*
    "     Check if any path and filename is available
    "---------------------------------------------------------------------*
    IF iv_path_file IS INITIAL AND
       mv_path_file IS INITIAL.
      "At least one of the following parameters must be passed: &1 &2 &3 &4
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>at_least_one
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IV_PATH_FILE'
          mv_msgv2 = 'MV_PATH_FILE' ##no_text.

    ELSEIF iv_path_file IS NOT INITIAL AND
           mv_path_file NE iv_path_file.
      "Set new path
      mv_path_file = iv_path_file.
      CLEAR mo_path_hdlr.
    ENDIF.
  ENDMETHOD.                    "is_path_file_available


  METHOD open_dataset.
    "---------------------------------------------------------------------*
    "     Open dataset
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_msg     TYPE bapi_msg,
      lx_fileacc TYPE REF TO zcx_ca_file_utility.

    TRY.
        IF mv_location NE mo_file_options->location-server.
          "Method &1 is not allowed at the client/PC
          RAISE EXCEPTION TYPE zcx_ca_file_utility
            EXPORTING
              textid   = zcx_ca_file_utility=>not_allowed_for_client
              mv_msgty = c_msgty_e
              mv_msgv1 = 'OPEN_DATASET' ##no_text.
        ENDIF.

        mo_file_options->is_mode_valid( iv_file_mode ).
        mo_file_options->is_operation_valid( iv_file_operation ).

        is_path_file_available( iv_path_file ).

        authority_check( iv_file_operation = iv_file_operation
                         iv_check_auth     = iv_check_auth ).

        "May implement a locking logic for appl. server files -> further details are here:
        "https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abenfile_interface_locking.htm

        CASE iv_file_mode.
          WHEN mo_file_options->mode-binary.
            "Open for  b i n a r y   m o d e
            CASE iv_file_operation.
              WHEN mo_file_options->operation-input.
                "Read or delete a file
                OPEN DATASET mv_path_file FOR INPUT
                                          IN BINARY MODE
                                          MESSAGE lv_msg.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = lv_msg ).
                ADD 1 TO zcl_ca_file_utility=>ms_dataset_cnt-input.

              WHEN mo_file_options->operation-output.
                "Create a new file
                OPEN DATASET mv_path_file FOR OUTPUT
                                          IN BINARY MODE
                                          MESSAGE lv_msg.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = lv_msg ).
                ADD 1 TO zcl_ca_file_utility=>ms_dataset_cnt-output.

              WHEN mo_file_options->operation-append.
                "Append new records to existing file or create a new file
                OPEN DATASET mv_path_file FOR APPENDING
                                          IN BINARY MODE
                                          MESSAGE lv_msg.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = lv_msg ).
                ADD 1 TO zcl_ca_file_utility=>ms_dataset_cnt-append.

              WHEN mo_file_options->operation-update.
                "Change a existing file
                OPEN DATASET mv_path_file FOR APPENDING
                                          IN BINARY MODE
                                          MESSAGE lv_msg.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = lv_msg ).
                ADD 1 TO zcl_ca_file_utility=>ms_dataset_cnt-update.
            ENDCASE.

          WHEN mo_file_options->mode-text.
            "Open for  t e x t   m o d e
            "Since the encoding of DEFAULT is equal to UTF-8 and NON-UNICODE is nearly
            "nowhere in use anymore. Due to this assumption files will always be opened
            "in UTF-8 mode and the passed codepage must be compatible to this.

            "SAP-Codepage ist eine UTF-8-Codepage?
            IF iv_codepage IS NOT INITIAL AND
               |{ cl_abap_codepage=>sap_to_http( iv_codepage ) CASE = UPPER }| NP 'UTF+8' ##no_text.
              "The passed SAP codepage &1 is no UTF-8 codepage
              RAISE EXCEPTION TYPE zcx_ca_file_utility
                EXPORTING
                  textid   = zcx_ca_file_utility=>no_utf8_codepage
                  mv_msgty = c_msgty_e
                  mv_msgv1 = CONV #( iv_codepage ).
            ENDIF.

            "Details for text mode files -> https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencodepage_file_guidl.htm
            CASE iv_file_operation.
              WHEN mo_file_options->operation-input.
                "Read or delete a file
                OPEN DATASET mv_path_file FOR INPUT
                                          IN TEXT MODE ENCODING UTF-8 SKIPPING BYTE-ORDER MARK
                                          MESSAGE lv_msg.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = lv_msg ).
                ADD 1 TO zcl_ca_file_utility=>ms_dataset_cnt-input.

              WHEN mo_file_options->operation-output.
                "Create a new file
                OPEN DATASET mv_path_file FOR OUTPUT
                                          IN TEXT MODE ENCODING UTF-8 WITH BYTE-ORDER MARK
                                          MESSAGE lv_msg.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = lv_msg ).
                ADD 1 TO zcl_ca_file_utility=>ms_dataset_cnt-output.

              WHEN mo_file_options->operation-append.
                "Append new records to existing file or create a new file
                OPEN DATASET mv_path_file FOR APPENDING
                                          IN TEXT MODE ENCODING UTF-8
                                          MESSAGE lv_msg.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = lv_msg ).
                ADD 1 TO zcl_ca_file_utility=>ms_dataset_cnt-append.

              WHEN mo_file_options->operation-update.
                "Change a existing file
                OPEN DATASET mv_path_file FOR UPDATE
                                          IN TEXT MODE ENCODING UTF-8
                                          MESSAGE lv_msg.
                check_result_of_open_dataset( iv_subrc = sy-subrc
                                              iv_msg   = lv_msg ).
                ADD 1 TO zcl_ca_file_utility=>ms_dataset_cnt-update.
            ENDCASE.
        ENDCASE.

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_sy_file_access_error
            cx_parameter_invalid INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "open_dataset


  METHOD print_log.
    "---------------------------------------------------------------------*
    "     Print statistics as simple list
    "---------------------------------------------------------------------*
    NEW-PAGE LINE-SIZE 132.

    IF ms_cnt-read IS NOT INITIAL.
      WRITE: /   'File name for INPUT (reading) ...................:'(s01),
      52(80) mv_path_file.
      IF mv_path_file+80 IS NOT INITIAL.
        WRITE /52(80) mv_path_file+80.
      ENDIF.
      IF mv_path_file+160 IS NOT INITIAL.
        WRITE /52(80) mv_path_file+160.
      ENDIF.
      WRITE: /   'Number of read records ..........................:'(s02),
      52 ms_cnt-read NO-ZERO.
      SKIP.
    ENDIF.

    IF ms_cnt-transfer IS NOT INITIAL.
      WRITE: /   'File name for OUTPUT (transfering) ..............:'(s03),
      52(80) mv_path_file.
      IF mv_path_file+80 IS NOT INITIAL.
        WRITE /52(80) mv_path_file+80.
      ENDIF.
      IF mv_path_file+160 IS NOT INITIAL.
        WRITE /52(80) mv_path_file+160.
      ENDIF.
      WRITE: /   'Number of transferred records ...................:'(s04),
      52 ms_cnt-transfer NO-ZERO.
    ENDIF.
    SKIP 2.
  ENDMETHOD.                    "print_log


  METHOD print_log_over_all.
    "---------------------------------------------------------------------*
    "     Print simple log with statistics over all activities
    "---------------------------------------------------------------------*
    NEW-PAGE LINE-SIZE 132.

    WRITE: /      'Number of opened files for ...'(s07).
    IF zcl_ca_file_utility=>ms_dataset_cnt-input IS NOT INITIAL.
      WRITE: /   '... action INPUT ................................:'(s08),
              52 zcl_ca_file_utility=>ms_dataset_cnt-input NO-ZERO.
    ENDIF.
    IF zcl_ca_file_utility=>ms_dataset_cnt-output IS NOT INITIAL.
      WRITE: /   '... action OUTPUT ...............................:'(s09),
              52 zcl_ca_file_utility=>ms_dataset_cnt-output NO-ZERO.
    ENDIF.
    IF zcl_ca_file_utility=>ms_dataset_cnt-append IS NOT INITIAL.
      WRITE: /   '... action APPEND ...............................:'(s10),
              52 zcl_ca_file_utility=>ms_dataset_cnt-append NO-ZERO.
    ENDIF.
    IF zcl_ca_file_utility=>ms_dataset_cnt-update IS NOT INITIAL.
      WRITE: /   '... action UPDATE ...............................:'(s11),
              52 zcl_ca_file_utility=>ms_dataset_cnt-update NO-ZERO.
    ENDIF.
    SKIP.

    IF zcl_ca_file_utility=>ms_cnt_all-read IS NOT INITIAL.
      WRITE: /   'Number of records read over all .................:'(s05),
              52 zcl_ca_file_utility=>ms_cnt_all-read NO-ZERO.
      SKIP.
    ENDIF.

    IF zcl_ca_file_utility=>ms_cnt_all-transfer IS NOT INITIAL.
      WRITE: /   'Number of transferred records over all ..........:'(s06),
              52 zcl_ca_file_utility=>ms_cnt_all-transfer NO-ZERO.
    ENDIF.
    SKIP 2.
  ENDMETHOD.                    "print_log_over_all


  METHOD read_dataset.
    "---------------------------------------------------------------------*
    "     Read data from file
    "---------------------------------------------------------------------*
    TRY.
        IF mv_location EQ mo_file_options->location-pc.
          "Method &1 is not allowed at the client/PC
          RAISE EXCEPTION TYPE zcx_ca_file_utility
            EXPORTING
              textid   = zcx_ca_file_utility=>not_allowed_for_client
              mv_msgty = c_msgty_e
              mv_msgv1 = 'READ_DATASET' ##no_text.
        ENDIF.

        READ DATASET mv_path_file INTO es_record
                                  MAXIMUM LENGTH iv_length
                                  ACTUAL  LENGTH ev_length.
        IF sy-subrc NE 0.
          ev_no_record = abap_true.
        ELSE.
          ev_no_record = abap_false.
        ENDIF.
        "For statistik purposes increase record counters
        ADD 1 TO: ms_cnt-read,
                  zcl_ca_file_utility=>ms_cnt_all-read.

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_sy_file_access_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    " read_dataset


  METHOD reset_statistics.
    "---------------------------------------------------------------------*
    "     Reset statistics
    "---------------------------------------------------------------------*
    CLEAR: zcl_ca_file_utility=>ms_cnt_all,
           zcl_ca_file_utility=>ms_dataset_cnt.
  ENDMETHOD.                    "reset_statistics


  METHOD set_physical_path_filename.
    "---------------------------------------------------------------------*
    "     Set physical path and file name
    "
    "     Use this method after you got the physical_filename_handler
    "     and changed the path with the help of it.
    "---------------------------------------------------------------------*
    mv_path_file = iv_path_file.
  ENDMETHOD.                    "set_physical_path_filename


  METHOD transfer_dataset.
    "---------------------------------------------------------------------*
    "     Transfer data to file
    "---------------------------------------------------------------------*
    TRY.
        IF mv_location EQ mo_file_options->location-pc.
          "Method &1 is not allowed at the client/PC
          RAISE EXCEPTION TYPE zcx_ca_file_utility
            EXPORTING
              textid   = zcx_ca_file_utility=>not_allowed_for_client
              mv_msgty = c_msgty_e
              mv_msgv1 = 'TRANSFER_DATASET' ##no_text.
        ENDIF.

        IF iv_rec_len LE 0.
          TRANSFER is_record TO mv_path_file.
        ELSE.
          TRANSFER is_record TO mv_path_file LENGTH iv_rec_len.
        ENDIF.

        ADD 1 TO: ms_cnt-transfer,
                  zcl_ca_file_utility=>ms_cnt_all-transfer.

      CATCH cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_sy_file_access_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    " transfer_dataset


  METHOD upload.
    "---------------------------------------------------------------------*
    "     Upload a file from the PC/client
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_codepage          TYPE abap_encod.

    CLEAR: ev_length,
           et_file.

    mo_file_options->is_mode_valid( iv_file_mode ).

    IF iv_codepage IS NOT INITIAL.
      lv_codepage = iv_codepage.
    ELSE.
      lv_codepage = mv_codepage.
    ENDIF.
    IF lv_codepage EQ '0000'.
      CLEAR lv_codepage.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = mv_path_file
        filetype                = SWITCH #( iv_file_mode
                                    WHEN mo_file_options->mode-binary THEN 'BIN'
                                    WHEN mo_file_options->mode-text   THEN 'ASC' ) ##no_text
        has_field_separator     = iv_has_field_separator
        codepage                = lv_codepage
      IMPORTING
        filelength              = ev_length
      CHANGING
        data_tab                = et_file
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                        iv_method   = 'GUI_UPLOAD'
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "upload


  METHOD write.
    "---------------------------------------------------------------------*
    "     Write file (TRANSFER to server / DOWNLOAD on client/PC)
    "---------------------------------------------------------------------*
    mo_file_options->is_operation_valid( iv_file_operation ).

    IF iv_file_operation EQ mo_file_options->operation-input.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_file_utility
        EXPORTING
          textid   = zcx_ca_file_utility=>param_invalid
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IV_FILE_OPERATION'
          mv_msgv2 = CONV #( iv_file_operation ) ##no_text.
    ENDIF.

    mo_file_options->is_mode_valid( iv_file_mode ).

    TRY.
        CASE mv_location.
          WHEN mo_file_options->location-pc.
            "P r e s e n t a t i o n   s e r v e r   /   c l i e n t / P C
            is_path_file_available( iv_path_file ).

            download(
                EXPORTING
                  iv_file_mode              = iv_file_mode
                  iv_check_auth             = iv_check_auth
                  iv_write_lf               = iv_write_lf
                  iv_confirm_overwrite      = iv_confirm_overwrite
                  iv_write_field_separator  = iv_write_field_separator
                  iv_trunc_trail_blanks     = iv_trunc_trail_blanks
                  iv_trunc_trail_blanks_eol = iv_trunc_trail_blanks_eol
                  iv_file_operation         = iv_file_operation
                  iv_codepage               = iv_codepage
                  iv_length                 = iv_length
                IMPORTING
                  ev_length                 = ev_length
                CHANGING
                  ct_file                   = ct_file ).

          WHEN mo_file_options->location-server.
            "A p p l i c a t i o n   s e r v e r
            open_dataset( iv_path_file      = iv_path_file
                          iv_file_mode      = iv_file_mode
                          iv_file_operation = iv_file_operation
                          iv_codepage       = iv_codepage
                          iv_check_auth     = iv_check_auth ).

            write_file( iv_file_mode = iv_file_mode
                        iv_length    = iv_length
                        it_file      = ct_file ).
            ev_length = iv_length.
        ENDCASE.

      CATCH zcx_ca_param INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                        ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "write


  METHOD write_file.
    "---------------------------------------------------------------------*
    "     Write file from application server
    "---------------------------------------------------------------------*
    "Local data definitions
    DATA:
      lr_record  TYPE REF TO data,
      lv_rec_len TYPE i,
      lv_remain  TYPE i.

    FIELD-SYMBOLS:
      <ls_record> TYPE data,
      <lv_column> TYPE data.

    mo_file_options->is_mode_valid( iv_file_mode ).

    "Prepare corresponding workarea for transfer
    CREATE DATA lr_record LIKE LINE OF it_file.
    ASSIGN lr_record->* TO <ls_record>.

    "Get length of structure
    lv_rec_len = get_records_length( iv_file_mode = iv_file_mode
                                         is_record    = <ls_record> ).

    "In binary mode the it is insufficient
    IF iv_file_mode EQ mo_file_options->mode-binary.
      DATA(lt_tabl_comps) = NEW zcl_ca_ddic( iv_data = it_file )->get_component_list( ).

      IF lines( lt_tabl_comps ) EQ 1.
        ASSIGN COMPONENT lt_tabl_comps[ 1 ]-name OF STRUCTURE <ls_record> TO <lv_column>.
        lv_rec_len = get_records_length( iv_file_mode = iv_file_mode
                                         is_record    = <lv_column> ).
      ENDIF.
    ENDIF.

    "Write file to server
    lv_remain = iv_length.
    LOOP AT it_file INTO <ls_record>.
      IF lv_remain LT lv_rec_len.
        lv_rec_len = lv_remain.
      ENDIF.

      IF <lv_column> IS ASSIGNED.
        transfer_dataset( is_record  = <lv_column>
                          iv_rec_len = lv_rec_len ).
      ELSE.
        transfer_dataset( is_record = <ls_record>
                          iv_rec_len = lv_rec_len ).
      ENDIF.

      lv_remain = lv_remain - lv_rec_len.
    ENDLOOP.

    close_dataset( ).
  ENDMETHOD.                    "write_file

ENDCLASS.
