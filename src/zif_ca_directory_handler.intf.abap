"! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for applic. server OR client/PC</p>
INTERFACE zif_ca_directory_handler PUBLIC.
* i n s t a n c e   a t t r i b u t e s
  DATA:
*   o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for file utility</p>
    cvc_file_util    TYPE REF TO zcl_ca_c_file_utility READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">File System Path handler</p>
    path_handler     TYPE REF TO cl_fs_path READ-ONLY,

*   t a b l e s
    "! <p class="shorttext synchronized" lang="en">Content of directory / file list</p>
    content          TYPE zca_tt_directory_entries READ-ONLY,

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
    "! <p class="shorttext synchronized" lang="en">Assemble path and file name (resolves logical name too)</p>
    "!
    "! @parameter processing_params   | <p class="shorttext synchronized" lang="en">Parameters where and how the file should be proceeded</p>
    "! @parameter path_file_name_hdlr | <p class="shorttext synchronized" lang="en">Path / file name handler</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    assemble_path_n_file_name
      EXPORTING
        path_file_name_hdlr TYPE REF TO cl_fs_path
      CHANGING
        processing_params   TYPE zca_s_file_util_sel_params
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Read directory content to the given path</p>
    "!
    "! @parameter content_type        | <p class="shorttext synchronized" lang="en">Content type (Dirs or Files, CVC_FILE_HDLR-&gt;VALUE_HELP-*)</p>
    "! @parameter path_file           | <p class="shorttext synchronized" lang="en">Complete physical path, may incl. file name</p>
    "! @parameter filter              | <p class="shorttext synchronized" lang="en">Generic filename (may end with *)</p>
    "! @parameter sort_by             | <p class="shorttext synchronized" lang="en">Sort file list (use const CVC_FILE_HDLR-&gt;LIST_SORTING-*)</p>
    "! @parameter refresh             | <p class="shorttext synchronized" lang="en">X = Refresh list by clearing list and reading directory again</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    read_content
      IMPORTING
        content_type TYPE zca_d_vht_dirs_files
        path_file    TYPE zca_d_path_n_file_name_n_ext OPTIONAL
        filter       TYPE string DEFAULT '*'
        sort_by      TYPE char1  DEFAULT zcl_ca_c_file_utility=>list_sorting-by_date_time_changed
        refresh      TYPE abap_boolean DEFAULT abap_false
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Get file handler to a directory entry</p>
    "!
    "! @parameter directory_entry     | <p class="shorttext synchronized" lang="en">CA-TBX: Directory entry details</p>
    "! @parameter mode                | <p class="shorttext synchronized" lang="en">Binary or text mode (use const CVC_FILE_HDLR-&gt;MODE-*)</p>
    "! @parameter operation           | <p class="shorttext synchronized" lang="en">Operation (use const CVC_FILE_HDLR-&gt;OPERATION-*)</p>
    "! @parameter result              | <p class="shorttext synchronized" lang="en">CA-TBX: File handler for applic. server OR client/PC</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    get_file_handler_2_dir_entry
      IMPORTING
        directory_entry TYPE zca_s_directory_entry
        mode            TYPE swr_filetype DEFAULT zcl_ca_c_file_utility=>mode-binary
        operation       TYPE dsetactype   DEFAULT zcl_ca_c_file_utility=>operation-input
        type            TYPE dxfiletyp    DEFAULT zcl_ca_c_file_utility=>type-physical
      RETURNING
        VALUE(result)   TYPE REF TO zif_ca_file_handler
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Get physical path and file name to a logical filename</p>
    "!
    "! <p><strong>Attention!</strong> The parameters {@link .DATA:iv_including_dir} and {@link .DATA:iv_with_file_extension}
    "! exclude each other, means you can either use one or the other with value ABAP_TRUE.</p>
    "!
    "! <p>Some more details about the parameters of this method you can find in the documentation of function
    "! module {@link FUNC:file_get_name} which has the same parameters as the function module
    "! {@link FUNC:file_get_name_and_validate} used in this method.</p>
    "!
    "! @parameter logical_filename    | <p class="shorttext synchronized" lang="en">Logical file name defined in TA FILE</p>
    "! @parameter parameter_1         | <p class="shorttext synchronized" lang="en">Replacement parameter 1</p>
    "! @parameter parameter_2         | <p class="shorttext synchronized" lang="en">Replacement parameter 2</p>
    "! @parameter parameter_3         | <p class="shorttext synchronized" lang="en">Replacement parameter 3</p>
    "! @parameter is_for_local_client | <p class="shorttext synchronized" lang="en">X = Location is the local client/PC</p>
    "! @parameter including_dir       | <p class="shorttext synchronized" lang="en">X = Expecting / return a physical path</p>
    "! @parameter with_file_extension | <p class="shorttext synchronized" lang="en">X = Append internal file extension to file name ..e. g. BIN</p>
    "! @parameter result              | <p class="shorttext synchronized" lang="en">Complete path and file name</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    get_pathfile_from_logical_name
      IMPORTING
        logical_filename    TYPE fileintern
        parameter_1         TYPE text255 DEFAULT space
        parameter_2         TYPE text255 DEFAULT space
        parameter_3         TYPE text255 DEFAULT space
        is_for_local_client TYPE abap_boolean DEFAULT abap_false
        including_dir       TYPE abap_boolean DEFAULT abap_true
        with_file_extension TYPE abap_boolean DEFAULT abap_false
      RETURNING
        VALUE(result)       TYPE zca_d_path_n_file_name_n_ext
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Get handler for physical or logical path or full file name</p>
    "!
    "! @parameter path_file           | <p class="shorttext synchronized" lang="en">Physical path and/or file name</p>
    "! @parameter result              | <p class="shorttext synchronized" lang="en">Path / file name handler</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    get_physical_filename_handler
      IMPORTING
        path_file     TYPE zca_d_path_n_file_name_n_ext
      RETURNING
        VALUE(result) TYPE REF TO cl_fs_path
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Resolve a dir. parameter of the AS into a directory path</p>
    "!
    "! @parameter directory_param     | <p class="shorttext synchronized" lang="en">Directory parameter as shown in the first column of TA AL11</p>
    "! @parameter result              | <p class="shorttext synchronized" lang="en">Parameter values</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    resolve_dir_param_2_dir_path
      IMPORTING
        directory_param TYPE dirprofilenames DEFAULT 'DIR_HOME' ##no_text
      RETURNING
        VALUE(result)   TYPE dirname_al11
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Set selected path and path handler</p>
    "!
    "! @parameter processing_params   | <p class="shorttext synchronized" lang="en">Parameters where and how the file should be proceeded</p>
    "! @parameter path_handler        | <p class="shorttext synchronized" lang="en">Path handler</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    set_selected_path
      IMPORTING
        processing_params TYPE zca_s_file_util_sel_params OPTIONAL
        path_handler      TYPE REF TO cl_fs_path OPTIONAL
      RAISING
        zcx_ca_file_utility.

ENDINTERFACE.
