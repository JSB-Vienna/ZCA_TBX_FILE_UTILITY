"! <p class="shorttext synchronized" lang="en">CA-TBX: File handler for applic. server OR client/PC</p>
INTERFACE zif_ca_file_handler PUBLIC.
* i n s t a n c e   a t t r i b u t e s
  DATA:
*   o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for applic. server OR client/PC</p>
    directory_hdlr    TYPE REF TO zif_ca_directory_handler READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">File name handler</p>
    file_name_handler TYPE REF TO cl_fs_path READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for file utility</p>
    cvc_file_util     TYPE REF TO zcl_ca_c_file_utility READ-ONLY,

*   s t r u c t u r e s
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Directory entry details</p>
    directory_entry   TYPE zca_s_directory_entry READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">Parameters where and how the file should be processed</p>
    processing_params TYPE zca_s_file_util_sel_params READ-ONLY,

*   s i n g l e   v a l u e s
    "! <p class="shorttext synchronized" lang="en">File length after reading</p>
    file_length       TYPE i READ-ONLY.

*   i n s t a n c e   m e t h o d s
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Read entire file</p>
    "!
    "! @parameter codepage            | <p class="shorttext synchronized" lang="en">Codepage (can use MV_CODEPAGE as default, see TCP00)</p>
    "! @parameter check_authority     | <p class="shorttext synchronized" lang="en">X = Check authority for path and file (appl. server only)</p>
    "! @parameter file                | <p class="shorttext synchronized" lang="en">File as table</p>
    "! @parameter length_of_file      | <p class="shorttext synchronized" lang="en">File length</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    read
      IMPORTING
        codepage        TYPE cpcodepage OPTIONAL
        check_authority TYPE abap_boolean DEFAULT abap_true
      EXPORTING
        file            TYPE STANDARD TABLE
        length_of_file  TYPE i
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Delete file</p>
    "!
    "! @parameter check_authority     | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    delete
      IMPORTING
        check_authority TYPE abap_boolean DEFAULT abap_true
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Set parameters where and how the file should be proceeded</p>
    "!
    "! <p><strong>This method has to be executed <em>BEFORE</em> you use any of the REST methods!!!</strong></p>
    "!
    "! <p>If you are using the class {@link zcl_ca_file_util_selscr_ctlr} (selection screen block controller)
    "! then use method of <strong>class {@link zcl_ca_file_util_selscr_ctlr.METH:provide_selscreen_param_values}
    "! </strong> to transfer the selection screen values into this instance.</p>
    "!
    "! @parameter processing_params   | <p class="shorttext synchronized" lang="en">Parameters where and how the file should be proceeded</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    set_processing_parameters
      IMPORTING
        processing_params TYPE zca_s_file_util_sel_params
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Write entire file</p>
    "!
    "! @parameter check_authority     | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
    "! @parameter codepage            | <p class="shorttext synchronized" lang="en">Codepage (can use MV_CODEPAGE as default, see TCP00)</p>
    "! @parameter max_file_length     | <p class="shorttext synchronized" lang="en">Max. length of the file to write (lower 0 = complete file)</p>
    "! @parameter length_of_file      | <p class="shorttext synchronized" lang="en">Transmitted length</p>
    "! @parameter file                | <p class="shorttext synchronized" lang="en">File as table</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    write
      IMPORTING
        check_authority TYPE abap_boolean DEFAULT abap_true
        codepage        TYPE cpcodepage   DEFAULT '4110'   "equates to UTF-8 which is recommended for outbound
        max_file_length TYPE i            OPTIONAL
      EXPORTING
        length_of_file  TYPE i
      CHANGING
        file            TYPE STANDARD TABLE
      RAISING
        zcx_ca_file_utility.

ENDINTERFACE.
