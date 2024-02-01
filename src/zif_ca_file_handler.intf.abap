"! <p class="shorttext synchronized" lang="en">CA-TBX: File handler for applic. server OR client/PC</p>
INTERFACE zif_ca_file_handler PUBLIC.
* i n s t a n c e   a t t r i b u t e s
  DATA:
*   o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for applic. server OR client/PC</p>
    directory_hdlr TYPE REF TO zif_ca_directory_handler READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for file utility</p>
    cvc_file_util  TYPE REF TO zcl_ca_c_file_utility READ-ONLY,

**   d a t a   r e f e r e n c e s
*    "! <p class="shorttext synchronized" lang="en">Description</p>
*    mr_...               TYPE REF TO x..
*
**   t a b l e s
*    "! <p class="shorttext synchronized" lang="en">Description</p>
*    mt_...               TYPE x..
*
**   s t r u c t u r e s
*    "! <p class="shorttext synchronized" lang="en">Description</p>
*    ms_...               TYPE x..

*   s i n g l e   v a l u e s
    "! <p class="shorttext synchronized" lang="en">Location: A = server / P = client/PC</p>
    mv_location    TYPE dxlocation READ-ONLY.

*   i n s t a n c e   m e t h o d s
  METHODS:
    create,

    "! <p class="shorttext synchronized" lang="en">Get file (READ from server / UPLOAD from client PC)</p>
    "!
    "! @parameter iv_path_file           | <p class="shorttext synchronized" lang="en">Complete physical path and file name</p>
    "! @parameter iv_file_mode           | <p class="shorttext synchronized" lang="en">Binary or character mode (CVC_FILE_HDLR-&gt;MODE-*)</p>
    "! @parameter iv_codepage            | <p class="shorttext synchronized" lang="en">Codepage (can use MV_CODEPAGE as default, see TCP00)</p>
    "! @parameter iv_has_field_separator | <p class="shorttext synchronized" lang="en">X = Fields are TAB separ. - result table needs corresp. cols</p>
    "! @parameter iv_check_auth          | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
    "! @parameter et_file                | <p class="shorttext synchronized" lang="en">File as table</p>
    "! @parameter ev_length              | <p class="shorttext synchronized" lang="en">File length</p>
    "! @raising   zcx_ca_file_utility    | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    read
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

    "! <p class="shorttext synchronized" lang="en">Delete</p>
    "!
    "! @parameter iv_path_file        | <p class="shorttext synchronized" lang="en">Complete physical path and file name</p>
    "! @parameter iv_check_auth       | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    delete
      IMPORTING
        iv_path_file  TYPE string    OPTIONAL      "is may be already availabe via e. g. GET_LOGICAL_FILENAME
        iv_check_auth TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Write file (TRANSFER to server / DOWNLOAD on client/PC)</p>
    "!
    "! @parameter iv_path_file              | <p class="shorttext synchronized" lang="en">Complete physical path and file name</p>
    "! @parameter iv_file_mode              | <p class="shorttext synchronized" lang="en">Binary or character mode (CVC_FILE_HDLR-&gt;MODE-*)</p>
    "! @parameter iv_check_auth             | <p class="shorttext synchronized" lang="en">X = Check authority for path and file</p>
    "! @parameter iv_write_lf               | <p class="shorttext synchronized" lang="en">X = Add CR/LF at end of CHAR lines (PC only)</p>
    "! @parameter iv_confirm_overwrite      | <p class="shorttext synchronized" lang="en">X = Confirm overwriting file (PC only)</p>
    "! @parameter iv_write_field_separator  | <p class="shorttext synchronized" lang="en">X = Separate fields by horizontal tabulator (PC only)</p>
    "! @parameter iv_trunc_trail_blanks     | <p class="shorttext synchronized" lang="en">X = Truncate trailing blanks at end of CHAR fields (PC only)</p>
    "! @parameter iv_trunc_trail_blanks_eol | <p class="shorttext synchronized" lang="en">X = Trunc. trailing blanks at end of the last col. (PC only)</p>
    "! @parameter iv_file_operation         | <p class="shorttext synchronized" lang="en">File operation type (CVC_FILE_HDLR-&gt;OPERATION-*)</p>
    "! @parameter iv_codepage               | <p class="shorttext synchronized" lang="en">Codepage (can use MV_CODEPAGE as default, see TCP00)</p>
    "! @parameter iv_length                 | <p class="shorttext synchronized" lang="en">Write file of this length</p>
    "! @parameter ev_length                 | <p class="shorttext synchronized" lang="en">Transmitted length</p>
    "! @parameter ct_file                   | <p class="shorttext synchronized" lang="en">File as table</p>
    "! @raising   zcx_ca_file_utility       | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
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
        iv_codepage               TYPE cpcodepage   DEFAULT '4110'   "equates to UTF-8 which is recommended for outbound
        iv_length                 TYPE i            OPTIONAL
      EXPORTING
        ev_length                 TYPE i
      CHANGING
        !ct_file                  TYPE STANDARD TABLE
      RAISING
        zcx_ca_file_utility.

ENDINTERFACE.
