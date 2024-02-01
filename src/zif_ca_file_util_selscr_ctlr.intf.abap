"! <p class="shorttext synchronized" lang="en">CA-TBX: Selection screen controller for File Utility</p>
INTERFACE zif_ca_file_util_selscr_ctlr PUBLIC.
* l o c a l   t y p e   d e f i n i t i o n
  TYPES:
    "! <p class="shorttext synchronized" lang="en">Names to all parts of a selection parameter</p>
    BEGIN OF ty_s_sel_param_names,
      field_name   TYPE fieldname,
      user_command TYPE ptc_command,
      memory_id    TYPE rsscr_spgp,
      modif_id     TYPE scrfgrp1,
    END   OF ty_s_sel_param_names,

    "! <p class="shorttext synchronized" lang="en">Names/Commands to selection fields</p>
    BEGIN OF ty_s_sel_field_names,
      location  TYPE ty_s_sel_param_names,
      path_type TYPE ty_s_sel_param_names,
      path      TYPE ty_s_sel_param_names,
      file_name TYPE ty_s_sel_param_names,
      operation TYPE ty_s_sel_param_names,
      mode      TYPE ty_s_sel_param_names,
    END   OF ty_s_sel_field_names.

*    "! <p class="shorttext synchronized" lang="en">Logical path names to selected location</p>
*    BEGIN OF ty_s_log_path_name,
*      filesys    TYPE filesys_d,
*      opsys      TYPE opsys,
*      pathintern TYPE pathintern,
*      pathextern TYPE pathextern,
*    END   OF ty_s_log_path_name,
*    "! <p class="shorttext synchronized" lang="en">All found logical path names to selected location</p>
*    ty_tt_log_path_names TYPE STANDARD TABLE OF ty_s_log_path_name WITH EMPTY KEY,
*
*    "! <p class="shorttext synchronized" lang="en">Logical file names to selected location</p>
*    BEGIN OF ty_s_log_file_name,
*      fileintern TYPE fileintern,
*      filename   TYPE filename_d,
*      fileextern TYPE fileextern,
*      fileformat TYPE fileformat,
*      pathintern TYPE pathintern,
*      pathextern TYPE pathextern,
*      filesys    TYPE filesys_d,
*      opsys      TYPE opsys,
*    END   OF ty_s_log_file_name,
*    "! <p class="shorttext synchronized" lang="en">All found logical file names to selected location</p>
*    ty_tt_log_file_names TYPE STANDARD TABLE OF ty_s_log_file_name WITH EMPTY KEY.

* i n s t a n c e   a t t r i b u t e s
  DATA:
*   s t r u c t u r e s
    "! <p class="shorttext synchronized" lang="en">All the names to each selection field</p>
    "!
    "! <p>Fields of the structure without a value have either no command or no memory id defined.</p>
    sel_field_names    TYPE ty_s_sel_field_names READ-ONLY,

*   s i n g l e   v a l u e s
    "! <p class="shorttext synchronized" lang="en">This instance is assigned to the selection fields n</p>
    id_selscr_fields TYPE num1 READ-ONLY.

* i n s t a n c e   m e t h o d s
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Browsing for logical or physical path / filename</p>
    f4_browse,

    "! <p class="shorttext synchronized" lang="en">Modifying / adjusting selection screen fields</p>
    "!
    "! <p>Call this method for each file you have a selection screen include. Use masks to hide or display
    "! only selection fields that are needed. Concatenate several  modification Ids delimited by a semicolon,
    "! e. g. like this: FM;FO;FT</p>
    "! Possible values are (in the order as displayed at the selection screen):<br>
    "!
    "! <ul>
    "! <li>FL - Location</li>
    "! <li>FT - Logical or physical name</li>
    "! <li>FP - Path name</li>
    "! <li>FN - File name</li>
    "! <li>FO - File operation type</li>
    "! <li>FM - Text or binary mode</li>
    "! </ul>
    "!
    "! @parameter iv_use_f4_help_only | <p class="shorttext synchronized" lang="en">X = Hide all parameter fields</p>
    "! @parameter iv_mask_hiding      | <p class="shorttext synchronized" lang="en">Mask to hide selection fields</p>
    "! @parameter iv_mask_disp_only   | <p class="shorttext synchronized" lang="en">Mask to set selection fields to display only</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    modify_selection_fields
      IMPORTING
        iv_use_f4_help_only TYPE abap_bool DEFAULT abap_false
        iv_mask_hiding      TYPE char100   DEFAULT space
        iv_mask_disp_only   TYPE char100   DEFAULT space
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Provide (hidden) selection screen parameter values</p>
    "!
    "! @parameter rs_selscr_param_vals | <p class="shorttext synchronized" lang="en">Parameter values</p>
    "! @raising   zcx_ca_file_utility  | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    provide_selscreen_param_values
      RETURNING
        VALUE(rs_selscr_param_vals) TYPE zca_s_file_util_sel_params
      RAISING
        zcx_ca_file_utility.

ENDINTERFACE.
