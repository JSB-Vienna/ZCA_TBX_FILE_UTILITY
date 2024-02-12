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
*   o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for applic. server OR client/PC</p>
    directory_hdlr  TYPE REF TO zif_ca_directory_handler READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">CA-TBX: File handler for applic. server OR client/PC</p>
    file_hdlr       TYPE REF TO zif_ca_file_handler READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for file utility</p>
    cvc_file_util   TYPE REF TO zcl_ca_c_file_utility READ-ONLY,

*   s t r u c t u r e s
    "! <p class="shorttext synchronized" lang="en">All the names to each selection field</p>
    "!
    "! <p>Fields of the structure without a value have either no command or no memory id defined.</p>
    sel_field_names TYPE ty_s_sel_field_names READ-ONLY,

*   s i n g l e   v a l u e s
    "! <p class="shorttext synchronized" lang="en">This instance is assigned to the selection fields n</p>
    sel_field_id    TYPE num1 READ-ONLY.

* i n s t a n c e   m e t h o d s
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Browsing for logical or physical path / filename</p>
    f4_browse,

    "! <p class="shorttext synchronized" lang="en">Get file handler for selected file (attribute FILE_HDLR)</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized" lang="en">New file handler if requested</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    get_file_handler
      RETURNING
        VALUE(result) TYPE REF TO zif_ca_file_handler
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Modifying / adjusting selection screen fields</p>
    "!
    "! <p>Call this method for each file you use a selection screen include. You can either use masks to hide
    "! or display the selection fields that are needed or you can hide them all using the parameter
    "! {@link .METH:modify_selection_fields.DATA:use_for_value_help_only}. The latter overrules the other two.</p>
    "!
    "! <p>Concatenate several  modification Ids delimited by a semicolon, e. g. like this: FM;FO;FT in the
    "! parameters {@link .METH:modify_selection_fields.DATA:mask_2_hide_sel_params} and/or
    "! {@link .METH:modify_selection_fields.DATA:mask_2_set_params_disp_only}.</p>
    "!
    "! Possible values are (in the order as displayed at the selection screen):<br>
    "! <ul>
    "! <li>FL - Location</li>
    "! <li>FT - Logical or physical name</li>
    "! <li>FP - Path name</li>
    "! <li>FN - File name</li>
    "! <li>FO - File operation type</li>
    "! <li>FM - Text or binary mode</li>
    "! </ul>
    "!
    "! @parameter use_for_value_help_only     | <p class="shorttext synchronized" lang="en">X = Hide all selection parameters</p>
    "! @parameter mask_2_hide_sel_params      | <p class="shorttext synchronized" lang="en">Mask to hide selection parameters</p>
    "! @parameter mask_2_set_params_disp_only | <p class="shorttext synchronized" lang="en">Mask to set selection parameters to display only</p>
    "! @raising   zcx_ca_file_utility         | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    modify_selection_fields
      IMPORTING
        use_for_value_help_only     TYPE abap_boolean DEFAULT abap_false
        mask_2_hide_sel_params      TYPE char30   DEFAULT space
        mask_2_set_params_disp_only TYPE char30   DEFAULT space
      RAISING
        zcx_ca_file_utility,

    "! <p class="shorttext synchronized" lang="en">Provide (hidden) selection screen parameter values</p>
    "!
    "! <p>Reads the values of the parameters, if necessary from the screen, concatenates path and file name
    "! delimited by the required operating system delimiter.</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized" lang="en">Parameter values</p>
    "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
    provide_selscreen_param_values
      RETURNING
        VALUE(result) TYPE zca_s_file_util_sel_params
      RAISING
        zcx_ca_file_utility.

ENDINTERFACE.
