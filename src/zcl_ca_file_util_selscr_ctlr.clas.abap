"! <p class="shorttext synchronized" lang="en">CA-TBX: Selection screen controller for File Utility</p>
"!
"! <strong>To read the following documentation in a formatted way, scroll down in this coding to the first
"! CLASS statement, make a single click on the class name ZCL_CA_... and press F2. Or instead open the view
"! 'ABAP Element info', activate 'Link with Selection' for this view and click again on the class name.</strong>
"! <br>... no Eclipse?? ... sorry :(.
"!
"! <h1>A. What is it for?</h1>
"! <h2>A1. Class for selection screen and file handling -&gt; two in one ;)</h2>
"! <p>The global class {@link ZCL_CA_DIRECTORY_HANDLER} should be usable in a program with selection fields.
"! <strong>This class and the includes {@link PROG:ZCA_FILE_UTIL_SLSCR_4_REUSEFL0} to -9</strong> are preparations
"! for that. This class provides a method to hide or display the selection fields. For more details see method
"! {@link ZCL_CA_FILE_UTIL_SELSCR_CTLR.METH:MODIFY_SELECTION_FIELDS}. Furthermore it provides a dynamic search
"! help for both fields, either the path and/or the file name. The search help supports both, either logical
"! or physical path and/or file name and can be used for both fields, path and field name. Have a look at
"! <strong>report {@link PROG:ZCA_DEMO_FILE_UTILITY}</strong>.</p>
"!
"! <h2>A2. Includes for the selection screen</h2>
"! <p>Each of the includes provide the same set of selection fields to supply controlling values per file. These
"! fields can also be hidden, but must be supplied with allowed values. So you need to include in your program
"! <strong>one of the includes for each file</strong> you want to handle. Each include represents a selection
"! screen block. So you can position each block between your other selection fields as you need to.</p>
"!
"! <p><strong>With recent releases, 7.56 and higher, it is no longer possible to use one include in multiple programs
"! respectively function groups. So it is necessary to copy the needed includes in the name space of your program.
"! During copying replace the name "ZCA_FILE_UTIL_SLSCR_4_REUSE" by the name of your main program and keep the
"! suffix FLn of the original as it is. The values 0 - 9 are the Ids provided as constants in class
"! {@link ZCL_CA_C_FILE_UTILITY} in structure {@link ZCL_CA_C_FILE_UTILITY.DATA:SELECTION_FIELD_ID}.</strong></p>
"!
"! <h2>A3. In your report</h2>
"! <p>Beside that most of the fields are drop-down fields bound to a command. All commands are defined as
"! constants in class {@link ZCL_CA_C_FILE_UTILITY} in structure {@link ZCL_CA_C_FILE_UTILITY.DATA:SELSCR_USER_COMMAND}.
"! So you are able to react on a change of such a value AT SELECTION-SCREEN (add structure SSCRFIELDS as tables
"! definition to your report!!) to e. g. recreate your instance or something else.</p>
"!
"! <p>Along with each include you have to define a <strong>separate attribute referring to the type of this
"! class</strong> which is then responsible for the corresponding file. With the instance creation, either in
"! the constructor of your local report class or the event INITIALIZATION, the attribute will be bound to the
"! number of the corresponding include. Creating the file instances this soon is necessary to have them available
"! for the event AT SELECTION-SCREEN OUTPUT.</p>
"!
"! <p>If you need to handle more files, than already prepared (currently 10), then copy simply e. g.
"! <strong>include {@link PROG:ZCA_FILE_UTIL_SLSCR_4_REUSEFL0}</strong> and rename the modification id and the
"! field names by changing <strong>only the numeric digit</strong> in any of the letters A - Z. Be aware that
"! you have then to define your own constants.</p>
"!
"! <h1>B. How to use an include in the selection screen</h1>
"! Due to the fact that selection screens are bound to the executable program there are unfortunately some
"! necessities to integrate such an include into your program.
"!
"! <h2>B1. Arranging in the selection screen</h2>
"! <p>Each include represents a selection screen block. You have only to take care of the rules for selection
"! screen blocks. So you are able to nest such an include in a block of yours or you position it before or behind
"! other selection block statements. Arrange the different includes where you like.</p>
"!
"! <h2>B2. Defining text elements</h2>
"! <p>Selection field texts and text symbols are always bound to the executable program. This is why some texts
"! have to be copied to the text elements area of your program. They are listed at the end of this description
"! together with some hints of how to. Of course can you replace these texts by your own.</p>
"!
"! <h2>B3. Additional events</h2>
"! <p>If you like to manipulate the appearance of the selection fields you need to add the <strong>report event
"! AT SELECTION-SCREEN OUTPUT</strong>. Use the above mentioned method
"! {@link ZCL_CA_FILE_UTIL_SELSCR_CTLR.METH:MODIFY_SELECTION_FIELDS} to hide or to set fields to display-only.</p>
"!
"! <p>To use the search helps for physical or logical path / file name add the <strong>report events AT
"! SELECTION-SCREEN ON VALUE-REQUEST FOR P_FLx...</strong> for each path / file name you want to provide a
"! search help and call there method {@link ZCL_CA_FILE_UTIL_SELSCR_CTLR.METH:F4_BROWSE}.</p>
"!
"! <h2>B4. Value checks</h2>
"! <p>To be as flexible as possible non of fields are obligatory. If needed use the possibilities of either
"! checking this at event <strong>AT SELECTION-SCREEN</strong> or set the corresponding fields to obligatory at
"! event <strong>AT SELECTION-SCREEN OUTPUT</strong> (that may be already exists -&gt; see above).</p>
"!
"! <h1>C. How to enhance your report with the following objects / parts</h1>
"!
"! <h2>C1. Coding for report events</h2>
"! Copy e. g. the following report events and the methods called there from
"! <strong>report {@link PROG:ZCA_DEMO_FILE_UTILITY}</strong>:
"! <ol>
"! <li>INITIALIZATION. -&gt; for initial instance creation</li>
"! <li>AT SELECTION-SCREEN OUTPUT. -&gt; for manipulation of the selection screen as needed</li>
"! <li>AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fl1pth. -&gt; to provide a search help for the path</li>
"! <li>AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fl1nam. -&gt; to provide a search help for the file name</li>
"! <li>....</li>
"! <li>AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fl?pth.</li>
"! <li>AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fl?nam.</li>
"! <li>AT SELECTION-SCREEN. -&gt; to check the values if necessary or to react to a change of a drop-down list,
"! e. g. to recreate the instance for the chosen environment / location.</li>
"! </ol>
"!
"! <h2>C2. Maintain text elements and selection field texts</h2>
"! <p>You can copy the sample texts below into the text elements maintenance view. Repeat this action for each
"! used selection screen include and take care of the right numbering.</p>
"!
"! <h3>Handling in SAP GUI</h3>
"! <p>Using the block copy function you should be able to copy the sample texts below into the corresponding tab
"! of the text elements maintenance view. Set therefore the cursor in front of the upper first digit of the block
"! you want to copy. Keep the <strong>shortcut ALT+SHIFT pressed while hovering over</strong> the editor area
"! below to mark it. After that <strong>press CTRL+C</strong> to copy this area.</p>
"! <p>Have a look at the following SCN blog for more details:
"! <em><strong>https://blogs.sap.com/2017/08/01/old-new-abap-editor</strong></em></p>
"!
"! <h3>Eclipse handling</h3>
"! <p>Start the <strong>block selection mode</strong> with <strong>shortcut ALT+SHIFT+A</strong>. Then hover
"! over the editor area below to mark it and copy it with <strong>CTRL+C</strong>. To <strong>deactivate the
"! block selection, press again</strong> the shortcut ALT+SHIFT+A.</p>
"!
"! <p>If your backend has a more recent release you can not use the block copy functionality and it is necessary
"! to copy each element individually into the new view design.</p>
"!
"! <h2>C3. Sample text elements</h2>
"! <p>In the following lists <strong>x represents the number of the file</strong> in the selection screen
"! (see number in the include name) and has to be replaced respectively.</p>
"!
"! <h3><em>T e x t   s y m b o l s</em></h3>
"! <ul><li>  FLx Specifications for file x                 (length 50)  </li></ul>
"!
"! <h3><em>S e l e c t i o n   t e x t s</em></h3>
"! <p>In alphabetic order because of the appearance in the maintenance view.</p>
"! <ul><li>  P_FLxLOC  Location x                        </li>
"! <li>      P_FLxMOD  Text or binary mode x             </li>
"! <li>      P_FLxPTH  Path name x                       </li>
"! <li>      P_FLxNAM  File name x                       </li>
"! <li>      P_FLxOP   File operation type x             </li>
"! <li>      P_FLxTYP  Logical or physical name x        </li></ul>
"!
CLASS zcl_ca_file_util_selscr_ctlr DEFINITION PUBLIC
                                              CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_file_util_selscr_ctlr.

*   a l i a s e s
    ALIASES:
*     Types
      ty_s_sel_param_names           FOR zif_ca_file_util_selscr_ctlr~ty_s_sel_param_names,
      ty_s_sel_field_names           FOR zif_ca_file_util_selscr_ctlr~ty_s_sel_field_names,
*      ty_s_log_path_name             FOR zif_ca_file_util_selscr_ctlr~ty_s_log_path_name,
*      ty_tt_log_path_names           FOR zif_ca_file_util_selscr_ctlr~ty_tt_log_path_names,
*      ty_s_log_file_name             FOR zif_ca_file_util_selscr_ctlr~ty_s_log_file_name,
*      ty_tt_log_file_names           FOR zif_ca_file_util_selscr_ctlr~ty_tt_log_file_names,
*     Attributes
      sel_field_names                FOR zif_ca_file_util_selscr_ctlr~sel_field_names,
      id_selscr_fields               FOR zif_ca_file_util_selscr_ctlr~id_selscr_fields,
*     Methods
      f4_browse                      FOR zif_ca_file_util_selscr_ctlr~f4_browse,
      modify_selection_fields        FOR zif_ca_file_util_selscr_ctlr~modify_selection_fields,
      provide_selscreen_param_values FOR zif_ca_file_util_selscr_ctlr~provide_selscreen_param_values.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter iv_location          | <p class="shorttext synchronized" lang="en">Location: server or client (CVC_FILE_HDLR-&gt;LOCATION-*)</p>
      "! @parameter iv_id_selscr_fields  | <p class="shorttext synchronized" lang="en">Id of assigned selection fields (CVC_FILE_HDLR-&gt;SELEC...</p>
      "! @parameter iv_progname          | <p class="shorttext synchronized" lang="en">ABAP Program Name were the sel.screen include is used</p>
      "! @parameter iv_screen_no         | <p class="shorttext synchronized" lang="en">Selection screen number</p>
      "! @raising   zcx_ca_file_utility  | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      "! @raising   zcx_ca_vh_tool       | <p class="shorttext synchronized" lang="en">CA-TBX exception: While calling / supporting value help</p>
      constructor
        IMPORTING
          iv_location         TYPE dxlocation
          iv_id_selscr_fields TYPE num1 DEFAULT zcl_ca_c_file_utility=>selection_field_id-for_file_1
          iv_progname         TYPE progname DEFAULT sy-cprog
          iv_screen_no        TYPE sydynnr DEFAULT sy-dynnr
        RAISING
          zcx_ca_file_utility
          zcx_ca_vh_tool.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   t y p e   d e f i n i t i o n s
    TYPES:
*      "! <p class="shorttext synchronized" lang="en">Names to all parts of a selection parameter</p>
*      BEGIN OF ty_s_sel_param_names,
*        field_name   TYPE fieldname,
*        user_command TYPE ptc_command,
*        memory_id    TYPE rsscr_spgp,
*        modif_id     TYPE scrfgrp1,
*      END   OF ty_s_sel_param_names,
*
*      "! <p class="shorttext synchronized" lang="en">Names/Commands to selection fields</p>
*      BEGIN OF ty_s_sel_field_names,
*        location  TYPE ty_s_sel_param_names,
*        path_type TYPE ty_s_sel_param_names,
*        path      TYPE ty_s_sel_param_names,
*        file_name TYPE ty_s_sel_param_names,
*        operation TYPE ty_s_sel_param_names,
*        mode      TYPE ty_s_sel_param_names,
*      END   OF ty_s_sel_field_names,

      "! <p class="shorttext synchronized" lang="en">Logical path names to selected location</p>
      BEGIN OF ty_s_log_path_name,
        filesys    TYPE filesys_d,
        opsys      TYPE opsys,
        pathintern TYPE pathintern,
        pathextern TYPE pathextern,
      END   OF ty_s_log_path_name,
      "! <p class="shorttext synchronized" lang="en">All found logical path names to selected location</p>
      ty_tt_log_path_names TYPE STANDARD TABLE OF ty_s_log_path_name WITH EMPTY KEY,

      "! <p class="shorttext synchronized" lang="en">Logical file names to selected location</p>
      BEGIN OF ty_s_log_file_name,
        fileintern TYPE fileintern,
        filename   TYPE filename_d,
        fileextern TYPE fileextern,
        fileformat TYPE fileformat,
        pathintern TYPE pathintern,
        pathextern TYPE pathextern,
        filesys    TYPE filesys_d,
        opsys      TYPE opsys,
      END   OF ty_s_log_file_name,
      "! <p class="shorttext synchronized" lang="en">All found logical file names to selected location</p>
      ty_tt_log_file_names TYPE STANDARD TABLE OF ty_s_log_file_name WITH EMPTY KEY.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Directory handler for applic. server OR client/PC</p>
      directory_hdlr             TYPE REF TO zif_ca_directory_handler,
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for file utility</p>
      cvc_file_util              TYPE REF TO zcl_ca_c_file_utility,
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Screen field attributes (usage with table SCREEN)</p>
      cvc_scr_fld_attr           TYPE REF TO zcl_ca_c_screen_field_attr,
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for value help tool</p>
      cvc_value_help             TYPE REF TO zcl_ca_c_vh_tool,
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Value help supporting tool</p>
      value_help_tool            TYPE REF TO zcl_ca_vh_tool,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Selected path in F4_BROWSE</p>
      with_f4_selected_path      TYPE dxlpath,
      "! <p class="shorttext synchronized" lang="en">Selected file name in F4_BROWSE</p>
      with_f4_selected_file_name TYPE dxfilename.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Call table value help for a logical name</p>
      "!
      "! @parameter iv_return_field_name | <p class="shorttext synchronized" lang="en">Name of column to be returned</p>
      "! @parameter it_value_list        | <p class="shorttext synchronized" lang="en">Selection value list</p>
      "! @parameter rt_selected_values   | <p class="shorttext synchronized" lang="en">List with selected values</p>
      "! @raising   zcx_ca_vh_tool       | <p class="shorttext synchronized" lang="en">CA-TBX exception: While calling / supporting value help</p>
      "! @raising   zcx_ca_ui            | <p class="shorttext synchronized" lang="en">CA-TBX exception: UI interaction messages</p>
      call_value_help_4_logical_name
        IMPORTING
          iv_return_field_name      TYPE fieldname
          it_value_list             TYPE STANDARD TABLE
        RETURNING
          VALUE(rt_selected_values) TYPE rsdm_f4_return_values
        RAISING
          zcx_ca_vh_tool
          zcx_ca_ui,

      "! <p class="shorttext synchronized" lang="en">Compile all names / commands / Ids for each selection field</p>
      compile_sel_screen_names,

      "! <p class="shorttext synchronized" lang="en">Create value help for requested path parameter</p>
      "!
      "! @parameter iv_progname    | <p class="shorttext synchronized" lang="en">ABAP Program Name were the sel.screen include is used</p>
      "! @parameter iv_screen_no   | <p class="shorttext synchronized" lang="en">Selection screen number</p>
      "! @raising   zcx_ca_vh_tool | <p class="shorttext synchronized" lang="en">CA-TBX exception: UI interaction messages</p>
      create_value_help_n_reg_fields
        IMPORTING
          iv_progname  TYPE progname DEFAULT sy-cprog
          iv_screen_no TYPE sydynnr DEFAULT sy-dynnr
        RAISING
          zcx_ca_vh_tool,

      "! <p class="shorttext synchronized" lang="en">Browsing for logical file name</p>
      "!
      "! @raising   zcx_ca_vh_tool | <p class="shorttext synchronized" lang="en">CA-TBX exception: While calling / supporting value help</p>
      "! @raising   zcx_ca_dbacc   | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      "! @raising   zcx_ca_ui      | <p class="shorttext synchronized" lang="en">CA-TBX exception: UI interaction messages</p>
      f4_browse_logical_file_name
        RAISING
          zcx_ca_vh_tool
          zcx_ca_dbacc
          zcx_ca_ui,

      "! <p class="shorttext synchronized" lang="en">Browsing for logical path / filename</p>
      "!
      "! @parameter iv_vh_type          | <p class="shorttext synchronized" lang="en">Value help type for directories or files</p>
      "! <p>Use the constants of cvc_file_util-&gt;VALUE_HELP-* or ZCL_CA_C_FILE_UTILITY=>VALUE_HELP-*</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      "! @raising   zcx_ca_vh_tool      | <p class="shorttext synchronized" lang="en">CA-TBX exception: While calling / supporting value help</p>
      "! @raising   zcx_ca_dbacc        | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      "! @raising   zcx_ca_ui           | <p class="shorttext synchronized" lang="en">CA-TBX exception: UI interaction messages</p>
      f4_browse_logical_name
        IMPORTING
          iv_vh_type TYPE zca_d_vht_dirs_files
        RAISING
          zcx_ca_file_utility
          zcx_ca_vh_tool
          zcx_ca_dbacc
          zcx_ca_ui,

      "! <p class="shorttext synchronized" lang="en">Browsing for logical path</p>
      "!
      "! @raising   zcx_ca_vh_tool | <p class="shorttext synchronized" lang="en">CA-TBX exception: While calling / supporting value help</p>
      "! @raising   zcx_ca_dbacc   | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      "! @raising   zcx_ca_ui      | <p class="shorttext synchronized" lang="en">CA-TBX exception: UI interaction messages</p>
      f4_browse_logical_path
        RAISING
          zcx_ca_vh_tool
          zcx_ca_dbacc
          zcx_ca_ui,

      "! <p class="shorttext synchronized" lang="en">Browsing for physical path / filename at the client PC</p>
      "!
      "! @parameter iv_vh_type          | <p class="shorttext synchronized" lang="en">Value help type for directories or files</p>
      "! <p>Use the constants of cvc_file_util-&gt;VALUE_HELP-* or ZCL_CA_C_FILE_UTILITY=>VALUE_HELP-*</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      "! @raising   zcx_ca_ui           | <p class="shorttext synchronized" lang="en">CA-TBX exception: UI interaction messages</p>
      f4_browse_pc
        IMPORTING
          iv_vh_type TYPE zca_d_vht_dirs_files
        RAISING
          zcx_ca_file_utility
          zcx_ca_ui,

      "! <p class="shorttext synchronized" lang="en">Browsing for a directory at the client PC</p>
      "!
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      "! @raising   zcx_ca_ui           | <p class="shorttext synchronized" lang="en">CA-TBX exception: UI interaction messages</p>
      f4_browse_pc_4_directory
        RAISING
          zcx_ca_file_utility
          zcx_ca_ui,

      "! <p class="shorttext synchronized" lang="en">Browsing for a file at the client PC</p>
      "!
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      "! @raising   zcx_ca_ui           | <p class="shorttext synchronized" lang="en">CA-TBX exception: UI interaction messages</p>
      f4_browse_pc_4_file
        RAISING
          zcx_ca_file_utility
          zcx_ca_ui,

      "! <p class="shorttext synchronized" lang="en">Browsing for physical path / filename</p>
      "!
      "! @parameter iv_vh_type          | <p class="shorttext synchronized" lang="en">Value help type for directories or files</p>
      "! <p>Use the constants of cvc_file_util-&gt;VALUE_HELP-* or ZCL_CA_C_FILE_UTILITY=>VALUE_HELP-*</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      "! @raising   zcx_ca_ui           | <p class="shorttext synchronized" lang="en">CA-TBX exception: UI interaction messages</p>
      f4_browse_physical_name
        IMPORTING
          iv_vh_type TYPE zca_d_vht_dirs_files
        RAISING
          zcx_ca_file_utility
          zcx_ca_ui,

      "! <p class="shorttext synchronized" lang="en">Browsing for physical path / filename at the server</p>
      "!
      "! @parameter iv_vh_type          | <p class="shorttext synchronized" lang="en">Value help type for directories or files</p>
      "! <p>Use the constants of cvc_file_util-&gt;VALUE_HELP-* or ZCL_CA_C_FILE_UTILITY=>VALUE_HELP-*</p>
      "! @raising   zcx_ca_file_utility | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      "! @raising   zcx_ca_ui           | <p class="shorttext synchronized" lang="en">CA-TBX exception: UI interaction messages</p>
      f4_browse_server
        IMPORTING
          iv_vh_type TYPE zca_d_vht_dirs_files
        RAISING
          zcx_ca_file_utility
          zcx_ca_ui,

      "! <p class="shorttext synchronized" lang="en">Get value either from screen or from default value</p>
      "!
      "! To fetch the default values is necessary in cases of hidden fields. They can't be
      "! fetched as they are inactive. Therefore the programmer has to provide the values instead.
      "!
      "! @parameter iv_fieldname   | <p class="shorttext synchronized" lang="en">Fieldname of requested value</p>
      "! @parameter ev_value       | <p class="shorttext synchronized" lang="en">Value</p>
      "! @raising   zcx_ca_vh_tool | <p class="shorttext synchronized" lang="en">CA-TBX exception: While calling / supporting value help</p>
      "! @raising   zcx_ca_conv    | <p class="shorttext synchronized" lang="en">CA-TBX exception: Conversion failed</p>
      get_field_value
        IMPORTING
          iv_fieldname TYPE fieldname
        EXPORTING
          ev_value     TYPE data
        RAISING
          zcx_ca_vh_tool
          zcx_ca_conv,

      "! <p class="shorttext synchronized" lang="en">Read logical file names for value help request</p>
      "!
      "! @parameter rt_log_file_names | <p class="shorttext synchronized" lang="en">Selected logical file names</p>
      "! @raising   zcx_ca_dbacc      | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      read_logical_file_names
        RETURNING
          VALUE(rt_log_file_names) TYPE ty_tt_log_file_names
        RAISING
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Read logical path names for value help request</p>
      "!
      "! @parameter rt_log_path_names | <p class="shorttext synchronized" lang="en">Selected logical file names</p>
      "! @raising   zcx_ca_dbacc      | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      read_logical_path_names
        RETURNING
          VALUE(rt_log_path_names) TYPE ty_tt_log_path_names
        RAISING
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Separate path and file name into attributes of it</p>
      "!
      "! @parameter iv_sel_path_n_filename | <p class="shorttext synchronized" lang="en">Chosen path and file name</p>
      "! @parameter iv_vh_type             | <p class="shorttext synchronized" lang="en">Value help type for directories or files</p>
      "! @raising   zcx_ca_file_utility    | <p class="shorttext synchronized" lang="en">CA-TBX exception: File handling errors</p>
      separate_file_name_from_sel
        IMPORTING
          iv_sel_path_n_filename TYPE string
          iv_vh_type             TYPE zca_d_vht_dirs_files
        RAISING
          zcx_ca_file_utility.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_file_util_selscr_ctlr IMPLEMENTATION.

  METHOD call_value_help_4_logical_name.
    "-----------------------------------------------------------------*
    "   Browsing for logical file name
    "-----------------------------------------------------------------*
    "Send pre-selected values as value-request-help
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = iv_return_field_name
        value_org       = 'S'      "= structured, but not in DDIC
        multiple_choice = space
      TABLES
        value_tab       = it_value_list
        return_tab      = rt_selected_values
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3 ##no_text.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_vh_tool( zcx_ca_error=>create_exception(
                                                             iv_excp_cls = zcx_ca_vh_tool=>c_zcx_ca_vh_tool
                                                             iv_function = 'F4IF_INT_TABLE_VALUE_REQUEST'
                                                             iv_subrc    = sy-subrc ) ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    IF rt_selected_values IS INITIAL.
      "Action canceled
      RAISE EXCEPTION TYPE zcx_ca_ui
        EXPORTING
          textid   = zcx_ca_ui=>action_cancelled
          mv_msgty = zcx_ca_ui=>c_msgty_w.
    ENDIF.
  ENDMETHOD.                    "call_value_help_4_logical_name


  METHOD compile_sel_screen_names.
    "-----------------------------------------------------------------*
    "   Compile all names / commands / Ids for each selection field
    "-----------------------------------------------------------------*
    sel_field_names-location-field_name   = |P_FL{ id_selscr_fields }LOC| ##no_text.
    sel_field_names-location-modif_id     = |FL{ id_selscr_fields }| ##no_text.
    sel_field_names-location-user_command = |LOCATION_{ id_selscr_fields }_CHANGED| ##no_text.

    sel_field_names-path_type-field_name   = |P_FL{ id_selscr_fields }TYP| ##no_text.
    sel_field_names-path_type-modif_id     = |FT{ id_selscr_fields }| ##no_text.
    sel_field_names-path_type-user_command = |TYPE_{ id_selscr_fields }_CHANGED| ##no_text.

    sel_field_names-path-field_name   = |P_FL{ id_selscr_fields }PTH| ##no_text.
    sel_field_names-path-memory_id    = |ZCA_PATH_{ id_selscr_fields }| ##no_text.
    sel_field_names-path-modif_id     = |FP{ id_selscr_fields }| ##no_text.

    sel_field_names-file_name-field_name   = |P_FL{ id_selscr_fields }NAM| ##no_text.
    sel_field_names-file_name-memory_id    = |ZCA_FILE_NAME_{ id_selscr_fields }| ##no_text.
    sel_field_names-file_name-modif_id     = |FN{ id_selscr_fields }| ##no_text.

    sel_field_names-operation-field_name   = |P_FL{ id_selscr_fields }OP| ##no_text.
    sel_field_names-operation-modif_id     = |FO{ id_selscr_fields }| ##no_text.
    sel_field_names-operation-user_command = |OPERATION_{ id_selscr_fields }_CHANGED| ##no_text.

    sel_field_names-mode-field_name   = |P_FL{ id_selscr_fields }MOD| ##no_text.
    sel_field_names-mode-modif_id     = |FM{ id_selscr_fields }| ##no_text.
    sel_field_names-mode-user_command = |MODE_{ id_selscr_fields }_CHANGED| ##no_text.
  ENDMETHOD.                    "compile_sel_screen_names


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    directory_hdlr = zcl_ca_directory_handler=>get_instance( iv_location ).

    cvc_file_util    = directory_hdlr->cvc_file_util.
    cvc_scr_fld_attr = zcl_ca_c_screen_field_attr=>get_instance( ).
    cvc_value_help   = zcl_ca_c_vh_tool=>get_instance( ).

    id_selscr_fields = iv_id_selscr_fields.
    compile_sel_screen_names( ).

    create_value_help_n_reg_fields( iv_progname  = iv_progname
                                    iv_screen_no = iv_screen_no ).
  ENDMETHOD.                    "Constructor


  METHOD create_value_help_n_reg_fields.
    "-----------------------------------------------------------------*
    "   Create value help for requested path parameter
    "-----------------------------------------------------------------*
    "Get selected values from selection screen
    value_help_tool = NEW zcl_ca_vh_tool( iv_progname  = iv_progname
                                          iv_screen_no = iv_screen_no
                                          iv_vh_type   = cvc_value_help->vh_type-internal_table ).

    "Register field names to catch up there values from screen
    value_help_tool->register_screen_field( sel_field_names-location-field_name ).
    value_help_tool->register_screen_field( sel_field_names-path_type-field_name ).
    value_help_tool->register_screen_field( sel_field_names-path-field_name ).
    value_help_tool->register_screen_field( sel_field_names-file_name-field_name ).
    value_help_tool->register_screen_field( sel_field_names-operation-field_name ).
    value_help_tool->register_screen_field( sel_field_names-mode-field_name ).
  ENDMETHOD.                    "create_value_help_n_reg_fields


  METHOD f4_browse_logical_file_name.
    "-----------------------------------------------------------------*
    "   Browsing for logical file name
    "-----------------------------------------------------------------*
    DATA(lt_selected_values) =
                 call_value_help_4_logical_name( iv_return_field_name = 'FILEINTERN'
                                                 it_value_list        = read_logical_file_names( ) ) ##no_text.

    with_f4_selected_file_name = VALUE #( lt_selected_values[ 1 ]-fieldval OPTIONAL ).
  ENDMETHOD.                    "f4_browse_logical_file_name


  METHOD f4_browse_logical_name.
    "-----------------------------------------------------------------*
    "   Browsing for logical path / filename
    "-----------------------------------------------------------------*
    CASE iv_vh_type.
      WHEN cvc_file_util->value_help-for_directories.
        f4_browse_logical_path( ).

      WHEN cvc_file_util->value_help-for_files.
        f4_browse_logical_file_name( ).
    ENDCASE.
  ENDMETHOD.                    "f4_browse_logical_name


  METHOD f4_browse_logical_path.
    "-----------------------------------------------------------------*
    "   Browsing for logical path name
    "-----------------------------------------------------------------*
    DATA(lt_selected_values) =
                 call_value_help_4_logical_name( iv_return_field_name = 'PATHINTERN'
                                                 it_value_list        = read_logical_path_names( ) ) ##no_text.

    with_f4_selected_path = VALUE #( lt_selected_values[ 1 ]-fieldval OPTIONAL ).
  ENDMETHOD.                    "f4_browse_logical_path


  METHOD f4_browse_pc.
    "-----------------------------------------------------------------*
    "   Browsing for physical path / filename at the client PC
    "-----------------------------------------------------------------*
    CASE iv_vh_type.
      WHEN cvc_file_util->value_help-for_directories.
        f4_browse_pc_4_directory( ).

      WHEN cvc_file_util->value_help-for_files.
        f4_browse_pc_4_file( ).
    ENDCASE.
  ENDMETHOD.                    "f4_browse_pc


  METHOD f4_browse_pc_4_directory.
    "-----------------------------------------------------------------*
    "   Browsing for directory at the client PC
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_path_pc           TYPE string.

    cl_gui_frontend_services=>directory_browse(
                                          EXPORTING
                                            window_title         = CONV #( 'Select directory'(pt2) )
                                            initial_folder       = CONV #( with_f4_selected_path )
                                          CHANGING
                                            selected_folder      = lv_path_pc
                                          EXCEPTIONS
                                            cntl_error           = 1
                                            error_no_gui         = 2
                                            not_supported_by_gui = 3
                                            OTHERS               = 4 ).
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                     iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                     iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                     iv_method   = 'DIRECTORY_BROWSE'
                                                     iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.

    ELSEIF lv_path_pc IS INITIAL.
      "Action canceled
      RAISE EXCEPTION TYPE zcx_ca_ui
        EXPORTING
          textid   = zcx_ca_ui=>action_cancelled
          mv_msgty = zcx_ca_ui=>c_msgty_w.

    ELSE.
      with_f4_selected_path = CONV #( lv_path_pc ).
    ENDIF.
  ENDMETHOD.                    "f4_browse_pc_4_directory


  METHOD f4_browse_pc_4_file.
    "-----------------------------------------------------------------*
    "   Browsing for a file at the client PC
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_sel_files   TYPE filetable,
      lv_subrc       TYPE i ##needed,
      lv_user_action TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
                                          EXPORTING
                                            window_title            = CONV #( 'Select file'(pt3) )
                                            initial_directory       = CONV #( with_f4_selected_path )
                                            multiselection          = abap_false
                                          CHANGING
                                            file_table              = lt_sel_files
                                            rc                      = lv_subrc
                                            user_action             = lv_user_action
                                          EXCEPTIONS
                                            file_open_dialog_failed = 1
                                            cntl_error              = 2
                                            error_no_gui            = 3
                                            not_supported_by_gui    = 4
                                            OTHERS                  = 5 ).
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                     iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                     iv_class    = 'CL_GUI_FRONTEND_SERVICES'
                                                     iv_method   = 'FILE_OPEN_DIALOG'
                                                     iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    DATA(lv_file_name_incl_path) = VALUE string( lt_sel_files[ 1 ]-filename OPTIONAL ).
    IF lv_user_action         EQ cl_gui_frontend_services=>action_cancel OR
       lv_file_name_incl_path IS INITIAL.
      "Action canceled
      RAISE EXCEPTION TYPE zcx_ca_ui
        EXPORTING
          textid   = zcx_ca_ui=>action_cancelled
          mv_msgty = zcx_ca_ui=>c_msgty_w.
    ENDIF.

    separate_file_name_from_sel( iv_sel_path_n_filename = lv_file_name_incl_path
                                 iv_vh_type             = cvc_file_util->value_help-for_files ).
  ENDMETHOD.                    "f4_browse_pc_4_file


  METHOD f4_browse_physical_name.
    "-----------------------------------------------------------------*
    "   Browsing for physical path / filename
    "-----------------------------------------------------------------*
    CASE directory_hdlr->location.
      WHEN cvc_file_util->location-pc.
        f4_browse_pc( iv_vh_type ).

      WHEN cvc_file_util->location-server.
        f4_browse_server( iv_vh_type ).
    ENDCASE.
  ENDMETHOD.                    "f4_browse_physical_name


  METHOD f4_browse_server.
    "-----------------------------------------------------------------*
    "   Browsing for physical path / filename at the server
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_server_file  TYPE string,
      lv_f4_file_name TYPE dxfilename.

    IF with_f4_selected_path IS INITIAL.
      TRY.
          DATA(lo_path) = cl_fs_path=>get_current_working_dir( path_kind = cl_fs_path=>path_kind_smart ).
          with_f4_selected_path = lo_path->get_path_name( ).

        CATCH cx_fs_path_not_defined INTO DATA(lx_catched) ##needed.
          CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_HOME'
                             ID 'VALUE' FIELD with_f4_selected_path. "#EC CI_CCALL
      ENDTRY.

    ELSE.
      DATA(lv_dir_name) = CONV char30( |{ with_f4_selected_path CASE = UPPER }| ).
      IF lv_dir_name(4) EQ 'DIR_' ##no_text.
        CALL 'C_SAPGPARAM' ID 'NAME'  FIELD lv_dir_name
                           ID 'VALUE' FIELD with_f4_selected_path. "#EC CI_CCALL
      ENDIF.
    ENDIF.

    "Use standard file mask because the generic search does not work
    lv_f4_file_name = '*'.       "If file name is specified and doesn't exist, no selection is possible
    CALL FUNCTION 'CM_F4_SERVERFILE'
      EXPORTING
        iv_directory     = CONV string( with_f4_selected_path )
        iv_filemask      = CONV string( lv_f4_file_name )
      IMPORTING
        ev_serverfile    = lv_server_file
      EXCEPTIONS
        canceled_by_user = 1    "<== is not set by the FM -> therefore checking the result
        OTHERS           = 2.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                     iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                     iv_function = '/SAPDMC/LSM_F4_SERVER_FILE'
                                                     iv_subrc    = sy-subrc ) ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    IF lv_server_file IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ca_ui
        EXPORTING
          textid   = zcx_ca_ui=>action_cancelled
          mv_msgty = zcx_ca_ui=>c_msgty_w.
    ENDIF.

    separate_file_name_from_sel( iv_sel_path_n_filename = lv_server_file
                                 iv_vh_type             = iv_vh_type ).
  ENDMETHOD.                    "f4_browse_server


  METHOD get_field_value.
    "-----------------------------------------------------------------*
    "   Get value either from screen or from default value
    "-----------------------------------------------------------------*
    "Local data definitions
    FIELD-SYMBOLS:
      <lv_value>           TYPE data.

    CLEAR ev_value.
    value_help_tool->get_screen_field_value(
                                        EXPORTING
                                           iv_scr_field_name  = iv_fieldname
                                        IMPORTING
                                           ev_scr_field_value = ev_value ).

    IF ev_value IS NOT INITIAL.
      RETURN.
    ENDIF.

    "If a field is hidden or inactive the method before is not able to read the value from the screen.
    "Therefore try to get the value from the field itself.
    "ATTENTION!! This can lead to an unintentional behavior. E. g. the file name had a default value which was
    "erased just before executing the value help for the path without hitting the ENTER key. After the path
    "selection the file will reappear although it was erased.
    "A possible solution for this is.
    DATA(lv_program_field_name) = CONV seocpdname( |({ value_help_tool->mv_progname }){ iv_fieldname }| ).
    ASSIGN (lv_program_field_name) TO <lv_value>.
    ASSERT sy-subrc EQ 0.

    ev_value = <lv_value>.
    value_help_tool->set_screen_field_value( iv_scr_field_name  = iv_fieldname
                                             iv_scr_field_value = ev_value ).
  ENDMETHOD.                    "get_field_value


  METHOD read_logical_file_names.
    "-----------------------------------------------------------------*
    "   Read logical file names for value help request
    "-----------------------------------------------------------------*
    DATA(lv_file) = CONV fileintern( translate( val  = |{ with_f4_selected_file_name CASE = UPPER }|
                                                from = '*+'   to = '%_' ) ).
    IF lv_file IS INITIAL.
      lv_file = '%'.
    ENDIF.

    SELECT FROM filenameci AS fn                       "#EC CI_BUFFJOIN
                INNER JOIN filetextci AS ft
                        ON ft~langu      EQ @sy-langu     AND
                           ft~fileintern EQ fn~fileintern
                LEFT OUTER JOIN path AS pa             "#EC CI_BUFFJOIN
                        ON pa~pathintern EQ fn~pathintern
                     INNER JOIN filesys AS fs          "#EC CI_BUFFJOIN
                             ON fs~filesys EQ pa~filesys
                     INNER JOIN opsystem AS os         "#EC CI_BUFFJOIN
                             ON os~filesys EQ pa~filesys
         FIELDS fn~fileintern,  fn~fileextern,  fn~fileformat,
                ft~filename,
                pa~pathintern,  pa~pathextern,  pa~filesys,
                os~opsys
          WHERE os~opsys      EQ   @directory_hdlr->operation_system
            AND fn~fileintern LIKE @lv_file
                ORDER BY fn~fileintern
           INTO CORRESPONDING FIELDS OF TABLE @rt_log_file_names.
    IF sy-subrc NE 0.
      "No data was found for the specified selection criteria
      RAISE EXCEPTION TYPE zcx_ca_dbacc
        EXPORTING
          textid   = zcx_ca_dbacc=>no_data
          mv_msgty = zcx_ca_dbacc=>c_msgty_w.
    ENDIF.
  ENDMETHOD.                    "read_logical_file_names


  METHOD read_logical_path_names.
    "-----------------------------------------------------------------*
    "   Read logical path names for value help request
    "-----------------------------------------------------------------*
    DATA(lv_path) = CONV pathintern( translate( val  = |{ with_f4_selected_path CASE = UPPER }|
                                                from = '*+'   to = '%_' ) ).
    IF lv_path IS INITIAL.
      lv_path = '%'.
    ENDIF.

    SELECT FROM opsystem AS os                         "#EC CI_BUFFJOIN
                INNER JOIN filesys AS fs               "#EC CI_BUFFJOIN
                        ON fs~filesys EQ os~filesys
                INNER JOIN path AS pa                  "#EC CI_BUFFJOIN
                        ON pa~filesys EQ fs~filesys
         FIELDS os~opsys,       fs~filesys,
                pa~pathintern,  pa~pathextern
          WHERE os~opsys      EQ   @directory_hdlr->operation_system
            AND pa~pathintern LIKE @lv_path
                ORDER BY pa~pathintern
           INTO  CORRESPONDING FIELDS OF TABLE @rt_log_path_names.
    IF sy-subrc NE 0.
      "No data was found for the specified selection criteria
      RAISE EXCEPTION TYPE zcx_ca_dbacc
        EXPORTING
          textid   = zcx_ca_dbacc=>no_data
          mv_msgty = zcx_ca_dbacc=>c_msgty_w.
    ENDIF.
  ENDMETHOD.                    "read_logical_path_names


  METHOD separate_file_name_from_sel.
    "-----------------------------------------------------------------*
    "   Separate path and file name into attributes of it
    "-----------------------------------------------------------------*
    TRY.
        CASE directory_hdlr->location.
          WHEN cvc_file_util->location-server.
            DATA(lo_path) = cl_fs_path=>create( name      = iv_sel_path_n_filename
                                                path_kind = cl_fs_path=>path_kind_smart ).

            CASE iv_vh_type.
              WHEN cvc_file_util->value_help-for_directories.
                with_f4_selected_path = lo_path->get_path_component( ).

              WHEN cvc_file_util->value_help-for_files.
                with_f4_selected_path      = lo_path->get_path_component( ).
                with_f4_selected_file_name = lo_path->get_file_name( ).
            ENDCASE.

          WHEN cvc_file_util->location-pc.
            DATA(lv_path) = substring_before( val = iv_sel_path_n_filename
                                              sub = directory_hdlr->path_separator
                                              occ = -1 ).
            DATA(lv_file_name) = substring_after( val = iv_sel_path_n_filename
                                                  sub = directory_hdlr->path_separator
                                                  occ = -1 ).

            CASE iv_vh_type.
              WHEN cvc_file_util->value_help-for_directories.
                with_f4_selected_path = lv_path.

              WHEN cvc_file_util->value_help-for_files.
                with_f4_selected_path      = lv_path.
                with_f4_selected_file_name = lv_file_name.
            ENDCASE.
        ENDCASE.

      CATCH cx_smart_path_syntax INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                       iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                       iv_class    = 'CL_FS_PATH'
                                                       iv_method   = 'CREATE'
                                                       ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "separate_file_name_from_sel


  METHOD zif_ca_file_util_selscr_ctlr~f4_browse.
    "-----------------------------------------------------------------*
    "   Browsing for logical or physical path / filename
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_cursor_field      TYPE fieldname.

    TRY.
        "Get selection screen values -> reads only the visible fields
        value_help_tool->read_field_values_from_screen(
                                        iv_selection_type = cvc_value_help->selection_type-all_screen_fields ).
        DATA(ls_selscr_params) = provide_selscreen_param_values( ).

        "Get selected value help type, path type, operation type and may be a file name pattern
        GET CURSOR FIELD lv_cursor_field.
        ASSERT sy-subrc EQ 0.

        DATA(lv_vh_type) = COND #( WHEN lv_cursor_field CP '*PTH' THEN cvc_file_util->value_help-for_directories
                                   WHEN lv_cursor_field CP '*NAM' THEN cvc_file_util->value_help-for_files
                                              "Parameter '&1' has invalid value '&2'
                                   ELSE THROW zcx_ca_file_utility( textid   = zcx_ca_file_utility=>param_invalid
                                                                   mv_msgty = 'E'
                                                                   mv_msgv1 = 'LV_CURSOR_FIELD'
                                                                   mv_msgv2 = CONV #( lv_cursor_field ) ) ) ##no_text.

        value_help_tool->get_screen_field_value(
                                            EXPORTING
                                               iv_scr_field_name  = sel_field_names-path-field_name
                                            IMPORTING
                                               ev_scr_field_value = with_f4_selected_path ).

        value_help_tool->get_screen_field_value(
                                            EXPORTING
                                               iv_scr_field_name  = sel_field_names-file_name-field_name
                                            IMPORTING
                                               ev_scr_field_value = with_f4_selected_file_name ).

        "Start dialog in dependence of path type
        CASE ls_selscr_params-type.
          WHEN cvc_file_util->type-logical.
            f4_browse_logical_name( lv_vh_type ).

          WHEN cvc_file_util->type-physical.
            f4_browse_physical_name( lv_vh_type ).
        ENDCASE.

        "Set selected value into screen field
        value_help_tool->set_screen_field_value( iv_scr_field_name  = sel_field_names-path-field_name
                                                 iv_scr_field_value = with_f4_selected_path ).
        value_help_tool->set_screen_field_value( iv_scr_field_name  = sel_field_names-file_name-field_name
                                                 iv_scr_field_value = with_f4_selected_file_name ).

        value_help_tool->write_values_into_scr_fields( iv_convert_to_ext = abap_false ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_file_util_selscr_ctlr~f4_browse


  METHOD zif_ca_file_util_selscr_ctlr~modify_selection_fields.
    "-----------------------------------------------------------------*
    "   Modifying / adjusting selection screen fields
    "-----------------------------------------------------------------*
    DATA(lv_lhide) = strlen( iv_mask_hiding ).
    DATA(lv_ldo)   = strlen( iv_mask_disp_only ).

    DATA(lv_name_pattern)   = CONV sychar132( |*FL{ id_selscr_fields }*BLOCK*| ) ##no_text.
    DATA(lv_group1_pattern) = CONV char3( |F+{ id_selscr_fields }| ) ##no_text.

    LOOP AT SCREEN INTO DATA(ls_screen).
      IF ls_screen-name   NP lv_name_pattern   AND
         ls_screen-group1 NP lv_group1_pattern ##no_text.
        CONTINUE.
      ENDIF.

      IF   iv_use_f4_help_only EQ abap_true           AND
           ls_screen-group3    EQ 'BLK'               AND
         ( ls_screen-name      CP lv_name_pattern      OR
           ls_screen-group1    CP lv_group1_pattern ) ##no_text.
        ls_screen-active = cvc_scr_fld_attr->switch-off.

      ELSEIF ls_screen-group1         IS NOT INITIAL AND
             iv_mask_hiding           IS NOT INITIAL AND
             iv_mask_hiding(lv_lhide) CS ls_screen-group1(2).
        ls_screen-active = cvc_scr_fld_attr->switch-off.

      ELSEIF ls_screen-group1          IS NOT INITIAL AND
             iv_mask_disp_only         IS NOT INITIAL AND
             iv_mask_disp_only(lv_ldo) CS ls_screen-group1(2).
        ls_screen-input = cvc_scr_fld_attr->switch-off.
      ENDIF.

      MODIFY SCREEN FROM ls_screen.
    ENDLOOP.
  ENDMETHOD.                    "zif_ca_file_util_selscr_ctlr~modify_selection_fields


  METHOD zif_ca_file_util_selscr_ctlr~provide_selscreen_param_values.
    "-----------------------------------------------------------------*
    "   Provide (hidden) selection screen parameter values
    "-----------------------------------------------------------------*
    TRY.
        get_field_value(
                    EXPORTING
                      iv_fieldname = sel_field_names-location-field_name
                    IMPORTING
                      ev_value     = rs_selscr_param_vals-location ).

        get_field_value(
                    EXPORTING
                      iv_fieldname = sel_field_names-path_type-field_name
                    IMPORTING
                      ev_value     = rs_selscr_param_vals-type ).

        get_field_value(
                    EXPORTING
                      iv_fieldname = sel_field_names-path-field_name
                    IMPORTING
                      ev_value     = rs_selscr_param_vals-path ).

        "Clean up directory delimiter for a uniform / consistent path + file name handling
        IF rs_selscr_param_vals-path IS NOT INITIAL.
          DATA(lv_offset) = strlen( rs_selscr_param_vals-path ) - 1.
          IF rs_selscr_param_vals-path+lv_offset(1) EQ directory_hdlr->path_separator.
            rs_selscr_param_vals-path+lv_offset(1) = space.
          ENDIF.

          rs_selscr_param_vals-path_file = rs_selscr_param_vals-path.
        ENDIF.

        get_field_value(
                    EXPORTING
                      iv_fieldname = sel_field_names-file_name-field_name
                    IMPORTING
                      ev_value     = rs_selscr_param_vals-file_name ).

        IF rs_selscr_param_vals-file_name IS NOT INITIAL.
          rs_selscr_param_vals-path_file =
                           |{ rs_selscr_param_vals-path_file }{ directory_hdlr->path_separator }| &
                           |{ rs_selscr_param_vals-file_name }|.
        ENDIF.

        get_field_value(
                    EXPORTING
                      iv_fieldname = sel_field_names-operation-field_name
                    IMPORTING
                      ev_value     = rs_selscr_param_vals-operation ).

        get_field_value(
                    EXPORTING
                      iv_fieldname = sel_field_names-mode-field_name
                    IMPORTING
                      ev_value     = rs_selscr_param_vals-mode ).

      CATCH zcx_ca_conv
            zcx_ca_vh_tool INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_file_utility( zcx_ca_error=>create_exception(
                                                       iv_excp_cls = zcx_ca_file_utility=>c_zcx_ca_file_utility
                                                       iv_class    = 'ZCL_CA_FILE_UTILITY'
                                                       iv_method   = 'PROVIDE_SELSCREEN_PARAM_VALUES'
                                                       ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_file_util_selscr_ctlr~provide_selscreen_param_values

ENDCLASS.

