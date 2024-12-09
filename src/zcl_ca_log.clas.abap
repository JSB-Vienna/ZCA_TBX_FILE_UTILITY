"! <p class="shorttext synchronized" lang="en">CA-TBX: Business Application Logging (BAL)</p>
CLASS zcl_ca_log DEFINITION PUBLIC
                            CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message,
      if_fsbp_const_range,
      zif_ca_log,
      zif_ca_c_log_techn.

*   a l i a s e s
    ALIASES:
*     Public methods and attributes of this class
      add_msg                      FOR  zif_ca_log~add_msg,
      add_msg_bal                  FOR  zif_ca_log~add_msg_bal,
      add_msg_bal_tab              FOR  zif_ca_log~add_msg_bal_tab,
      add_msg_bapiret2             FOR  zif_ca_log~add_msg_bapiret2,
      add_msg_bapiret2_tab         FOR  zif_ca_log~add_msg_bapiret2_tab,
      add_msg_exc                  FOR  zif_ca_log~add_msg_exc,
      add_msg_syst                 FOR  zif_ca_log~add_msg_syst,
      close                        FOR  zif_ca_log~close,
      display                      FOR  zif_ca_log~display,
      get_ext_number               FOR  zif_ca_log~get_ext_number,
      get_msg_count                FOR  zif_ca_log~get_msg_count,
      get_msg_list_bal             FOR  zif_ca_log~get_msg_list_bal,
      get_msg_list_bapiret2        FOR  zif_ca_log~get_msg_list_bapiret2,
      get_profile                  FOR  zif_ca_log~get_profile,
      release                      FOR  zif_ca_log~release,
      save                         FOR  zif_ca_log~save,
      set_additional_ref_object_id FOR  zif_ca_log~set_additional_ref_object_id,
      set_ext_number               FOR  zif_ca_log~set_ext_number,
      set_program_name             FOR  zif_ca_log~set_program_name,
      set_source_position          FOR  zif_ca_log~set_source_position,
      set_transaction_code         FOR  zif_ca_log~set_transaction_code,
      write                        FOR  zif_ca_log~write,
      mo_log_options               FOR  zif_ca_log~mo_log_options,
*     Message types
      c_msgty_e                    FOR  if_xo_const_message~error,
      c_msgty_i                    FOR  if_xo_const_message~info,
      c_msgty_s                    FOR  if_xo_const_message~success,
      c_msgty_w                    FOR  if_xo_const_message~warning.


*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Display one or more saved application logs via log handles</p>
      "!
      "! @parameter io_parent      | <p class="shorttext synchronized" lang="en">Parent container to display log in-place</p>
      "! @parameter iv_title       | <p class="shorttext synchronized" lang="en">Application Log: Dynpro Title</p>
      "! @parameter iv_loghandle   | <p class="shorttext synchronized" lang="en">Application log: log handle</p>
      "! @parameter it_loghandle   | <p class="shorttext synchronized" lang="en">Application Log: LOG_HANDLE Range Table</p>
      "! @parameter iv_popup       | <p class="shorttext synchronized" lang="en">X = Display as popup</p>
      "! @parameter iv_opt_cwidth  | <p class="shorttext synchronized" lang="en">X = Optimize column width</p>
      "! @parameter iv_disp_srcpos | <p class="shorttext synchronized" lang="en">X = Display source code positions</p>
      "! @parameter is_profile     | <p class="shorttext synchronized" lang="en">Profile with settings log output / display</p>
      display_for_loghandle
        IMPORTING
          io_parent      TYPE REF TO cl_gui_container OPTIONAL
          iv_title       TYPE baltitle   OPTIONAL
          iv_loghandle   TYPE balloghndl OPTIONAL
          it_loghandle   TYPE bal_r_logh OPTIONAL
          iv_popup       TYPE abap_bool  DEFAULT abap_false
          iv_opt_cwidth  TYPE abap_bool  DEFAULT abap_true
          iv_disp_srcpos TYPE abap_bool  DEFAULT abap_false
          is_profile     TYPE bal_s_prof OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Display one or more saved application logs</p>
      "!
      "! @parameter io_parent      | <p class="shorttext synchronized" lang="en">Parent container to display log in-place</p>
      "! @parameter iv_title       | <p class="shorttext synchronized" lang="en">Application Log: Dynpro Title</p>
      "! @parameter iv_lognumber   | <p class="shorttext synchronized" lang="en">Application log: log number</p>
      "! @parameter it_lognumber   | <p class="shorttext synchronized" lang="en">Application Log: LOGNUMBER Range Table</p>
      "! @parameter iv_popup       | <p class="shorttext synchronized" lang="en">X = Display as popup</p>
      "! @parameter iv_opt_cwidth  | <p class="shorttext synchronized" lang="en">X = Optimize column width</p>
      "! @parameter iv_disp_srcpos | <p class="shorttext synchronized" lang="en">X = Display source code positions</p>
      "! @parameter is_profile     | <p class="shorttext synchronized" lang="en">Profile with settings log output / display</p>
      display_for_lognr
        IMPORTING
          io_parent      TYPE REF TO cl_gui_container OPTIONAL
          iv_title       TYPE baltitle    OPTIONAL
          iv_lognumber   TYPE balognr     OPTIONAL
          it_lognumber   TYPE bal_r_logn  OPTIONAL
          iv_popup       TYPE abap_bool   DEFAULT abap_false
          iv_opt_cwidth  TYPE abap_bool   DEFAULT abap_true
          iv_disp_srcpos TYPE abap_bool   DEFAULT abap_false
          is_profile     TYPE bal_s_prof  OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Display all logs for reference object and key</p>
      "!
      "! @parameter io_parent      | <p class="shorttext synchronized" lang="en">Parent container to display log in-place</p>
      "! @parameter iv_title       | <p class="shorttext synchronized" lang="en">Application Log: Dynpro Title</p>
      "! @parameter is_lpor        | <p class="shorttext synchronized" lang="en">Business object/class key - BOR Compatible</p>
      "! @parameter iv_add_key     | <p class="shorttext synchronized" lang="en">Additional key (for several logs to one business object)</p>
      "! @parameter iv_popup       | <p class="shorttext synchronized" lang="en">X = Display as popup</p>
      "! @parameter iv_opt_cwidth  | <p class="shorttext synchronized" lang="en">X = Optimize column width</p>
      "! @parameter iv_disp_srcpos | <p class="shorttext synchronized" lang="en">X = Display source code positions</p>
      "! @parameter is_profile     | <p class="shorttext synchronized" lang="en">Application Log: Log Output Format Profile</p>
      display_for_reference_obj
        IMPORTING
          io_parent      TYPE REF TO cl_gui_container OPTIONAL
          iv_title       TYPE baltitle   OPTIONAL
          is_lpor        TYPE sibflporb
          iv_add_key     TYPE zca_d_log_add_key OPTIONAL
          iv_popup       TYPE abap_bool  DEFAULT abap_false
          iv_opt_cwidth  TYPE abap_bool  DEFAULT abap_true
          iv_disp_srcpos TYPE abap_bool  DEFAULT abap_false
          is_profile     TYPE bal_s_prof OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Release buffered instances</p>
      free_buffer,

      "! <p class="shorttext synchronized" lang="en">Get instance to LPOR and, if needed, to an additional key</p>
      "!
      "! <p>Use this method only if multiple instances are required. This is e. g. the case for workflow objects
      "! (classes and business objects) or for mass processing like in data migration programs.</p>
      "! <p>The key structure IS_LPOR can be used as you like as long as it meets the uniqueness as required. If it
      "! is in use for workflow or archiving purposes it is recommended to pass a fully qualified key. Use for field
      "! CATID the constants SWFCO_OBJTYPE_BOR or _BC).</p>
      "!
      "! @parameter iv_object        | <p class="shorttext synchronized" lang="en">Application Log: Object Name (Application Code)</p>
      "! @parameter iv_subobj        | <p class="shorttext synchronized" lang="en">Application Log: Subobject</p>
      "! @parameter is_lpor          | <p class="shorttext synchronized" lang="en">Business object/class key - relevant for buffering</p>
      "! @parameter iv_add_key       | <p class="shorttext synchronized" lang="en">Additional key (for several logs to one business object)</p>
      "! @parameter iv_extnumber     | <p class="shorttext synchronized" lang="en">Application Log: External ID</p>
      "! @parameter iv_repid         | <p class="shorttext synchronized" lang="en">ABAP Program: Current Main Program</p>
      "! @parameter iv_tcode         | <p class="shorttext synchronized" lang="en">ABAP System Field: Current Transaction Code</p>
      "! @parameter iv_def_msgid     | <p class="shorttext synchronized" lang="en">Default message class</p>
      "! @parameter iv_def_probclass | <p class="shorttext synchronized" lang="en">Default problem class (use const. C_PROBCLAS_*)</p>
      "! @parameter iv_mode          | <p class="shorttext synchronized" lang="en">Application Log: Operating mode  (use const. C_MODE_*)</p>
      "! @parameter iv_del_before    | <p class="shorttext synchronized" lang="en">Application Log: Keep log until expiry</p>
      "! @parameter iv_del_date      | <p class="shorttext synchronized" lang="en">Application Log: Expiration Date</p>
      "! @parameter iv_log_cls_name  | <p class="shorttext synchronized" lang="en">Name of (inherited) application log class</p>
      "! @parameter ro_log           | <p class="shorttext synchronized" lang="en">CA-TBX: Application log (BAL)</p>
      get_instance
        IMPORTING
          iv_object        TYPE balobj_d
          iv_subobj        TYPE balsubobj
          is_lpor          TYPE sibflporb
          iv_add_key       TYPE zca_d_log_add_key OPTIONAL
          iv_extnumber     TYPE balnrext   OPTIONAL
          VALUE(iv_repid)  TYPE syrepid    DEFAULT sy-cprog
          VALUE(iv_tcode)  TYPE syst_tcode DEFAULT sy-tcode
          iv_def_msgid     TYPE symsgid    OPTIONAL
          iv_def_probclass TYPE balprobcl  DEFAULT zcl_ca_c_log=>problem_class-very_important
          iv_mode          TYPE balmode    DEFAULT 'D'
          iv_del_before    TYPE abap_bool  DEFAULT abap_false
          iv_del_date      TYPE aldate_del OPTIONAL
          iv_log_cls_name  TYPE seoclsname DEFAULT 'ZCL_CA_LOG' ##no_text
        RETURNING
          VALUE(ro_log)    TYPE REF TO zif_ca_log,

      "! <p class="shorttext synchronized" lang="en">Get instance for a single log handle</p>
      "!
      "! @parameter iv_loghandle | <p class="shorttext synchronized" lang="en">Application Log: Log Handle</p>
      "! @parameter is_lpor      | <p class="shorttext synchronized" lang="en">Business object/class key - as buffering key</p>
      "! @parameter iv_repid     | <p class="shorttext synchronized" lang="en">ABAP Program: Current Main Program</p>
      "! @parameter iv_tcode     | <p class="shorttext synchronized" lang="en">ABAP System Field: Current Transaction Code</p>
      "! @parameter iv_mode      | <p class="shorttext synchronized" lang="en">Application Log: Operating mode  (use const. C_MODE_*)</p>
      "! @parameter ro_log       | <p class="shorttext synchronized" lang="en">CA-TBX: Application log (BAL)</p>
      get_instance_to_log_handle
        IMPORTING
          iv_loghandle    TYPE balloghndl
          is_lpor         TYPE sibflporb OPTIONAL
          VALUE(iv_repid) TYPE syrepid DEFAULT sy-cprog
          VALUE(iv_tcode) TYPE syst_tcode DEFAULT sy-tcode
          iv_mode         TYPE balmode DEFAULT zcl_ca_c_log=>operating_mode-dialog
        RETURNING
          VALUE(ro_log)   TYPE REF TO zif_ca_log,

      "! <p class="shorttext synchronized" lang="en">Get list of logs to reference object</p>
      "!
      "! @parameter is_lpor    | <p class="shorttext synchronized" lang="en">Business object key</p>
      "! @parameter iv_add_key | <p class="shorttext synchronized" lang="en">Additional key (for several logs to one business object)</p>
      "! @parameter iv_msgty   | <p class="shorttext synchronized" lang="en">ABAP System Field: Message Type</p>
      "! @parameter result     | <p class="shorttext synchronized" lang="en">Messages to the reference object</p>
      get_messages_to_reference_obj
        IMPORTING
          is_lpor       TYPE sibflporb
          iv_add_key    TYPE zca_d_log_add_key OPTIONAL
          iv_msgty      TYPE symsgty OPTIONAL
        RETURNING
          VALUE(result) TYPE aco_tt_bal_msg.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter iv_object        | <p class="shorttext synchronized" lang="en">Application Log: Object Name (Application Code)</p>
      "! @parameter iv_subobj        | <p class="shorttext synchronized" lang="en">Application Log: Subobject</p>
      "! @parameter is_lpor          | <p class="shorttext synchronized" lang="en">Business object/class key - BOR Compatible</p>
      "! @parameter iv_add_key       | <p class="shorttext synchronized" lang="en">Additional key (for several logs to one business object)</p>
      "! @parameter iv_extnumber     | <p class="shorttext synchronized" lang="en">Application Log: External ID</p>
      "! @parameter iv_loghandle     | <p class="shorttext synchronized" lang="en">Application Log: Log Handle</p>
      "! @parameter iv_repid         | <p class="shorttext synchronized" lang="en">ABAP Program: Current Main Program</p>
      "! @parameter iv_tcode         | <p class="shorttext synchronized" lang="en">ABAP System Field: Current Transaction Code</p>
      "! @parameter iv_def_msgid     | <p class="shorttext synchronized" lang="en">Default message class</p>
      "! @parameter iv_def_probclass | <p class="shorttext synchronized" lang="en">Default problem class (use const. C_PROBCLAS_*)</p>
      "! @parameter iv_mode          | <p class="shorttext synchronized" lang="en">Application Log: Operating mode  (use const. C_MODE_*)</p>
      "! @parameter iv_del_before    | <p class="shorttext synchronized" lang="en">Application Log: Keep log until expiry</p>
      "! @parameter iv_del_date      | <p class="shorttext synchronized" lang="en">Application Log: Expiration Date</p>
      constructor
        IMPORTING
          iv_object        TYPE balobj_d
          iv_subobj        TYPE balsubobj
          is_lpor          TYPE sibflporb  OPTIONAL
          iv_add_key       TYPE zca_d_log_add_key OPTIONAL
          iv_extnumber     TYPE balnrext   OPTIONAL
          iv_loghandle     TYPE balloghndl OPTIONAL
          VALUE(iv_repid)  TYPE syrepid    DEFAULT sy-cprog
          VALUE(iv_tcode)  TYPE syst_tcode DEFAULT sy-tcode
          iv_def_msgid     TYPE symsgid    OPTIONAL
          iv_def_probclass TYPE balprobcl  DEFAULT zcl_ca_c_log=>problem_class-important
          iv_mode          TYPE balmode    DEFAULT zcl_ca_c_log=>operating_mode-dialog
          iv_del_before    TYPE abap_bool  DEFAULT abap_false
          iv_del_date      TYPE aldate_del OPTIONAL.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   a l i a s e s
    ALIASES:
*     Problem classes
      c_fname_s_excep_srcpos  FOR zif_ca_c_log_techn~c_fname_s_excep_srcpos,
      c_fname_class           FOR zif_ca_c_log_techn~c_fname_class,
      c_fname_method          FOR zif_ca_c_log_techn~c_fname_method,
      c_fname_line            FOR zif_ca_c_log_techn~c_fname_line,
*     Signs and options for RANGES/SELECT-OPTIONS
      c_sign_i                FOR  if_fsbp_const_range~sign_include,
      c_sign_e                FOR  if_fsbp_const_range~sign_exclude,
      c_opt_eq                FOR  if_fsbp_const_range~option_equal,
      c_opt_ne                FOR  if_fsbp_const_range~option_not_equal,
      c_opt_gt                FOR  if_fsbp_const_range~option_greater,
      c_opt_ge                FOR  if_fsbp_const_range~option_greater_equal,
      c_opt_lt                FOR  if_fsbp_const_range~option_less,
      c_opt_le                FOR  if_fsbp_const_range~option_less_equal,
      c_opt_cp                FOR  if_fsbp_const_range~option_contains_pattern,
      c_opt_np                FOR  if_fsbp_const_range~option_not_contains_pattern,
      c_opt_bt                FOR  if_fsbp_const_range~option_between,
      c_opt_nb                FOR  if_fsbp_const_range~option_not_between.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Name function module: Call back for read of source position</p>
      c_fm_name_cb_read       TYPE rs38l_fnam        VALUE 'Z_CA_LOG_CB_READ'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Name function module: Call back to handle user commands</p>
      c_fm_name_cb_ucom       TYPE rs38l_fnam        VALUE 'Z_CA_LOG_CB_UCOM'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Name function module: Call back to pass source positions</p>
      c_fm_name_cb_set_srcpos TYPE rs38l_fnam        VALUE 'Z_CA_LOG_CB_SET_SRCPOS'  ##no_text.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for select options / range tables</p>
      mo_sel_options   TYPE REF TO zcl_ca_c_sel_options,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Application log - Business object reference</p>
      mt_objref        TYPE zca_tt_log_objref,
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Application log - Source code position</p>
      mt_srcpos        TYPE zca_tt_log_srcpos,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Application Log: Log header data</p>
      ms_log           TYPE bal_s_log,
      "! <p class="shorttext synchronized" lang="en">Business Object key</p>
      ms_lpor          TYPE sibflporb,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Additional key to log reference / BO</p>
      mv_add_key       TYPE zca_d_log_add_key,
      "! <p class="shorttext synchronized" lang="en">Default message class</p>
      mv_def_msgid     TYPE symsgid,
      "! <p class="shorttext synchronized" lang="en">Default message problem class</p>
      mv_def_probclass TYPE balprobcl,
      "! <p class="shorttext synchronized" lang="en">Application Log: Log Handle</p>
      mv_loghndl       TYPE balloghndl.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Add message to log</p>
      "!
      "! @parameter iv_loghndl | <p class="shorttext synchronized" lang="en">Application Log: Log Handle</p>
      "! @parameter is_msg     | <p class="shorttext synchronized" lang="en">Application Log: Message Data</p>
      "! @parameter rs_msgh    | <p class="shorttext synchronized" lang="en">Application Log: Message handle</p>
      intern_add_message
        IMPORTING
          iv_loghndl     TYPE balloghndl
          is_msg         TYPE bal_s_msg
        RETURNING
          VALUE(rs_msgh) TYPE balmsghndl,

      "! <p class="shorttext synchronized" lang="en">Create new log</p>
      "!
      "! @parameter is_log     | <p class="shorttext synchronized" lang="en">Application Log: Log header data</p>
      "! @parameter rv_loghndl | <p class="shorttext synchronized" lang="en">Application Log: Log Handle</p>
      intern_create
        IMPORTING
          is_log            TYPE bal_s_log
        RETURNING
          VALUE(rv_loghndl) TYPE balloghndl,

      "! <p class="shorttext synchronized" lang="en">Change log header</p>
      "!
      "! @parameter iv_loghndl | <p class="shorttext synchronized" lang="en">Application Log: Log Handle</p>
      "! @parameter is_log     | <p class="shorttext synchronized" lang="en">Application Log: Log header data</p>
      intern_change
        IMPORTING
          iv_loghndl TYPE balloghndl
          is_log     TYPE bal_s_log,

      "! <p class="shorttext synchronized" lang="en">Display log</p>
      "!
      "! @parameter io_parent | <p class="shorttext synchronized" lang="en">Parent container to display log in-place</p>
      "! @parameter is_prof   | <p class="shorttext synchronized" lang="en">Application Log: Log Output Format Profile</p>
      "! @parameter it_logh   | <p class="shorttext synchronized" lang="en">Application Log: Log Handle Table</p>
      "! @parameter it_msgh   | <p class="shorttext synchronized" lang="en">Application Log: Message Handle Table</p>
      "! @parameter it_srcpos | <p class="shorttext synchronized" lang="en">CA-TBX: Application log - Source code position</p>
      intern_display
        IMPORTING
          io_parent TYPE REF TO cl_gui_container OPTIONAL
          is_prof   TYPE bal_s_prof
          it_logh   TYPE bal_t_logh
          it_msgh   TYPE bal_t_msgh        OPTIONAL
          it_srcpos TYPE zca_tt_log_srcpos OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Get lognr from loghandle</p>
      "!
      "! @parameter iv_log_handle | <p class="shorttext synchronized" lang="en">Application Log: Log Handle</p>
      "! @parameter rv_lognr      | <p class="shorttext synchronized" lang="en">Application log: log number</p>
      intern_get_lognr_from_handle
        IMPORTING
          iv_log_handle   TYPE balloghndl
        RETURNING
          VALUE(rv_lognr) TYPE balognr,

      "! <p class="shorttext synchronized" lang="en">Get list of logs to reference object</p>
      "!
      "! @parameter is_lpor    | <p class="shorttext synchronized" lang="en">Business object key</p>
      "! @parameter iv_add_key | <p class="shorttext synchronized" lang="en">Additional key (for several logs to one business object)</p>
      "! @parameter result     | <p class="shorttext synchronized" lang="en">Prepared filter for log number and log handle</p>
      intern_get_log_list_to_ref_obj
        IMPORTING
          is_lpor       TYPE sibflporb
          iv_add_key    TYPE zca_d_log_add_key OPTIONAL
        RETURNING
          VALUE(result) TYPE bal_s_lfil,

      "! <p class="shorttext synchronized" lang="en">Get display profile</p>
      "!
      "! @parameter iv_title       | <p class="shorttext synchronized" lang="en">Application Log: Dynpro Title</p>
      "! @parameter iv_popup       | <p class="shorttext synchronized" lang="en">X = Display as popup</p>
      "! @parameter iv_use_grid    | <p class="shorttext synchronized" lang="en">X = Display as grid</p>
      "! @parameter iv_report      | <p class="shorttext synchronized" lang="en">Program for saving layout variant</p>
      "! @parameter iv_show_all    | <p class="shorttext synchronized" lang="en">X = Show all messages</p>
      "! @parameter iv_opt_cwidth  | <p class="shorttext synchronized" lang="en">X = Optimize column width</p>
      "! @parameter iv_disp_srcpos | <p class="shorttext synchronized" lang="en">X = Display source code position of exception</p>
      "! @parameter rs_prof        | <p class="shorttext synchronized" lang="en">Application Log: Log Output Format Profile</p>
      intern_get_profile
        IMPORTING
          iv_title       TYPE baltitle  OPTIONAL
          iv_popup       TYPE abap_bool DEFAULT abap_false
          iv_use_grid    TYPE abap_bool DEFAULT abap_true
          iv_report      TYPE syrepid   OPTIONAL
          iv_show_all    TYPE abap_bool DEFAULT abap_false
          iv_opt_cwidth  TYPE abap_bool DEFAULT abap_true
          iv_disp_srcpos TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rs_prof) TYPE bal_s_prof,

      "! <p class="shorttext synchronized" lang="en">Load messages and source code positions to exceptions</p>
      "!
      "! @parameter it_lhdr   | <p class="shorttext synchronized" lang="en">Application Log: Log header data table</p>
      "! @parameter et_logh   | <p class="shorttext synchronized" lang="en">Application Log: Log Handle Table</p>
      "! @parameter et_msgh   | <p class="shorttext synchronized" lang="en">Application Log: Message Handle Table</p>
      "! @parameter et_srcpos | <p class="shorttext synchronized" lang="en">CA-TBX: Application log - Source code position</p>
      intern_load
        IMPORTING
          it_lhdr   TYPE balhdr_t
        EXPORTING
          et_logh   TYPE bal_t_logh
          et_msgh   TYPE bal_t_msgh
          et_srcpos TYPE zca_tt_log_srcpos,

      "! <p class="shorttext synchronized" lang="en">Load messages and source code positions to exceptions</p>
      "!
      "! @parameter it_msgh   | <p class="shorttext synchronized" lang="en">Application Log: Message Handle Table</p>
      "! @parameter rt_srcpos | <p class="shorttext synchronized" lang="en">CA-TBX: Application log - Source code position</p>
      intern_load_pos
        IMPORTING
          it_msgh          TYPE bal_t_msgh
        RETURNING
          VALUE(rt_srcpos) TYPE zca_tt_log_srcpos,

      "! <p class="shorttext synchronized" lang="en">Read log message</p>
      "!
      "! @parameter is_msgh | <p class="shorttext synchronized" lang="en">Application Log: Message handle</p>
      "! @parameter rs_msg  | <p class="shorttext synchronized" lang="en">Application Log: Message Data</p>
      intern_msg_read
        IMPORTING
          is_msgh       TYPE balmsghndl
        RETURNING
          VALUE(rs_msg) TYPE bal_s_msg,

      "! <p class="shorttext synchronized" lang="en">Refresh memory</p>
      "!
      "! @parameter iv_loghndl | <p class="shorttext synchronized" lang="en">Application Log: Log Handle</p>
      intern_refresh
        IMPORTING
          iv_loghndl TYPE balloghndl,

      "! <p class="shorttext synchronized" lang="en">Save log</p>
      "!
      "! @parameter it_logh        | <p class="shorttext synchronized" lang="en">Application Log: Log Handle Table</p>
      "! @parameter iv_in_upd_task | <p class="shorttext synchronized" lang="en">X = Save log in update task</p>
      "! @parameter rt_log_numbers | <p class="shorttext synchronized" lang="en">Application Log: Newly-Assigned LOGNUMBER Table</p>
      intern_save
        IMPORTING
          it_logh               TYPE bal_t_logh
          iv_in_upd_task        TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rt_log_numbers) TYPE bal_t_lgnm,

      "! <p class="shorttext synchronized" lang="en">Search log</p>
      "!
      "! @parameter is_lfil | <p class="shorttext synchronized" lang="en">Application Log: Log filter criteria</p>
      "! @parameter rt_lhdr | <p class="shorttext synchronized" lang="en">Application Log: Log header data table</p>
      intern_search
        IMPORTING
          is_lfil        TYPE bal_s_lfil
        RETURNING
          VALUE(rt_lhdr) TYPE balhdr_t,

      "! <p class="shorttext synchronized" lang="en">Search for logs and display</p>
      "!
      "! @parameter is_log_filter  | <p class="shorttext synchronized" lang="en">Application Log: Log filter criteria</p>
      "! @parameter io_parent      | <p class="shorttext synchronized" lang="en">Parent container to display log in-place</p>
      "! @parameter iv_title       | <p class="shorttext synchronized" lang="en">Application Log: Dynpro Title</p>
      "! @parameter iv_popup       | <p class="shorttext synchronized" lang="en">X = Display as popup</p>
      "! @parameter iv_opt_cwidth  | <p class="shorttext synchronized" lang="en">X = Optimize column width</p>
      "! @parameter iv_disp_srcpos | <p class="shorttext synchronized" lang="en">X = Display source code positions</p>
      "! @parameter is_profile     | <p class="shorttext synchronized" lang="en">Profile with settings log output / display</p>
      intern_search_n_display
        IMPORTING
          is_log_filter  TYPE bal_s_lfil
          io_parent      TYPE REF TO cl_gui_container OPTIONAL
          iv_title       TYPE baltitle    OPTIONAL
          iv_popup       TYPE abap_bool DEFAULT abap_false
          iv_opt_cwidth  TYPE abap_bool DEFAULT abap_true
          iv_disp_srcpos TYPE abap_bool DEFAULT abap_false
          is_profile     TYPE bal_s_prof  OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Has user authority to display development objects</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">X = User has authority to display development objects</p>
      intern_has_user_developm_auth
        RETURNING
          VALUE(result) TYPE abap_bool.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Save ref objects with lognr</p>
      "!
      "! @parameter ir_log_number | <p class="shorttext synchronized" lang="en">Application log: log number</p>
      "! @parameter rv_changed    | <p class="shorttext synchronized" lang="en">X = Messages are saved</p>
      intern_save_logref
        IMPORTING
          ir_log_number     TYPE REF TO bal_s_lgnm
        RETURNING
          VALUE(rv_changed) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">Save error positions with lognr</p>
      "!
      "! @parameter ir_log_number | <p class="shorttext synchronized" lang="en">Application log: log number</p>
      "! @parameter rv_changed    | <p class="shorttext synchronized" lang="en">X = Messages are saved</p>
      intern_save_srcpos
        IMPORTING
          ir_log_number     TYPE REF TO bal_s_lgnm
        RETURNING
          VALUE(rv_changed) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">Check log is opened?</p>
      is_open,

      "! <p class="shorttext synchronized" lang="en">Check if message exist in current system</p>
      "!
      "! @parameter iv_msgid | <p class="shorttext synchronized" lang="en">Message id</p>
      "! @parameter iv_msgno | <p class="shorttext synchronized" lang="en">Message number</p>
      "! @parameter rv_exist | <p class="shorttext synchronized" lang="en">X = Messages exists</p>
      message_exist
        IMPORTING
          iv_msgid        TYPE syst_msgid
          iv_msgno        TYPE syst_msgno
        RETURNING
          VALUE(rv_exist) TYPE abap_bool.

* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   l o c a l   t y p e   d e f i n i t i o n
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Buffered instance</p>
      BEGIN OF ty_s_buffer.
        INCLUDE TYPE sibflporb AS s_lpor.
    TYPES:
      " e. g. for work item id, were several WIs can occur to one BO/BC
        add_key TYPE sibfinstid,
        o_log   TYPE REF TO zif_ca_log,
      END   OF ty_s_buffer,
      "! <p class="shorttext synchronized" lang="en">Instance buffer</p>
      ty_t_buffer TYPE SORTED TABLE OF ty_s_buffer
                              WITH NON-UNIQUE KEY primary_key
                                       COMPONENTS instid  typeid  catid  add_key
                              WITH UNIQUE HASHED KEY ky_log_instance
                                       COMPONENTS o_log,

      "! <p class="shorttext synchronized" lang="en">Buffered message</p>
      BEGIN OF ty_s_msg,
        msgid TYPE syst_msgid,
        msgno TYPE syst_msgno,
        exist TYPE abap_bool,
      END   OF ty_s_msg,
      "! <p class="shorttext synchronized" lang="en">Message buffer</p>
      ty_t_msgs TYPE SORTED TABLE OF ty_s_msg
                                     WITH UNIQUE KEY msgid  msgno.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Instance buffer</p>
      mt_buffer TYPE ty_t_buffer,
      "! <p class="shorttext synchronized" lang="en">Message buffer</p>
      mt_msgs   TYPE ty_t_msgs.

ENDCLASS.



CLASS ZCL_CA_LOG IMPLEMENTATION.


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    "Is deletion date valid
    IF   iv_del_before EQ abap_true AND
       ( iv_del_date   IS INITIAL    OR
         iv_del_date   LT sy-datlo ).
      RAISE EXCEPTION TYPE zcx_ca_log
        EXPORTING
          textid   = zcx_ca_log=>del_date_invalid
          mv_msgty = c_msgty_e
          mv_msgv1 = CONV #( |{ iv_del_date DATE = USER }| ).
    ENDIF.

    "Prepare constant instances
    mo_log_options = zcl_ca_c_log=>get_instance( ).
    mo_sel_options = zcl_ca_c_sel_options=>get_instance( ).

    "Keep global log settings
    mv_def_msgid      = iv_def_msgid.     "Default message class
    mv_def_probclass  = iv_def_probclass. "Default problem class

    ms_log-object     = iv_object.        "Object type (e. g. BUS2012 or self-defined)
    ms_log-subobject  = iv_subobj.        "Sub object
    ms_log-extnumber  = iv_extnumber.     "External identifier
    ms_log-alprog     = iv_repid.         "Program name
    ms_log-aluser     = sy-uname.         "Current user
    ms_log-altcode    = iv_tcode.         "Transaction code
    ms_log-almode     = iv_mode.          "Processing mode (Dialog, Batch, Batch-Input)
    ms_log-del_before = abap_true.        "Keep log until deletion date
    ms_log-aldate_del = iv_del_date.      "Deletion date

    ms_lpor    = is_lpor.
    mv_add_key = iv_add_key.
    set_additional_ref_object_id( is_lpor    = ms_lpor
                                  iv_add_key = mv_add_key ).

    "Open new log
    IF iv_loghandle IS INITIAL.
      mv_loghndl = zcl_ca_log=>intern_create( ms_log ).

    ELSE.
      mv_loghndl = iv_loghandle.
      "Load BAL messages
      zcl_ca_log=>intern_load(
                zcl_ca_log=>intern_search( VALUE #( log_handle = VALUE #( ( sign   = c_sign_i
                                                                            option = c_opt_eq
                                                                            low    = iv_loghandle ) ) ) ) ).
    ENDIF.
  ENDMETHOD.                    "constructor


  METHOD display_for_loghandle.
    "-----------------------------------------------------------------*
    "   Display one or more saved application logs
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_log_filter        TYPE bal_s_lfil.

    "Set filter criteria
    IF iv_loghandle IS NOT INITIAL.
      ls_log_filter-log_handle = VALUE #( ( sign   = c_sign_i
                                            option = c_opt_eq
                                            low    = iv_loghandle ) ).

    ELSEIF it_loghandle IS NOT INITIAL.
      ls_log_filter-log_handle = it_loghandle.

    ELSE.
      "Parameter error occurred
      RAISE EXCEPTION TYPE zcx_ca_log
        EXPORTING
          textid   = zcx_ca_log=>at_least_one
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IV_LOGHANDLE'
          mv_msgv2 = 'IT_LOGHANDLE' ##no_text.
    ENDIF.

    zcl_ca_log=>intern_search_n_display( is_log_filter  = ls_log_filter
                                         io_parent      = io_parent
                                         iv_title       = iv_title
                                         iv_popup       = iv_popup
                                         iv_opt_cwidth  = iv_opt_cwidth
                                         iv_disp_srcpos = iv_disp_srcpos
                                         is_profile     = is_profile ).
  ENDMETHOD.                    "display_for_loghandle


  METHOD display_for_lognr.
    "-----------------------------------------------------------------*
    "   Display one or more saved application logs
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_log_filter        TYPE bal_s_lfil.

    "Set filter criteria
    IF iv_lognumber IS NOT INITIAL.
      ls_log_filter-lognumber = VALUE #( ( sign   = c_sign_i
                                           option = c_opt_eq
                                           low    = iv_lognumber ) ).


    ELSEIF it_lognumber IS NOT INITIAL.
      ls_log_filter-lognumber = it_lognumber.

    ELSE.
      "Parameter error occurred
      RAISE EXCEPTION TYPE zcx_ca_log
        EXPORTING
          textid   = zcx_ca_log=>at_least_one
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IV_LOGNUMBER'
          mv_msgv2 = 'IT_LOGNUMBER' ##no_text.
    ENDIF.

    zcl_ca_log=>intern_search_n_display( is_log_filter  = ls_log_filter
                                         io_parent      = io_parent
                                         iv_title       = iv_title
                                         iv_popup       = iv_popup
                                         iv_opt_cwidth  = iv_opt_cwidth
                                         iv_disp_srcpos = iv_disp_srcpos
                                         is_profile     = is_profile ).
  ENDMETHOD.                    "display_for_lognr


  METHOD display_for_reference_obj.
    "-----------------------------------------------------------------*
    "   Display all logs for reference object and key
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_title TYPE baltitle.

    "Set window title
    IF iv_title IS NOT INITIAL.
      lv_title = iv_title.
    ELSE.
      "TEXT-T01 = Log(s) to object
      DATA(lv_obj_key) = zcl_ca_wf_utils=>prepare_object_key_for_ouput( is_lpor ).
      lv_title = condense( |{ TEXT-t01 } { is_lpor-typeid } { lv_obj_key } { iv_add_key }| ).
    ENDIF.

    "Read log number for object type and key
    DATA(ls_log_filter) = zcl_ca_log=>intern_get_log_list_to_ref_obj( is_lpor    = is_lpor
                                                                      iv_add_key = iv_add_key ).

    "Display all logs
    zcl_ca_log=>intern_search_n_display( is_log_filter  = ls_log_filter
                                         io_parent      = io_parent
                                         iv_title       = iv_title
                                         iv_popup       = iv_popup
                                         iv_opt_cwidth  = iv_opt_cwidth
                                         iv_disp_srcpos = iv_disp_srcpos
                                         is_profile     = is_profile ).
  ENDMETHOD.                    "display_for_refobj


  METHOD free_buffer.
    "-----------------------------------------------------------------*
    "   Release buffered instances
    "-----------------------------------------------------------------*
    LOOP AT zcl_ca_log=>mt_buffer REFERENCE INTO DATA(lr_buffer).
      lr_buffer->o_log->close( ).
    ENDLOOP.

    FREE zcl_ca_log=>mt_buffer.
  ENDMETHOD.                    "free_buffer


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance to LPOR and, if needed, to an additional key
    "-----------------------------------------------------------------*
    IF is_lpor-instid IS INITIAL OR
       is_lpor-typeid IS INITIAL OR
       is_lpor-catid  IS INITIAL.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_log
        EXPORTING
          textid   = zcx_ca_log=>param_invalid
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IS_LPOR'
          mv_msgv2 = 'SPACE' ##no_text.
    ENDIF.

    TRY.
        DATA(ls_buffer) = zcl_ca_log=>mt_buffer[ s_lpor  = is_lpor
                                                 add_key = iv_add_key ].

      CATCH cx_sy_itab_line_not_found.
        CREATE OBJECT ls_buffer-o_log TYPE (iv_log_cls_name)
          EXPORTING
            iv_object        = iv_object
            iv_subobj        = iv_subobj
            is_lpor          = is_lpor
            iv_add_key       = iv_add_key
            iv_extnumber     = iv_extnumber
            iv_repid         = iv_repid
            iv_tcode         = iv_tcode
            iv_def_msgid     = iv_def_msgid
            iv_def_probclass = iv_def_probclass
            iv_mode          = iv_mode
            iv_del_before    = iv_del_before
            iv_del_date      = iv_del_date.

        ls_buffer-s_lpor  = is_lpor.
        ls_buffer-add_key = iv_add_key.
        INSERT ls_buffer INTO TABLE zcl_ca_log=>mt_buffer.
    ENDTRY.

    ro_log = ls_buffer-o_log.
  ENDMETHOD.                    "get_instance


  METHOD get_instance_to_log_handle.
    "-----------------------------------------------------------------*
    "   Get instance for a single log handle
    "-----------------------------------------------------------------*
    IF iv_loghandle IS INITIAL.
      "Parameter '&1' is not specified
      RAISE EXCEPTION TYPE zcx_ca_log
        EXPORTING
          textid   = zcx_ca_log=>param_not_supplied
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IV_LOGHANDLE' ##no_text.
    ENDIF.

    "Search logs
    DATA(lt_lhdr) = intern_search(
                          VALUE #( log_handle = VALUE #( ( sign   = c_sign_i
                                                           option = c_opt_eq
                                                           low    = iv_loghandle ) ) ) ).

    "Load BAL messages
    intern_load( lt_lhdr ).

    DATA(ls_lhdr) = lt_lhdr[ 1 ].

    IF is_lpor IS INITIAL.
      ro_log = CAST #( NEW zcl_ca_log( iv_object     = ls_lhdr-object
                                       iv_subobj     = ls_lhdr-subobject
                                       iv_extnumber  = ls_lhdr-extnumber
                                       iv_loghandle  = ls_lhdr-log_handle
                                       iv_repid      = ls_lhdr-alprog
                                       iv_tcode      = ls_lhdr-altcode
                                       iv_mode       = iv_mode
                                       iv_del_before = ls_lhdr-del_before
                                       iv_del_date   = ls_lhdr-aldate_del ) ).

    ELSE.
      ro_log = get_instance( iv_object     = ls_lhdr-object
                             iv_subobj     = ls_lhdr-subobject
                             is_lpor       = is_lpor
                             iv_extnumber  = ls_lhdr-extnumber
                             iv_repid      = ls_lhdr-alprog
                             iv_tcode      = ls_lhdr-altcode
                             iv_mode       = iv_mode
                             iv_del_before = ls_lhdr-del_before
                             iv_del_date   = ls_lhdr-aldate_del ).
    ENDIF.
  ENDMETHOD.                    "get_instance_to_log_handle


  METHOD get_messages_to_reference_obj.
    "-----------------------------------------------------------------*
    "   Get messages to a reference object
    "-----------------------------------------------------------------*
    DATA(lt_log_headers) = intern_search( is_lfil = intern_get_log_list_to_ref_obj( is_lpor    = is_lpor
                                                                                    iv_add_key = iv_add_key ) ).
    intern_load(
            EXPORTING
              it_lhdr = lt_log_headers
            IMPORTING
              et_msgh = DATA(lt_msg_handles) ).

    LOOP AT lt_msg_handles REFERENCE INTO DATA(lr_msg_handle).
      APPEND zcl_ca_log=>intern_msg_read( lr_msg_handle->* ) TO result.
    ENDLOOP.

    IF iv_msgty IS NOT INITIAL.
      DELETE result WHERE msgty NE iv_msgty.
    ENDIF.
  ENDMETHOD.                    "get_messages_to_reference_obj


  METHOD intern_add_message.
    "-----------------------------------------------------------------*
    "   Add message to BAL
    "-----------------------------------------------------------------*
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = iv_loghndl
        i_s_msg          = is_msg
      IMPORTING
        e_s_msg_handle   = rs_msgh
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                                iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                                                iv_function = 'BAL_LOG_MSG_ADD' ##no_text
                                                                iv_subrc    = sy-subrc ) ).
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "intern_add_message


  METHOD intern_create.
    "-----------------------------------------------------------------*
    "   Create BAL instance
    "-----------------------------------------------------------------*
    "neues Log öffnen
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = is_log
      IMPORTING
        e_log_handle            = rv_loghndl
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                                iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                                                iv_function = 'BAL_LOG_CREATE' ##no_text
                                                                iv_subrc    = sy-subrc ) ).
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "intern_create


  METHOD intern_change.
    "-----------------------------------------------------------------*
    "   Change log header
    "-----------------------------------------------------------------*
    "Provide BAL
    CALL FUNCTION 'BAL_LOG_HDR_CHANGE'
      EXPORTING
        i_log_handle            = iv_loghndl
        i_s_log                 = is_log
      EXCEPTIONS
        log_not_found           = 1
        log_header_inconsistent = 2
        OTHERS                  = 3.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                               iv_excp_cls    = zcx_ca_log=>c_zcx_ca_log
                                                               iv_function    = 'BAL_LOG_HDR_CHANGE'
                                                               iv_subrc       = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "zif_ca_log~set_additional_ref_object_id


  METHOD intern_display.
    "-----------------------------------------------------------------*
    "   Display BAL for log object with display profile
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error             TYPE REF TO zcx_ca_log.

    "Set source pos. data into global data of call back function group (= CB)
    IF is_prof-clbk_read-userexitf EQ c_fm_name_cb_read.
      CALL FUNCTION 'Z_CA_LOG_CB_SET_SRCPOS'
        EXPORTING
          it_srcpos = it_srcpos.
    ENDIF.

    IF io_parent IS BOUND.
      "Display log in-place
      CALL FUNCTION 'BAL_CNTL_CREATE'
        EXPORTING
          i_container          = io_parent
          i_s_display_profile  = is_prof
          i_t_log_handle       = it_logh
          i_t_msg_handle       = it_msgh
        EXCEPTIONS
          profile_inconsistent = 1
          internal_error       = 2
          OTHERS               = 3.
      IF sy-subrc NE 0.
        lx_error = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                            iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                                            iv_function = 'BAL_CNTL_CREATE'
                                                            iv_subrc    = sy-subrc ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
      ENDIF.

    ELSEIF it_msgh IS     SUPPLIED AND
           it_msgh IS NOT INITIAL.
      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
        EXPORTING
          i_s_display_profile  = is_prof
          i_t_log_handle       = it_logh
          i_t_msg_handle       = it_msgh
        EXCEPTIONS
          profile_inconsistent = 1
          internal_error       = 2
          no_data_available    = 3
          no_authority         = 4
          OTHERS               = 5.
      IF sy-subrc NE 0.
        lx_error = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                            iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                                            iv_function = 'BAL_DSP_LOG_DISPLAY' ##no_text
                                                            iv_subrc    = sy-subrc ) ).
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
      ENDIF.

    ELSE.
      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
        EXPORTING
          i_s_display_profile  = is_prof
          i_t_log_handle       = it_logh
        EXCEPTIONS
          profile_inconsistent = 1
          internal_error       = 2
          no_data_available    = 3
          no_authority         = 4
          OTHERS               = 5.
      IF sy-subrc NE 0.
        lx_error = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                            iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                                            iv_function = 'BAL_DSP_LOG_DISPLAY' ##no_text
                                                            iv_subrc    = sy-subrc ) ).
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "intern_display


  METHOD intern_get_lognr_from_handle.
    "-----------------------------------------------------------------*
    "   Get BAL log number from handle
    "-----------------------------------------------------------------*
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle  = iv_log_handle
      IMPORTING
        e_lognumber   = rv_lognr
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                                iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                                                iv_function = 'BAL_LOG_HDR_READ' ##no_text
                                                                iv_subrc    = sy-subrc ) ).
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "intern_get_lognr_from_handle


  METHOD intern_get_log_list_to_ref_obj.
    "-----------------------------------------------------------------*
    "   Get list of logs to reference object
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_obj_refs          TYPE zca_tt_log_objref.

    IF iv_add_key IS INITIAL.
      SELECT * INTO  TABLE lt_obj_refs
               FROM  zca_log_objref
               WHERE instid EQ is_lpor-instid
                 AND typeid EQ is_lpor-typeid
                 AND catid  EQ is_lpor-catid.

    ELSE.
      DATA(lv_add_key) = iv_add_key.
      IF strlen( lv_add_key ) LE 30.
        lv_add_key = |%{ lv_add_key }%|.
      ELSE.
        lv_add_key = |%{ lv_add_key(30) }%|.
      ENDIF.
      SELECT * INTO  TABLE lt_obj_refs
               FROM  zca_log_objref
               WHERE instid  EQ is_lpor-instid
                 AND typeid  EQ is_lpor-typeid
                 AND catid   EQ is_lpor-catid
                 AND add_key LIKE lv_add_key.
    ENDIF.

    IF sy-subrc NE 0.
      DATA(lv_obj_key) = zcl_ca_wf_utils=>prepare_object_key_for_ouput( is_lpor ).
      "No entry exists for & in Table &
      RAISE EXCEPTION TYPE zcx_ca_log
        EXPORTING
          textid   = zcx_ca_log=>no_entry
          mv_msgty = c_msgty_e
          mv_msgv1 = CONV #( |{ is_lpor-catid } { is_lpor-typeid } { lv_obj_key } { iv_add_key }| )
          mv_msgv2 = 'ZCA_LOG_OBJREF' ##no_text.
    ENDIF.

    DATA(lo_sel_opt) = zcl_ca_c_sel_options=>get_instance( ).
    result-lognumber = VALUE #( FOR ls_obj_ref_logno IN lt_obj_refs
                                              ( sign   = lo_sel_opt->sign-incl
                                                option = lo_sel_opt->option-eq
                                                low    = ls_obj_ref_logno-lognr ) ).
    SORT result-lognumber.
    DELETE ADJACENT DUPLICATES FROM result-lognumber COMPARING ALL FIELDS.
  ENDMETHOD.                    "intern_get_log_list_to_ref_obj


  METHOD intern_get_profile.
    "-----------------------------------------------------------------*
    "   Get display profile
    "-----------------------------------------------------------------*
    IF iv_popup EQ abap_true.
      "Display as popup for selection by user
      CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
        IMPORTING
          e_s_display_profile = rs_prof.

    ELSE.
      "Single log default
      CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
        IMPORTING
          e_s_display_profile = rs_prof.
    ENDIF.

    "Set several values to control layout of log display
    rs_prof-show_all   = iv_show_all.
    rs_prof-use_grid   = iv_use_grid.
    rs_prof-cwidth_opt = iv_opt_cwidth.

    rs_prof-disvariant-report = COND #( WHEN iv_report IS NOT INITIAL
                                          THEN iv_report
                                          ELSE space ).

    rs_prof-title = COND #( WHEN iv_title IS NOT INITIAL
                              THEN iv_title
                              ELSE space ).

    IF rs_prof-grid_title-gridtitle IS INITIAL.
      rs_prof-grid_title-gridtitle = rs_prof-title.
    ENDIF.

    "Display source code position only if the user has any development authorities
    DATA(lv_disp_srcpos) = iv_disp_srcpos.
    IF zcl_ca_log=>intern_has_user_developm_auth( ) EQ abap_false.
      lv_disp_srcpos = abap_false.
    ENDIF.

    "Prepare settings for output of source code position
    IF lv_disp_srcpos EQ abap_true.
      "Enhance field catalog for ALV log display
      APPEND: VALUE #( ref_table  = c_fname_s_excep_srcpos
                       ref_field  = c_fname_class
                       col_pos    = 2
                       colddictxt = abap_true
                       outputlen  = 40
                       hotspot    = abap_true
                       is_extern  = abap_true ) TO rs_prof-mess_fcat,
              VALUE #( ref_table  = c_fname_s_excep_srcpos
                       ref_field  = c_fname_method
                       col_pos    = 3
                       colddictxt = abap_true
                       outputlen  = 60
                       hotspot    = abap_true
                       is_extern  = abap_true ) TO rs_prof-mess_fcat,
              VALUE #( ref_table  = c_fname_s_excep_srcpos
                       ref_field  = c_fname_line
                       col_pos    = 4
                       colddictxt = abap_true
                       outputlen  = 8
                       hotspot    = abap_true
                       is_extern  = abap_true ) TO rs_prof-mess_fcat.

      MODIFY rs_prof-mess_fcat FROM VALUE #( outputlen = '100' )
                               TRANSPORTING outputlen
                               WHERE ref_field EQ 'T_MSG' ##no_text.


      "Define callback routine to read external data to error message
      rs_prof-clbk_read = VALUE #( userexitf = c_fm_name_cb_read
                                   userexitt = 'F' ) ##no_text.  "= Function module
      "Source pos. cols are definied as HOTSPOT. Set call back routine here.
      rs_prof-clbk_ucom = VALUE #( userexitf = c_fm_name_cb_ucom
                                   userexitt = 'F' ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "intern_get_profile


  METHOD intern_has_user_developm_auth.
    "-----------------------------------------------------------------*
    "   Has user authority to display development objects
    "-----------------------------------------------------------------*
    result = abap_true.
    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
                        ID 'DEVCLASS' DUMMY
                        ID 'OBJTYPE'  DUMMY
                        ID 'OBJNAME'  DUMMY
                        ID 'P_GROUP'  DUMMY
                        ID 'ACTVT'    FIELD '03'.   "Display
    IF sy-subrc NE 0.
      result = abap_false.
    ENDIF.
  ENDMETHOD.                    "intern_has_user_developm_auth


  METHOD intern_load.
    "-----------------------------------------------------------------*
    "   Load messages to log numbers
    "-----------------------------------------------------------------*
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header     = it_lhdr
      IMPORTING
        e_t_log_handle     = et_logh
        e_t_msg_handle     = et_msgh
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                                iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                                                iv_function = 'BAL_DB_LOAD' ##no_text
                                                                iv_subrc    = sy-subrc ) ).
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    et_srcpos = zcl_ca_log=>intern_load_pos( et_msgh ).
  ENDMETHOD.                    "intern_load


  METHOD intern_load_pos.
    "-----------------------------------------------------------------*
    "   Load messages and corresponding source position
    "-----------------------------------------------------------------*
    LOOP AT it_msgh REFERENCE INTO DATA(lr_msgh).
      DATA(lv_lognr) = zcl_ca_log=>intern_get_lognr_from_handle( lr_msgh->log_handle ).

      SELECT * APPENDING TABLE rt_srcpos
               FROM zca_log_srcpos
               WHERE lognr     EQ lv_lognr
                 AND msgnumber EQ lr_msgh->msgnumber.
    ENDLOOP.
  ENDMETHOD.                    "intern_load_pos


  METHOD intern_msg_read.
    "-----------------------------------------------------------------*
    "   Read messages from BAL
    "-----------------------------------------------------------------*
    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = is_msgh
      IMPORTING
        e_s_msg        = rs_msg
      EXCEPTIONS
        log_not_found  = 1
        msg_not_found  = 2
        OTHERS         = 3.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                                 iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                                                 iv_function = 'BAL_LOG_MSG_READ' ##no_text
                                                                 iv_subrc    = sy-subrc ) ).
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "intern_msg_read


  METHOD intern_refresh.
    "-----------------------------------------------------------------*
    "   Remove log data from BAL
    "-----------------------------------------------------------------*
    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING
        i_log_handle  = iv_loghndl
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                                iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                                                iv_function = 'BAL_LOG_REFRESH' ##no_text
                                                                iv_subrc    = sy-subrc ) ).
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "intern_refresh


  METHOD intern_save.
    "-----------------------------------------------------------------*
    "   Save log on BAL DBs
    "-----------------------------------------------------------------*
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = iv_in_upd_task
        i_t_log_handle   = it_logh
      IMPORTING
        e_new_lognumbers = rt_log_numbers
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                               iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                                               iv_function = 'BAL_DB_SAVE' ##no_text
                                                               iv_subrc    = sy-subrc ) ).
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "intern_save


  METHOD intern_save_logref.
    "-----------------------------------------------------------------*
    "   Save log references in DB table providing the log number
    "-----------------------------------------------------------------*
    rv_changed = abap_false.

    IF mt_objref     IS INITIAL OR
       ir_log_number IS INITIAL.
      RETURN.
    ENDIF.

    "Set log number into all entries
    MODIFY mt_objref FROM VALUE #( lognr      = ir_log_number->lognumber
                                   log_handle = ir_log_number->log_handle )
                     TRANSPORTING lognr  log_handle
                     WHERE lognr NE ir_log_number->lognumber.

    "Insert entries into DB table
    INSERT zca_log_objref FROM TABLE mt_objref
                          ACCEPTING DUPLICATE KEYS.
    IF lines( mt_objref ) LE sy-dbcnt AND
       sy-dbcnt           NE 0.
      "If all of the entries were already in DB SY-DBCNT is 0 and nothing was changed
      rv_changed = abap_true.
    ENDIF.
  ENDMETHOD.                    "intern_save_logref


  METHOD intern_save_srcpos.
    "-----------------------------------------------------------------*
    "   Save source position in DB table providing the log number
    "-----------------------------------------------------------------*
    rv_changed = abap_false.

    IF lines( mt_srcpos ) LE 0        OR
       ir_log_number      IS INITIAL.
      RETURN.
    ENDIF.

    "Set log number into all entries
    MODIFY mt_srcpos FROM VALUE #( lognr      = ir_log_number->lognumber
                                   log_handle = ir_log_number->log_handle )
                          TRANSPORTING lognr  log_handle
                          WHERE lognr NE ir_log_number->lognumber.

    DELETE ADJACENT DUPLICATES FROM mt_srcpos COMPARING lognr msgnumber.

    "Insert entries into DB table
    INSERT zca_log_srcpos FROM TABLE mt_srcpos
                          ACCEPTING DUPLICATE KEYS.
    IF lines( mt_srcpos ) LE sy-dbcnt AND
       sy-dbcnt           NE 0.
      "If all of the entries were already in DB SY-DBCNT is 0 and nothing was changed
      rv_changed = abap_true.
    ENDIF.
  ENDMETHOD.                    "intern_save_srcpos


  METHOD intern_search.
    "-----------------------------------------------------------------*
    "   Search logs with filter
    "-----------------------------------------------------------------*
    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter     = is_lfil
      IMPORTING
        e_t_log_header     = rt_lhdr
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                                 iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                                                 iv_function = 'BAL_DB_SEARCH' ##no_text
                                                                 iv_subrc    = sy-subrc ) ).
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "intern_search


  METHOD intern_search_n_display.
    "-----------------------------------------------------------------*
    "   Display one or more saved application logs
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_prof              TYPE bal_s_prof.

    "Search logs
    DATA(lt_lhdr) = zcl_ca_log=>intern_search( is_log_filter ).

    "Load BAL messages
    zcl_ca_log=>intern_load(
                        EXPORTING
                          it_lhdr   = lt_lhdr
                        IMPORTING
                          et_logh   = DATA(lt_logh)
                          et_msgh   = DATA(lt_msgh)
                          et_srcpos = DATA(lt_srcpos) ).

    "Provide display profile
    IF is_profile IS NOT INITIAL.
      "Use imported profile
      ls_prof = is_profile.

    ELSE.
      "Get display profile
      READ TABLE lt_lhdr INTO DATA(ls_lhdr)
                         INDEX 1.
      ls_prof = zcl_ca_log=>intern_get_profile(
                                     iv_title       = iv_title
                                     iv_popup       = iv_popup
                                     iv_use_grid    = abap_true
                                     "in case of one log - open all messages
                                     iv_show_all    = COND #( WHEN lines( lt_lhdr ) EQ 1
                                                                THEN abap_true
                                                                ELSE abap_false )
                                     iv_opt_cwidth  = iv_opt_cwidth
                                     iv_report      = ls_lhdr-alprog
                                     iv_disp_srcpos = iv_disp_srcpos ).
    ENDIF.

    "Display logs
    zcl_ca_log=>intern_display( io_parent = io_parent
                                is_prof   = ls_prof
                                it_logh   = lt_logh
                                it_msgh   = lt_msgh
                                it_srcpos = lt_srcpos ).

    "Remove log
    LOOP AT lt_logh ASSIGNING FIELD-SYMBOL(<lv_hndl>).
      zcl_ca_log=>intern_refresh( <lv_hndl> ).
    ENDLOOP.
  ENDMETHOD.                    "intern_search_n_display


  METHOD is_open.
    "-----------------------------------------------------------------*
    "   Is currently a BAL open
    "-----------------------------------------------------------------*
    IF ms_log IS INITIAL.
      "No current log
      RAISE EXCEPTION TYPE zcx_ca_log
        EXPORTING
          textid   = zcx_ca_log=>log_not_open
          mv_msgty = mo_log_options->msg_type-error.
    ENDIF.
  ENDMETHOD.                    "is_open


  METHOD message_exist.
    "-----------------------------------------------------------------*
    "   Check if message exist in current system
    "   In cases where a far system call is executed, the returned
    "   messages do not necessarily exist in the caller's system.
    "-----------------------------------------------------------------*
    DATA(ls_msg) = VALUE #( zcl_ca_log=>mt_msgs[ msgid = iv_msgid
                                                 msgno = iv_msgno ] DEFAULT VALUE #( exist = abap_false ) ).

    IF ls_msg-msgno IS INITIAL.
      "Existence check only (without data transmission)
      SELECT SINGLE 'X' INTO  @ls_msg-exist             "#EC CI_GENBUFF
                        FROM  t100
                        WHERE arbgb EQ @iv_msgid
                          AND msgnr EQ @iv_msgno.

      INSERT VALUE #( msgid = iv_msgid
                      msgno = iv_msgno
                      exist = ls_msg-exist ) INTO TABLE zcl_ca_log=>mt_msgs.
    ENDIF.

    rv_exist = ls_msg-exist.
  ENDMETHOD.                    "message_exist


  METHOD zif_ca_log~add_msg.
    "-----------------------------------------------------------------*
    "   Add single message using message variables
    "-----------------------------------------------------------------*
    add_msg_bal( is_srcpos = is_srcpos
                 is_msg    = VALUE #( msgid     = iv_msgid
                                      msgty     = iv_msgty
                                      msgno     = iv_msgno
                                      msgv1     = iv_msgv1
                                      msgv2     = iv_msgv2
                                      msgv3     = iv_msgv3
                                      msgv4     = iv_msgv4
                                      probclass = iv_probclass
                                      detlevel  = iv_detlevel ) ).
  ENDMETHOD.                    "zif_ca_log~add_msg


  METHOD zif_ca_log~add_msg_bal.
    "-----------------------------------------------------------------*
    "   Add message from structure BAL_S_MSG
    "-----------------------------------------------------------------*
    "Is log open/created?
    is_open( ).

    DATA(ls_msg) = is_msg.

    IF ls_msg-msgid IS INITIAL.
      "use default message ID
      ls_msg-msgid = mv_def_msgid.
    ENDIF.

    IF ls_msg-probclass IS INITIAL.
      "use default message problem class
      ls_msg-probclass = mv_def_probclass.
    ENDIF.

    "add message
    DATA(ls_msgh) = zcl_ca_log=>intern_add_message( iv_loghndl = mv_loghndl
                                                    is_msg     = ls_msg ).

    "set error position
    set_source_position( is_msgh   = ls_msgh
                         is_srcpos = is_srcpos ).
  ENDMETHOD.                    "zif_ca_log~add_msg_bal


  METHOD zif_ca_log~add_msg_bal_tab.
    "-----------------------------------------------------------------*
    "   Add messages from tabletype ACO_TT_BAL_MSG
    "-----------------------------------------------------------------*
    LOOP AT it_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).
      add_msg_bal( <ls_msg> ).
    ENDLOOP.
  ENDMETHOD.                    "zif_ca_log~add_msg_bal_tab


  METHOD zif_ca_log~add_msg_bapiret2.
    "-----------------------------------------------------------------*
    "   Add message from structure BAPIRET2
    "-----------------------------------------------------------------*
    IF is_bapiret2-message                 IS NOT INITIAL AND
       message_exist(
           iv_msgid = is_bapiret2-id
           iv_msgno = is_bapiret2-number ) EQ abap_false.
      add_msg( iv_msgty     = is_bapiret2-type
               iv_msgid     = 'S1'
               iv_msgno     = '897'
               iv_msgv1     = is_bapiret2-message(50)
               iv_msgv2     = is_bapiret2-message+50(50)
               iv_msgv3     = is_bapiret2-message+100(50)
               iv_msgv4     = is_bapiret2-message+150(50)
               iv_probclass = iv_probclass
               iv_detlevel  = iv_detlevel
               is_srcpos    = is_srcpos ).

    ELSE.
      add_msg( iv_msgty     = is_bapiret2-type
               iv_msgid     = is_bapiret2-id
               iv_msgno     = is_bapiret2-number
               iv_msgv1     = is_bapiret2-message_v1
               iv_msgv2     = is_bapiret2-message_v2
               iv_msgv3     = is_bapiret2-message_v3
               iv_msgv4     = is_bapiret2-message_v4
               iv_probclass = iv_probclass
               iv_detlevel  = iv_detlevel
               is_srcpos    = is_srcpos ).
    ENDIF.
  ENDMETHOD.                    "zif_ca_log~add_msg_bapiret2


  METHOD zif_ca_log~add_msg_bapiret2_tab.
    "-----------------------------------------------------------------*
    "   Add messages from table type BAPIRET2_T
    "-----------------------------------------------------------------*
    LOOP AT it_bapiret2 REFERENCE INTO DATA(lr_bapiret2).
      "add message
      add_msg_bapiret2( is_bapiret2  = lr_bapiret2->*
                        iv_probclass = iv_probclass
                        iv_detlevel  = iv_detlevel
                        is_srcpos    = is_srcpos ).
    ENDLOOP.
  ENDMETHOD.                    "zif_ca_log~add_msg_bapiret2_tab


  METHOD zif_ca_log~add_msg_exc.
    "-----------------------------------------------------------------*
    "   Add message from exception class
    "-----------------------------------------------------------------*
    DATA(lx_prev) = ix_excep.
    WHILE lx_prev IS BOUND.
      "Get message details and source code position
      DATA(ls_return) = zcx_ca_error=>get_msg_details_from_excep( lx_prev ).
      "Set source code position
      DATA(ls_srcpos) = zcx_ca_error=>get_exception_position( lx_prev ).

      "Use message type if provided
      CASE TYPE OF lx_prev.
        WHEN TYPE zcx_ca_error INTO DATA(lx_catched).
          ls_return-type = lx_catched->mv_msgty.
        WHEN TYPE zcx_ca_intern INTO DATA(lx_intern).
          ls_return-type = lx_intern->mv_msgty.
      ENDCASE.

      "Set problem class
      IF iv_probclass IS NOT INITIAL.
        DATA(lv_probclass) = iv_probclass.

      ELSE.
        IF ls_return-type IS INITIAL.
          lv_probclass = mv_def_probclass.

        ELSE.
          CASE ls_return-type.
            WHEN mo_log_options->msg_type-success.
              lv_probclass = mo_log_options->problem_class-info.

            WHEN mo_log_options->msg_type-info     OR
                 mo_log_options->msg_type-warning.
              lv_probclass = mo_log_options->problem_class-default.

            WHEN mo_log_options->msg_type-error.
              lv_probclass = mo_log_options->problem_class-important.

            WHEN mo_log_options->msg_type-abort OR
                 mo_log_options->msg_type-exit.
              lv_probclass = mo_log_options->problem_class-very_important.
          ENDCASE.
        ENDIF.
      ENDIF.

      "Set missing message type depending on problem class
      IF ls_return-type IS INITIAL.
        CASE lv_probclass.
          WHEN mo_log_options->problem_class-info       OR
               mo_log_options->problem_class-undefined.
            ls_return-type = mo_log_options->msg_type-success.

          WHEN mo_log_options->problem_class-default.
            ls_return-type = mo_log_options->msg_type-info.

          WHEN mo_log_options->problem_class-important.
            ls_return-type = mo_log_options->msg_type-error.

          WHEN mo_log_options->problem_class-very_important.
            ls_return-type = mo_log_options->msg_type-abort.
        ENDCASE.
      ENDIF.

      "add message to log
      add_msg( iv_msgid     = ls_return-id
               iv_msgty     = ls_return-type
               iv_msgno     = ls_return-number
               iv_msgv1     = ls_return-message_v1
               iv_msgv2     = ls_return-message_v2
               iv_msgv3     = ls_return-message_v3
               iv_msgv4     = ls_return-message_v4
               iv_probclass = lv_probclass
               iv_detlevel  = iv_detlevel
               is_srcpos    = ls_srcpos ).

      IF iv_all EQ abap_false.
        EXIT.
      ENDIF.

      lx_prev = lx_prev->previous.
    ENDWHILE.
  ENDMETHOD.                    "zif_ca_log~add_msg_exc


  METHOD zif_ca_log~add_msg_syst.
    "-----------------------------------------------------------------*
    "   Add message from structure SYST
    "-----------------------------------------------------------------*
    add_msg( iv_msgid     = sy-msgid
             iv_msgty     = sy-msgty
             iv_msgno     = sy-msgno
             iv_msgv1     = sy-msgv1
             iv_msgv2     = sy-msgv2
             iv_msgv3     = sy-msgv3
             iv_msgv4     = sy-msgv4
             iv_probclass = iv_probclass
             iv_detlevel  = iv_detlevel
             is_srcpos    = is_srcpos  ).
  ENDMETHOD.                    "zif_ca_log~add_msg_sy


  METHOD zif_ca_log~close.
    "-----------------------------------------------------------------*
    "   Close log
    "-----------------------------------------------------------------*
    "log already closed?
    IF ms_log IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR ms_log.

    "refresh memory
    zcl_ca_log=>intern_refresh( mv_loghndl ).
  ENDMETHOD.                    "zif_ca_log~close


  METHOD zif_ca_log~display.
    "-----------------------------------------------------------------*
    "   Display current messages
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_prof              TYPE bal_s_prof.

    IF is_profile IS NOT INITIAL.
      "use imported profile
      ls_prof = is_profile.

    ELSE.
      "get display profile
      ls_prof = get_profile( iv_title       = iv_title
                             iv_popup       = iv_popup
                             iv_use_grid    = iv_use_grid
                             iv_disp_srcpos = iv_disp_srcpos
                             iv_opt_cwidth  = iv_opt_cwidth
                             iv_show_all    = iv_show_all ).
    ENDIF.

    "display logs
    zcl_ca_log=>intern_display( io_parent = io_parent
                                is_prof   = ls_prof
                                it_logh   = VALUE #( ( mv_loghndl ) )
                                it_srcpos = mt_srcpos ).
  ENDMETHOD.                    "zif_ca_log~display


  METHOD zif_ca_log~get_ext_number.
    "-----------------------------------------------------------------*
    "   Get display profile
    "-----------------------------------------------------------------*
    rv_extnumber = ms_log-extnumber.
  ENDMETHOD.                    "get_ext_number


  METHOD zif_ca_log~get_msg_count.
    "-----------------------------------------------------------------*
    "   Get number of all BAL messages depending on message type
    "-----------------------------------------------------------------*
    TRY.
        rv_result = lines( get_msg_list_bapiret2( iv_msgty ) ).

      CATCH zcx_ca_log.
        rv_result = 0.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_log~get_msg_count


  METHOD zif_ca_log~get_msg_list_bal.
    "-----------------------------------------------------------------*
    "   Get all messages from BAL (table type ACO_TT_BAL_MSG)
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_msgh TYPE bal_t_msgh.

    DATA(lo_sel_options) = zcl_ca_c_sel_options=>get_instance( ).
    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = VALUE bal_t_logh( ( mv_loghndl ) )
        i_s_msg_filter = VALUE bal_s_mfil(
                                  msgty = COND #(
                                            WHEN iv_msgty IS INITIAL
                                              THEN VALUE #( )  "initial filter
                                              ELSE VALUE #( ( sign   = lo_sel_options->sign-incl
                                                              option = lo_sel_options->option-eq
                                                              low    = iv_msgty ) ) ) )
      IMPORTING
        e_t_msg_handle = lt_msgh
      EXCEPTIONS
        msg_not_found  = 1
        OTHERS         = 2.
    IF sy-subrc NE 0.
      DATA(lx_error) =
             CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                        iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                        iv_function  = 'BAL_GLB_SEARCH_MSG'
                                        iv_subrc     = sy-subrc ) ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    LOOP AT lt_msgh ASSIGNING FIELD-SYMBOL(<lv_msgh>).
      APPEND zcl_ca_log=>intern_msg_read( <lv_msgh> ) TO rt_data.
    ENDLOOP.
  ENDMETHOD.                    "zif_ca_log~get_msg_list_bal


  METHOD zif_ca_log~get_msg_list_bapiret2.
    "-----------------------------------------------------------------*
    "   Get messages from BAL and convert into BAPIRET2 format
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_bapiret2 TYPE bapiret2.

    DATA(lt_msg) = get_msg_list_bal( iv_msgty ).

    LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = <ls_msg>-msgty
          cl     = <ls_msg>-msgid
          number = <ls_msg>-msgno
          par1   = <ls_msg>-msgv1
          par2   = <ls_msg>-msgv2
          par3   = <ls_msg>-msgv3
          par4   = <ls_msg>-msgv4
        IMPORTING
          return = ls_bapiret2.

      APPEND ls_bapiret2 TO rt_data.
    ENDLOOP.
  ENDMETHOD.                    "zif_ca_log~get_msg_list_bapiret2


  METHOD zif_ca_log~get_profile.
    "-----------------------------------------------------------------*
    "   Get display profile
    "-----------------------------------------------------------------*
    result = zcl_ca_log=>intern_get_profile( iv_title       = iv_title
                                             iv_popup       = iv_popup
                                             iv_use_grid    = iv_use_grid
                                             iv_report      = iv_report
                                             iv_disp_srcpos = iv_disp_srcpos
                                             iv_show_all    = iv_show_all
                                             iv_opt_cwidth  = iv_opt_cwidth ).
  ENDMETHOD.                    "zif_ca_log~get_display_profile


  METHOD zif_ca_log~release.
    "-----------------------------------------------------------------*
    "   Delete instance from buffer
    "-----------------------------------------------------------------*
    IF iv_save EQ abap_true.
      save( iv_in_upd_task = iv_in_upd_task
            iv_close       = abap_false ).
    ENDIF.

    close( ).

    DELETE TABLE zcl_ca_log=>mt_buffer WITH TABLE KEY ky_log_instance
                                           COMPONENTS o_log = me.
  ENDMETHOD.                    "zif_ca_log~release


  METHOD zif_ca_log~save.
    "-----------------------------------------------------------------*
    "   Save messages into BAL DBs and corresponding informations
    "-----------------------------------------------------------------*
    "Is log open/created?
    is_open( ).

    IF iv_no_empty      EQ abap_false OR   "quantity of msg entries doesn't matter
       get_msg_count( ) GT 0.              "OR minimum 1 message exists
      "save log
      DATA(lt_log_numbers) = zcl_ca_log=>intern_save( it_logh        = VALUE #( ( mv_loghndl ) )
                                                      iv_in_upd_task = iv_in_upd_task ).

      DATA(lr_log_number) = REF #( lt_log_numbers[ log_handle = mv_loghndl ] OPTIONAL ). "#EC CI_SORTSEQ

      IF lr_log_number IS BOUND.
        rv_lognumber = lr_log_number->lognumber.

        IF lr_log_number->log_handle IS INITIAL.
          lr_log_number->log_handle = mv_loghndl.
        ENDIF.

        intern_save_logref( lr_log_number ). "save object references
        intern_save_srcpos( lr_log_number ). "save error positions

        IF cl_system_transaction_state=>get_in_update_task( ) EQ oscon_version_inactive AND
           iv_commit EQ abap_true.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDIF.

    IF iv_close EQ abap_true.
      close( ).
    ENDIF.
  ENDMETHOD.                    "zif_ca_log~save


  METHOD zif_ca_log~set_additional_ref_object_id.
    "-----------------------------------------------------------------*
    "   Set source position into buffer
    "-----------------------------------------------------------------*
    IF is_lpor IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_obj_ref)   = CORRESPONDING zca_log_objref( is_lpor ).
    ls_obj_ref-add_key = iv_add_key.

    IF NOT line_exists( mt_objref[ instid  = ls_obj_ref-instid
                                   typeid  = ls_obj_ref-typeid
                                   catid   = ls_obj_ref-catid
                                   add_key = ls_obj_ref-add_key ] ).
      APPEND ls_obj_ref TO mt_objref.
    ENDIF.
  ENDMETHOD.                    "zif_ca_log~set_additional_ref_object_id


  METHOD zif_ca_log~set_ext_number.
    "-----------------------------------------------------------------*
    "   Set new external number
    "-----------------------------------------------------------------*
    ms_log-extnumber = iv_extnumber.
    intern_change( is_log     = ms_log
                   iv_loghndl = mv_loghndl ).
  ENDMETHOD.                    "zif_ca_log~set_ext_number


  METHOD zif_ca_log~set_program_name.
    "-----------------------------------------------------------------*
    "   Set new external number
    "-----------------------------------------------------------------*
    ms_log-alprog = iv_program_name.
    intern_change( is_log     = ms_log
                   iv_loghndl = mv_loghndl ).
  ENDMETHOD.                    "zif_ca_log~set_program_name


  METHOD zif_ca_log~set_source_position.
    "-----------------------------------------------------------------*
    "   Set source position into buffer
    "-----------------------------------------------------------------*
    "only if information exists
    IF is_srcpos IS INITIAL.
      RETURN.
    ENDIF.

    "add to global attribute
    APPEND VALUE #( msgnumber    = is_msgh-msgnumber
                    s_exc_srcpos = is_srcpos ) TO mt_srcpos.
  ENDMETHOD.                    "zif_ca_log~set_source_position


  METHOD zif_ca_log~set_transaction_code.
    "-----------------------------------------------------------------*
    "   Set new external number
    "-----------------------------------------------------------------*
    ms_log-altcode = iv_transaction_code.
    intern_change( is_log     = ms_log
                   iv_loghndl = mv_loghndl ).
  ENDMETHOD.                    "zif_ca_log~set_program_name


  METHOD zif_ca_log~write.
    "-----------------------------------------------------------------*
    "   Print messages as list without
    "-----------------------------------------------------------------*
    mo_log_options->is_message_type_valid( iv_msgty ).

    LOOP AT get_msg_list_bapiret2( iv_msgty ) REFERENCE INTO DATA(lr_msg).
      WRITE / lr_msg->message.
    ENDLOOP.
  ENDMETHOD.                    "zif_ca_log~write


  method ZIF_CA_LOG~ADD_MSG_FREE_TEXT.

    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle     = mv_loghndl
        i_msgty          = msgty
        i_text           = text
*      IMPORTING
*        e_s_msg_handle   = rs_msgh
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                                iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                                                iv_function = 'BAL_LOG_MSG_ADD_FREE_TEXT' ##no_text
                                                                iv_subrc    = sy-subrc ) ).
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

  endmethod.
ENDCLASS.
