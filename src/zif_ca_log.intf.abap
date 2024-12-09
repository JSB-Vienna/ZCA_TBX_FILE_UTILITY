"! <p class="shorttext synchronized" lang="en">CA-TBX: Business Application Logging (BAL)</p>
interface ZIF_CA_LOG
  public .


* i n s t a n c e   a t t r i b u t e s
*   o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">Constants and value checks for application log</p>
  data MO_LOG_OPTIONS type ref to ZCL_CA_C_LOG read-only .

*   s t a t i c   m e t h o d s
    "! <p class="shorttext synchronized" lang="en">Get prepared display profile</p>
    "!
    "! @parameter iv_title       | <p class="shorttext synchronized" lang="en">Application Log: Dynpro Title</p>
    "! @parameter iv_popup       | <p class="shorttext synchronized" lang="en">X = Display as popup</p>
    "! @parameter iv_use_grid    | <p class="shorttext synchronized" lang="en">X = Display as grid</p>
    "! @parameter iv_report      | <p class="shorttext synchronized" lang="en">Program for saving layout variant</p>
    "! @parameter iv_show_all    | <p class="shorttext synchronized" lang="en">X = Show all messages</p>
    "! @parameter iv_opt_cwidth  | <p class="shorttext synchronized" lang="en">X = Optimize column width</p>
    "! @parameter iv_disp_srcpos | <p class="shorttext synchronized" lang="en">X = Display source code position of exception</p>
    "! @parameter result         | <p class="shorttext synchronized" lang="en">Application Log: Log Output Format Profile</p>
  class-methods GET_PROFILE
    importing
      !IV_TITLE type BALTITLE optional
      !IV_POPUP type ABAP_BOOL default ABAP_FALSE
      !IV_USE_GRID type ABAP_BOOL default ABAP_TRUE
      !IV_REPORT type SYREPID default SY-CPROG
      !IV_SHOW_ALL type ABAP_BOOL default ABAP_FALSE
      !IV_OPT_CWIDTH type ABAP_BOOL default ABAP_TRUE
      !IV_DISP_SRCPOS type ABAP_BOOL default ABAP_FALSE
    returning
      value(RESULT) type BAL_S_PROF .
* i n s t a n c e   m e t h o d s
    "! <p class="shorttext synchronized" lang="en">Add single message using message variables</p>
    "!
    "! @parameter iv_msgty     | <p class="shorttext synchronized" lang="en">ABAP System Field: Message Type</p>
    "! @parameter iv_msgid     | <p class="shorttext synchronized" lang="en">ABAP System Field: Message ID</p>
    "! @parameter iv_msgno     | <p class="shorttext synchronized" lang="en">ABAP System Field: Message Number</p>
    "! @parameter iv_msgv1     | <p class="shorttext synchronized" lang="en">Message variable 1</p>
    "! @parameter iv_msgv2     | <p class="shorttext synchronized" lang="en">Message variable 2</p>
    "! @parameter iv_msgv3     | <p class="shorttext synchronized" lang="en">Message variable 3</p>
    "! @parameter iv_msgv4     | <p class="shorttext synchronized" lang="en">Message variable 4</p>
    "! @parameter iv_probclass | <p class="shorttext synchronized" lang="en">Application log: Message problem class</p>
    "! @parameter iv_detlevel  | <p class="shorttext synchronized" lang="en">Application Log: Level of detail</p>
    "! @parameter is_srcpos    | <p class="shorttext synchronized" lang="en">CA-TBX: Exception position in source code</p>
  methods ADD_MSG
    importing
      !IV_MSGTY type SYST_MSGTY default ZCL_CA_C_LOG=>MSG_TYPE-ERROR
      !IV_MSGID type SYST_MSGID optional
      !IV_MSGNO type SYST_MSGNO default '000'
      !IV_MSGV1 type CSEQUENCE optional
      !IV_MSGV2 type CSEQUENCE optional
      !IV_MSGV3 type CSEQUENCE optional
      !IV_MSGV4 type CSEQUENCE optional
      !IV_PROBCLASS type BALPROBCL optional
      !IV_DETLEVEL type BALLEVEL optional
      !IS_SRCPOS type ZCA_S_EXCEP_SRCPOS optional .
    "! <p class="shorttext synchronized" lang="en">Add message from structure BAL_S_MSG</p>
    "!
    "! @parameter is_msg    | <p class="shorttext synchronized" lang="en">Application Log: Message Data</p>
    "! @parameter is_srcpos | <p class="shorttext synchronized" lang="en">CA-TBX: Exception position in source code</p>
  methods ADD_MSG_BAL
    importing
      !IS_MSG type BAL_S_MSG
      !IS_SRCPOS type ZCA_S_EXCEP_SRCPOS optional .
    "! <p class="shorttext synchronized" lang="en">Add messages from tabletype ACO_TT_BAL_MSG</p>
    "!
    "! @parameter it_msg | <p class="shorttext synchronized" lang="en">Message Log</p>
  methods ADD_MSG_BAL_TAB
    importing
      !IT_MSG type ACO_TT_BAL_MSG .
    "! <p class="shorttext synchronized" lang="en">Add message from structure BAPIRET2</p>
    "!
    "! @parameter is_bapiret2  | <p class="shorttext synchronized" lang="en">Return Parameter</p>
    "! @parameter iv_probclass | <p class="shorttext synchronized" lang="en">Application log: Message problem class</p>
    "! @parameter iv_detlevel  | <p class="shorttext synchronized" lang="en">Application Log: Level of detail</p>
    "! @parameter is_srcpos    | <p class="shorttext synchronized" lang="en">CA-TBX: Exception position in source code</p>
  methods ADD_MSG_BAPIRET2
    importing
      !IS_BAPIRET2 type BAPIRET2
      !IV_PROBCLASS type BALPROBCL optional
      !IV_DETLEVEL type BALLEVEL optional
      !IS_SRCPOS type ZCA_S_EXCEP_SRCPOS optional .
    "! <p class="shorttext synchronized" lang="en">Add messages from tabletype BAPIRET2_T</p>
    "!
    "! @parameter it_bapiret2  | <p class="shorttext synchronized" lang="en">Return table</p>
    "! @parameter iv_probclass | <p class="shorttext synchronized" lang="en">Application log: Message problem class</p>
    "! @parameter iv_detlevel  | <p class="shorttext synchronized" lang="en">Application Log: Level of detail</p>
    "! @parameter is_srcpos    | <p class="shorttext synchronized" lang="en">CA-TBX: Exception position in source code</p>
  methods ADD_MSG_BAPIRET2_TAB
    importing
      !IT_BAPIRET2 type BAPIRET2_T
      !IV_PROBCLASS type BALPROBCL optional
      !IV_DETLEVEL type BALLEVEL optional
      !IS_SRCPOS type ZCA_S_EXCEP_SRCPOS optional .
    "! <p class="shorttext synchronized" lang="en">Add message from exception class</p>
    "!
    "! @parameter ix_excep     | <p class="shorttext synchronized" lang="en">Catched exception</p>
    "! @parameter iv_all       | <p class="shorttext synchronized" lang="en">X = Append all previous messages</p>
    "! @parameter iv_probclass | <p class="shorttext synchronized" lang="en">Application log: Message problem class</p>
    "! @parameter iv_detlevel  | <p class="shorttext synchronized" lang="en">Application Log: Level of detail</p>
  methods ADD_MSG_EXC
    importing
      !IX_EXCEP type ref to CX_ROOT
      !IV_ALL type ABAP_BOOL default ABAP_FALSE
      !IV_PROBCLASS type BALPROBCL optional
      !IV_DETLEVEL type BALLEVEL optional .
    "! <p class="shorttext synchronized" lang="en">Add message from structure SYST</p>
    "!
    "! @parameter iv_probclass | <p class="shorttext synchronized" lang="en">Application log: Message problem class</p>
    "! @parameter iv_detlevel  | <p class="shorttext synchronized" lang="en">Application Log: Level of detail</p>
    "! @parameter is_srcpos    | <p class="shorttext synchronized" lang="en">CA-TBX: Exception position in source code</p>
  methods ADD_MSG_SYST
    importing
      !IV_PROBCLASS type BALPROBCL optional
      !IV_DETLEVEL type BALLEVEL optional
      !IS_SRCPOS type ZCA_S_EXCEP_SRCPOS optional .
  methods ADD_MSG_FREE_TEXT
    importing
      !TEXT type STRING
      !MSGTY type SYMSGTY default 'I' .
    "! <p class="shorttext synchronized" lang="en">Close log</p>
  methods CLOSE .
    "! <p class="shorttext synchronized" lang="en">Display log</p>
    "!
    "! @parameter io_parent      | <p class="shorttext synchronized" lang="en">Parent container to display log in-place</p>
    "! @parameter iv_title       | <p class="shorttext synchronized" lang="en">Application Log: Dynpro Title</p>
    "! @parameter iv_use_grid    | <p class="shorttext synchronized" lang="en">X = Display as grid</p>
    "! @parameter iv_popup       | <p class="shorttext synchronized" lang="en">X = Display as popup</p>
    "! @parameter iv_disp_srcpos | <p class="shorttext synchronized" lang="en">X = Display source code position of exception</p>
    "! @parameter iv_opt_cwidth  | <p class="shorttext synchronized" lang="en">X = Optimize column width</p>
    "! @parameter iv_show_all    | <p class="shorttext synchronized" lang="en">X = Show all messages</p>
    "! @parameter is_profile     | <p class="shorttext synchronized" lang="en">Application Log: Log Output Format Profile</p>
  methods DISPLAY
    importing
      !IO_PARENT type ref to CL_GUI_CONTAINER optional
      !IV_TITLE type BALTITLE optional
      !IV_USE_GRID type ABAP_BOOL default ABAP_TRUE
      !IV_POPUP type ABAP_BOOL default ABAP_FALSE
      !IV_DISP_SRCPOS type ABAP_BOOL default ABAP_FALSE
      !IV_OPT_CWIDTH type ABAP_BOOL default ABAP_TRUE
      !IV_SHOW_ALL type ABAP_BOOL default ABAP_FALSE
      !IS_PROFILE type BAL_S_PROF optional .
    "! <p class="shorttext synchronized" lang="en">Get current external number</p>
    "!
    "! @parameter rv_extnumber | <p class="shorttext synchronized" lang="en">Application Log: External ID</p>
  methods GET_EXT_NUMBER
    returning
      value(RV_EXTNUMBER) type BALNREXT .
    "! <p class="shorttext synchronized" lang="en">Get number of all BAL messages depending on message type</p>
    "!
    "! @parameter iv_msgty  | <p class="shorttext synchronized" lang="en">ABAP System Field: Message Type</p>
    "! @parameter rv_result | <p class="shorttext synchronized" lang="en">Number of message found</p>
  methods GET_MSG_COUNT
    importing
      !IV_MSGTY type SYST_MSGTY optional
    returning
      value(RV_RESULT) type I .
    "! <p class="shorttext synchronized" lang="en">Get all messages from BAL (table type ACO_TT_BAL_MSG)</p>
    "!
    "! @parameter iv_msgty | <p class="shorttext synchronized" lang="en">ABAP System Field: Message Type</p>
    "! @parameter rt_data  | <p class="shorttext synchronized" lang="en">Message Log</p>
  methods GET_MSG_LIST_BAL
    importing
      !IV_MSGTY type SYMSGTY optional
    returning
      value(RT_DATA) type ACO_TT_BAL_MSG .
    "! <p class="shorttext synchronized" lang="en">Get messages from BAL and convert into BAPIRET2 format</p>
    "!
    "! @parameter iv_msgty | <p class="shorttext synchronized" lang="en">ABAP System Field: Message Type</p>
    "! @parameter rt_data  | <p class="shorttext synchronized" lang="en">Return table</p>
  methods GET_MSG_LIST_BAPIRET2
    importing
      !IV_MSGTY type SYMSGTY optional
    returning
      value(RT_DATA) type BAPIRET2_T .
    "! <p class="shorttext synchronized" lang="en">Delete instance from buffer (closes log)</p>
    "!
    "! @parameter iv_save        | <p class="shorttext synchronized" lang="en">X = Save log</p>
    "! @parameter iv_in_upd_task | <p class="shorttext synchronized" lang="en">X = Save log in update task</p>
  methods RELEASE
    importing
      !IV_SAVE type ABAP_BOOL default ABAP_TRUE
      !IV_IN_UPD_TASK type ABAP_BOOL default ABAP_FALSE .
    "! <p class="shorttext synchronized" lang="en">Save all messages (optional with CLOSE)</p>
    "!
    "! @parameter iv_close       | <p class="shorttext synchronized" lang="en">X = Close log</p>
    "! @parameter iv_commit      | <p class="shorttext synchronized" lang="en">X = Execute commit work</p>
    "! @parameter iv_in_upd_task | <p class="shorttext synchronized" lang="en">X = Save log in update task</p>
    "! @parameter iv_no_empty    | <p class="shorttext synchronized" lang="en">X = Do not save empty logs</p>
    "! @parameter rv_lognumber   | <p class="shorttext synchronized" lang="en">Application log: log number</p>
  methods SAVE
    importing
      !IV_CLOSE type ABAP_BOOL default ABAP_TRUE
      !IV_COMMIT type ABAP_BOOL default ABAP_TRUE
      !IV_IN_UPD_TASK type ABAP_BOOL default ABAP_FALSE
      !IV_NO_EMPTY type ABAP_BOOL default ABAP_TRUE
    preferred parameter IV_CLOSE
    returning
      value(RV_LOGNUMBER) type BALOGNR .
    "! <p class="shorttext synchronized" lang="en">Set additional reference object id</p>
    "!
    "! <p>Provide another business object key (= LS_LPOR) to find log entries to different objects. E. g. the
    "! leading object is the FI document, but it is required to find log entries also to the corresponding
    "! logistic invoice document. By default the business object key for the instantiation is always set.</p>
    "! <p>Parameter IV_ADD_KEY is to enhance the business object key to find entries e. g. for a specific
    "! action or workitem or ... .</p>
    "!
    "! @parameter is_lpor    | <p class="shorttext synchronized" lang="en">Additional object key</p>
    "! @parameter iv_add_key | <p class="shorttext synchronized" lang="en">Additional key of e. g. a subobject</p>
  methods SET_ADDITIONAL_REF_OBJECT_ID
    importing
      !IS_LPOR type SIBFLPORB
      !IV_ADD_KEY type ZCA_D_LOG_ADD_KEY optional .
    "! <p class="shorttext synchronized" lang="en">Set new external number (late, before saving/displaying log)</p>
    "!
    "! @parameter iv_extnumber | <p class="shorttext synchronized" lang="en">External ID (or something else)</p>
  methods SET_EXT_NUMBER
    importing
      !IV_EXTNUMBER type BALNREXT .
    "! <p class="shorttext synchronized" lang="en">Set program name (late, before saving/displaying log)</p>
    "!
    "! @parameter iv_program_name | <p class="shorttext synchronized" lang="en">Program name (or something else)</p>
  methods SET_PROGRAM_NAME
    importing
      !IV_PROGRAM_NAME type BALPROG .
    "! <p class="shorttext synchronized" lang="en">Set source code position to message</p>
    "!
    "! @parameter is_msgh   | <p class="shorttext synchronized" lang="en">Application Log: Message handle</p>
    "! @parameter is_srcpos | <p class="shorttext synchronized" lang="en">CA-TBX: Exception position in source code</p>
  methods SET_SOURCE_POSITION
    importing
      !IS_MSGH type BALMSGHNDL
      !IS_SRCPOS type ZCA_S_EXCEP_SRCPOS .
    "! <p class="shorttext synchronized" lang="en">Set transaction code (late, before saving/displaying log)</p>
    "!
    "! @parameter iv_transaction_code | <p class="shorttext synchronized" lang="en">Transaction code (or something else)</p>
  methods SET_TRANSACTION_CODE
    importing
      !IV_TRANSACTION_CODE type BALTCODE .
    "! <p class="shorttext synchronized" lang="en">Write all messages (use of WRITE command)</p>
    "!
    "! @parameter iv_msgty | <p class="shorttext synchronized" lang="en">ABAP System Field: Message Type</p>
  methods WRITE
    importing
      !IV_MSGTY type SYST_MSGTY optional .
endinterface.
