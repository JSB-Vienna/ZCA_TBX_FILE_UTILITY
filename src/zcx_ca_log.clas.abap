"! <p class="shorttext synchronized" lang="en">CA-TBX exception: Application log action failed</p>
class ZCX_CA_LOG definition
  public
  inheriting from ZCX_CA_INTERN
  create public .

public section.

  constants:
    begin of ZCX_CA_LOG,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '018',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CA_LOG .
  constants:
    begin of INSERT_FAILED,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value 'MV_MSGV3',
      attr4 type scx_attrname value '',
    end of INSERT_FAILED .
  constants:
    begin of LOG_NOT_OPEN,
      msgid type symsgid value 'BL',
      msgno type symsgno value '212',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of LOG_NOT_OPEN .
  constants:
    begin of NO_ENTRY,
      msgid type symsgid value 'SD',
      msgno type symsgno value '850',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_ENTRY .
  constants:
    begin of DEL_DATE_INVALID,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '020',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DEL_DATE_INVALID .
  constants:
    begin of INSUFFICIENT_DATA_FOR_SRC_POS,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '021',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INSUFFICIENT_DATA_FOR_SRC_POS .
  "! <p class="shorttext synchronized" lang="en">My own name</p>
  constants C_ZCX_CA_LOG type SEOCLSNAME value 'ZCX_CA_LOG' ##NO_TEXT.

  "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MT_RETURN type BAPIRET2_T optional
      !MV_SUBRC type SYST_SUBRC optional
      !MV_MSGTY type SYMSGTY optional
      !MV_MSGV1 type SYMSGV optional
      !MV_MSGV2 type SYMSGV optional
      !MV_MSGV3 type SYMSGV optional
      !MV_MSGV4 type SYMSGV optional
      !MV_SEVERITY type T_SEVERITY optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CA_LOG IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MT_RETURN = MT_RETURN
MV_SUBRC = MV_SUBRC
MV_MSGTY = MV_MSGTY
MV_MSGV1 = MV_MSGV1
MV_MSGV2 = MV_MSGV2
MV_MSGV3 = MV_MSGV3
MV_MSGV4 = MV_MSGV4
MV_SEVERITY = MV_SEVERITY
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_CA_LOG .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
