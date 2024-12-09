"! <p class="shorttext synchronized" lang="en">CA-TBX: Technical constants for application log</p>
INTERFACE zif_ca_c_log_techn PUBLIC.
*   c o n s t a n t s
  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Application Log: Object ZCA</p>
    c_object_zca           TYPE balobj_d          VALUE 'ZCA' ##no_text,
    "! <p class="shorttext synchronized" lang="en">Field name: Structure with exception position</p>
    c_fname_s_excep_srcpos TYPE fieldname         VALUE 'ZCA_S_EXCEP_SRCPOS'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Field name: Column for class or program name</p>
    c_fname_class          TYPE fieldname         VALUE 'CLASS'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Field name: Column for method or subroutine name</p>
    c_fname_method         TYPE fieldname         VALUE 'METH'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Field name: Column for source code line</p>
    c_fname_line           TYPE fieldname         VALUE 'LINE'  ##no_text.
ENDINTERFACE.
