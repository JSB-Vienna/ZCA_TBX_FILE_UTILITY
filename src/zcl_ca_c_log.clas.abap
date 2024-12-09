"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for application logging</p>
CLASS zcl_ca_c_log DEFINITION PUBLIC
                              CREATE PROTECTED.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Object category</p>
      BEGIN OF object_category,
        "! <p class="shorttext synchronized" lang="en">Object category: Class</p>
        class        TYPE sibfcatid         VALUE swfco_objtype_cl,
        "! <p class="shorttext synchronized" lang="en">Object category: Business object (SWO1/old)</p>
        business_obj TYPE sibfcatid         VALUE swfco_objtype_bor,
      END OF object_category,

      "! <p class="shorttext synchronized" lang="en">Message types</p>
      BEGIN OF msg_type,
        "! <p class="shorttext synchronized" lang="en">Message type: Abort</p>
        abort   TYPE bapi_mtype VALUE if_xo_const_message=>abort,
        "! <p class="shorttext synchronized" lang="en">Message type: Error</p>
        error   TYPE bapi_mtype VALUE if_xo_const_message=>error,
        "! <p class="shorttext synchronized" lang="en">Message type: Information</p>
        info    TYPE bapi_mtype VALUE if_xo_const_message=>info,
        "! <p class="shorttext synchronized" lang="en">Message type: Success</p>
        success TYPE bapi_mtype VALUE if_xo_const_message=>success,
        "! <p class="shorttext synchronized" lang="en">Message type: Warning</p>
        warning TYPE bapi_mtype VALUE if_xo_const_message=>warning,
        "! <p class="shorttext synchronized" lang="en">Message type: Exit</p>
        exit    TYPE bapi_mtype VALUE if_xo_const_message=>exit,
      END OF msg_type,

      "! <p class="shorttext synchronized" lang="en">Problem class</p>
      BEGIN OF problem_class,
        "! <p class="shorttext synchronized" lang="en">Problem class: Undefined</p>
        undefined      TYPE balprobcl         VALUE space,
        "! <p class="shorttext synchronized" lang="en">Problem class: Very important</p>
        very_important TYPE balprobcl         VALUE '1',
        "! <p class="shorttext synchronized" lang="en">Problem class: Important</p>
        important      TYPE balprobcl         VALUE '2',
        "! <p class="shorttext synchronized" lang="en">Problem class: Default</p>
        default        TYPE balprobcl         VALUE '3',
        "! <p class="shorttext synchronized" lang="en">Problem class: Information</p>
        info           TYPE balprobcl         VALUE '4',
      END OF problem_class,

      "! <p class="shorttext synchronized" lang="en">Operating mode</p>
      BEGIN OF operating_mode,
        "! <p class="shorttext synchronized" lang="en">Operating mode: Batch processing</p>
        batch       TYPE balmode           VALUE 'B',
        "! <p class="shorttext synchronized" lang="en">Operating mode: Dialog processing</p>
        dialog      TYPE balmode           VALUE 'D',
        "! <p class="shorttext synchronized" lang="en">Operating mode: Batch input processing</p>
        batch_input TYPE balmode           VALUE 'I',
      END OF operating_mode.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_log.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Valid object category passed?</p>
      "!
      "! @parameter object_category | <p class="shorttext synchronized" lang="en">Object category</p>
      is_object_category_valid FINAL
        IMPORTING
          object_category TYPE sibfcatid,

      "! <p class="shorttext synchronized" lang="en">Valid message type passed?</p>
      "!
      "! @parameter message_type | <p class="shorttext synchronized" lang="en">Message type</p>
      is_message_type_valid FINAL
        IMPORTING
          message_type TYPE syst_msgty,

      "! <p class="shorttext synchronized" lang="en">Valid problem class passed?</p>
      "!
      "! @parameter problem_class | <p class="shorttext synchronized" lang="en">Problem class</p>
      is_problem_class_valid FINAL
        IMPORTING
          problem_class TYPE balprobcl,

      "! <p class="shorttext synchronized" lang="en">Valid operating mode passed?</p>
      "!
      "! @parameter operating_mode | <p class="shorttext synchronized" lang="en">Operating mode</p>
      is_operating_mode_valid FINAL
        IMPORTING
          operating_mode TYPE balmode.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check value against fixed_values</p>
      "!
      "! @parameter value      | <p class="shorttext synchronized" lang="en">Value under test</p>
      "! @parameter param_name | <p class="shorttext synchronized" lang="en">Name of field/parameter for output in error message</p>
      check_against_fixed_values
        IMPORTING
          value      TYPE simple
          param_name TYPE csequence.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_log.

ENDCLASS.



CLASS zcl_ca_c_log IMPLEMENTATION.

  METHOD check_against_fixed_values.
    "-----------------------------------------------------------------*
    "   Check value against fixed_values
    "-----------------------------------------------------------------*
    TRY.
        NEW zcl_ca_ddic( iv_data       = value
                         iv_param_name = param_name )->check_fixed_values( iv_value       = value
                                                                           iv_raise_excep = abap_true ).

      CATCH zcx_ca_param INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_log( zcx_ca_intern=>create_exception(
                                                               iv_excp_cls = zcx_ca_log=>c_zcx_ca_log
                                                               iv_class    = 'ZCL_CA_DDIC'
                                                               iv_method   = 'CHECK_FIXED_VALUE'
                                                               ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "check_against_fixed_values


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_log=>singleton_instance IS NOT BOUND.
      zcl_ca_c_log=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_log=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD is_object_category_valid.
    "-----------------------------------------------------------------*
    "   Valid object category passed?
    "-----------------------------------------------------------------*
    IF object_category NE me->object_category-business_obj AND
       object_category NE me->object_category-class.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_log
        EXPORTING
          textid   = zcx_ca_log=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'OBJECT_CATEGORY'
          mv_msgv2 = CONV #( object_category ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_object_category_valid


  METHOD is_message_type_valid.
    "-----------------------------------------------------------------*
    "   Valid message type passed?
    "-----------------------------------------------------------------*
    IF message_type CN 'AEISWX' ##no_text.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_log
        EXPORTING
          textid   = zcx_ca_log=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'MESSAGE_TYPE'
          mv_msgv2 = CONV #( message_type ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_message_type_valid


  METHOD is_problem_class_valid.
    "-----------------------------------------------------------------*
    "   Valid problem class passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = problem_class
                                param_name = 'PROBLEM_CLASS' ) ##no_text.
  ENDMETHOD.                    "is_problem_class_valid


  METHOD is_operating_mode_valid.
    "-----------------------------------------------------------------*
    "   Valid operating mode passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = operating_mode
                                param_name = 'OPERATING_MODE' ) ##no_text.
  ENDMETHOD.                    "is_operating_mode_valid

ENDCLASS.
