*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>ZDEBUGGER_TO_TDC</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SCRIPT_COMMENT>Debugger Skript: Default Template</SCRIPT_COMMENT>
*<SINGLE_STEP>X</SINGLE_STEP>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>

*----------------------------------------------------------------------*
*       CLASS lcl_debugger_script DEFINITION
*----------------------------------------------------------------------*
*       Original Author: Klaus Steinbach <D047201>
*       Enhanced by: Thomas Jansen <D050049>, Rene Längert <D049583>
*
*       Version 0.71 from 01.07.2011
*
*       Debugger Scripts require SAP_BASIS Release 7.10+ or 7.02+!
*
*       Abstract:
*       This debugger script helps reading values from the debugger
*       and stores them directly in a write enabled local or remote Test
*       Data Container (TDC)
*
*       1) Create Test Data Container (TDC) locally or in another system,
*          reachable via RFC Destination (SM59)
*
*       2) In the TDC attributes set the flag "API Access Permitted"
*          to 'X'
*
*       3) In the new debugger load tool "Script Wrapper" under
*          "Special Tools" in any sub-window
*
*       4) Load this script with name "DEBUGGER_TO_TDC" if available in
*          the system. If it is not available in the system, it can be
*          loaded from a local file.
*
*       5) Enter Test Data Container name, version, variant (new or existing)
*          and RFC Destination (if needed)
*
*       6) Enter names of variables as input for TDC parameter values
*          and start script. All TDC parameters, for which a local variable
*          is assigned, are filled. Any other TDC parameters remain unchanged.
*
*----------------------------------------------------------------------*

CLASS lcl_debugger_script DEFINITION INHERITING FROM cl_tpda_script_class_super FINAL.
  PUBLIC SECTION.
    INTERFACES:
      if_tpda_script_w_input.

    TYPES:
      BEGIN OF ts_data_ref,
        instance_name TYPE        string,
        ref_data      TYPE REF TO data,
      END OF ts_data_ref,
      tt_data_ref TYPE HASHED TABLE OF ts_data_ref WITH UNIQUE KEY instance_name.

    DATA:
      mt_data_ref    TYPE                   tt_data_ref,
      mv_tdc_name    TYPE                   etobj_name,
      mv_tdc_version TYPE                   etobj_ver,
      mv_rfc_dest    TYPE                   rfcdest,
      mv_tdc_variant TYPE                   etvar_id,
      mo_tdc_api     TYPE REF TO            cl_apl_ecatt_tdc_api,
      mt_parameter   TYPE STANDARD TABLE OF tpda_transfer_struc,
      mv_debug_mode  TYPE                   abap_bool VALUE abap_false,
      mv_msg         TYPE                   string.

    METHODS:
      script REDEFINITION,

      get_var_data_rtti
        IMPORTING
          i_var_name    TYPE csequence
          i_par_name    TYPE csequence
        RETURNING
          VALUE(result) TYPE REF TO data
        RAISING
          cx_dynamic_check,

      get_var_data_krnl
        IMPORTING
          i_var_name    TYPE csequence
          i_par_name    TYPE csequence
        RETURNING
          VALUE(result) TYPE REF TO data
        RAISING
          cx_dynamic_check,

      get_var_data_krnl2
        IMPORTING
          i_var_name    TYPE csequence
          i_par_name    TYPE csequence
        RETURNING
          VALUE(result) TYPE REF TO data
        RAISING
          cx_dynamic_check,

      get_dataref_from_tdc_param_def
        IMPORTING
          i_par_name    TYPE csequence
        RETURNING
          VALUE(result) TYPE REF TO data
        RAISING
          cx_dynamic_check,

      fill_data
        IMPORTING
          i_var_name       TYPE string
          i_ref_type_descr TYPE REF TO cl_abap_typedescr
        CHANGING
          c_data           TYPE any
        RAISING
          cx_dynamic_check,

      fill_data_table
        IMPORTING
          i_var_name        TYPE string
          i_ref_table_descr TYPE REF TO cl_abap_tabledescr
        CHANGING
          c_data            TYPE ANY TABLE
        RAISING
          cx_dynamic_check,

      fill_data_structure
        IMPORTING
          i_var_name            TYPE string
          i_ref_structure_descr TYPE REF TO cl_abap_structdescr
        CHANGING
          c_data                TYPE any
        RAISING
          cx_dynamic_check,

      fill_data_element
        IMPORTING
          i_var_name TYPE string
        CHANGING
          c_data     TYPE any
        RAISING
          cx_dynamic_check,

      fill_data_reference
        IMPORTING
          i_var_name           TYPE string
          i_ref_data_ref_descr TYPE REF TO cl_abap_refdescr
        CHANGING
          c_data               TYPE any
        RAISING
          cx_dynamic_check,

      fill_data_ref_data
        IMPORTING
          i_var_name TYPE string
        CHANGING
          c_data     TYPE any
        RAISING
          cx_dynamic_check.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_debugger_script IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

CLASS lcl_debugger_script IMPLEMENTATION.
  "--------------------------------------------------------------------"
  "    macro definitions
  "--------------------------------------------------------------------"
  DEFINE mac_dereference_verify.
    unassign &2.
    assert &1 is not initial.
    assign &1->* to &2.
    assert &2 is assigned.
  END-OF-DEFINITION.

  "--------------------------------------------------------------------"
  "    get parameter values
  "--------------------------------------------------------------------"
  METHOD if_tpda_script_w_input~get_parameters.
    TYPES:
      BEGIN OF ts_local,
        name     TYPE string,
        kind     TYPE i,
        parkind  TYPE i,
        parisval TYPE c LENGTH 1,
      END OF ts_local.
    DATA:
      lt_field        TYPE STANDARD TABLE OF sval,
      ls_field        TYPE                   sval,
      lt_parameter    TYPE                   etp_name_tabtype,
      lv_rc           TYPE                   c,
      lo_controller   TYPE REF TO            object,
      lt_local        TYPE STANDARD TABLE OF ts_local,
      lt_global       TYPE STANDARD TABLE OF ts_local,
      lv_parameter_id TYPE                   c LENGTH 20.
    FIELD-SYMBOLS:
      <ls_parameter> LIKE LINE OF lt_parameter,
      <rs_parameter> LIKE LINE OF p_parameters_it.
    ls_field-tabname   = 'ECTD_VER'.
    ls_field-fieldname = 'NAME'.
    lv_parameter_id    = 'D_ECTD_NAME'.
    GET PARAMETER ID lv_parameter_id FIELD ls_field-value.
    ls_field-field_obl = abap_true.
    APPEND ls_field TO lt_field.
    ls_field-tabname   = 'ECTD_VER'.
    ls_field-fieldname = 'VERSION'.
    lv_parameter_id    = 'D_ECTD_VERSION'.
    GET PARAMETER ID lv_parameter_id FIELD ls_field-value.
    ls_field-field_obl = abap_true.
    APPEND ls_field TO lt_field.
    ls_field-tabname   = 'ECTD_VAR'.
    ls_field-fieldname = 'VARID'.
    ls_field-value     = ''.
    ls_field-field_obl = abap_true.
    APPEND ls_field TO lt_field.
    ls_field-tabname   = 'CATR'.
    ls_field-fieldname = 'DEST'.
    ls_field-value     = ''.
    ls_field-fieldtext = 'RFC destination'.                 "#EC NOTEXT
    ls_field-field_obl = abap_false.
    APPEND ls_field TO lt_field.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Test Data Container'
      IMPORTING
        returncode      = lv_rc
      TABLES
        fields          = lt_field
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.                                "#EC NOTEXT

    IF sy-subrc <> 0 OR lv_rc IS NOT INITIAL.
      MESSAGE 'Action cancelled' TYPE 'W'.                  "#EC NOTEXT
    ENDIF.

    READ TABLE lt_field WITH KEY fieldname = 'NAME' INTO ls_field.
    IF ls_field-value(1) = '@'.
      me->mv_debug_mode = 'X'.
      me->mv_tdc_name = ls_field-value+1.
    ELSE.
      me->mv_tdc_name = ls_field-value.
    ENDIF.
    CLEAR ls_field.
    READ TABLE lt_field WITH KEY fieldname = 'VERSION' INTO ls_field.
    me->mv_tdc_version = ls_field-value.
    CLEAR ls_field.
    READ TABLE lt_field WITH KEY fieldname = 'VARID' INTO ls_field.
    me->mv_tdc_variant = ls_field-value.
    CLEAR ls_field.
    READ TABLE lt_field WITH KEY fieldname = 'DEST' INTO ls_field.
    me->mv_rfc_dest = ls_field-value.
    IF me->mv_tdc_name IS INITIAL OR me->mv_tdc_variant IS INITIAL.
      MESSAGE 'Please provide a TDC name and a variant name' TYPE 'E'. "#EC NOTEXT
    ENDIF.

    IF me->mv_debug_mode = 'X'.
      BREAK-POINT.                                         "#EC NOBREAK
    ENDIF.

    APPEND INITIAL LINE TO p_parameters_it ASSIGNING <rs_parameter>.
    <rs_parameter>-id    = 'TDC name'.                      "#EC NOTEXT
    <rs_parameter>-value = me->mv_tdc_name && '(CAN NOT BE MODIFIED HERE!)'.
    APPEND INITIAL LINE TO p_parameters_it ASSIGNING <rs_parameter>.
    <rs_parameter>-id    = 'Variant name'.                  "#EC NOTEXT
    <rs_parameter>-value = me->mv_tdc_variant && '(CAN NOT BE MODIFIED HERE!)'.

    TRY.
        me->mo_tdc_api = cl_apl_ecatt_tdc_api=>get_instance(
            i_testdatacontainer         = me->mv_tdc_name
            i_testdatacontainer_version = me->mv_tdc_version
            i_tdc_rfcdest               = me->mv_rfc_dest
            i_write_access              = abap_true ).
      CATCH cx_ecatt_tdc_access.
        TRY.
            me->mo_tdc_api = cl_apl_ecatt_tdc_api=>get_instance(
                i_testdatacontainer         = me->mv_tdc_name
                i_testdatacontainer_version = me->mv_tdc_version
                i_tdc_rfcdest               = me->mv_rfc_dest
                i_write_access              = abap_false ).
            MESSAGE 'TDC could not be opened with write access' TYPE 'E'. "#EC NOTEXT
          CATCH cx_ecatt_tdc_access.
            MESSAGE 'TDC could not be opened' TYPE 'E'.     "#EC NOTEXT
        ENDTRY.
    ENDTRY.

    IF me->mv_rfc_dest IS INITIAL OR me->mv_rfc_dest = 'NONE'.
      lv_parameter_id    = 'D_ECTD_NAME'.
      SET PARAMETER ID lv_parameter_id FIELD me->mv_tdc_name.
      lv_parameter_id    = 'D_ECTD_VERSION'.
      SET PARAMETER ID lv_parameter_id FIELD me->mv_tdc_version.
    ENDIF.

    TRY.
        lt_parameter = me->mo_tdc_api->get_param_list( ).
        LOOP AT lt_parameter ASSIGNING <ls_parameter>.
          APPEND INITIAL LINE TO p_parameters_it ASSIGNING <rs_parameter>.
          <rs_parameter>-id = <ls_parameter>.

**********************************************************************
* Parameter (default) assignemnts can be done HERE!
*
* Standard method is to search TDC parameters and variables from the
* Debugger context and match those with the same name
*
**********************************************************************
          IF lo_controller IS NOT BOUND.
            CREATE OBJECT lo_controller TYPE ('CL_TPDA_CONTROL').
            TRY.
                CALL METHOD lo_controller->('GET_LOCALS')
                  IMPORTING
                    localstab = lt_local.
              CATCH cx_root.
                CLEAR lt_local.
            ENDTRY.

            TRY.
                CALL METHOD lo_controller->('GET_GLOBALS')
                  IMPORTING
                    globalstab = lt_global.
              CATCH cx_root.
                CLEAR lt_global.
            ENDTRY.

          ENDIF.

          READ TABLE lt_local WITH KEY name = <ls_parameter> TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            <rs_parameter>-value = <ls_parameter>.
          ENDIF.

          READ TABLE lt_global WITH KEY name = <ls_parameter> TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            <rs_parameter>-value = <ls_parameter>.
          ENDIF.
        ENDLOOP.

      CATCH cx_ecatt_tdc_access.
        MESSAGE 'Parameter list of TDC could not be retrieved' TYPE 'E'. "#EC NOTEXT
    ENDTRY.
  ENDMETHOD.

  "--------------------------------------------------------------------"
  "    main script method
  "--------------------------------------------------------------------"
  METHOD script.
    DATA:
      lo_data                   TYPE REF TO data,
      lo_actual_data            TYPE REF TO data,
      lv_variant_already_exists TYPE        abap_bool,
      lv_data_was_written       TYPE        abap_bool,
      lv_answer                 TYPE        c,
      lv_msg                    TYPE        string.
    FIELD-SYMBOLS:
      <parameter> LIKE LINE OF me->mt_parameter,
      <data>      TYPE         any.

    IF me->mv_debug_mode = 'X'.
      BREAK-POINT.                                         "#EC NOBREAK
    ENDIF.

    IF me->mo_tdc_api IS NOT BOUND AND me->mv_tdc_name IS INITIAL.
      MESSAGE 'Please load Debugger Script with Script Wrapper' TYPE 'E'. "#EC NOTEXT
    ENDIF.
    TRY.
        me->mo_tdc_api->get_variant_description( me->mv_tdc_variant ).
        lv_variant_already_exists = abap_true.
      CATCH cx_ecatt_tdc_access.
        TRY.
            me->mo_tdc_api->create_variant( me->mv_tdc_variant ).
          CATCH cx_ecatt_tdc_access.
            MESSAGE 'Variant could not be created' TYPE 'E'. "#EC NOTEXT
        ENDTRY.
    ENDTRY.
    TRY.
        LOOP AT me->mt_parameter ASSIGNING <parameter>
          WHERE value IS NOT INITIAL AND
                id <> 'TDC name' AND
                id <> 'Variant name'.
          <parameter>-value = to_upper( <parameter>-value ).
          TRY.
              TRY.
                  lo_data = me->get_var_data_krnl(
                           i_var_name = <parameter>-value
                           i_par_name = <parameter>-id ).
                CATCH cx_sy_dyn_call_error
                      cx_sy_create_data_error
                      cx_sy_create_object_error.
                  TRY.
                      lo_data = me->get_var_data_krnl2(
                        i_var_name = <parameter>-value
                        i_par_name = <parameter>-id ).
                    CATCH
                      cx_sy_dyn_call_error
                      cx_sy_create_data_error
                      cx_sy_create_object_error.

                      lo_data = me->get_var_data_rtti(
                        i_var_name = <parameter>-value
                        i_par_name = <parameter>-id ).
                  ENDTRY.
              ENDTRY.
            CATCH cx_static_check.
              CONCATENATE 'Error while mapping values for ''' <parameter>-value ''''
                INTO me->mv_msg SEPARATED BY space.         "#EC NOTEXT
              MESSAGE me->mv_msg TYPE 'E'.
          ENDTRY.
          " check if parameter exists and is not initial
          UNASSIGN <data>.
          IF lv_variant_already_exists = abap_true.
            CLEAR lo_actual_data.
            lo_actual_data = get_dataref_from_tdc_param_def( '' && <parameter>-id ).
            ASSIGN lo_actual_data->* TO <data>.
            me->mo_tdc_api->get_value_ref(
              EXPORTING
                i_param_name   = '' && <parameter>-id
                i_variant_name = me->mv_tdc_variant
              CHANGING
                e_param_ref    = lo_actual_data ).
          ENDIF.
          " Popup if overwrite is allowed
          lv_answer = '1'. "yes
          IF <data> IS ASSIGNED AND <data> IS NOT INITIAL.
            CONCATENATE 'Do you want to overwrite the existing value for parameter'
                        <parameter>-id '?' INTO lv_msg SEPARATED BY space. "#EC NOTEXT
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar       = 'Overwrite Confirmation'
                text_question  = lv_msg
              IMPORTING
                answer         = lv_answer
              EXCEPTIONS
                text_not_found = 1
                OTHERS         = 2.                         "#EC NOTEXT
            IF sy-subrc <> 0.
              MESSAGE 'Error in POPUP_TO_CONFIRM' TYPE 'E'. "#EC NOTEXT
            ENDIF.
            IF lv_answer = 'A'. "abort
              MESSAGE 'Cancelled! No data has been written to the TDC' TYPE 'E'. "#EC NOTEXT
            ENDIF.
          ENDIF.
          IF lv_answer = '1'. "yes
            me->mo_tdc_api->set_value_ref(
                i_param_name   = '' && <parameter>-id
                i_variant_name = me->mv_tdc_variant
                i_param_ref    = lo_data ).
            lv_data_was_written = abap_true.
          ENDIF.
        ENDLOOP.
        IF lv_data_was_written = abap_true.
          me->mo_tdc_api->commit_changes( ).
          SET UPDATE TASK LOCAL.
          COMMIT WORK AND WAIT.
          MESSAGE 'Data has been written to the TDC' TYPE 'S'. "#EC NOTEXT
        ELSE.
          MESSAGE 'No data has been written to the TDC' TYPE 'I'. "#EC NOTEXT
        ENDIF.
      CATCH cx_static_check.
        MESSAGE 'Error during write access, changes are not commited' TYPE 'E'. "#EC NOTEXT
    ENDTRY.
  ENDMETHOD.

  "--------------------------------------------------------------------"
  "    set parameter values
  "--------------------------------------------------------------------"
  METHOD if_tpda_script_w_input~set_parameter_values.
    me->mt_parameter = p_parameter_values_it.
  ENDMETHOD.

  "--------------------------------------------------------------------"
  "    read values of variables (kernel functionality)
  "--------------------------------------------------------------------"
  METHOD get_var_data_krnl.
    DATA:
      lv_symbname  TYPE        c LENGTH 255,
      lr_symbquick TYPE REF TO data,
      xml          TYPE        xstring,
      control      TYPE REF TO object,
      results      TYPE        abap_trans_resbind_tab.
    FIELD-SYMBOLS:
      <ls_symbquick> TYPE         any,
      <lv_value>     TYPE         any,
      <result>       LIKE LINE OF results.

    IF me->mv_debug_mode = 'X'.
      BREAK-POINT.                                         "#EC NOBREAK
    ENDIF.

    lv_symbname = i_var_name.
    CREATE OBJECT control TYPE ('CL_TPDA_CONTROL_ABAP').

    CREATE DATA lr_symbquick TYPE ('TPDA_SYS_SYMBQUICK').
    ASSIGN lr_symbquick->* TO <ls_symbquick>.
    CALL METHOD control->('IF_TPDA_CONTROL~GET_SYMB_ASXML')
      EXPORTING
        symbname  = lv_symbname
        offset    = -1
        len       = -1
      IMPORTING
        symbquick = <ls_symbquick>
        xml       = xml.

    APPEND INITIAL LINE TO results ASSIGNING <result>.
    ASSIGN COMPONENT 'ABSTYPENAME' OF STRUCTURE <ls_symbquick> TO <lv_value>.
    <result>-name = <lv_value>.

    TRY.
        CREATE DATA <result>-value TYPE (<lv_value>).
      CATCH cx_sy_create_data_error.
        <result>-value = get_dataref_from_tdc_param_def( i_par_name ).
    ENDTRY.

    CALL TRANSFORMATION id
      SOURCE XML xml
      RESULT (results).

    result = <result>-value.
  ENDMETHOD.

  "--------------------------------------------------------------------"
  "    read values of variables (kernel functionality)
  "--------------------------------------------------------------------"
  METHOD get_var_data_krnl2.
    DATA:
      lv_symbname  TYPE        c LENGTH 255,
      lr_symbquick TYPE REF TO data,
      xml          TYPE        xstring,
      control      TYPE REF TO object,
      results      TYPE        abap_trans_resbind_tab.
    FIELD-SYMBOLS:
      <ls_symbquick> TYPE         any,
      <lv_value>     TYPE         any,
      <result>       LIKE LINE OF results.

    IF me->mv_debug_mode = 'X'.
      BREAK-POINT.                                         "#EC NOBREAK
    ENDIF.

    lv_symbname = i_var_name.
    CREATE OBJECT control TYPE ('CL_TPDA_CONTROL').

    CREATE DATA lr_symbquick TYPE ('TPDA_SYS_SYMBQUICK').
    ASSIGN lr_symbquick->* TO <ls_symbquick>.
    CALL METHOD control->('IF_TPDA_CONTROL~GET_SYMB_ASXML')
      EXPORTING
        symbname  = lv_symbname
        offset    = -1
        len       = -1
      IMPORTING
        symbquick = <ls_symbquick>
        xml       = xml.

    APPEND INITIAL LINE TO results ASSIGNING <result>.
    ASSIGN COMPONENT 'ABSTYPENAME' OF STRUCTURE <ls_symbquick> TO <lv_value>.
    <result>-name = <lv_value>.

    TRY.
        CREATE DATA <result>-value TYPE (<lv_value>).
      CATCH cx_sy_create_data_error.
        <result>-value = get_dataref_from_tdc_param_def( i_par_name ).
    ENDTRY.

    CALL TRANSFORMATION id
      SOURCE XML xml
      RESULT (results).

    result = <result>-value.
  ENDMETHOD.

  METHOD get_dataref_from_tdc_param_def.
    DATA:
      l_is_table   TYPE abap_bool,
      l_param_def  TYPE string,
      l_p_type     TYPE string,
      l_p_name     TYPE etp_name,
      l_p_length   TYPE c,
      l_p_decimals TYPE c.
    TRY.
        l_p_name    = i_par_name.
        l_param_def = me->mo_tdc_api->get_param_definition( l_p_name ).
        IF l_param_def CS 'STANDARD TABLE OF'.
          REPLACE FIRST OCCURRENCE OF 'STANDARD TABLE OF' IN l_param_def WITH ''.
          CONDENSE l_param_def.
          l_is_table = abap_true.
        ENDIF.
        SPLIT l_param_def AT space INTO l_p_type l_param_def.
        TRANSLATE l_p_type TO UPPER CASE.
        IF l_param_def CS 'LENGTH'.
          REPLACE FIRST OCCURRENCE OF 'LENGTH' IN l_param_def WITH ''.
          CONDENSE l_param_def.
          SPLIT l_param_def AT space INTO l_p_length l_param_def.
        ENDIF.
        IF l_param_def CS 'DECIMALS'.
          REPLACE FIRST OCCURRENCE OF 'DECIMALS' IN l_param_def WITH ''.
          CONDENSE l_param_def.
          SPLIT l_param_def AT space INTO l_p_decimals l_param_def.
        ENDIF.
        IF l_is_table = abap_true.
          CREATE DATA result TYPE STANDARD TABLE OF (l_p_type).
        ELSE.
          IF     l_p_length IS     INITIAL.
            CREATE DATA result TYPE (l_p_type).
          ELSE.
            IF l_p_type = 'P' AND l_p_decimals IS NOT INITIAL.
              CREATE DATA result TYPE p LENGTH l_p_length DECIMALS l_p_decimals.
            ELSE.
              CREATE DATA result TYPE (l_p_type) LENGTH l_p_length.
            ENDIF.
          ENDIF.
        ENDIF.
      CATCH cx_ecatt_tdc_access.
        RAISE EXCEPTION TYPE cx_sy_create_data_error.
    ENDTRY.
  ENDMETHOD.

  "--------------------------------------------------------------------"
  "    read values of variables (rtti method)
  "--------------------------------------------------------------------"
  METHOD get_var_data_rtti.
    CONSTANTS:
      lc_dereference TYPE string VALUE '->*'.               "#EC NOTEXT

    DATA:
      l_str_quickinfo TYPE        tpda_scr_quick_info,
      lo_type_descr   TYPE REF TO cl_abap_typedescr,
      lo_data_descr   TYPE REF TO cl_abap_datadescr,
      lo_data         TYPE REF TO data,
      l_var_name      TYPE        string.

    FIELD-SYMBOLS:
      <data> TYPE any.

    IF me->mv_debug_mode = 'X'.
      BREAK-POINT.                                         "#EC NOBREAK
    ENDIF.

    l_var_name = i_var_name.

    DO 2 TIMES.
      TRY.
          l_str_quickinfo = cl_tpda_script_data_descr=>get_quick_info( l_var_name ).
        CATCH cx_tpda_varname.                          "#EC NO_HANDLER
          " will be handled below
      ENDTRY.
      cl_abap_typedescr=>describe_by_name(
        EXPORTING
          p_name         = l_str_quickinfo-abstypename
        RECEIVING
          p_descr_ref    = lo_type_descr
        EXCEPTIONS
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        l_var_name = l_var_name && lc_dereference.
      ENDIF.
    ENDDO.

    IF lo_type_descr IS NOT BOUND.
      TRY.
          l_var_name = i_var_name.
          lo_data = get_dataref_from_tdc_param_def( i_par_name ).
          cl_abap_typedescr=>describe_by_data_ref(
            EXPORTING
              p_data_ref           = lo_data
            RECEIVING
              p_descr_ref          = lo_type_descr
            EXCEPTIONS
              reference_is_initial = 1
              OTHERS               = 2
                 ).
          IF sy-subrc <> 0.
            CONCATENATE 'Unable to retrieve data type information for ''' i_var_name ''''
              INTO me->mv_msg SEPARATED BY space.           "#EC NOTEXT
            MESSAGE me->mv_msg TYPE 'E'.
          ENDIF.
        CATCH cx_sy_create_data_error.
          CONCATENATE 'Unable to retrieve data type information for ''' i_var_name ''''
            INTO me->mv_msg SEPARATED BY space.             "#EC NOTEXT
          MESSAGE me->mv_msg TYPE 'E'.
      ENDTRY.
    ENDIF.

    IF lo_type_descr->kind <> cl_abap_typedescr=>kind_elem   AND
       lo_type_descr->kind <> cl_abap_typedescr=>kind_ref    AND
       lo_type_descr->kind <> cl_abap_typedescr=>kind_struct AND
       lo_type_descr->kind <> cl_abap_typedescr=>kind_table.
      CONCATENATE 'Data type kind ''' lo_type_descr->kind ''' is not supported'
        INTO me->mv_msg SEPARATED BY space.                 "#EC NOTEXT
      MESSAGE me->mv_msg TYPE 'E'.
    ENDIF.

    lo_data_descr ?= lo_type_descr.

    TRY.
        CREATE DATA result TYPE HANDLE lo_data_descr.
      CATCH cx_sy_create_data_error.
        CONCATENATE 'Transfer structure could not be created for ''' i_var_name ''''
          INTO me->mv_msg SEPARATED BY space.               "#EC NOTEXT
        MESSAGE me->mv_msg TYPE 'E'.
    ENDTRY.

    mac_dereference_verify result <data>.

    me->fill_data(
          EXPORTING
            i_var_name       = l_var_name
            i_ref_type_descr = lo_type_descr
          CHANGING
            c_data           = <data> ).
  ENDMETHOD.

  "--------------------------------------------------------------------"
  "    fill data
  "--------------------------------------------------------------------"
  METHOD fill_data.
    DATA:
      lo_table_descr    TYPE REF TO cl_abap_tabledescr,
      lo_struct_descr   TYPE REF TO cl_abap_structdescr,
      lo_data_ref_descr TYPE REF TO cl_abap_refdescr.

    CASE i_ref_type_descr->kind.
      WHEN cl_abap_typedescr=>kind_table.
        lo_table_descr ?= i_ref_type_descr.
        me->fill_data_table(
              EXPORTING
                i_var_name        = i_var_name
                i_ref_table_descr = lo_table_descr
              CHANGING
                c_data            = c_data ).

      WHEN cl_abap_typedescr=>kind_struct.
        lo_struct_descr ?= i_ref_type_descr.
        me->fill_data_structure(
              EXPORTING
                i_var_name            = i_var_name
                i_ref_structure_descr = lo_struct_descr
              CHANGING
                c_data                = c_data ).

      WHEN cl_abap_typedescr=>kind_elem.
        me->fill_data_element(
              EXPORTING
                i_var_name          = i_var_name
              CHANGING
                c_data              = c_data ).

      WHEN cl_abap_typedescr=>kind_ref.
        lo_data_ref_descr ?= i_ref_type_descr.
        me->fill_data_reference(
           EXPORTING
             i_var_name           = i_var_name
             i_ref_data_ref_descr = lo_data_ref_descr
           CHANGING
             c_data               = c_data ).

      WHEN OTHERS.
        CONCATENATE 'Data type is not supported for ''' i_var_name ''''
          INTO me->mv_msg SEPARATED BY space.               "#EC NOTEXT
        MESSAGE me->mv_msg TYPE 'E'.
    ENDCASE.
  ENDMETHOD.

  "--------------------------------------------------------------------"
  "    fill data reference
  "--------------------------------------------------------------------"
  METHOD fill_data_reference.
    CASE i_ref_data_ref_descr->type_kind.
      WHEN cl_abap_typedescr=>typekind_dref.
        me->fill_data_ref_data(
              EXPORTING
                i_var_name           = i_var_name
              CHANGING
                c_data               = c_data ).
      WHEN OTHERS.
        " Don't do anysthing
    ENDCASE.
  ENDMETHOD.

  "--------------------------------------------------------------------"
  "    fill data reference data
  "--------------------------------------------------------------------"
  METHOD fill_data_ref_data.
    CONSTANTS:
      lc_initial_data_ref TYPE string VALUE '{A:initial}',  "#EC NOTEXT
      lc_dereference      TYPE string                             VALUE '->*'. "#EC NOTEXT
    DATA:
      lo_script_dataref_descr TYPE REF TO  cl_tpda_script_datrefdescr,
      l_instance_name         TYPE         string,
      l_str_quickinfo         TYPE         tpda_scr_quick_info,
      lo_type_descr           TYPE REF TO  cl_abap_typedescr,
      lo_referenced_type      TYPE REF TO  cl_abap_datadescr,
      l_var_name              LIKE         i_var_name,
      l_str_data_ref          LIKE LINE OF me->mt_data_ref.
    FIELD-SYMBOLS:
      <str_data_ref> LIKE LINE OF me->mt_data_ref,
      <data>         TYPE         any.

    TRY.
        lo_script_dataref_descr ?= cl_tpda_script_data_descr=>factory( i_var_name ).
        l_instance_name = lo_script_dataref_descr->instancename( ).
        IF lc_initial_data_ref = l_instance_name.
          RETURN."initial reference => nothing to do
        ENDIF.
      CATCH cx_static_check.
        CONCATENATE 'Data type is not supported for ''' i_var_name ''''
          INTO me->mv_msg SEPARATED BY space.               "#EC NOTEXT
        MESSAGE me->mv_msg TYPE 'E'.
    ENDTRY.

    READ TABLE me->mt_data_ref ASSIGNING <str_data_ref>
         WITH TABLE KEY instance_name = l_instance_name.
    IF <str_data_ref> IS ASSIGNED. "found in buffer
      c_data ?= <str_data_ref>-ref_data.
    ELSE. "create data and fill it
      CONCATENATE i_var_name lc_dereference INTO l_var_name.

      TRY.
          l_str_quickinfo = cl_tpda_script_data_descr=>get_quick_info( l_var_name ).
        CATCH cx_tpda_varname.
          CONCATENATE 'Unable to retrieve data type information for ''' i_var_name ''''
            INTO me->mv_msg SEPARATED BY space.             "#EC NOTEXT
          MESSAGE me->mv_msg TYPE 'E'.
      ENDTRY.

      cl_abap_typedescr=>describe_by_name(
        EXPORTING
          p_name         = l_str_quickinfo-abstypename
        RECEIVING
          p_descr_ref    = lo_type_descr
        EXCEPTIONS
          OTHERS         = 2 ).
      IF sy-subrc <> 0 OR lo_type_descr IS NOT BOUND.
        CONCATENATE 'Data type is not supported for ''' i_var_name ''''
          INTO me->mv_msg SEPARATED BY space.               "#EC NOTEXT
        MESSAGE me->mv_msg TYPE 'E'.
      ENDIF.

      lo_referenced_type ?= lo_type_descr.

      CREATE DATA c_data TYPE HANDLE lo_referenced_type.
      mac_dereference_verify c_data <data>.

      me->fill_data(
        EXPORTING
          i_var_name       = l_var_name
          i_ref_type_descr = lo_referenced_type
        CHANGING
          c_data           = <data> ).

      "add to buffer
      l_str_data_ref-instance_name = l_instance_name.
      l_str_data_ref-ref_data      = c_data.
      INSERT l_str_data_ref INTO TABLE me->mt_data_ref.
    ENDIF.
  ENDMETHOD.

  "--------------------------------------------------------------------"
  "    fill data table
  "--------------------------------------------------------------------"
  METHOD fill_data_table.
    CONSTANTS:
      lc_table_open  TYPE string VALUE '[',                 "#EC NOTEXT
      lc_table_close TYPE string VALUE ']'.                 "#EC NOTEXT
    DATA:
      lo_script_table_descr TYPE REF TO cl_tpda_script_tabledescr,
      lo_type_descr         TYPE REF TO cl_abap_typedescr,
      lv_var_name           TYPE        string,
      lo_data               TYPE REF TO data,
      lv_lines              TYPE        i.
    FIELD-SYMBOLS:
      <initial_line> TYPE any.

    TRY.
        lo_script_table_descr ?= cl_tpda_script_data_descr=>factory( i_var_name ).
        CREATE DATA lo_data LIKE LINE OF c_data.
        mac_dereference_verify lo_data <initial_line>.
        lv_lines = lo_script_table_descr->linecnt( ).
      CATCH cx_static_check.
        CONCATENATE 'Data type is not supported for ''' i_var_name ''''
          INTO me->mv_msg SEPARATED BY space.               "#EC NOTEXT
        MESSAGE me->mv_msg TYPE 'E'.
    ENDTRY.

    lo_type_descr = i_ref_table_descr->get_table_line_type( ).

    DO lv_lines TIMES.
      lv_var_name = sy-index. CONDENSE lv_var_name.
      CLEAR <initial_line>.
      CONCATENATE i_var_name lc_table_open lv_var_name lc_table_close INTO lv_var_name.
      me->fill_data(
            EXPORTING
              i_var_name       = lv_var_name
              i_ref_type_descr = lo_type_descr
            CHANGING
              c_data           = <initial_line> ).
      INSERT <initial_line> INTO TABLE c_data.
    ENDDO.
  ENDMETHOD.

  "--------------------------------------------------------------------"
  "    fill data structure
  "--------------------------------------------------------------------"
  METHOD fill_data_structure.
    CONSTANTS:
      lc_structure TYPE string VALUE '-',                   "#EC NOTEXT
      lc_dref_str  TYPE string VALUE '->*-',                "#EC NOTEXT
      lc_ref_str   TYPE string VALUE '->'.                  "#EC NOTEXT
    DATA:
      lv_var_name       TYPE string,
      lv_tab_components TYPE cl_abap_structdescr=>component_table.
    FIELD-SYMBOLS:
      <str_component> LIKE LINE OF lv_tab_components,
      <field>         TYPE         any.

    lv_tab_components = i_ref_structure_descr->get_components( ).
    LOOP AT lv_tab_components ASSIGNING <str_component>.
      IF <str_component>-as_include IS NOT INITIAL AND <str_component>-name IS INITIAL.
        me->fill_data(
              EXPORTING
                i_var_name       = i_var_name
                i_ref_type_descr = <str_component>-type
              CHANGING
                c_data           = c_data ).
      ELSE.
        CONCATENATE i_var_name lc_structure <str_component>-name INTO lv_var_name.
        REPLACE lc_dref_str IN lv_var_name WITH lc_ref_str.
        ASSIGN COMPONENT <str_component>-name OF STRUCTURE c_data TO <field>.
        me->fill_data(
              EXPORTING
                i_var_name       = lv_var_name
                i_ref_type_descr = <str_component>-type
              CHANGING
                c_data           = <field> ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  "--------------------------------------------------------------------"
  "    fill data element
  "--------------------------------------------------------------------"
  METHOD fill_data_element.
    TRY.
        c_data = cl_tpda_script_data_descr=>get_simple_value( i_var_name ).
      CATCH cx_static_check.
        CONCATENATE 'Data type is not supported for ''' i_var_name ''''
          INTO me->mv_msg SEPARATED BY space.               "#EC NOTEXT
        MESSAGE me->mv_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
