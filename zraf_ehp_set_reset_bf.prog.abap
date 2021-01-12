*&---------------------------------------------------------------------*
*& Report  ZRAF_EHP_SET_RESET_BF
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zraf_ehp_set_reset_bf.

*----------------------------------------------------------------------*
*       CLASS lc_switch_bf DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lc_switch_bf DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      switch_bf IMPORTING bf_name TYPE sfw_bfunction bf_set TYPE char1.
ENDCLASS.                    "lc_switch_bf DEFINITION

PARAMETERS: bf_set  TYPE char1 NO-DISPLAY, "Set/Reset/Delete
            bf_name TYPE sfw_bf-bfunction, "sfw_bfunction,
            sw_name TYPE sfw_switch_id,
            display  RADIOBUTTON GROUP user DEFAULT 'X',
            user_on  RADIOBUTTON GROUP user,
            user_off RADIOBUTTON GROUP user,
            user_del RADIOBUTTON GROUP user.

START-OF-SELECTION.

  CASE 'X'.
    WHEN user_on.
      bf_set = 'S'.
    WHEN user_off.
      bf_set = 'R'.
    WHEN user_del.
      bf_set = 'D'.
  ENDCASE.

*  AUTHORITY-CHECK OBJECT 'S_DEVELOP'
*    ID 'OBJTYPE' FIELD 'SFSS'
*    ID 'ACTVT'   FIELD '02'.
*  IF sy-subrc NE 0.
*    MESSAGE e149(00) WITH 'SFSS'.
*  ENDIF.

  IF bf_set CA 'RDS'.
    SELECT COUNT(*) FROM sfw_sw_bf WHERE bfunction = bf_name.
    IF sy-dbcnt EQ 0.
      SELECT COUNT(*) FROM sfw_switch WHERE switch_id = sw_name.
    ENDIF.
    IF sy-dbcnt EQ 0.
      MESSAGE i398(00) WITH 'Please enter valid business function or switch'.
      STOP.
    ENDIF.
    lc_switch_bf=>switch_bf( bf_name = bf_name bf_set = bf_set ).
  ENDIF.

  PERFORM display_setting.

*&---------------------------------------------------------------------*
*&      Form  display_setting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_setting.
  DATA: lt_state TYPE TABLE OF sfw_switch_state.

  SELECT * FROM sfw_switch_state INTO TABLE lt_state
         WHERE username = sy-uname.

  DATA: lr_table  TYPE REF TO cl_salv_table.

  CALL METHOD cl_salv_table=>factory
    IMPORTING
      r_salv_table = lr_table
    CHANGING
      t_table      = lt_state.

*... Display table
  lr_table->display( ).

ENDFORM.                    "display_setting

*----------------------------------------------------------------------*
*       CLASS lc_switch_bf IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lc_switch_bf IMPLEMENTATION.

  METHOD switch_bf.
    DATA: l_swtab TYPE TABLE OF sfw_sw_bf,
          l_sstate TYPE sfw_switch_state,
          l_switch TYPE sfw_sw_bf,
          l_ssettings TYPE sfw_settings.
* Get all switches for given business function including the enhancements
    SELECT * FROM sfw_sw_bf INTO TABLE l_swtab WHERE bfunction = bf_name.
* Get the switches according the child-->parent relation
    SELECT * FROM sfw_bf_sw APPENDING TABLE l_swtab WHERE bfunction = bf_name.

* Add the explicitely given switch
    IF sw_name IS NOT INITIAL.
      l_switch-switch_id = sw_name.
      APPEND l_switch TO l_swtab.
    ENDIF.

    IF NOT l_swtab IS INITIAL.
** Invalidate buffer of the runtime tables for active business functions on all servers
*      cl_sfw_activate=>invalidate_buffer( ).
      LOOP AT l_swtab INTO l_switch.
        l_sstate-switch_id = l_switch-switch_id.
        l_sstate-version = cl_abap_switch_state_admin=>c_active.
        l_sstate-client = sy-mandt.
        l_sstate-username = sy-uname.
        CASE bf_set.
          WHEN 'R'.
* Reset or turn off
            l_sstate-state = cl_abap_switch=>c_off.
          WHEN 'S'.
* Set or turn on
            l_sstate-state = cl_abap_switch=>c_on.
        ENDCASE.
        APPEND l_sstate TO l_ssettings.
      ENDLOOP.
      TRY.
        CASE bf_set.
          WHEN 'D'.
* Delete user dependant settings
            cl_abap_switch_state_admin=>delete( l_ssettings ).
          WHEN OTHERS.
            cl_abap_switch_state_admin=>modify( l_ssettings ).
        ENDCASE.
      ENDTRY.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.                    "switch_bf
ENDCLASS.                    "lc_switch_bf IMPLEMENTATION
