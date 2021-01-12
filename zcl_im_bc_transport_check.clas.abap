class ZCL_IM_BC_TRANSPORT_CHECK definition
  public
  final
  create public .

*"* public components of class ZCL_IM_BC_TRANSPORT_CHECK
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_CTS_REQUEST_CHECK .
protected section.
*"* protected components of class ZCL_IM_BC_TRANSPORT_CHECK
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_BC_TRANSPORT_CHECK
*"* do not include other source files here!!!

  type-pools ABAP .
  methods IS_NAME_CORRECT
    importing
      !TYPE type TRFUNCTION
      !TEXT type AS4TEXT
    returning
      value(RV_CORRECT) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_IM_BC_TRANSPORT_CHECK IMPLEMENTATION.


METHOD if_ex_cts_request_check~check_before_add_objects.



  ENDMETHOD.


method IF_EX_CTS_REQUEST_CHECK~CHECK_BEFORE_CHANGING_OWNER.
endmethod.


METHOD if_ex_cts_request_check~check_before_creation.

  IF me->is_name_correct( type = type text = text ) = abap_false.
*    CONCATENATE lv_type '??_TTMMJJJJ: TODO bitte deinen Text anpassen!!! ' INTO text.
*      text = 'WB_??_TTMMJJJJ: TODO Bitte hier Textbeschreibung erfassen'.
    MESSAGE i000(zlocal).
*    RAISE cancel.
*      MESSAGE e000(zlocal)." RAISING cancel.
  ENDIF.
*  MESSAGE e003(zlocal) RAISING cancel.
ENDMETHOD.


METHOD if_ex_cts_request_check~check_before_release.
  DATA lv_description TYPE  text60.
  DATA lt_objects     TYPE STANDARD TABLE OF  cts_obj.

*   MESSAGE e002(zlocal) RAISING cancel.

  IF me->is_name_correct( type = type text = text ) = abap_false.
*      text = 'WB_??_TTMMJJJJ: TODO Bitte hier Textbeschreibung erfassen'.
    MESSAGE e000(zlocal) RAISING cancel.
*    RAISE cancel.
  ENDIF.

  IF type = 'K'. "nur bei WB
    CALL FUNCTION 'CTS_API_READ_CHANGE_REQUEST' DESTINATION 'LS1CLNT100'
      EXPORTING
        request     = 'LS1K900031'
      IMPORTING
        description = lv_description
      TABLES
        objects     = lt_objects.
    LOOP AT objects ASSIGNING FIELD-SYMBOL(<ls_object>).
      LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<ls_s4_object>) WHERE object = <ls_object>-object
                                                                  AND name   = <ls_object>-obj_name.
        MESSAGE e004(zlocal) WITH <ls_object>-object lv_description RAISING cancel.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

*  IF sy-uname = 'TCON' OR sy-uname = 'TCON1'.
*  MESSAGE e003(zlocal) RAISING cancel.
*  ENDIF.
ENDMETHOD.


  method IF_EX_CTS_REQUEST_CHECK~CHECK_BEFORE_RELEASE_SLIN.
  endmethod.


METHOD is_name_correct.
  DATA lo_regex TYPE REF TO cl_abap_regex.
  DATA lo_matcher TYPE REF TO cl_abap_matcher.
  DATA lv_type TYPE string.
  DATA lv_date TYPE string.
  DATA lv_changerequest TYPE string.
  DATA lv_pattern TYPE string.
  DATA lv_pattern2 TYPE string.
  CONSTANTS lc_moduls TYPE string VALUE '(SD|BW|MM|BC|PP|FI|CO|WM|QM|WF|LMPC|PLC)_'.
  CONSTANTS lc_text TYPE string VALUE ': [ [:word:][:punct:] ]{20}'. "minimum 15 characters text
*example regex 'WB_(SD|MM|BC|PP|FI|CO|WM|QM)_([0-9]{4}201[4-9]|201[4-9][0-9]{4}|C[0-9]{5}): [0-9a-zA-Zäöüß ]{15}'.
  rv_correct = abap_false.

  IF type = 'K'.
    lv_type = 'WB_'.
    lv_changerequest = 'CR[0-9]{5})'.
  ELSEIF type = 'W'.
    lv_type = 'CU_'.
    lv_changerequest = 'C[0-9]{5})'.
  ELSE.
*    nur Transporte - keine Aufgaben prüfen
    rv_correct = abap_true.
    RETURN.
  ENDIF.
  lv_date = '([0-9]{4}20[1-4][0-9]|20[1-4][0-9][0-9]{4}'.
  CONCATENATE lv_date lv_changerequest INTO lv_date SEPARATED BY '|'.

  CONCATENATE lv_type lc_moduls lv_date lc_text INTO lv_pattern.
*  only to compare if regex pattern is correct concatenated
  lv_pattern2 = 'WB_(SD|MM|BC|PP|FI|CO|WM|QM)_([0-9]{4}20[1-4][0-9]|20[1-4][0-9][0-9]{4}|C[0-9]{5}): [0-9a-zA-Zäöüß_ ]{15}'.
  TRY.
      CREATE OBJECT lo_regex
        EXPORTING
          pattern     = lv_pattern
          ignore_case = abap_true.
      lo_matcher = lo_regex->create_matcher( text  = text ).
      rv_correct = lo_matcher->find_next( ).
    CATCH cx_sy_regex cx_sy_matcher .
      RETURN.
  ENDTRY.
  IF rv_correct = abap_false.
    RETURN.
  ENDIF.
  SPLIT text AT ':' INTO: DATA(pre) DATA(description).
  IF sy-uname CS 'TCON'.
    lv_pattern = '[0-9]{3,6}'.
    TRY.
        CREATE OBJECT lo_regex
          EXPORTING
            pattern     = lv_pattern
            ignore_case = abap_true.
        lo_matcher = lo_regex->create_matcher( text  = description ).
        rv_correct = lo_matcher->find_next( ).
      CATCH cx_sy_regex cx_sy_matcher .
        RETURN.
    ENDTRY.
  ENDIF.
ENDMETHOD.
ENDCLASS.
