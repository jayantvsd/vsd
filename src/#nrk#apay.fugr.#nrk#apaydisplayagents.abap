FUNCTION /nrk/apaydisplayagents.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYNO
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: BEGIN OF i_wiagents OCCURS 0.
          INCLUDE STRUCTURE swhactor.
  DATA: END OF i_wiagents.

  DATA: BEGIN OF i_subs OCCURS 0.
          INCLUDE STRUCTURE struc.
  DATA: END OF i_subs.

  DATA: BEGIN OF i_wisubs OCCURS 0.
          INCLUDE STRUCTURE swhactor.
  DATA: END OF i_wisubs.

  DATA: user_list LIKE swhactor OCCURS 0 WITH HEADER LINE,
        fullnames TYPE addr3_val-name_text OCCURS 0 WITH HEADER LINE,
        user_address     TYPE addr3_val.

  SELECT SINGLE workflowid FROM /nrk/apayhd INTO wi_id
    WHERE apayno EQ apayno.

  IF wi_id IS INITIAL.
    EXIT.
  ENDIF.

* Get workflow steps
  PERFORM get_wf_steps.

  DELETE ADJACENT DUPLICATES FROM g_swwwihead
    COMPARING wi_id.
  DESCRIBE TABLE g_swwwihead LINES wi_counter.

  IF wi_counter > 1.
    READ TABLE g_swwwihead INDEX wi_counter.
  ELSE.
    READ TABLE g_swwwihead INDEX 1.
  ENDIF.

  CLEAR: i_wiagents[].

  IF wi_counter NE 0.

    CALL FUNCTION 'RH_WI_ORGTASK_READ'
      EXPORTING
        wi_id       = g_swwwihead-wi_id
      TABLES
        wi_agents   = i_wiagents
      EXCEPTIONS
        read_failed = 1
        OTHERS      = 2.

    LOOP AT i_wiagents WHERE otype <> 'US'.

      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = i_wiagents-otype
          act_objid      = i_wiagents-objid
          act_wegid      = 'WF_ORGUS'
        TABLES
          result_tab     = user_list
        EXCEPTIONS
          no_plvar_found = 1
          no_entry_found = 2
          OTHERS         = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF NOT user_list IS INITIAL.
        LOOP AT user_list.
          MOVE user_list TO i_wiagents.
          APPEND i_wiagents.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    DELETE i_wiagents WHERE otype <> 'US'.

    SORT i_wiagents BY objid.

    DELETE ADJACENT DUPLICATES FROM i_wiagents.

  ENDIF.

  REFRESH: i_subs[],
           i_wisubs[],
           i_wisubs.

  LOOP AT i_wiagents.

    CALL FUNCTION 'RH_SUBSTITUTES_GET'
      EXPORTING
        act_plvar           = '01'
        act_otype           = i_wiagents-otype
        act_objid           = i_wiagents-objid
*       ACT_BEGDA           = SY-DATUM
*       ACT_ENDDA           = '99991231'
*       SHOW_HOLDER_FLAG    = 'X'
*       AUTHORITY_CHECK     = 'X'
      TABLES
        subst_str           = i_subs
      EXCEPTIONS
        no_substitute_found = 1
        OTHERS              = 2.
    IF sy-subrc =  0.
      LOOP AT i_subs WHERE otype = 'US'.
        i_wisubs-objid = i_subs-objid.
        i_wisubs-otype = i_subs-otype.
        APPEND i_wisubs.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

  LOOP AT i_wisubs.
    i_wiagents-objid = i_wisubs-objid.
    i_wiagents-otype = i_wisubs-otype.
    APPEND i_wiagents.
  ENDLOOP.

  SORT i_wiagents BY objid.

  DELETE ADJACENT DUPLICATES FROM i_wiagents.

  LOOP AT i_wiagents.
    CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
      EXPORTING
        user_name                    = i_wiagents-objid
*   READ_DB_DIRECTLY             = ' '
      IMPORTING
        user_address                 = user_address
*   USER_USR03                   =
     EXCEPTIONS
       user_address_not_found       = 1
       OTHERS                       = 2.

    fullnames = user_address-name_text.
    APPEND fullnames.

  ENDLOOP.

  CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY_OK'
    EXPORTING
      endpos_col         = 40
      endpos_row         = 10
      startpos_col       = 1
      startpos_row       = 1
      titletext          = 'Current agents of work item'
* IMPORTING
*   CHOISE             =
    TABLES
      valuetab           = fullnames
 EXCEPTIONS
   break_off          = 1
   OTHERS             = 2.

ENDFUNCTION.
