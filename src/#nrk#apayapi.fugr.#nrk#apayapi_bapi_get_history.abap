FUNCTION /nrk/apayapi_bapi_get_history.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYNO) TYPE  /NRK/APAYHD-APAYNO OPTIONAL
*"  TABLES
*"      HISTORY STRUCTURE  /NRK/APAYHIS_DIS OPTIONAL
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"----------------------------------------------------------------------

  DATA: t_his  LIKE /nrk/apayhis OCCURS 0,
        wa_his LIKE /nrk/apayhis,
        sdescr LIKE /nrk/apaysdef-sdescr.

  SELECT * FROM /nrk/apayhis INTO TABLE t_his
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '008'.
    messages-msg_text = text-008.
    APPEND messages.
    EXIT.
  ENDIF.

  LOOP AT t_his INTO wa_his.

    history-item = wa_his-item.
    history-fullname = wa_his-sname.
    history-sdate = wa_his-sdate.
    history-stime = wa_his-stime.

    SELECT SINGLE sdescr FROM /nrk/apaysdef INTO sdescr
      WHERE status EQ wa_his-status
      AND langu  EQ sy-langu.

    history-sdescr = sdescr.
    APPEND history.

  ENDLOOP.

ENDFUNCTION.
