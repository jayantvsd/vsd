FUNCTION /nrk/apay_display_history.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"  EXCEPTIONS
*"      NO_HISTORY_FOUND
*"----------------------------------------------------------------------

  DATA: t_his  TYPE /nrk/apayhis OCCURS 0,
        wa_his TYPE /nrk/apayhis.

  CLEAR: t_his[],
         wa_his,
         t_history[],
         t_history.

  SELECT * FROM /nrk/apayhis INTO TABLE t_his
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE no_history_found.
  ENDIF.

  SORT t_his BY item ASCENDING.

  LOOP AT t_his INTO wa_his.
    t_history-item = wa_his-item.
    t_history-fullname = wa_his-sname.
    t_history-sdate = wa_his-sdate.
    t_history-stime = wa_his-stime.

    SELECT SINGLE sdescr FROM /nrk/apaysdef INTO t_history-sdescr
      WHERE status EQ wa_his-status
        AND langu  EQ sy-langu.

    APPEND t_history.

  ENDLOOP.

  CALL SCREEN 0901 STARTING AT 10 5 ENDING AT 123 21.


ENDFUNCTION.
