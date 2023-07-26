FUNCTION /nrk/apayexception.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IAPAYNO) TYPE  /NRK/APAYHD-APAYNO
*"  EXPORTING
*"     REFERENCE(EEXCEPT) TYPE  /NRK/APAYHD-STATUS
*"----------------------------------------------------------------------

  DATA: status       LIKE /nrk/apayhd-status,
        fullname     LIKE /nrk/apayuser-fullname,
        wa_exception LIKE /nrk/apayedef,
        wa_comment   LIKE /nrk/apaycmnt.

  CLEAR: extype_value,
         extype_list,
         wa_exception,
         wa_comment,
         comment,
         except.

  REFRESH: extype_list, extype_list[].

  SELECT SINGLE status FROM /nrk/apayhd INTO status
    WHERE apayno EQ iapayno.

  SELECT * FROM /nrk/apayedef INTO wa_exception
    WHERE pstatus EQ status
      AND langu EQ sy-langu.

    extype_value-key = wa_exception-status.
    extype_value-text = wa_exception-sdescr.
    APPEND extype_value TO extype_list.

  ENDSELECT.

  MOVE iapayno TO apayno.

  CALL SCREEN 902 STARTING AT 10 5 ENDING AT 70 8.

  MOVE except TO eexcept.

* Store comment
  IF NOT comment IS INITIAL.
    wa_comment-mandt = sy-mandt.
    wa_comment-apayno = apayno.
    wa_comment-crea_date = sy-datum.
    wa_comment-crea_time = sy-uzeit.
    wa_comment-user_id   = sy-uname.
    wa_comment-process_comment = comment.

    SELECT SINGLE fullname FROM /nrk/apayuser INTO fullname
      WHERE objid EQ sy-uname.

    MOVE fullname TO wa_comment-user_name.

    INSERT /nrk/apaycmnt FROM wa_comment.
  ENDIF.

ENDFUNCTION.
