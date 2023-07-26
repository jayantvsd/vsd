FUNCTION /nrk/apay_update_status.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"     REFERENCE(STATUS) TYPE  /NRK/APAYHIS-STATUS
*"     REFERENCE(DATE) TYPE  /NRK/APAYHIS-SDATE
*"     REFERENCE(TIME) TYPE  /NRK/APAYHIS-STIME
*"     REFERENCE(USER) TYPE  XUBNAME
*"     REFERENCE(USER_EXT) TYPE  /NRK/APAYHIS-SEXTUSER OPTIONAL
*"  EXCEPTIONS
*"      UPDATE_FAILED
*"----------------------------------------------------------------------

  DATA: wa_hd  LIKE /nrk/apayhd,
        wa_his LIKE /nrk/apayhis,
        t_his  LIKE /nrk/apayhis OCCURS 0,
        lines  TYPE i.

  CLEAR: wa_hd,
         t_his[],
         wa_his,
         lines.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE update_failed.
  ENDIF.

  wa_hd-status = status.

  IF wa_hd-cdate IS INITIAL.
    wa_hd-cdate = date.
  ENDIF.

  IF wa_hd-ctime IS INITIAL.
    wa_hd-ctime = time.
  ENDIF.

  wa_hd-lastchange = sy-datum.

* MODIFY /nrk/apayhd FROM wa_hd.

* update history
  SELECT * FROM /nrk/apayhis INTO TABLE t_his
    WHERE apayno EQ apayno.

  DESCRIBE TABLE t_his LINES lines.

  lines = lines + 1.

  wa_his-apayno = apayno.
  wa_his-item = lines.
  wa_his-status = status.
  wa_his-sdate = date.
  wa_his-stime = time.
  wa_his-suser = user.
* wa_his-SNAME
* wa_his-sextuser = externaluserid.

  SELECT SINGLE fullname FROM /nrk/apayuser INTO wa_his-sname
    WHERE objid EQ user.

  INSERT /nrk/apayhis FROM wa_his.

* Update last change
  wa_hd-lastchange = date.
  wa_hd-lastchanget = time.

* Modify header row
  MODIFY /nrk/apayhd FROM wa_hd.

ENDFUNCTION.
