FUNCTION /nrk/apayapi_forward.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYHD) LIKE  /NRK/APAYAPI_HD STRUCTURE  /NRK/APAYAPI_HD
*"       OPTIONAL
*"     VALUE(COMMENT) TYPE  /NRK/APAYCOMMENT OPTIONAL
*"     VALUE(EXTUSER) TYPE  /NRK/APAYEXUSER OPTIONAL
*"     VALUE(ORIGEXTUSER) TYPE  /NRK/APAYEXUSER OPTIONAL
*"     VALUE(SEND_SAP_EMAIL) TYPE  BOOLE OPTIONAL
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"      ITEMS STRUCTURE  /NRK/APAYITEMS OPTIONAL
*"      SAP_EMAIL_TEXT STRUCTURE  /NRK/APAYAPI_TEXT OPTIONAL
*"----------------------------------------------------------------------

  DATA: wa_apayhd       LIKE /nrk/apayhd,
        wa_approverlist LIKE /nrk/apayapistoreappr,
        parked          LIKE boole-boole,
        bdcmsgcoll      LIKE bdcmsgcoll OCCURS 0,
        receiver        TYPE soud-usrnam,
        ext_receiver    LIKE /nrk/apayuser-extuser,
        value1          TYPE /nrk/apayconfig-val1,
        senderemail     LIKE  soos1-recextnam,
        t_msg           TYPE line OCCURS 0,
        tlines          TYPE i,
        item            LIKE /nrk/apayhis-item,
        status          LIKE /nrk/apayhis-status,
        t_his           LIKE /nrk/apayhis OCCURS 1,
        wa_his          LIKE /nrk/apayhis.

* origextuser is current approver
* extuser is new approver


* get APay Center record
  SELECT SINGLE * FROM /nrk/apayhd INTO wa_apayhd WHERE apayno = apayhd-apayno.

* add function to update APay Center approver
  wa_approverlist-apayno = wa_apayhd-apayno.
  wa_approverlist-extuser_in = origextuser.
  wa_approverlist-extuser_out = extuser.

  TRANSLATE wa_approverlist-extuser_in TO UPPER CASE.
  TRANSLATE wa_approverlist-extuser_out TO UPPER CASE.

  CALL FUNCTION '/NRK/APAYAPI_BAPI_STORE_APPR'
    EXPORTING
      approver = wa_approverlist
    TABLES
      messages = messages.

  READ TABLE messages WITH KEY msg_type = 'E'.

  IF sy-subrc EQ 0.
    IF sy-subrc EQ 0.
      messages-msg_type = 'E'.
      messages-msg_nbr = '045'.
      messages-msg_text = text-045.
      APPEND messages.
      EXIT.
    ENDIF.
  ENDIF.

* store line item data in APay Center
  IF NOT items[] IS INITIAL.

    CALL FUNCTION '/NRK/APAYSTORECODING'
      EXPORTING
        apayhd                   = wa_apayhd
      IMPORTING
        parked                   = parked
      TABLES
        items                    = items
        bdcmsgcoll               = bdcmsgcoll
      EXCEPTIONS
        parking_failed           = 1
        coding_completed         = 2
        line_item_amount_missing = 3
        company_code_missing     = 4
        OTHERS                   = 5.

    IF sy-subrc NE 0.
      IF sy-subrc EQ 2.
        messages-msg_type = 'I'.
        messages-msg_nbr = '034'.
        messages-msg_text = text-034.
        APPEND messages.
      ELSEIF sy-subrc EQ 3.
        messages-msg_type = 'E'.
        messages-msg_nbr = '038'.
        messages-msg_text = text-038.
        APPEND messages.
        EXIT.
      ELSEIF sy-subrc EQ 4.
        messages-msg_type = 'E'.
        messages-msg_nbr = '042'.
        messages-msg_text = text-042.
        APPEND messages.
        EXIT.
      ELSE.
        messages-msg_type = 'E'.
        messages-msg_nbr = '031'.
        messages-msg_text = text-031.
        APPEND messages.
        EXIT.
      ENDIF.
    ENDIF.

  ENDIF.

** Change approver in APay Center to recipient
*  wa_approverlist-apayno = wa_apayhd-apayno.
*  wa_approverlist-extuser_in = origextuser.
*  wa_approverlist-extuser_out = extuser.
*
*  CALL FUNCTION '/NRK/APAYAPI_BAPI_STORE_APPR'
*    EXPORTING
*      approver = wa_approverlist
*    TABLES
*      messages = messages.
*
*  READ TABLE messages WITH KEY msg_type = 'E'.
*
*  IF sy-subrc EQ 0.
*    messages-msg_type = 'E'.
*    messages-msg_nbr = '045'.
*    messages-msg_text = text-045.
*    APPEND messages.
*    EXIT.
*  ENDIF.

* Send email to new recipient
  IF send_sap_email EQ 'X'.

*   receiver = extuser.
    IF NOT extuser IS INITIAL.
      ext_receiver = extuser.

    ENDIF.

    SELECT SINGLE  val1 FROM /nrk/apayconfig INTO value1
      WHERE key1 EQ 'APAYSP'
        AND key2 EQ 'SENDER'.

    IF sy-subrc NE 0. " error
      messages-msg_type = 'E'.
      messages-msg_nbr = '040'.
      messages-msg_text = text-040.
      APPEND messages.
      EXIT.
    ENDIF.

    MOVE value1 TO senderemail.
    MOVE sap_email_text[] TO t_msg[].

    CALL FUNCTION '/NRK/APAYSENDEMAIL_ENH'
      EXPORTING
        apayno         = wa_apayhd-apayno
        senderemail    = senderemail
        ext_receiver   = ext_receiver
        mail_subject   = 'Invoice forwarded for approval'
        send_now       = 'X'
      TABLES
        msg            = t_msg
      EXCEPTIONS
        invalid_record = 1
        invalid_userid = 2
        invalid_email  = 3
        msg_error      = 4
        url_error      = 5
        send_error     = 6
        error          = 7
        OTHERS         = 8.

    IF sy-subrc <> 0.
      messages-msg_type = 'W'.
      messages-msg_nbr = '037'.
      messages-msg_text = text-037.
      APPEND messages.
    ENDIF.

  ENDIF.

* Update APay Center header with forwarding status
  SELECT SINGLE  val1 FROM /nrk/apayconfig INTO value1
    WHERE key1 EQ 'APAYSP'
      AND key2 EQ 'FORWSTATUS'.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '041'.
    messages-msg_text = text-041.
    APPEND messages.
    EXIT.
  ENDIF.

  MOVE value1 TO status.
  wa_apayhd-status = status.
  wa_apayhd-ext_approver = extuser.
  MODIFY /nrk/apayhd FROM wa_apayhd.

* Update APay Center history with forwarding status
  SELECT * FROM /nrk/apayhis INTO TABLE t_his
     WHERE apayno = apayhd-apayno.

  DESCRIBE TABLE t_his LINES tlines.
  item = tlines + 1.

  wa_his-apayno = wa_apayhd-apayno.
  wa_his-item   = item.
  wa_his-status = status.
  wa_his-sdate  = sy-datum.
  wa_his-stime  = sy-uzeit.

  SELECT SINGLE fullname FROM /nrk/apayuser INTO wa_his-sname
    WHERE extuser EQ origextuser.

  INSERT /nrk/apayhis FROM wa_his.

* Set message to frowarded
  messages-msg_type = 'I'.
  messages-msg_nbr = '030'.
  messages-msg_text = text-030.
  APPEND messages.


ENDFUNCTION.
