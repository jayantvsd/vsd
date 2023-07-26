FUNCTION /nrk/apayapi_bapi_code_approve.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYHD) LIKE  /NRK/APAYAPI_HD STRUCTURE  /NRK/APAYAPI_HD
*"       OPTIONAL
*"     VALUE(CODED) LIKE  BOOLE STRUCTURE  BOOLE OPTIONAL
*"     VALUE(APPROVED) LIKE  BOOLE STRUCTURE  BOOLE OPTIONAL
*"     VALUE(REJECTED) LIKE  BOOLE STRUCTURE  BOOLE OPTIONAL
*"     VALUE(COMMENT) TYPE  /NRK/APAYCOMMENT OPTIONAL
*"     VALUE(EXTUSER) TYPE  /NRK/APAYEXUSER OPTIONAL
*"     VALUE(ORIGEXTUSER) TYPE  /NRK/APAYEXUSER OPTIONAL
*"     VALUE(FORWARDED) TYPE  BOOLE OPTIONAL
*"     VALUE(SEND_SAP_EMAIL) TYPE  BOOLE OPTIONAL
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"      ITEMS STRUCTURE  /NRK/APAYITEMS OPTIONAL
*"      SAP_EMAIL_TEXT STRUCTURE  /NRK/APAYAPI_TEXT OPTIONAL
*"----------------------------------------------------------------------

* ORIGEXTUSER eq initial -> legacy mode
* ORIGEXTUSER ne initial -> Nintex mode

  INCLUDE <cntn01>.

  DATA: parked          LIKE boole-boole,
        acgl_item       LIKE acgl_item OCCURS 0,
        bdcmsgcoll      LIKE bdcmsgcoll OCCURS 0,
        l_apayhd        LIKE /nrk/apayhd,
        l_objectkey     LIKE  swr_struct-object_key,
        input_container TYPE TABLE OF swr_cont,
        wa_container    LIKE LINE OF input_container,
        t_approver      LIKE /nrk/apayappr OCCURS 0,
        wa_approver     LIKE /nrk/apayappr,
        msg_approve     LIKE /nrk/apayapi_messages OCCURS 0.

  DATA: lt_apayitems_t  TYPE /nrk/apayitems OCCURS 0,
        ls_apayitems_t  TYPE /nrk/apayitems,
        ls_items_t      TYPE /nrk/apayitems,
        lv_tabix        LIKE sy-tabix,
        lines(6)        TYPE n,
        parkflag        LIKE boole-boole,
        belnr           LIKE /nrk/apayhd-belnr,
        bukrs           LIKE /nrk/apayhd-bukrs,
        gjahr           LIKE /nrk/apayhd-gjahr,
        items_total     LIKE /nrk/apayhd-wrbtr,
        wa_bdcmsgcoll   TYPE bdcmsgcoll,
        changed         LIKE boole-boole,
        msg_txt(255)    TYPE c,
        rstatus         LIKE /nrk/apayhd-status,
        approvers       LIKE /nrk/apayapistoreappr,
        t_msg           TYPE line OCCURS 0.

  DATA: reciever    TYPE soud-usrnam,
        senderemail LIKE  soos1-recextnam,
        value1      TYPE /nrk/apayconfig-val1,
        t_his       LIKE /nrk/apayhis OCCURS 1,
        wa_his      LIKE /nrk/apayhis,
        l_tlines    TYPE i,
        l_item      LIKE /nrk/apayhis-item,
        status      LIKE /nrk/apayhis-status,
        uname       LIKE /nrk/apayhd-uname.

  DATA: wa_approverlist LIKE /nrk/apayapistoreappr.

  DATA: l_dtype LIKE /nrk/apaydtype,
        l_lines TYPE i.

* Temporary select
  SELECT SINGLE * FROM /nrk/apayhd INTO l_apayhd WHERE apayno = apayhd-apayno.

* Check existing items
  IF coded EQ 'X'.
* Check for items in parked document
    SELECT SINGLE * FROM vbsegs WHERE ausbk = l_apayhd-bukrs AND belnr = l_apayhd-belnr AND gjahr = l_apayhd-gjahr.
    IF sy-subrc EQ 0.
* Items are already coded, do not change anything
      CLEAR coded.
    ENDIF.
  ENDIF.

* Add header to structure
  l_apayhd-apayno = apayhd-apayno.
  l_apayhd-lifnr  = apayhd-lifnr.
  l_apayhd-xblnr  = apayhd-xblnr.
  l_apayhd-bldat  = apayhd-bldat.
  l_apayhd-wrbtr  = apayhd-wrbtr.
  l_apayhd-waers  = apayhd-waers.
  l_apayhd-budat  = apayhd-budat.
  l_apayhd-bukrs  = apayhd-bukrs.
  l_apayhd-wmwst  = apayhd-wmwst.
  l_apayhd-status = apayhd-status.

  IF rejected EQ 'X'.
    rstatus = apayhd-status.
  ENDIF.

* Forwarding only
  IF forwarded EQ 'X'. "work items forwarded


    IF NOT items[] IS INITIAL.
      CALL FUNCTION '/NRK/APAYSTORECODING'
        EXPORTING
          apayhd                   = l_apayhd
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

* exit, if coding only
*    IF NOT origextuser IS INITIAL.
*      EXIT.
*    ENDIF.

* check for notification email
    IF send_sap_email EQ 'X'.
      reciever = extuser.

* send email to new approver

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
          apayno                  = l_apayhd-apayno
*         ARCHIV_ID               =
*         ARC_DOC_ID              =
          senderemail             = senderemail
*         SENDER                  = 'WF-BATCH'
         receiver                = reciever
*          receiver_email          = EXTUSER
*         RECEIVER_CC_EMAIL       =
          mail_subject            = 'Invoice forwarded for approval'
          send_now                = 'X'
        TABLES
          msg                     = t_msg
*         DOCS                    =
        EXCEPTIONS
          invalid_record          = 1
          invalid_userid          = 2
          invalid_email           = 3
          msg_error               = 4
          url_error               = 5
          send_error              = 6
          error                   = 7
          OTHERS                  = 8.

      IF sy-subrc <> 0.
        messages-msg_type = 'W'.
        messages-msg_nbr = '037'.
        messages-msg_text = text-037.
        APPEND messages.
*       EXIT.
      ENDIF.

** APay Status history update
*      CLEAR: l_item,
*             l_tlines.
*
** Get forwarding status
*      SELECT SINGLE  val1 FROM /nrk/apayconfig INTO value1
*        WHERE key1 EQ 'APAYSP'
*          AND key2 EQ 'FORWSTATUS'.
*
*      IF sy-subrc NE 0.
*        messages-msg_type = 'E'.
*        messages-msg_nbr = '041'.
*        messages-msg_text = text-041.
*        APPEND messages.
*        EXIT.
*      ENDIF.
*
*      MOVE value1 TO status.

**** start change 10/09/2015
** change old user with new user
*      wa_approverlist-apayno = l_apayhd-apayno.
*      wa_approverlist-extuser_in = origextuser.
*      wa_approverlist-extuser_out = extuser.
*
*      CALL FUNCTION '/NRK/APAYAPI_BAPI_STORE_APPR'
*        EXPORTING
*          approver = wa_approverlist
*        TABLES
*          messages = messages.

** Get history
*      SELECT * FROM /nrk/apayhis INTO TABLE t_his
*         WHERE apayno = apayhd-apayno.
*
*      DESCRIBE TABLE t_his LINES l_tlines.
*      l_item = l_tlines + 1.
*
*      wa_his-apayno = apayhd-apayno.
*      wa_his-item   = l_item.
*      wa_his-status = status.
*      wa_his-sdate  = sy-datum.
*      wa_his-stime  = sy-uzeit.
**     wa_his-suser  = uname.
*
*      SELECT SINGLE fullname FROM /nrk/apayuser INTO wa_his-sname
*        WHERE extuser EQ extuser.
*
*      INSERT /nrk/apayhis FROM wa_his.
**** end change 10/09/2015

    ENDIF.

*** start change 10/09/2015
* change old user with new user
    wa_approverlist-apayno = l_apayhd-apayno.
    wa_approverlist-extuser_in = origextuser.
    wa_approverlist-extuser_out = extuser.

    CALL FUNCTION '/NRK/APAYAPI_BAPI_STORE_APPR'
      EXPORTING
        approver = wa_approverlist
      TABLES
        messages = messages.

    READ TABLE messages WITH KEY msg_type = 'E'.

    IF sy-subrc EQ 0.
      EXIT.
    ELSE.

* APay Status history update
      CLEAR: l_item,
             l_tlines.

* Get forwarding status
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
      l_apayhd-status = status.

      l_apayhd-ext_approver = extuser.

* Update header row
      MODIFY /nrk/apayhd FROM l_apayhd.

* Get history
      SELECT * FROM /nrk/apayhis INTO TABLE t_his
         WHERE apayno = apayhd-apayno.

      DESCRIBE TABLE t_his LINES l_tlines.
      l_item = l_tlines + 1.

      wa_his-apayno = apayhd-apayno.
      wa_his-item   = l_item.
      wa_his-status = status.
      wa_his-sdate  = sy-datum.
      wa_his-stime  = sy-uzeit.
*     wa_his-suser  = uname.

      SELECT SINGLE fullname FROM /nrk/apayuser INTO wa_his-sname
        WHERE extuser EQ origextuser.

      INSERT /nrk/apayhis FROM wa_his.

** Set return message work item forwarded
      messages-msg_type = 'I'.
      messages-msg_nbr = '030'.
      messages-msg_text = text-030.
      APPEND messages.
      EXIT.
    ENDIF.
  ENDIF.

*** end change 10/09/2015


* Check coding data already in SAP
  SELECT * FROM /nrk/apayitems INTO TABLE lt_apayitems_t WHERE apayno EQ apayhd-apayno.
  IF sy-subrc NE 0.
* There is no coding data in SAP, if there is something in items, then change the parked document
    DESCRIBE TABLE items LINES lines.
    IF lines GT 0.
* coding data entered -> parking
      parkflag = 'X'.
    ELSE.
* no coding data entered -> no parking
      parkflag = ' '.
      parkflag = ' '.
      messages-msg_type = 'I'.
      messages-msg_nbr = '028'.
      messages-msg_text = text-028.
      APPEND messages.
    ENDIF.
  ENDIF.

* There is already coding data in SAP, check for changes, and change parked document if necessary
  LOOP AT lt_apayitems_t INTO ls_apayitems_t.
    lv_tabix = sy-tabix.
    LOOP AT items INTO ls_items_t.

      IF ls_items_t-shkzg = 'C'.
        ls_items_t-shkzg = 'H'.
      ELSEIF ls_items_t-shkzg = 'D'.
        ls_items_t-shkzg = 'S'.
      ENDIF.

      IF   ls_apayitems_t-kostl NE ls_items_t-kostl
        OR ls_apayitems_t-hkont NE ls_items_t-hkont
        OR ls_apayitems_t-matnr NE ls_items_t-matnr
        OR ls_apayitems_t-bukrs NE ls_items_t-bukrs
        OR ls_apayitems_t-sgtxt NE ls_items_t-sgtxt
        OR ls_apayitems_t-wrbtr NE ls_items_t-wrbtr
        OR ls_apayitems_t-shkzg NE ls_items_t-shkzg
        OR ls_apayitems_t-projk NE ls_items_t-projk
        OR ls_apayitems_t-zuonr NE ls_items_t-zuonr
        OR ls_apayitems_t-aufnr NE ls_items_t-aufnr.
*       OR ls_apayitems_t-pspnr NE ls_items_t-pspnr.

* line item data changed
      ELSE.
* No changes in line item
        DELETE lt_apayitems_t INDEX lv_tabix.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  DESCRIBE TABLE lt_apayitems_t LINES lines.
  IF lines GT 0.
* coding changed -> change parked document
    parkflag = 'X'.
  ELSE.
* coding did not change -> no change of parked document
    parkflag = ' '.
*    messages-msg_type = 'I'.
*    messages-msg_nbr = '021'.
*    messages-msg_text = text-023.
*    APPEND messages.
  ENDIF.

* Wenn lt_apayitems_t leer ist, dann ist alles unveraendert => No change to parked document

* Get APay header data
  SELECT SINGLE * FROM /nrk/apayhd INTO l_apayhd
    WHERE apayno EQ l_apayhd-apayno.

* set document status
  IF coded EQ 'X'
    AND l_apayhd-ebeln IS INITIAL.

    IF l_apayhd-belnr IS INITIAL
      OR l_apayhd-bukrs IS INITIAL
      OR l_apayhd-gjahr IS INITIAL.

* No document exists.
*** don't update parked document
      messages-msg_type = 'I'.
      messages-msg_nbr = '019'.
      messages-msg_text = text-019.
      APPEND messages.

    ELSE.

* Parse line items passed into BAPI
      IF NOT items[] IS INITIAL.

* Check balance
        CLEAR: items_total.
        LOOP AT items.
          IF items-shkzg = 'D'.
* if debit, add line item
            items_total = items_total + items-wrbtr.
          ELSEIF items-shkzg = 'C'.
* if credit, subtract line item
            items_total = items_total - items-wrbtr.
          ENDIF.
        ENDLOOP.

* Calculate net, if blank
        IF l_apayhd-wrbtr_net IS INITIAL.
          l_apayhd-wrbtr_net = l_apayhd-wrbtr - l_apayhd-wmwst.
        ENDIF.

* Compare net with line item total
        IF items_total NE l_apayhd-wrbtr_net.
          messages-msg_type = 'E'.       "BUEHLER - Make this a warning
*          messages-msg_type = 'W'.
          messages-msg_nbr = '027'.
          messages-msg_text = text-027.
          APPEND messages.
          EXIT.
        ENDIF.

* Change parked document
        CALL FUNCTION '/NRK/APAY_CHANGE_PARKED_DOC_V2'
          EXPORTING
            apayno                      = l_apayhd-apayno
            header                      = l_apayhd
          IMPORTING
            changed                     = changed
          TABLES
            t_items                     = items
            t_messages                  = bdcmsgcoll
          EXCEPTIONS
            no_apay_record_id           = 1
            parked_document_not_changed = 2
            lineitem_update_failed      = 3
            OTHERS                      = 4.

        IF sy-subrc NE 0.
          messages-msg_type = 'W'.
          messages-msg_nbr = '044'.
          messages-msg_text = text-044.
          APPEND messages.
        ENDIF.

        IF changed NE 'X'.

          LOOP AT bdcmsgcoll INTO wa_bdcmsgcoll.

            IF wa_bdcmsgcoll-msgtyp EQ 'E'
              OR wa_bdcmsgcoll-msgtyp EQ 'W'.

              CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                EXPORTING
                  msgid               = wa_bdcmsgcoll-msgid
                  msgnr               = wa_bdcmsgcoll-msgnr
                  msgv1               = wa_bdcmsgcoll-msgv1
                  msgv2               = wa_bdcmsgcoll-msgv2
                  msgv3               = wa_bdcmsgcoll-msgv3
                  msgv4               = wa_bdcmsgcoll-msgv4
                IMPORTING
                  message_text_output = msg_txt.

              messages-msg_type = wa_bdcmsgcoll-msgtyp.
              messages-msg_nbr = wa_bdcmsgcoll-msgnr.
              messages-msg_text = msg_txt.
              APPEND messages.

            ENDIF.
          ENDLOOP.

          messages-msg_type = 'E'.
          messages-msg_nbr = '021'.
          messages-msg_text = text-021.
          APPEND messages.
          EXIT.
        ELSE.
          messages-msg_type = 'S'.
          messages-msg_nbr = '022'.
          messages-msg_text = text-022.
          APPEND messages.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF approved EQ 'X'.

**** start change 11/12/2015

* check if line items required

    SELECT SINGLE * FROM /nrk/apaydtype INTO l_dtype
      WHERE ar_object EQ l_apayhd-ar_object.

    IF l_dtype-appr_req EQ 'X'. " line item required

* check if line items exisits.

      DESCRIBE TABLE items LINES l_lines.

      IF l_lines EQ 0. " no line items
        messages-msg_type = 'E'.
        messages-msg_nbr = '043'.
        messages-msg_text = text-043.
        APPEND messages.
        EXIT.
      ENDIF.

    ENDIF.

**** end change 11/12/2015

* add comment
    IF NOT comment IS INITIAL.
      CALL FUNCTION '/NRK/APAY_ADD_COMMENT_API'
        EXPORTING
          iv_apayno           = l_apayhd-apayno
          iv_user             = extuser
          iv_comment          = comment
        EXCEPTIONS
          user_not_registered = 1
          db_insert_failed    = 2
          OTHERS              = 3.

      IF sy-subrc <> 0.
        messages-msg_type = 'E'.
        messages-msg_nbr = '006'.
        messages-msg_text = text-006.
        APPEND messages.
        EXIT.
      ENDIF.
    ENDIF.

* Update approver table

    IF origextuser IS INITIAL.

      TRANSLATE extuser TO UPPER CASE.

      SELECT * FROM /nrk/apayappr INTO TABLE t_approver
        WHERE apayno EQ l_apayhd-apayno
          AND ext_user EQ extuser
          AND approved EQ space.

    ELSE. " original approver different

      SELECT * FROM /nrk/apayappr INTO TABLE t_approver
        WHERE apayno EQ l_apayhd-apayno
          AND ext_user EQ origextuser
          AND approved EQ space.

    ENDIF.

    LOOP AT t_approver INTO wa_approver.
      wa_approver-approved = 'A'.
      MODIFY t_approver FROM wa_approver.
    ENDLOOP.

    UPDATE /nrk/apayappr FROM TABLE t_approver.

    MOVE l_apayhd-apayno TO l_objectkey.

    CLEAR: wa_container.
    wa_container-element = 'ExternalUserID'.
    wa_container-value = extuser.
    APPEND wa_container TO input_container.

    CLEAR: wa_container.
    wa_container-element = 'STATUS'.
    wa_container-value = l_apayhd-status.
    APPEND wa_container TO input_container.

* Trigger workflow event
    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
      EXPORTING
        object_type             = '/NRK/APAY'
        object_key              = l_objectkey
        event                   = 'approved'
        commit_work             = 'X'
        event_language          = sy-langu
*       LANGUAGE                = SY-LANGU
        user                    = sy-uname
*       IFS_XML_CONTAINER       =
*     IMPORTING
*       RETURN_CODE             =
*       EVENT_ID                =
      TABLES
        input_container         = input_container.
*       MESSAGE_LINES           =
*       MESSAGE_STRUCT          =

    messages-msg_type = 'S'.
    messages-msg_nbr = '032'.
    messages-msg_text = text-032.
    APPEND messages.

  ENDIF.

  IF rejected EQ 'X'. "Invoice rejected

* add comment
    IF NOT comment IS INITIAL.

      CALL FUNCTION '/NRK/APAY_ADD_COMMENT_API'
        EXPORTING
          iv_apayno           = l_apayhd-apayno
          iv_user             = extuser
          iv_comment          = comment
        EXCEPTIONS
          user_not_registered = 1
          db_insert_failed    = 2
          OTHERS              = 3.

      IF sy-subrc <> 0.
        messages-msg_type = 'E'.
        messages-msg_nbr = '006'.
        messages-msg_text = text-006.
        APPEND messages.
        EXIT.
      ENDIF.

    ENDIF.

* Update approver table
    IF origextuser IS INITIAL.

      SELECT * FROM /nrk/apayappr INTO TABLE t_approver
        WHERE apayno EQ l_apayhd-apayno
          AND ext_user EQ extuser.

    ELSE. " original approver different

      SELECT * FROM /nrk/apayappr INTO TABLE t_approver
        WHERE apayno EQ l_apayhd-apayno
          AND ext_user EQ origextuser
          AND approved EQ space.

    ENDIF.

    LOOP AT t_approver INTO wa_approver.
      wa_approver-approved = 'R'.
      MODIFY t_approver FROM wa_approver.
    ENDLOOP.

    UPDATE /nrk/apayappr FROM TABLE t_approver.

* Trigger event
    MOVE l_apayhd-apayno TO l_objectkey.

    CLEAR: wa_container.
    wa_container-element = 'ExternalUserID'.
    wa_container-value = extuser.
    APPEND wa_container TO input_container.

    CLEAR: wa_container.
    wa_container-element = 'STATUS'.
    wa_container-value = rstatus.
    APPEND wa_container TO input_container.

    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
      EXPORTING
        object_type             = '/NRK/APAY'
        object_key              = l_objectkey
        event                   = 'rejected'
        commit_work             = 'X'
        event_language          = sy-langu
*       LANGUAGE                = SY-LANGU
        user                    = sy-uname
*       IFS_XML_CONTAINER       =
*     IMPORTING
*       RETURN_CODE             =
*       EVENT_ID                =
      TABLES
        input_container         = input_container.
*       MESSAGE_LINES           =
*       MESSAGE_STRUCT          =

    messages-msg_type = 'I'.
    messages-msg_nbr = '029'.
    messages-msg_text = text-029.
    APPEND messages.
  ENDIF.

ENDFUNCTION.
