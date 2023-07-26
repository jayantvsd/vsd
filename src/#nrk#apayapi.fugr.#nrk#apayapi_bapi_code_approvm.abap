FUNCTION /nrk/apayapi_bapi_code_approvm .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYHD) LIKE  /NRK/APAYAPI_HD STRUCTURE  /NRK/APAYAPI_HD
*"         OPTIONAL
*"     VALUE(CODED) LIKE  BOOLE STRUCTURE  BOOLE OPTIONAL
*"     VALUE(APPROVED) LIKE  BOOLE STRUCTURE  BOOLE OPTIONAL
*"     VALUE(REJECTED) LIKE  BOOLE STRUCTURE  BOOLE OPTIONAL
*"     VALUE(COMMENT) TYPE  /NRK/APAYCOMMENT OPTIONAL
*"     VALUE(EXTUSER) TYPE  /NRK/APAYEXUSER OPTIONAL
*"     VALUE(FORWARDED) TYPE  BOOLE OPTIONAL
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"      ITEMS STRUCTURE  /NRK/APAYITEMS OPTIONAL
*"--------------------------------------------------------------------

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

  DATA: lt_existing_t  TYPE /nrk/apayitems OCCURS 0,
        ls_existing_t  TYPE /nrk/apayitems,
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
        lv_buzeimax     TYPE buzei.

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
  IF forwarded EQ 'X' AND NOT items[] IS INITIAL.
* Check coding data already in SAP
    CLEAR: lt_existing_t.
* Check if there are existing entries in coding table
    SELECT * FROM /nrk/apayitems INTO TABLE lt_existing_t WHERE apayno EQ apayhd-apayno.
    IF sy-subrc NE 0.
* There are no entries in coding table yet, just add new entries
      CALL FUNCTION '/NRK/APAYSTORECODING'
        EXPORTING
          apayhd         = l_apayhd
        IMPORTING
          parked         = parked
        TABLES
          items          = items
          bdcmsgcoll     = bdcmsgcoll
        EXCEPTIONS
          parking_failed = 1
          OTHERS         = 2.

      IF sy-subrc NE 0.
        messages-msg_type = 'E'.
        messages-msg_nbr = '031'.
        messages-msg_text = text-031.
        APPEND messages.
        EXIT.
      ENDIF.
* Set return message
      messages-msg_type = 'I'.
      messages-msg_nbr = '030'.
      messages-msg_text = text-030.
      APPEND messages.
      EXIT.
    ENDIF.
  ELSE.
* There are entries in coding table, run check routine
    LOOP AT lt_existing_t INTO ls_existing_t.
* Check for line
      READ TABLE items INTO ls_items_t WITH KEY buzei = ls_existing_t-buzei.
      IF sy-subrc EQ 0.
* Overwrite fields
        ls_existing_t-bukrs = ls_items_t-bukrs.
        ls_existing_t-hkont = ls_items_t-hkont.
        ls_existing_t-kostl = ls_items_t-kostl.
        ls_existing_t-matnr = ls_items_t-matnr.
        ls_existing_t-menge = ls_items_t-menge.
        ls_existing_t-projk = ls_items_t-projk.

        IF ls_items_t-shkzg = 'C'.
          ls_existing_t-shkzg = 'H'.
        ELSEIF ls_items_t-shkzg = 'D'.
          ls_existing_t-shkzg = 'S'.
        ENDIF.

        ls_existing_t-sgtxt = ls_items_t-sgtxt.
        ls_existing_t-wrbtr = ls_items_t-wrbtr.
        ls_existing_t-aufnr = ls_items_t-aufnr.
        INSERT ls_existing_t INTO lt_existing_t INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

* Now add new lines that are coming in from Sharepoint, if any
    SORT items BY buzei DESCENDING.
* Get max BUZEI
    READ TABLE items INTO ls_items_t INDEX 1.
    CLEAR: ls_items_t.
* Update BUZEI with new records
    LOOP AT items INTO ls_items_t WHERE buzei EQ lv_buzeimax.
      lv_buzeimax = lv_buzeimax + 1.
      ls_items_t-buzei = lv_buzeimax.
      APPEND ls_items_t TO lt_existing_t.
    ENDLOOP.

* Now write to existing items table
    CALL FUNCTION '/NRK/APAYSTORECODING'
      EXPORTING
        apayhd         = l_apayhd
      IMPORTING
        parked         = parked
      TABLES
        items          = lt_existing_t
        bdcmsgcoll     = bdcmsgcoll
      EXCEPTIONS
        parking_failed = 1
        OTHERS         = 2.

    IF sy-subrc NE 0.
      messages-msg_type = 'E'.
      messages-msg_nbr = '031'.
      messages-msg_text = text-031.
      APPEND messages.
      EXIT.
    ENDIF.
* Set return message
    messages-msg_type = 'I'.
    messages-msg_nbr = '030'.
    messages-msg_text = text-030.
    APPEND messages.
    EXIT.
  ENDIF.

* Check coding data already in SAP
  SELECT * FROM /nrk/apayitems INTO TABLE lt_existing_t WHERE apayno EQ apayhd-apayno.
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
  LOOP AT lt_existing_t INTO ls_existing_t.
    lv_tabix = sy-tabix.
    LOOP AT items INTO ls_items_t.

      IF ls_items_t-shkzg = 'C'.
        ls_items_t-shkzg = 'H'.
      ELSEIF ls_items_t-shkzg = 'D'.
        ls_items_t-shkzg = 'S'.
      ENDIF.

      IF   ls_existing_t-kostl NE ls_items_t-kostl
        OR ls_existing_t-hkont NE ls_items_t-hkont
        OR ls_existing_t-matnr NE ls_items_t-matnr
        OR ls_existing_t-bukrs NE ls_items_t-bukrs
        OR ls_existing_t-sgtxt NE ls_items_t-sgtxt
        OR ls_existing_t-wrbtr NE ls_items_t-wrbtr
        OR ls_existing_t-shkzg NE ls_items_t-shkzg
        OR ls_existing_t-projk NE ls_items_t-projk
        OR ls_existing_t-zuonr NE ls_items_t-zuonr
        OR ls_existing_t-aufnr NE ls_items_t-aufnr.

* line item data changed
      ELSE.
* No changes in line item
        DELETE lt_existing_t INDEX lv_tabix.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  DESCRIBE TABLE lt_existing_t LINES lines.
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

* Wenn lt_existing_t leer ist, dann ist alles unveraendert => No change to parked document

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
*          messages-msg_type = 'E'.       "BUEHLER - Make this a warning
          messages-msg_type = 'W'.
          messages-msg_nbr = '027'.
          messages-msg_text = text-027.
          APPEND messages.
*          exit.
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
            OTHERS                      = 3.

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
    TRANSLATE extuser TO UPPER CASE.
    SELECT * FROM /nrk/apayappr INTO TABLE t_approver
      WHERE apayno EQ l_apayhd-apayno
        AND ext_user EQ extuser
        AND approved EQ space.

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
    SELECT * FROM /nrk/apayappr INTO TABLE t_approver
      WHERE apayno EQ l_apayhd-apayno
        AND ext_user EQ extuser.

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
