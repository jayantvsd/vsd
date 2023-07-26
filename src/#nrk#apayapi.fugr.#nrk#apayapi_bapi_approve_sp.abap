FUNCTION /nrk/apayapi_bapi_approve_sp.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYHD) LIKE  /NRK/APAYAPI_HD STRUCTURE  /NRK/APAYAPI_HD
*"       OPTIONAL
*"     VALUE(APPROVED) LIKE  BOOLE STRUCTURE  BOOLE OPTIONAL
*"     VALUE(REJECTED) LIKE  BOOLE STRUCTURE  BOOLE OPTIONAL
*"     VALUE(COMMENT) TYPE  /NRK/APAYCOMMENT OPTIONAL
*"     VALUE(EXTUSER) TYPE  /NRK/APAYEXUSER OPTIONAL
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"      ITEMS STRUCTURE  /NRK/APAYITEMS OPTIONAL
*"----------------------------------------------------------------------

  INCLUDE <cntn01>.

  DATA: wa_apayhd       LIKE /nrk/apayhd,
        dtype           LIKE /nrk/apaydtype,
        lines           TYPE i,
        t_approver      LIKE /nrk/apayappr OCCURS 0,
        wa_approver     LIKE /nrk/apayappr,
        input_container TYPE TABLE OF swr_cont,
        wa_container    LIKE LINE OF input_container,
        objectkey       LIKE swr_struct-object_key,
        t_apayitems     TYPE /nrk/apayitems OCCURS 0,
        wa_apayitems    TYPE /nrk/apayitems,
        wa_items        TYPE /nrk/apayitems,
        tabix           LIKE sy-tabix,
        items_total     LIKE /nrk/apayhd-wrbtr,
        net             LIKE /nrk/apayhd-wrbtr,
        changed         LIKE boole-boole,
*       bdcmsgcoll      LIKE bdcmsgcoll OCCURS 0,
        bdcmsgcoll      TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
        parked          LIKE boole-boole,
        msg_text        LIKE bapiret2-message,
        l_msgnr         LIKE bapiret2-number,
        msg_v1  LIKE  bapiret2-message_v1,
        msg_v2  LIKE  bapiret2-message_v2,
        msg_v3  LIKE  bapiret2-message_v3,
        msg_v4  LIKE  bapiret2-message_v4,
        l_apprchk LIKE /nrk/apayconfig-val1.


* get APay Center record
  SELECT SINGLE * FROM /nrk/apayhd INTO wa_apayhd WHERE apayno = apayhd-apayno.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '002'.
    messages-msg_text = text-002.
    APPEND messages.
    EXIT.
  ENDIF.

*** Start Change 04/30/2019
  CLEAR: l_apprchk.

  SELECT SINGLE val1 FROM /nrk/apayconfig INTO l_apprchk
    WHERE key1 EQ 'APAYSP'
      AND key2 EQ 'APPRCHKOFF'.

  IF l_apprchk IS INITIAL.

    TRANSLATE extuser TO UPPER CASE.

* Check APay Center approver authorization
    SELECT * FROM /nrk/apayappr INTO TABLE t_approver
      WHERE apayno EQ wa_apayhd-apayno
        AND ext_user EQ extuser
        AND approved EQ space.

    IF sy-subrc NE 0.
      messages-msg_type = 'E'.
      messages-msg_nbr = '046'.
      messages-msg_text = text-046.
      APPEND messages.
      EXIT.
    ENDIF.

  ELSE.

    TRANSLATE extuser TO UPPER CASE.

* Check APay Center approver authorization
    SELECT * FROM /nrk/apayappr INTO TABLE t_approver
      WHERE apayno EQ wa_apayhd-apayno
        AND approved EQ space.

    IF sy-subrc NE 0.
      messages-msg_type = 'E'.
      messages-msg_nbr = '046'.
      messages-msg_text = text-046.
      APPEND messages.
      EXIT.
    ENDIF.

  ENDIF.

*** End Change 04/30/2019

* Process invoice approval
  IF approved EQ 'X'.

* check, if coding if required
    SELECT SINGLE * FROM /nrk/apaydtype INTO dtype
      WHERE ar_object EQ wa_apayhd-ar_object.

    IF dtype-appr_req EQ 'X'. " line item required
      DESCRIBE TABLE items LINES lines.

      IF lines EQ 0. " no line items
        messages-msg_type = 'E'.
        messages-msg_nbr = '043'.
        messages-msg_text = text-043.
        APPEND messages.
        EXIT.
      ENDIF.
    ENDIF.

* Check parked document for line items
    SELECT SINGLE * FROM vbsegs
      WHERE ausbk = wa_apayhd-bukrs
        AND belnr = wa_apayhd-belnr
        AND gjahr = wa_apayhd-gjahr.
    IF sy-subrc EQ 0.
* Items are already coded, do not change anything
* check, if coding changed
      SELECT * FROM /nrk/apayitems INTO TABLE t_apayitems
        WHERE apayno EQ apayhd-apayno.

      LOOP AT t_apayitems INTO wa_apayitems.
        tabix = sy-tabix.
        LOOP AT items INTO wa_items.

          IF wa_items-shkzg = 'C'.
            wa_items-shkzg = 'H'.
          ELSEIF wa_items-shkzg = 'D'.
            wa_items-shkzg = 'S'.
          ENDIF.

          IF   wa_apayitems-kostl NE wa_items-kostl
            OR wa_apayitems-hkont NE wa_items-hkont
            OR wa_apayitems-matnr NE wa_items-matnr
            OR wa_apayitems-bukrs NE wa_items-bukrs
            OR wa_apayitems-sgtxt NE wa_items-sgtxt
            OR wa_apayitems-wrbtr NE wa_items-wrbtr
            OR wa_apayitems-shkzg NE wa_items-shkzg
            OR wa_apayitems-projk NE wa_items-projk
            OR wa_apayitems-zuonr NE wa_items-zuonr
            OR wa_apayitems-aufnr NE wa_items-aufnr.
* line item data changed
          ELSE.
* No changes in line item
            DELETE t_apayitems INDEX tabix.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      DESCRIBE TABLE t_apayitems LINES lines.

      IF lines GT 0. " coding changed
        messages-msg_type = 'W'.
        messages-msg_nbr = '034'.
        messages-msg_text = text-034.
        APPEND messages.
      ENDIF.
    ENDIF.

* Check invoice balance
    CLEAR: items_total.
    IF NOT items[] IS INITIAL.
      LOOP AT items.
        IF items-shkzg = 'D'.
          items_total = items_total + items-wrbtr.
        ELSEIF items-shkzg = 'C'.
          items_total = items_total - items-wrbtr.
        ENDIF.
      ENDLOOP.
    ENDIF.

* Calculate net, if not blank
    IF wa_apayhd-wrbtr_net IS INITIAL.
      wa_apayhd-wrbtr_net = wa_apayhd-wrbtr - wa_apayhd-wmwst.
    ENDIF.

** Compare net with line item total
*    IF items_total NE wa_apayhd-wrbtr_net
*      AND NOT items[] IS INITIAL.
*      messages-msg_type = 'E'.
*      messages-msg_nbr = '027'.
*      messages-msg_text = text-027.
*      APPEND messages.
*      EXIT.
*    ENDIF.

* Compare if invoice
    IF wa_apayhd-shkzg = 'S'. " Invoice
      IF NOT items_total IS INITIAL.
        net = wa_apayhd-wrbtr_net - items_total.
      ENDIF.
* Compare if credit memo
    ELSEIF wa_apayhd-shkzg = 'H'. "Credit memo
*     net =  wa_apayhd-wrbtr + items_total.
      net = wa_apayhd-wrbtr_net + items_total.
    ENDIF.

*   IF net NE 0.
    IF net NE 0 AND items_total NE 0.
      messages-msg_type = 'E'.
      messages-msg_nbr = '027'.
      messages-msg_text = text-027.
      APPEND messages.
      EXIT.
    ENDIF.

    IF NOT items[] IS INITIAL
      AND dtype-wf_type = 'FI'.

* Change parked document, if it was not changed before
      CALL FUNCTION '/NRK/APAY_CHANGE_PARKED_DOC_V2'
        EXPORTING
          apayno                      = wa_apayhd-apayno
          header                      = wa_apayhd
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

      LOOP AT bdcmsgcoll WHERE msgtyp EQ 'E'.
        messages-msg_type = bdcmsgcoll-msgtyp.
        messages-msg_nbr = bdcmsgcoll-msgnr.
        MOVE bdcmsgcoll-msgnr TO l_msgnr.
        MOVE bdcmsgcoll-msgv1 TO msg_v1.
        MOVE bdcmsgcoll-msgv2 TO msg_v2.
        MOVE bdcmsgcoll-msgv3 TO msg_v3.
        MOVE bdcmsgcoll-msgv4 TO msg_v4.

        CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
          EXPORTING
            id                = bdcmsgcoll-msgid
            number            = l_msgnr
            language          = sy-langu
            textformat        = 'ASC'
*         LINKPATTERN       =
            message_v1        = msg_v1
            message_v2        = msg_v2
            message_v3        = msg_v3
            message_v4        = msg_v4
          IMPORTING
            message           = msg_text.
*         RETURN            =
*       TABLES
*         TEXT              =

        MOVE msg_text TO messages-msg_text.
        APPEND messages.

      ENDLOOP.

      READ TABLE messages WITH KEY msg_type = 'E'.

      IF sy-subrc EQ 0. " Errors when changing parked document
        EXIT.
      ENDIF.

      IF changed EQ 'X'.
        messages-msg_type = 'I'.
        messages-msg_nbr = '022'.
        messages-msg_text = text-022.
        APPEND messages.
      ENDIF.

    ELSE.
      messages-msg_type = 'I'.
      messages-msg_nbr = '083'.
      messages-msg_text = text-083.
      APPEND messages.
    ENDIF. " change 08/21/2018

* Update APay Center with approval
    LOOP AT t_approver INTO wa_approver.
      wa_approver-approved = 'A'.
      MODIFY t_approver FROM wa_approver.
    ENDLOOP.
    UPDATE /nrk/apayappr FROM TABLE t_approver.

* Create event for APay Center approval
    CLEAR: wa_container.

    MOVE wa_apayhd-apayno TO objectkey.
*   wa_container-element = 'ExternalUserID'.
    wa_container-element = 'EXTUSER'.
    wa_container-value = extuser.
    APPEND wa_container TO input_container.

    CLEAR: wa_container.
    wa_container-element = 'STATUS'.
    wa_container-value = wa_apayhd-status.
    APPEND wa_container TO input_container.

* Trigger workflow event
    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
      EXPORTING
        object_type     = '/NRK/APAY'
        object_key      = objectkey
        event           = 'approved'
        commit_work     = 'X'
        event_language  = sy-langu
        user            = sy-uname
      TABLES
        input_container = input_container.

    messages-msg_type = 'S'.
    messages-msg_nbr = '032'.
    messages-msg_text = text-032.
    APPEND messages.

  ELSEIF rejected EQ 'X'.

* Update APay Center with rejection
    LOOP AT t_approver INTO wa_approver.
      wa_approver-approved = 'R'.
      MODIFY t_approver FROM wa_approver.
    ENDLOOP.
    UPDATE /nrk/apayappr FROM TABLE t_approver.

* Store coding information, if exists
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

* Create APay Center rejection event
    MOVE wa_apayhd-apayno TO objectkey.
    CLEAR: wa_container.
*   wa_container-element = 'ExternalUserID'.
    wa_container-element = 'EXTUSER'.
    wa_container-value = extuser.
    APPEND wa_container TO input_container.

    CLEAR: wa_container.
    wa_container-element = 'STATUS'.
    wa_container-value = apayhd-status.
    APPEND wa_container TO input_container.

    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
      EXPORTING
        object_type             = '/NRK/APAY'
        object_key              = objectkey
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

* Store comment of approval
  IF NOT comment IS INITIAL.
    CALL FUNCTION '/NRK/APAY_ADD_COMMENT_API'
      EXPORTING
        iv_apayno           = wa_apayhd-apayno
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


ENDFUNCTION.
