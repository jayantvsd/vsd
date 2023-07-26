FUNCTION /nrk/apayapi_bapi_code_approv2.
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
        wa_approver     LIKE /nrk/apayappr.

  DATA: lt_apayitems_t  TYPE /nrk/apayitems OCCURS 0,
        ls_apayitems_t  TYPE /nrk/apayitems,
        ls_items_t      TYPE /nrk/apayitems,
        lv_tabix        LIKE sy-tabix,
        lines(6)        TYPE n,
        parkflag        LIKE boole-boole,
        belnr           LIKE /nrk/apayhd-belnr,
        bukrs           LIKE /nrk/apayhd-bukrs,
        gjahr           LIKE /nrk/apayhd-gjahr,
        items_total     LIKE /nrk/apayhd-wrbtr.

** Add header to structure
  l_apayhd-apayno = apayhd-apayno.
  l_apayhd-lifnr  = apayhd-lifnr.
  l_apayhd-xblnr = apayhd-xblnr.
  l_apayhd-bldat = apayhd-bldat.
  l_apayhd-wrbtr = apayhd-wrbtr.
  l_apayhd-waers = apayhd-waers.
  l_apayhd-budat = apayhd-budat.
  l_apayhd-bukrs = apayhd-bukrs.
  l_apayhd-wmwst = apayhd-wmwst.
  l_apayhd-status = apayhd-status.

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
  ELSE.

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
      messages-msg_type = 'I'.
      messages-msg_nbr = '021'.
      messages-msg_text = text-023.
      APPEND messages.
    ENDIF.

  ENDIF.

* Wenn lt_apayitems_t leer ist, dann ist alles unveraendert => No change to parked document

  IF parkflag EQ 'X'.
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

    IF sy-subrc <> 0.
      messages-msg_type = 'E'.
      messages-msg_nbr = '005'.
      messages-msg_text = text-005.
      APPEND messages.
      EXIT.
    ENDIF.

  ENDIF.

  SELECT SINGLE * FROM /nrk/apayhd INTO l_apayhd
    WHERE apayno EQ l_apayhd-apayno.

* set document status
  IF coded EQ 'X'.
*   AND approved NE 'X'. "Invoice coded

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

      IF parkflag = 'X'.
* Check balance, if line items exist

*       describe table items lines lines.

        IF NOT items[] IS INITIAL.

          CLEAR: items_total.

          LOOP AT items.
            items_total = items_total + items-wrbtr.
          ENDLOOP.

          IF l_apayhd-wrbtr_net IS INITIAL.
            l_apayhd-wrbtr_net = l_apayhd-wrbtr - l_apayhd-wmwst.
          ENDIF.

          IF items_total NE l_apayhd-wrbtr_net.
            messages-msg_type = 'E'.
            messages-msg_nbr = '027'.
            messages-msg_text = text-027.
            APPEND messages.
            EXIT.
          ENDIF.

        ENDIF.

* Parked document exists
        CALL FUNCTION '/NRK/APAY_CHANGE_PARKED_DOC'
          EXPORTING
            apayno                      = l_apayhd-apayno
            header                      = l_apayhd
          TABLES
            t_items                     = items
          EXCEPTIONS
            no_apay_record_id           = 1
            parked_document_not_changed = 2
            OTHERS                      = 3.

        IF sy-subrc <> 0.
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
*        ELSE.
** posted document exists (no update)
*          messages-msg_type = 'I'.
*          messages-msg_nbr = '024'.
*          messages-msg_text = text-024.
*          APPEND messages.
*        ENDIF.
      ENDIF.

    ENDIF.

  ENDIF.

  IF approved EQ 'X'.
*
*    IF parkflag = 'X'.
*
*      CALL FUNCTION '/NRK/APAY_CHANGE_PARKED_DOC'
*        EXPORTING
*          apayno                  = l_apayhd-apayno
**     KOSTL                   =
*        TABLES
*          t_items                 = items
*        EXCEPTIONS
*          no_apay_record_id       = 1
*          parked_document_not_changed = 2
*          OTHERS                  = 3.
*
*      IF sy-subrc <> 0.
*        messages-msg_type = 'E'.
*        messages-msg_nbr = '021'.
*        messages-msg_text = text-021.
*        APPEND messages.
*        EXIT.
*      ELSE.
*        messages-msg_type = 'S'.
*        messages-msg_nbr = '022'.
*        messages-msg_text = text-022.
*        APPEND messages.
*      ENDIF.
*
*    ENDIF.
*
*  ENDIF.

*    CALL FUNCTION '/NRK/APAYSTORECODING'
*      EXPORTING
*        apayhd         = l_apayhd
*      IMPORTING
*        parked         = parked
*      TABLES
*        items          = items
*        bdcmsgcoll     = bdcmsgcoll
*      EXCEPTIONS
*        parking_failed = 1
*        OTHERS         = 2.
*
*    IF sy-subrc <> 0.
*      messages-msg_type = 'E'.
*      messages-msg_nbr = '005'.
*      messages-msg_text = text-005.
*      APPEND messages.
*      EXIT.
*    ENDIF.

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

  ENDIF.

  IF rejected EQ 'X'. "Invoice rejected

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

    SELECT * FROM /nrk/apayappr INTO TABLE t_approver
      WHERE apayno EQ l_apayhd-apayno
        AND ext_user EQ extuser.
*       AND NOT approved EQ space.

    LOOP AT t_approver INTO wa_approver.
      wa_approver-approved = 'R'.
      MODIFY t_approver FROM wa_approver.
    ENDLOOP.

    UPDATE /nrk/apayappr FROM TABLE t_approver.

    MOVE l_apayhd-apayno TO l_objectkey.
*    swc_set_element input_container 'ExternalUserID' extuser.
*    swc_set_element input_container 'Status' l_apayhd-status.

    CLEAR: wa_container.
    wa_container-element = 'ExternalUserID'.
    wa_container-value = extuser.
    APPEND wa_container TO input_container.

    CLEAR: wa_container.
    wa_container-element = 'STATUS'.
    wa_container-value = l_apayhd-status.
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

  ENDIF.

ENDFUNCTION.
