FUNCTION /nrk/updateapaywithbkpf.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYNO
*"     REFERENCE(BUKRS) TYPE  BUKRS
*"     REFERENCE(BELNR) TYPE  BELNR_D
*"     REFERENCE(GJAHR) TYPE  GJAHR
*"     REFERENCE(STATUS) TYPE  /NRK/APAYSTATUS OPTIONAL
*"     REFERENCE(STATUS_POST) TYPE  /NRK/APAYSTATUS OPTIONAL
*"     REFERENCE(STATUS_PARK) TYPE  /NRK/APAYSTATUS OPTIONAL
*"     REFERENCE(AR_OBJECT) TYPE  SAEOBJART
*"     REFERENCE(UNAME) TYPE  UNAME
*"     REFERENCE(ARCHIV_ID) TYPE  SAEARCHIVI OPTIONAL
*"     REFERENCE(ARC_DOC_ID) TYPE  SAEARDOID OPTIONAL
*"  EXCEPTIONS
*"      NO_APAY_RECORD_FOUND
*"      BKPF_ERROR
*"----------------------------------------------------------------------

  DATA: wa_header    LIKE /nrk/apayhd,
        wa_his       LIKE /nrk/apayhis,
        t_his        LIKE /nrk/apayhis OCCURS 0 WITH HEADER LINE,
        t_items      LIKE /nrk/apayitems OCCURS 0 WITH HEADER LINE,
        wa_items     LIKE /nrk/apayitems,
        tlines       TYPE i,
        item         LIKE /nrk/apayhis-item,
        address      LIKE  bapiaddr3,
        return       LIKE  bapiret2 OCCURS 0,
        object_id    LIKE toav0-object_id,
        t_documents  LIKE toav0 OCCURS 0,
        wa_documents LIKE toav0,
        object_key LIKE  swr_struct-object_key.

  CLEAR: wa_header,
         wa_his,
         t_his, t_his[],
         t_items, t_items[],
         wa_items,
         wa_documents,
         t_documents[],
         return[],
         address.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_header
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE no_apay_record_found.
  ENDIF.

  wa_header-uname = uname.
  wa_header-status = status.
  wa_header-ar_object = ar_object.

  CALL FUNCTION '/NRK/UPDATEBKPF'
    EXPORTING
      bukrs                   = bukrs
      belnr                   = belnr
      gjahr                   = gjahr
    TABLES
      lineitems               = t_items
    CHANGING
      header                  = wa_header
    EXCEPTIONS
      no_accounting_doc_found = 1
      no_line_item_found      = 2
      no_vendor_found         = 3
      OTHERS                  = 4.

  IF sy-subrc <> 0.
    RAISE bkpf_error.
  ENDIF.


  MOVE apayno TO object_id.
  CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
    EXPORTING
      objecttype               = '/NRK/APAY'
      object_id                = object_id
*   CLIENT                   =
*   ARCHIV_ID                =
*   ARC_DOC_ID               =
*   documenttype             = ar_object
*   FROM_AR_DATE             =
*   UNTIL_AR_DATE            = SY-DATUM
*   DOCUMENTCLASS            =
*   DEL_DATE                 =
*   LIMITED                  =
*   LIMIT                    =
* IMPORTING
*   COUNT                    =
*   REDUCEDBYLIMIT           =
*   REDUCEDBYAUTHORITY       =
    TABLES
      connections              = t_documents
*   PARAMETER                =
    EXCEPTIONS
      nothing_found            = 1
      OTHERS                   = 2.

  IF sy-subrc EQ 0.

    LOOP AT t_documents INTO wa_documents.

      CONCATENATE bukrs belnr gjahr INTO object_id.
* Check status
      IF wa_header-bstat EQ 'V'.
*        AND NOT status IS INITIAL. " parked

        wa_header-status = status_park.

        CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
          EXPORTING
            archiv_id                   = wa_documents-archiv_id
            arc_doc_id                  = wa_documents-arc_doc_id
            ar_date                     = sy-datum
            ar_object                   = wa_documents-ar_object
*       DEL_DATE                    = ' '
*       MANDANT                     = ' '
            object_id                   = object_id
            sap_object                  = 'FIPP'
*       DOC_TYPE                    = ' '
*       BARCODE                     = ' '
          EXCEPTIONS
            error_connectiontable       = 1
            OTHERS                      = 2.

        IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.

          MOVE object_id TO object_key.

          CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
            EXPORTING
              object_type = 'FIPP'
              object_key  = object_key
              event       = 'ASSIGNED'
              commit_work = 'X'
              user        = sy-uname.

        ENDIF.

      ELSEIF wa_header-bstat IS INITIAL
          OR wa_header-bstat EQ 'S'.

**       AND NOT status IS INITIAL. " posted
*
*        wa_header-status = status_post.
*
        CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
          EXPORTING
            archiv_id                   = wa_documents-archiv_id
            arc_doc_id                  = wa_documents-arc_doc_id
            ar_date                     = sy-datum
            ar_object                   = wa_documents-ar_object
*       DEL_DATE                    = ' '
*       MANDANT                     = ' '
            object_id                   = object_id
            sap_object                  = 'BKPF'
*       DOC_TYPE                    = ' '
*       BARCODE                     = ' '
          EXCEPTIONS
            error_connectiontable       = 1
            OTHERS                      = 2.

        IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.

          MOVE object_id TO object_key.

          CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
            EXPORTING
              object_type = 'BKPF'
              object_key  = object_key
              event       = 'ASSIGNED'
              commit_work = 'X'
              user        = sy-uname.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDIF.


* Update header
  MODIFY /nrk/apayhd FROM wa_header.

  IF sy-subrc EQ 0.
    CALL FUNCTION '/NRK/APAYTRIGGEREVENTCHANGED'
      EXPORTING
        apayno = wa_header-apayno.
  ENDIF.

* Update line items
  DELETE FROM /nrk/apayitems
    WHERE apayno EQ wa_header-apayno.

  LOOP AT t_items INTO wa_items.

    wa_items-apayno = wa_header-apayno.

*** start change

    wa_items-pspnr = wa_items-projk.

    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = wa_items-pspnr
      IMPORTING
        output = wa_items-projk.

*** end change

    INSERT /nrk/apayitems FROM wa_items.

  ENDLOOP.

* Update history
  SELECT * FROM /nrk/apayhis INTO TABLE t_his
     WHERE apayno = wa_header-apayno.

  DESCRIBE TABLE t_his LINES tlines.
  item = tlines + 1.

  wa_his-apayno = wa_header-apayno.
  wa_his-item   = item.
  wa_his-status = wa_header-status.
  wa_his-sdate  = sy-datum.
  wa_his-stime  = sy-uzeit.
  wa_his-suser  = uname.

  SELECT SINGLE fullname FROM /nrk/apayuser INTO wa_his-sname
    WHERE objid EQ wa_his-suser.

  IF sy-subrc NE 0.
    SELECT SINGLE fullname FROM /nrk/apayuser INTO wa_his-sname
      WHERE extuser EQ wa_his-sextuser.
  ENDIF.

  INSERT /nrk/apayhis FROM wa_his.

ENDFUNCTION.
