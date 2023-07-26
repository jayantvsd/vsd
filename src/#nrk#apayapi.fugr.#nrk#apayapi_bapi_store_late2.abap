FUNCTION /NRK/APAYAPI_BAPI_STORE_LATE2.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DATALATE) TYPE  /NRK/APAYATCHLATE OPTIONAL
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES
*"  EXCEPTIONS
*"      BKPF_NOT_FOUND
*"--------------------------------------------------------------------

  INCLUDE <cntn01>.

  DATA: wa_hd        LIKE /nrk/apayhd,
        wa_lfa1      LIKE lfa1,
        wa_lfb1      LIKE lfb1,
        l_archiv_id  LIKE toa01-archiv_id,
        l_arc_doc_id LIKE toa01-arc_doc_id,
*       l_user       TYPE xubname,
        l_kostl      TYPE kostl,
        l_object_id  LIKE sapb-sapobjid,
        l_doc_class  LIKE toadd-doc_type,
        t_items      LIKE /nrk/apayitems OCCURS 0,
        wa_items     LIKE /nrk/apayitems,
        l_barcode    LIKE boole-boole,
        l_ar_object  LIKE toa01-ar_object.

  CLEAR: wa_hd,
         wa_lfa1,
         wa_lfb1,
         wa_items,
         l_barcode.

  MOVE datalate-archiv_id TO l_archiv_id.
  MOVE datalate-ar_object TO l_ar_object.
  MOVE datalate-archiv_doc_id TO l_arc_doc_id.
  MOVE datalate-user TO  wa_hd-uname.
*  MOVE datalate-bukrs TO wa_hd-bukrs.
*  MOVE datalate-gjahr TO wa_hd-gjahr.
*  MOVE datalate-belnr TO wa_hd-belnr.
  MOVE datalate-status TO wa_hd-status.

* Get accounting document from barcode table
  IF datalate-barcode IS NOT INITIAL. " with barcode

    SELECT SINGLE object_key FROM bds_bar_in INTO l_object_id
      WHERE docutype EQ datalate-ar_object
        AND contrep  EQ l_archiv_id.

    IF sy-subrc EQ 0.
      l_barcode = 'X'.
    ENDIF.

  ELSE. "without barcode

    CONCATENATE datalate-bukrs
                datalate-belnr
                datalate-gjahr INTO l_object_id.

  ENDIF.

  IF l_object_id IS INITIAL.

    IF sy-subrc <> 0.
      messages-msg_type = 'E'.
      messages-msg_nbr = '019'.
      messages-msg_text = text-019.
      APPEND messages.
      EXIT.
    ENDIF.

  ENDIF.

  SELECT SINGLE * FROM bkpf INTO CORRESPONDING FIELDS OF wa_hd
    WHERE bukrs EQ l_object_id(4)
      AND belnr EQ l_object_id+4(10)
      AND gjahr EQ l_object_id+14(4).

  wa_hd-ar_object = l_ar_object.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
      EXPORTING
        archiv_id                   = l_archiv_id
        arc_doc_id                  = l_arc_doc_id
*       AR_DATE                     = ' '
        ar_object                   = l_ar_object
*       DEL_DATE                    = ' '
*       MANDANT                     = ' '
        object_id                   = l_object_id
        sap_object                  = 'BKPF'
*       DOC_TYPE                    = ' '
*       BARCODE                     = ' '
      EXCEPTIONS
        error_connectiontable       = 1
        OTHERS                      = 2.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


  ENDIF.

* Get APay record id
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr                   = '01'
      object                        = '/NRK/APAY'
    IMPORTING
      number                        = wa_hd-apayno
*   QUANTITY                      =
*   RETURNCODE                    =
    EXCEPTIONS
     interval_not_found            = 1
     number_range_not_intern       = 2
     object_not_found              = 3
     quantity_is_0                 = 4
     quantity_is_not_1             = 5
     interval_overflow             = 6
     buffer_overflow               = 7
     OTHERS                        = 8.

  IF sy-subrc <> 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '011'.
    messages-msg_text = text-011.
    APPEND messages.
    EXIT.
  ENDIF.

* Store metadata in APay Center
*  INSERT /nrk/apayhd FROM wa_hd.
*  COMMIT WORK AND WAIT.

* Update APay Center
  IF datalate-user IS INITIAL.
    datalate-user = sy-uname.
  ENDIF.

  CALL FUNCTION '/NRK/UPDATEAPAYWITHBKPF'
    EXPORTING
      apayno                     = wa_hd-apayno
      bukrs                      = wa_hd-bukrs
      belnr                      = wa_hd-belnr
      gjahr                      = wa_hd-gjahr
*   STATUS                     =
*   STATUS_POST                =
*   STATUS_PARK                =
      ar_object                  = wa_hd-ar_object
      uname                      = datalate-user
    EXCEPTIONS
      no_apay_record_found       = 1
      bkpf_error                 = 2
      OTHERS                     = 3.

  IF sy-subrc NE 0.
    RAISE bkpf_not_found.
  ENDIF.

* Get image details
  CALL FUNCTION 'ARCHIV_CONNECTDEFINITION_GET'
    EXPORTING
      objecttype          = 'BKPF'
      documenttype        = wa_hd-ar_object
      client              = sy-mandt
    IMPORTING
*   CONNECTION          =
*   ARCHIVID            =
*   EXPIRYTIME          =
      documentclass       = l_doc_class
    EXCEPTIONS
      nothing_found       = 1
      OTHERS              = 2.

  IF sy-subrc <> 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '015'.
    messages-msg_text = text-015.
    APPEND messages.
    EXIT.
  ENDIF.

* link image to APay Center
  l_object_id = wa_hd-apayno.
  CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
    EXPORTING
      archiv_id                   = l_archiv_id
      arc_doc_id                  = l_arc_doc_id
      ar_object                   = wa_hd-ar_object
      object_id                   = l_object_id
      sap_object                  = '/NRK/APAY'
*   DOC_TYPE                    = ' '
*   BARCODE                     = ' '
    EXCEPTIONS
      error_connectiontable       = 1
      OTHERS                      = 2.

  IF sy-subrc <> 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '012'.
    messages-msg_text = text-012.
    APPEND messages.
    EXIT.
  ENDIF.

  IF l_barcode = 'X'.

    CALL FUNCTION 'ARCHIV_BARCODE_DELETE'
      EXPORTING
        barcode             = datalate-barcode
        objecttype          = 'BKPF'
        objectid            = l_object_id
*       ARCHIVE             = ' '
*       ARC_DOC_ID          = ' '
        client              = sy-mandt
      EXCEPTIONS
        nothing_found       = 1
        OTHERS              = 2.

  ENDIF.

  wa_hd-status = '1900'.

  CALL FUNCTION '/NRK/APAY_UPDATE_STATUS'
    EXPORTING
      apayno              = wa_hd-apayno
      status              = wa_hd-status
      date                = sy-datum
      time                = sy-uzeit
      user                = datalate-user
*   user_ext            =
    EXCEPTIONS
      update_failed       = 1
      OTHERS              = 2.

  IF sy-subrc <> 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '014'.
    messages-msg_text = text-014.
    APPEND messages.
    EXIT.
  ENDIF.

  messages-msg_type = 'I'.
  messages-msg_nbr = '010'.
  messages-msg_text = text-010.
  APPEND messages.

ENDFUNCTION.
