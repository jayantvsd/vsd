FUNCTION /nrk/updateapaywithbus2081.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYNO
*"     REFERENCE(BELNR) TYPE  RBKP-BELNR
*"     REFERENCE(GJAHR) TYPE  RBKP-GJAHR
*"     REFERENCE(STATUS) TYPE  /NRK/APAYSTATUS
*"     REFERENCE(AR_OBJECT) TYPE  SAEOBJART
*"     REFERENCE(UNAME) TYPE  UNAME
*"  EXCEPTIONS
*"      NO_APAY_RECORD_FOUND
*"      BKPF_ERROR
*"----------------------------------------------------------------------

  DATA: wa_header    LIKE /nrk/apayhd,
        wa_rbkp      LIKE rbkp,
        wa_ekko      LIKE ekko,
        awkey        LIKE bkpf-awkey,
        t_bkpf       LIKE bkpf OCCURS 0 WITH HEADER LINE,
        t_ebeln      LIKE bseg-ebeln OCCURS 0 WITH HEADER LINE,
        t_ekpo       LIKE ekpo OCCURS 0,
        wa_ekpo      LIKE ekpo,
        wa_his       LIKE /nrk/apayhis,
        t_his        LIKE /nrk/apayhis OCCURS 0 WITH HEADER LINE,
        tlines       TYPE i,
        item         LIKE /nrk/apayhis-item,
        address      LIKE  bapiaddr3,
        return       LIKE  bapiret2 OCCURS 0,
        object_id    LIKE toav0-object_id,
        t_documents  LIKE toav0 OCCURS 0,
        wa_documents LIKE toav0,
        l_ebeln      LIKE ekko-ebeln,
        wa_lfa1      LIKE lfa1,
        wa_lfb1      LIKE lfb1,
        object_key LIKE  swr_struct-object_key,
        t_bseg  LIKE bseg OCCURS 0 WITH HEADER LINE.

  CLEAR: wa_header,
         wa_rbkp,
         wa_ekko,
         t_bkpf,
         t_bkpf[],
         return[],
         address,
         ebeln.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_header
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE no_apay_record_found.
  ENDIF.

  IF NOT wa_header-ebeln IS INITIAL.
    l_ebeln = wa_header-ebeln.
  ENDIF.

  wa_header-uname = uname.
  wa_header-status = status.
  wa_header-ar_object = ar_object.

  SELECT SINGLE * FROM rbkp INTO wa_rbkp
   WHERE belnr = belnr
     AND gjahr = gjahr.

  MOVE-CORRESPONDING wa_rbkp TO wa_header.
  wa_header-liv_belnr = wa_rbkp-belnr.
  wa_header-liv_gjahr = wa_rbkp-gjahr.

  awkey+0(10) = wa_rbkp-belnr.
  awkey+10(4) = wa_rbkp-gjahr.

  SELECT * FROM bkpf INTO TABLE t_bkpf
        WHERE  awtyp = 'RMRP'
        AND   awkey = awkey
        AND   bukrs = wa_rbkp-bukrs.

  READ TABLE t_bkpf INDEX 1.

  IF sy-subrc EQ 0.

* Add MM information
*   MOVE-CORRESPONDING wa_rbkp TO wa_header.
    MOVE-CORRESPONDING t_bkpf TO wa_header.
    wa_header-belnr = t_bkpf-belnr.
    wa_header-bukrs = t_bkpf-bukrs.
    wa_header-gjahr = t_bkpf-gjahr.

  ELSE.

    CLEAR: wa_header-belnr,
           wa_header-gjahr,
           wa_header-bukrs.

  ENDIF.

* Add FI information

  SELECT * FROM bseg INTO TABLE t_bseg
    WHERE bukrs EQ wa_header-bukrs
      AND belnr EQ wa_header-belnr
      AND gjahr EQ wa_header-gjahr.

  IF sy-subrc EQ 0. " Document posted

    READ TABLE t_bseg WITH KEY koart = 'K'.

*   MOVE-CORRESPONDING t_bseg TO wa_header.

    IF NOT t_bseg-wrbtr IS INITIAL.
      wa_header-wrbtr = t_bseg-wrbtr.
    ENDIF.
*   IF NOT t_bseg-wmwst IS INITIAL.
    wa_header-wmwst = t_bseg-wmwst.
*   ENDIF.
*   header-wrbtr_net = header-wrbtr - header-wmwst.

    IF sy-subrc = 0.
      IF t_bseg-bschl = '21'.
        wa_header-shkzg = 'H'.
      ELSEIF t_bseg-bschl = '31'.
        wa_header-shkzg = 'S'.
      ENDIF.
    ENDIF.

    IF t_bseg-zfbdt IS INITIAL.
      t_bseg-zfbdt = t_bkpf-budat.
    ENDIF.
    IF t_bseg-zbd1t IS INITIAL.
      wa_header-duedate = t_bseg-zfbdt.
    ELSE.
      wa_header-duedate = t_bseg-zfbdt + t_bseg-zbd1t.
    ENDIF.

    wa_header-bukrs = t_bseg-bukrs.
    wa_header-lifnr = t_bseg-lifnr.

  ENDIF.

* Add PO information
  CLEAR: t_ebeln, t_ebeln[].
  SELECT ebeln FROM rseg INTO TABLE t_ebeln
    WHERE belnr EQ wa_rbkp-belnr
      AND gjahr EQ wa_rbkp-gjahr.

  READ TABLE t_ebeln INDEX 1.
  wa_header-ebeln = t_ebeln.

  IF wa_header-ebeln IS INITIAL.
    wa_header-ebeln = l_ebeln.
  ENDIF.

  SELECT SINGLE * FROM ekko INTO wa_ekko
    WHERE ebeln = wa_header-ebeln.

  IF sy-subrc EQ 0.
    wa_header-bsart = wa_ekko-bsart.
    wa_header-ekgrp = wa_ekko-ekgrp.
    wa_header-ekorg = wa_ekko-ekorg.
    wa_header-bedat = wa_ekko-bedat.
    wa_header-bstyp = wa_ekko-bstyp.
    wa_header-zterm = wa_ekko-zterm.
    wa_header-zbd1t = wa_ekko-zbd1t.
    wa_header-zbd2t = wa_ekko-zbd2t.
    wa_header-zbd3t = wa_ekko-zbd3t.
    wa_header-zbd1p = wa_ekko-zbd1p.
    wa_header-lifnr = wa_ekko-lifnr.
    wa_header-bukrs = wa_ekko-bukrs.
    wa_header-duedate = wa_header-bldat + wa_ekko-zbd1t.
    wa_header-ernam = wa_ekko-ernam.

    SELECT * FROM ekpo INTO TABLE t_ekpo
      WHERE ebeln EQ wa_header-ebeln.

    LOOP AT t_ekpo INTO wa_ekpo.
      IF wa_ekpo-webre EQ 'X'. " GR required
        wa_header-webre = 'X'.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Get vendor info
  IF NOT wa_header-lifnr IS INITIAL
    AND NOT wa_header-bukrs IS INITIAL.
    CALL FUNCTION 'VENDOR_READ'
      EXPORTING
        i_bukrs   = wa_header-bukrs
        i_lifnr   = wa_header-lifnr
      IMPORTING
        e_lfa1    = wa_lfa1
        e_lfb1    = wa_lfb1
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc = 0.
      wa_header-lifname = wa_lfa1-name1.
      wa_header-zterm   = wa_lfb1-zterm.
      wa_header-stras   = wa_lfa1-stras.
      wa_header-pstlz   = wa_lfa1-pstlz.
      wa_header-ort01   = wa_lfa1-ort01.
      wa_header-land1   = wa_lfa1-land1.

      TRANSLATE wa_header-lifname TO UPPER CASE.

      wa_header-zsabe = wa_lfb1-zsabe.
      wa_header-zahls = wa_lfb1-zahls.
      wa_header-zterm = wa_lfb1-zterm.
      wa_header-busab = wa_lfb1-busab.
    ENDIF.
  ENDIF.

* Calculate due date
  CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
    EXPORTING
      i_bldat         = wa_header-bldat
      i_budat         = sy-datum
      i_zterm         = wa_header-zterm
    IMPORTING
      e_zbd1t         = wa_header-zbd1t
      e_zbd1p         = wa_header-zbd1p
      e_zbd2t         = wa_header-zbd2t
      e_zbd2p         = wa_header-zbd2p
      e_zbd3t         = wa_header-zbd3t
      e_zfbdt         = wa_header-duedate
    EXCEPTIONS
      terms_not_found = 1
      OTHERS          = 2.

  IF sy-subrc EQ 0.
*   wa_hd-duedate = wa_hd-bldat + wa_hd-zbd1t.
  ENDIF.

  wa_header-lastchange = sy-datum.

* Update header
  MODIFY /nrk/apayhd FROM wa_header.

* Get images from APay object
  CLEAR: object_id.
  MOVE apayno TO object_id.
  CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
    EXPORTING
      objecttype    = '/NRK/APAY'
      object_id     = object_id
*     documenttype  = wa_header-ar_object
    TABLES
      connections   = t_documents
    EXCEPTIONS
      nothing_found = 1
      OTHERS        = 2.

* Create links to images from BUS2081
  IF sy-subrc EQ 0.

    CONCATENATE wa_header-liv_belnr wa_header-liv_gjahr INTO object_id.

    LOOP AT t_documents INTO wa_documents.

      CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
        EXPORTING
          archiv_id                   = wa_documents-archiv_id
          arc_doc_id                  = wa_documents-arc_doc_id
          ar_date                     = sy-datum
          ar_object                   = wa_documents-ar_object
          object_id                   = object_id
          sap_object                  = 'BUS2081'
*         DOC_TYPE                    = ' '
*         BARCODE                     = ' '
        EXCEPTIONS
          error_connectiontable       = 1
          OTHERS                      = 2.

      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.

        MOVE object_id TO object_key.

        CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
          EXPORTING
            object_type = 'BUS2081'
            object_key  = object_key
            event       = 'ASSIGNED'
            commit_work = 'X'
            user        = sy-uname.

      ENDIF.

    ENDLOOP.

  ENDIF.

*  IF sy-subrc EQ 0.
  CALL FUNCTION '/NRK/APAYTRIGGEREVENTCHANGED'
    EXPORTING
      apayno = wa_header-apayno.
*  ENDIF.

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
