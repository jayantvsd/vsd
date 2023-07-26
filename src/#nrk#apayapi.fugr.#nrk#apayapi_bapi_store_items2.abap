FUNCTION /nrk/apayapi_bapi_store_items2.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ARCHIV_ID) TYPE  SAEARCHIVI OPTIONAL
*"     VALUE(AR_OBJECT) TYPE  SAEOBJART OPTIONAL
*"     VALUE(ARCHIV_DOC_ID) TYPE  SAEARDOID OPTIONAL
*"     VALUE(USER) TYPE  XUBNAME OPTIONAL
*"     VALUE(STATUS) TYPE  /NRK/APAYSTATUS DEFAULT '1000'
*"     VALUE(BUKRS) TYPE  BUKRS OPTIONAL
*"     VALUE(GJAHR) TYPE  GJAHR OPTIONAL
*"     VALUE(XBLNR) TYPE  XBLNR OPTIONAL
*"     VALUE(BLDAT) TYPE  BLDAT OPTIONAL
*"     VALUE(INVINDICATOR) TYPE  CHAR1 DEFAULT 'S'
*"     VALUE(KOSTL) TYPE  KOSTL OPTIONAL
*"     VALUE(BKTXT) TYPE  BKTXT OPTIONAL
*"     VALUE(SGTXT) TYPE  SGTXT OPTIONAL
*"     VALUE(LIFNR) TYPE  LIFNR OPTIONAL
*"     VALUE(LIFNAME) TYPE  /NRK/APAYLNAME OPTIONAL
*"     VALUE(WRBTR) TYPE  WRBTR OPTIONAL
*"     VALUE(WRBTR_NET) TYPE  WRBTR OPTIONAL
*"     VALUE(WMWST) TYPE  WMWST OPTIONAL
*"     VALUE(MWSKZ) TYPE  MWSKZ OPTIONAL
*"     VALUE(WAERS) TYPE  WAERS DEFAULT 'USD'
*"     VALUE(EBELN) TYPE  EBELN OPTIONAL
*"     VALUE(EXT_USER) TYPE  /NRK/APAYEXUSER OPTIONAL
*"     VALUE(FREIGHT) TYPE  /NRK/APAYFREIGHT OPTIONAL
*"     VALUE(LFSNR1) TYPE  /NRK/APAYHD-LFSNR1 OPTIONAL
*"  EXPORTING
*"     VALUE(APAYNO) TYPE  /NRK/APAYNO
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES
*"      LINEITEMS STRUCTURE  /NRK/APAYITEMS OPTIONAL
*"----------------------------------------------------------------------

  INCLUDE <cntn01>.

  DATA: wa_hd        LIKE /nrk/apayhd,
        wa_lfa1      LIKE lfa1,
        wa_lfb1      LIKE lfb1,
        wa_ekko      LIKE ekko,
        wa_vcode     LIKE /nrk/apayvcode,
        l_archiv_id  LIKE toa01-archiv_id,
        l_arc_doc_id LIKE toa01-arc_doc_id,
        l_kostl      TYPE kostl,
        l_object_id  LIKE sapb-sapobjid,
        l_doc_class  LIKE toadd-doc_type,
        t_items      LIKE /nrk/apayitems OCCURS 0,
        wa_items     LIKE /nrk/apayitems,
        t_ekpo       LIKE ekpo OCCURS 0,
        wa_ekpo      LIKE ekpo,
        l_wrbtr(16),
        l_wrbtr_net(16),
        l_wmwst(16),
        l_buzei   LIKE /nrk/apayitems-buzei,
        l_ebelp   LIKE ekpo-ebelp,
        l_itemcount TYPE i.

  CLEAR: wa_hd,
         wa_lfa1,
         wa_lfb1,
         wa_items,
         wa_ekpo.

  MOVE archiv_id TO l_archiv_id.
  MOVE ar_object TO wa_hd-ar_object.
  MOVE archiv_doc_id TO l_arc_doc_id.
  MOVE bukrs TO wa_hd-bukrs.
  MOVE gjahr TO wa_hd-gjahr.
  MOVE xblnr TO wa_hd-xblnr.
  MOVE bldat TO wa_hd-bldat.
  MOVE bktxt TO wa_hd-bktxt.
  MOVE sgtxt TO wa_hd-sgtxt.
  MOVE lifnr TO wa_hd-lifnr.
  MOVE lifname TO wa_hd-lifname.
  MOVE ebeln TO wa_hd-ebeln.
  MOVE status TO wa_hd-status.
  MOVE mwskz TO wa_hd-mwskz.
  MOVE wrbtr TO wa_hd-wrbtr.
  MOVE wrbtr_net TO wa_hd-wrbtr_net.
  MOVE wmwst TO wa_hd-wmwst.
  MOVE waers TO wa_hd-waers.

  IF NOT lfsnr1 IS INITIAL.
    MOVE lfsnr1 TO wa_hd-lfsnr1.
  ENDIF.

  IF freight IS NOT INITIAL.
    MOVE freight TO wa_hd-freight.
  ENDIF.

  IF user IS NOT INITIAL.
    MOVE user TO  wa_hd-uname.
  ELSE.
    wa_hd-uname = sy-uname.
    user = sy-uname.
  ENDIF.

  IF NOT ext_user IS INITIAL.
    MOVE ext_user TO wa_hd-ext_approver.
  ELSE.
    CLEAR: wa_hd-ext_approver.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_hd-lifnr
    IMPORTING
      output = wa_hd-lifnr.

  IF invindicator = 'S'.
    wa_hd-shkzg = 'S'.
    wa_items-shkzg = 'H'.
  ELSEIF invindicator = 'H'.
    wa_hd-shkzg = 'H'.
    wa_items-shkzg = 'S'.
  ELSEIF invindicator = 'D'.
    wa_hd-shkzg = 'S'.
    wa_items-shkzg = 'H'.
  ELSEIF invindicator = 'C'.
    wa_hd-shkzg = 'H'.
    wa_items-shkzg = 'S'.
  ENDIF.

  wa_hd-ctime = sy-uzeit.
  wa_hd-cdate = sy-datum.
  wa_hd-lastchange = sy-datum.

* Get PO information
  IF NOT wa_hd-ebeln IS INITIAL.

    SELECT SINGLE * FROM ekko INTO wa_ekko
      WHERE ebeln = wa_hd-ebeln.

    IF sy-subrc EQ 0.
      wa_hd-bsart = wa_ekko-bsart.
      wa_hd-ekgrp = wa_ekko-ekgrp.
      wa_hd-ekorg = wa_ekko-ekorg.
      wa_hd-bedat = wa_ekko-bedat.
      wa_hd-bstyp = wa_ekko-bstyp.
      wa_hd-zterm = wa_ekko-zterm.
      wa_hd-zbd1t = wa_ekko-zbd1t.
      wa_hd-zbd2t = wa_ekko-zbd2t.
      wa_hd-zbd3t = wa_ekko-zbd3t.
      wa_hd-zbd1p = wa_ekko-zbd1p.
      wa_hd-lifnr = wa_ekko-lifnr.
      wa_hd-bukrs = wa_ekko-bukrs.
      wa_hd-duedate = wa_hd-bldat + wa_ekko-zbd1t.
      wa_hd-ernam = wa_ekko-ernam.
    ENDIF.

    SELECT * FROM ekpo INTO TABLE t_ekpo
      WHERE ebeln EQ wa_hd-ebeln.


    LOOP AT t_ekpo INTO wa_ekpo.
      IF wa_ekpo-webre EQ 'X'. " GR required
        wa_hd-webre = 'X'.
      ENDIF.
    ENDLOOP.

  ENDIF.

* Get vendor information
  IF NOT wa_hd-lifnr IS INITIAL
    AND NOT wa_hd-bukrs IS INITIAL.
    CALL FUNCTION 'VENDOR_READ'
      EXPORTING
        i_bukrs   = wa_hd-bukrs
        i_lifnr   = wa_hd-lifnr
      IMPORTING
        e_lfa1    = wa_lfa1
        e_lfb1    = wa_lfb1
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      wa_hd-lifname = lifname.
    ELSE.
      wa_hd-lifname = wa_lfa1-name1.
      wa_hd-zterm   = wa_lfb1-zterm.
      wa_hd-stras   = wa_lfa1-stras.
      wa_hd-pstlz   = wa_lfa1-pstlz.
      wa_hd-ort01   = wa_lfa1-ort01.
      wa_hd-land1   = wa_lfa1-land1.
      wa_hd-zsabe = wa_lfb1-zsabe.
      wa_hd-zahls = wa_lfb1-zahls.
      wa_hd-zterm = wa_lfb1-zterm.
      wa_hd-busab = wa_lfb1-busab.

      TRANSLATE wa_hd-lifname TO UPPER CASE.

    ENDIF.
  ENDIF.

* Calculate due date
  CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
    EXPORTING
      i_bldat         = wa_hd-bldat
      i_budat         = sy-datum
      i_zterm         = wa_hd-zterm
    IMPORTING
      e_zbd1t         = wa_hd-zbd1t
      e_zbd1p         = wa_hd-zbd1p
      e_zbd2t         = wa_hd-zbd2t
      e_zbd2p         = wa_hd-zbd2p
      e_zbd3t         = wa_hd-zbd3t
      e_zfbdt         = wa_hd-duedate
    EXCEPTIONS
      terms_not_found = 1
      OTHERS          = 2.

  IF sy-subrc EQ 0.
*   wa_hd-duedate = wa_hd-bldat + wa_hd-zbd1t.
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
  INSERT /nrk/apayhd FROM wa_hd.
  COMMIT WORK AND WAIT.

*** Line Items
  CLEAR: t_items,
         l_buzei,
         l_ebelp.

  REFRESH: t_items.

  DESCRIBE TABLE lineitems LINES l_itemcount.

  IF l_itemcount > 0. " Line items imported

    LOOP AT lineitems INTO wa_items.

      l_buzei = l_buzei + 1.
      wa_items-buzei = l_buzei.

      wa_items-apayno = wa_hd-apayno.


      IF invindicator = 'S'.
        wa_items-shkzg = 'H'.
      ELSEIF invindicator = 'H'.
        wa_items-shkzg = 'S'.
      ELSEIF invindicator = 'D'.
        wa_items-shkzg = 'H'.
      ELSEIF invindicator = 'C'.
        wa_items-shkzg = 'S'.
      ENDIF.

      IF wa_hd-ebeln IS INITIAL. " Non-PO invoice

        CLEAR: wa_items-ebeln,
               wa_items-ebelp,
               wa_items-menge,
               wa_items-meins.

        IF wa_items-bukrs IS INITIAL.
          wa_items-bukrs = wa_hd-bukrs.
        ENDIF.

        IF wa_items-mwskz IS INITIAL.
          wa_items-mwskz = wa_hd-mwskz.
        ENDIF.

        IF wa_items-kostl IS INITIAL
          AND NOT wa_hd-kostl IS INITIAL.
          wa_items-kostl = wa_hd-kostl.
        ENDIF.

*** Start change 09/19/2018

        SELECT SINGLE hkont FROM /nrk/apayvcode INTO wa_items-hkont
          WHERE bukrs EQ wa_hd-bukrs
            AND lifnr EQ wa_hd-lifnr.

*** End change 09/19/2018

        APPEND wa_items TO t_items.

      ELSE.                      " PO invoice

        CLEAR: wa_items-hkont,
               wa_items-kostl.

        IF wa_items-ebeln IS INITIAL.
          wa_items-ebeln = wa_hd-ebeln.
        ENDIF.

        l_ebelp = l_ebelp + 10.
        l_buzei = l_buzei + 1.

        wa_items-ebelp = l_ebelp.
        wa_items-buzei = l_buzei.

        IF wa_items-bukrs IS INITIAL.
          wa_items-bukrs = wa_hd-bukrs.
        ENDIF.

        IF wa_items-mwskz IS INITIAL.
          wa_items-mwskz = wa_hd-mwskz.
        ENDIF.

        APPEND wa_items TO t_items.

      ENDIF.

*   APPEND wa_items TO t_items.

    ENDLOOP.

  ELSE. " No line items imported

    CLEAR: l_buzei.

    IF wa_hd-ebeln IS INITIAL. " Non-PO line items

      CLEAR: wa_items-ebeln,
             wa_items-ebelp,
             wa_items-menge,
             wa_items-meins.

      SELECT * FROM /nrk/apayvcode INTO wa_vcode
        WHERE bukrs EQ wa_hd-bukrs
          AND lifnr EQ wa_hd-lifnr.

        l_buzei = l_buzei + 1.
        wa_items-buzei = l_buzei.

        wa_items-apayno = wa_hd-apayno.

        IF invindicator = 'S'.
          wa_items-shkzg = 'H'.
        ELSEIF invindicator = 'H'.
          wa_items-shkzg = 'S'.
        ELSEIF invindicator = 'D'.
          wa_items-shkzg = 'H'.
        ELSEIF invindicator = 'C'.
          wa_items-shkzg = 'S'.
        ENDIF.

        IF NOT wa_vcode-hkont IS INITIAL.
          wa_items-hkont = wa_vcode-hkont.
        ENDIF.

        IF NOT wa_vcode-kostl IS INITIAL.
          wa_items-kostl = wa_vcode-kostl.
        ENDIF.

*        IF wa_items-buzei = 1.
*          wa_items-wrbtr = wa_hd-wrbtr_net - 100000.
*        ELSE.
*          wa_items-wrbtr = 100000.
*        ENDIF.
        IF wa_items-buzei = 1.
          wa_items-wrbtr = wa_hd-wrbtr_net.
        ELSE.
          CLEAR: wa_items-wrbtr.
        ENDIF.

        APPEND wa_items TO t_items.

      ENDSELECT.

    ENDIF.

  ENDIF.

  INSERT /nrk/apayitems FROM TABLE t_items.

* Get image details
  CALL FUNCTION 'ARCHIV_CONNECTDEFINITION_GET'
    EXPORTING
      objecttype    = '/NRK/APAY'
      documenttype  = wa_hd-ar_object
      client        = sy-mandt
    IMPORTING
      documentclass = l_doc_class
    EXCEPTIONS
      nothing_found = 1
      OTHERS        = 2.

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
      archiv_id             = l_archiv_id
      arc_doc_id            = l_arc_doc_id
      ar_object             = wa_hd-ar_object
      object_id             = l_object_id
      sap_object            = '/NRK/APAY'
    EXCEPTIONS
      error_connectiontable = 1
      OTHERS                = 2.

  IF sy-subrc <> 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '012'.
    messages-msg_text = text-012.
    APPEND messages.
    EXIT.
  ENDIF.

* Start workflow
  CALL FUNCTION '/NRK/APAY_START_WORKFLOW'
    EXPORTING
      apayno                = wa_hd-apayno
      doc_class             = l_doc_class
      archiv_id             = l_archiv_id
      arc_doc_id            = l_arc_doc_id
      user                  = user
    EXCEPTIONS
      workflow_start_failed = 1
      OTHERS                = 2.

  IF sy-subrc <> 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '013'.
    messages-msg_text = text-013.
    APPEND messages.
    EXIT.
  ENDIF.

* Update APay Center
  CALL FUNCTION '/NRK/APAY_UPDATE_STATUS'
    EXPORTING
      apayno        = wa_hd-apayno
      status        = wa_hd-status
      date          = sy-datum
      time          = sy-uzeit
      user          = user
    EXCEPTIONS
      update_failed = 1
      OTHERS        = 2.

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

  apayno = wa_hd-apayno.

ENDFUNCTION.
