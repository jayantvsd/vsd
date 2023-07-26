FUNCTION /nrk/apayapi_bapi_store_data.
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
*"     VALUE(WAERS) TYPE  WAERS DEFAULT 'USD'
*"     VALUE(EBELN) TYPE  EBELN OPTIONAL
*"     VALUE(HKONT) TYPE  HKONT OPTIONAL
*"     VALUE(MATNR) TYPE  MATNR OPTIONAL
*"     VALUE(MWSKZ) TYPE  MWSKZ OPTIONAL
*"     VALUE(UZAWE) TYPE  UZAWE OPTIONAL
*"     VALUE(MEINS) TYPE  MEINS OPTIONAL
*"     VALUE(MENGE) TYPE  MENGE_D OPTIONAL
*"     VALUE(EXPENSETYPE) TYPE  /NRK/APAYEXPTYPE OPTIONAL
*"     VALUE(EXT_USER) TYPE  /NRK/APAYEXUSER OPTIONAL
*"     VALUE(EQ_EINDT) TYPE  WRF_POHF_EQ_EINDT OPTIONAL
*"  EXPORTING
*"     VALUE(APAYNO) TYPE  /NRK/APAYNO
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES
*"----------------------------------------------------------------------

  INCLUDE <cntn01>.

  DATA: wa_hd        LIKE /nrk/apayhd,
        wa_lfa1      LIKE lfa1,
        wa_lfb1      LIKE lfb1,
        wa_ekko      LIKE ekko,
        l_archiv_id  LIKE toa01-archiv_id,
        l_arc_doc_id LIKE toa01-arc_doc_id,
*       l_user       TYPE xubname,
        l_kostl      TYPE kostl,
        l_object_id  LIKE sapb-sapobjid,
        l_doc_class  LIKE toadd-doc_type,
        t_items      LIKE /nrk/apayitems OCCURS 0,
        wa_items     LIKE /nrk/apayitems,
        t_ekpo       LIKE ekpo OCCURS 0,
        wa_ekpo      LIKE ekpo,
        l_wrbtr(16),
        l_wrbtr_net(16),
        l_wmwst(16).

  CLEAR: wa_hd,
         wa_lfa1,
         wa_lfb1,
         wa_items,
         wa_ekpo.

  MOVE archiv_id TO l_archiv_id.
  MOVE ar_object TO wa_hd-ar_object.
  MOVE archiv_doc_id TO l_arc_doc_id.

  IF user IS NOT INITIAL.
    MOVE user TO  wa_hd-uname.
  ELSE.
    wa_hd-uname = sy-uname.
    user = sy-uname.
  ENDIF.
  IF NOT ext_user IS INITIAL.

    MOVE ext_user TO wa_hd-ext_approver.

*  ELSE.

  ENDIF.
  MOVE bukrs TO wa_hd-bukrs.
  MOVE gjahr TO wa_hd-gjahr.
  MOVE xblnr TO wa_hd-xblnr.
  MOVE bldat TO wa_hd-bldat.
  MOVE bktxt TO wa_hd-bktxt.
  MOVE sgtxt TO wa_hd-sgtxt.
  MOVE lifnr TO wa_hd-lifnr.
  MOVE expensetype TO wa_hd-expensetype.
  MOVE eq_eindt TO wa_hd-eq_eindt.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_hd-lifnr
    IMPORTING
      output = wa_hd-lifnr.

  MOVE lifname TO wa_hd-lifname.
  MOVE ebeln TO wa_hd-ebeln.
  MOVE status TO wa_hd-status.
  MOVE mwskz TO wa_hd-mwskz.
  MOVE uzawe TO wa_hd-uzawe.
  MOVE kostl TO l_kostl.
  wa_hd-kostl = l_kostl.

  MOVE wrbtr TO wa_hd-wrbtr.
  MOVE wrbtr_net TO wa_hd-wrbtr_net.
  MOVE wmwst TO wa_hd-wmwst.
  MOVE waers TO wa_hd-waers.

  IF wa_hd-wrbtr < 0.
    wa_hd-wrbtr = wa_hd-wrbtr * -1.
  ENDIF.

  IF wa_hd-wrbtr_net < 0.
    wa_hd-wrbtr_net = wa_hd-wrbtr_net * -1.
  ENDIF.

  IF wa_hd-wmwst < 0.
    wa_hd-wmwst = wa_hd-wmwst * -1.
  ENDIF.

*  write wa_hd-wrbtr currency wa_hd-waers
*    to l_wrbtr no-grouping.
*
*  write wa_hd-wrbtr_net currency wa_hd-waers
*    to l_wrbtr_net no-grouping.
*
*  write wa_hd-wmwst currency wa_hd-waers
*    to l_wmwst no-grouping.

*  MOVE l_wrbtr TO wa_hd-wrbtr.
*  MOVE l_wrbtr_net TO wa_hd-wrbtr_net.
*  MOVE l_wmwst TO wa_hd-wmwst.
*  MOVE waers TO wa_hd-waers.

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
      IF lifname IS INITIAL.
        wa_hd-lifname = wa_lfa1-name1.
      ENDIF.
      wa_hd-zterm   = wa_lfb1-zterm.
      wa_hd-stras   = wa_lfa1-stras.
      wa_hd-pstlz   = wa_lfa1-pstlz.
      wa_hd-ort01   = wa_lfa1-ort01.
      wa_hd-land1   = wa_lfa1-land1.
*      wa_hd-lifname = wa_lfa1-name1.

      TRANSLATE wa_hd-lifname TO UPPER CASE.

      wa_hd-zsabe = wa_lfb1-zsabe.
      wa_hd-zahls = wa_lfb1-zahls.
      wa_hd-zterm = wa_lfb1-zterm.
      wa_hd-busab = wa_lfb1-busab.
    ENDIF.
  ENDIF.

* Update due date
  CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
    EXPORTING
      i_bldat               = wa_hd-bldat
      i_budat               = sy-datum
*   I_CPUDT               = SY-DATUM
*   I_ZFBDT               =
      i_zterm               = wa_hd-zterm
*   I_REINDAT             =
*   I_LIFNR               =
*   I_BUKRS               =
    IMPORTING
      e_zbd1t               = wa_hd-zbd1t
      e_zbd1p               = wa_hd-zbd1p
      e_zbd2t               = wa_hd-zbd2t
      e_zbd2p               = wa_hd-zbd2p
      e_zbd3t               = wa_hd-zbd3t
      e_zfbdt               = wa_hd-duedate
*   E_SPLIT               =
*   E_ZSCHF               =
*   E_ZLSCH               =
*   E_T052                =
    EXCEPTIONS
      terms_not_found       = 1
      OTHERS                = 2.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
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

* Add one line item
  IF NOT hkont IS INITIAL
  AND NOT hkont EQ '0000000000'. " Non-PO Invoice
    wa_items-apayno = wa_hd-apayno.
    wa_items-buzei = '000001'.
    wa_items-bukrs = wa_hd-bukrs.
    wa_items-hkont = hkont.

    IF NOT wa_hd-wrbtr_net IS INITIAL.
      wa_items-wrbtr = wa_hd-wrbtr_net.
*    ELSE.
*      wa_items-wrbtr = wa_hd-wrbtr.
    ENDIF.
    wa_items-mwskz = wa_hd-mwskz.
    wa_items-kostl = wa_hd-kostl.
    wa_items-matnr = matnr.
    wa_items-uzawe = uzawe.

    IF invindicator = 'S'.
      wa_items-shkzg = 'H'.
    ELSEIF invindicator = 'H'.
      wa_items-shkzg = 'S'.
    ELSEIF invindicator = 'D'.
      wa_items-shkzg = 'H'.
    ELSEIF invindicator = 'C'.
      wa_items-shkzg = 'S'.
    ENDIF.

    APPEND wa_items TO t_items.
    INSERT /nrk/apayitems FROM TABLE t_items.

  ELSEIF NOT ebeln IS INITIAL
    AND NOT menge IS INITIAL
    AND NOT meins IS INITIAL. " PO Invoice

    wa_items-apayno = wa_hd-apayno.
    wa_items-buzei = '000001'.
    wa_items-ebelp = '00010'.
    wa_items-bukrs = wa_hd-bukrs.
    wa_items-ebeln = wa_hd-ebeln.
    wa_items-mwskz = wa_hd-mwskz.
    wa_items-wrbtr = wa_hd-wrbtr.
    wa_items-menge = menge.
    wa_items-meins = meins.
    APPEND wa_items TO t_items.
    INSERT /nrk/apayitems FROM TABLE t_items.
  ENDIF.

* Get image details
  CALL FUNCTION 'ARCHIV_CONNECTDEFINITION_GET'
    EXPORTING
      objecttype          = '/NRK/APAY'
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

* wa_hd-ar_object = '/NRK/APAY'.

* Start workflow
  CALL FUNCTION '/NRK/APAY_START_WORKFLOW'
    EXPORTING
      apayno                      = wa_hd-apayno
      doc_class                   = l_doc_class
      archiv_id                   = l_archiv_id
      arc_doc_id                  = l_arc_doc_id
      user                        = user
* IMPORTING
*   WF_ID                       =
    EXCEPTIONS
      workflow_start_failed       = 1
      OTHERS                      = 2.

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
      apayno              = wa_hd-apayno
      status              = wa_hd-status
      date                = sy-datum
      time                = sy-uzeit
      user                = user
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

  apayno = wa_hd-apayno.

ENDFUNCTION.
