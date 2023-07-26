FUNCTION /nrk/apay_create_po_by_user.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"  EXCEPTIONS
*"      NO_APAY_RECORD
*"      NO_PO_RECEIVED
*"----------------------------------------------------------------------

  DATA: wa_header    LIKE /nrk/apayhd,
        wa_ekko      LIKE ekko,
        wa_lfa1      LIKE lfa1,
        wa_lfb1      LIKE lfb1,
        l_ebeln      LIKE ekko-ebeln,
        p_i_bdcdata  TYPE bdcdata OCCURS 0 WITH HEADER LINE,
        p_i_messages TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

  CLEAR: wa_header,
         wa_ekko,
         wa_lfa1,
         wa_lfb1,
         l_ebeln.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_header
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE no_apay_record.
  ENDIF.

  IF NOT wa_header-lifnr IS INITIAL.

    PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPLMEGUI' '0014'.

    PERFORM bdc_field  TABLES p_i_bdcdata USING 'MEPO_TOPLINE-SUPERFIELD' wa_header-lifnr.
    PERFORM bdc_field TABLES p_i_bdcdata USING 'BDC_OKCODE' '/00'.
  ENDIF.

  CALL TRANSACTION 'ME21N' USING    p_i_bdcdata
                           MODE     'E'
                           UPDATE   'S'
                           MESSAGES INTO p_i_messages.

* CALL TRANSACTION 'ME21N'.

  GET PARAMETER ID 'BES' FIELD l_ebeln.

  IF NOT l_ebeln IS INITIAL.

    wa_header-ebeln = l_ebeln.

    SELECT SINGLE * FROM ekko INTO wa_ekko
      WHERE ebeln EQ l_ebeln.

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
      wa_header-ernam = wa_ekko-ernam.
      wa_header-duedate = wa_header-bldat + wa_ekko-zbd1t.

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
          wa_header-zsabe = wa_lfb1-zsabe.
          wa_header-zahls = wa_lfb1-zahls.
          wa_header-zterm = wa_lfb1-zterm.
          wa_header-busab = wa_lfb1-busab.

          TRANSLATE wa_header-lifname TO UPPER CASE.

        ENDIF.
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

    UPDATE /nrk/apayhd FROM wa_header.

  ELSE.
    RAISE no_po_received.
  ENDIF.

ENDFUNCTION.
