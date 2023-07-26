FUNCTION /nrk/apayenterpo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYNO
*"  EXCEPTIONS
*"      CANCEL
*"      CREATE
*"----------------------------------------------------------------------

  DATA: wa_hd LIKE /nrk/apayhd,
        wa_ekko LIKE ekko,
        wa_lfa1 LIKE lfa1,
        wa_lfb1 LIKE lfb1.

  CLEAR: wa_hd,
         wa_ekko,
         wa_lfa1,
         wa_lfb1,
         cancel,
         create.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayno.

  IF sy-subrc EQ 0.
    ebeln = wa_hd-ebeln.
  ENDIF.

  CALL SCREEN 0903 STARTING AT 10 5 ENDING AT 50 10.

  IF cancel EQ 'X'.
    RAISE cancel.
  ENDIF.

  IF create EQ 'X'.
    RAISE create.
  ENDIF.

  IF NOT ebeln IS INITIAL.

    wa_hd-ebeln = ebeln.

    SELECT SINGLE * FROM ekko INTO wa_ekko
      WHERE ebeln EQ ebeln.

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
      wa_hd-ernam = wa_ekko-ernam.
      wa_hd-duedate = wa_hd-bldat + wa_ekko-zbd1t.

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

        IF sy-subrc = 0.
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

    UPDATE /nrk/apayhd FROM wa_hd.

  ELSE.
    RAISE cancel.
  ENDIF.

ENDFUNCTION.
