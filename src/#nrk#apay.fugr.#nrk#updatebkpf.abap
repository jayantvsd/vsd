FUNCTION /nrk/updatebkpf.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS
*"     REFERENCE(BELNR) TYPE  BELNR_D
*"     REFERENCE(GJAHR) TYPE  GJAHR
*"  EXPORTING
*"     REFERENCE(POSTED) TYPE  BOOLE-BOOLE
*"     REFERENCE(DELETED) TYPE  BOOLE-BOOLE
*"  TABLES
*"      LINEITEMS STRUCTURE  /NRK/APAYITEMS OPTIONAL
*"  CHANGING
*"     REFERENCE(HEADER) TYPE  /NRK/APAYHD
*"  EXCEPTIONS
*"      NO_ACCOUNTING_DOC_FOUND
*"      NO_LINE_ITEM_FOUND
*"      NO_VENDOR_FOUND
*"----------------------------------------------------------------------

  DATA: wa_bkpf LIKE bkpf,
        wa_lfa1 LIKE lfa1,
        wa_lfb1 LIKE lfb1,
        t_bseg  LIKE bseg OCCURS 0 WITH HEADER LINE,
        t_vbsegk LIKE vbsegk OCCURS 0 WITH HEADER LINE,
        t_vbsegs LIKE vbsegs OCCURS 0 WITH HEADER LINE.

  CLEAR: wa_bkpf,
         wa_lfa1,
         wa_lfb1,
         t_bseg,
         t_bseg[],
         t_vbsegk,
         t_vbsegk[],
         t_vbsegs,
         t_vbsegs[],
         lineitems,
         lineitems[],
         posted,
         deleted.

* Read header

  SELECT SINGLE * FROM bkpf INTO wa_bkpf
    WHERE bukrs EQ bukrs
      AND belnr EQ belnr
      AND gjahr EQ gjahr.

  IF sy-subrc NE 0.
    RAISE no_accounting_doc_found.
  ENDIF.

  MOVE-CORRESPONDING wa_bkpf TO header.
  header-uname = wa_bkpf-usnam.
* Read line items

  SELECT * FROM bseg INTO TABLE t_bseg
    WHERE bukrs EQ bukrs
      AND belnr EQ belnr
      AND gjahr EQ gjahr.

  IF sy-subrc EQ 0. " Document posted

    posted = 'X'.

    header-sap_object = 'BKPF'.

    READ TABLE t_bseg WITH KEY koart = 'K'.

    MOVE-CORRESPONDING t_bseg TO header.

    IF NOT t_bseg-wrbtr IS INITIAL.
      header-wrbtr = t_bseg-wrbtr.
    ENDIF.
    IF NOT t_bseg-wmwst IS INITIAL.
      header-wmwst = t_bseg-wmwst.
    ENDIF.
*   header-wrbtr_net = header-wrbtr - header-wmwst.

    IF sy-subrc = 0.
      IF t_bseg-bschl = '21'.
        header-shkzg = 'H'.
      ELSEIF t_bseg-bschl = '31'.
        header-shkzg = 'S'.
      ENDIF.
    ENDIF.

    IF t_bseg-zfbdt IS INITIAL.
      t_bseg-zfbdt = wa_bkpf-budat.
    ENDIF.
    IF t_bseg-zbd1t IS INITIAL.
      header-duedate = t_bseg-zfbdt.
    ELSE.
      header-duedate = t_bseg-zfbdt + t_bseg-zbd1t.
    ENDIF.

    header-bukrs = t_bseg-bukrs.
    header-lifnr = t_bseg-lifnr.

    LOOP AT t_bseg.

      IF t_bseg-lifnr IS INITIAL
        OR t_bseg-lifnr NE header-lifnr.

*** start change 01/13/2016

        IF NOT t_bseg-matnr IS INITIAL.
          lineitems-matnr = t_bseg-matnr.
        ENDIF.
        IF NOT t_bseg-bukrs IS INITIAL.
          lineitems-bukrs = t_bseg-bukrs.
        ENDIF.
        IF NOT t_bseg-saknr IS INITIAL.
          lineitems-hkont = t_bseg-saknr.
        ENDIF.
        IF NOT t_bseg-sgtxt IS INITIAL.
          lineitems-sgtxt = t_bseg-sgtxt.
        ENDIF.
        IF NOT t_bseg-kostl IS INITIAL.
          lineitems-kostl = t_bseg-kostl.
        ENDIF.
        IF NOT t_bseg-wrbtr IS INITIAL.
          lineitems-wrbtr = t_bseg-wrbtr.
        ENDIF.
        IF NOT t_bseg-zuonr IS INITIAL.
          lineitems-zuonr = t_bseg-zuonr.
        ENDIF.
        IF NOT t_bseg-aufnr IS INITIAL.
          lineitems-aufnr = t_bseg-aufnr.
        ENDIF.
        IF NOT t_bseg-shkzg IS INITIAL.
          lineitems-shkzg = t_bseg-shkzg.
        ENDIF.
        IF NOT t_bseg-projk IS INITIAL.
          lineitems-projk = t_bseg-projk.
          lineitems-pspnr = t_bseg-projk.

          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              input  = lineitems-pspnr
            IMPORTING
              output = lineitems-projk.
        ENDIF.

*** end change 01/13/2016

        MOVE-CORRESPONDING t_bseg TO lineitems.
        APPEND lineitems.

      ENDIF.

    ENDLOOP.

  ELSE. " parked document

    SELECT * FROM vbsegk INTO TABLE t_vbsegk
      WHERE ausbk = header-bukrs
        AND belnr = header-belnr
        AND gjahr = header-gjahr.

    IF sy-subrc EQ 0.

      header-sap_object = 'FIPP'.

      READ TABLE t_vbsegk INDEX 1.

*      MOVE-CORRESPONDING t_vbsegk TO header.

*      header-wrbtr = t_vbsegk-wrbtr.
*      header-wmwst = t_vbsegk-wmwst.
*      header-wrbtr_net = header-wrbtr - header-wmwst.

      IF NOT t_vbsegk-wrbtr IS INITIAL.
        header-wrbtr = t_vbsegk-wrbtr.
      ENDIF.
      IF NOT t_vbsegk-wmwst IS INITIAL.
        header-wmwst = t_vbsegk-wmwst.
      ENDIF.

*     IF sy-subrc = 0.
      IF t_vbsegk-bschl = '21'.
        header-shkzg = 'H'.
      ELSEIF t_vbsegk-bschl = '31'.
        header-shkzg = 'S'.
      ENDIF.
*     ENDIF.

      IF t_vbsegk-zfbdt IS INITIAL.
        t_vbsegk-zfbdt = wa_bkpf-budat.
      ENDIF.
      IF t_vbsegk-zbd1t IS INITIAL.
        header-duedate = t_vbsegk-zfbdt.
      ELSE.
        header-duedate = t_vbsegk-zfbdt + t_vbsegk-zbd1t.
      ENDIF.

      header-lifnr = t_vbsegk-lifnr.

      LOOP AT t_vbsegk.

        IF t_vbsegk-lifnr IS INITIAL.

          MOVE-CORRESPONDING t_vbsegk TO lineitems.
          APPEND lineitems.

        ENDIF.

      ENDLOOP.

      SELECT * FROM vbsegs INTO TABLE t_vbsegs
        WHERE ausbk = header-bukrs
          AND belnr = header-belnr
          AND gjahr = header-gjahr.

      IF sy-subrc = 0.

        LOOP AT t_vbsegs.

          MOVE-CORRESPONDING t_vbsegs TO lineitems.
          lineitems-hkont = t_vbsegs-saknr.
          lineitems-shkzg = t_vbsegs-shkzg.

*** start changes 01/13/2016

          IF NOT t_vbsegs-matnr IS INITIAL.
            lineitems-matnr = t_vbsegs-matnr.
          ENDIF.
          IF NOT t_vbsegs-ausbk IS INITIAL.
            lineitems-bukrs = t_vbsegs-ausbk.
          ENDIF.
          IF NOT t_vbsegs-sgtxt IS INITIAL.
            lineitems-sgtxt = t_vbsegs-sgtxt.
          ENDIF.
          IF NOT t_vbsegs-kostl IS INITIAL.
            lineitems-kostl = t_vbsegs-kostl.
          ENDIF.
          IF NOT t_vbsegs-wrbtr IS INITIAL.
            lineitems-wrbtr = t_vbsegs-wrbtr.
          ENDIF.
          IF NOT t_vbsegs-zuonr IS INITIAL.
            lineitems-zuonr = t_vbsegs-zuonr.
          ENDIF.
          IF NOT t_vbsegs-zuonr IS INITIAL.
            lineitems-aufnr = t_vbsegs-zuonr.
          ENDIF.
          IF NOT t_vbsegs-ps_psp_pnr IS INITIAL.
            lineitems-projk = t_vbsegs-ps_psp_pnr.
            lineitems-pspnr = t_vbsegs-ps_psp_pnr.

            CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
              EXPORTING
                input  = lineitems-pspnr
              IMPORTING
                output = lineitems-projk.
          ENDIF.

*** end changes 01/13/2016

          APPEND lineitems.

        ENDLOOP.

      ENDIF.

    ELSE. "No vendor line found
      IF wa_bkpf-bstat EQ 'Z'. "Parked document deleted
        deleted = 'X'.
      ENDIF.
    ENDIF.

  ENDIF.

  CALL FUNCTION 'VENDOR_READ'
    EXPORTING
      i_bukrs   = header-bukrs
      i_lifnr   = header-lifnr
    IMPORTING
      e_lfa1    = wa_lfa1
      e_lfb1    = wa_lfb1
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  IF sy-subrc <> 0.
    RAISE no_vendor_found.
  ENDIF.

  header-stras = wa_lfa1-stras.
  header-pstlz = wa_lfa1-pstlz.
  header-ort01 = wa_lfa1-ort01.
  header-land1 = wa_lfa1-land1.
  header-lifname = wa_lfa1-name1.
  TRANSLATE header-lifname TO UPPER CASE.

  header-zsabe = wa_lfb1-zsabe.
  header-zahls = wa_lfb1-zahls.
  header-zterm = wa_lfb1-zterm.
  header-busab = wa_lfb1-busab.

  IF NOT header-zterm IS INITIAL.

*   SELECT SINGLE ztag1 FROM t052
*     INTO header-ztag1 WHERE zterm = header-zterm.

*   IF header-ztag1 IS NOT INITIAL.
*     header-duedate = header-bldat + header-ztag1.
*   ENDIF.

* Calculate due date
    CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
      EXPORTING
        i_bldat         = header-bldat
        i_budat         = sy-datum
        i_zterm         = header-zterm
      IMPORTING
        e_zbd1t         = header-zbd1t
        e_zbd1p         = header-zbd1p
        e_zbd2t         = header-zbd2t
        e_zbd2p         = header-zbd2p
        e_zbd3t         = header-zbd3t
        e_zfbdt         = header-duedate
      EXCEPTIONS
        terms_not_found = 1
        OTHERS          = 2.

    IF sy-subrc EQ 0.
*   wa_hd-duedate = wa_hd-bldat + wa_hd-zbd1t.
    ENDIF.

  ENDIF.

*  IF NOT t_bseg[] IS INITIAL.
*    CLEAR header-wmwst.

*    LOOP AT t_bseg
*       WHERE wmwst GT 0.

*      header-wmwst = header-wmwst + t_bseg-wmwst.

*    ENDLOOP.
*  ENDIF.

ENDFUNCTION.
