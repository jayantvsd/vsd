FUNCTION /nrk/apaycreatebus2081.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYNO) TYPE  /NRK/APAYNO
*"     VALUE(MODE) TYPE  CHAR1 DEFAULT 'N'
*"     VALUE(TCODE) TYPE  TCODE DEFAULT 'MIRO'
*"  EXPORTING
*"     VALUE(PARKED) TYPE  BOOLE-BOOLE
*"     VALUE(POSTED) TYPE  BOOLE-BOOLE
*"     VALUE(LIV_BELNR) TYPE  RE_BELNR
*"     VALUE(LIV_GJAHR) TYPE  GJAHR
*"  TABLES
*"      ITEMS STRUCTURE  /NRK/APAYITEMS OPTIONAL
*"      BDCMSGCOLL STRUCTURE  BDCMSGCOLL OPTIONAL
*"  EXCEPTIONS
*"      PARKING_FAILED
*"      RECORD_NOT_FOUND
*"----------------------------------------------------------------------

  DATA:   l_amount(16),
          l_tax(16),
          l_item_amount(16),
          l_item_quantity(16),
          l_bldat(10),
          l_zfbdt(10),
          l_vendor         LIKE invfo-accnt,
          l_inv_type(1)    TYPE c,
          l_date(10)       TYPE c,
          l_field(10)      TYPE c,
          l_pid_bukrs      LIKE bkpf-bukrs,
          l_bukrs          LIKE bkpf-bukrs,
          l_belnr          LIKE bkpf-belnr,
          l_gjahr          LIKE bkpf-gjahr,
          taxcode          LIKE t169v-vstki,
          wa_hd            LIKE /nrk/apayhd,
          p_i_bdcdata      TYPE bdcdata OCCURS 0 WITH HEADER LINE,
          p_i_messages     TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
          wa_messages      TYPE bdcmsgcoll,
          t_items          LIKE /nrk/apayitems OCCURS 0,
          wa_item          LIKE /nrk/apayitems.

  CLEAR: bdcmsgcoll.
  REFRESH: bdcmsgcoll.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE record_not_found.
  ENDIF.

  SET PARAMETER ID 'BUK' FIELD wa_hd-bukrs.
  SET PARAMETER ID 'GJR' FIELD space.
  SET PARAMETER ID 'BLP' FIELD space.
  SET PARAMETER ID 'BLN' FIELD '          '.

*** set transaction mode
  s_ctu_params-dismode  = mode.
  s_ctu_params-updmode  = 'S'.
  s_ctu_params-cattmode = ' '.
  s_ctu_params-defsize  = ' '.
  s_ctu_params-nobinpt  = 'X'.
  s_ctu_params-nobiend  = ' '.

  WRITE: wa_hd-wrbtr CURRENCY wa_hd-waers
         TO l_amount NO-GROUPING.

  WRITE: wa_hd-wmwst CURRENCY wa_hd-waers
         TO l_tax NO-GROUPING.

  WRITE wa_hd-bldat TO l_date.

  PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPLMR1M' '6000'.

  SET PARAMETER ID 'BUK' FIELD wa_hd-bukrs.

  PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-BLDAT' l_date.
  PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-XBLNR' wa_hd-xblnr.

  IF wa_hd-shkzg = 'S'. " invoice
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_CURSOR' 'RM08M-VORGANG'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'RM08M-VORGANG' '1'.
  ELSE. " Credit memo
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_CURSOR' 'RM08M-VORGANG'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'RM08M-VORGANG' '2'.
  ENDIF.

  PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_SUBSCR' 'SAPLFDCB'.
  PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-BLDAT' l_date.
  PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-WRBTR' l_amount.
  PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-WAERS' wa_hd-waers.

  IF wa_hd-wmwst IS INITIAL.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_CURSOR' 'INVFO-XMWST'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-XMWST' 'X'.
  ELSE.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_CURSOR' 'INVFO-XMWST'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-XMWST' ' '.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-WMWST' l_tax.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-MWSKZ' wa_hd-mwskz.
  ENDIF.

  PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_SUBSCR' 'SAPLMR1M'.
  PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_CURSOR' 'RM08M-EBELN'.
  PERFORM bdc_field  TABLES p_i_bdcdata USING 'RM08M-EBELN' wa_hd-ebeln.
  PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_SUBSCR' 'SAPLMR1M'.
  PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_OKCODE' '/00'.

  IF NOT t_items IS INITIAL.
    LOOP AT t_items INTO wa_item.
      PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPLMR1M' '6000'.
      PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_CURSOR' 'RM08M-SKIP_TO'.
      PERFORM bdc_field  TABLES p_i_bdcdata USING 'RM08M-SKIP_TO' wa_item-ebelp.
      PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_OKCODE' '=POS'.

      PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPLMR1M' '6000'.
      PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_SUBSCR' 'SAPLMR1M'.

*      IF NOT wa_item-menge IS INITIAL.
      PERFORM bdc_field  TABLES p_i_bdcdata USING 'DRSEG-SELKZ(01)' 'X'.
*      ENDIF.

      IF NOT wa_item-wrbtr IS INITIAL.
        WRITE wa_item-wrbtr TO l_item_amount.
        PERFORM bdc_field  TABLES p_i_bdcdata USING 'DRSEG-WRBTR(01)' l_item_amount.
      ENDIF.

      IF NOT wa_item-menge IS INITIAL.
        WRITE wa_item-menge TO l_item_quantity.
        PERFORM bdc_field  TABLES p_i_bdcdata USING 'DRSEG-MENGE(01)' l_item_quantity.
        PERFORM bdc_field  TABLES p_i_bdcdata USING 'DRSEG-OK(01)' 'X'.
      ENDIF.

      PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_OKCODE' '/00'.

    ENDLOOP.

    PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPLMR1M' '6000'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_CURSOR' 'RM08M-SKIP_TO'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'RM08M-SKIP_TO' '1'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_OKCODE' '=POS'.
  ENDIF.

  CALL TRANSACTION tcode USING         p_i_bdcdata
                         OPTIONS FROM  s_ctu_params
                         MESSAGES INTO p_i_messages.

  READ TABLE p_i_messages INTO wa_messages
    WITH KEY msgtyp = 'S' msgid = 'M8' msgnr = '388'. "parked

  IF sy-subrc EQ 0.
    liv_belnr = wa_messages-msgv1.
    liv_gjahr = wa_messages-msgv2.
    parked = 'X'.
  ENDIF.

  READ TABLE p_i_messages INTO wa_messages
  WITH KEY msgtyp = 'S' msgid = 'M8' msgnr = '060'. "posted

  IF sy-subrc EQ 0.
    liv_belnr = wa_messages-msgv1.
    liv_gjahr = wa_messages-msgv2.
    posted = 'X'.
    parked = ' '.
  ENDIF.

  READ TABLE p_i_messages INTO wa_messages
  WITH KEY msgtyp = 'S' msgid = 'M8' msgnr = '075'. "posted and blocked

  IF sy-subrc EQ 0.
    liv_belnr = wa_messages-msgv1.
    liv_gjahr = wa_messages-msgv2.
    posted = 'X'.
    parked = ' '.
  ENDIF.

  MOVE p_i_messages[] TO bdcmsgcoll[].

  CLEAR: bdcdata.
  REFRESH: bdcdata.

ENDFUNCTION.
