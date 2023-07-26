FUNCTION /nrk/apaycreatebkpf.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYHD) TYPE  /NRK/APAYHD
*"     REFERENCE(MODE) TYPE  CHAR1 DEFAULT 'N'
*"  EXPORTING
*"     VALUE(POSTED) TYPE  BOOLE-BOOLE
*"     REFERENCE(BELNR) TYPE  /NRK/APAYHD-BELNR
*"     REFERENCE(BUKRS) TYPE  /NRK/APAYHD-BUKRS
*"     REFERENCE(GJAHR) TYPE  /NRK/APAYHD-GJAHR
*"  TABLES
*"      ITEMS STRUCTURE  /NRK/APAYITEMS
*"      BDCMSGCOLL STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      POSTING_FAILED
*"----------------------------------------------------------------------

* Local fields for memory id import
  DATA: BEGIN OF xbltab OCCURS 1.
          INCLUDE STRUCTURE blntab.
  DATA: END OF xbltab.

* Local fields for G/L data input
  DATA: lv_typ   TYPE c LENGTH 1,
        lv_line  TYPE c LENGTH 2,
        lv_field TYPE fnam_____4,
        lv_wrbtr TYPE c LENGTH 13,
        p_i_bdcdata TYPE bdcdata OCCURS 0 WITH HEADER LINE,
        p_i_messages TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
        lv_bldat(10) TYPE c,
        lv_amount(16),
        t_items  LIKE /nrk/apayitems OCCURS 0,
        wa_items LIKE /nrk/apayitems,
        l_wrbtr(10) TYPE c,
        lv_amount_net(16),
        lv_tax(16),
        l_wrbtr_net(10) TYPE c.

  WRITE apayhd-bldat TO lv_bldat.
  WRITE apayhd-wrbtr CURRENCY apayhd-waers
    TO lv_amount NO-GROUPING.
  WRITE apayhd-wrbtr_net CURRENCY apayhd-waers
    TO lv_amount_net NO-GROUPING.
  WRITE apayhd-wmwst CURRENCY apayhd-waers
  TO lv_tax NO-GROUPING.

  SELECT * FROM /nrk/apayitems INTO TABLE t_items
    WHERE apayno EQ apayhd-apayno.

* Start batch input table maintenance
  SET PARAMETER ID 'BUK' FIELD apayhd-bukrs.

  PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPLACHD' '1000'.

  IF mode = 'N'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'BKPF-BUKRS' apayhd-bukrs.
    PERFORM bdc_field TABLES p_i_bdcdata USING 'BDC_OKCODE' '/00'.
  ENDIF.

  PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPMF05A' '1100'.

  IF mode = 'N'.
    PERFORM bdc_field TABLES p_i_bdcdata USING 'BDC_OKCODE' '/00'.
  ENDIF.

  IF apayhd-shkzg EQ 'H'.   " if credit
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'RF05A-BUSCS' 'G'.
  ELSE.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'RF05A-BUSCS' 'R'.
  ENDIF.

  PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-ACCNT' apayhd-lifnr.

  PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-BLDAT' lv_bldat.

  PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-XBLNR' apayhd-xblnr.

  PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-WRBTR' lv_amount.
  PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-WAERS' apayhd-waers.

  IF NOT apayhd-wmwst IS INITIAL.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_CURSOR' 'INVFO-XMWST'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-XMWST' ' '.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-WMWST' lv_tax.
  ELSE.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_CURSOR' 'INVFO-XMWST'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-XMWST' 'X'.
  ENDIF.

  IF NOT apayhd-mwskz IS INITIAL.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-MWSKZ' apayhd-mwskz.
  ENDIF.

  IF NOT apayhd-sgtxt IS INITIAL.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-SGTXT' apayhd-sgtxt.
  ENDIF.

* payment tab
  IF NOT apayhd-uzawe IS INITIAL.
    PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPMF05A' '1100'.
    PERFORM bdc_field TABLES p_i_bdcdata USING 'BDC_OKCODE' '=PAYM'.
    PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPMF05A' '1100'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'INVFO-UZAWE' apayhd-uzawe.
    PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPMF05A' '1100'.
    PERFORM bdc_field TABLES p_i_bdcdata USING 'BDC_OKCODE' '=MAIN'.
  ENDIF.

* Add line item
  LOOP AT t_items INTO wa_items.

    PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPMF05A' '1100'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_SUBSCR' 'SAPLFSKB'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'ACGL_ITEM-MARKSP(01)' 'X'.
*   PERFORM bdc_field TABLES p_i_bdcdata USING 'BDC_OKCODE' '=0005'.

    PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPMF05A' '1100'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_SUBSCR' 'SAPLFSKB'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'ACGL_ITEM-HKONT(01)' wa_items-hkont.

    IF NOT wa_items-wrbtr IS INITIAL.
      WRITE wa_items-wrbtr CURRENCY apayhd-waers
        TO l_wrbtr_net NO-GROUPING.
*    ELSE.
*      WRITE apayhd-wrbtr CURRENCY apayhd-waers
*    TO l_wrbtr_net NO-GROUPING.
    ENDIF.

    IF NOT l_wrbtr_net IS INITIAL.
      PERFORM bdc_field  TABLES p_i_bdcdata USING 'ACGL_ITEM-WRBTR(01)' l_wrbtr_net.
    ENDIF.

    IF NOT wa_items-mwskz IS INITIAL.
      PERFORM bdc_field  TABLES p_i_bdcdata USING 'ACGL_ITEM-MWSKZ(01)' apayhd-mwskz.
    ENDIF.

    IF NOT wa_items-kostl IS INITIAL.
      PERFORM bdc_field  TABLES p_i_bdcdata USING 'ACGL_ITEM-KOSTL(01)' wa_items-kostl.
    ENDIF.

    IF NOT wa_items-matnr IS INITIAL.
      PERFORM bdc_field  TABLES p_i_bdcdata USING 'ACGL_ITEM-MATNR(01)' wa_items-matnr.
    ENDIF.

*   PERFORM bdc_field TABLES p_i_bdcdata USING 'BDC_OKCODE' '/00'.

  ENDLOOP.

  IF mode EQ 'N'.
    PERFORM bdc_field TABLES p_i_bdcdata USING 'BDC_OKCODE' '=BP'.
  ENDIF.

* Call transaction
  CALL TRANSACTION 'FB60' USING     p_i_bdcdata
*                          MODE     'E'
                           MODE     mode
                           UPDATE   'S'
                           MESSAGES INTO p_i_messages.

  READ TABLE p_i_messages WITH KEY msgtyp = 'S'
                                   msgid = 'F5'
                                   msgnr = '312'.

  IF sy-subrc = 0. " document posted
    MOVE p_i_messages-msgv1 TO belnr.
    GET PARAMETER ID 'BUK' FIELD bukrs.
    GET PARAMETER ID 'GJR' FIELD gjahr.
    posted = 'X'.
  ELSE.
    posted = ' '.
  ENDIF.

  MOVE p_i_messages[] TO bdcmsgcoll[].

ENDFUNCTION.
