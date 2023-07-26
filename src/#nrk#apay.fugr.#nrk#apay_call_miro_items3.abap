FUNCTION /nrk/apay_call_miro_items3.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"  EXPORTING
*"     REFERENCE(BELNR) TYPE  BELNR_D
*"     REFERENCE(GJAHR) TYPE  GJAHR
*"     REFERENCE(STATUS) TYPE  CHAR1
*"  EXCEPTIONS
*"      INVALID_RECORD
*"      ERROR
*"----------------------------------------------------------------------

* Data declarations
  DATA: ls_hd TYPE /nrk/apayhd.

  DATA: lv_wrbtr(16) TYPE c,
        lv_bldat     LIKE sy-datum,
        lv_cnt       TYPE i,
        lv_field(10)      TYPE c,
        lv_tax_amount(10) TYPE c,
        n TYPE n.

  DATA: lv_freight(16) TYPE c,
        lv_pallet(16) TYPE c,
        lv_val1 LIKE /nrk/apayconfig-val1.

  DATA: i_po_items LIKE /nrk/apaypoitems
        OCCURS 100
        WITH HEADER LINE.

  DATA: BEGIN OF i_messages OCCURS 0.
          INCLUDE STRUCTURE bdcmsgcoll.
  DATA: END OF i_messages.
  DATA: BEGIN OF i_bdcdata OCCURS 0.
          INCLUDE STRUCTURE bdcdata.
  DATA: END OF i_bdcdata.
  DATA: BEGIN OF i_bdcparam OCCURS 0.
          INCLUDE STRUCTURE ctu_params.
  DATA: END OF i_bdcparam.

  DATA: l_awkey LIKE bkpf-awkey,
        wa_bkpf LIKE bkpf.

  DATA: l_indx(2) TYPE n,
        l_drseg_item(20) TYPE c.

* Local constants and data
  CONSTANTS: c_msgtyp LIKE bdcmsgcoll-msgtyp VALUE 'S',
             c_msgid  LIKE bdcmsgcoll-msgid  VALUE 'M8',
             c_msg60  LIKE bdcmsgcoll-msgnr  VALUE '060',
             c_msg75  LIKE bdcmsgcoll-msgnr  VALUE '075',
             c_msg97  LIKE bdcmsgcoll-msgnr  VALUE '097'.

* Get Apay data
  SELECT SINGLE * FROM /nrk/apayhd INTO ls_hd WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE invalid_record.
  ENDIF.

* Prepare special format amount
  IF ls_hd-wrbtr NE 0.
    PERFORM set_amount USING ls_hd-waers ls_hd-wrbtr lv_wrbtr.
  ENDIF.

* Prepare special format date
  IF NOT ls_hd-bldat IS INITIAL.
    PERFORM set_date  USING ls_hd-bldat lv_bldat.
  ENDIF.

* Get coding information
  IF NOT ls_hd-freight IS INITIAL.

    PERFORM set_amount USING ls_hd-waers ls_hd-freight lv_freight.

    IF ls_hd-freightgl IS INITIAL.
      SELECT SINGLE val1 FROM /nrk/apayconfig INTO lv_val1
        WHERE key1 EQ 'APAY'
        AND key2 EQ 'FREIGHTGL'.

      IF sy-subrc EQ 0.
        MOVE lv_val1 TO ls_hd-freightgl.
      ENDIF.
    ENDIF.

    IF ls_hd-freightcc IS INITIAL.
      SELECT SINGLE val1 FROM /nrk/apayconfig INTO lv_val1
        WHERE key1 EQ 'APAY'
        AND key2 EQ 'FREIGHTCC'.

      IF sy-subrc EQ 0.
        MOVE lv_val1 TO ls_hd-freightcc.
      ENDIF.
    ENDIF.

  ENDIF.

  IF NOT ls_hd-pallet IS INITIAL.
    PERFORM set_amount USING ls_hd-waers ls_hd-pallet lv_pallet.

    IF ls_hd-palletgl IS INITIAL.
      SELECT SINGLE val1 FROM /nrk/apayconfig INTO lv_val1
        WHERE key1 EQ 'APAY'
        AND key2 EQ 'PALLETGL'.

      IF sy-subrc EQ 0.
        MOVE lv_val1 TO ls_hd-palletgl.
      ENDIF.
    ENDIF.

    IF ls_hd-palletcc IS INITIAL.
      SELECT SINGLE val1 FROM /nrk/apayconfig INTO lv_val1
        WHERE key1 EQ 'APAY'
        AND key2 EQ 'PALLETCC'.

      IF sy-subrc EQ 0.
        MOVE lv_val1 TO ls_hd-palletcc.
      ENDIF.
    ENDIF.

  ENDIF.

*---------------------------------------------
* Start batch input table preparation
*---------------------------------------------
  IF ls_hd-bukrs NE space.
    SET PARAMETER ID 'BUK' FIELD ls_hd-bukrs.
  ENDIF.

  PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.

* Invoice date
  IF NOT lv_bldat IS INITIAL.
    PERFORM bdc_field  TABLES i_bdcdata USING 'INVFO-BLDAT' lv_bldat.
  ENDIF.

* Invoice number
  IF ls_hd-xblnr NE space.
    PERFORM bdc_field TABLES i_bdcdata USING 'INVFO-XBLNR' ls_hd-xblnr.
  ENDIF.

* Invoice or credit memo
  IF ls_hd-shkzg EQ 'H'.
* Credit memo
    PERFORM bdc_field TABLES i_bdcdata USING 'RM08M-VORGANG' '2'.
  ELSE.
    PERFORM bdc_field TABLES i_bdcdata USING 'RM08M-VORGANG' '1'.
  ENDIF.

  PERFORM bdc_field TABLES i_bdcdata USING 'BDC_SUBSCR' 'SAPLFDCB'.

* Invoice date
  IF NOT lv_bldat IS INITIAL.
    PERFORM bdc_field  TABLES i_bdcdata USING 'INVFO-BLDAT' lv_bldat.
  ENDIF.

* Amount
  IF lv_wrbtr NE space.
    PERFORM bdc_field TABLES i_bdcdata USING 'INVFO-WRBTR' lv_wrbtr.
  ENDIF.

* Currency
  IF ls_hd-waers NE space.
    PERFORM bdc_field TABLES i_bdcdata USING 'INVFO-WAERS' ls_hd-waers.
  ENDIF.

* Tax
  IF ls_hd-wmwst IS INITIAL.
    PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_CURSOR' 'INVFO-XMWST'.
    PERFORM bdc_field  TABLES i_bdcdata USING 'INVFO-XMWST' 'X'.
*   bdc_field   'BDC_CURSOR'    'INVFO-XMWST'.
*   bdc_field   'INVFO-XMWST'   'X'.              " calculate tax
  ELSE.
    WRITE: ls_hd-wmwst CURRENCY ls_hd-waers
      TO lv_tax_amount NO-GROUPING.
*   bdc_field   'INVFO-WMWST'  lv_tax_amount.     " input tax
    PERFORM bdc_field  TABLES i_bdcdata USING 'INVFO-WMWST' lv_tax_amount.
  ENDIF.

* Modify user's personal settings.
* TABLES: esdus.
  UPDATE esdus SET active = '1' WHERE
    uname = sy-uname AND
    action = 'MIRO' AND
    element = 'REFERENZBELEGTYP'.
  IF sy-subrc NE 0.
    esdus-uname = sy-uname.
    esdus-action = 'MIRO'.
    esdus-element = 'REFERENZBELEGTYP'.
    esdus-active = '1'.
    INSERT esdus.
  ENDIF.

* change tab 'details'
  PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_OKCODE' '=HEADER_FI'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_SUBSCR' 'SAPLMR1M'.
* PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_OKCODE' '/00'.

* Vendor
* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_SUBSCR' 'SAPLFDCB'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_CURSOR' 'INVFO-LIFRE'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'INVFO-LIFRE' ls_hd-lifnr.

* Purchase orders
* IF ls_hd-ebeln NE space.
*   PERFORM bdc_field TABLES i_bdcdata USING 'RM08M-EBELN' ls_hd-ebeln.
* ENDIF.

  PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_SUBSCR' 'SAPLMR1M'.
  PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_CURSOR' 'RM08M-EBELN'.
  PERFORM bdc_field  TABLES i_bdcdata USING 'RM08M-EBELN' ls_hd-ebeln.
  PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_SUBSCR' 'SAPLMR1M'.

* change tab 'payment'
* PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_OKCODE' '=HEADER_PAY'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_SUBSCR' 'SAPLMR1M'.
* PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_OKCODE' '/00'.

* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_SUBSCR' 'SAPLFDCB'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_CURSOR' 'INVFO-ZTERM'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'INVFO-ZTERM'  ls_hd-zterm.

* change back to first tab
* PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_OKCODE' '=HEADER_TOTAL'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_SUBSCR' 'SAPLMR1M'.
* PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_OKCODE' '/00'.

* deselect all lines
* PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
* PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_OKCODE' '=ADMA'.

  SET PARAMETER ID 'RBN' FIELD ls_hd-belnr.
  SET PARAMETER ID 'GJR' FIELD ls_hd-gjahr.

*** Line items processing

* Check for PO line items and prefill
  CALL FUNCTION '/NRK/APAY_GET_PO_ITEMS'
    EXPORTING
      apayno   = ls_hd-apayno
      ebeln    = ls_hd-ebeln
    TABLES
      po_items = i_po_items
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  IF sy-subrc <> 0.
    RAISE error.
  ENDIF.
""

  ""

  DESCRIBE TABLE i_po_items LINES lv_cnt.

  IF lv_cnt GT 0.

    LOOP AT i_po_items.

* Go to the correct line
      PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
      PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_CURSOR' 'RM08M-SKIP_TO'.
      PERFORM bdc_field  TABLES i_bdcdata USING 'RM08M-SKIP_TO' i_po_items-line.
      PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_OKCODE' '=POS'.

* Pre-fill in the line info
      PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
      PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_SUBSCR' 'SAPLMR1M'.

      IF i_po_items-menge GT 0.
        PERFORM bdc_field  TABLES i_bdcdata USING 'DRSEG-SELKZ(01)' 'X'.
*       CONCATENATE 'DRSEG-SELKZ(' l_indx ')' INTO l_drseg_item.
*       PERFORM bdc_field TABLES i_bdcdata USING l_drseg_item 'X'.
      ENDIF.

      WRITE i_po_items-wrbtr TO lv_field.
      PERFORM bdc_field  TABLES i_bdcdata USING 'DRSEG-WRBTR(01)' lv_field.
*     CONCATENATE 'DRSEG-WRBTR(' l_indx ')' INTO l_drseg_item.
*     PERFORM bdc_field TABLES i_bdcdata USING l_drseg_item lv_field.

      WRITE i_po_items-menge TO lv_field.
      PERFORM bdc_field  TABLES i_bdcdata USING 'DRSEG-MENGE(01)' lv_field.
*     CONCATENATE 'DRSEG-MENGE(' l_indx ')' INTO l_drseg_item.
*     PERFORM bdc_field TABLES i_bdcdata USING l_drseg_item lv_field.

      IF i_po_items-menge GT 0.
        PERFORM bdc_field  TABLES i_bdcdata USING'DRSEG-OK(01)' 'X'.
*       CONCATENATE 'DRSEG-OK(' l_indx ')' INTO l_drseg_item.
*       PERFORM bdc_field TABLES i_bdcdata USING l_drseg_item 'X'.
      ENDIF.

      bdc_field       'BDC_OKCODE'      '/00'.

       perform bdc_field    TABLES i_bdcdata     using 'RM08M-ITEM_LIST_VERSION'
                                                           '7_6310'.

    ENDLOOP.



*    perform bdc_dynpro    TABLES i_bdcdata  using 'SAPLMR1M' '6000'.
*    perform bdc_field   TABLES i_bdcdata    using 'BDC_OKCODE'
*                                  '=ITEMS_PO'.

*ENDDO.
"""


"""
*G/l tab
 PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
 PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_OKCODE' '=ITEMS_G/L'.
 PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_SUBSCR' 'SAPLMR1M'.
 PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
 PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_OKCODE' '/00'.
"""

*     bdc_field       'BDC_OKCODE' '=ITEMS_G/L'.

    perform bdc_field  TABLES i_bdcdata        using 'BDC_CURSOR'
                                  'ACGL_ITEM-HKONT(01)'.
    perform bdc_field  TABLES i_bdcdata       using 'ACGL_ITEM-HKONT(01)'
                                                      ls_hd-freightgl.
        perform bdc_field TABLES i_bdcdata       using 'ACGL_ITEM-WRBTR(01)'  lv_freight .
        perform bdc_field  TABLES i_bdcdata      using 'ACGL_ITEM-KOSTL(01)'  ls_hd-freightcc .
    perform bdc_field TABLES i_bdcdata       using 'ACGL_ITEM-HKONT(02)'
                                                      ls_hd-palletgl.

    perform bdc_field  TABLES i_bdcdata       using 'ACGL_ITEM-WRBTR(02)'
                                 lv_pallet.


    perform bdc_field  TABLES i_bdcdata      using 'ACGL_ITEM-KOSTL(02)'
                                                    ls_hd-palletcc.


"""""
*po tab
 PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
 PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_OKCODE' '=ITEMS_PO'.
 PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_SUBSCR' 'SAPLMR1M'.
 PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
 PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_OKCODE' '/00'.

* return to line 1
    bdc_screen      'SAPLMR1M'      '6000'.
    bdc_field       'BDC_CURSOR'    'RM08M-SKIP_TO'.
    bdc_field       'RM08M-SKIP_TO' '1'.
    bdc_field       'BDC_OKCODE'    '=POS'.

  ENDIF.





* BDC options
  i_bdcparam-dismode  = 'E'.            "Display mode
  i_bdcparam-updmode  = 'S'.            "Sync
  i_bdcparam-cattmode = ' '.
  i_bdcparam-defsize  = ' '.
  i_bdcparam-nobinpt = 'X'.
  i_bdcparam-nobiend  = ' '.

**---------------------------------------------
* Call transaction
*---------------------------------------------
  CALL TRANSACTION 'MIRO'   USING     i_bdcdata
                            OPTIONS FROM i_bdcparam
*                           MODE     mode
*                           UPDATE   'S'
                            MESSAGES INTO i_messages.

* Check if document posted
  READ TABLE i_messages WITH KEY msgid  = c_msgid msgnr  = c_msg60.

  IF sy-subrc EQ 0.
    status = 'P'.
    belnr = i_messages-msgv1.
*   GET PARAMETER ID 'RBN' FIELD belnr.
    GET PARAMETER ID 'GJR' FIELD gjahr.

    l_awkey    = belnr.
    l_awkey+10 = gjahr.

    DO 15 TIMES.

      SELECT SINGLE * FROM bkpf INTO wa_bkpf
        WHERE awkey EQ l_awkey.

      IF sy-subrc EQ 0.
        EXIT.
      ELSE.
        WAIT UP TO 2 SECONDS.
      ENDIF.

    ENDDO.

    EXIT.
  ENDIF.

* Check if document blocked
  READ TABLE i_messages WITH KEY msgid  = c_msgid msgnr  = c_msg75.
  IF sy-subrc EQ 0.
    status = 'B'.
    GET PARAMETER ID 'RBN' FIELD belnr.
    GET PARAMETER ID 'GJR' FIELD gjahr.
    EXIT.
  ENDIF.
* Check if document held
  READ TABLE i_messages WITH KEY msgid  = c_msgid msgnr  = c_msg97.
  IF sy-subrc EQ 0.
    status = 'H'.
    GET PARAMETER ID 'RBN' FIELD belnr.
    GET PARAMETER ID 'GJR' FIELD gjahr.
    EXIT.
  ELSE.
* Processing cancelled
    status = 'C'.
    CLEAR belnr.
    CLEAR gjahr.
    EXIT.
  ENDIF.

ENDFUNCTION.
