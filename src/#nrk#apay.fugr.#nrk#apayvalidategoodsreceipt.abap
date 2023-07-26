FUNCTION /nrk/apayvalidategoodsreceipt.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"  EXPORTING
*"     REFERENCE(GR_RESULT) TYPE  SYST-INPUT
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: ebeln         TYPE ebeln,
         wa_ekko       LIKE ekko,
         wa_ekpo       LIKE ekpo,
         wa_ekbe       LIKE ekbe,
         t_ekbe        LIKE ekbe OCCURS 0,
         t_ekpo        LIKE ekpo OCCURS 0,
         lv_menge      TYPE menge_d,
         lv_menge_we   TYPE menge_d,
         lv_menge_we2  TYPE menge_d,
         lv_menge_re   TYPE menge_d,
         lv_menge_po   TYPE menge_d,
         lv_shkzg      TYPE shkzg,
         lv_elikz      LIKE ekbe-elikz,
         lv_ebelp      LIKE ekpo-ebelp,
         goodsrcvd     LIKE ekbe-menge,   "goods received in units
         goodsinvoiced LIKE ekbe-menge,   "goods invoiced in units
         result        TYPE  char1,
         l_ekbe        LIKE ekbe-bpmng.

  DATA: BEGIN OF sums OCCURS 4,
          bewtp LIKE ekbe-bewtp,
          menge LIKE ekbe-menge,
          shkzg LIKE ekbe-shkzg,          "Debt/Credit indicator
        END OF sums.

  DATA: BEGIN OF items OCCURS 0,
          ebeln    LIKE ekko-ebeln,
          ebelp    LIKE ekpo-ebelp,
          po_menge LIKE ekpo-menge,
          gr_menge LIKE ekbe-menge,
          re_menge LIKE ekbe-menge,
          elikz    LIKE ekpo-elikz,
          validate LIKE syst-input,
        END OF items.

  DATA: t_items LIKE items OCCURS 0.

  CLEAR: gr_result,
         ebeln,
         wa_ekko,
         wa_ekpo,
         t_ekpo[],
         wa_ekbe,
         lv_menge,
         lv_menge_we,
         lv_menge_re,
         lv_menge_po,
         lv_shkzg.

  SELECT SINGLE ebeln FROM /nrk/apayhd INTO ebeln
    WHERE apayno EQ apayno.

* check if PO exists in APay record
  IF ebeln IS INITIAL.
    gr_result = '9'.
    EXIT.
  ENDIF.

* Check if purchase order number is valid
  SELECT SINGLE * FROM ekko INTO wa_ekko WHERE ebeln EQ ebeln.

  IF sy-subrc NE 0.
    gr_result = '9'.
    EXIT.
  ENDIF.

* Check if invoice verification is needed
  SELECT * FROM ekpo INTO TABLE t_ekpo WHERE ebeln EQ ebeln
    AND webre EQ 'X'.

  IF sy-subrc NE 0. " No invoice verification needed
    gr_result = '3'.
    EXIT.
  ELSE.
    LOOP AT t_ekpo INTO wa_ekpo.
      lv_menge_po = lv_menge_po + wa_ekpo-menge.
      items-ebeln = wa_ekpo-ebeln.
      items-ebelp = wa_ekpo-ebelp.
      items-po_menge = wa_ekpo-menge.
      items-elikz = wa_ekpo-elikz.
      APPEND items TO t_items.
    ENDLOOP.
  ENDIF.

* Check if we need a goods receipt
  SELECT SINGLE * FROM ekpo INTO wa_ekpo WHERE ebeln EQ ebeln
                                           AND wepos EQ 'X'.
  IF sy-subrc NE 0.
* Nothing found, no goods receipt needed
    gr_result = '2'.
    EXIT.
  ELSE.
* Get purchase order history
    CLEAR: lv_menge, lv_menge_we, lv_menge_re, lv_elikz.

* Get GR quantity
    SELECT ebelp shkzg menge INTO (lv_ebelp, lv_shkzg, lv_menge)
      FROM ekbe WHERE ebeln EQ ebeln AND vgabe EQ '1'.
      IF lv_shkzg EQ 'H'.
* Credit with negative amount
        lv_menge = lv_menge * ( -1 ).
      ENDIF.

      READ TABLE t_items INTO items WITH KEY ebelp = lv_ebelp.
      items-gr_menge = items-gr_menge + lv_menge.
      MODIFY t_items FROM items INDEX sy-tabix.

    ENDSELECT.

    CLEAR: lv_menge, lv_shkzg.

* Get RE quantity
    SELECT ebelp shkzg menge INTO (lv_ebelp, lv_shkzg, lv_menge)
      FROM ekbe WHERE ebeln EQ ebeln AND vgabe EQ '2'.
      IF lv_shkzg EQ 'H'.
* Credit with negative amount
        lv_menge = lv_menge * ( -1 ).
      ENDIF.

      READ TABLE t_items INTO items WITH KEY ebelp = lv_ebelp.
      items-re_menge = items-re_menge + lv_menge.
      MODIFY t_items FROM items INDEX sy-tabix.

    ENDSELECT.

*** Validate GR scenarios
    LOOP AT t_items INTO items.

* GR scenario 1
      IF items-po_menge  > 0
        AND items-gr_menge = 0
        AND items-re_menge = 0.  " Waiting for GR

        items-validate = '1'.
        MODIFY t_items FROM items.

* GR scenario 2
      ELSEIF items-po_menge > items-gr_menge
        AND items-gr_menge > items-re_menge. " 0 - Open goods receipt found

        items-validate = '0'.
        MODIFY t_items FROM items.

* GR scenario 3
      ELSEIF items-po_menge > items-gr_menge
        AND items-gr_menge = items-re_menge. " 1 - Waiting for GR

        IF items-elikz = 'X'.
          items-validate = '4'. " all goods invoiced
        ELSE.
          items-validate = '1'. " Waiting for GR
        ENDIF.

        MODIFY t_items FROM items.

* GR scenario 4&5
      ELSEIF items-po_menge = items-gr_menge
        AND items-gr_menge > items-re_menge. " 0 - Open goods receipt found

        items-validate = '0'.
        MODIFY t_items FROM items.

* GR scneario 6
      ELSEIF items-po_menge > 0
        AND items-po_menge = items-gr_menge
        AND items-gr_menge = items-re_menge. " 4 - All received goods invoiced

        items-validate = '4'.
        MODIFY t_items FROM items.

* GR scenario 7
      ELSEIF items-po_menge < items-gr_menge
         AND items-gr_menge = items-re_menge. " 4 - All received goods invoiced

        items-validate = '4'.
        MODIFY t_items FROM items.

      ELSEIF items-po_menge < items-gr_menge
         AND items-gr_menge > items-re_menge. " 0 - Open goods receipt found

        items-validate = '0'.
        MODIFY t_items FROM items.

      ELSEIF items-po_menge = 0. " 2 - No GR needed

        items-validate = '2'.
        MODIFY t_items FROM items.

      ENDIF.

    ENDLOOP.

    READ TABLE t_items INTO items WITH KEY validate = '2'.
    IF sy-subrc = 0. " No GR needed
      gr_result = '2'.
    ENDIF.

    READ TABLE t_items INTO items WITH KEY validate = '4'.
    IF sy-subrc = 0. " All received goods invoiced
      gr_result = '4'.
    ENDIF.

    READ TABLE t_items INTO items WITH KEY validate = '1'.
    IF sy-subrc = 0. " Waiting for GR
      gr_result = '1'.
    ENDIF.

    READ TABLE t_items INTO items WITH KEY validate = '0'.
    IF sy-subrc = 0. " Open goods receipt found
      gr_result = '0'.
    ENDIF.

    CLEAR: lv_menge, lv_shkzg, lv_menge_po, lv_menge_we, lv_menge_re.

  ENDIF.

ENDFUNCTION.
