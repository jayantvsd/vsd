FUNCTION /nrk/apay_get_po_items.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"     REFERENCE(EBELN) TYPE  /NRK/APAYHD-EBELN
*"  TABLES
*"      PO_ITEMS STRUCTURE  /NRK/APAYPOITEMS
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

* This function performs po line item matching and
* builds the po line items that
* will later be used for pre-filling transaction MIRO

  DATA: l_cnt      TYPE i,
        l_index    TYPE i,
        l_wrbtr    TYPE wrbtr,
        l_gr_menge TYPE bstmg.

  DATA: BEGIN OF itab_items
              OCCURS 100,
        ebelp TYPE ebelp,
        menge TYPE bstmg,
        wrbtr TYPE wrbtr,
        netpr TYPE bprei,
        matnr TYPE matnr,
        sgtxt TYPE sgtxt,
        flag  TYPE c,
        END OF   itab_items.

  DATA: itab LIKE /nrk/apayitems
             OCCURS 100
             WITH HEADER LINE.

  DATA: itab_wa LIKE /nrk/apayitems.

  DATA: l_po_number    LIKE bapiekko-po_number,
        l_mat_descr(40) TYPE c.

  DATA: itab_po_items  LIKE bapiekpo
        OCCURS 100
        WITH HEADER LINE.

  DATA: itab_po_items_saved  LIKE bapiekpo
        OCCURS 100
        WITH HEADER LINE.

  DATA: itab_his_totals LIKE bapiekbes
        OCCURS 100
        WITH HEADER LINE.

  DATA: itab_bapiret    LIKE bapireturn
        OCCURS 10
        WITH HEADER LINE.

* clear passed table.

  CLEAR: po_items[], po_items.
  CLEAR: itab[], itab.
  CLEAR: itab_items[], itab_items.

  SELECT * INTO TABLE itab FROM /nrk/apayitems
    WHERE apayno = apayno.

  LOOP AT itab.
    MOVE itab-buzei TO itab_items-ebelp.
    itab_items-menge = itab-menge.
    itab_items-wrbtr = itab-wrbtr.
    itab_items-netpr = itab-wrbtr.
    itab_items-matnr = itab-matnr.
    APPEND itab_items.
  ENDLOOP.

* sanity checks - this should not happen

  SORT itab_items BY ebelp.

  DESCRIBE TABLE itab_items LINES l_cnt.
  READ TABLE itab_items INDEX l_cnt.
  IF itab_items-ebelp NE l_cnt.
*   EXIT.
  ENDIF.

* at this point we have all the line item data
* that was scanned in

* now get PO data

  l_po_number = ebeln.

  CALL FUNCTION 'BAPI_PO_GETDETAIL'
    EXPORTING
      purchaseorder          = l_po_number
      items                  = 'X'
      history                = 'X'
    TABLES
      po_items               = itab_po_items
      po_item_history_totals = itab_his_totals
      return                 = itab_bapiret.


  READ TABLE itab_bapiret WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    EXIT.
  ENDIF.

* save line items

  SORT itab_po_items BY po_item.

  itab_po_items_saved[] = itab_po_items[].

* get receipts and save only the latest

  SORT itab_his_totals BY po_item ASCENDING
                          serial_no DESCENDING.

  DELETE ADJACENT DUPLICATES FROM itab_his_totals COMPARING po_item.

* start matching

  LOOP AT itab_items.

* first test. find unit price match and quantity

    CLEAR: po_items.

    CLEAR: l_index, l_cnt.

    LOOP AT itab_po_items
      WHERE net_price = itab_items-netpr
        AND quantity = itab_items-menge.

      l_index = itab_po_items-po_item.
      ADD +1 TO l_cnt.

    ENDLOOP.

*   if match found then save data and remove
*   matched item

    IF l_cnt EQ 1.

      READ TABLE itab_po_items
        WITH KEY po_item = l_index
        BINARY SEARCH.
      po_items-ebelp = itab_po_items-po_item.
      po_items-menge = itab_items-menge.
      po_items-wrbtr = itab_items-wrbtr.
      APPEND po_items.

      itab_items-flag = 'X'.         " mark as matched
      MODIFY itab_items.

      DELETE itab_his_totals WHERE po_item = itab_po_items-po_item.
      DELETE itab_po_items WHERE po_item = itab_po_items-po_item.

      CONTINUE.

    ENDIF.

* second test. search for matching total amount

    CLEAR: l_index, l_cnt.

    LOOP AT itab_po_items
      WHERE net_value = itab_items-wrbtr.

      l_index = itab_po_items-po_item.
      ADD +1 TO l_cnt.

    ENDLOOP.

*   if match found then save

    IF l_cnt EQ 1.

      READ TABLE itab_po_items
        WITH KEY po_item = l_index
        BINARY SEARCH.
      po_items-ebelp = itab_po_items-po_item.
      po_items-menge = itab_items-menge.
      po_items-wrbtr = itab_items-wrbtr.
      APPEND po_items.

      itab_items-flag = 'X'.         " mark as matched
      MODIFY itab_items.

      DELETE itab_his_totals WHERE po_item = itab_po_items-po_item.
      DELETE itab_po_items WHERE po_item = itab_po_items-po_item.

      CONTINUE.

    ENDIF.

* third test. look at gr - ir and netprice

    CLEAR: l_index, l_cnt.

    LOOP AT itab_his_totals.

      l_gr_menge = itab_his_totals-deliv_qty - itab_his_totals-iv_qty.

      IF l_gr_menge = itab_items-menge.

        READ TABLE itab_po_items INDEX itab_his_totals-po_item.

        IF sy-subrc EQ 0
           AND itab_po_items-net_price = itab_items-netpr.

          l_index = itab_his_totals-po_item.
          ADD +1 TO l_cnt.

        ENDIF.

      ENDIF.

    ENDLOOP.

*   if match found then save

    IF l_cnt EQ 1.

      READ TABLE itab_po_items
        WITH KEY po_item = l_index
        BINARY SEARCH.
      po_items-ebelp = itab_po_items-po_item.
      po_items-menge = itab_items-menge.
      po_items-wrbtr = itab_items-wrbtr.
      APPEND po_items.

      itab_items-flag = 'X'.         " mark as matched
      MODIFY itab_items.

      DELETE itab_his_totals WHERE po_item = itab_po_items-po_item.
      DELETE itab_po_items WHERE po_item = itab_po_items-po_item.

      CONTINUE.

    ENDIF.

* fourth test. match material number

    CLEAR: l_index, l_cnt.

    IF NOT itab_items-matnr IS INITIAL.

      LOOP AT itab_po_items
        WHERE material = itab_items-matnr.

        l_index = itab_po_items-po_item.
        ADD +1 TO l_cnt.

      ENDLOOP.

    ENDIF.

*   if match found then save

    IF l_cnt EQ 1.

      READ TABLE itab_po_items
        WITH KEY po_item = l_index
        BINARY SEARCH.
      po_items-ebelp = itab_po_items-po_item.
      po_items-menge = itab_items-menge.
      po_items-wrbtr = itab_items-wrbtr.
      APPEND po_items.

      itab_items-flag = 'X'.         " mark as matched
      MODIFY itab_items.

      DELETE itab_his_totals WHERE po_item = itab_po_items-po_item.
      DELETE itab_po_items WHERE po_item = itab_po_items-po_item.

      CONTINUE.

    ENDIF.


* fifth test. match material number against manufacture part number (MPN)

    CLEAR: l_index, l_cnt.

    LOOP AT itab_po_items
      WHERE manu_mat = itab_items-matnr.

      l_index = itab_po_items-po_item.
      ADD +1 TO l_cnt.

    ENDLOOP.

*   if match found then save

    IF l_cnt EQ 1.

      READ TABLE itab_po_items
        WITH KEY po_item = l_index
        BINARY SEARCH.
      po_items-ebelp = itab_po_items-po_item.
      po_items-menge = itab_items-menge.
      po_items-wrbtr = itab_items-wrbtr.
      APPEND po_items.

      itab_items-flag = 'X'.         " mark as matched
      MODIFY itab_items.

      DELETE itab_his_totals WHERE po_item = itab_po_items-po_item.
      DELETE itab_po_items WHERE po_item = itab_po_items-po_item.

      CONTINUE.

    ENDIF.

* sixth test. match line item description

    CLEAR: l_index, l_cnt.

    MOVE itab_items-sgtxt TO l_mat_descr.
    LOOP AT itab_po_items
      WHERE short_text = l_mat_descr.

      l_index = itab_po_items-po_item.
      ADD +1 TO l_cnt.

    ENDLOOP.

*   if match found then save

    IF l_cnt EQ 1.

      READ TABLE itab_po_items
        WITH KEY po_item = l_index
        BINARY SEARCH.
      po_items-ebelp = itab_po_items-po_item.
      po_items-menge = itab_items-menge.
      po_items-wrbtr = itab_items-wrbtr.
      APPEND po_items.

      itab_items-flag = 'X'.         " mark as matched
      MODIFY itab_items.

      DELETE itab_his_totals WHERE po_item = itab_po_items-po_item.
      DELETE itab_po_items WHERE po_item = itab_po_items-po_item.

      CONTINUE.

    ENDIF.

  ENDLOOP.

*
* look at items remaining on the PO
* and set to zero on posting
*


  LOOP AT itab_po_items.

    READ TABLE po_items WITH KEY ebelp = itab_po_items-po_item.

    IF sy-subrc <> 0.

      po_items-ebelp = itab_po_items-po_item.
      po_items-menge = '0'.
      po_items-wrbtr = '0.0'.
      APPEND po_items.

    ENDIF.

  ENDLOOP.

  SORT po_items BY ebelp.

* final check....
* see if there are any items remaining that
* we did not find a match for....
* if we have unmatched items then bail out

  LOOP AT itab_items WHERE flag LT 'X'.

    CLEAR: po_items, po_items[].
    EXIT.

  ENDLOOP.

*
* now assign screen position line numbers
* to each line item. This is an important
* step as each line item number may not
* equate directly to its position screen.
* We need the screen position for posting.
*

  LOOP AT po_items.

    READ TABLE itab_po_items_saved
        WITH KEY po_item = po_items-ebelp
        BINARY SEARCH.

    po_items-line = sy-tabix.
    MODIFY po_items.

  ENDLOOP.

ENDFUNCTION.
