FUNCTION /nrk/apaystorecoding.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYHD) TYPE  /NRK/APAYHD OPTIONAL
*"  EXPORTING
*"     VALUE(PARKED) TYPE  BOOLE-BOOLE
*"  TABLES
*"      ITEMS STRUCTURE  /NRK/APAYITEMS OPTIONAL
*"      BDCMSGCOLL STRUCTURE  BDCMSGCOLL OPTIONAL
*"  EXCEPTIONS
*"      PARKING_FAILED
*"      CODING_COMPLETED
*"      LINE_ITEM_AMOUNT_MISSING
*"      COMPANY_CODE_MISSING
*"----------------------------------------------------------------------

  DATA: wa_item LIKE /nrk/apayitems,
         t_item LIKE /nrk/apayitems OCCURS 0,
         buzei   LIKE /nrk/apayitems-buzei.

  CLEAR: wa_item,
         t_item[],
         t_item,
         buzei.

* Add line items
  LOOP AT items.

    buzei = buzei + 1.

    wa_item-apayno = apayhd-apayno.
    wa_item-buzei  = buzei.

    IF items-bukrs IS INITIAL.
      RAISE company_code_missing.
    ELSE.
      wa_item-bukrs  = items-bukrs.
    ENDIF.
    wa_item-bschl  = items-bschl.
    wa_item-hkont  = items-hkont.
    wa_item-koart  = items-koart.

    IF NOT items-kostl EQ 'undefined'
      AND NOT items-kostl EQ 'null'
      AND NOT items-kostl IS INITIAL.

      wa_item-kostl  = items-kostl.

    ENDIF.

* Begin change of 12/13/2017

    IF NOT items-txjcd EQ 'undefined'
      AND NOT items-txjcd EQ 'null'
      AND NOT items-txjcd IS INITIAL.

      wa_item-txjcd  = items-txjcd.

    ENDIF.
* End of change 12/13/2017

    IF NOT items-matnr EQ 'undefined'
      AND NOT items-matnr EQ 'null'
      AND NOT items-matnr IS INITIAL.

      wa_item-matnr  = items-matnr.

    ENDIF.

    wa_item-mwskz  = items-mwskz.
    wa_item-prctr  = items-prctr.

    IF NOT items-projk EQ 'undefined'
      AND NOT items-projk EQ 'null'
      AND NOT items-projk IS INITIAL.

* change start 12/30/2015

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          input     = items-projk
        IMPORTING
          output    = wa_item-pspnr
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

* change end 12/30/2015

      wa_item-projk  = items-projk.

    ENDIF.

    IF NOT items-aufnr EQ 'undefined'
      AND NOT items-aufnr EQ 'null'
      AND NOT items-aufnr IS INITIAL.

      wa_item-aufnr  = items-aufnr.

    ENDIF.

    IF NOT items-sgtxt EQ 'undefined'
      AND NOT items-sgtxt EQ 'null'
      AND NOT items-sgtxt IS INITIAL.

      wa_item-sgtxt  = items-sgtxt.

    ENDIF.

    IF NOT items-wrbtr IS INITIAL
      OR items-wrbtr NE 0.
      wa_item-wrbtr  = items-wrbtr.
    ELSE.
      RAISE line_item_amount_missing.
    ENDIF.

    IF items-shkzg = 'D'.
      wa_item-shkzg = 'S'.
    ELSEIF items-shkzg = 'C'.
      wa_item-shkzg = 'H'.
    ENDIF.

    APPEND wa_item TO t_item.
    CLEAR: wa_item.

  ENDLOOP.

  SELECT SINGLE * FROM /nrk/apayitems INTO wa_item
    WHERE apayno EQ apayhd-apayno.

*** start change 10/14/2015

  IF sy-subrc EQ 0.
    DELETE FROM /nrk/apayitems
      WHERE apayno EQ apayhd-apayno.
    COMMIT WORK.
  ENDIF.
  INSERT /nrk/apayitems FROM TABLE t_item.
  COMMIT WORK AND WAIT.
  .


*  IF sy-subrc NE 0.
*
** Update DB table
** MODIFY /nrk/apayitems FROM TABLE t_item.
*    INSERT /nrk/apayitems FROM TABLE t_item.
*
** Write to DB and wait until data is written to DB
*    COMMIT WORK AND WAIT.
*
*  ELSE.
*    RAISE coding_completed.
*  ENDIF.

*** end change 10/14/2015

ENDFUNCTION.
