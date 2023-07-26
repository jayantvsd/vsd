FUNCTION /nrk/apay_call_miro.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"     REFERENCE(MODE) TYPE  CHAR1 DEFAULT 'E'
*"  EXPORTING
*"     REFERENCE(BELNR) TYPE  BELNR_D
*"     REFERENCE(GJAHR) TYPE  GJAHR
*"     REFERENCE(STATUS) TYPE  CHAR1
*"  EXCEPTIONS
*"      INVALID_RECORD
*"----------------------------------------------------------------------

* Data declarations
  DATA: ls_hd TYPE /nrk/apayhd.

  DATA: lv_wrbtr(16) TYPE c,
        lv_bldat     LIKE sy-datum,
        lv_tax_amount(10) TYPE c.

  DATA: BEGIN OF i_messages OCCURS 0.
          INCLUDE STRUCTURE bdcmsgcoll.
  DATA: END OF i_messages.
  DATA: BEGIN OF i_bdcdata OCCURS 0.
          INCLUDE STRUCTURE bdcdata.
  DATA: END OF i_bdcdata.
  DATA: BEGIN OF i_bdcparam OCCURS 0.
          INCLUDE STRUCTURE ctu_params.
  DATA: END OF i_bdcparam.

* Local constants and data
  CONSTANTS: c_msgtyp LIKE bdcmsgcoll-msgtyp VALUE 'S',
             c_msgid  LIKE bdcmsgcoll-msgid  VALUE 'M8',
  c_msg60  LIKE bdcmsgcoll-msgnr  VALUE '060',
  c_msg75  LIKE bdcmsgcoll-msgnr  VALUE '075',
  c_msg97  LIKE bdcmsgcoll-msgnr  VALUE '097'.

  CLEAR: lv_wrbtr,
         lv_tax_amount.

* Get Apay data
  SELECT SINGLE * FROM /nrk/apayhd INTO ls_hd WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE invalid_record.
  ENDIF.

* Prepare special format amount
  IF ls_hd-wrbtr NE 0.
    PERFORM set_amount USING ls_hd-waers ls_hd-wrbtr lv_wrbtr.
  ENDIF.

* Prepare special format tax amount
  IF ls_hd-wmwst NE 0.
    PERFORM set_amount USING ls_hd-waers ls_hd-wmwst lv_tax_amount.
  ENDIF.

* Prepare special format date
  IF NOT ls_hd-bldat IS INITIAL.
    PERFORM set_date  USING ls_hd-bldat lv_bldat.
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
* Amount
  IF lv_wrbtr NE space.
    PERFORM bdc_field TABLES i_bdcdata USING 'INVFO-WRBTR' lv_wrbtr.
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

  PERFORM bdc_dynpro TABLES i_bdcdata USING 'SAPLMR1M' '6000'.
  PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_SUBSCR' 'SAPLMR1M'.
  PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_CURSOR' 'RM08M-EBELN'.
  PERFORM bdc_field  TABLES i_bdcdata USING 'RM08M-EBELN' ls_hd-ebeln.
  PERFORM bdc_field  TABLES i_bdcdata USING 'BDC_SUBSCR' 'SAPLMR1M'.

  SET PARAMETER ID 'RBN' FIELD ls_hd-belnr.
  SET PARAMETER ID 'GJR' FIELD ls_hd-gjahr.

* BDC options
  i_bdcparam-dismode  = 'E'.            "Display mode
  i_bdcparam-updmode  = 'S'.            "Sync
  i_bdcparam-cattmode = ' '.
  i_bdcparam-defsize  = ' '.
  i_bdcparam-nobinpt = 'X'.
  i_bdcparam-nobiend  = ' '.

*---------------------------------------------
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
    GET PARAMETER ID 'RBN' FIELD belnr.
    GET PARAMETER ID 'GJR' FIELD gjahr.
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
