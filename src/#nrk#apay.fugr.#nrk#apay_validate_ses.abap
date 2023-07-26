FUNCTION /nrk/apay_validate_ses.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(EBELN) TYPE  EBELN
*"  EXPORTING
*"     REFERENCE(RESULT) TYPE  NUM1
*"     REFERENCE(ERNAM) TYPE  CHAR12
*"----------------------------------------------------------------------

* Result 1: No service entry sheet exists
* Result 2: Services partially received
*           Total SES value: ESSR-LWERT
* Result 3: All services received
* Result 4: More services received than on PO

  DATA: it_essr LIKE essr OCCURS 0,
        wa_essr LIKE essr,
        it_ekpo LIKE ekpo OCCURS 0,
        wa_ekpo LIKE ekpo,
        po_total TYPE wrbtr,
        ses_total TYPE wrbtr.

  CLEAR: po_total,
         ses_total.

* Get SES header
  SELECT * FROM essr INTO TABLE it_essr
    WHERE ebeln EQ ebeln.

  IF sy-subrc NE 0. " No SES exists
    result = 1.
    EXIT.
  ENDIF.

  LOOP AT it_essr INTO wa_essr.
    ses_total = ses_total + wa_essr-lwert.
    ernam = wa_essr-ernam.
  ENDLOOP.

* Get PO line items
  SELECT * FROM ekpo INTO TABLE it_ekpo
    WHERE ebeln EQ ebeln.

  LOOP AT it_ekpo INTO wa_ekpo.
    po_total = po_total + wa_ekpo-netwr.
  ENDLOOP.

  IF ses_total EQ po_total.
    result = 3.
  ELSEIF ses_total < po_total.
    result = 2.
  ELSEIF ses_total > po_total.
    result = 4.
  ENDIF.

ENDFUNCTION.
