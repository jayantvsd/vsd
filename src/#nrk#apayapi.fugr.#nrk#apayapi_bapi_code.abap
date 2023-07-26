FUNCTION /nrk/apayapi_bapi_code.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYHD) LIKE  /NRK/APAYAPI_HD STRUCTURE  /NRK/APAYAPI_HD
*"       OPTIONAL
*"     VALUE(COMMENT) TYPE  /NRK/APAYCOMMENT OPTIONAL
*"     VALUE(EXTUSER) TYPE  /NRK/APAYEXUSER OPTIONAL
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"      ITEMS STRUCTURE  /NRK/APAYITEMS OPTIONAL
*"----------------------------------------------------------------------

  DATA: wa_apayhd       LIKE /nrk/apayhd,
        wa_approverlist LIKE /nrk/apayapistoreappr,
        parked          LIKE boole-boole,
        bdcmsgcoll      LIKE bdcmsgcoll OCCURS 0.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_apayhd
    WHERE apayno EQ apayhd-apayno.

* check, if document was already coded
  SELECT SINGLE * FROM vbsegs
    WHERE ausbk = wa_apayhd-bukrs
      AND belnr = wa_apayhd-belnr
      AND gjahr = wa_apayhd-gjahr.

  IF sy-subrc EQ 0.
* Items are already coded, do not change anything

    messages-msg_type = 'I'.
    messages-msg_nbr = '034'.
    messages-msg_text = text-034.
    APPEND messages.
  ENDIF.

* store line item data in APay Center
  IF NOT items[] IS INITIAL.

    CALL FUNCTION '/NRK/APAYSTORECODING_V2'
      EXPORTING
        apayhd                   = wa_apayhd
      TABLES
        items                    = items
      EXCEPTIONS
        line_item_amount_missing = 1
        company_code_missing     = 2
        OTHERS                   = 3.

    IF sy-subrc NE 0.
      IF sy-subrc EQ 1.
        messages-msg_type = 'E'.
        messages-msg_nbr = '038'.
        messages-msg_text = text-038.
        APPEND messages.
        EXIT.
      ELSEIF sy-subrc EQ 2.
        messages-msg_type = 'E'.
        messages-msg_nbr = '042'.
        messages-msg_text = text-042.
        APPEND messages.
        EXIT.
      ELSE.
        messages-msg_type = 'E'.
        messages-msg_nbr = '031'.
        messages-msg_text = text-031.
        APPEND messages.
        EXIT.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFUNCTION.
