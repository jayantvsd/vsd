FUNCTION /nrk/apayapi_get_details.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYNO) TYPE  /NRK/APAYHD-APAYNO OPTIONAL
*"  EXPORTING
*"     VALUE(APAYHD) TYPE  /NRK/APAYAPI_HD
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES
*"--------------------------------------------------------------------

  DATA: wa_hd LIKE /nrk/apayhd.

  CLEAR: wa_hd.

* SELECT SINGLE * FROM /nrk/apayhd INTO CORRESPONDING FIELDS OF apayhd
*   WHERE apayno EQ apayno.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '003'.
    messages-msg_text = text-003.
    APPEND messages.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING wa_hd TO apayhd.

* Get Status description
  SELECT SINGLE sdescr FROM /nrk/apaysdef INTO apayhd-sdescr
    WHERE status EQ apayhd-status
      AND langu  EQ sy-langu.

* Get Document type description
  SELECT SINGLE objecttext FROM toasp INTO apayhd-objecttext
    WHERE ar_object = apayhd-ar_object
      AND language = sy-langu.

ENDFUNCTION.
