FUNCTION /NRK/APAYAPI_BAPI_GET_COMMENTS.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYNO) TYPE  /NRK/APAYHD-APAYNO OPTIONAL
*"  TABLES
*"      COMMENTS STRUCTURE  /NRK/APAYCMNT
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES
*"--------------------------------------------------------------------

  SELECT * FROM /nrk/apaycmnt INTO TABLE comments
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0. "no comments
    messages-msg_type = 'E'.
    messages-msg_nbr = '020'.
    messages-msg_text = text-020.
    APPEND messages.
    EXIT.
  ENDIF.

ENDFUNCTION.
