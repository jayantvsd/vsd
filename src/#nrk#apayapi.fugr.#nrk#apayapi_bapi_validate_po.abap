FUNCTION /nrk/apayapi_bapi_validate_po.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_PODATA) TYPE  /NRK/APAYPOVAL OPTIONAL
*"  EXPORTING
*"     VALUE(E_PODATA) TYPE  /NRK/APAYPOVAL
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES
*"----------------------------------------------------------------------

  DATA: wa_ekko LIKE ekko.

  CLEAR: wa_ekko.

  SELECT SINGLE * FROM ekko INTO wa_ekko
    WHERE ebeln EQ i_podata-ebeln
      AND bstyp EQ 'F'.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '016'.
    messages-msg_text = text-016.
    APPEND messages.
    EXIT.
  ELSE.
    e_podata-ebeln = i_podata-ebeln.
  ENDIF.

  IF i_podata-bukrs NE wa_ekko-bukrs
    AND NOT i_podata-bukrs IS INITIAL.
    messages-msg_type = 'E'.
    messages-msg_nbr = '017'.
    messages-msg_text = text-017.
    APPEND messages.
    EXIT.
  ELSE.
    e_podata-bukrs = wa_ekko-bukrs.
  ENDIF.

  IF i_podata-lifnr NE wa_ekko-lifre
    AND NOT i_podata-lifnr IS INITIAL.
    messages-msg_type = 'E'.
    messages-msg_nbr = '018'.
    messages-msg_text = text-018.
    APPEND messages.
    EXIT.
  ELSEIF i_podata-lifnr IS INITIAL.
    e_podata-lifnr = wa_ekko-lifnr.

    SELECT SINGLE name1 FROM lfa1 INTO e_podata-lifname
      WHERE lifnr EQ e_podata-lifnr.

  ENDIF.

ENDFUNCTION.
