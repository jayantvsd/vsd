FUNCTION /nrk/apayrule_po_creator.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      AC_CONTAINER STRUCTURE  SWCONT
*"      ACTOR_TAB STRUCTURE  SWHACTOR
*"  EXCEPTIONS
*"      NO_NO_CREATOR_FOUND
*"      NO_VALID_APAY_RECORD_FOUND
*"----------------------------------------------------------------------

  INCLUDE <cntn01>.

  DATA: apayno   LIKE /nrk/apayhd-apayno,
        wa_hd    LIKE /nrk/apayhd,
        wa_ekko  LIKE ekko,
        wa_actor TYPE swhactor,
        lines    TYPE i.

  CLEAR: apayno,
         wa_hd,
         wa_ekko,
         wa_actor,
         lines.

  swc_get_element ac_container 'APayRecordID' apayno.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE no_valid_apay_record_found.
  ENDIF.

  IF NOT wa_hd-ebeln IS INITIAL.
    SELECT SINGLE * FROM ekko INTO wa_ekko
      WHERE ebeln EQ wa_hd-ebeln.

    IF NOT wa_ekko-ernam IS INITIAL.
      wa_actor-otype = 'US'.
      wa_actor-objid = wa_ekko-ernam.
      APPEND wa_actor TO actor_tab.
    ENDIF.
  ENDIF.

  DESCRIBE TABLE actor_tab LINES lines.
  IF lines IS INITIAL.

    SELECT otype objid FROM /nrk/apayuser
      INTO (wa_actor-otype, wa_actor-objid)
      WHERE bukrs EQ wa_hd-bukrs
       AND  buyer EQ 'X'.

      IF wa_actor-otype NE 'EX'.
        APPEND wa_actor TO actor_tab.
      ENDIF.
    ENDSELECT.

    DESCRIBE TABLE actor_tab LINES lines.
    IF lines IS INITIAL.
      RAISE no_po_creator_found.
    ENDIF.

  ENDIF.

  DELETE actor_tab WHERE objid IS INITIAL.

ENDFUNCTION.
