FUNCTION /nrk/apayrule_ap_processor.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      AC_CONTAINER STRUCTURE  SWCONT
*"      ACTOR_TAB STRUCTURE  SWHACTOR
*"  EXCEPTIONS
*"      NO_AP_PROCESSOR_FOUND
*"      NO_VALID_APAY_RECORD_FOUND
*"----------------------------------------------------------------------

  INCLUDE <cntn01>.

  DATA: apayno LIKE /nrk/apayhd-apayno,
        wa_hd  LIKE /nrk/apayhd,
        wa_actor TYPE swhactor,
        lines TYPE i.

  CLEAR: apayno,
         wa_hd,
         wa_actor,
         lines.

  swc_get_element ac_container 'APayRecordID' apayno.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE no_valid_apay_record_found.
  ENDIF.

  IF NOT wa_hd-bukrs IS INITIAL. " with company code

    SELECT otype objid FROM /nrk/apayuser
      INTO (wa_actor-otype, wa_actor-objid)
      WHERE bukrs EQ wa_hd-bukrs
       AND  processor EQ 'X'.

      IF wa_actor-otype NE 'EX'.
        APPEND wa_actor TO actor_tab.
      ENDIF.
    ENDSELECT.

  ENDIF.

  DESCRIBE TABLE actor_tab LINES lines.
  IF lines IS INITIAL.

    SELECT otype objid FROM /nrk/apayuser
    INTO (wa_actor-otype, wa_actor-objid)
    WHERE bukrs EQ space
     AND  processor EQ 'X'.

      IF NOT wa_actor-objid IS INITIAL.
*       wa_actor-otype = 'US'.
*     IF wa_actor-otype NE 'EX'.
        APPEND wa_actor TO actor_tab.
*     ENDIF.
      ENDIF.
    ENDSELECT.

    DESCRIBE TABLE actor_tab LINES lines.

    IF lines IS INITIAL.
      RAISE no_ap_processor_found.
    ENDIF.

  ENDIF.

  DELETE ADJACENT DUPLICATES FROM actor_tab
     COMPARING otype objid.

  DELETE actor_tab WHERE otype EQ 'EX'.

ENDFUNCTION.
