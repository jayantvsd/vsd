FUNCTION /nrk/apayapi_authorize_bukrs.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS
*"  EXPORTING
*"     REFERENCE(NOT_AUTHORIZED) TYPE  BOOLE-BOOLE
*"----------------------------------------------------------------------

  DATA: lt_config LIKE /nrk/apayconfig OCCURS 0,
        wa_config LIKE /nrk/apayconfig,
        l_bukrs   LIKE bkpf-bukrs.

* Initialize authorization
  not_authorized = 'X'.

* Get authorized company codes from configuration
  SELECT * FROM /nrk/apayconfig INTO TABLE lt_config
    WHERE key1 EQ 'APAYFILTER'.

  LOOP AT lt_config INTO wa_config.
    IF wa_config-key2 CN 'BUKRS'.
      MOVE wa_config-val1(5) TO l_bukrs.
      IF bukrs EQ l_bukrs. "authorized
        not_authorized = space.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
