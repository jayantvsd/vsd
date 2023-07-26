FUNCTION /nrk/apayapi_bapi_get_doctypes.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      DOCUMENTTYPES STRUCTURE  /NRK/APAYDOCTYPES OPTIONAL
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"----------------------------------------------------------------------

  DATA: t_dtypes         LIKE /nrk/apaydtype OCCURS 0,
        wa_dtypes        LIKE /nrk/apaydtype,
        wa_documenttypes LIKE /nrk/apaydoctypes.

  SELECT * FROM /nrk/apaydtype INTO TABLE t_dtypes.

  IF sy-subrc NE 0. " No document types configured
    messages-msg_type = 'E'.
    messages-msg_nbr = '004'.
    messages-msg_text = text-004.
    APPEND messages.
    EXIT.
  ENDIF.

  LOOP AT t_dtypes INTO wa_dtypes.

* Get Document type description
    SELECT SINGLE objecttext FROM toasp INTO wa_documenttypes-objecttext
      WHERE ar_object = wa_dtypes-ar_object
        AND language = sy-langu.

    IF sy-subrc EQ 0.
      wa_documenttypes-ar_object = wa_dtypes-ar_object.
      APPEND wa_documenttypes TO documenttypes.
    ENDIF.

  ENDLOOP.

ENDFUNCTION.
