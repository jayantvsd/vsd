FUNCTION /nrk/apaydetermineapprover2.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYNO
*"     REFERENCE(SAP_USER) TYPE  ACTORID OPTIONAL
*"     REFERENCE(EXT_USER) TYPE  /NRK/APAYEXUSER OPTIONAL
*"  EXPORTING
*"     REFERENCE(APPROVAL) TYPE  BOOLE-BOOLE
*"  EXCEPTIONS
*"      HIERARCHY_CREATION_FAILED
*"      RECORD_NOT_FOUND
*"----------------------------------------------------------------------

  DATA: kostl        LIKE /nrk/apayhd-kostl,
        wrbtr        LIKE /nrk/apayhd-wrbtr,
        bukrs        LIKE /nrk/apayhd-bukrs,
        ext_approver TYPE /nrk/apayexuser,
        item         LIKE /nrk/apayappr-item,
        lines        TYPE i,
        wa_approver  LIKE /nrk/apayappr,
        wa_user      LIKE /nrk/apayuser,
        t_approver   LIKE /nrk/apayappr OCCURS 0,
        t_user       LIKE /nrk/apayuser OCCURS 0,
        t_manager    LIKE /nrk/apayuser OCCURS 0,
        wa_manager   LIKE /nrk/apayuser,
        wa_manager2  LIKE /nrk/apayuser.

  CLEAR: wa_approver,
         wa_user,
         wa_manager.

  REFRESH: t_approver[],
           t_user[],
           t_manager[].

  SELECT * FROM /nrk/apayappr INTO TABLE t_approver
    WHERE apayno EQ apayno.

  IF sy-subrc EQ 0.
    REFRESH t_approver[].
    DELETE FROM /nrk/apayappr WHERE apayno EQ apayno.
  ENDIF.

* Add initial user
*  IF NOT sap_user IS INITIAL
*    OR NOT ext_user IS INITIAL.

* Add additional user
  SELECT SINGLE bukrs wrbtr kostl ext_approver FROM /nrk/apayhd
    INTO (bukrs, wrbtr, kostl, ext_approver)
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE record_not_found.
  ENDIF.

  IF NOT ext_approver IS INITIAL.

    SELECT SINGLE * FROM /nrk/apayuser INTO wa_user
      WHERE approver EQ 'X'
        AND extuser EQ ext_approver.

    IF sy-subrc EQ 0. " approver found
      item = 1.
      wa_approver-apayno = apayno.
      wa_approver-item   = item.
      wa_approver-sap_user = wa_user-objid.
      wa_approver-ext_user = wa_user-extuser.
      wa_approver-smtp_addr = wa_user-smtp_addr.

      TRANSLATE wa_approver-ext_user TO UPPER CASE.
      APPEND wa_approver TO t_approver.
    ENDIF.

* Get Manager
    IF NOT wa_user-motype IS INITIAL. " Manager exists
      IF wa_user-motype EQ 'US'. " SAP user

        SELECT SINGLE * FROM /nrk/apayuser INTO wa_manager
          WHERE otype EQ 'US'
            AND objid EQ wa_user-mobjid
            AND wrbtr LE wrbtr.

      ELSEIF wa_user-motype EQ 'EX'. " External user

        TRANSLATE wa_user-mextuser TO UPPER CASE.
        SELECT SINGLE * FROM /nrk/apayuser INTO wa_manager
          WHERE otype EQ 'EX'
            AND extuser EQ wa_user-mextuser
            AND wrbtr LE wrbtr.

      ENDIF.


      IF NOT wa_manager IS INITIAL.
        APPEND wa_manager TO t_manager.
        CLEAR: wa_manager.
      ENDIF.

      LOOP AT t_manager INTO wa_manager.

        item = item + 1.
        wa_approver-apayno = apayno.
        wa_approver-item   = item.
        wa_approver-sap_user = wa_manager-objid.
        wa_approver-ext_user = wa_manager-extuser.
        wa_approver-smtp_addr = wa_manager-smtp_addr.

        TRANSLATE wa_approver-ext_user TO UPPER CASE.
        APPEND wa_approver TO t_approver.

        IF NOT wa_manager-motype IS INITIAL. " Manager exists
          IF wa_manager-motype EQ 'US'. " SAP user

            SELECT SINGLE * FROM /nrk/apayuser INTO wa_manager2
              WHERE otype EQ 'US'
                AND objid EQ wa_manager-mobjid.
*               and wrbtr GT wrbtr.

          ELSEIF wa_manager-motype EQ 'EX'. " External user

            TRANSLATE wa_manager-mextuser TO UPPER CASE.
            SELECT SINGLE * FROM /nrk/apayuser INTO wa_manager2
              WHERE otype EQ 'EX'
                AND extuser EQ wa_manager-mextuser.
*               and wrbtr GT wrbtr.

          ENDIF.

          IF NOT wa_manager2 IS INITIAL.
            APPEND wa_manager2 TO t_manager.
          ENDIF.

        ENDIF.

        DELETE t_manager WHERE wrbtr GT wrbtr.

      ENDLOOP.

    ENDIF.

  ENDIF.

*  IF NOT kostl IS INITIAL
*    AND NOT wrbtr IS INITIAL
*    AND NOT bukrs IS INITIAL.
*
*    SELECT * FROM /nrk/apayuser INTO TABLE t_user
*      WHERE approver EQ 'X'
*        AND kostl    EQ kostl
*        AND wrbtr    LE wrbtr
*        AND bukrs    EQ bukrs.
*
*    IF sy-subrc NE 0.
*      SELECT * FROM /nrk/apayuser INTO TABLE t_user
*        WHERE approver EQ 'X'
*          AND wrbtr    LE wrbtr.
*    ENDIF.
*
*  ELSEIF NOT bukrs IS INITIAL
*     AND NOT wrbtr IS INITIAL.
*
*    SELECT * FROM /nrk/apayuser INTO TABLE t_user
*      WHERE approver EQ 'X'
*        AND wrbtr    LE wrbtr
*        AND bukrs    EQ bukrs.
*
*    IF sy-subrc NE 0.
*      SELECT * FROM /nrk/apayuser INTO TABLE t_user
*        WHERE approver EQ 'X'
*          AND wrbtr    LE wrbtr.
*    ENDIF.
*
*  ELSE.
*
*    SELECT * FROM /nrk/apayuser INTO TABLE t_user
*      WHERE approver EQ 'X'
*        AND wrbtr    LE wrbtr.
*  ENDIF.
*
*  DELETE t_user WHERE wrbtr EQ 0.
*  SORT t_user ASCENDING BY wrbtr.
*
*  READ TABLE t_user INTO wa_user INDEX 1.
*
*  item = item + 1.
*  wa_approver-apayno = apayno.
*  wa_approver-item   = item.
*  wa_approver-sap_user = wa_user-objid.
*  wa_approver-ext_user = wa_user-extuser.
*  wa_approver-smtp_addr  = wa_user-smtp_addr.
*  APPEND wa_approver TO t_approver.
*
*  LOOP AT t_user INTO wa_user.
*
*    IF NOT wa_user-motype IS INITIAL. " Manager exists
*      IF wa_user-motype EQ 'US'. " SAP user
*
*        SELECT SINGLE * FROM /nrk/apayuser INTO wa_manager
*          WHERE otype EQ 'US'
*            AND objid EQ wa_user-mobjid.
*
*      ELSEIF wa_user-motype EQ 'EX'. " External user
*
*        SELECT SINGLE * FROM /nrk/apayuser INTO wa_manager
*          WHERE otype EQ 'EX'
*            AND extuser EQ wa_user-mextuser.
*
*      ENDIF.
*
*      IF NOT wa_manager IS INITIAL
*        AND wa_manager-wrbtr LE wrbtr.
*        APPEND wa_manager TO t_manager.
*      ENDIF.
*
*    ENDIF.
*
*  ENDLOOP.
*
*  LOOP AT t_manager INTO wa_manager.
*    item = item + 1.
*    wa_approver-apayno = apayno.
*    wa_approver-item   = item.
*    wa_approver-sap_user = wa_manager-objid.
*    wa_approver-ext_user = wa_manager-extuser.
*    APPEND wa_approver TO t_approver.
*
*  ENDLOOP.

* DELETE ADJACENT DUPLICATES FROM t_approver.

  DELETE ADJACENT DUPLICATES FROM t_approver
    COMPARING apayno sap_user ext_user.

* Check if approval required
  DESCRIBE TABLE t_approver LINES lines.

  IF lines GT 0. "approval required
    INSERT /nrk/apayappr FROM TABLE t_approver.
    UPDATE /nrk/apayhd SET approvalreq = 'X'.
    approval = 'X'.
  ELSE. " no approval required
    UPDATE /nrk/apayhd SET approvalreq = ' '.
    approval = ' '.
  ENDIF.

ENDFUNCTION.
