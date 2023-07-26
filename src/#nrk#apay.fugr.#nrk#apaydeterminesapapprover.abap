FUNCTION /nrk/apaydeterminesapapprover.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYNO
*"  EXPORTING
*"     REFERENCE(APPROVAL) TYPE  BOOLE-BOOLE
*"  EXCEPTIONS
*"      HIERARCHY_CREATION_FAILED
*"      RECORD_NOT_FOUND
*"      USER_NOT_FOUND
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
        ls_approver  TYPE /nrk/apayappr,
        t_user       LIKE /nrk/apayuser OCCURS 0,
        t_manager    LIKE /nrk/apayuser OCCURS 0,
        wa_manager   LIKE /nrk/apayuser,
        wa_manager2  LIKE /nrk/apayuser,
        sap_approver LIKE sy-uname.
*       wa_approver TYPE /nrk/apayappr.

  CLEAR: wa_approver, wa_user, wa_manager.

  REFRESH: t_approver[], t_user[], t_manager[].

* Check if any records in approval table
*  SELECT * FROM /nrk/apayappr INTO TABLE t_approver WHERE apayno EQ apayno.

*  IF sy-subrc EQ 0.
  REFRESH t_approver[].
  DELETE FROM /nrk/apayappr WHERE apayno EQ apayno.
*  ENDIF.

* Get initial user
  SELECT SINGLE bukrs wrbtr kostl ext_approver FROM /nrk/apayhd INTO (bukrs, wrbtr, kostl, ext_approver)
                                               WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE record_not_found.
  ENDIF.

  MOVE ext_approver TO sap_approver.
  TRANSLATE ext_approver TO UPPER CASE.

  IF NOT sap_approver IS INITIAL.
* Check if valid approver
    SELECT SINGLE * FROM /nrk/apayuser INTO wa_user WHERE approver EQ 'X' AND objid EQ sap_approver.

    IF sy-subrc EQ 0.
* Approver found
      item = 1.

* Set data
      wa_approver-apayno    = apayno.
      wa_approver-item      = item.
      wa_approver-sap_user  = wa_user-objid.
*     wa_approver-ext_user  = wa_user-extuser.
      wa_approver-smtp_addr = wa_user-smtp_addr.

* Append to approver table
      APPEND wa_approver TO t_approver.

* Search for additional approvers
      DO.
* Do we need additional approver?
        IF wrbtr <=  wa_user-wrbtr.
          EXIT.
        ENDIF.

        IF wa_user-motype EQ 'US'.
* Get manager info
          SELECT SINGLE * FROM /nrk/apayuser INTO wa_user
            WHERE otype EQ 'US' AND objid EQ wa_user-mobjid.
          IF sy-subrc EQ 0.
* Write to approver table
            item = item + 1.

* Set data
            wa_approver-apayno    = apayno.
            wa_approver-item      = item.
            wa_approver-sap_user  = wa_user-objid.
*           wa_approver-ext_user  = wa_user-extuser.
            wa_approver-smtp_addr = wa_user-smtp_addr.

* Append to approver table
            APPEND wa_approver TO t_approver.

          ELSE.
* Manager not found, append default user
            item = item + 1.
            PERFORM get_default_user TABLES t_approver USING wa_user item apayno.
          ENDIF.
        ELSEIF wa_user-motype EQ 'US'.
* Get manager info - Internal
          SELECT SINGLE * FROM /nrk/apayuser INTO wa_user WHERE otype EQ 'US' AND objid EQ wa_user-mobjid.
          IF sy-subrc EQ 0.
* Write to approver table
            item = item + 1.
            PERFORM append_approver TABLES t_approver USING wa_user item apayno.
          ELSE.
* Manager not found, append default user
            item = item + 1.
            PERFORM get_default_user TABLES t_approver USING wa_user item apayno.
          ENDIF.
        ENDIF.

      ENDDO.

    ELSE.
* Get administrator as default
      item = 1.
      PERFORM get_default_user TABLES t_approver USING wa_user item apayno.

    ENDIF.
  ENDIF.

* Make sure there are no duplicates
  DELETE ADJACENT DUPLICATES FROM t_approver COMPARING apayno sap_user ext_user.

* Check if approval required
  READ TABLE t_approver INTO ls_approver WITH KEY item = 1.

*** Start Change 10/06/2015
*  if sy-subrc eq 0. "approval required
*    insert /nrk/apayappr from table t_approver.
*    update /nrk/apayhd set approvalreq = 'X'.
*    approval = 'X'.
*  else. " no approval required
*    update /nrk/apayhd set approvalreq = ' '.
*    approval = ' '.
*  endif.

  IF sy-subrc EQ 0. "approval required
    INSERT /nrk/apayappr FROM TABLE t_approver.
    UPDATE /nrk/apayhd SET approvalreq = 'X' WHERE apayno EQ apayno.
    approval = 'X'.
  ELSE. " no approval required
    UPDATE /nrk/apayhd SET approvalreq = ' ' WHERE apayno EQ apayno.
    approval = ' '.
  ENDIF.
*** End Change 10/06/2015

ENDFUNCTION.
