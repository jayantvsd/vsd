FUNCTION /nrk/apaycheckapproval.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYNO
*"     VALUE(SAP_USER) TYPE  ACTORID
*"     VALUE(EXT_USER) TYPE  /NRK/APAYEXUSER
*"  EXPORTING
*"     REFERENCE(APPROVAL_COMPLETE) TYPE  BOOLE-BOOLE
*"----------------------------------------------------------------------

  DATA: wa_approver LIKE /nrk/apayappr,
        t_approver LIKE /nrk/apayappr OCCURS 0,
        lv_tabix LIKE sy-tabix.

  SELECT * FROM /nrk/apayappr INTO TABLE t_approver
    WHERE apayno EQ apayno.

* SAP user only
  IF NOT sap_user IS INITIAL
    AND ext_user IS INITIAL.

    READ TABLE t_approver WITH KEY
      apayno = apayno
      sap_user = sap_user
      approved = ' '
      INTO wa_approver.

*** start change 01/120/2016

* External user only and SAP user not WF-BATCH
  ELSEIF NOT ext_user IS INITIAL
    AND sap_user EQ 'WF-BATCH'.

    TRANSLATE ext_user TO UPPER CASE.

    READ TABLE t_approver WITH KEY
      apayno = apayno
*     sap_user = sap_user
      ext_user = ext_user
      approved = ' '
      INTO wa_approver.

* External user only
  ELSEIF NOT ext_user IS INITIAL
    AND sap_user IS INITIAL.

    TRANSLATE ext_user TO UPPER CASE.

    READ TABLE t_approver WITH KEY
      apayno = apayno
      ext_user = ext_user
      approved = ' '
      INTO wa_approver.

*** end change 01/120/2016

* SAP user and external user
  ELSEIF NOT sap_user IS INITIAL
   AND NOT ext_user IS INITIAL.

    TRANSLATE ext_user TO UPPER CASE.

    READ TABLE t_approver WITH KEY
      apayno = apayno
      sap_user = sap_user
      ext_user = ext_user
      approved = ' '
      INTO wa_approver.

  ENDIF.

  lv_tabix = sy-tabix.

  IF NOT wa_approver IS INITIAL.
    wa_approver-approved = 'A'.
    MODIFY t_approver FROM wa_approver INDEX lv_tabix.

  ENDIF.

* Check approval complete
  CLEAR: wa_approver.
  READ TABLE t_approver WITH KEY approved = ' '
    INTO wa_approver.

  IF NOT wa_approver IS INITIAL. " approval not complete
    approval_complete = ' '.
    MODIFY /nrk/apayappr FROM TABLE t_approver.
  ELSE. "approval complete
    approval_complete = 'X'.
    REFRESH t_approver[].
    DELETE FROM /nrk/apayappr WHERE apayno EQ apayno.
  ENDIF.

ENDFUNCTION.
