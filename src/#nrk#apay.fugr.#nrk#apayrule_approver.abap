FUNCTION /nrk/apayrule_approver.
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
        lines TYPE i,
        wa_approver LIKE /nrk/apayappr,
        t_approver LIKE /nrk/apayappr OCCURS 0.

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


  SELECT * FROM /nrk/apayappr INTO TABLE t_approver
      WHERE approved EQ space
*     AND sap_user EQ sy-uname
      AND apayno EQ apayno.

  READ TABLE t_approver INTO wa_approver INDEX 1.

*  LOOP AT t_approver INTO wa_approver.
  IF NOT wa_approver-sap_user IS INITIAL.
    wa_actor-otype = 'US'.
    wa_actor-objid = wa_approver-sap_user.
    APPEND wa_actor TO actor_tab.
  ENDIF.
* ENDLOOP.

  DESCRIBE TABLE actor_tab LINES lines.
  IF lines IS INITIAL.
    RAISE no_ap_processor_found.
  ENDIF.

ENDFUNCTION.
