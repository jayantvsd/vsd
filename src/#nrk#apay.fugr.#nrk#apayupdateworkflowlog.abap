FUNCTION /nrk/apayupdateworkflowlog.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"     REFERENCE(WORKFLOW_ID) TYPE  SWW_WIID
*"  EXCEPTIONS
*"      WORKFLOW_LOG_UPDATE_FAILED
*"----------------------------------------------------------------------

  DATA: t_wf  LIKE /nrk/apaywf OCCURS 0,
        wa_wf LIKE /nrk/apaywf.

  wa_wf-mandt  = sy-mandt.
  wa_wf-apayno = apayno.
  wa_wf-wi_id  = workflow_id.
  wa_wf-sdate  = sy-datum.
  wa_wf-stime  = sy-uzeit.
  wa_wf-suser  = sy-uname.

  INSERT /nrk/apaywf FROM wa_wf.

  IF sy-subrc NE 0.
    RAISE workflow_log_update_failed.
  ENDIF.

ENDFUNCTION.
