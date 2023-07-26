FUNCTION /nrk/apayrestartworkflow.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"  EXCEPTIONS
*"      NO_WORKFLOWS_FOUND
*"      NO_CANCELLATION
*"----------------------------------------------------------------------

  DATA: t_wf       LIKE /nrk/apaywf OCCURS 0,
        wa_wf      LIKE /nrk/apaywf,
        lines      TYPE i,
        return     TYPE sysubrc,
        t_wihead   LIKE swwwihead OCCURS 5 WITH HEADER LINE,
        answer(1)  TYPE c,
        doc_class  TYPE toadd-doc_type,
        archiv_id  TYPE saearchivi,
        arc_doc_id TYPE saeardoid,
        ar_object  TYPE saeobjart,
        object_id  LIKE toav0-object_id,
        t_connections LIKE toav0 OCCURS 0 WITH HEADER LINE,
        url        TYPE char255.

  CLEAR: lines.

  SELECT * FROM /nrk/apaywf INTO TABLE t_wf
    WHERE apayno EQ apayno ORDER BY PRIMARY KEY.

  IF sy-subrc NE 0.
* No active workflow -> start only
    CALL FUNCTION '/NRK/APAY_START_WORKFLOW'
    EXPORTING
      apayno                      = apayno
*     doc_class                   = doc_class
*     archiv_id                   = t_connections-archiv_id
*     arc_doc_id                  = t_connections-arc_doc_id
      user                        = sy-uname
*   IMPORTING
*      WF_ID                       =
    EXCEPTIONS
      workflow_start_failed       = 1
      OTHERS                      = 2.

    IF sy-subrc EQ 0.
      EXIT.
    ELSE.
      RAISE no_workflows_found.
    ENDIF.


  ENDIF.

  LOOP AT t_wf INTO wa_wf.

    SELECT SINGLE * FROM swwwihead
      WHERE wi_id EQ wa_wf-wi_id.

    IF swwwihead-wi_stat EQ 'COMPLETED'
       OR swwwihead-wi_stat EQ 'CANCELLED'.

      CONTINUE.

    ELSE.

      APPEND swwwihead TO t_wihead.

    ENDIF.

  ENDLOOP.

  DESCRIBE TABLE t_wihead LINES lines.

  IF lines GT 0.

*** add pop-up to confirm cancellation
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar                    = text-906
        text_question               = text-905
        text_button_1               = text-907
        text_button_2               = text-908
        default_button              = '2'
      IMPORTING
        answer                      = answer
* TABLES
*   PARAMETER                   =
      EXCEPTIONS
        text_not_found              = 1
        OTHERS                      = 2.

    IF answer = '1'.
* Cancel workflows
    ELSE. "
* Don't cancel workflows
      EXIT.
*     RAISE no_cancellation.
    ENDIF.

  ENDIF.

  LOOP AT t_wihead.

    CALL FUNCTION 'SAP_WAPI_ADM_WORKFLOW_CANCEL'
      EXPORTING
        workitem_id  = t_wihead-wi_id
        actual_agent = sy-uname
        language     = sy-langu
        do_commit    = 'X'
      IMPORTING
        return_code  = return.

  ENDLOOP.

  SELECT SINGLE val1 FROM /nrk/apayconfig INTO url
    WHERE key1 = 'APAYSP'
      AND key2 = 'CANCELURL'.

  IF sy-subrc EQ 0.

    CALL FUNCTION '/NRK/APAYAPI_BAPI_CANCEL_EXTWF'
      EXPORTING
        apayno                 = apayno
      EXCEPTIONS
        no_sharepoint_workflow = 1
        cancellation_failed    = 2
        OTHERS                 = 3.

*  IF sy-subrc = 2.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

  ENDIF.

* start new workflow
* READ TABLE t_connections INDEX 1.
  CALL FUNCTION '/NRK/APAY_START_WORKFLOW'
    EXPORTING
      apayno                      = apayno
*     doc_class                   = doc_class
*     archiv_id                   = t_connections-archiv_id
*     arc_doc_id                  = t_connections-arc_doc_id
      user                        = sy-uname
*   IMPORTING
*      WF_ID                       =
    EXCEPTIONS
      workflow_start_failed       = 1
      OTHERS                      = 2.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFUNCTION.
