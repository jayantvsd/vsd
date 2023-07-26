*&---------------------------------------------------------------------*
*& Report  /NRK/APAYSTATUSUPDATE
*&
*&---------------------------------------------------------------------*
*& APay Center 2.1 - APay Center record update program - 07/31/2014
*& (c) by Norikkon, LLC 2014
*&---------------------------------------------------------------------*
REPORT  /nrk/apaystatusupdate MESSAGE-ID 00.

TYPE-POOLS: slis.

TABLES: /nrk/apayhd,
        /nrk/apaysdef.

DATA: t_hd      LIKE /nrk/apayhd OCCURS 1,
      t_bseg    LIKE bseg OCCURS 1,
      t_items   LIKE /nrk/apayitems OCCURS 1,
      t_his     LIKE /nrk/apayhis OCCURS 1,
      wa_hd     LIKE /nrk/apayhd,
      wa_bkpf   LIKE bkpf,
      wa_items  LIKE /nrk/apayitems,
      wa_his    LIKE /nrk/apayhis,
      l_posted  LIKE boole-boole,
      l_deleted LIKE boole-boole,
      l_failed  LIKE boole-boole,
      l_tlines  TYPE i,
      l_item    LIKE /nrk/apayhis-item,
      l_update  LIKE boole-boole.

TYPES: BEGIN OF t_output,
         apayno  LIKE /nrk/apayhd-apayno,
         bukrs   TYPE bukrs,
         belnr   TYPE belnr_d,
         gjahr   TYPE gjahr,
         msg_doc(50) TYPE c,
         msg_wf(50) TYPE c,
       END OF t_output.

DATA: lt_fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      ls_layout       TYPE slis_layout_alv,
      lv_repid        LIKE sy-repid,
      lt_out          TYPE t_output OCCURS 500,
      ls_out          TYPE t_output,
      l_msg_doc(50)   TYPE c,
      l_msg_wf(50)    TYPE c.

* -----------------------------------------------------------
* SELECTION SCREEN DEFINITION
* -----------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK records WITH FRAME TITLE text-001.

SELECT-OPTIONS sapayno FOR /nrk/apayhd-apayno.        "APay record number
SELECT-OPTIONS scdate FOR /nrk/apayhd-cdate.          "Creation date
SELECT-OPTIONS sbukrs FOR /nrk/apayhd-bukrs.          "Company code
SELECT-OPTIONS sbelnr FOR /nrk/apayhd-belnr.          "SAP document number
SELECT-OPTIONS sgjahr FOR /nrk/apayhd-gjahr.          "Fiscal year
SELECT-OPTIONS slifnr FOR /nrk/apayhd-lifnr.          "Vendor number

SELECTION-SCREEN END OF BLOCK records.

SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE text-002.

PARAMETERS: pstatcur LIKE /nrk/apayhd-status,     "Current status
            pstatnew LIKE /nrk/apayhd-status,     "New status
            pstatpst LIKE /nrk/apayhd-status OBLIGATORY DEFAULT '1900',     "Post status
            pstatdel LIKE /nrk/apayhd-status OBLIGATORY DEFAULT '2220'.     "Deleted status

SELECTION-SCREEN ULINE.

PARAMETER: pdefault RADIOBUTTON GROUP wf DEFAULT 'X',
           pcancel RADIOBUTTON GROUP wf,
           prestart RADIOBUTTON GROUP wf.

SELECTION-SCREEN ULINE.

PARAMETERS: ptest AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK options.

PARAMETERS: phits TYPE i DEFAULT 500.
* -----------------------------------------------------------
* AT SELECTION-SCREEN
* -----------------------------------------------------------
AT SELECTION-SCREEN ON pstatcur.
* Check status
  IF pstatcur IS INITIAL.
    MESSAGE e398 WITH text-005.
  ENDIF.
  SELECT SINGLE * FROM /nrk/apaysdef WHERE status EQ pstatcur.
  IF sy-subrc NE 0.
    MESSAGE e398 WITH text-003 pstatcur text-004.
  ENDIF.

AT SELECTION-SCREEN ON pstatnew.
* Check status
  IF NOT pstatnew IS INITIAL.
    SELECT SINGLE * FROM /nrk/apaysdef WHERE status EQ pstatnew.
    IF sy-subrc NE 0.
      MESSAGE e398 WITH text-003 pstatnew text-004.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON pstatpst.
* Check status
  SELECT SINGLE * FROM /nrk/apaysdef WHERE status EQ pstatpst.
  IF sy-subrc NE 0.
    MESSAGE e398 WITH text-003 pstatpst text-004.
  ENDIF.

AT SELECTION-SCREEN ON pstatdel.
* Check status
  SELECT SINGLE * FROM /nrk/apaysdef WHERE status EQ pstatdel.
  IF sy-subrc NE 0.
    MESSAGE e398 WITH text-003 pstatdel text-004.
  ENDIF.

* -----------------------------------------------------------
* START OF SELECTION
* -----------------------------------------------------------
START-OF-SELECTION.
* Get APay records
  SELECT * INTO TABLE t_hd FROM /nrk/apayhd UP TO phits ROWS
    WHERE apayno IN sapayno
      AND cdate  IN scdate
      AND bukrs  IN sbukrs
      AND belnr  IN sbelnr
      AND lifnr  IN slifnr
      AND status EQ pstatcur.

*  break torsten.

* Check record list
  LOOP AT t_hd INTO wa_hd.

    CLEAR: l_update,
           l_deleted,
           l_posted,
           l_msg_doc,
           l_msg_wf.

* Check for documents for posting/deletion
    PERFORM check_documents.

    IF l_posted EQ 'X'. "Document posted
      wa_hd-status = pstatpst.
      l_update = 'X'.
      MOVE text-011 TO l_msg_doc.
    ELSEIF l_deleted EQ 'X'. "Parked document deleted
      wa_hd-status = pstatdel.
      l_update = 'X'.
      MOVE text-010 TO l_msg_doc.
    ELSEIF l_failed EQ 'X'.
      MOVE text-014 TO l_msg_doc.
      IF NOT pstatnew IS INITIAL.
        wa_hd-status = pstatnew.
        l_update = 'X'.
      ENDIF.
    ELSE. "Update status
      IF NOT pstatnew IS INITIAL.
        wa_hd-status = pstatnew.
        l_update = 'X'.
        MOVE text-009 TO l_msg_doc.
      ENDIF.
    ENDIF.

    IF l_update EQ 'X'. "Update APay Center

      IF NOT ptest EQ 'X'.
* update header
        MODIFY /nrk/apayhd FROM wa_hd.

* Update line items
        DELETE FROM /nrk/apayitems
          WHERE apayno EQ wa_hd-apayno.

        LOOP AT t_items INTO wa_items.
          wa_items-apayno = wa_hd-apayno.
          INSERT /nrk/apayitems FROM wa_items.
        ENDLOOP.

* Update history
        SELECT * FROM /nrk/apayhis INTO TABLE t_his
           WHERE apayno = wa_hd-apayno.

        DESCRIBE TABLE t_his LINES l_tlines.
        l_item = l_tlines + 1.

        wa_his-apayno = wa_hd-apayno.
        wa_his-item   = l_item.
        wa_his-status = wa_hd-status.
        wa_his-sdate  = sy-datum.
        wa_his-stime  = sy-uzeit.
        wa_his-suser  = wa_hd-uname.

        SELECT SINGLE fullname FROM /nrk/apayuser INTO wa_his-sname
          WHERE objid EQ wa_his-suser.

        IF sy-subrc NE 0.
          SELECT SINGLE fullname FROM /nrk/apayuser INTO wa_his-sname
            WHERE extuser EQ wa_his-sextuser.
        ENDIF.

        INSERT /nrk/apayhis FROM wa_his.
      ENDIF.

      IF pcancel EQ 'X'. "Cancel workflow

        CALL FUNCTION 'SAP_WAPI_ADM_WORKFLOW_CANCEL'
          EXPORTING
            workitem_id  = wa_hd-workflowid
            actual_agent = sy-uname
            language     = sy-langu
            do_commit    = 'X'.

        IF sy-subrc EQ 0. "Cancelled
          MOVE text-008 TO l_msg_wf.
          DELETE FROM /nrk/apayappr WHERE apayno EQ wa_hd-apayno.
        ENDIF.

      ENDIF.

      IF prestart EQ 'X'. "Restart workflow

        CALL FUNCTION '/NRK/APAYRESTARTWORKFLOW'
          EXPORTING
            apayno             = wa_hd-apayno
          EXCEPTIONS
            no_workflows_found = 1
            no_cancellation    = 2
            OTHERS             = 3.

        IF sy-subrc EQ 0. "Restarted
          MOVE text-006 TO l_msg_wf.
        ELSE. "Restart failed
          MOVE text-007 TO l_msg_wf.
        ENDIF.
      ENDIF.

    ENDIF.

    IF l_msg_doc IS INITIAL.
      MOVE text-012 TO l_msg_doc.
    ENDIF.

    IF l_msg_wf IS INITIAL.
      MOVE text-013 TO l_msg_wf.
    ENDIF.

    ls_out-apayno = wa_hd-apayno.
    ls_out-bukrs  = wa_hd-bukrs.
    ls_out-belnr  = wa_hd-belnr.
    ls_out-gjahr  = wa_hd-gjahr.
    ls_out-msg_doc = l_msg_doc.
    ls_out-msg_wf = l_msg_wf.
    APPEND ls_out TO lt_out.

  ENDLOOP.

* Display output
  PERFORM build_fieldcatalog.
  PERFORM build_layout.
  PERFORM display_output.

*&---------------------------------------------------------------------*
*&      Form  CHECK_DOCUMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_documents .

  CALL FUNCTION '/NRK/UPDATEBKPF'
    EXPORTING
      bukrs                   = wa_hd-bukrs
      belnr                   = wa_hd-belnr
      gjahr                   = wa_hd-gjahr
    IMPORTING
      posted                  = l_posted
      deleted                 = l_deleted
    TABLES
      lineitems               = t_items
    CHANGING
      header                  = wa_hd
    EXCEPTIONS
      no_accounting_doc_found = 1
      no_line_item_found      = 2
      no_vendor_found         = 3
      OTHERS                  = 4.

  IF sy-subrc <> 0.
    l_failed = 'X'.
  ELSE.
    l_failed = ' '.
  ENDIF.

ENDFORM.                    " CHECK_DOCUMENTS
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .

  CLEAR: lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'APAYNO'.
  lt_fieldcatalog-seltext_m   = 'Apay record'.
  lt_fieldcatalog-col_pos     = 0.
  lt_fieldcatalog-outputlen   = 30.
  lt_fieldcatalog-emphasize   = 'X'.
  lt_fieldcatalog-key         = 'X'.
  APPEND lt_fieldcatalog TO lt_fieldcatalog.
  CLEAR lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'BUKRS'.
  lt_fieldcatalog-seltext_m   = 'Company code'.
  lt_fieldcatalog-col_pos     = 1.
  APPEND lt_fieldcatalog TO lt_fieldcatalog.
  CLEAR lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'BELNR'.
  lt_fieldcatalog-seltext_m   = 'Document number'.
  lt_fieldcatalog-col_pos     = 2.
  APPEND lt_fieldcatalog TO lt_fieldcatalog.
  CLEAR lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'GJAHR'.
  lt_fieldcatalog-seltext_m   = 'Fiscal year'.
  lt_fieldcatalog-col_pos     = 3.
  APPEND lt_fieldcatalog TO lt_fieldcatalog.
  CLEAR lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'MSG_DOC'.
  lt_fieldcatalog-seltext_m   = 'Message document'.
  lt_fieldcatalog-col_pos     = 4.
  APPEND lt_fieldcatalog TO lt_fieldcatalog.
  CLEAR lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'MSG_WF'.
  lt_fieldcatalog-seltext_m   = 'Message workflow'.
  lt_fieldcatalog-col_pos     = 4.
  APPEND lt_fieldcatalog TO lt_fieldcatalog.
  CLEAR lt_fieldcatalog.

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout .

  ls_layout-no_input          = 'X'.
  ls_layout-colwidth_optimize = 'X'.
  ls_layout-totals_text       = 'Totals'.

ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_output .

  lv_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = lv_repid
      is_layout          = ls_layout
      it_fieldcat        = lt_fieldcatalog[]
      i_save             = 'X'
    TABLES
      t_outtab           = lt_out
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH text-305 space space space.
  ENDIF.

ENDFORM.                    " DISPLAY_OUTPUT
