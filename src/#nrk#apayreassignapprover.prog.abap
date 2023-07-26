*&---------------------------------------------------------------------*
*& Report  /NRK/APAYREASSIGNAPPROVER
*&
*&---------------------------------------------------------------------*
*& APay Center 2.1 - APay Center reassign approver program - 07/31/2014
*& (c) by Norikkon, LLC 2014
*&---------------------------------------------------------------------*
REPORT  /nrk/apayreassignapprover MESSAGE-ID 00.

TYPE-POOLS: slis.

TABLES: /nrk/apayhd,
        /nrk/apaysdef.

DATA: t_hd      LIKE /nrk/apayhd OCCURS 1,
      t_bseg    LIKE bseg OCCURS 1,
      t_items   LIKE /nrk/apayitems OCCURS 1,
      t_his     LIKE /nrk/apayhis OCCURS 1,
      t_appr    LIKE /nrk/apayappr OCCURS 1,
      wa_hd     LIKE /nrk/apayhd,
      wa_items  LIKE /nrk/apayitems,
      wa_his    LIKE /nrk/apayhis,
      wa_appr   LIKE /nrk/apayappr,
      wa_user   LIKE /nrk/apayuser,
      l_tlines  TYPE i,
      l_item    LIKE /nrk/apayhis-item,
      l_update  LIKE boole-boole,
      l_approval LIKE boole-boole.

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

PARAMETERS: pusrsapc LIKE /nrk/apayappr-sap_user, "Current SAP user
            pusrspc  LIKE /nrk/apayappr-ext_user, "Current AD user
            pusrsapn LIKE /nrk/apayappr-sap_user, "New SAP user
            pusrspn  LIKE /nrk/apayappr-ext_user. "New AD user

SELECTION-SCREEN ULINE.

PARAMETER: pmanager AS CHECKBOX,
           prestart AS CHECKBOX,
           pstatus LIKE /nrk/apayhd-status OBLIGATORY DEFAULT '8010'.

SELECTION-SCREEN ULINE.

PARAMETERS: ptest AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK options.

PARAMETERS: phits TYPE i DEFAULT 500.
* -----------------------------------------------------------
* AT SELECTION-SCREEN
* -----------------------------------------------------------
AT SELECTION-SCREEN ON pusrsapc.
* Check current SAP user
*  SELECT SINGLE * FROM /nrk/apayappr INTO wa_appr
*    WHERE sap_user EQ pusrsapc.
*  IF sy-subrc NE 0.
*    MESSAGE e398 WITH text-015 pusrsapc text-016.
*  ENDIF.

AT SELECTION-SCREEN ON pusrspc.
* Check current Sharepoint user
*  SELECT SINGLE * FROM /nrk/apayappr INTO wa_appr
*    WHERE ext_user EQ pusrspc.
*  IF sy-subrc NE 0.
*    MESSAGE e398 WITH text-015 pusrspc text-016.
*  ENDIF.

AT SELECTION-SCREEN ON pusrsapn.
* Check new SAP user
  IF NOT pusrsapn IS INITIAL.
    SELECT SINGLE * FROM /nrk/apayuser INTO wa_user
      WHERE objid EQ pusrsapn.
    IF sy-subrc NE 0.
      CLEAR: wa_user.
      MESSAGE e398 WITH text-015 pusrsapn text-017.
    ENDIF.
  ELSE.
    CLEAR: wa_user.
  ENDIF.

AT SELECTION-SCREEN ON pusrspn.
* Check new AD user
  SELECT SINGLE * FROM /nrk/apayuser INTO wa_user
    WHERE extuser EQ pusrspn.
  IF sy-subrc NE 0.
    CLEAR: wa_user.
    MESSAGE e398 WITH text-015 pusrspn text-016.

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
      AND lifnr  IN slifnr.

* Get details of new approver
  IF NOT pusrsapn IS INITIAL.
* Check current SAP user
    SELECT SINGLE * FROM /nrk/apayuser INTO wa_user
      WHERE objid EQ pusrsapn.
    IF sy-subrc NE 0.
      MESSAGE e398 WITH text-025 ':' pusrsapc.
    ENDIF.
  ELSE.
* Check current Sharepoint user
    SELECT SINGLE * FROM /nrk/apayuser INTO wa_user
      WHERE extuser EQ pusrspn.
    IF sy-subrc NE 0.
      MESSAGE e398 WITH text-025 ':' pusrsapc.
    ENDIF.
  ENDIF.

* Check approval work items
  LOOP AT t_hd INTO wa_hd.

    CLEAR: l_update,
           l_msg_doc,
           l_msg_wf,
           t_appr[],
           t_appr,
           wa_appr.

* Get open work items from approver table
    IF NOT pusrspc IS INITIAL.
      SELECT * INTO TABLE t_appr FROM /nrk/apayappr
        WHERE apayno EQ wa_hd-apayno
          AND ext_user EQ pusrspc.
    ELSEIF NOT pusrsapc IS INITIAL.
      SELECT * INTO TABLE t_appr FROM /nrk/apayappr
        WHERE apayno EQ wa_hd-apayno
          AND sap_user EQ pusrsapc.
    ELSE.
* current user is blank (no workflow)
      IF wa_hd-ext_approver IS INITIAL.
        wa_appr-sap_user = wa_user-objid.
        wa_appr-ext_user = wa_user-extuser.
        wa_appr-smtp_addr = wa_user-smtp_addr.
        wa_appr-approved = ' '.
        APPEND wa_appr TO t_appr.
      ENDIF.
    ENDIF.

    CLEAR: l_update,
           wa_appr.

    IF NOT t_appr[] IS INITIAL.

* Change to new approver
      LOOP AT t_appr INTO wa_appr.

        IF wa_appr-approved = 'A'.
          MOVE text-021 TO l_msg_doc.
        ELSEIF wa_appr-approved = 'R'.
          MOVE text-020 TO l_msg_doc.
        ELSE.
          IF NOT wa_user IS INITIAL.
            wa_appr-sap_user = wa_user-objid.
            wa_appr-ext_user = wa_user-extuser.
            wa_appr-smtp_addr = wa_user-smtp_addr.
            wa_appr-approved = ' '.
            MODIFY t_appr INDEX sy-tabix FROM wa_appr.
            l_update = 'X'.
          ELSE.
            MOVE text-022 TO l_msg_doc.
          ENDIF.
        ENDIF.
      ENDLOOP.

* update document status after reassignement
      MOVE text-018 TO l_msg_doc.
    ELSE.
      MOVE text-019 TO l_msg_doc.
    ENDIF.

* upate approver table
    IF NOT ptest EQ 'X' AND l_update EQ 'X'.
* update approver table
      MODIFY /nrk/apayappr FROM TABLE t_appr.

* update header
      wa_hd-uname = wa_user-objid.
      wa_hd-ext_approver = wa_user-extuser.
      MODIFY /nrk/apayhd FROM wa_hd.

* Update history
      wa_hd-status = pstatus.
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

* Determine new approver
    IF pmanager EQ 'X'
      AND l_update EQ 'X'.

      CALL FUNCTION '/NRK/APAYDETERMINEAPPROVER'
        EXPORTING
          apayno                          = wa_hd-apayno
          sap_user                        = wa_user-objid
*         EXT_USER                        =
        IMPORTING
          approval                        = l_approval
        EXCEPTIONS
          hierarchy_creation_failed       = 1
          record_not_found                = 2
          user_not_found                  = 3
          OTHERS                          = 4.

      IF sy-subrc <> 0.
        MOVE text-023 TO l_msg_doc.
      ENDIF.

      IF l_approval EQ 'X'.
        MOVE text-024 TO l_msg_doc.
      ELSE.
        MOVE text-023 TO l_msg_doc.
      ENDIF.

    ENDIF.

* Restart workflow
    IF prestart EQ 'X'
      AND NOT ptest EQ 'X'
      AND l_update EQ 'X'.

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

* Update messages
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
