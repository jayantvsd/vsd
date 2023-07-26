*&---------------------------------------------------------------------*
*& APay Center 2.0 - APay Center record update program
*&             2.1 - 06/30/2014
*& (c) by Norikkon, LLC 2014
*&---------------------------------------------------------------------*
REPORT  /nrk/apaypayupdate MESSAGE-ID 00.

TABLES: /nrk/apayhd, /nrk/apayhis, /nrk/apaysdef, bkpf, bseg, rbkp.

TYPES: BEGIN OF t_output,
         apayno  LIKE /nrk/apayhd-apayno,
         bukrs   TYPE bukrs,
         belnr   TYPE belnr_d,
         gjahr   TYPE gjahr,
         msg(50) TYPE c,
       END OF t_output.

DATA: lt_hd    LIKE /nrk/apayhd OCCURS 1,
      ls_hd    LIKE /nrk/apayhd,
      lt_his   LIKE /nrk/apayhis OCCURS 1,
      ls_his   LIKE /nrk/apayhis,
      lt_out   TYPE t_output OCCURS 500,
      ls_out   TYPE t_output,
      lv_subrc LIKE sy-subrc.

DATA: lv_lines    TYPE i,
      lt_thist    LIKE /nrk/apayhis OCCURS 1,
      ls_addr3    TYPE bapiaddr3,
      lt_ret2     TYPE bapiret2 OCCURS 0,
      l_objectkey LIKE  swr_struct-object_key,
*     wa_regup    LIKE regup,
      wa_bsak     LIKE bsak,
      wa_payr     LIKE payr,
      wa_bkpf     LIKE bkpf.

TYPE-POOLS: slis.

DATA: lt_fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      ls_layout       TYPE slis_layout_alv,
      lv_repid        LIKE sy-repid.

CONSTANTS: c_bsc21    LIKE bseg-bschl     VALUE '21',
           c_bsc31    LIKE bseg-bschl     VALUE '31'.

* -----------------------------------------------------------
* SELECTION SCREEN DEFINITION
* -----------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK procs WITH FRAME TITLE text-001.

SELECT-OPTIONS sapayno FOR /nrk/apayhd-apayno.        "APay record number
SELECT-OPTIONS sbudat  FOR /nrk/apayhd-budat.         "Posting date
SELECT-OPTIONS sbukrs  FOR /nrk/apayhd-bukrs.         "Company code
SELECT-OPTIONS slifnr  FOR /nrk/apayhd-lifnr.         "Vendor number

SELECTION-SCREEN END OF BLOCK procs.

SELECTION-SCREEN BEGIN OF BLOCK statusto WITH FRAME TITLE text-002.

PARAMETERS: pspost LIKE /nrk/apaysdef-status OBLIGATORY DEFAULT '1900'.       "Status posted
SELECTION-SCREEN ULINE.
PARAMETERS: psrvrs LIKE /nrk/apaysdef-status OBLIGATORY DEFAULT '8000',       "Status reversed
            pspaid LIKE /nrk/apaysdef-status OBLIGATORY DEFAULT '9000'.       "Status paid

SELECTION-SCREEN END OF BLOCK statusto.

SELECTION-SCREEN BEGIN OF BLOCK workflow WITH FRAME TITLE text-003.

PARAMETER: workflow AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK workflow.

PARAMETERS: phits TYPE i DEFAULT 500.

* -----------------------------------------------------------
* AT SELECTION-SCREEN
* -----------------------------------------------------------
AT SELECTION-SCREEN ON pspost.
* Check input status
  SELECT SINGLE * FROM /nrk/apaysdef WHERE status EQ pspost.
  IF sy-subrc NE 0.
    MESSAGE e398 WITH text-200 pspost text-300 space.
  ENDIF.

AT SELECTION-SCREEN ON psrvrs.
* Check input status
  SELECT SINGLE * FROM /nrk/apaysdef WHERE status EQ psrvrs.
  IF sy-subrc NE 0.
    MESSAGE e398 WITH text-200 psrvrs text-300 space.
  ENDIF.

AT SELECTION-SCREEN ON pspaid.
* Check input status
  SELECT SINGLE * FROM /nrk/apaysdef WHERE status EQ pspaid.
  IF sy-subrc NE 0.
    MESSAGE e398 WITH text-200 pspaid text-300 space.
  ENDIF.

* -----------------------------------------------------------
* START OF SELECTION
* -----------------------------------------------------------
START-OF-SELECTION.
* Check all posted invoices in APay
  SELECT * INTO TABLE lt_hd FROM /nrk/apayhd UP TO phits ROWS
                                               WHERE apayno IN sapayno AND budat IN sbudat
                                               AND bukrs IN sbukrs AND lifnr IN slifnr
                                               AND status EQ pspost.

  LOOP AT lt_hd INTO ls_hd.
* Check if document has been reversed (NPO)
    IF ls_hd-ebeln EQ space.
      PERFORM reversal_check_npo.
* Check if document has been reversed (PO)
    ELSEIF ls_hd-ebeln NE space.
      PERFORM reversal_check_po.
    ENDIF.
* Document not reversed, check if invoice has been cleared/paid
    IF lv_subrc EQ 4.
      PERFORM clearing_check.
    ENDIF.
* Clear return value
    CLEAR lv_subrc.
  ENDLOOP.

* Update tables
  READ TABLE lt_hd INTO ls_hd INDEX 1.
  IF sy-subrc EQ 0.
    UPDATE /nrk/apayhd FROM TABLE lt_hd.
    MODIFY /nrk/apayhis FROM TABLE lt_his.
    COMMIT WORK.
  ELSE.
    MESSAGE e398 WITH text-305 space space space.
  ENDIF.

* Display output
  PERFORM build_fieldcatalog.
  PERFORM build_layout.
  PERFORM display_output.

  CLEAR:   ls_hd, lt_hd, ls_his, lt_his, lt_out, ls_out.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  REVERSAL_CHECK_NPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reversal_check_npo .

* Clean up
  CLEAR: lt_thist, ls_his.

* Get invoice information
  SELECT SINGLE * FROM bkpf WHERE bukrs EQ ls_hd-bukrs AND belnr EQ ls_hd-belnr
                              AND gjahr EQ ls_hd-gjahr AND stblg NE space.
  IF sy-subrc NE 0.
* Invoice not reversed
    lv_subrc = 4.
  ELSE.

* Invoice reversed, get document data from reversal document
    SELECT SINGLE * FROM bkpf WHERE bukrs EQ ls_hd-bukrs AND belnr EQ bkpf-stblg AND gjahr EQ bkpf-stjah.
    IF sy-subrc NE 0.
* No reversal document found
      ls_out-apayno = ls_hd-apayno.
      ls_out-bukrs  = ls_hd-bukrs.
      ls_out-belnr  = ls_hd-belnr.
      ls_out-gjahr  = ls_hd-gjahr.
      ls_out-msg    = text-303.
      APPEND ls_out TO lt_out.
      lv_subrc = 4.

    ELSE.
* Reversal document found, set status at header level
      MOVE psrvrs TO ls_hd-status.
      MODIFY lt_hd FROM ls_hd.
* Fill status history
      SELECT * FROM /nrk/apayhis INTO TABLE lt_thist WHERE apayno EQ ls_hd-apayno.
* Process item number
      DESCRIBE TABLE lt_thist LINES lv_lines.
* Set values
      CLEAR ls_his.
      ls_his-apayno = ls_hd-apayno.
      ls_his-item   = lv_lines + 1.
      ls_his-status = psrvrs.
      ls_his-sdate  = bkpf-cpudt.
      ls_his-stime  = bkpf-cputm.
      ls_his-suser  = bkpf-usnam.
* Get full user name
      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          username = bkpf-usnam
        IMPORTING
          address  = ls_addr3
        TABLES
          return   = lt_ret2.

      CONCATENATE ls_addr3-firstname ls_addr3-lastname INTO ls_his-sname SEPARATED BY space.
* Append history table
      APPEND ls_his TO lt_his.
* Write to output table
      ls_out-apayno = ls_hd-apayno.
      ls_out-bukrs  = ls_hd-bukrs.
      ls_out-belnr  = ls_hd-belnr.
      ls_out-gjahr  = ls_hd-gjahr.
      ls_out-msg    = text-301.
      APPEND ls_out TO lt_out.

* Raise event REVERSED
      IF workflow EQ 'X'.

        MOVE ls_hd-apayno TO l_objectkey.
        CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
          EXPORTING
            object_type             = '/NRK/APAY'
            object_key              = l_objectkey
            event                   = 'reversed'
            commit_work             = 'X'
            event_language          = sy-langu
*           LANGUAGE                = SY-LANGU
            user                    = sy-uname.

      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    " REVERSAL_CHECK_NPO
*&---------------------------------------------------------------------*
*&      Form  REVERSAL_CHECK_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reversal_check_po .

  DATA: l_belnr LIKE rbkp-belnr.

* Clean up
  CLEAR: lt_thist, ls_his.

* Get LIV information
  SELECT SINGLE * FROM rbkp WHERE belnr EQ ls_hd-liv_belnr AND gjahr EQ ls_hd-liv_gjahr AND stblg NE space.

  IF sy-subrc NE 0.
* Invoice not reversed
    lv_subrc = 4.
  ELSE.

* Invoice reversed, get document data from reversal document
    SELECT SINGLE * FROM rbkp WHERE belnr EQ rbkp-stblg AND gjahr EQ rbkp-stjah.

    IF sy-subrc NE 0.
* No reversal document found
      ls_out-apayno = ls_hd-apayno.
      ls_out-bukrs  = ls_hd-bukrs.
      ls_out-belnr  = ls_hd-belnr.
      ls_out-gjahr  = ls_hd-gjahr.
      ls_out-msg    = text-303.
      APPEND ls_out TO lt_out.
      lv_subrc = 4.

    ELSE.
* Reversal document found, set status at header level
      MOVE psrvrs TO ls_hd-status.
      MODIFY lt_hd FROM ls_hd.
* Fill status history
      SELECT * FROM /nrk/apayhis INTO TABLE lt_thist WHERE apayno EQ ls_hd-apayno.
* Process item number
      DESCRIBE TABLE lt_thist LINES lv_lines.
* Set values
      CLEAR ls_his.
      ls_his-apayno = ls_hd-apayno.
      ls_his-item   = lv_lines + 1.
      ls_his-status = psrvrs.
      ls_his-sdate  = rbkp-cpudt.
      ls_his-stime  = rbkp-cputm.
      ls_his-suser  = rbkp-usnam.
* Get full user name
      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          username = rbkp-usnam
        IMPORTING
          address  = ls_addr3
        TABLES
          return   = lt_ret2.
      CONCATENATE ls_addr3-firstname ls_addr3-lastname INTO ls_his-sname SEPARATED BY space.

* Append history table
      APPEND ls_his TO lt_his.

* Write to output table
      ls_out-apayno = ls_hd-apayno.
      ls_out-bukrs  = ls_hd-bukrs.
      ls_out-belnr  = ls_hd-belnr.
      ls_out-gjahr  = ls_hd-gjahr.
      ls_out-msg    = text-301.
      APPEND ls_out TO lt_out.

* Raise event REVERSED
      IF workflow EQ 'X'.

        MOVE ls_hd-apayno TO l_objectkey.
        CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
          EXPORTING
            object_type             = '/NRK/APAY'
            object_key              = l_objectkey
            event                   = 'reversed'
            commit_work             = 'X'
            event_language          = sy-langu
*           LANGUAGE                = SY-LANGU
            user                    = sy-uname.

      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    " REVERSAL_CHECK_PO
*&---------------------------------------------------------------------*
*&      Form  CLEARING_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clearing_check .

  DATA: ls_payr TYPE payr.

* Clean up
  CLEAR: lt_thist, ls_his.

  SELECT SINGLE * FROM bseg WHERE bukrs EQ ls_hd-bukrs AND belnr EQ ls_hd-belnr
                              AND gjahr EQ ls_hd-gjahr AND bschl IN (c_bsc31, c_bsc21)
                              AND augbl NE space.

  IF sy-subrc NE 0.
* Invoice not cleared
    lv_subrc = 4.
  ELSE.

*** change 04/22/2015 - APay Center 2.1.29
*** change 07/30/2015 - APay Center 2.2.1

** Invoice cleared, get document data from clearing document
*    SELECT SINGLE * FROM bkpf
*      WHERE bukrs EQ ls_hd-bukrs
*        AND belnr EQ bseg-augbl
*        AND cpudt EQ bseg-augcp. " AND budat EQ bseg-augdt.
*
*    IF sy-subrc NE 0.
** Clearing document not found
*      ls_out-apayno = ls_hd-apayno.
*      ls_out-bukrs  = ls_hd-bukrs.
*      ls_out-belnr  = ls_hd-belnr.
*      ls_out-gjahr  = ls_hd-gjahr.
*      ls_out-msg    = text-304.
*      APPEND ls_out TO lt_out.
*      lv_subrc = 4.
*
*    ELSE.

    SELECT SINGLE * FROM bsak INTO wa_bsak
      WHERE bukrs EQ ls_hd-bukrs
        AND belnr EQ ls_hd-belnr
        AND gjahr EQ ls_hd-gjahr.

    IF sy-subrc NE 0. "not cleared

** Clearing document not found
      ls_out-apayno = ls_hd-apayno.
      ls_out-bukrs  = ls_hd-bukrs.
      ls_out-belnr  = ls_hd-belnr.
      ls_out-gjahr  = ls_hd-gjahr.
      ls_out-msg    = text-304.
      APPEND ls_out TO lt_out.
      lv_subrc = 4.

    ELSE.

* Clearing document found, add payment information

      SELECT SINGLE * FROM payr INTO wa_payr
        WHERE zbukr EQ wa_bsak-bukrs
          AND vblnr EQ wa_bsak-augbl
          AND gjahr EQ wa_bsak-gjahr.

      IF sy-subrc EQ 0.
        ls_hd-chect = wa_payr-chect.
        ls_hd-rwbtr = wa_payr-rwbtr.
        ls_hd-bancd = wa_payr-bancd.
      ENDIF.

      ls_hd-augbl = wa_bsak-augbl.
      ls_hd-augdt = wa_bsak-augdt.

      SELECT SINGLE * FROM bkpf INTO wa_bkpf
        WHERE bukrs EQ ls_hd-bukrs
          AND belnr EQ ls_hd-augbl
          AND gjahr EQ ls_hd-gjahr.

*      CALL FUNCTION 'GET_CHECK_INFORMATION'
*        EXPORTING
*          i_augbl   = bkpf-belnr
*          i_augdt   = bkpf-budat
*          i_belnr   = ls_hd-belnr
*          i_bukrs   = ls_hd-bukrs
*          i_bvorg   = bkpf-bvorg
*          i_gjahr   = bkpf-gjahr
*        IMPORTING
*          e_payr    = ls_payr
*        EXCEPTIONS
*          not_found = 1
*          OTHERS    = 2.
*
*      IF sy-subrc EQ 0.
** Check information found
*        ls_hd-chect = ls_payr-chect.
*        ls_hd-rwbtr = ls_payr-rwbtr.
*        ls_hd-bancd = ls_payr-bancd.
*      ENDIF.
*
* Set data at header level
*     ls_hd-augbl = bkpf-belnr.
*     ls_hd-augdt = bkpf-budat.

      MOVE pspaid TO ls_hd-status.
      MODIFY lt_hd FROM ls_hd.

*** end of change

* Fill status history
      SELECT * FROM /nrk/apayhis INTO TABLE lt_thist WHERE apayno EQ ls_hd-apayno.
* Process item number
      DESCRIBE TABLE lt_thist LINES lv_lines.
* Set values
      CLEAR ls_his.
      ls_his-apayno = ls_hd-apayno.
      ls_his-item   = lv_lines + 1.
      ls_his-status = pspaid.
      ls_his-sdate  = wa_bkpf-cpudt.
      ls_his-stime  = wa_bkpf-cputm.
      ls_his-suser  = wa_bkpf-usnam.

* Get full user name
      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          username = wa_bkpf-usnam
        IMPORTING
          address  = ls_addr3
        TABLES
          return   = lt_ret2.

      CONCATENATE ls_addr3-firstname ls_addr3-lastname INTO ls_his-sname SEPARATED BY space.
* Append history table
      APPEND ls_his TO lt_his.
* Write to output table
      ls_out-apayno = ls_hd-apayno.
      ls_out-bukrs  = ls_hd-bukrs.
      ls_out-belnr  = ls_hd-belnr.
      ls_out-gjahr  = ls_hd-gjahr.
      ls_out-msg    = text-302.
      APPEND ls_out TO lt_out.

* Raise event CLEARED
      IF workflow EQ 'X'.

        MOVE ls_hd-apayno TO l_objectkey.
        CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
          EXPORTING
            object_type             = '/NRK/APAY'
            object_key              = l_objectkey
            event                   = 'cleared'
            commit_work             = 'X'
            event_language          = sy-langu
*           LANGUAGE                = SY-LANGU
            user                    = sy-uname.

      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    " CLEARING_CHECK
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

  lt_fieldcatalog-fieldname   = 'MSG'.
  lt_fieldcatalog-seltext_m   = 'Message'.
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
