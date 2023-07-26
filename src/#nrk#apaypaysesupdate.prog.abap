*&---------------------------------------------------------------------*
*& APay Center 1.0 - APay Center service entry sheet update program
*& (c) by Norikkon, LLC 2017
*&---------------------------------------------------------------------*
REPORT  /nrk/apaypaysesupdate MESSAGE-ID 00.

TABLES: /nrk/apayhd, /nrk/apayhis, /nrk/apaysdef, mseg.

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
      l_objectkey LIKE  swr_struct-object_key.

TYPE-POOLS: slis.

DATA: lt_fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      ls_layout       TYPE slis_layout_alv,
      lv_repid        LIKE sy-repid.

* -----------------------------------------------------------
* SELECTION SCREEN DEFINITION
* -----------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK procs WITH FRAME TITLE text-001.

SELECT-OPTIONS sapayno FOR /nrk/apayhd-apayno.        "APay record number
SELECT-OPTIONS sbukrs  FOR /nrk/apayhd-bukrs.         "Company code
SELECT-OPTIONS slifnr  FOR /nrk/apayhd-lifnr.         "Vendor number
SELECT-OPTIONS sebeln  FOR /nrk/apayhd-ebeln.         "Purchase Order

SELECTION-SCREEN END OF BLOCK procs.

SELECTION-SCREEN BEGIN OF BLOCK statusto WITH FRAME TITLE text-002.

PARAMETERS: pswait LIKE /nrk/apaysdef-status OBLIGATORY DEFAULT '3310'.       "Waiting for services
SELECTION-SCREEN ULINE.
PARAMETERS: pspart LIKE /nrk/apaysdef-status OBLIGATORY DEFAULT '3320'.       "Services received

SELECTION-SCREEN END OF BLOCK statusto.

SELECTION-SCREEN BEGIN OF BLOCK workflow WITH FRAME TITLE text-003.

PARAMETER: workflow AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK workflow.

PARAMETERS: phits TYPE i DEFAULT 500.

* -----------------------------------------------------------
* AT SELECTION-SCREEN
* -----------------------------------------------------------
AT SELECTION-SCREEN ON pswait.
* Check input status
  SELECT SINGLE * FROM /nrk/apaysdef WHERE status EQ pswait.
  IF sy-subrc NE 0.
    MESSAGE e398 WITH text-200 pswait text-300 space.
  ENDIF.

AT SELECTION-SCREEN ON pspart.
* Check input status
  SELECT SINGLE * FROM /nrk/apaysdef WHERE status EQ pspart.
  IF sy-subrc NE 0.
    MESSAGE e398 WITH text-200 pspart text-300 space.
  ENDIF.

* -----------------------------------------------------------
* START OF SELECTION
* -----------------------------------------------------------
START-OF-SELECTION.
* Check all posted invoices in APay
  SELECT * INTO TABLE lt_hd FROM /nrk/apayhd UP TO phits ROWS
    WHERE apayno IN sapayno
      AND ebeln IN sebeln
      AND bukrs IN sbukrs
      AND lifnr IN slifnr
      AND status EQ pswait.

  LOOP AT lt_hd INTO ls_hd.

* check if document has purchase order.
    IF NOT ls_hd-ebeln IS INITIAL.

* Check if PO received GR
*     PERFORM check_gr_for_po.
      PERFORM check_ses_for_po.

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
*&---------------------------------------------------------------------*
*&      Form  CHECK_GR_FOR_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_gr_for_po .

* Check if goods receipt exists for purchase order
* 0 - Open GR exists
* 1 - No open GR exists

  DATA: lv_grexist    TYPE syst-input,
        ls_ekbe       TYPE ekbe,
        ls_ekko       TYPE ekko,
        ls_ekpo       TYPE ekpo,
        lv_menge      TYPE menge_d,
        lv_menge_we   TYPE menge_d,
        lv_menge_re   TYPE menge_d,
        lv_shkzg      TYPE shkzg.

* Clean up
  CLEAR: lv_grexist, ls_ekbe, ls_ekko, ls_ekpo, lv_menge, lv_menge_we, lv_menge_re, lv_shkzg.

* Check if purchase order number is valid
  SELECT SINGLE * FROM ekko INTO ls_ekko WHERE ebeln EQ ls_hd-ebeln.
  IF sy-subrc EQ 0.

* Check document type
    CASE ls_ekko-bstyp.

      WHEN 'L'.
* Scheduling agreement / Lieferplan, check if GR quantity is less than RE quantity
        CLEAR: lv_menge, lv_menge_we, lv_menge_re.
* Get GR quantity
        SELECT shkzg menge INTO (lv_shkzg, lv_menge) FROM ekbe WHERE ebeln EQ ls_hd-ebeln AND vgabe EQ '1'.
          IF lv_shkzg EQ 'H'.
* Credit with negative amount
            lv_menge = lv_menge * ( -1 ).
          ENDIF.
          lv_menge_we = lv_menge_we + lv_menge.
        ENDSELECT.
        CLEAR: lv_menge, lv_shkzg.
* Get RE quantity
        SELECT shkzg menge INTO (lv_shkzg, lv_menge)
          FROM ekbe WHERE ebeln EQ ls_hd-ebeln AND vgabe EQ '2'.
          IF lv_shkzg EQ 'H'.
* Credit with negative amount
            lv_menge = lv_menge * ( -1 ).
          ENDIF.
          lv_menge_re = lv_menge_re + lv_menge.
        ENDSELECT.
        CLEAR: lv_menge, lv_shkzg.
* Compare
        IF lv_menge_we > lv_menge_re.
* Quantity delivered is larger than quantity invoiced, open GR exists
          lv_grexist = '0'.
        ELSE.
* Quantity delivered is less or equal than quantity invoiced, no open GR exists
          lv_grexist = '1'.
        ENDIF.

* Purchase order / Normalbestellung, check only if GR exists
      WHEN OTHERS.
* Get purchase order history
        SELECT SINGLE * FROM ekbe INTO ls_ekbe WHERE ebeln EQ ls_hd-ebeln
                                                 AND vgabe EQ '1'.
        IF sy-subrc EQ 0.
* Open goods receipt exists
          lv_grexist = '0'.
        ELSE.
* No goods receipt exists
          lv_grexist = '1'.
        ENDIF.

    ENDCASE.
  ENDIF.

* Open goods receipt exists
  IF lv_grexist EQ 0.
* Update APay Center Header
    MOVE pspart TO ls_hd-status.
    MODIFY lt_hd FROM ls_hd.

* Update APay Center History
    SELECT * FROM /nrk/apayhis INTO TABLE lt_thist WHERE apayno EQ ls_hd-apayno.
* Process item number
    DESCRIBE TABLE lt_thist LINES lv_lines.
* Set values
    CLEAR ls_his.
    ls_his-apayno = ls_hd-apayno.
    ls_his-item   = lv_lines + 1.
    ls_his-sdate  = sy-datum.
    ls_his-stime  = sy-uzeit.
    ls_his-suser  = sy-uname.
    ls_his-status = pspart.
* Get full user name
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = sy-uname
      IMPORTING
        address  = ls_addr3
      TABLES
        return   = lt_ret2.

    CONCATENATE ls_addr3-firstname ls_addr3-lastname INTO ls_his-sname SEPARATED BY space.
* Append history table
    APPEND ls_his TO lt_his.

* Trigger Workflow event
    IF workflow EQ 'X'.

      MOVE ls_hd-apayno TO l_objectkey.
      CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
        EXPORTING
          object_type             = '/NRK/APAY'
          object_key              = l_objectkey
          event                   = 'GoodsReceived'
          commit_work             = 'X'
          event_language          = sy-langu
*           LANGUAGE                = SY-LANGU
          user                    = sy-uname.

    ENDIF.

  ENDIF.

ENDFORM.                    " CHECK_GR_FOR_PO
*&---------------------------------------------------------------------*
*&      Form  CHECK_SES_FOR_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ses_for_po .

  DATA: result TYPE num1,
        ernam  TYPE char12.

  CLEAR: result.

  CALL FUNCTION '/NRK/APAY_VALIDATE_SES'
    EXPORTING
      ebeln  = ls_hd-ebeln
    IMPORTING
      RESULT = RESULT
      ernam  = ernam.

* Result 1: No service entry sheet exists
* Result 2: Services partially received
*           Total SES value: ESSR-LWERT
* Result 3: All services received
* Result 4: More services received than on PO

  IF result EQ 1. " No services received
    CLEAR: workflow.
  ELSEIF result EQ 2. "Some service received
    workflow = 'X'.
  ELSEIF result EQ 3. " All services received
    workflow = 'X'.
  ELSEIF result EQ 4.
    workflow = 'X'.
  ENDIF.

* Trigger Workflow event
  IF workflow EQ 'X'.

    MOVE ls_hd-apayno TO l_objectkey.
    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
      EXPORTING
        object_type    = '/NRK/APAY'
        object_key     = l_objectkey
        event          = 'ServicesReceived'
        commit_work    = 'X'
        event_language = sy-langu
        user           = sy-uname.

  ENDIF.

ENDFORM.                    " CHECK_SES_FOR_PO
