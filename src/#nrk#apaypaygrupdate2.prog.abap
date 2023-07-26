*&---------------------------------------------------------------------*
*& APay Center 3.0 - APayCenter Goods Receipt update program
*&
*& (c) by Norikkon, LLC 2023
*&---------------------------------------------------------------------*
REPORT  /nrk/apaypaygrupdate2 MESSAGE-ID 00.

TABLES: /nrk/apayhd, /nrk/apayhis, /nrk/apaysdef, mseg.

TYPES: BEGIN OF t_output,
         apayno  LIKE /nrk/apayhd-apayno,
         ebeln   LIKE /nrk/apayhd-ebeln,
         xblnr   LIKE /nrk/apayhd-xblnr,
         bukrs   TYPE bukrs,
         lifname LIKE /nrk/apayhd-lifname,
         lifnr   LIKE /nrk/apayhd-lifnr,
*        belnr   TYPE belnr_d,
*        gjahr   TYPE gjahr,
*        msg(50) TYPE c,
       END OF t_output.

DATA: lt_hd    LIKE /nrk/apayhd OCCURS 500,
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

*CONSTANTS: c_bsc21    LIKE bseg-bschl     VALUE '21',
*           c_bsc31    LIKE bseg-bschl     VALUE '31'.

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

PARAMETERS: pswait LIKE /nrk/apaysdef-status OBLIGATORY DEFAULT '3110'.       "Status posted
SELECTION-SCREEN ULINE.
PARAMETERS: pspart LIKE /nrk/apaysdef-status OBLIGATORY DEFAULT '3040'.       "Status paid

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
      PERFORM check_gr_for_po.

    ENDIF.

* Clear return value
    CLEAR lv_subrc.

  ENDLOOP.

* Update tables
  READ TABLE lt_hd INTO ls_hd INDEX 1.
  IF sy-subrc EQ 0.
    UPDATE /nrk/apayhd FROM TABLE lt_hd.
    IF NOT lt_his[] IS INITIAL.
      MODIFY /nrk/apayhis FROM TABLE lt_his.
    ENDIF.
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

* MOVE-CORRESPONDING lt_hd to lt_out.

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

  lt_fieldcatalog-fieldname   = 'EBELN'.
  lt_fieldcatalog-seltext_m   = 'Purchase Order'.
  lt_fieldcatalog-col_pos     = 1.
  APPEND lt_fieldcatalog TO lt_fieldcatalog.
  CLEAR lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'XBLNR'.
  lt_fieldcatalog-seltext_m   = 'External Reference Number'.
  lt_fieldcatalog-col_pos     = 2.
  APPEND lt_fieldcatalog TO lt_fieldcatalog.
  CLEAR lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'BUKRS'.
  lt_fieldcatalog-seltext_m   = 'Company Code'.
  lt_fieldcatalog-col_pos     = 3.
  APPEND lt_fieldcatalog TO lt_fieldcatalog.
  CLEAR lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'LIFNAME'.
  lt_fieldcatalog-seltext_m   = 'Vendor name'.
  lt_fieldcatalog-col_pos     = 4.
  APPEND lt_fieldcatalog TO lt_fieldcatalog.
  CLEAR lt_fieldcatalog.

  lt_fieldcatalog-fieldname   = 'LIFNR'.
  lt_fieldcatalog-seltext_m   = 'Vendor number'.
  lt_fieldcatalog-col_pos     = 5.
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

  DATA: lv_grexist  TYPE syst-input,
        ls_ekbe     TYPE ekbe,
        ls_ekko     TYPE ekko,
        ls_ekpo     TYPE ekpo,
        lv_menge    TYPE menge_d,
        lv_menge_we TYPE menge_d,
        lv_menge_re TYPE menge_d,
        lv_menge_po TYPE menge_d,
        lv_ebelp    LIKE ekpo-ebelp,
        lv_elikz    LIKE ekpo-elikz,
        lv_shkzg    TYPE shkzg,
        t_ekpo      TYPE ekpo OCCURS 0.

  DATA: gr_result LIKE syst-input.

  DATA: BEGIN OF items OCCURS 0,
          ebeln    LIKE ekko-ebeln,
          ebelp    LIKE ekpo-ebelp,
          po_menge LIKE ekpo-menge,
          gr_menge LIKE ekbe-menge,
          re_menge LIKE ekbe-menge,
          elikz    LIKE ekpo-elikz,
          validate LIKE syst-input,
        END OF items.

  DATA: t_items LIKE items OCCURS 0.

* Clean up
  CLEAR: lv_grexist, ls_ekbe, ls_ekko, ls_ekpo, lv_menge, lv_menge_we, lv_menge_re, lv_shkzg, t_ekpo[].

* Check if purchase order number is valid
  SELECT SINGLE * FROM ekko INTO ls_ekko WHERE ebeln EQ ls_hd-ebeln.

  IF sy-subrc EQ 0.

* Check if invoice verification is needed
    SELECT * FROM ekpo INTO TABLE t_ekpo WHERE ebeln EQ ls_hd-ebeln
      AND webre EQ 'X'.

    IF sy-subrc NE 0. " No invoice verification needed
      gr_result = '3'.
*     EXIT.
    ELSE.
      LOOP AT t_ekpo INTO ls_ekpo.
        lv_menge_po = lv_menge_po + ls_ekpo-menge.
        items-ebeln = ls_ekpo-ebeln.
        items-ebelp = ls_ekpo-ebelp.
        items-po_menge = ls_ekpo-menge.
        items-elikz = ls_ekpo-elikz.
        APPEND items TO t_items.
      ENDLOOP.
    ENDIF.

* Check if we need a goods receipt
    SELECT SINGLE * FROM ekpo INTO ls_ekpo WHERE ebeln EQ ls_hd-ebeln
                                             AND wepos EQ 'X'.
    IF sy-subrc NE 0.
* Nothing found, no goods receipt needed
      gr_result = '2'.
*   EXIT.
    ELSE.
* Get purchase order history
      CLEAR: lv_menge, lv_menge_we, lv_menge_re, lv_elikz.

* Get GR quantity
      SELECT ebelp shkzg menge INTO (lv_ebelp, lv_shkzg, lv_menge)
        FROM ekbe WHERE ebeln EQ ls_hd-ebeln AND vgabe EQ '1'.
        IF lv_shkzg EQ 'H'.
* Credit with negative amount
          lv_menge = lv_menge * ( -1 ).
        ENDIF.

        READ TABLE t_items INTO items WITH KEY ebelp = lv_ebelp.
        items-gr_menge = items-gr_menge + lv_menge.
        MODIFY t_items FROM items INDEX sy-tabix.

      ENDSELECT.

      CLEAR: lv_menge, lv_shkzg.

* Get RE quantity
      SELECT ebelp shkzg menge INTO (lv_ebelp, lv_shkzg, lv_menge)
        FROM ekbe WHERE ebeln EQ ls_hd-ebeln AND vgabe EQ '2'.
        IF lv_shkzg EQ 'H'.
* Credit with negative amount
          lv_menge = lv_menge * ( -1 ).
        ENDIF.

        READ TABLE t_items INTO items WITH KEY ebelp = lv_ebelp.
        items-re_menge = items-re_menge + lv_menge.
        MODIFY t_items FROM items INDEX sy-tabix.

      ENDSELECT.

*** Validate GR scenarios
      LOOP AT t_items INTO items.

* GR scenario 1
        IF items-po_menge  > 0
          AND items-gr_menge = 0
          AND items-re_menge = 0.  " Waiting for GR

          items-validate = '1'.
          MODIFY t_items FROM items.

* GR scenario 2
        ELSEIF items-po_menge > items-gr_menge
          AND items-gr_menge > items-re_menge. " 0 - Open goods receipt found

          items-validate = '0'.
          MODIFY t_items FROM items.

* GR scenario 3
        ELSEIF items-po_menge > items-gr_menge
          AND items-gr_menge = items-re_menge. " 1 - Waiting for GR

          IF items-elikz = 'X'.
            items-validate = '4'. " all goods invoiced
          ELSE.
            items-validate = '1'. " Waiting for GR
          ENDIF.

          MODIFY t_items FROM items.

* GR scenario 4&5
        ELSEIF items-po_menge = items-gr_menge
          AND items-gr_menge > items-re_menge. " 0 - Open goods receipt found

          items-validate = '0'.
          MODIFY t_items FROM items.

* GR scneario 6
        ELSEIF items-po_menge > 0
          AND items-po_menge = items-gr_menge
          AND items-gr_menge = items-re_menge. " 4 - All received goods invoiced

          items-validate = '4'.
          MODIFY t_items FROM items.

* GR scenario 7
        ELSEIF items-po_menge < items-gr_menge
           AND items-gr_menge = items-re_menge. " 4 - All received goods invoiced

          items-validate = '4'.
          MODIFY t_items FROM items.

        ELSEIF items-po_menge < items-gr_menge
           AND items-gr_menge > items-re_menge. " 0 - Open goods receipt found

          items-validate = '0'.
          MODIFY t_items FROM items.

        ELSEIF items-po_menge = 0. " 2 - No GR needed

          items-validate = '2'.
          MODIFY t_items FROM items.

        ENDIF.

      ENDLOOP.

      READ TABLE t_items INTO items WITH KEY validate = '2'.
      IF sy-subrc = 0. " No GR needed
        gr_result = '2'.
      ENDIF.

      READ TABLE t_items INTO items WITH KEY validate = '4'.
      IF sy-subrc = 0. " All received goods invoiced
        gr_result = '4'.
      ENDIF.

      READ TABLE t_items INTO items WITH KEY validate = '1'.
      IF sy-subrc = 0. " Waiting for GR
        gr_result = '1'.
      ENDIF.

      READ TABLE t_items INTO items WITH KEY validate = '0'.
      IF sy-subrc = 0. " Open goods receipt found
        gr_result = '0'.
      ENDIF.

      CLEAR: lv_menge, lv_shkzg, lv_menge_po, lv_menge_we, lv_menge_re.

    ENDIF.

    CLEAR: lv_menge, lv_shkzg, lv_menge_po, lv_menge_we, lv_menge_re.

    IF gr_result = '0'. " Open goods receipt exists
      lv_grexist = '0'.
    ELSEIF gr_result = '1'. " Waiting for GR
      lv_grexist = '1'.
    ELSEIF gr_result = '2'. " No GR needed
      lv_grexist = '1'.
    ELSEIF gr_result = '3'. "
      lv_grexist = '1'.
    ELSEIF gr_result = '4'. " All received goods invoiced
      lv_grexist = '1'.
    ENDIF.

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
*   APPEND ls_his TO lt_his.

* Trigger Workflow event
    IF workflow EQ 'X'.

      MOVE ls_hd-apayno TO l_objectkey.
      CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
        EXPORTING
          object_type    = '/NRK/APAY'
          object_key     = l_objectkey
          event          = 'GoodsReceived'
          commit_work    = 'X'
          event_language = sy-langu
*         LANGUAGE       = SY-LANGU
          user           = sy-uname.

    ENDIF.

  ENDIF.

ENDFORM.                    " CHECK_GR_FOR_PO
