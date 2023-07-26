*&---------------------------------------------------------------------*
*&  Include           /NRK/APAYFORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_RECORD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_record_data .

  CLEAR: t_apayhd_dis, t_apayhd_dis[], g_count.

*By Athma 11/02/2021
  DATA : tab_bukrs TYPE STANDARD TABLE OF rsis_s_range.
  DATA w_bukrs TYPE rsis_s_range.

* Check user permission based on company code
  CALL FUNCTION '/NRK/APAYCHECKUSERBYBUKRS'
    EXPORTING
      user              = sy-uname
    IMPORTING
      all_documents     = all_docs
*By Athma 11/02/2021
      limited_documents = limit_docs
    TABLES
      tab_user          = tab_user_bukrs
      range             = tab_bukrs
    EXCEPTIONS
      no_permission     = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH text-985 space space space.
  ENDIF.

* Filter records by company code.
  IF limit_docs EQ 'X'.

    IF NOT s_bukrs[] IS INITIAL.

      DATA: l_bukrs       LIKE /nrk/apayhd-bukrs,
            wa_bukrs      LIKE s_bukrs,
            wa_user_bukrs LIKE /nrk/apayuser.

*     CHECK selection BUKRS against ALLOWED bukrs
      LOOP AT s_bukrs INTO wa_bukrs.

        MOVE wa_bukrs-low TO l_bukrs.
        READ TABLE tab_user_bukrs INTO wa_user_bukrs
          WITH KEY bukrs = l_bukrs.
        IF sy-subrc NE 0.
*         Delete selection from s_bukrs
          DELETE s_bukrs WHERE low EQ wa_bukrs-low.
        ENDIF.
      ENDLOOP.

    ELSE. " No selection

      LOOP AT tab_user_bukrs INTO wa_user_bukrs.
        MOVE 'EQ' TO s_bukrs-option.
        MOVE 'I'  TO s_bukrs-sign.
        MOVE wa_user_bukrs-bukrs TO s_bukrs-low.
        APPEND s_bukrs.
      ENDLOOP.
    ENDIF.

  ENDIF.

  IF s_hstat[] IS INITIAL.

* Get data
    SELECT * FROM /nrk/apayhd
      INTO CORRESPONDING FIELDS OF TABLE t_apayhd_dis
      UP TO p_max ROWS
      WHERE apayno IN s_apayno
        AND bukrs  IN s_bukrs
        AND belnr  IN s_belnr
        AND gjahr  IN s_gjahr
        AND xblnr  IN s_xblnr
        AND bldat  IN s_bldat
        AND wrbtr  IN s_wrbtr
        AND waers  IN s_waers
        AND lifnr  IN s_lifnr
        AND ebeln  IN s_ebeln
        AND cdate  IN s_cdate
        AND ekgrp  IN s_ekgrp
        AND ernam  IN s_ernam
        AND liv_belnr IN s_livbl
        AND lifname IN s_lname
        AND duedate IN s_dueda
        AND status IN s_cstat
        AND ext_approver IN s_extu
        AND ar_object IN s_arobj.

  ELSE.

* get data
    SELECT * FROM /nrk/apayhd
      INTO CORRESPONDING FIELDS OF TABLE t_apayhd_dis
      UP TO p_max ROWS
      WHERE apayno IN s_apayno
        AND bukrs  IN s_bukrs
        AND belnr  IN s_belnr
        AND gjahr  IN s_gjahr
        AND xblnr  IN s_xblnr
        AND bldat  IN s_bldat
        AND wrbtr  IN s_wrbtr
        AND waers  IN s_waers
        AND lifnr  IN s_lifnr
        AND ebeln  IN s_ebeln
        AND cdate  IN s_cdate
        AND ekgrp  IN s_ekgrp
        AND ernam  IN s_ernam
        AND liv_belnr IN s_livbl
        AND lifname IN s_lname
        AND duedate IN s_dueda
        AND status IN s_cstat
        AND ext_approver IN s_extu
        AND ar_object IN s_arobj
        AND apayno IN ( SELECT DISTINCT apayno FROM /nrk/apayhis
          WHERE status IN s_hstat ).

  ENDIF.

*by Athma 11/02/2021
  DESCRIBE TABLE tab_bukrs.
  IF sy-tfill GE 1.
    READ TABLE tab_bukrs INTO w_bukrs WITH KEY low = '*'.
    IF sy-subrc NE 0.
      DELETE t_apayhd_dis WHERE bukrs NOT IN tab_bukrs.
    ENDIF.
  ENDIF.

* Add more header information
  LOOP AT t_apayhd_dis INTO wa_dis.

* status description
    READ TABLE t_status INTO wa_status
      WITH KEY status = wa_dis-status.

    IF sy-subrc EQ 0.
      wa_dis-sdescr = wa_status-sdescr.
    ELSE.
      SELECT SINGLE * FROM /nrk/apaysdef INTO wa_status
        WHERE status EQ wa_dis-status
          AND langu EQ sy-langu.

      wa_dis-sdescr = wa_status-sdescr.
      APPEND wa_status TO t_status.
    ENDIF.

* Add status icons
    IF wa_status-stype = 'C'.
      wa_dis-status_light = '@08@'.   "Green light
    ELSEIF wa_status-stype = 'P'.
      wa_dis-status_light = '@09@'.   "Yello light
    ELSEIF wa_status-stype = 'E'.
      wa_dis-status_light = '@0A@'.   "Red light
    ELSEIF wa_status-stype = 'R'.
      wa_dis-status_light = '@0A@'.   "Red light
    ENDIF.

* Check for comments
    SELECT SINGLE * FROM /nrk/apaycmnt WHERE apayno EQ wa_dis-apayno.
    IF sy-subrc EQ 0.
      wa_dis-comment_icon = '@0L@'.   "Comment exists
    ENDIF.

* Document type description
    SELECT SINGLE objecttext FROM toasp INTO wa_dis-objecttext
      WHERE ar_object = wa_dis-ar_object
        AND language = sy-langu.


* Set debit/credit
    MOVE wa_dis-shkzg TO domvalue.

    CALL FUNCTION 'RV_DOMAIN_VALUE_TEXTS'
      EXPORTING
        domname  = 'SHKZG'
        domvalue = domvalue
        single   = 'X'
      IMPORTING
        ddtext   = ddtext.

    IF ddtext IS NOT INITIAL.
      MOVE ddtext TO wa_dis-dcind.
    ENDIF.

* Calculate idle time
    IF wa_status-stype NE 'C'.
      CALL FUNCTION 'DELTA_TIME_DAY_HOUR'
        EXPORTING
          t1      = wa_dis-lastchanget
          t2      = sy-uzeit
          d1      = wa_dis-lastchange
          d2      = sy-datum
        IMPORTING
          minutes = minutes.

      hours = minutes / 60.
      days = hours / 24.
      wa_dis-idletime = days.
      wa_dis-idlehours = hours.

    ELSE.
      wa_dis-idletime = 0.
    ENDIF.

    CLEAR: minutes,
           hours,
           days.

* Calculate processing time
    IF wa_status-stype EQ 'C'.
      basedate = wa_dis-lastchange.
      basetime = wa_dis-lastchanget.
    ELSE.
      basedate = sy-datum.
      basetime = sy-uzeit.
    ENDIF.

    CALL FUNCTION 'DELTA_TIME_DAY_HOUR'
      EXPORTING
        t1      = wa_dis-ctime
        t2      = basetime
        d1      = wa_dis-cdate
        d2      = basedate
      IMPORTING
        minutes = minutes.

    hours = minutes / 60.
    days = hours / 24.
    wa_dis-processtime = days.
    wa_dis-processhours = hours.

    CLEAR: minutes,
           hours,
           days.

* convert system time to local time
    DATA: timestamp LIKE tzonref-tstamps,
          tzonesys LIKE ttzcu-tzonesys.

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = tzonesys
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    CALL FUNCTION 'IB_CONVERT_INTO_TIMESTAMP'
      EXPORTING
        i_datlo     = wa_dis-cdate
        i_timlo     = wa_dis-ctime
        i_tzone     = tzonesys
      IMPORTING
        e_timestamp = timestamp.

    CALL FUNCTION 'IB_CONVERT_FROM_TIMESTAMP'
      EXPORTING
        i_timestamp = timestamp
        i_tzone     = sy-zonlo
      IMPORTING
        e_datlo     = wa_dis-cdate
        e_timlo     = wa_dis-ctime.

* Update grid
    MODIFY t_apayhd_dis FROM wa_dis.

* Add count
    g_count = g_count + 1.

  ENDLOOP.

* Sent message
  MESSAGE s398(00) WITH g_count text-908 space space.

  IF g_view = '1'. " Ledger

  ELSEIF g_view = '2'. " Batch

  ELSEIF g_view = '3'. " Workflow

  ELSEIF g_view = '4'. " Documents

  ENDIF.

ENDFORM.                    " GET_RECORD_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_grid .

* Check if initial
  IF alv_grid IS INITIAL.

* create TOP-Document
    CREATE OBJECT alv_top
      EXPORTING
        style = 'ALV_GRID'.

* create BOTTOM-Document
    CREATE OBJECT alv_bottom
      EXPORTING
        style = 'ALV_GRID'.

    CREATE OBJECT alv_container
      EXPORTING
        container_name              = 'ALV_CONTAINER'
        lifetime                    = cntl_lifetime_dynpro
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT alv_grid
      EXPORTING
        i_parent          = alv_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

* create event receiver
    CREATE OBJECT event_receiver.

* set configuration of alv
    variant-handle   = 'ALV1'.
    variant-report   = sy-repid.
    variant-username = sy-uname.
    variant-variant  = p_vari.

    layout-cwidth_opt   = 'X'.
    layout-grid_title   = 'APay Center'.
    layout-smalltitle   = 'X'.
    layout-sel_mode     = 'A'.
    layout-sgl_clk_hd   = ' '.
    layout-totals_bef   = 'X'.
    layout-zebra        = 'X'.

* setup the field catalog
    PERFORM set_field_catalog.

* instanciate class for hotspot click and set handler
    CREATE OBJECT go_event_processor.
    SET HANDLER go_event_processor->handle_hotspot_click
      FOR alv_grid
      ACTIVATION 'X'.

* display the grid
    CALL METHOD alv_grid->set_table_for_first_display
      EXPORTING
        is_layout       = layout
        is_variant      = variant
        i_save          = 'A'
      CHANGING
        it_outtab       = t_apayhd_dis[]
        it_fieldcatalog = t_fieldcat.

  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " CREATE_GRID
*&---------------------------------------------------------------------*
*&      Form  DRILL_DOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_INDEX  text
*----------------------------------------------------------------------*
FORM drill_down  USING    p_e_row_index.

ENDFORM.                    " DRILL_DOWN
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_catalog .

  DATA: ls_fcat TYPE lvc_s_fcat,
        idx TYPE sy-tabix.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = '/NRK/APAYHD_DIS'
    CHANGING
      ct_fieldcat            = t_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

* Add hotspot flag
  LOOP AT t_fieldcat INTO ls_fcat.
    idx = sy-tabix.

    CASE ls_fcat-fieldname.
      WHEN 'LIFNR' OR 'LIFNAME' OR 'BELNR' OR 'EBELN' OR 'LIV_BELNR'
        OR 'AUGBL' OR 'OBJECTTEXT' OR 'COMMENT_ICON' OR 'SDESCR'
        OR 'WORKFLOWID' OR 'APAYNO'.
        ls_fcat-hotspot = 'X'.
        MODIFY t_fieldcat FROM ls_fcat INDEX idx.
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " SET_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_view .

  DATA: value1 LIKE /nrk/apayconfig-val1.

* Set screen view
*  IF p_ledg EQ 'X'. " Ledger
*
* g_view = '1'.
*
*  ELSEIF p_bat EQ 'X'. " Batch
*
*    g_view = '2'.
*
*  ELSEIF p_docs EQ 'X'. " Documents
*
*    g_view = '3'.
*
*  ELSEIF p_wfs EQ 'X'. " Workflows
*
*    g_view = '4'.
*
*  ENDIF.

  CLEAR: value1.

  SELECT SINGLE  val1 FROM /nrk/apayconfig INTO value1
    WHERE key1 EQ 'APAY'
      AND key2 EQ 'VERSION'.

  IF sy-subrc NE 0. " not configured
    g_view = '1'.
  ELSE.
    IF value1 EQ 1.
      g_view = '1'.
    ELSEIF value1 EQ 3.
      g_view = '7'.
    ENDIF.
  ENDIF.

ENDFORM.                    " SET_SCREEN_VIEW
*&---------------------------------------------------------------------*
*&      Form  LEAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM leave .

  CALL METHOD alv_container->free.
  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " LEAVE
*&---------------------------------------------------------------------*
*&      Form  GET_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_line .

  DATA: lt_index_rows TYPE lvc_t_row,
        ls_index_rows TYPE lvc_s_row.

  CLEAR: lt_index_rows, ls_index_rows, wa_dis.

  CALL METHOD alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_index_rows.

  IF lt_index_rows IS INITIAL.
    MESSAGE s499(sy) WITH text-953 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE lt_index_rows INTO ls_index_rows INDEX 1.

  READ TABLE t_apayhd_dis  INDEX ls_index_rows-index INTO wa_dis.

  IF sy-subrc NE 0.
    MESSAGE s398(00) WITH text-953 space space space.
    EXIT.
  ENDIF.

ENDFORM.                    " GET_LINE
*&---------------------------------------------------------------------*
*&      Form  GET_STATUS_HISTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_status_history .

  CLEAR: t_apay_his, t_apay_his[].

  SELECT * FROM /nrk/apayhis INTO TABLE t_apay_his
    WHERE apayno = wa_dis-apayno.

  LOOP AT t_apay_his INTO wa_his.

    SELECT SINGLE sdescr FROM /nrk/apaysdef INTO wa_his-sdescr
      WHERE status EQ wa_his-status
        AND langu  EQ sy-langu.

    MODIFY t_apay_his FROM wa_his.

  ENDLOOP.

  SORT t_apay_his BY item ASCENDING.

ENDFORM.                    " GET_STATUS_HISTORY
*&---------------------------------------------------------------------*
*&      Form  CREATE_HISTORY_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_history_grid .


* create TOP-Document
  CREATE OBJECT alv_top
    EXPORTING
      style = 'ALVHIS_GRID'.

* create BOTTOM-Document
  CREATE OBJECT alv_bottom
    EXPORTING
      style = 'ALVHIS_GRID'.

  CREATE OBJECT alvhis_container
    EXPORTING
      container_name              = 'ALV_CONTHIS'
      lifetime                    = cntl_lifetime_dynpro
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  CREATE OBJECT alvhis_grid
    EXPORTING
      i_parent          = alvhis_container
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

* create event receiver
  CREATE OBJECT event_receiver.

* set configuration of alv
  varianthis-handle   = 'ALV5'.
  varianthis-report   = sy-repid.
  varianthis-username = sy-uname.
  varianthis-variant  = p_vari.

  layouthis-cwidth_opt   = 'X'.
  layouthis-grid_title   = 'APay Center - Document History'.
  layouthis-smalltitle   = 'X'.
  layouthis-sel_mode     = 'B'.
  layouthis-sgl_clk_hd   = ' '.
  layouthis-totals_bef   = 'X'.
  layouthis-zebra        = 'X'.

* setup the field catalog
  PERFORM set_field_catalog_his.

* display the grid
  CALL METHOD alvhis_grid->set_table_for_first_display
    EXPORTING
      is_layout       = layouthis
      is_variant      = varianthis
      i_save          = 'A'
*     i_default       = ' '
    CHANGING
      it_outtab       = t_apay_his[]
      it_fieldcatalog = t_fieldcat.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " CREATE_HISTORY_GRID
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CATALOG_HIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_catalog_his .

  DATA ls_fcat TYPE lvc_s_fcat.

  CLEAR: t_fieldcat[],
         ls_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = '/NRK/APAY_HIS'
    CHANGING
      ct_fieldcat            = t_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM.                    " SET_FIELD_CATALOG_HIS
*&---------------------------------------------------------------------*
*&      Form  POST_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_document .

* Check for park or post.
  IF wa_dis-belnr IS INITIAL
    AND wa_dis-bukrs IS INITIAL
    AND wa_dis-gjahr IS INITIAL. " park document

    CALL TRANSACTION 'FB60'.

    GET PARAMETER ID 'BUK' FIELD wa_dis-bukrs.
    GET PARAMETER ID 'GJR' FIELD wa_dis-gjahr.
    GET PARAMETER ID 'BLP' FIELD wa_dis-belnr.

    IF wa_dis-belnr IS INITIAL.
      GET PARAMETER ID 'BLN' FIELD wa_dis-belnr.
    ENDIF.

    COMMIT WORK.

    CALL FUNCTION '/NRK/UPDATEAPAYWITHBKPF'
      EXPORTING
        apayno                     = wa_dis-apayno
        bukrs                      = wa_dis-bukrs
        belnr                      = wa_dis-belnr
        gjahr                      = wa_dis-gjahr
*       STATUS                     =
        status_post                = '1900'
        status_park                = '1800'
        ar_object                  = wa_dis-ar_object
        uname                      = sy-uname
      EXCEPTIONS
        no_apay_record_found       = 1
        bkpf_error                 = 2
        OTHERS                     = 3.

    IF sy-subrc <> 0.

    ENDIF.


  ELSE. " post document

  ENDIF.

ENDFORM.                    " POST_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  CREATE_LINEITEM_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_lineitem_grid .

* create TOP-Document
  CREATE OBJECT alv_top
    EXPORTING
      style = 'ALVITEM_GRID'.

* create BOTTOM-Document
  CREATE OBJECT alv_bottom
    EXPORTING
      style = 'ALVITEM_GRID'.

  CREATE OBJECT alvitem_container
    EXPORTING
      container_name              = 'ALVITEM_CONT'
      lifetime                    = cntl_lifetime_dynpro
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  CREATE OBJECT alvitem_grid
    EXPORTING
      i_parent          = alvitem_container
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

* create event receiver
  CREATE OBJECT event_receiver.

* set configuration of alv
  variantitem-handle   = 'ALV6'.
  variantitem-report   = sy-repid.
  variantitem-username = sy-uname.
  variantitem-variant  = p_vari.

  layoutitem-cwidth_opt   = 'X'.
  layoutitem-grid_title   = 'APay Center - Line Items'.
  layoutitem-smalltitle   = 'X'.
  layoutitem-sel_mode     = 'B'.
  layoutitem-sgl_clk_hd   = ' '.
  layoutitem-totals_bef   = 'X'.
  layoutitem-zebra        = 'X'.

* setup the field catalog
  PERFORM set_field_catalog_item.

* display the grid
  CALL METHOD alvitem_grid->set_table_for_first_display
    EXPORTING
      is_layout       = layoutitem
      is_variant      = variantitem
      i_save          = 'A'
*     i_default       = ' '
    CHANGING
      it_outtab       = t_apayitem[]
      it_fieldcatalog = t_fieldcat.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " CREATE_LINEITEM_GRID
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CATALOG_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_catalog_item .

  DATA ls_fcat TYPE lvc_s_fcat.

  CLEAR: t_fieldcat[],
         ls_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = '/NRK/APAYITEMS'
    CHANGING
      ct_fieldcat            = t_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM.                    " SET_FIELD_CATALOG_ITEM
*&---------------------------------------------------------------------*
*&      Form  GET_LINE_ITEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_line_items .

  SELECT * FROM /nrk/apayitems INTO TABLE t_apayitem
    WHERE apayno = wa_dis-apayno.

ENDFORM.                    " GET_LINE_ITEMS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DIS  text
*----------------------------------------------------------------------*
FORM display_vendor  USING    p_wa_dis TYPE /nrk/apayhd_dis.

  CALL FUNCTION 'BAPI_VENDOR_DISPLAY'
    EXPORTING
      vendorno = p_wa_dis-lifnr.

ENDFORM.                    " DISPLAY_VENDOR
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_FI_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DIS  text
*----------------------------------------------------------------------*
FORM display_fi_document  USING    p_wa_dis TYPE /nrk/apayhd_dis.

  SET PARAMETER ID 'BUK' FIELD p_wa_dis-bukrs.
  SET PARAMETER ID 'BLN' FIELD p_wa_dis-belnr.
  SET PARAMETER ID 'GJR' FIELD p_wa_dis-gjahr.
  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

ENDFORM.                    " DISPLAY_FI_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PURCHASE_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_purchase_order USING    p_wa_dis TYPE /nrk/apayhd_dis.

  DATA: lv_tac   LIKE sy-tcode,
        ls_ekko TYPE ekko.

  CLEAR: ls_ekko, lv_tac.

  SELECT SINGLE * FROM ekko INTO ls_ekko WHERE ebeln EQ p_wa_dis-ebeln.

  IF ls_ekko-bstyp EQ 'L'.
    MOVE 'ME33L' TO lv_tac.
    SET PARAMETER ID 'SAG' FIELD wa_dis-ebeln.
  ELSEIF ls_ekko-bstyp EQ 'K'.
    MOVE 'ME33K' TO lv_tac.
    SET PARAMETER ID 'CTR' FIELD wa_dis-ebeln.
  ELSE.
    MOVE 'ME23N' TO lv_tac.
    SET PARAMETER ID 'BES' FIELD wa_dis-ebeln.
  ENDIF.

  CALL TRANSACTION lv_tac AND SKIP FIRST SCREEN.

ENDFORM.                    " DISPLAY_PURCHASE_ORDER
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MM_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DIS  text
*----------------------------------------------------------------------*
FORM display_mm_document  USING    p_wa_dis TYPE /nrk/apayhd_dis.

  SET PARAMETER ID 'RBN' FIELD p_wa_dis-liv_belnr.
  SET PARAMETER ID 'GJR' FIELD p_wa_dis-gjahr.
* SET PARAMETER ID 'CHG' FIELD ' '.
  CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

ENDFORM.                    " DISPLAY_MM_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_CLEARING_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DIS  text
*----------------------------------------------------------------------*
FORM display_clearing_document  USING    p_wa_dis TYPE /nrk/apayhd_dis.

  SET PARAMETER ID 'BUK' FIELD p_wa_dis-bukrs.
  SET PARAMETER ID 'BLN' FIELD p_wa_dis-augbl.
  SET PARAMETER ID 'GJR' FIELD p_wa_dis-gjahr.
  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

ENDFORM.                    " DISPLAY_CLEARING_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_IMAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DIS  text
*----------------------------------------------------------------------*
FORM display_image  USING    p_wa_dis TYPE /nrk/apayhd_dis.

* Local data declaration
  CONSTANTS: c_recor TYPE c               LENGTH 11  VALUE 'APay Record',
             c_objtp TYPE toav0-sap_object           VALUE '/NRK/APAY'.

  DATA:      lv_recno         TYPE                   toav0-object_id,
             lv_objkey        TYPE                   swotobjid-objkey,
             lv_count         TYPE                   sy-index,
             lv_title         TYPE                   sapb-sapwintitl,
             lv_doctp         TYPE                   toaom-doc_type,
             lt_connections   TYPE STANDARD TABLE OF toav0,
             ls_connections   TYPE                   toav0,
             lv_object        TYPE  sibflporb.

* Get all linked documents
  lv_recno = p_wa_dis-apayno.

  CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
    EXPORTING
      objecttype    = c_objtp
      object_id     = lv_recno
      client        = sy-mandt
    IMPORTING
      count         = lv_count
    TABLES
      connections   = lt_connections
    EXCEPTIONS
      nothing_found = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH text-950 lv_recno space space.
  ENDIF.

* Display document(s)
  IF lv_count = 1.
* Display single document
    CONCATENATE c_recor lv_recno INTO lv_title SEPARATED BY space.
    READ TABLE lt_connections INTO ls_connections INDEX 1.
    lv_doctp = ls_connections-reserve.

    CALL FUNCTION 'ARCHIVOBJECT_DISPLAY'
      EXPORTING
        archiv_doc_id            = ls_connections-arc_doc_id
        archiv_id                = ls_connections-archiv_id
        ar_object                = ls_connections-ar_object
        window_title             = lv_title
        doc_type                 = lv_doctp
      EXCEPTIONS
        error_archiv             = 1
        error_communicationtable = 2
        error_kernel             = 3
        OTHERS                   = 4.

  ELSEIF lv_count > 1.
* Display document hit list
*    lv_objkey = lv_recno.
*
*    call function 'ARCHIV_DISPLAY_CONNECTION'
*      exporting
*        objecttype = c_objtp
*        objectid   = lv_objkey
*      exceptions
*        not_found  = 1
*        others     = 2.
*
*    if sy-subrc <> 0.
*      message s398(00) with text-951 lv_recno text-952 space.
*    endif.
*  endif.

    lv_object-instid = lv_recno.
    lv_object-typeid = '/NRK/APAY'.
    lv_object-catid  = 'BO'.

    CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
      EXPORTING
        is_object      = lv_object
        ip_check_arl   = 'X'
        ip_check_bds   = 'X'
        ip_notes       = 'X'
        ip_attachments = 'X'
        ip_urls        = 'X'
        ip_mode        = 'D'.
*    IMPORTING
*      EP_SAVE_REQUEST       =
*    TABLES
*      IT_OBJECTS            =

  ENDIF.


ENDFORM.                    " DISPLAY_IMAGE
*&---------------------------------------------------------------------*
*&      Form  BUILD_FCAT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0355   text
*      -->P_0356   text
*      -->P_0357   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_0360   text
*      -->P_TEXT_140  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM build_fcat_table  USING    p_fnam
                                p_ref_field
                                p_ref_tab
                                p_hotspot
                                p_key
                                p_icon
                                p_text
                                p_out_len
                                p_out.

  DATA: ls_fcat TYPE lvc_s_fcat.

  ls_fcat-fieldname = p_fnam.
  ls_fcat-ref_field = p_ref_field.
  ls_fcat-ref_table = p_ref_tab.
  ls_fcat-hotspot   = p_hotspot.
  ls_fcat-key       = p_key.
  ls_fcat-icon      = p_icon.
  ls_fcat-coltext   = p_text.
  ls_fcat-outputlen = p_out_len.
  ls_fcat-no_out    = p_out.
  APPEND ls_fcat TO t_fieldcat.


ENDFORM.                    " BUILD_FCAT_TABLE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_COMMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DIS  text
*----------------------------------------------------------------------*
FORM display_comment  USING    p_wa_dis TYPE /nrk/apayhd_dis.


  CALL FUNCTION '/NRK/APAY_DISPLAY_COMMENT'
    EXPORTING
      iv_apayno        = p_wa_dis-apayno
    EXCEPTIONS
      entry_not_found  = 1
      db_insert_failed = 2
      OTHERS           = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_COMMENT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_HISTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DIS  text
*----------------------------------------------------------------------*
FORM display_history  USING    p_wa_dis TYPE /nrk/apayhd_dis.

  CALL FUNCTION '/NRK/APAY_DISPLAY_HISTORY'
    EXPORTING
      apayno           = p_wa_dis-apayno
    EXCEPTIONS
      no_history_found = 1
      OTHERS           = 2.

  IF sy-subrc NE 0.
    MESSAGE w398(00) WITH text-980 space space space.
  ENDIF.

ENDFORM.                    " DISPLAY_HISTORY
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_IMAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DIS  text
*----------------------------------------------------------------------*
FORM download_images  TABLES    p_wt_dis STRUCTURE /nrk/apayhd_dis.

* Local data
  DATA: lt_connections TYPE toav0 OCCURS 1,
        ls_connections TYPE toav0,
        lt_connections_proc TYPE toav0 OCCURS 1,
        lv_object_id   TYPE saeobjid,
        lv_objecttype  TYPE saeanwdid VALUE '/NRK/APAY',
        lv_doc_type    TYPE saedoktyp,
        lv_win_title   TYPE string,
        lv_pathname    TYPE string,
        lv_fullpath    TYPE string,
        lv_bin_length  LIKE sapb-length,
        lt_litab_bin   LIKE tbl1024 OCCURS 5000,
        lv_num(3)      TYPE n,
        lv_gui_length  TYPE i,
        p_wa_dis       TYPE /nrk/apayhd_dis.

* Clean up
  CLEAR: ls_connections, lv_object_id, lv_objecttype, lv_doc_type, lv_win_title, lv_pathname, lv_fullpath, lv_bin_length, lv_num, lv_gui_length, p_wa_dis.
  CLEAR: lt_connections, lt_litab_bin, lt_connections_proc.

* Get path for download
  MOVE text-963 TO lv_win_title.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = lv_win_title
    CHANGING
      selected_folder      = lv_pathname
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF lv_pathname LE space.
    MESSAGE w398(00) WITH text-964 space space space.
    EXIT.
  ENDIF.

* Loop at selected records table
  LOOP AT p_wt_dis INTO p_wa_dis.

* Set format
    lv_object_id = p_wa_dis-apayno.

* Get images for record
    CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
      EXPORTING
        objecttype    = lv_objecttype
        object_id     = lv_object_id
        client        = sy-mandt
      TABLES
        connections   = lt_connections
      EXCEPTIONS
        nothing_found = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      MESSAGE s398(00) WITH text-961 space space space.
      EXIT.
    ENDIF.

* Check access authorizations
    LOOP AT lt_connections INTO ls_connections.
* Check authorization
      AUTHORITY-CHECK OBJECT 'S_WFAR_OBJ'
                ID 'OAARCHIV'   FIELD ls_connections-archiv_id
                ID 'OAOBJEKTE'  FIELD ls_connections-sap_object
                ID 'OADOKUMENT' FIELD ls_connections-ar_object
                ID 'ACTVT'      FIELD '03'.
      IF sy-subrc NE 0.
        DELETE lt_connections INDEX sy-tabix.
      ELSE.
* Append to processing table
        APPEND ls_connections TO lt_connections_proc.
      ENDIF.
    ENDLOOP.

* Refresh work area
    CLEAR ls_connections.

  ENDLOOP.

* Make sure there are images to display
  READ TABLE lt_connections_proc INTO ls_connections INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE w398(00) WITH text-962 space space space.
    EXIT.
  ENDIF.

* Download
  LOOP AT lt_connections_proc INTO ls_connections.

    CLEAR: lv_bin_length, lt_litab_bin[], lt_litab_bin.
    lv_doc_type = ls_connections-reserve.

    CALL FUNCTION 'ARCHIVOBJECT_GET_TABLE'
      EXPORTING
        archiv_id                = ls_connections-archiv_id
        document_type            = lv_doc_type
        archiv_doc_id            = ls_connections-arc_doc_id
      IMPORTING
        binlength                = lv_bin_length
      TABLES
        binarchivobject          = lt_litab_bin
      EXCEPTIONS
        error_archiv             = 1
        error_communicationtable = 2
        error_kernel             = 3
        OTHERS                   = 4.

    IF sy-subrc <> 0.
      MESSAGE w398(00) WITH text-965 ls_connections-archiv_id ls_connections-arc_doc_id space.
    ENDIF.

* Download file to specified path
    CONCATENATE lv_pathname '\' ls_connections-object_id  '_' ls_connections-ar_object '_' lv_num '.' ls_connections-reserve INTO lv_fullpath.
    lv_gui_length = lv_bin_length.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize            = lv_gui_length
        filename                = lv_fullpath
        filetype                = 'BIN'
      TABLES
        data_tab                = lt_litab_bin
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        OTHERS                  = 5.

    IF sy-subrc NE 0.
      MESSAGE w398(00) WITH text-966 lv_fullpath space space.
    ENDIF.

    lv_num = lv_num + 1.

  ENDLOOP.

* Send success message
  MESSAGE s398(00) WITH lv_num text-967 space space.

* Clean up
  CLEAR: ls_connections, lv_object_id, lv_objecttype, lv_doc_type, lv_win_title, lv_pathname, lv_fullpath, lv_bin_length, lv_num, lv_gui_length, p_wa_dis.
  CLEAR: lt_connections, lt_litab_bin, lt_connections_proc.

ENDFORM.                    " DOWNLOAD_IMAGES
*&---------------------------------------------------------------------*
*&      Form  PRINT_IMAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DIS  text
*----------------------------------------------------------------------*
FORM print_images  TABLES    p_wt_dis STRUCTURE /nrk/apayhd_dis.

* Local data
  DATA: lt_connections TYPE toav0 OCCURS 1,
        ls_connections TYPE toav0,
        lt_connections_proc TYPE toav0 OCCURS 1,
        lv_object_id   TYPE saeobjid,
        lv_objecttype  TYPE saeanwdid VALUE '/NRK/APAY',
        lv_doc_type    TYPE saedoktyp,
        lv_win_title   TYPE string,
        lv_pathname    TYPE string,
        lv_fullpath    TYPE string,
        lv_bin_length  LIKE sapb-length,
        lt_litab_bin   LIKE tbl1024 OCCURS 5000,
        lv_num(3)      TYPE n,
        lv_gui_length  TYPE i,
        lv_print_pdf   TYPE string,
        lv_rc          TYPE i,
        p_wa_dis       TYPE /nrk/apayhd_dis.

* Clean up
  CLEAR: ls_connections, lv_object_id, lv_doc_type, lv_win_title, lv_pathname, lv_fullpath, lv_bin_length, lv_num, lv_gui_length, lv_print_pdf, lv_rc, p_wa_dis.
  CLEAR: lt_connections, lt_litab_bin, lt_connections_proc.

* Get SAP workdir path for download
  CALL METHOD cl_gui_frontend_services=>get_sapgui_workdir
    CHANGING
      sapworkdir            = lv_pathname
    EXCEPTIONS
      get_sapworkdir_failed = 1
      cntl_error            = 2
      error_no_gui          = 3
      not_supported_by_gui  = 4
      OTHERS                = 5.

  IF sy-subrc NE 0.
* No SAP work dir found, select path manually
    MOVE text-963 TO lv_win_title.
    CALL METHOD cl_gui_frontend_services=>directory_browse
      EXPORTING
        window_title         = lv_win_title
      CHANGING
        selected_folder      = lv_pathname
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

    IF lv_pathname LE space.
      MESSAGE w398(00) WITH text-964 space space space.
      EXIT.
    ENDIF.

  ENDIF.

  LOOP AT p_wt_dis INTO p_wa_dis.

* Set format
    lv_object_id = p_wa_dis-apayno.

* Get images for record
    CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
      EXPORTING
        objecttype    = lv_objecttype
        object_id     = lv_object_id
        client        = sy-mandt
      TABLES
        connections   = lt_connections
      EXCEPTIONS
        nothing_found = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      MESSAGE s398(00) WITH text-961 space space space.
      EXIT.
    ENDIF.

* Check access authorizations
    LOOP AT lt_connections INTO ls_connections.
* Check authorization
      AUTHORITY-CHECK OBJECT 'S_WFAR_OBJ'
                ID 'OAARCHIV'   FIELD ls_connections-archiv_id
                ID 'OAOBJEKTE'  FIELD ls_connections-sap_object
                ID 'OADOKUMENT' FIELD ls_connections-ar_object
                ID 'ACTVT'      FIELD '03'.
      IF sy-subrc NE 0.
* No authorizations
        DELETE lt_connections INDEX sy-tabix.
      ELSEIF ls_connections-reserve NE 'PDF' AND ls_connections-reserve NE 'TIF' AND ls_connections-reserve NE 'FAX'.
* Process only TIF and PDF images
        DELETE lt_connections INDEX sy-tabix.
      ELSE.
* Append to processing table
        APPEND ls_connections TO lt_connections_proc.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

* Make sure there are images to display
  READ TABLE lt_connections_proc INTO ls_connections INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE w398(00) WITH text-962 space space space.
    EXIT.
  ENDIF.

* Refresh work area
  CLEAR ls_connections.

* Download files to local directory
  LOOP AT lt_connections_proc INTO ls_connections.

    CLEAR: lv_bin_length, lt_litab_bin[], lt_litab_bin.
    lv_doc_type = ls_connections-reserve.

    CALL FUNCTION 'ARCHIVOBJECT_GET_TABLE'
      EXPORTING
        archiv_id                = ls_connections-archiv_id
        document_type            = lv_doc_type
        archiv_doc_id            = ls_connections-arc_doc_id
      IMPORTING
        binlength                = lv_bin_length
      TABLES
        binarchivobject          = lt_litab_bin
      EXCEPTIONS
        error_archiv             = 1
        error_communicationtable = 2
        error_kernel             = 3
        OTHERS                   = 4.

    IF sy-subrc <> 0.
      MESSAGE w398(00) WITH text-965 ls_connections-archiv_id ls_connections-arc_doc_id space.
    ENDIF.

* Download file to specified path
    IF ls_connections-reserve CS 'FAX'.
      CONCATENATE lv_pathname '\' ls_connections-object_id  '_' ls_connections-ar_object '_' lv_num '.tif' INTO lv_fullpath.
    ELSE.
      CONCATENATE lv_pathname '\' ls_connections-object_id  '_' ls_connections-ar_object '_' lv_num '.' ls_connections-reserve INTO lv_fullpath.
    ENDIF.
    lv_gui_length = lv_bin_length.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize            = lv_gui_length
        filename                = lv_fullpath
        filetype                = 'BIN'
      TABLES
        data_tab                = lt_litab_bin
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        OTHERS                  = 5.

    IF sy-subrc NE 0.
      MESSAGE w398(00) WITH text-966 lv_fullpath space space.
    ENDIF.

*-----------------------------------
* Print image from specified path
*-----------------------------------
    IF ls_connections-reserve EQ 'PDF'.
* Print PDF
      CONCATENATE '/p/h' lv_fullpath INTO lv_print_pdf SEPARATED BY space.
      CALL METHOD cl_gui_frontend_services=>execute
        EXPORTING
          application            = 'acrord32.exe'
          parameter              = lv_print_pdf
          minimized              = 'X'
          operation              = ''
        EXCEPTIONS
          cntl_error             = 1
          error_no_gui           = 2
          bad_parameter          = 3
          file_not_found         = 4
          path_not_found         = 5
          file_extension_unknown = 6
          error_execute_failed   = 7
          synchronous_failed     = 8
          not_supported_by_gui   = 9
          OTHERS                 = 10.
    ELSE.
* Print TIF
      CALL METHOD cl_gui_frontend_services=>execute
        EXPORTING
          document               = lv_fullpath
          minimized              = 'X'
          synchronous            = 'X'
          operation              = 'PRINT'
        EXCEPTIONS
          cntl_error             = 1
          error_no_gui           = 2
          bad_parameter          = 3
          file_not_found         = 4
          path_not_found         = 5
          file_extension_unknown = 6
          error_execute_failed   = 7
          synchronous_failed     = 8
          not_supported_by_gui   = 9
          OTHERS                 = 10.
    ENDIF.

    IF sy-subrc NE 0.
      MESSAGE s398(00) WITH text-968 lv_fullpath space space.
    ELSE.
* Increase counter
      lv_num = lv_num + 1.
* Wait to unlock
      WAIT UP TO 6 SECONDS.

* Delete file from frontend
      CALL METHOD cl_gui_frontend_services=>file_delete
        EXPORTING
          filename             = lv_fullpath
        CHANGING
          rc                   = lv_rc
        EXCEPTIONS
          file_delete_failed   = 1
          cntl_error           = 2
          error_no_gui         = 3
          file_not_found       = 4
          access_denied        = 5
          unknown_error        = 6
          not_supported_by_gui = 7
          wrong_parameter      = 8
          OTHERS               = 9.
      IF sy-subrc NE 0.
        MESSAGE s398(00) WITH text-969 lv_fullpath text-970 space.
      ENDIF.
    ENDIF.

  ENDLOOP.

* Send success message
  MESSAGE s398(00) WITH lv_num text-971 space space.

* Clean up
  CLEAR: ls_connections, lv_object_id, lv_doc_type, lv_win_title, lv_pathname, lv_fullpath, lv_bin_length, lv_num, lv_gui_length, lv_print_pdf, lv_rc, p_wa_dis.
  CLEAR: lt_connections, lt_litab_bin, lt_connections_proc.


ENDFORM.                    " PRINT_IMAGES
*&---------------------------------------------------------------------*
*&      Form  GET_LINES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_lines .

  DATA: lt_index_rows TYPE lvc_t_row,
        ls_index_rows TYPE lvc_s_row.

  CLEAR: lt_index_rows, ls_index_rows, wa_dis, wt_dis_seltab.

* Get selected rows
  CALL METHOD alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_index_rows.

  IF lt_index_rows IS INITIAL.
    MESSAGE s499(sy) WITH text-953 space space space DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* Read data from display table for selected rows
  LOOP AT lt_index_rows INTO ls_index_rows.
    READ TABLE t_apayhd_dis INDEX ls_index_rows-index INTO wa_dis.
    IF sy-subrc EQ 0.
* Move to selected records table
      APPEND wa_dis TO wt_dis_seltab.
    ENDIF.
  ENDLOOP.

  READ TABLE wt_dis_seltab INTO wa_dis INDEX 1.
* Check if anything was selected
  IF sy-subrc NE 0.
    MESSAGE s398(00) WITH text-953 space space space DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.                    " GET_LINES
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_WORKFLOWLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DIS  text
*----------------------------------------------------------------------*
FORM display_workflowlog  USING    p_wa_dis_workflowid.

* Perform authority check
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD 'SWI1'.
  IF sy-subrc NE 0.
    MESSAGE e172(pg) WITH text-982.
  ENDIF.

* Display workflow step log
  CALL FUNCTION 'SWL_WI_DISPLAY'
    EXPORTING
      wi_id       = p_wa_dis_workflowid
    EXCEPTIONS
      read_failed = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    MESSAGE s499(sy) WITH text-983 text-984 space space DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " DISPLAY_WORKFLOWLOG
*&---------------------------------------------------------------------*
*&      Form  GET_HELP_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_HSTAT_HIGH  text
*----------------------------------------------------------------------*
FORM get_help_status  USING    p_stat p_field.

  DATA:  lv_index     TYPE sy-tabix,
         lv_fieldname LIKE dynpread-fieldname,
         lv_repid     LIKE sy-repid,
         lv_dynnr     LIKE sy-dynnr.

  DATA: BEGIN OF lt_t OCCURS 0,
          stats  LIKE /nrk/apaysdef-status,
          stdes  LIKE /nrk/apaysdef-sdescr,
        END OF lt_t.

  lv_repid = sy-repid.
  lv_dynnr = sy-dynnr.
  lv_fieldname = p_field.

* Get status values from table
  SELECT status sdescr FROM /nrk/apaysdef INTO TABLE lt_t WHERE langu = sy-langu.

  IF sy-subrc <> 0.
    SELECT status sdescr FROM /nrk/apaysdef INTO TABLE lt_t WHERE langu = 'EN'.
  ENDIF.

  SORT lt_t BY stats.

* Show search help
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'STATS'
      dynpprog        = lv_repid
      dynpnr          = lv_dynnr
      dynprofield     = lv_fieldname
      value_org       = 'S'
    TABLES
      value_tab       = lt_t
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " GET_HELP_STATUS
*&---------------------------------------------------------------------*
*&      Form  CHANGE_APAYRECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DIS_APAYNO  text
*----------------------------------------------------------------------*
FORM change_apayrecord  USING    p_wa_dis_apayno.

  CALL FUNCTION '/NRK/APAYCHANGERECORD'
    EXPORTING
      apayno               = p_wa_dis_apayno
    EXCEPTIONS
      no_apay_record_found = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CHANGE_APAYRECORD
*&---------------------------------------------------------------------*
*&      Form  GET_HELP_AR_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_AROBJ_LOW  text
*      -->P_0175   text
*----------------------------------------------------------------------*
FORM get_help_ar_object  USING    p_s_arobj_low
                                  value(p_field).

  DATA:  lv_index     TYPE sy-tabix,
         lv_fieldname LIKE dynpread-fieldname,
         lv_repid     LIKE sy-repid,
         lv_dynnr     LIKE sy-dynnr.

  DATA: BEGIN OF lt_t OCCURS 0,
          ar_object LIKE /nrk/apaydtype-ar_object,
          objecttext LIKE toasp-objecttext,
        END OF lt_t.

  lv_repid = sy-repid.
  lv_dynnr = sy-dynnr.
  lv_fieldname = p_field.

* Get status values from table
  SELECT ar_object FROM /nrk/apaydtype INTO TABLE lt_t.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  LOOP AT lt_t.

    SELECT SINGLE objecttext FROM toasp INTO lt_t-objecttext
      WHERE ar_object EQ lt_t-ar_object
        AND language  EQ sy-langu.

    MODIFY lt_t INDEX sy-tabix.

  ENDLOOP.

  SORT lt_t BY objecttext.

* Show search help
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'AR_OBJECT'
      dynpprog        = lv_repid
      dynpnr          = lv_dynnr
      dynprofield     = lv_fieldname
      value_org       = 'S'
    TABLES
      value_tab       = lt_t
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " GET_HELP_AR_OBJECT
