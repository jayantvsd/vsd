*----------------------------------------------------------------------*
***INCLUDE /NRK/LAPAYTACSFRM .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_data .

  IF rad_debit EQ 'X'.
    wa_hd-shkzg = 'S'.
  ELSEIF rad_credit EQ 'X'.
    wa_hd-shkzg = 'H'.
  ENDIF.

  wa_hd-bukrs = bukrs.
* wa_hd-ar_object = doctype_desc.
  wa_hd-ar_object = ar_object.
  wa_hd-ebeln = ebeln.
  wa_hd-ekgrp = ekgrp.
  wa_hd-bldat = bldat.
  wa_hd-ekorg = ekorg.
  wa_hd-xblnr = xblnr.
  wa_hd-lifnr = lifnr.
  wa_hd-wrbtr_net = wrbtr_net.
  wa_hd-lifname = lifname.
  wa_hd-wmwst = wmwst.
  wa_hd-wrbtr = wrbtr.
  wa_hd-waers = waers.
  IF NOT wi_id IS INITIAL.
    wa_hd-workflowid = wi_id.
  ENDIF.

  TRANSLATE wa_hd-lifname TO UPPER CASE.

  IF NOT fullname IS INITIAL.
    MOVE fullname TO wa_hd-ext_approver.
    wa_hd-approvalreq = 'X'.
  ELSE.
    CLEAR: wa_hd-ext_approver,
           wa_hd-approvalreq.
  ENDIF.

  wa_hd-lfsnr1 = lfsnr1.

  IF wa_hd-ebeln IS NOT INITIAL.

    SELECT SINGLE * FROM ekko INTO wa_ekko
      WHERE ebeln EQ wa_hd-ebeln.

    IF sy-subrc EQ 0.

      wa_hd-bsart = wa_ekko-bsart.
      wa_hd-ekgrp = wa_ekko-ekgrp.
      wa_hd-ekorg = wa_ekko-ekorg.
      wa_hd-bedat = wa_ekko-bedat.
      wa_hd-bstyp = wa_ekko-bstyp.
      wa_hd-zterm = wa_ekko-zterm.
      wa_hd-zbd1t = wa_ekko-zbd1t.
      wa_hd-zbd2t = wa_ekko-zbd2t.
      wa_hd-zbd3t = wa_ekko-zbd3t.
      wa_hd-zbd1p = wa_ekko-zbd1p.
      wa_hd-lifnr = wa_ekko-lifnr.
      wa_hd-bukrs = wa_ekko-bukrs.
      wa_hd-ernam = wa_ekko-ernam.
*     wa_hd-duedate = wa_hd-bldat + wa_ekko-zbd1t.

    ENDIF.

  ENDIF.

  IF NOT wa_hd-lifnr IS INITIAL
    AND NOT wa_hd-bukrs IS INITIAL.

    CALL FUNCTION 'VENDOR_READ'
      EXPORTING
        i_bukrs   = wa_hd-bukrs
        i_lifnr   = wa_hd-lifnr
      IMPORTING
        e_lfa1    = wa_lfa1
        e_lfb1    = wa_lfb1
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc = 0.
      wa_hd-lifname = wa_lfa1-name1.
      wa_hd-zterm   = wa_lfb1-zterm.
      wa_hd-stras   = wa_lfa1-stras.
      wa_hd-pstlz   = wa_lfa1-pstlz.
      wa_hd-ort01   = wa_lfa1-ort01.
      wa_hd-land1   = wa_lfa1-land1.
      wa_hd-zsabe = wa_lfb1-zsabe.
      wa_hd-zahls = wa_lfb1-zahls.
      wa_hd-zterm = wa_lfb1-zterm.
      wa_hd-busab = wa_lfb1-busab.

      TRANSLATE wa_hd-lifname TO UPPER CASE.

    ENDIF.
  ENDIF.

*   ENDIF.
* ENDIF.

  CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
    EXPORTING
      i_bldat         = wa_hd-bldat
      i_budat         = sy-datum
      i_zterm         = wa_hd-zterm
      i_lifnr         = wa_hd-lifnr
      i_bukrs         = wa_hd-bukrs
    IMPORTING
      e_zbd1t         = wa_hd-zbd1t
      e_zbd1p         = wa_hd-zbd1p
      e_zbd2t         = wa_hd-zbd2t
      e_zbd2p         = wa_hd-zbd2p
      e_zbd3t         = wa_hd-zbd3t
      e_zfbdt         = wa_hd-duedate
    EXCEPTIONS
      terms_not_found = 1
      OTHERS          = 2.

* IF sy-subrc EQ 0.
*   wa_hd-duedate = wa_hd-bldat + wa_hd-zbd1t.
* ENDIF.

  MODIFY /nrk/apayhd FROM wa_hd.
  COMMIT WORK.

*** update history


ENDFORM.                    " UPDATE_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data_change .

  CLEAR: wa_hd_orig.
  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd_orig
    WHERE apayno EQ wa_hd-apayno.

* Check for change
  CLEAR: datachanged.
  IF bukrs NE wa_hd_orig-bukrs.
    datachanged = 'X'.
  ENDIF.
  IF bldat NE wa_hd_orig-bldat.
    datachanged = 'X'.
  ENDIF.
  IF xblnr NE wa_hd_orig-xblnr.
    datachanged = 'X'.
  ENDIF.
  IF wrbtr NE wa_hd_orig-wrbtr.
    datachanged = 'X'.
  ENDIF.
  IF waers NE wa_hd_orig-waers.
    datachanged = 'X'.
  ENDIF.
  IF ebeln NE wa_hd_orig-ebeln.
    datachanged = 'X'.
  ENDIF.
  IF lifnr NE wa_hd_orig-lifnr.
    datachanged = 'X'.
  ENDIF.
  MOVE doctype_desc TO ar_object.
  IF ar_object NE wa_hd_orig-ar_object.
    datachanged = 'X'.
  ENDIF.

  IF lfsnr1 NE wa_hd_orig-lfsnr1.
    datachanged = 'X'.
  ENDIF.

ENDFORM.                    " CHECK_DATA_CHANGE
*&---------------------------------------------------------------------*
*&      Form  CHECK_WORKFLOWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_workflows .

* if data changed, check for existing workflows
  IF datachanged EQ 'X'.

* Check existing workflows
    SELECT * FROM swwwihead INTO TABLE t_swwwihead
      WHERE wi_id EQ wa_hd-workflowid.

* Cancel existing workflows
    IF sy-subrc EQ 0.

* confirm with user
*      CALL FUNCTION 'POPUP_TO_INFORM'
*        EXPORTING
*          titel = text-105
*          txt1  = text-106
*          txt2  = space.

      MESSAGE w208(00) WITH text-106.

    ENDIF.

  ENDIF.

ENDFORM.                    " CHECK_WORKFLOWS
*&---------------------------------------------------------------------*
*&      Form  RESTART_WORKFLOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM restart_workflow .

* Check existing workflows
  SELECT * FROM swwwihead INTO TABLE t_swwwihead
    WHERE wi_id EQ wa_hd-workflowid.

* Cancel existing workflows
  IF sy-subrc EQ 0.

* confirm with user
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = text-101
        text_question         = text-102
        text_button_1         = text-103
        text_button_2         = text-104
        default_button        = '1'
        display_cancel_button = 'X'
        start_column          = 25
        start_row             = 6
      IMPORTING
        answer                = answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF sy-subrc <> 0
      OR answer NE '1'.
      EXIT.
    ENDIF.

    CLEAR: cancelled.

    LOOP AT t_swwwihead INTO wa_swwwihead.
      IF wa_swwwihead-wi_stat NE 'COMPLETED'
        AND wa_swwwihead-wi_stat NE 'CANCELLED'.

        CALL FUNCTION 'SAP_WAPI_ADM_WORKFLOW_CANCEL'
          EXPORTING
            workitem_id  = wa_swwwihead-wi_id
            actual_agent = sy-uname
            language     = sy-langu
            do_commit    = 'X'
          IMPORTING
            return_code  = wapi_return.

        cancelled = 'X'.

      ENDIF.
    ENDLOOP.

    IF cancelled EQ 'X'.

      SELECT SINGLE val1 FROM /nrk/apayconfig INTO value1
        WHERE key1 EQ 'APAY'
          AND key2 EQ 'CANCELSTAT'.

      IF sy-subrc EQ 0.
        MOVE value1 TO cancelstatus.
      ENDIF.

* Update APay Center
      CALL FUNCTION '/NRK/APAY_UPDATE_STATUS'
        EXPORTING
          apayno        = wa_hd-apayno
          status        = cancelstatus
          date          = sy-datum
          time          = sy-uzeit
          user          = sy-uname
        EXCEPTIONS
          update_failed = 1
          OTHERS        = 2.
    ENDIF.

  ENDIF.

* start new workflow
  CALL FUNCTION '/NRK/APAY_START_WORKFLOW'
    EXPORTING
      apayno                = wa_hd-apayno
      user                  = sy-uname
      ar_object             = ar_object
    IMPORTING
      wf_id                 = wi_id
    EXCEPTIONS
      workflow_start_failed = 1
      OTHERS                = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " RESTART_WORKFLOW
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_IMAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_HD  text
*----------------------------------------------------------------------*
FORM display_image  USING    p_wa_hd TYPE /nrk/apayhd.

* call function to display image(s)
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
  lv_recno = p_wa_hd-apayno.

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
*&      Form  DISPLAY_HISTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_HD  text
*----------------------------------------------------------------------*
FORM display_history  USING    p_wa_hd.

* call function to display history
  CALL FUNCTION '/NRK/APAY_DISPLAY_HISTORY'
    EXPORTING
      apayno           = wa_hd-apayno
    EXCEPTIONS
      no_history_found = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_HISTORY
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data .

  IF NOT ar_object IS INITIAL.

    SELECT SINGLE * FROM /nrk/apaydtype INTO wa_dtype
      WHERE ar_object EQ ar_object
        AND bukrs EQ bukrs.

    IF sy-subrc NE 0.
      MESSAGE e208(00) WITH text-108.
    ENDIF.

    wf_type = wa_dtype-wf_type.

    IF wf_type EQ 'FI'.
      CLEAR: ebeln,
             ekgrp,
             ekorg,
             lfsnr1.

      SELECT SINGLE * FROM lfa1 INTO wa_lfa1
        WHERE lifnr EQ lifnr.

      IF sy-subrc EQ 0.
        lifname = wa_lfa1-name1.
        street = wa_lfa1-stras.
        city = wa_lfa1-ort01.
        zip = wa_lfa1-pstlz.
        region = wa_lfa1-regio.
      ELSE.
        CLEAR: lifname,
               street,
               city,
               zip,
               region.
      ENDIF.


    ELSEIF wf_type EQ 'MM'.

      CLEAR: fullname,
             preapr.

      IF ebeln IS INITIAL.

        SELECT SINGLE * FROM /nrk/apayconfig INTO wa_config
           WHERE key1 EQ 'APAY'
             AND key2 EQ 'POOPTIONAL'.

        IF sy-subrc EQ 0.
          IF wa_config-val1 EQ 'X'. " PO optional
*         MESSAGE i208(00) WITH text-111.
          ELSE.
            MESSAGE e208(00) WITH text-109.
          ENDIF.
        ELSE.
          MESSAGE e208(00) WITH text-109.
        ENDIF.

      ELSE.
        SELECT SINGLE * FROM ekko INTO wa_ekko
          WHERE ebeln EQ ebeln.

        IF sy-subrc NE 0.
          MESSAGE e208(00) WITH text-110.
        ELSE.
          bukrs = wa_ekko-bukrs.
          lifnr = wa_ekko-lifnr.
          waers = wa_ekko-waers.
          ekorg = wa_ekko-ekorg.
          ekgrp = wa_ekko-ekgrp.

          SELECT SINGLE * FROM lfa1 INTO wa_lfa1
            WHERE lifnr EQ lifnr.

          lifname = wa_lfa1-name1.
          street = wa_lfa1-stras.
          city = wa_lfa1-ort01.
          zip = wa_lfa1-pstlz.
          region = wa_lfa1-regio.

        ENDIF.

      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_document .

* Get Archive ID
  SELECT SINGLE * INTO wa_toaom FROM toaom WHERE sap_object EQ '/NRK/APAY' AND ar_object EQ ar_object.
  IF sy-subrc NE 0.

  ENDIF.

* Upload document
  CALL FUNCTION 'ALINK_DOCUMENTS_CREATE_DIALOG'
    EXPORTING
      archiv_id                     = wa_toaom-archiv_id
      document_class                = wa_toaom-doc_type
      document_type                 = ar_object
    IMPORTING
      archiv_doc_id                 = arc_doc_id
    TABLES
      outdoctab                     = outdoctab
    CHANGING
      infiletab                     = filetab
    EXCEPTIONS
      error_contentrepository       = 1
      error_archivelink_customizing = 2
      canceled_by_user              = 3
      blocked_by_policy             = 4
      OTHERS                        = 5.

  IF sy-subrc <> 0.
    IF  sy-subrc = 3.
      cancelled = 'X'.
    ELSE.
      MESSAGE e398(00) WITH 'Document upload failed.' space space space.
    ENDIF.
  ELSE.
    cancelled = ' '.
  ENDIF.

  CLEAR: ok_code.

ENDFORM.                    " UPLOAD_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  CREATE_APAY_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_apay_record .

  IF rad_debit EQ 'X'.
    wa_hd-shkzg = 'S'.
  ELSEIF rad_credit EQ 'X'.
    wa_hd-shkzg = 'H'.
  ENDIF.

  wa_hd-bukrs = bukrs.
  wa_hd-ar_object = ar_object.
  wa_hd-ebeln = ebeln.
  wa_hd-ekgrp = ekgrp.
  wa_hd-bldat = bldat.
  wa_hd-rdate = rdate.
  wa_hd-ekorg = ekorg.
  wa_hd-xblnr = xblnr.
  wa_hd-lifnr = lifnr.
  wa_hd-wrbtr_net = wrbtr_net.
  wa_hd-lifname = lifname.
  wa_hd-wmwst = wmwst.
  wa_hd-wrbtr = wrbtr.
  wa_hd-waers = waers.

  IF NOT lfsnr1 IS INITIAL.
    wa_hd-lfsnr1 = lfsnr1.
  ENDIF.

  IF NOT wi_id IS INITIAL.
    wa_hd-workflowid = wi_id.
  ENDIF.

  TRANSLATE wa_hd-lifname TO UPPER CASE.

* Add approver
  MOVE fullname TO wa_hd-ext_approver.

* Update PO data
  IF wa_hd-ebeln IS NOT INITIAL.

    SELECT SINGLE * FROM ekko INTO wa_ekko
      WHERE ebeln EQ wa_hd-ebeln.

    IF sy-subrc EQ 0.
      wa_hd-bsart = wa_ekko-bsart.
      wa_hd-ekgrp = wa_ekko-ekgrp.
      wa_hd-ekorg = wa_ekko-ekorg.
      wa_hd-bedat = wa_ekko-bedat.
      wa_hd-bstyp = wa_ekko-bstyp.
      wa_hd-zterm = wa_ekko-zterm.
      wa_hd-zbd1t = wa_ekko-zbd1t.
      wa_hd-zbd2t = wa_ekko-zbd2t.
      wa_hd-zbd3t = wa_ekko-zbd3t.
      wa_hd-zbd1p = wa_ekko-zbd1p.
      wa_hd-lifnr = wa_ekko-lifnr.
      wa_hd-bukrs = wa_ekko-bukrs.
      wa_hd-ernam = wa_ekko-ernam.
    ENDIF.

  ENDIF.

* Update vendor data
  IF NOT wa_hd-lifnr IS INITIAL
    AND NOT wa_hd-bukrs IS INITIAL.

    CALL FUNCTION 'VENDOR_READ'
      EXPORTING
        i_bukrs   = wa_hd-bukrs
        i_lifnr   = wa_hd-lifnr
      IMPORTING
        e_lfa1    = wa_lfa1
        e_lfb1    = wa_lfb1
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc = 0.
      wa_hd-lifname = wa_lfa1-name1.
      wa_hd-stras   = wa_lfa1-stras.
      wa_hd-pstlz   = wa_lfa1-pstlz.
      wa_hd-ort01   = wa_lfa1-ort01.
      wa_hd-land1   = wa_lfa1-land1.
      wa_hd-zsabe = wa_lfb1-zsabe.
      wa_hd-zahls = wa_lfb1-zahls.
      wa_hd-busab = wa_lfb1-busab.

      IF wa_hd-zterm IS INITIAL.
        wa_hd-zterm   = wa_lfb1-zterm.
      ENDIF.

      TRANSLATE wa_hd-lifname TO UPPER CASE.
    ENDIF.
  ENDIF.

* Update Payment terms
  CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
    EXPORTING
      i_bldat         = wa_hd-bldat
      i_budat         = sy-datum
      i_zterm         = wa_hd-zterm
      i_lifnr         = wa_hd-lifnr
      i_bukrs         = wa_hd-bukrs
    IMPORTING
      e_zbd1t         = wa_hd-zbd1t
      e_zbd1p         = wa_hd-zbd1p
      e_zbd2t         = wa_hd-zbd2t
      e_zbd2p         = wa_hd-zbd2p
      e_zbd3t         = wa_hd-zbd3t
      e_zfbdt         = wa_hd-duedate
    EXCEPTIONS
      terms_not_found = 1
      OTHERS          = 2.

* IF sy-subrc EQ 0.
*   wa_hd-duedate = wa_hd-bldat + wa_hd-zbd1t.
* ENDIF.

* MODIFY /nrk/apayhd FROM wa_hd.
* COMMIT WORK.

* Get initial status
  SELECT SINGLE val1 FROM /nrk/apayconfig INTO value1
    WHERE key1 EQ 'APAY'
      AND key2 EQ 'INITSTAT'.

  IF sy-subrc EQ 0.
    MOVE value1 TO initstatus.
  ELSE.
    MESSAGE e398(00) WITH 'No initial status.' space space space.
  ENDIF.

* Get APay record id
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr                   = '01'
      object                        = '/NRK/APAY'
    IMPORTING
      number                        = wa_hd-apayno
*   QUANTITY                      =
*   RETURNCODE                    =
    EXCEPTIONS
     interval_not_found            = 1
     number_range_not_intern       = 2
     object_not_found              = 3
     quantity_is_0                 = 4
     quantity_is_not_1             = 5
     interval_overflow             = 6
     buffer_overflow               = 7
     OTHERS                        = 8.

  IF sy-subrc NE 0.
    MESSAGE e398(00) WITH 'Number generation failed' space space space.
  ENDIF.

  INSERT /nrk/apayhd FROM wa_hd.
  COMMIT WORK.

* Update APay Center
  CALL FUNCTION '/NRK/APAY_UPDATE_STATUS'
    EXPORTING
      apayno        = wa_hd-apayno
      status        = initstatus
      date          = sy-datum
      time          = sy-uzeit
      user          = sy-uname
    EXCEPTIONS
      update_failed = 1
      OTHERS        = 2.

* start new workflow
  CALL FUNCTION '/NRK/APAY_START_WORKFLOW'
    EXPORTING
      apayno                = wa_hd-apayno
      user                  = sy-uname
      ar_object             = ar_object
    IMPORTING
      wf_id                 = wi_id
    EXCEPTIONS
      workflow_start_failed = 1
      OTHERS                = 2.

  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH 'Workflow start failed' space space space.
  ENDIF.

  IF NOT wi_id IS INITIAL.
    UPDATE /nrk/apayhd SET workflowid = wi_id
      WHERE apayno EQ wa_hd-apayno.
  ENDIF.

ENDFORM.                    " CREATE_APAY_RECORD
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data_0200 .

  IF bukrs IS INITIAL.
    MESSAGE e398(00) WITH 'Company code required.' space space space.
  ENDIF.

  IF NOT doctype_desc IS INITIAL.

* Check document type details
    MOVE doctype_desc TO ar_object.

    SELECT SINGLE * FROM /nrk/apaydtype INTO wa_dtype
      WHERE ar_object EQ ar_object.
    IF sy-subrc NE 0.
      MESSAGE e208(00) WITH text-108.
    ENDIF.

    IF wa_dtype-wf_type EQ 'FI'.
      po_flag = ' '.
      CLEAR: ebeln,
             ekgrp,
             ekorg.

* check for approver
      IF fullname IS INITIAL
        AND preapr NE 'X'.
        MESSAGE e398(00) WITH 'Approver required.' space space space.
      ENDIF.

      IF preapr EQ 'X'.
        CLEAR: fullname.
      ENDIF.

    ELSEIF wa_dtype-wf_type EQ 'MM'.
      po_flag = 'X'.
      IF ebeln IS INITIAL.

        SELECT SINGLE * FROM /nrk/apayconfig INTO wa_config
           WHERE key1 EQ 'APAY'
             AND key2 EQ 'POOPTIONAL'.

        IF sy-subrc EQ 0.
          IF wa_config-val1 EQ 'X'. " PO optional
*           MESSAGE i208(00) WITH text-111.
          ELSE.
            MESSAGE e208(00) WITH text-109.
          ENDIF.
        ELSE.
          MESSAGE e208(00) WITH text-109.
        ENDIF.

      ELSE.
* Check PO details
        SELECT SINGLE * FROM ekko INTO wa_ekko
          WHERE ebeln EQ ebeln.
        IF sy-subrc NE 0.
          MESSAGE e208(00) WITH text-110.
        ELSE.
          bukrs = wa_ekko-bukrs.
          lifnr = wa_ekko-lifnr.
          waers = wa_ekko-waers.
          ekorg = wa_ekko-ekorg.
          ekgrp = wa_ekko-ekgrp.

          SELECT SINGLE * FROM lfa1 INTO wa_lfa1
            WHERE lifnr EQ lifnr.

          lifname = wa_lfa1-name1.
          street = wa_lfa1-stras.
          city = wa_lfa1-ort01.
          zip = wa_lfa1-pstlz.
          region = wa_lfa1-regio.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE. " company code required
*   MESSAGE e398(00) WITH 'Company code required.' space space space.
  ENDIF.

* Check amount and tax amount
  wrbtr_total = wrbtr_net + wmwst.
  IF wrbtr_total NE wrbtr. " amount not balanced
    MESSAGE e398(00) WITH 'Amount not balanced.' space space space.
  ENDIF.

ENDFORM.                    " CHECK_DATA_0200
*&---------------------------------------------------------------------*
*&      Form  CREATE_APAY_LINK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_apay_link .

* Insert to link table
  READ TABLE outdoctab INTO wa_outdoctab INDEX 1.

  IF sy-subrc EQ 0.

    MOVE wa_hd-apayno TO object_id.

    CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
      EXPORTING
        archiv_id             = wa_toaom-archiv_id
        arc_doc_id            = arc_doc_id
        ar_object             = ar_object
        object_id             = object_id
        sap_object            = '/NRK/APAY'
        doc_type              = wa_outdoctab-doc_class
      EXCEPTIONS
        error_connectiontable = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      MESSAGE e398(00) WITH 'ArchiveLink error updating connection in database.'(931) space space space.
    ENDIF.
  ENDIF.

  MESSAGE i398(00) WITH 'Document successfully stored.' space space space.

ENDFORM.                    " CREATE_APAY_LINK
*&---------------------------------------------------------------------*
*&      Form  CLEAR_AFTER_INDEX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_after_index .

  CLEAR: arc_doc_id,
         filetab,
         wa_hd,
*        ok_code,
         outdoctab[].

  REFRESH: outdoctab.

ENDFORM.                    " CLEAR_AFTER_INDEX
