*----------------------------------------------------------------------*
***INCLUDE /NRK/LAPAYTACSPBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  UPDATE_DATA_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_data_0100 OUTPUT.

* Get document types
  CLEAR: doctype_list,
         doctype_value.
  REFRESH: doctype_list.

*  SELECT * FROM /nrk/apaydtype INTO wa_dtype.
*    CLEAR: wa_toasp.
*    SELECT SINGLE * FROM toasp INTO wa_toasp
*      WHERE ar_object = wa_dtype-ar_object
*        AND language = sy-langu.
*    IF sy-subrc EQ 0.
*      doctype_value-key = wa_dtype-ar_object.
*      MOVE wa_toasp-objecttext TO doctype_value-text.
*      APPEND doctype_value TO doctype_list.
*    ENDIF.
*  ENDSELECT.
  IF bukrs IS INITIAL.
    CLEAR: wa_toasp.
    SELECT SINGLE * FROM toasp INTO wa_toasp
      WHERE ar_object = wa_dtype-ar_object
        AND language = sy-langu.
    IF sy-subrc EQ 0.
      doctype_value-key = wa_dtype-ar_object.
      MOVE wa_toasp-objecttext TO doctype_value-text.
      APPEND doctype_value TO doctype_list.
    ENDIF.
  ELSE.
    SELECT * FROM /nrk/apaydtype INTO wa_dtype.
      IF NOT wa_dtype-bukrs IS INITIAL.
        IF wa_dtype-bukrs EQ bukrs.
          CLEAR: wa_toasp.
          SELECT SINGLE * FROM toasp INTO wa_toasp
            WHERE ar_object = wa_dtype-ar_object
              AND language = sy-langu.
          IF sy-subrc EQ 0.
            doctype_value-key = wa_dtype-ar_object.
            MOVE wa_toasp-objecttext TO doctype_value-text.
            APPEND doctype_value TO doctype_list.
          ENDIF.
        ENDIF.
      ELSE. " doctype bukrs is initial
        CLEAR: wa_toasp.
        SELECT SINGLE * FROM toasp INTO wa_toasp
          WHERE ar_object = wa_dtype-ar_object
            AND language = sy-langu.
        IF sy-subrc EQ 0.
          doctype_value-key = wa_dtype-ar_object.
          MOVE wa_toasp-objecttext TO doctype_value-text.
          APPEND doctype_value TO doctype_list.
        ENDIF.
      ENDIF.
    ENDSELECT.
  ENDIF.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'DOCTYPE_DESC'
      values          = doctype_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

* Fill document type header with wa_hd-ar_object
  IF doctype_desc IS INITIAL.
    doctype_desc = wa_hd-ar_object.
  ENDIF.

* Get approver list for Non-PO invoices
  IF NOT bukrs IS INITIAL.
    CLEAR: extuser_list,
           extuser_value.
    REFRESH: extuser_list.

    SELECT * FROM /nrk/apayuser
      INTO CORRESPONDING FIELDS OF TABLE t_approver
        WHERE approver EQ 'X'
          AND bukrs EQ bukrs.

    IF sy-subrc EQ 0.
      LOOP AT t_approver INTO wa_approver.
        MOVE wa_approver-extuser TO extuser_value-key.
        MOVE wa_approver-fullname TO extuser_value-text.
        APPEND extuser_value TO extuser_list.
      ENDLOOP.
    ELSE. " no approver by company code
      SELECT * FROM /nrk/apayuser
        INTO CORRESPONDING FIELDS OF TABLE t_approver
          WHERE approver EQ 'X'.
      LOOP AT t_approver INTO wa_approver.
        MOVE wa_approver-extuser TO extuser_value-key.
        MOVE wa_approver-fullname TO extuser_value-text.
        APPEND extuser_value TO extuser_list.
      ENDLOOP.
    ENDIF.
  ENDIF.

* Get coder/approver
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'FULLNAME'
      values          = extuser_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

* Set credit/debit radio buttons
  IF wa_hd-shkzg EQ 'S'.
    rad_debit = 'X'.
    rad_credit = ''.
  ELSEIF wa_hd-shkzg EQ 'H'.
    rad_debit = ''.
    rad_credit = 'X'.
  ENDIF.

* Get current status description
  SELECT SINGLE sdescr FROM /nrk/apaysdef INTO sdescr
    WHERE status EQ wa_hd-status
      AND langu EQ sy-langu.

* Get purchase order data
  IF NOT wa_hd-ebeln IS INITIAL.
    SELECT SINGLE * FROM ekko INTO wa_ekko
      WHERE ebeln = wa_hd-ebeln.

    IF sy-subrc EQ 0.
      ekgrp = wa_ekko-ekgrp.
      ekorg = wa_ekko-ekorg.
    ENDIF.
  ENDIF.

* Get vendor address
  IF NOT lifnr IS INITIAL
    AND NOT bukrs IS INITIAL.
    CALL FUNCTION 'VENDOR_READ'
      EXPORTING
        i_bukrs   = bukrs
        i_lifnr   = lifnr
      IMPORTING
        e_lfa1    = wa_lfa1
        e_lfb1    = wa_lfb1
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc EQ 0.
      lifname = wa_lfa1-name1.
      street = wa_lfa1-stras.
      city = wa_lfa1-ort01.
      zip = wa_lfa1-pstlz.
      region = wa_lfa1-regio.
    ENDIF.
  ENDIF.

ENDMODULE.                 " UPDATE_DATA_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS '0200'.
  SET TITLEBAR '200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  UPDATE_DATA_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_data_0200 OUTPUT.

* Generate list of document types
  IF NOT bukrs IS INITIAL.
    CLEAR: doctype_list[].
    SELECT * FROM /nrk/apaydtype INTO wa_dtype
      WHERE bukrs EQ bukrs.
*     IF NOT wa_dtype-bukrs IS INITIAL.
*       IF wa_dtype-bukrs EQ bukrs.
      CLEAR: wa_toasp.
      SELECT SINGLE * FROM toasp INTO wa_toasp
        WHERE ar_object = wa_dtype-ar_object
          AND language = sy-langu.
      IF sy-subrc EQ 0.
        doctype_value-key = wa_dtype-ar_object.
        MOVE wa_toasp-objecttext TO doctype_value-text.
        APPEND doctype_value TO doctype_list.
      ENDIF.
    ENDSELECT.

    IF sy-subrc NE 0. " no document types for company code
      CLEAR: wa_toasp.
      SELECT SINGLE * FROM toasp INTO wa_toasp
        WHERE ar_object = wa_dtype-ar_object
          AND language = sy-langu.
      IF sy-subrc EQ 0.
        doctype_value-key = wa_dtype-ar_object.
        MOVE wa_toasp-objecttext TO doctype_value-text.
        APPEND doctype_value TO doctype_list.
      ENDIF.
    ENDIF.
  ENDIF.

* Get document types
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'DOCTYPE_DESC'
      values          = doctype_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

* Generate list of approvers
  IF NOT bukrs IS INITIAL.
    CLEAR: extuser_list,
           extuser_value.
    REFRESH: extuser_list.

    SELECT * FROM /nrk/apayuser
      INTO CORRESPONDING FIELDS OF TABLE t_approver
        WHERE approver EQ 'X'
          AND bukrs EQ bukrs.

    IF sy-subrc EQ 0.
      LOOP AT t_approver INTO wa_approver.
        MOVE wa_approver-extuser TO extuser_value-key.
        MOVE wa_approver-fullname TO extuser_value-text.
        APPEND extuser_value TO extuser_list.
      ENDLOOP.
    ELSE. " no approver by company code
      SELECT * FROM /nrk/apayuser
        INTO CORRESPONDING FIELDS OF TABLE t_approver
          WHERE approver EQ 'X'.
      LOOP AT t_approver INTO wa_approver.
        MOVE wa_approver-extuser TO extuser_value-key.
        MOVE wa_approver-fullname TO extuser_value-text.
        APPEND extuser_value TO extuser_list.
      ENDLOOP.
    ENDIF.
  ENDIF.

* Get coder/approver
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'FULLNAME'
      values          = extuser_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

* Get info from company code
  IF NOT bukrs IS INITIAL.
    SELECT SINGLE * FROM t001 INTO wa_t001
      WHERE bukrs EQ bukrs.

    butxt = wa_t001-butxt.

    IF waers IS INITIAL
      OR waers NE wa_t001-waers.
      waers = wa_t001-waers.
    ENDIF.

  ENDIF.

* Get vendor address
  IF NOT lifnr IS INITIAL
    AND NOT bukrs IS INITIAL.
    CALL FUNCTION 'VENDOR_READ'
      EXPORTING
        i_bukrs   = bukrs
        i_lifnr   = lifnr
      IMPORTING
        e_lfa1    = wa_lfa1
        e_lfb1    = wa_lfb1
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc EQ 0.
      lifname = wa_lfa1-name1.
      street = wa_lfa1-stras.
      city = wa_lfa1-ort01.
      zip = wa_lfa1-pstlz.
      region = wa_lfa1-regio.
    ENDIF.
  ENDIF.

ENDMODULE.                 " UPDATE_DATA_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen_0200 OUTPUT.

  IF po_flag EQ 'X'. " PO invoice

    LOOP AT SCREEN.

      IF screen-name EQ 'EBELN'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name EQ 'LFSNR1'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ELSE. " Non-PO invoice

    LOOP AT SCREEN.

      IF screen-name EQ 'EBELN'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name EQ 'LFSNR1'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_0106  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen_0106 OUTPUT.

  LOOP AT SCREEN.

* approver fields
    IF screen-name EQ 'TXT_APPROVER'.
      IF wf_type EQ 'FI'.
        screen-active = 1.
        MODIFY SCREEN.
      ELSEIF wf_type EQ 'MM'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name EQ 'FULLNAME'.
      IF wf_type EQ 'FI'.
        screen-active = 1.
        MODIFY SCREEN.
      ELSEIF wf_type EQ 'MM'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name EQ 'PREAPR'.
      IF wf_type EQ 'FI'.
        screen-active = 1.
        MODIFY SCREEN.
      ELSEIF wf_type EQ 'MM'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

* PO fields
    IF screen-name EQ 'TXT_EBELN'.
      IF wf_type EQ 'FI'.
        screen-active = 0.
        MODIFY SCREEN.
      ELSEIF wf_type EQ 'MM'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name EQ 'EBELN'.
      IF wf_type EQ 'FI'.
        screen-active = 0.
        MODIFY SCREEN.
      ELSEIF wf_type EQ 'MM'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name EQ 'EKGRP'.
      IF wf_type EQ 'FI'.
        screen-active = 0.
        MODIFY SCREEN.
      ELSEIF wf_type EQ 'MM'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name EQ 'TXT_EKGRP'.
      IF wf_type EQ 'FI'.
        screen-active = 0.
        MODIFY SCREEN.
      ELSEIF wf_type EQ 'MM'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name EQ 'EKORG'.
      IF wf_type EQ 'FI'.
        screen-active = 0.
        MODIFY SCREEN.
      ELSEIF wf_type EQ 'MM'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name EQ 'TXT_EKORG'.
      IF wf_type EQ 'FI'.
        screen-active = 0.
        MODIFY SCREEN.
      ELSEIF wf_type EQ 'MM'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name EQ 'LFSNR1'.
      IF wf_type EQ 'FI'.
        screen-active = 0.
        MODIFY SCREEN.
      ELSEIF wf_type EQ 'MM'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name EQ 'TXT_LFSNR1'.
      IF wf_type EQ 'FI'.
        screen-active = 0.
        MODIFY SCREEN.
      ELSEIF wf_type EQ 'MM'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_0106  OUTPUT
