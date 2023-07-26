*----------------------------------------------------------------------*
***INCLUDE /NRK/LAPAYTACSPAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'SAVE'.
      PERFORM check_data_change.
      PERFORM check_workflows.
      PERFORM restart_workflow.
      PERFORM update_data.
      result = 'C'.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'IMAGE'.
      PERFORM display_image USING wa_hd.
    WHEN 'HIS'.
      PERFORM display_history USING wa_hd.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0100 INPUT.

  CASE ok_code.
    WHEN 'EXIT'.
      result = 'E'.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'IMAGE'.
      PERFORM display_image USING wa_hd.
    WHEN 'HIS'.
      PERFORM display_history USING wa_hd.
  ENDCASE.

ENDMODULE.                 " EXIT_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATA_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_data_0100 INPUT.

  PERFORM check_data_change.
  PERFORM check_data.
* PERFORM check_workflows.

ENDMODULE.                 " CHECK_DATA_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATA_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_data_0200 INPUT.

* PERFORM check_data_0200.
  PERFORM check_data_change.
  PERFORM check_data.

ENDMODULE.                 " CHECK_DATA_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0200 INPUT.

  CASE ok_code.
    WHEN 'EXIT'.
      result = 'E'.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'CLEAR'.
      CLEAR: wa_hd,
             wa_hd_orig,
             wa_ekko,
             wa_lfa1,
             wa_lfb1,
             ebeln,
             lifnr,
             lifname,
             wrbtr,
             wmwst,
             wrbtr_net,
             waers,
             bukrs,
             butxt,
             bldat,
             xblnr,
             rdate,
             fullname,
             street,
             city,
             zip,
             region,
             ekgrp,
             ekorg.

      CLEAR: arc_doc_id,
             filetab,
             outdoctab[].

      REFRESH: outdoctab.

  ENDCASE.

ENDMODULE.                 " EXIT_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE ok_code.
    WHEN 'UPLD'.

      PERFORM upload_document.

      IF cancelled NE 'X'.
        PERFORM create_apay_record.
        PERFORM create_apay_link.
        PERFORM clear_after_index.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  CAPTURE_RDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE capture_rdate INPUT.

  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = rdate
    IMPORTING
      select_date                  = rdate
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDMODULE.                 " CAPTURE_RDATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CAPTURE_BLDAT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE capture_bldat INPUT.

  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = bldat
    IMPORTING
      select_date                  = bldat
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDMODULE.                 " CAPTURE_BLDAT  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_VENDOR_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_vendor_info INPUT.

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

ENDMODULE.                 " GET_VENDOR_INFO  INPUT
