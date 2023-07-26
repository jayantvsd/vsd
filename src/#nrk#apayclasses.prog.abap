*&---------------------------------------------------------------------*
*&  Include           /NRK/APAYCLASSES
*&---------------------------------------------------------------------*

* pre-load class
CLASS cl_gui_cfw DEFINITION LOAD.

* class for event receiver implementation
CLASS cl_event_receiver IMPLEMENTATION.

  METHOD handle_double_click.
    PERFORM drill_down USING e_row-index.
  ENDMETHOD.                    "handle_double_click

  METHOD handle_user_command.
* code missing
  ENDMETHOD.                    "handle_user_command

*Hotspot handling
  METHOD handle_hotspot_click.

    CASE e_column_id-fieldname.

      WHEN 'LIFNR' OR 'LIFNAME'.
        READ TABLE t_apayhd_dis INTO wa_dis INDEX e_row_id-index.
* Display vendor master
        IF sy-subrc = 0.
          IF wa_dis-lifnr IS INITIAL.
            CLEAR: wa_dis.
            MESSAGE s398(00) WITH text-988 space space space.
            EXIT.
          ENDIF.
          CALL FUNCTION 'OM_FUNC_MODULE_EXIST'
            EXPORTING
              function_module = '/NRK/DSETMV_SUBMIT'
            EXCEPTIONS
              not_existent    = 1
              OTHERS          = 2.
          IF sy-subrc = 0.
            CALL FUNCTION '/NRK/DSETMV_SUBMIT'
              EXPORTING
                lifnr = wa_dis-lifnr.
          ELSE.
            IF wa_dis-lifnr IS INITIAL.
              CLEAR: wa_dis.
              MESSAGE s398(00) WITH text-988 space space space.
              EXIT.
            ENDIF.
            PERFORM display_vendor USING wa_dis.
          ENDIF.
        ELSE.
          MESSAGE s398(00) WITH text-900 wa_dis-lifnr text-901 space.
        ENDIF.

      WHEN 'BELNR'.
        READ TABLE t_apayhd_dis INTO wa_dis INDEX e_row_id-index.
* Display invoice
        IF sy-subrc = 0.

          IF wa_dis-belnr IS INITIAL.
            CLEAR: wa_dis.
            MESSAGE s398(00) WITH text-987 space space space.
            EXIT.
          ENDIF.

          CALL FUNCTION 'OM_FUNC_MODULE_EXIST'
            EXPORTING
              function_module = '/NRK/DSETAP_SUBMIT'
            EXCEPTIONS
              not_existent    = 1
              OTHERS          = 2.
          IF sy-subrc = 0.
            CALL FUNCTION '/NRK/DSETAP_SUBMIT'
              EXPORTING
                bukrs = wa_dis-bukrs
                belnr = wa_dis-belnr
                gjahr = wa_dis-gjahr.
          ELSE.
            IF wa_dis-belnr IS INITIAL.
              CLEAR: wa_dis.
              MESSAGE s398(00) WITH text-987 space space space.
              EXIT.
            ENDIF.
            PERFORM display_fi_document USING wa_dis.
          ENDIF.
        ELSE.
          MESSAGE s398(00) WITH text-902 wa_dis-belnr text-901 space.
        ENDIF.

      WHEN 'EBELN'.
        READ TABLE t_apayhd_dis INTO wa_dis INDEX e_row_id-index.
* Display purchase order
        IF sy-subrc = 0.

          IF wa_dis-ebeln IS INITIAL.
            CLEAR: wa_dis.
            MESSAGE s398(00) WITH text-986 space space space.
            EXIT.
          ENDIF.

          CALL FUNCTION 'OM_FUNC_MODULE_EXIST'
            EXPORTING
              function_module = '/NRK/DSETPO_SUBMIT'
            EXCEPTIONS
              not_existent    = 1
              OTHERS          = 2.

          IF sy-subrc = 0.
            CALL FUNCTION '/NRK/DSETPO_SUBMIT'
              EXPORTING
                ebeln = wa_dis-ebeln.
          ELSE.

            IF wa_dis-ebeln IS INITIAL.
              CLEAR: wa_dis.
              MESSAGE s398(00) WITH text-986 space space space.
              EXIT.
            ENDIF.

            PERFORM display_purchase_order USING wa_dis.
          ENDIF.
        ELSE.
          MESSAGE s398(00) WITH text-903 wa_dis-ebeln text-901 space.
        ENDIF.

      WHEN 'LIV_BELNR'.
        READ TABLE t_apayhd_dis INTO wa_dis INDEX e_row_id-index.
* Display LIV invoice
        IF sy-subrc = 0.
          IF wa_dis-liv_belnr IS INITIAL.
            CLEAR: wa_dis.
            MESSAGE s398(00) WITH text-989 space space space.
            EXIT.
          ENDIF.
          CALL FUNCTION 'OM_FUNC_MODULE_EXIST'
            EXPORTING
              function_module = '/NRK/DSETMI_SUBMIT'
            EXCEPTIONS
              not_existent    = 1
              OTHERS          = 2.

          IF sy-subrc = 0.
            CALL FUNCTION '/NRK/DSETMI_SUBMIT'
              EXPORTING
                belnr = wa_dis-liv_belnr
                gjahr = wa_dis-liv_gjahr
                blart = wa_dis-blart.
          ELSE.
            IF wa_dis-liv_belnr IS INITIAL.
              CLEAR: wa_dis.
              MESSAGE s398(00) WITH text-989 space space space.
              EXIT.
            ENDIF.
            PERFORM display_mm_document USING wa_dis.
          ENDIF.
        ELSE.
          MESSAGE s398(00) WITH text-904 wa_dis-liv_belnr text-901 space.
        ENDIF.

      WHEN 'AUGBL'.
        READ TABLE t_apayhd_dis INTO wa_dis INDEX e_row_id-index.
* Display clearing document
        IF sy-subrc = 0.
          IF wa_dis-augbl IS INITIAL.
            CLEAR: wa_dis.
            MESSAGE s398(00) WITH text-990 space space space.
            EXIT.
          ENDIF.
          PERFORM display_clearing_document USING wa_dis.
        ELSE.
          MESSAGE s398(00) WITH text-905 wa_dis-augbl text-901 space.
        ENDIF.

      WHEN 'OBJECTTEXT'.
        READ TABLE t_apayhd_dis INTO wa_dis INDEX e_row_id-index.
* Display images
        IF sy-subrc = 0.
          PERFORM display_image USING wa_dis.
        ELSE.
          MESSAGE s398(00) WITH text-906 wa_dis-objecttext text-901 space.
        ENDIF.

      WHEN 'COMMENT_ICON'.
        READ TABLE t_apayhd_dis INTO wa_dis INDEX e_row_id-index.
* Display comment
        IF sy-subrc = 0.
          PERFORM display_comment USING wa_dis.
        ELSE.
          MESSAGE s398(00) WITH text-906 wa_dis-objecttext text-901 space.
        ENDIF.

      WHEN 'SDESCR'.
        READ TABLE t_apayhd_dis INTO wa_dis INDEX e_row_id-index.
* Display history
        IF sy-subrc = 0.
          PERFORM display_history USING wa_dis.
        ELSE.
          MESSAGE s398(00) WITH text-907 wa_dis-apayno space space.
        ENDIF.

      WHEN 'WORKFLOWID'.
        READ TABLE t_apayhd_dis INTO wa_dis INDEX e_row_id-index.
* Display workflow history
        IF sy-subrc = 0.
          PERFORM display_workflowlog USING wa_dis-workflowid.
        ELSE.
          MESSAGE s398(00) WITH text-907 wa_dis-apayno space space.
        ENDIF.
      WHEN 'APAYNO'.
        READ TABLE t_apayhd_dis INTO wa_dis INDEX e_row_id-index.
        IF sy-subrc = 0.
          PERFORM change_apayrecord USING wa_dis-apayno.
        ELSE.
          MESSAGE s398(00) WITH text-908 wa_dis-apayno space space.
        ENDIF.
      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "cl_event_receiver IMPLEMENTATION
