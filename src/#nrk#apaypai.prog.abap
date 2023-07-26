*&---------------------------------------------------------------------*
*&  Include           /NRK/APAYPAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'SHIS'. " Display status history
      PERFORM get_line.
*     g_view = '5'.

      PERFORM get_status_history.

      CALL SCREEN 0500.

    WHEN 'RFSH'.

      PERFORM get_record_data.

      CALL METHOD alv_grid->refresh_table_display.
      CALL METHOD cl_gui_control=>set_focus
        EXPORTING
          control           = alv_grid
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2.

    WHEN 'POST'.
      PERFORM get_line.
      PERFORM post_document.

    WHEN 'LITM'.
      PERFORM get_line.
      PERFORM get_line_items.

      CALL SCREEN 0600.

    WHEN 'ECOM'.
* Enter comment
      PERFORM get_line.
      CALL FUNCTION '/NRK/APAY_ADD_COMMENT'
        EXPORTING
          iv_apayno        = wa_dis-apayno
        EXCEPTIONS
          entry_not_found  = 1
          db_insert_failed = 2
          OTHERS           = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    WHEN 'DCOM'.
* Display comments
      PERFORM get_line.
      CALL FUNCTION '/NRK/APAY_DISPLAY_COMMENT'
        EXPORTING
          iv_apayno        = wa_dis-apayno
        EXCEPTIONS
          entry_not_found  = 1
          db_insert_failed = 2
          OTHERS           = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    WHEN 'DWNL'.
* Download images
      PERFORM get_lines.
      PERFORM download_images TABLES wt_dis_seltab.

    WHEN 'PRNT'.
* Print images
      PERFORM get_lines.
      PERFORM print_images TABLES wt_dis_seltab.

    WHEN 'RSTR'.
* Restart workflow
*     AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD 'SWPR'.
      AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD 'SBWP'.
      IF sy-subrc <> 0.
        MESSAGE e172(pg) WITH text-981.
      ENDIF.

      PERFORM get_lines.
      CALL FUNCTION '/NRK/APAYRESTARTWORKFLOW'
        EXPORTING
          apayno             = wa_dis-apayno
        EXCEPTIONS
          no_workflows_found = 1
          no_cancellation    = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        MESSAGE e208(00) WITH text-100 .
      ENDIF.
    WHEN 'NEW'.

      CLEAR: cancel.
      CALL FUNCTION '/NRK/APAYUPLOADNEWDOCUMENT'
        IMPORTING
*         APAYNO                       =
          cancel                       = cancel
        EXCEPTIONS
          document_upload_failed       = 1
          OTHERS                       = 2.

      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      IF cancel NE 'X'.

        PERFORM get_record_data.

        CALL METHOD alv_grid->refresh_table_display.
        CALL METHOD cl_gui_control=>set_focus
          EXPORTING
            control           = alv_grid
          EXCEPTIONS
            cntl_error        = 1
            cntl_system_error = 2.

      ENDIF.
    WHEN 'AGEN'. " Display agents

      PERFORM get_line.

      CALL FUNCTION '/NRK/APAYDISPLAYAGENTS'
        EXPORTING
          apayno = wa_dis-apayno
        EXCEPTIONS
          error  = 1
          OTHERS = 2.

      IF sy-subrc <> 0.
        MESSAGE e208(00) WITH text-101 .
      ENDIF.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_exit_0100 INPUT.

  CASE ok_code.
    WHEN 'BACK'.
*     CALL METHOD cl_gui_cfw=>flush.
      PERFORM leave.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
*     CALL METHOD cl_gui_cfw=>flush.
      PERFORM leave.
      LEAVE PROGRAM.
  ENDCASE.

  CLEAR ok_code.

ENDMODULE.                 " USER_EXIT_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.

ENDMODULE.                 " USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_exit_0500 INPUT.

  CASE ok_code.
    WHEN 'BACK'.

      PERFORM leave.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.

      PERFORM leave.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_EXIT_0500  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0600 INPUT.

ENDMODULE.                 " USER_COMMAND_0600  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_0600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_exit_0600 INPUT.

  CASE ok_code.
    WHEN 'BACK'.

      PERFORM leave.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.

      PERFORM leave.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_EXIT_0600  INPUT
