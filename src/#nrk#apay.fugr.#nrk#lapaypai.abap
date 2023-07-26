*----------------------------------------------------------------------*
***INCLUDE /NRK/LAPAYPAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0901  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0901 INPUT.

  CASE ok_code.
    WHEN 'CONT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR ok_code.

ENDMODULE.                 " USER_COMMAND_0901  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0902  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0902 INPUT.
  CASE ok_code.
    WHEN 'CONT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR: ok_code.

ENDMODULE.                 " USER_COMMAND_0902  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0902  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0902 INPUT.

  CASE ok_code.
    WHEN 'EXIT'.
      CLEAR except.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR: ok_code.

ENDMODULE.                 " EXIT_COMMAND_0902  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0903  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0903 INPUT.

  CASE ok_code.
    WHEN 'CONT'.
*     IF ebeln IS INITIAL.
*       MESSAGE text-112 TYPE 'E'.
*     ELSE.
        cancel = ' '.
        LEAVE TO SCREEN 0.
*     ENDIF.
  ENDCASE.

  CLEAR ok_code.

ENDMODULE.                 " USER_COMMAND_0903  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATA_0903  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_data_0903 INPUT.

  IF NOT ebeln IS INITIAL.

    SELECT SINGLE ebeln FROM ekko INTO ebeln WHERE ebeln EQ ebeln.

    IF sy-subrc NE 0.
      MESSAGE text-111 TYPE 'E'.
    ENDIF.
*  ELSE.
*    MESSAGE text-112 TYPE 'E'.
  ENDIF.

ENDMODULE.                 " CHECK_DATA_0903  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_0903  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_exit_0903 INPUT.

  CASE ok_code.
    WHEN 'EXIT'.
      cancel = 'X'.
      LEAVE TO SCREEN 0.
        WHEN 'CREA'. " create PO
      create = 'X'.
      cancel = ' '.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR ok_code.

ENDMODULE.                 " USER_EXIT_0903  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATA_0904  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_data_0904 INPUT.

  IF NOT lifnr IS INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lifnr
      IMPORTING
        output = lifnr.

    SELECT SINGLE lifnr FROM lfa1 INTO lifnr WHERE lifnr EQ lifnr.

    IF sy-subrc NE 0.
      MESSAGE text-113 TYPE 'E'.
    ENDIF.
*  ELSE.
*    MESSAGE text-114 TYPE 'E'.
  ENDIF.

ENDMODULE.                 " CHECK_DATA_0904  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0904  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0904 INPUT.

  CASE ok_code.
    WHEN 'CONT'.
*      IF lifnr IS INITIAL.
*        MESSAGE text-114 TYPE 'E'.
*      ELSE.
      cancel = ' '.
      LEAVE TO SCREEN 0.
*      ENDIF.
  ENDCASE.

  CLEAR ok_code.


ENDMODULE.                 " USER_COMMAND_0904  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_0904  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_exit_0904 INPUT.

  CASE ok_code.
    WHEN 'EXIT'.
      cancel = 'X'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR ok_code.

ENDMODULE.                 " USER_EXIT_0904  INPUT
