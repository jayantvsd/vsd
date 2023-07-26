*----------------------------------------------------------------------*
***INCLUDE /NRK/LAPAYPBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0901  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0901 OUTPUT.
  SET PF-STATUS '0901'.
  SET TITLEBAR '901'.
ENDMODULE.                 " STATUS_0901  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0902  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0902 OUTPUT.
  SET PF-STATUS '0902'.
  SET TITLEBAR '902'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'EXCEPT'
      values          = extype_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMODULE.                 " STATUS_0902  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0903  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0903 OUTPUT.
  SET PF-STATUS '0903'.
  SET TITLEBAR '903'.

ENDMODULE.                 " STATUS_0903  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0904  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0904 output.
  SET PF-STATUS '0904'.
  SET TITLEBAR '904'.

endmodule.                 " STATUS_0904  OUTPUT
