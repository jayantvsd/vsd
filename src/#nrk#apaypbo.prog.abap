*&---------------------------------------------------------------------*
*&  Include           /NRK/APAYPBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

* Set Status
  IF g_view EQ '1'. " Ledger

    SET PF-STATUS '/NRK/APAY1'.
    SET TITLEBAR '100'.

    PERFORM create_grid.

  ELSEIF g_view EQ '2'. " Batch

    SET PF-STATUS '/NRK/APAY4'.
    SET TITLEBAR '130'.

    PERFORM create_grid.

  ELSEIF g_view EQ '3'. " Documents

    SET PF-STATUS '/NRK/APAY2'.
    SET TITLEBAR '110'.

    PERFORM create_grid.

  ELSEIF g_view EQ '4'. " Workflows

    SET PF-STATUS '/NRK/APAY3'.
    SET TITLEBAR '120'.

    PERFORM create_grid.

  ELSEIF g_view EQ '7'. " Ledger v3

    SET PF-STATUS '/NRK/APAY7'.
    SET TITLEBAR '100'.

    PERFORM create_grid.

  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.

  SET PF-STATUS '/NRK/APAY5'.
  SET TITLEBAR '500'.

* PERFORM get_line.
* PERFORM get_status_history.

  PERFORM create_history_grid.

ENDMODULE.                 " STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0600  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0600 OUTPUT.

  SET PF-STATUS '/NRK/APAY6'.
  SET TITLEBAR '600'.

  PERFORM create_lineitem_grid.

ENDMODULE.                 " STATUS_0600  OUTPUT
