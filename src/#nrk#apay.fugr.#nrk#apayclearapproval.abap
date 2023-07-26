FUNCTION /nrk/apayclearapproval.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"  EXPORTING
*"     REFERENCE(CLEARED) TYPE  BOOLE-BOOLE
*"----------------------------------------------------------------------

  DELETE FROM /nrk/apayappr WHERE apayno EQ apayno.

  IF sy-subrc EQ 0.
    cleared = 'X'.
  ENDIF.

ENDFUNCTION.
