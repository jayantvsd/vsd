FUNCTION /nrk/apaychangetablesize.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      ITAB
*"      OTAB
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: l_offset    TYPE i,
        l_row       TYPE i.

  DATA: l_itab_length TYPE i,
        l_otab_length TYPE i.

  DATA: l_line(1024)  TYPE c.


  DESCRIBE FIELD itab LENGTH l_itab_length IN CHARACTER MODE.
  DESCRIBE FIELD otab LENGTH l_otab_length IN CHARACTER MODE.

  IF l_otab_length GT 1024.
    RAISE error.
  ENDIF.

  DO.

    CALL FUNCTION '/NRK/APAYGET_TABLE_STRING'
      EXPORTING
        row         = l_row
        offset      = l_offset
        line_length = l_otab_length
        itab_length = l_itab_length
      IMPORTING
        erow        = l_row
        eoffset     = l_offset
        line        = l_line
      TABLES
        itab        = itab.

    APPEND l_line TO otab.
    CLEAR l_line.

    IF l_row EQ 0.
      EXIT.
    ENDIF.

  ENDDO.

ENDFUNCTION.
