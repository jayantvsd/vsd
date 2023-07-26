FUNCTION /nrk/apayget_table_string.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(ROW) TYPE  I
*"     REFERENCE(OFFSET) TYPE  I
*"     REFERENCE(LINE_LENGTH) TYPE  I
*"     REFERENCE(ITAB_LENGTH) TYPE  I
*"  EXPORTING
*"     REFERENCE(EROW) TYPE  I
*"     REFERENCE(EOFFSET) TYPE  I
*"     REFERENCE(LINE)
*"  TABLES
*"      ITAB
*"----------------------------------------------------------------------

  DATA: l_row TYPE i,
        l_rcnt TYPE i,
        l_itab_len TYPE i,
        l_chunk_len TYPE i,
        l_offset    TYPE i.

  FIELD-SYMBOLS: <ifs> TYPE ANY,
                 <ofs> TYPE ANY.

  IF row EQ 0.
    l_row = 1.
  ELSE.
    l_row = row.
  ENDIF.

  l_chunk_len = itab_length - offset.
  eoffset = offset.

  ASSIGN line TO <ofs>.

  LOOP AT itab FROM l_row ASSIGNING <ifs>.

    IF l_chunk_len > itab_length.
      l_chunk_len = itab_length.
    ENDIF.

    IF line_length < l_chunk_len.

      l_chunk_len = line_length.
      <ofs>+l_offset = <ifs>+eoffset(l_chunk_len).
      eoffset = eoffset + l_chunk_len.
      EXIT.

    ELSEIF line_length = l_chunk_len.

      l_chunk_len = line_length.
      <ofs>+l_offset = <ifs>+eoffset(l_chunk_len).
      l_rcnt = l_rcnt + 1.
      eoffset = 0.
      EXIT.

    ELSE.

      <ofs>+l_offset = <ifs>+eoffset(l_chunk_len).
      l_offset = l_offset + l_chunk_len.
      eoffset = eoffset + l_chunk_len.

      IF itab_length = eoffset.
        l_rcnt = l_rcnt + 1.
        eoffset = 0.
      ENDIF.

      IF l_offset = line_length.
        EXIT.
      ENDIF.

    ENDIF.

    l_chunk_len = line_length - l_offset.

  ENDLOOP.

  erow = l_row + l_rcnt.

  DESCRIBE TABLE itab LINES l_rcnt.

  IF erow > l_rcnt.
    erow = 0.
  ENDIF.



ENDFUNCTION.
