FUNCTION /nrk/apayentervendor.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYNO
*"  EXCEPTIONS
*"      CANCEL
*"----------------------------------------------------------------------

  DATA: wa_hd LIKE /nrk/apayhd,
        wa_lfa1 LIKE lfa1,
        wa_lfb1 LIKE lfb1,
        lifname LIKE /nrk/apayhd-lifname.

  CLEAR: wa_hd.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayno.

  IF sy-subrc EQ 0.
    lifnr = wa_hd-lifnr.
  ENDIF.

  CALL SCREEN 0904 STARTING AT 10 5 ENDING AT 50 10.

  IF cancel EQ 'X'.
    RAISE cancel.
  ENDIF.

  IF NOT lifnr IS INITIAL.
    wa_hd-lifnr = lifnr.

* Get vendor information
    IF NOT wa_hd-lifnr IS INITIAL
      AND NOT wa_hd-bukrs IS INITIAL.
      CALL FUNCTION 'VENDOR_READ'
        EXPORTING
          i_bukrs   = wa_hd-bukrs
          i_lifnr   = wa_hd-lifnr
        IMPORTING
          e_lfa1    = wa_lfa1
          e_lfb1    = wa_lfb1
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      IF sy-subrc <> 0.
        wa_hd-lifname = lifname.
      ELSE.
        IF lifname IS INITIAL.
          wa_hd-lifname = wa_lfa1-name1.
        ENDIF.
        wa_hd-zterm   = wa_lfb1-zterm.
        wa_hd-stras   = wa_lfa1-stras.
        wa_hd-pstlz   = wa_lfa1-pstlz.
        wa_hd-ort01   = wa_lfa1-ort01.
        wa_hd-land1   = wa_lfa1-land1.
*      wa_hd-lifname = wa_lfa1-name1.

        TRANSLATE wa_hd-lifname TO UPPER CASE.

        wa_hd-zsabe = wa_lfb1-zsabe.
        wa_hd-zahls = wa_lfb1-zahls.
        wa_hd-zterm = wa_lfb1-zterm.
        wa_hd-busab = wa_lfb1-busab.
      ENDIF.
    ENDIF.

    UPDATE /nrk/apayhd FROM wa_hd.
  ELSE.
    RAISE cancel.
  ENDIF.

ENDFUNCTION.
