FUNCTION /nrk/apaycheckgrforpo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"  EXPORTING
*"     REFERENCE(RESULT_GR) TYPE  CHAR1
*"     REFERENCE(RESULT_INV) TYPE  CHAR1
*"     REFERENCE(RESULT) TYPE  CHAR1
*"----------------------------------------------------------------------

* Result values:
*  E = No record found
*  M = No PO found
*  L = No PO line items found
*
*  N = No GR received for PO
*  P = Goods (partially) invoiced
*  R = Goods (partially) received
*  C = Goods completely received
*  I = All goods invoiced

  DATA: wa_hd         LIKE /nrk/apayhd,
        t_ekbe        LIKE ekbe OCCURS 0,
        t_ekpo        LIKE ekpo OCCURS 0,
        wa_ekbe       LIKE ekbe,
        goodsrcvd     LIKE ekbe-menge,   "goods received in units
        goodsinvoiced LIKE ekbe-menge,   "goods invoiced in units
        bpmng        LIKE ekbe-bpmng.

  DATA: BEGIN OF sums OCCURS 4,
          bewtp LIKE ekbe-bewtp,
          menge LIKE ekbe-menge,
          shkzg LIKE ekbe-shkzg,          "Debt/Credit indicator
        END OF sums.

  CLEAR: wa_hd,
         t_ekbe[],
         wa_ekbe,
         bpmng.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno = apayno.

  IF sy-subrc <> 0.
    result = 'E'.
    EXIT.
  ENDIF.

  IF wa_hd-ebeln IS INITIAL.
    result = 'M'.
    EXIT.
  ENDIF.

  SELECT * FROM ekbe INTO TABLE t_ekbe
    WHERE ebeln = wa_hd-ebeln.

  IF sy-subrc <> 0.

    SELECT * FROM ekpo INTO TABLE t_ekpo
      WHERE ebeln = wa_hd-ebeln.

    IF sy-subrc <> 0.

      result = 'L'.
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE t_ekbe WITH KEY bewtp = 'E' INTO wa_ekbe.

  IF sy-subrc <> 0.
    result_gr = 'N'.
    EXIT.
  ENDIF.

  LOOP AT t_ekbe INTO wa_ekbe.

    CLEAR sums. CLEAR sums[].

    SELECT bewtp menge shkzg FROM ekbe INTO sums
        WHERE ebeln = wa_ekbe-ebeln AND ebelp = wa_ekbe-ebelp.
      COLLECT sums.
    ENDSELECT.

  ENDLOOP.

  goodsrcvd = 0. goodsinvoiced = 0.

  LOOP AT sums.                      "sums table should now have 4 totals
    IF sums-shkzg = 'S'.             "positive value
      IF sums-bewtp = 'E'.           "goods received
        goodsrcvd = goodsrcvd + sums-menge.
        result_inv = 'P'.
        EXIT.
      ELSE.                          "goods invoiced
        goodsinvoiced = goodsinvoiced + sums-menge.
      ENDIF.
    ELSE.                            "negative value
      IF sums-bewtp = 'E'.           "goods received
        goodsrcvd = goodsrcvd - sums-menge.
      ELSE.                          "goods invoiced
        goodsinvoiced = goodsinvoiced - sums-menge.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF goodsrcvd = goodsinvoiced.        "amount received = amount invoiced
    result_inv = 'I'.
    EXIT.
  ELSE.
    bpmng = goodsrcvd - goodsinvoiced. "missing invoice amount
    result_inv = 'P'.
    EXIT.
  ENDIF.

ENDFUNCTION.
