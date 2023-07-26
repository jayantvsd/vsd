FUNCTION /nrk/apayapi_bapi_search_help.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FIELD) TYPE  CHAR50 OPTIONAL
*"     VALUE(VALUE) TYPE  CHAR50 OPTIONAL
*"     VALUE(EXACTMATCH) TYPE  BOOLE OPTIONAL
*"     VALUE(APAYNO) TYPE  /NRK/APAYHD-APAYNO OPTIONAL
*"  TABLES
*"      SEARCHRESULT STRUCTURE  /NRK/APAYAPISEARCH OPTIONAL
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"----------------------------------------------------------------------

  DATA: t_lfa1  LIKE lfa1 OCCURS 0,
        wa_lfa1 LIKE lfa1,
        t_hd    LIKE /nrk/apayhd OCCURS 0,
        wa_hd   LIKE /nrk/apayhd,
        t_stat LIKE /nrk/apaysdef OCCURS 0,
        wa_stat LIKE /nrk/apaysdef,
        t_approver LIKE /nrk/apayuser OCCURS 0,
        wa_approver LIKE /nrk/apayuser,
        t_bukrs LIKE t001 OCCURS 0 WITH HEADER LINE,
        t_csks LIKE csks OCCURS 0,
        wa_csks LIKE csks,
        t_prps LIKE prps OCCURS 0,
        wa_prps LIKE prps,
        t_prpmp LIKE v_prpmp OCCURS 0,
        wa_prpmp LIKE v_prpmp,
        t_skb1 LIKE skb1 OCCURS 0,
        wa_skb1 LIKE skb1,
        t_aufk LIKE aufk OCCURS 0,
        wa_aufk LIKE aufk.

  DATA:  i_lifnr  TYPE RANGE OF lifnr INITIAL SIZE 0,
         i_lifname TYPE RANGE OF /nrk/apaylname INITIAL SIZE 0,
         wa_lifnr LIKE LINE OF  i_lifnr,
         wa_lifname LIKE LINE OF i_lifname,
         i_xblnr TYPE RANGE OF xblnr INITIAL SIZE 0,
         wa_xblnr LIKE LINE OF i_xblnr,
         i_status TYPE RANGE OF /nrk/apaystatus INITIAL SIZE 0,
         wa_status LIKE LINE OF i_status,
         i_bukrs TYPE RANGE OF bukrs INITIAL SIZE 0,
         wa_bukrs LIKE LINE OF i_bukrs,
         i_kostl TYPE RANGE OF kostl INITIAL SIZE 0,
         wa_kostl LIKE LINE OF i_kostl,
         i_posid TYPE RANGE OF ps_posid INITIAL SIZE 0,
         wa_posid LIKE LINE OF i_posid,
         i_poski TYPE RANGE OF ps_poski INITIAL SIZE 0,
         wa_poski LIKE LINE OF i_poski,
         i_saknr  TYPE RANGE OF saknr INITIAL SIZE 0,
         wa_saknr LIKE LINE OF i_saknr,
         i_aufnr  TYPE RANGE OF aufnr INITIAL SIZE 0,
         wa_aufnr LIKE LINE OF i_aufnr,
         l_length TYPE i.

  DATA: l_bukrs TYPE bukrs,
        l_ktopl LIKE t001-ktopl,
        l_saknr LIKE skb1-saknr,
        l_aufnr LIKE aufk-aufnr,
        l_posid LIKE prps-posid,
        wa_skat LIKE skat.

  CLEAR: i_lifnr[],
         i_lifname[],
         wa_lifnr,
         wa_lifname,
         i_xblnr[],
         i_xblnr,
         wa_xblnr,
         wa_status,
         i_status,
         i_status[],
         t_hd[],
         wa_hd.

  DATA: configvalue TYPE char255.

  IF field EQ 'LIFNR'. " vendor number

    wa_lifnr-sign = 'I'.
    wa_lifnr-option = 'CP'.
    CONCATENATE '*' value '*' INTO wa_lifnr-low.
    APPEND wa_lifnr TO i_lifnr.

    SELECT * FROM lfa1
      INTO CORRESPONDING FIELDS OF TABLE t_lfa1
      WHERE lifnr IN i_lifnr.

    LOOP AT t_lfa1 INTO wa_lfa1.

      IF wa_lfa1-lifnr CN wa_lifnr-low.

      ELSE.

        MOVE wa_lfa1-lifnr TO searchresult-value1.
        MOVE wa_lfa1-name1 TO searchresult-value2.
        APPEND searchresult.

      ENDIF.

    ENDLOOP.

  ELSEIF field EQ 'LIFNAME'. " Vendor Name

    TRANSLATE value TO UPPER CASE.

    wa_lifname-sign = 'I'.
    wa_lifname-option = 'CP'.
    CONCATENATE '*' value '*' INTO wa_lifname-low.
    APPEND wa_lifname TO i_lifname.

    SELECT * FROM lfa1
      INTO CORRESPONDING FIELDS OF TABLE t_lfa1
      WHERE mcod1 IN i_lifname.

    LOOP AT t_lfa1 INTO wa_lfa1.

      IF wa_lfa1-mcod1 CN wa_lifname-low.

        MOVE wa_lfa1-lifnr TO searchresult-value1.
        MOVE wa_lfa1-name1 TO searchresult-value2.
        APPEND searchresult.

      ENDIF.

    ENDLOOP.

  ELSEIF field EQ 'XBLNR'. " External invoice number

    wa_xblnr-sign = 'I'.
    wa_xblnr-option = 'CP'.
    CONCATENATE value '*' INTO wa_xblnr-low.
    APPEND wa_xblnr TO i_xblnr.

    SELECT * FROM /nrk/apayhd
      INTO CORRESPONDING FIELDS OF TABLE t_hd
      WHERE xblnr IN i_xblnr.

    LOOP AT t_hd INTO wa_hd.

      MOVE wa_hd-xblnr TO searchresult-value1.
      MOVE wa_hd-lifname TO searchresult-value2.
      APPEND searchresult.

    ENDLOOP.

  ELSEIF field EQ 'BELNR'. " Accounting document number

  ELSEIF field EQ 'BUKRS'. " Company Code
    wa_bukrs-sign = 'I'.
    wa_bukrs-option = 'CP'.
    CONCATENATE value '*' INTO wa_bukrs-low.
    APPEND wa_bukrs TO i_bukrs.

    SELECT * FROM t001
      INTO CORRESPONDING FIELDS OF TABLE t_bukrs
      WHERE bukrs IN i_bukrs.
*       and langu eq sy-langu.

    LOOP AT t_bukrs.
      MOVE t_bukrs-bukrs TO searchresult-value1.
      MOVE t_bukrs-butxt TO searchresult-value2.
      APPEND searchresult.
    ENDLOOP.

  ELSEIF field EQ 'STATUS'.

    wa_status-sign = 'I'.
    wa_status-option = 'CP'.
    CONCATENATE value '*' INTO wa_status-low.
    APPEND wa_status TO i_status.

    SELECT * FROM /nrk/apaysdef
      INTO CORRESPONDING FIELDS OF TABLE t_stat
      WHERE status IN i_status
        AND langu  EQ sy-langu.

    LOOP AT t_stat INTO wa_stat.

      MOVE wa_stat-status TO searchresult-value1.
      MOVE wa_stat-sdescr TO searchresult-value2.
      APPEND searchresult.

    ENDLOOP.
  ELSEIF field EQ 'APPROVER'.

    SELECT * FROM /nrk/apayuser
      INTO CORRESPONDING FIELDS OF TABLE t_approver
      WHERE otype EQ 'X'
        AND approver EQ 'X'.

    LOOP AT t_approver INTO wa_approver.
      MOVE wa_approver-extuser TO searchresult-value1.
      MOVE wa_approver-fullname TO searchresult-value2.
      APPEND searchresult.
    ENDLOOP.

  ELSEIF field EQ 'PROJK'. " WBS Element

* initial logic
*    wa_posid-sign = 'I'.
*    wa_posid-option = 'CP'.
*    CONCATENATE value '*' INTO wa_posid-low.
*    APPEND wa_posid TO i_posid.
*    SELECT * FROM prps INTO TABLE t_prps
*      UP TO 100 ROWS
*      WHERE posid IN i_posid.

* 1st update logic
*    TRANSLATE value TO UPPER CASE.
*    wa_poski-sign = 'I'.
*    wa_poski-option = 'CP'.
*    CONCATENATE value '*' INTO wa_poski-low.
*    APPEND wa_poski TO i_poski.
*
*    SELECT * FROM prps INTO TABLE t_prps
*      UP TO 100 ROWS
*      WHERE poski IN i_poski.
*
*    LOOP AT t_prps INTO wa_prps.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
*        EXPORTING
*          input  = wa_prps-posid
*        IMPORTING
*          output = wa_prps-posid.
*
*      MOVE wa_prps-posid TO searchresult-value1.
*      APPEND searchresult.
*    ENDLOOP.

* 2nd update logic
    TRANSLATE value TO UPPER CASE.
    wa_posid-sign = 'I'.
    wa_posid-option = 'CP'.
    CONCATENATE value '*' INTO wa_posid-low.
    APPEND wa_posid TO i_posid.
    SELECT * FROM v_prpmp INTO TABLE t_prpmp
      UP TO 100 ROWS
      WHERE posid IN i_posid.

*** start change 10/07/2015
    IF sy-subrc NE 0.
      TRANSLATE value TO UPPER CASE.
      wa_poski-sign = 'I'.
      wa_poski-option = 'CP'.
      CONCATENATE value '*' INTO wa_poski-low.
      APPEND wa_poski TO i_poski.

      SELECT * FROM v_prpmp INTO TABLE t_prpmp
        UP TO 100 ROWS
        WHERE poski IN i_poski.
    ENDIF.
*** end change 10/07/2015

    LOOP AT t_prpmp INTO wa_prpmp.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          input  = wa_prpmp-posid
        IMPORTING
          output = wa_prpmp-posid.

* value 2 is visible
* value 1 is internal numbers, which is passed back

      IF NOT wa_prpmp-poski IS INITIAL.
        MOVE wa_prpmp-posid TO searchresult-value1.
        MOVE wa_prpmp-poski TO searchresult-value2.
      ELSE.
        MOVE wa_prpmp-posid TO searchresult-value2.
        MOVE wa_prpmp-posid TO searchresult-value1.
*       MOVE wa_prpmp-posid TO searchresult-value3.
      ENDIF.

* MOVE 'XXX' TO searchresult-value1.

      APPEND searchresult.
    ENDLOOP.

  ELSEIF field EQ 'KOSTL'. " cost center

    wa_kostl-sign = 'I'.
    wa_kostl-option = 'CP'.

    l_length = STRLEN( value ).

    MOVE value TO wa_kostl-low.

    IF l_length GT 8.

    ELSE.

      CONCATENATE '*' value '*' INTO wa_kostl-low.

    ENDIF.

    APPEND wa_kostl TO i_kostl.

    SELECT * FROM csks INTO TABLE t_csks
      UP TO 100 ROWS
      WHERE kostl IN i_kostl.


    LOOP AT t_csks INTO wa_csks.
      MOVE wa_csks-kostl TO searchresult-value2.
      MOVE wa_csks-bukrs TO searchresult-value3.
      APPEND searchresult.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM searchresult
        COMPARING value2.

  ELSEIF field EQ 'SAKNR'. " GL account

* Configuration check
    SELECT SINGLE val1 FROM /nrk/apayconfig INTO configvalue
      WHERE key1 = 'APAY'
        AND key2 = 'SAKNR'.

    IF configvalue NE 'X'.

      SELECT SINGLE bukrs FROM /nrk/apayhd INTO l_bukrs
        WHERE apayno EQ apayno.

      IF sy-subrc EQ 0.

*       CALL FUNCTION 'FSC_GET_CHART_OF_ACCOUNTS'
*         EXPORTING
*           x_bukrs      = l_bukrs
*         IMPORTING
*           y_ktopl      = l_ktopl
*         EXCEPTIONS
*           not_found    = 1
*           system_error = 2
*           OTHERS       = 3.

        SELECT SINGLE ktopl FROM t001 INTO l_ktopl
          WHERE bukrs EQ l_bukrs.

        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        IF exactmatch NE 'X'.

          wa_saknr-sign = 'I'.
          wa_saknr-option = 'CP'.
          CONCATENATE '*' value '*' INTO wa_saknr-low.
          APPEND wa_saknr TO i_saknr.

          SELECT * FROM skb1
            INTO CORRESPONDING FIELDS OF TABLE t_skb1
            UP TO 10 ROWS
            WHERE saknr IN i_saknr
            AND bukrs EQ l_bukrs.

          LOOP AT t_skb1 INTO wa_skb1.

            SELECT SINGLE * FROM skat INTO wa_skat
              WHERE ktopl EQ l_ktopl
                AND spras EQ sy-langu
                AND saknr EQ wa_skb1-saknr.

            MOVE wa_skb1-saknr TO searchresult-value1.
            MOVE wa_skat-txt20 TO searchresult-value2.
            APPEND searchresult.
          ENDLOOP.

        ELSE. " exact match

          MOVE value TO l_saknr.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_saknr
            IMPORTING
              output = l_saknr.

          SELECT SINGLE * FROM skb1 INTO wa_skb1
            WHERE saknr EQ l_saknr
            AND bukrs EQ l_bukrs.

          IF sy-subrc NE 0. " GL not found
            messages-msg_type = 'E'.
            messages-msg_nbr = '080'.
            messages-msg_text = text-080.
            APPEND messages.
            EXIT.
          ENDIF.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.

    ENDIF.

  ELSEIF field EQ 'AUFNR'. " Internal order
    IF exactmatch NE 'X'. " order suggestions

      wa_aufnr-sign = 'I'.
      wa_aufnr-option = 'CP'.
      CONCATENATE '*' value '*' INTO wa_aufnr-low.
      APPEND wa_aufnr TO i_aufnr.

      SELECT * FROM aufk
        INTO CORRESPONDING FIELDS OF TABLE t_aufk
        UP TO 10 ROWS
        WHERE aufnr IN i_aufnr.

      LOOP AT t_aufk INTO wa_aufk.
        MOVE wa_aufk-aufnr TO searchresult-value1.
        MOVE wa_aufk-ktext TO searchresult-value2.
        APPEND searchresult.
      ENDLOOP.

    ELSE. " order exact match

      MOVE value TO l_aufnr.

      SELECT SINGLE * FROM aufk INTO wa_aufk
        WHERE aufnr EQ l_aufnr.

      IF sy-subrc NE 0. " Order not found
        messages-msg_type = 'E'.
        messages-msg_nbr = '081'.
        messages-msg_text = text-081.
        APPEND messages.
        EXIT.
      ENDIF.

    ENDIF.
  ELSEIF field EQ 'PSPNR'. " WBS

    IF exactmatch NE 'X'. " WBS suggestions

      wa_posid-sign = 'I'.
      wa_posid-option = 'CP'.
      CONCATENATE '*' value '*' INTO wa_posid-low.
      APPEND wa_posid TO i_posid.

      SELECT * FROM prps
        INTO CORRESPONDING FIELDS OF TABLE t_prps
        UP TO 10 ROWS
        WHERE posid IN i_posid.

      LOOP AT t_prps INTO wa_prps.
        MOVE wa_prps-posid TO searchresult-value1.
        MOVE wa_prps-post1 TO searchresult-value2.
        APPEND searchresult.
      ENDLOOP.

    ELSE. " WBS exact match

      MOVE value TO l_posid.

      SELECT SINGLE * FROM prps
        INTO wa_prps
        WHERE posid EQ l_posid.

      IF sy-subrc NE 0. " Order not found
        messages-msg_type = 'E'.
        messages-msg_nbr = '082'.
        messages-msg_text = text-082.
        APPEND messages.
        EXIT.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFUNCTION.
