FUNCTION /nrk/apay_change_parked_doc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"     REFERENCE(KOSTL) TYPE  KOSTL OPTIONAL
*"     REFERENCE(HEADER) TYPE  /NRK/APAYHD OPTIONAL
*"  EXPORTING
*"     REFERENCE(CHANGED) TYPE  BOOLE-BOOLE
*"  TABLES
*"      T_ITEMS STRUCTURE  /NRK/APAYITEMS
*"      T_MESSAGES STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      NO_APAY_RECORD_ID
*"      PARKED_DOCUMENT_NOT_CHANGED
*"----------------------------------------------------------------------

* Local fields
  DATA: lv_line      TYPE n LENGTH 2,
        lv_field     TYPE fnam_____4,
        lv_wrbtr     TYPE c LENGTH 13,
        p_i_bdcdata  TYPE bdcdata OCCURS 0 WITH HEADER LINE,
        p_i_messages TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
        wa_header    TYPE /nrk/apayhd,
        wa_items     LIKE /nrk/apayitems,
        l_wrbtr      TYPE c LENGTH 13,
        t_vbsegk     LIKE vbsegk OCCURS 0,
        t_vbsegs     LIKE vbsegs OCCURS 0,
        wa_vbsegk    LIKE vbsegk,
        wa_vbsegs    LIKE vbsegs,
        wa_bkpf      LIKE bkpf,
        wa_lfa1      LIKE lfa1,
        wa_lfb1      LIKE lfb1.

* Make sure APay record exists
  IF apayno IS INITIAL.
    RAISE no_apay_record_id.
  ENDIF.

* Fill initial screen
  SET PARAMETER ID 'BUK' FIELD header-bukrs.
  SET PARAMETER ID 'GJR' FIELD header-gjahr.

  PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPMF05V' '0100'.
  PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_OKCODE' '/00'.

  PERFORM bdc_field  TABLES p_i_bdcdata USING 'RF05V-BELNR' header-belnr.

* Fill bdc header screen
  PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPMF05A' '1100'.
  PERFORM bdc_field TABLES p_i_bdcdata USING  'BDC_SUBSCR' 'SAPLFDCB'.
  PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_OKCODE' '/00'.

  PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPMF05A' '1100'.
  PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_OKCODE' '/00'.

* Now process items
  LOOP AT t_items INTO wa_items.
* Enter line
    lv_line = lv_line + 1.

* Fill bdc item screen
    PERFORM bdc_write_item TABLES p_i_bdcdata USING lv_line wa_items header.

* After three lines, enter new lines
    IF lv_line EQ '03'.
* Select first line
      lv_line = '01'.
* Add new lines
      PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPMF05A'   '1100'.
      PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_OKCODE' '=0006'.
      CONCATENATE 'ACGL_ITEM-STATE(' lv_line ')' INTO lv_field.
      PERFORM bdc_field TABLES p_i_bdcdata USING 'BDC_CURSOR' lv_field.
      CONCATENATE 'ACGL_ITEM-MARKSP(' lv_line ')' INTO lv_field.
      PERFORM bdc_field TABLES p_i_bdcdata USING lv_field 'X'.
* Reset line value
      CLEAR lv_line.
    ENDIF.

* Hit enter
    PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPMF05A' '1100'.
    PERFORM bdc_field  TABLES p_i_bdcdata USING 'BDC_OKCODE' '/00'.

  ENDLOOP.

* End bdc command with complete
  PERFORM bdc_dynpro TABLES p_i_bdcdata USING 'SAPMF05A' '1100'.
  PERFORM bdc_field TABLES p_i_bdcdata USING 'BDC_OKCODE' '=BP'.

* Call transaction
  CALL TRANSACTION 'FBV2' USING     p_i_bdcdata
                           MODE     'N'
*                           mode     'A'
                           UPDATE   'S'
                           MESSAGES INTO p_i_messages.

  READ TABLE p_i_messages WITH KEY msgtyp = 'S'
                                   msgid = 'FP'
                                   msgnr = '029'.

  IF sy-subrc NE 0.
* No change message, try again
    READ TABLE p_i_messages WITH KEY msgtyp = 'S'
                                   msgid = 'FP'
                                   msgnr = '092'.

    IF sy-subrc NE 0.
* Document not changed
      MOVE p_i_messages[] TO t_messages[].
      CLEAR changed.
    ELSE.
* Document changed
      changed = 'X'.

* Update APay header and line item
      MOVE header TO wa_header.

      WAIT UP TO 3 SECONDS.

      SELECT SINGLE * FROM bkpf INTO wa_bkpf
        WHERE bukrs EQ header-bukrs
          AND belnr EQ header-belnr
          AND gjahr EQ header-gjahr.

      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING wa_bkpf TO wa_header.
      ENDIF.

      SELECT * FROM vbsegk INTO TABLE t_vbsegk
        WHERE ausbk = header-bukrs
          AND belnr = header-belnr
          AND gjahr = header-gjahr.

      IF sy-subrc EQ 0.
        CLEAR: t_items[], wa_items.

* Update header
        READ TABLE t_vbsegk INDEX 1 INTO wa_vbsegk.

        IF NOT wa_vbsegk-wrbtr IS INITIAL.
          wa_header-wrbtr = wa_vbsegk-wrbtr.
        ENDIF.
        IF NOT wa_vbsegk-wmwst IS INITIAL.
          wa_header-wmwst = wa_vbsegk-wmwst.
        ENDIF.

        IF wa_vbsegk-bschl = '21'.
          wa_header-shkzg = 'H'.
        ELSEIF wa_vbsegk-bschl = '31'.
          wa_header-shkzg = 'S'.
        ENDIF.

        IF wa_vbsegk-zfbdt IS INITIAL.
          wa_vbsegk-zfbdt = wa_bkpf-budat.
        ENDIF.
        IF wa_vbsegk-zbd1t IS INITIAL.
          wa_header-duedate = wa_vbsegk-zfbdt.
        ELSE.
          wa_header-duedate = wa_vbsegk-zfbdt + wa_vbsegk-zbd1t.
        ENDIF.

        wa_header-lifnr = wa_vbsegk-lifnr.

        CALL FUNCTION 'VENDOR_READ'
          EXPORTING
            i_bukrs   = wa_header-bukrs
            i_lifnr   = wa_header-lifnr
          IMPORTING
            e_lfa1    = wa_lfa1
            e_lfb1    = wa_lfb1
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.

        IF sy-subrc <> 0.
          RAISE no_vendor_found.
        ENDIF.

        wa_header-stras = wa_lfa1-stras.
        wa_header-pstlz = wa_lfa1-pstlz.
        wa_header-ort01 = wa_lfa1-ort01.
        wa_header-land1 = wa_lfa1-land1.
        wa_header-lifname = wa_lfa1-name1.
        TRANSLATE wa_header-lifname TO UPPER CASE.

        wa_header-zsabe = wa_lfb1-zsabe.
        wa_header-zahls = wa_lfb1-zahls.
        wa_header-zterm = wa_lfb1-zterm.
        wa_header-busab = wa_lfb1-busab.

        IF NOT wa_header-zterm IS INITIAL.
          SELECT SINGLE ztag1 FROM t052
            INTO wa_header-ztag1 WHERE zterm = wa_header-zterm.

          IF header-ztag1 IS NOT INITIAL.
            wa_header-duedate = wa_header-bldat + wa_header-ztag1.
          ENDIF.
        ENDIF.

        SELECT * FROM vbsegs INTO TABLE t_vbsegs
          WHERE ausbk = header-bukrs
            AND belnr = header-belnr
            AND gjahr = header-gjahr.

        IF sy-subrc EQ 0.
          LOOP AT t_vbsegs INTO wa_vbsegs.
            MOVE-CORRESPONDING wa_vbsegs TO wa_items.
            wa_items-apayno = header-apayno.
*           wa_items-projk = wa_vbsegs-ps_psp_pnr.
            MOVE wa_vbsegs-saknr TO wa_items-hkont.
            IF wa_items-projk IS INITIAL.
              wa_items-projk = space.
            ENDIF.
            APPEND wa_items TO t_items.
          ENDLOOP.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  IF NOT t_items[] IS INITIAL AND changed EQ 'X'.
* Delete current line items
    DELETE FROM /nrk/apayitems WHERE apayno EQ wa_header-apayno.
* add new line items.
    INSERT  /nrk/apayitems FROM TABLE t_items.
    UPDATE /nrk/apayhd FROM wa_header.
  ENDIF.

ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  BDC_WRITE_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_write_item TABLES pp_i_bdcdata STRUCTURE bdcdata
                     USING plv_line
                           pwa_items STRUCTURE  /nrk/apayitems
                           pheader   STRUCTURE /nrk/apayhd.

  DATA: plv_field TYPE fnam_____4,
        pl_wrbtr  TYPE c LENGTH 13.

* GL account
  IF NOT pwa_items-hkont IS INITIAL.
    CONCATENATE 'ACGL_ITEM-HKONT(' plv_line ')' INTO plv_field.
    PERFORM bdc_field  TABLES pp_i_bdcdata USING plv_field pwa_items-hkont.
  ENDIF.
* Debit/Credit indicator
  IF pwa_items-shkzg = 'C'.
    CONCATENATE 'ACGL_ITEM-SHKZG(' plv_line ')' INTO plv_field.
    PERFORM bdc_field  TABLES pp_i_bdcdata USING plv_field 'H'.
  ELSE.
    CONCATENATE 'ACGL_ITEM-SHKZG(' plv_line ')' INTO plv_field.
    PERFORM bdc_field  TABLES pp_i_bdcdata USING plv_field 'S'.
  ENDIF.
* Amount
  IF NOT pwa_items-wrbtr IS INITIAL.
    CLEAR pl_wrbtr.
    WRITE pwa_items-wrbtr TO pl_wrbtr.
    CONCATENATE 'ACGL_ITEM-WRBTR(' plv_line ')' INTO plv_field.
    PERFORM bdc_field  TABLES pp_i_bdcdata USING plv_field pl_wrbtr.
  ENDIF.
* Tax code
  IF NOT pwa_items-mwskz IS INITIAL.
    CONCATENATE 'ACGL_ITEM-MWSKZ(' plv_line ')' INTO plv_field.
    PERFORM bdc_field  TABLES pp_i_bdcdata USING plv_field pwa_items-mwskz.
  ELSE.
    IF NOT pheader-mwskz IS INITIAL.
      CONCATENATE 'ACGL_ITEM-MWSKZ(' plv_line ')' INTO plv_field.
      PERFORM bdc_field  TABLES pp_i_bdcdata USING plv_field pheader-mwskz.
    ENDIF.
  ENDIF.
* Text
  IF NOT pwa_items-sgtxt IS INITIAL.
    IF NOT pwa_items-sgtxt EQ 'undefined'
      AND NOT pwa_items-sgtxt EQ 'null'
      AND NOT pwa_items-sgtxt IS INITIAL.
      CONCATENATE 'ACGL_ITEM-SGTXT(' plv_line ')' INTO plv_field.
      PERFORM bdc_field  TABLES pp_i_bdcdata USING plv_field pwa_items-sgtxt.
    ENDIF.
  ENDIF.
* Company code
  IF NOT pwa_items-bukrs IS INITIAL.
    CONCATENATE 'ACGL_ITEM-BUKRS(' plv_line ')' INTO plv_field.
    PERFORM bdc_field  TABLES pp_i_bdcdata USING plv_field pwa_items-bukrs.
  ENDIF.
* Cost center
  IF NOT pwa_items-kostl IS INITIAL.
    IF NOT pwa_items-kostl EQ 'undefined'
      AND NOT pwa_items-kostl EQ 'null'
      AND NOT pwa_items-kostl IS INITIAL.
      CONCATENATE 'ACGL_ITEM-KOSTL(' plv_line ')' INTO plv_field.
      PERFORM bdc_field TABLES pp_i_bdcdata USING plv_field pwa_items-kostl.
    ENDIF.
  ENDIF.
* Material
  IF NOT pwa_items-matnr IS INITIAL.
    IF NOT pwa_items-matnr EQ 'undefined'
      AND NOT pwa_items-matnr EQ 'null'
      AND NOT pwa_items-matnr IS INITIAL.
      CONCATENATE 'ACGL_ITEM-MATNR(' plv_line ')' INTO plv_field.
      PERFORM bdc_field  TABLES pp_i_bdcdata USING plv_field pwa_items-matnr.
    ENDIF.
  ENDIF.
* WBS
  IF NOT pwa_items-projk IS INITIAL.
    IF NOT pwa_items-projk EQ 'undefined'
      AND NOT pwa_items-projk EQ 'null'
      AND NOT pwa_items-projk IS INITIAL.
      CONCATENATE 'ACGL_ITEM-PROJK(' plv_line ')' INTO plv_field.
      PERFORM bdc_field  TABLES pp_i_bdcdata USING plv_field pwa_items-projk.
    ENDIF.
  ENDIF.
* Assignment
  IF NOT pwa_items-zuonr IS INITIAL.
    IF NOT pwa_items-zuonr EQ 'undefined'
      AND NOT pwa_items-zuonr EQ 'null'
      AND NOT pwa_items-zuonr IS INITIAL.
      CONCATENATE 'ACGL_ITEM-ZUONR(' plv_line ')' INTO plv_field.
      PERFORM bdc_field  TABLES pp_i_bdcdata USING plv_field pwa_items-zuonr.
    ENDIF.
  ENDIF.
* Internal order
  IF NOT pwa_items-aufnr IS INITIAL.
    IF NOT pwa_items-aufnr EQ 'undefined'
      AND NOT pwa_items-aufnr EQ 'null'
      AND NOT pwa_items-aufnr IS INITIAL.
      CONCATENATE 'ACGL_ITEM-AUFNR(' plv_line ')' INTO plv_field.
      PERFORM bdc_field  TABLES pp_i_bdcdata USING plv_field pwa_items-aufnr.
    ENDIF.
  ENDIF.

ENDFORM.                    " BDC_WRITE_ITEM
