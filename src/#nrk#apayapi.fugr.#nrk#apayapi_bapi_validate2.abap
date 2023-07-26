FUNCTION /nrk/apayapi_bapi_validate2.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FIELD) TYPE  CHAR50 OPTIONAL
*"     VALUE(VALUE) TYPE  CHAR50 OPTIONAL
*"     VALUE(EXACTMATCH) TYPE  BOOLE OPTIONAL
*"     VALUE(FILTER) TYPE  CHAR5 OPTIONAL
*"  TABLES
*"      SEARCHRESULT STRUCTURE  /NRK/APAYAPISEARCH OPTIONAL
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"      DATA STRUCTURE  /NRK/APAYAPIDATA OPTIONAL
*"----------------------------------------------------------------------

  DATA: l_bukrs      LIKE /nrk/apayhd-bukrs,
        l_wrbtr      LIKE /nrk/apayhd-wrbtr,
        l_ebeln      LIKE /nrk/apayhd-ebeln,
        l_waers      LIKE /nrk/apayhd-waers,
        l_lifnr      LIKE /nrk/apayhd-lifnr,
        l_approver   LIKE /nrk/apayhd-ext_approver,
        l_objecttext LIKE toasp-objecttext,
        l_ar_object  LIKE /nrk/apayhd-ar_object,
        l_lifname    LIKE lfa1-name1.

  DATA: t_lfa1     LIKE lfa1 OCCURS 0,
        wa_lfa1    LIKE lfa1,
        t_ekko     LIKE ekko OCCURS 0,
        wa_ekko    LIKE ekko,
        t_bukrs    LIKE t001 OCCURS 0 WITH HEADER LINE,
        wa_user    LIKE /nrk/apayuser,
        t_user     LIKE /nrk/apayuser OCCURS 0,
        t_doctype  LIKE /nrk/apaydtype OCCURS 0,
        wa_doctype LIKE /nrk/apaydtype,
        wa_t001    LIKE t001.

  DATA:  i_lifnr      TYPE RANGE OF lifnr INITIAL SIZE 0,
         wa_lifnr     LIKE LINE OF  i_lifnr,
         i_ebeln      TYPE RANGE OF ebeln INITIAL SIZE 0,
         wa_ebeln     LIKE LINE OF i_ebeln,
         i_bukrs      TYPE RANGE OF bukrs INITIAL SIZE 0,
         wa_bukrs     LIKE LINE OF i_bukrs,
         i_approver   TYPE RANGE OF /nrk/apayexuser INITIAL SIZE 0,
         wa_approver  LIKE LINE OF i_approver,
         i_ar_object  TYPE RANGE OF saeobjart INITIAL SIZE 0,
         wa_ar_object LIKE LINE OF i_ar_object.

  DATA: not_authorized LIKE boole-boole.

  CLEAR: l_bukrs,
         l_ebeln,
         l_waers,
         l_lifnr,
         wa_ekko,
         wa_t001.

  IF exactmatch EQ 'X'.
    IF field EQ 'BUKRS'. " Company code

      MOVE value TO l_bukrs.
      SELECT SINGLE bukrs FROM t001 INTO wa_t001
        WHERE bukrs EQ l_bukrs.
      IF sy-subrc EQ 0. " Company code validated
        messages-msg_type = 'I'.
        messages-msg_nbr = '051'.
        messages-msg_text = text-051.
        APPEND messages.
*** Pull currency from company code
        data-field = 'WAERS'.
        MOVE wa_t001-waers TO data-value.
        APPEND data.

      ELSE. " Company code doesn't exist
        messages-msg_type = 'E'.
        messages-msg_nbr = '052'.
        messages-msg_text = text-052.
        APPEND messages.
      ENDIF.

* Validate authorization
      IF filter = 'BUKRS'.
        CALL FUNCTION '/NRK/APAYAPI_AUTHORIZE_BUKRS'
          EXPORTING
            bukrs          = l_bukrs
          IMPORTING
            not_authorized = not_authorized.

        IF not_authorized EQ 'X'.
          messages-msg_type = 'E'.
          messages-msg_nbr = '084'.
          messages-msg_text = text-084.
          APPEND messages.
        ENDIF.

      ENDIF.

    ELSEIF field EQ 'EBELN'. " Purchase order

      MOVE value TO l_ebeln.
      SELECT SINGLE * FROM ekko INTO wa_ekko
        WHERE ebeln EQ l_ebeln.

      IF sy-subrc EQ 0. " Purchase order validated
        messages-msg_type = 'I'.
        messages-msg_nbr = '053'.
        messages-msg_text = text-053.
        APPEND messages.
* Pull company code from PO and pass to capture
        data-field = 'BUKRS'.
        MOVE wa_ekko-bukrs TO data-value.
        APPEND data.
* Pull vendor from PO and pass to capture
        data-field = 'LIFNR'.
        MOVE wa_ekko-lifnr TO data-value.
        APPEND data.
* Pull vendor name
        SELECT SINGLE name1 FROM lfa1 INTO l_lifname
          WHERE lifnr EQ wa_ekko-lifnr.
        IF sy-subrc EQ 0.
          data-field = 'LIFNAME'.
          MOVE l_lifname TO data-value.
          APPEND data.
        ENDIF.
* Pull currency from capture
        data-field = 'WAERS'.
        MOVE wa_ekko-waers TO data-value.
        APPEND data.
      ELSE. " Purchase order doesn't exist
        messages-msg_type = 'E'.
        messages-msg_nbr = '054'.
        messages-msg_text = text-054.
        APPEND messages.
      ENDIF.

    ELSEIF field EQ 'WAERS'. " Currency

      MOVE value TO l_waers.
      SELECT SINGLE waers FROM t500w INTO l_waers
        WHERE waers EQ l_waers.
      IF sy-subrc EQ 0. " Currency validated
        messages-msg_type = 'I'.
        messages-msg_nbr = '055'.
        messages-msg_text = text-055.
        APPEND messages.
      ELSE. " Currency invalid
        messages-msg_type = 'E'.
        messages-msg_nbr = '056'.
        messages-msg_text = text-056.
        APPEND messages.
      ENDIF.

    ELSEIF field EQ 'LIFNR'. " Vendor number

      MOVE value TO l_lifnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = l_lifnr
        IMPORTING
          output = l_lifnr.

      SELECT SINGLE lifnr FROM lfa1 INTO l_lifnr
        WHERE lifnr EQ l_lifnr.
      IF sy-subrc EQ 0. " Vendor validated
        messages-msg_type = 'I'.
        messages-msg_nbr = '057'.
        messages-msg_text = text-057.
        APPEND messages.
* Pass possible company codes to capture
      ELSE. " Vendor invalid
        messages-msg_type = 'E'.
        messages-msg_nbr = '058'.
        messages-msg_text = text-058.
        APPEND messages.
      ENDIF.

    ELSEIF field EQ 'EXTUSER'. " Approver

* Check for import values
      READ TABLE data WITH KEY field = 'BUKRS'.

      IF sy-subrc EQ 0.
        MOVE data-value TO l_bukrs.
      ENDIF.

      READ TABLE data WITH KEY field = 'WRBTR'.

      IF sy-subrc EQ 0.
        MOVE data-value TO l_wrbtr.
      ENDIF.

      READ TABLE data WITH KEY field = 'LIFNR'.

      IF sy-subrc EQ 0.
        MOVE data-value TO l_lifnr.
      ENDIF.

      MOVE value TO l_approver.

      IF NOT l_bukrs IS INITIAL
        AND NOT l_wrbtr IS INITIAL.

        SELECT * FROM /nrk/apayuser
        INTO CORRESPONDING FIELDS OF TABLE t_user
        WHERE extuser EQ l_approver
          AND approver EQ 'X'
          AND bukrs EQ l_bukrs
          AND wrbtr GT l_wrbtr.

      ELSEIF NOT l_bukrs IS INITIAL
        AND NOT l_lifnr IS INITIAL.

        SELECT * FROM /nrk/apayuser
        INTO CORRESPONDING FIELDS OF TABLE t_user
        WHERE extuser EQ l_approver
          AND approver EQ 'X'
          AND bukrs EQ l_bukrs
          AND lifnr EQ l_lifnr.

      ELSE.

        SELECT * FROM /nrk/apayuser
        INTO CORRESPONDING FIELDS OF TABLE t_user
        WHERE extuser EQ l_approver
          AND approver EQ 'X'.

      ENDIF.

      IF sy-subrc NE 0.
        messages-msg_type = 'E'.
        messages-msg_nbr = '059'.
        messages-msg_text = text-059.
        APPEND messages.
      ELSE.
        messages-msg_type = 'I'.
        messages-msg_nbr = '060'.
        messages-msg_text = text-060.
        APPEND messages.
      ENDIF.

    ELSEIF field EQ 'AR_OBJECT'. " Document type

      SELECT * FROM /nrk/apaydtype
        INTO CORRESPONDING FIELDS OF TABLE t_doctype
        WHERE ar_object EQ l_ar_object.

      IF sy-subrc NE 0.
        messages-msg_type = 'E'.
        messages-msg_nbr = '059'.
        messages-msg_text = text-059.
        APPEND messages.
      ELSE.
        messages-msg_type = 'I'.
        messages-msg_nbr = '060'.
        messages-msg_text = text-060.
        APPEND messages.
      ENDIF.

    ENDIF.
  ELSE. " Provide list of search results

    IF field EQ 'BUKRS'. " Company code

      wa_bukrs-sign = 'I'.
      wa_bukrs-option = 'CP'.
      CONCATENATE value '*' INTO wa_bukrs-low.
      APPEND wa_bukrs TO i_bukrs.

      SELECT * FROM t001
        INTO CORRESPONDING FIELDS OF TABLE t_bukrs
        WHERE bukrs IN i_bukrs.
*       and langu eq sy-langu.

      IF filter EQ 'BUKRS'.
        LOOP AT t_bukrs.
          CALL FUNCTION '/NRK/APAYAPI_AUTHORIZE_BUKRS'
            EXPORTING
              bukrs          = t_bukrs-bukrs
            IMPORTING
              not_authorized = not_authorized.

          IF not_authorized NE 'X'.
            MOVE t_bukrs-bukrs TO searchresult-value1.
            MOVE t_bukrs-butxt TO searchresult-value2.
            APPEND searchresult.
          ENDIF.
        ENDLOOP.
      ELSE.
        LOOP AT t_bukrs.
          MOVE t_bukrs-bukrs TO searchresult-value1.
          MOVE t_bukrs-butxt TO searchresult-value2.
          APPEND searchresult.
        ENDLOOP.
      ENDIF.

    ELSEIF field EQ 'EBELN'. " Purchase order

      wa_ebeln-sign = 'I'.
      wa_ebeln-option = 'CP'.
      CONCATENATE '*' value '*' INTO wa_ebeln-low.
      APPEND wa_ebeln TO i_ebeln.

      SELECT * FROM ekko
        INTO CORRESPONDING FIELDS OF TABLE t_ekko
        WHERE ebeln IN i_ebeln.

      LOOP AT t_ekko INTO wa_ekko.
        MOVE wa_ekko-ebeln TO searchresult-value1.
        MOVE wa_ekko-lifnr TO searchresult-value2.
        APPEND searchresult.
      ENDLOOP.

    ELSEIF field EQ 'LIFNR'. " Vendor number

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

*   ELSEIF field EQ 'APPROVER'. " Approver
    ELSEIF field EQ 'EXTUSER'. " Approver

* Check for import values
      READ TABLE data WITH KEY field = 'BUKRS'.

      IF sy-subrc EQ 0.
        MOVE data-value TO l_bukrs.
      ENDIF.

      READ TABLE data WITH KEY field = 'WRBTR'.

      IF sy-subrc EQ 0.
        MOVE data-value TO l_wrbtr.
      ENDIF.

      wa_approver-sign = 'I'.
      wa_approver-option = 'CP'.
      CONCATENATE '*' value '*' INTO wa_approver-low.
      APPEND wa_approver TO i_approver.

      IF NOT l_bukrs IS INITIAL
        AND NOT l_wrbtr IS INITIAL.

        SELECT * FROM /nrk/apayuser
          INTO CORRESPONDING FIELDS OF TABLE t_user
          WHERE extuser IN i_approver
            AND approver EQ 'X'
            AND bukrs EQ l_bukrs
            AND wrbtr GT l_wrbtr.

      ELSEIF NOT l_bukrs IS INITIAL
        AND l_wrbtr IS INITIAL.

        SELECT * FROM /nrk/apayuser
          INTO CORRESPONDING FIELDS OF TABLE t_user
          WHERE extuser IN i_approver
            AND approver EQ 'X'
            AND bukrs EQ l_bukrs.

      ELSE. " Get default user

        SELECT * FROM /nrk/apayuser
          INTO CORRESPONDING FIELDS OF TABLE t_user
          WHERE extuser IN i_approver
            AND approver EQ 'X'.

      ENDIF.

      LOOP AT t_user INTO wa_user.
        MOVE wa_user-extuser TO searchresult-value1.
        MOVE wa_user-fullname TO searchresult-value2.
        APPEND searchresult.
      ENDLOOP.

    ELSEIF field EQ 'AR_OBJECT'. " document type

      wa_ar_object-sign = 'I'.
      wa_ar_object-option = 'CP'.
      CONCATENATE '*' value '*' INTO wa_ar_object-low.
      APPEND wa_ar_object TO i_ar_object.

      SELECT * FROM /nrk/apaydtype
        INTO CORRESPONDING FIELDS OF TABLE t_doctype
        WHERE ar_object IN i_ar_object.

      LOOP AT t_doctype INTO wa_doctype.
        MOVE wa_doctype-ar_object TO searchresult-value1.

        SELECT SINGLE objecttext FROM toasp
          INTO l_objecttext
          WHERE ar_object EQ wa_doctype-ar_object
            AND language EQ sy-langu.

        IF sy-subrc EQ 0.
          MOVE l_objecttext TO searchresult-value2.
        ENDIF.

        APPEND searchresult.
      ENDLOOP.

    ENDIF.

  ENDIF.

ENDFUNCTION.
