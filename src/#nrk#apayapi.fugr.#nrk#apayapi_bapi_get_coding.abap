FUNCTION /nrk/apayapi_bapi_get_coding.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYNO) LIKE  /NRK/APAYHD-APAYNO OPTIONAL
*"     VALUE(USER) LIKE  /NRK/APAYUSER-EXTUSER OPTIONAL
*"     VALUE(BALANCESHEET) TYPE  BOOLE OPTIONAL
*"  EXPORTING
*"     VALUE(CODER) LIKE  BOOLE STRUCTURE  BOOLE
*"     VALUE(APPROVER) LIKE  BOOLE STRUCTURE  BOOLE
*"     VALUE(APAYHD) LIKE  /NRK/APAYAPI_HD STRUCTURE  /NRK/APAYAPI_HD
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"      ITEMS STRUCTURE  /NRK/APAYITEMS OPTIONAL
*"      REJECTIONREASON STRUCTURE  /NRK/APAYAPISEARCH OPTIONAL
*"----------------------------------------------------------------------

  DATA: t_item       LIKE /nrk/apayitems OCCURS 0,
        t_exception  LIKE /nrk/apayedef OCCURS 0.

  DATA: wa_hd        LIKE /nrk/apayhd,
        wa_item      LIKE /nrk/apayitems,
        wa_exception LIKE /nrk/apayedef,
        wa_user      LIKE /nrk/apayuser,
        wa_approver  LIKE /nrk/apayappr,
        t_approver   LIKE /nrk/apayappr OCCURS 0,
        wa_ska1      LIKE ska1,
        bs_indicator LIKE boole-boole,
        pitem        LIKE /nrk/apayappr-item,
        approved(1)  TYPE c.

  DATA: lv_count       TYPE sy-index,
        lt_connections TYPE STANDARD TABLE OF toav0,
        ls_connections TYPE                   toav0,
        object_id      LIKE toav0-object_id,
        wa_toaom       LIKE toaom,
        wa_toasp       LIKE toasp.

* Check coding and approval authorization
  TRANSLATE user TO UPPER CASE.

  SELECT SINGLE * FROM /nrk/apayuser INTO wa_user
    WHERE extuser = user.

  IF wa_user-coder EQ 'X'.
    coder = 'X'.
  ENDIF.

  IF wa_user-approver EQ 'X'.
    approver = 'X'.
  ENDIF.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayno.

  MOVE-CORRESPONDING wa_hd TO apayhd.

  IF apayhd-shkzg = 'H'.
    apayhd-shkzg = 'C'.
  ELSEIF apayhd-shkzg = 'S'.
    apayhd-shkzg = 'D'.
  ENDIF.

* get all approver from table
  TRANSLATE user TO UPPER CASE.

  SELECT * FROM /nrk/apayappr INTO TABLE t_approver
    WHERE apayno EQ wa_hd-apayno
      AND approved EQ space
      AND ext_user EQ user.

  IF sy-subrc EQ 0.

    LOOP AT t_approver INTO wa_approver.
      IF wa_approver-item = 1.
        approver = 'X'.
      ELSE.
        pitem = wa_approver-item - 1.
        SELECT SINGLE approved FROM /nrk/apayappr INTO approved
          WHERE apayno EQ wa_approver-apayno
            AND item = pitem.

        IF approved EQ 'A'.
          approver = 'X'.
        ELSE.
          approver = ' '.
        ENDIF.
      ENDIF.

      CLEAR: approved,
             pitem.

    ENDLOOP.

* begin old code
** Get 1st approver, who has not approved yet.
*    CLEAR: wa_approver.
*    READ TABLE t_approver WITH KEY approved = ' '
*      INTO wa_approver.
*
*    IF wa_approver-ext_user EQ user.
*      approver = 'X'.
*    ELSE.
*      approver = ' '.
*    ENDIF.
* end old code

  ELSE.

*** no approver found in hierachy

  ENDIF.

* commented out 03/28/2017
*  IF NOT wa_hd-belnr IS INITIAL
*   AND NOT wa_hd-bukrs IS INITIAL
*   AND NOT wa_hd-gjahr IS INITIAL. " Acc Doc exists
*
*    SELECT SINGLE bstat FROM bkpf INTO wa_hd-bstat
*      WHERE belnr EQ wa_hd-belnr
*        AND gjahr EQ wa_hd-gjahr
*        AND bukrs EQ wa_hd-bukrs.
*
*    IF sy-subrc EQ 0.
*      coder = ' '.
*    ENDIF.
*
*  ENDIF.
* commented out 03/28/2017

* Get status description
  SELECT SINGLE sdescr FROM /nrk/apaysdef INTO apayhd-sdescr
    WHERE status EQ apayhd-status.

* Get document type description
  SELECT SINGLE objecttext FROM toasp INTO apayhd-objecttext
    WHERE ar_object EQ wa_hd-ar_object
      AND language EQ sy-langu.

* Get line items
  SELECT * FROM /nrk/apayitems INTO TABLE t_item
      WHERE apayno EQ apayno.

  IF sy-subrc EQ 0.
    coder = ' '.
  ENDIF.

  LOOP AT t_item INTO wa_item.

    CLEAR: wa_ska1.

    MOVE-CORRESPONDING wa_item TO items.

    IF items-shkzg = 'H'.
      items-shkzg = 'C'.
    ELSEIF items-shkzg = 'S'.
      items-shkzg = 'D'.
    ENDIF.

*** start - new conversation routine 10/05/2015
*** start - added back in 09/11/14
** Convert WBS
*    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*      EXPORTING
*        input  = items-projk
*      IMPORTING
*        output = items-projk.
*** end
* Convert WBS
*    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
*      EXPORTING
*        input  = items-projk
*      IMPORTING
*        output = items-projk.
*** end - new conversation routine 10/05/2015
*** start new conversion routine 12/30/2015

    IF NOT items-pspnr IS INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = items-pspnr
        IMPORTING
          output = items-projk.

    ENDIF.


*** end new conversion routine 12/30/2015

    SELECT SINGLE * FROM ska1 INTO wa_ska1
      WHERE ktopl EQ 'INT'
        AND saknr EQ items-hkont.

    IF sy-subrc EQ 0 AND wa_ska1-xbilk EQ 'X'. " balance sheet account
      bs_indicator = 'X'.
    ELSE. " Not a balance sheet account
      bs_indicator = ' '.
    ENDIF.

    APPEND items.
  ENDLOOP.

* Get rejection reasons
  SELECT * FROM /nrk/apayedef INTO TABLE t_exception
    WHERE pstatus EQ wa_hd-status
      AND langu EQ sy-langu.

  LOOP AT t_exception INTO wa_exception.
    MOVE wa_exception-status TO rejectionreason-value1.
    MOVE wa_exception-sdescr TO rejectionreason-value2.
    APPEND rejectionreason.
  ENDLOOP.

* Get document image
  CLEAR: wa_toaom,
         wa_toasp.

  MOVE wa_hd-apayno TO object_id.
  CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
    EXPORTING
      objecttype    = '/NRK/APAY'
      object_id     = object_id
      client        = sy-mandt
    IMPORTING
      count         = lv_count
    TABLES
      connections   = lt_connections
    EXCEPTIONS
      nothing_found = 1
      OTHERS        = 2.

  IF sy-subrc EQ 0.

*   READ TABLE lt_connections INTO ls_connections INDEX 1.
    READ TABLE lt_connections INTO ls_connections WITH KEY reserve = 'PDF'.
    apayhd-arc_doc_id = ls_connections-arc_doc_id.
    apayhd-archiv_id  = ls_connections-archiv_id.

    SELECT SINGLE * FROM toaom INTO wa_toaom
      WHERE sap_object EQ '/NRK/APAY'
        AND ar_object  EQ ls_connections-ar_object.

    MOVE wa_toaom-doc_type TO apayhd-amime.

    SELECT SINGLE * FROM toasp INTO wa_toasp
      WHERE ar_object EQ ls_connections-ar_object
        AND language EQ sy-langu.

    apayhd-adesc = wa_toasp-objecttext.

  ENDIF.

ENDFUNCTION.
