FUNCTION /nrk/apayapi_bapi_get.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SEARCHCRITERIA) TYPE  /NRK/APAYAPI_SEARCH OPTIONAL
*"  TABLES
*"      APAYHD STRUCTURE  /NRK/APAYAPI_HD OPTIONAL
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"      DOC_LINKS STRUCTURE  /NRK/APAYAPIDOCS OPTIONAL
*"----------------------------------------------------------------------

  DATA: t_user         LIKE /nrk/apayuser OCCURS 0,
        wa_user        LIKE /nrk/apayuser,
        wa_apayhd      LIKE /nrk/apayapi_hd,
        hits           TYPE i,
        hits_c(10)     TYPE c,
        msg_text       LIKE /nrk/apayapi_messages-msg_text,
        approver       LIKE boole-boole,
        t_approver     LIKE /nrk/apayappr OCCURS 0,
        wa_approver    LIKE /nrk/apayappr,
        tabix          LIKE sy-tabix,
        wa_hd          LIKE /nrk/apayapi_hd,
        t_hd           LIKE /nrk/apayapi_hd OCCURS 0,
        object_id      LIKE toav0-object_id,
        t_connections  TYPE toav0 OCCURS 0,
        wa_connections TYPE toav0,
        pitem          LIKE /nrk/apayappr-item,
        approved(1)    TYPE c.

  CLEAR: t_user,t_user[],
         wa_user,
         t_approver, t_approver[],
         wa_approver,
         pitem,
         approved.

  CLEAR: gt_apayno[],
         gt_lifname[],
         gt_lifnr[],
         gt_xblnr[],
         gt_bldat[],
         gt_wrbtr[],
         gt_waers[],
         gt_duedate[],
         gt_budat[],
         gt_belnr[],
         gt_bukrs[],
         gt_gjahr[],
         gt_status[].

* Check user authorization
  SELECT * FROM /nrk/apayuser INTO TABLE t_user
    WHERE extuser EQ searchcriteria-extuser.

  IF sy-subrc NE 0. " Not authorized
    messages-msg_type = 'E'.
    messages-msg_nbr = '001'.
    messages-msg_text = text-001.
    APPEND messages.
    EXIT.
  ELSE.
    READ TABLE t_user WITH KEY approver = 'X' INTO wa_user.
    IF sy-subrc EQ 0.

      approver = 'X'.

    ELSE.
      approver = ' '.
    ENDIF.
  ENDIF.

** Convert vendor number range
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = searchcriteria-lifnrfr
    IMPORTING
      output = searchcriteria-lifnrfr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = searchcriteria-lifnrto
    IMPORTING
      output = searchcriteria-lifnrto.

  PERFORM create_selection_pars USING
     searchcriteria-apaynofr  searchcriteria-apaynoto
     searchcriteria-lifnrfr   searchcriteria-lifnrto
     searchcriteria-lifnamefr searchcriteria-lifnameto
     searchcriteria-xblnrfr   searchcriteria-xblnrto
     searchcriteria-bldatfr   searchcriteria-bldatto
     searchcriteria-wrbtrfr   searchcriteria-wrbtrto
     searchcriteria-waersfr   searchcriteria-waersto
     searchcriteria-duedatefr searchcriteria-duedateto
     searchcriteria-budatfr   searchcriteria-budatto
     searchcriteria-belnrfr   searchcriteria-belnrto
     searchcriteria-bukrsfr   searchcriteria-bukrsto
     searchcriteria-gjahrfr   searchcriteria-gjahrto
     searchcriteria-statusfr  searchcriteria-statusto
     searchcriteria-ebelnfr   searchcriteria-ebelnto.

* Get approval work items, if user is approver
  IF approver EQ 'X'.

* Get all approval records
    TRANSLATE searchcriteria-extuser TO UPPER CASE.

    SELECT * FROM /nrk/apayappr INTO TABLE t_approver
      WHERE apayno IN gt_apayno
        AND ext_user EQ searchcriteria-extuser
        AND approved EQ ' '.

    IF sy-subrc EQ 0.
      CLEAR: gt_apayno.
*   ENDIF.
    ELSE.
* no approver
      messages-msg_type = 'I'.
      messages-msg_nbr = '035'.
      messages-msg_text = text-035.
      APPEND messages.
      EXIT.
    ENDIF.

* create search for header
    LOOP AT t_approver INTO wa_approver.

      IF wa_approver-item NE 0
        AND wa_approver-apayno NE 0.

        IF wa_approver-item = 1.
          gs_apayno-sign   = c_i.
          gs_apayno-option = c_eq.
          gs_apayno-low    = wa_approver-apayno.
          APPEND gs_apayno TO gt_apayno.
          CLEAR: approved.
        ELSE.
          pitem = wa_approver-item - 1.
          SELECT SINGLE approved FROM /nrk/apayappr INTO approved
            WHERE apayno EQ wa_approver-apayno
              AND item = pitem.

          IF approved EQ 'A'.
            gs_apayno-sign   = c_i.
            gs_apayno-option = c_eq.
            gs_apayno-low    = wa_approver-apayno.
            APPEND gs_apayno TO gt_apayno.
            CLEAR: approved.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR: gs_apayno.

    ENDLOOP.

    IF gt_apayno[] IS INITIAL.
* No current approval work items
      messages-msg_type = 'I'.
      messages-msg_nbr = '035'.
      messages-msg_text = text-035.
      APPEND messages.
      EXIT.
    ENDIF.

  ENDIF.

* Get header records
  SELECT * INTO CORRESPONDING FIELDS OF wa_apayhd FROM /nrk/apayhd
    UP TO 100 ROWS
    WHERE apayno IN gt_apayno
      AND lifname IN gt_lifname
      AND lifnr   IN gt_lifnr
      AND xblnr   IN gt_xblnr
      AND bldat   IN gt_bldat
      AND wrbtr   IN gt_wrbtr
      AND waers   IN gt_waers
      AND duedate IN gt_duedate
      AND budat   IN gt_budat
      AND belnr   IN gt_belnr
      AND bukrs   IN gt_bukrs
      AND gjahr   IN gt_gjahr
      AND status  IN gt_status
      AND ebeln   IN gt_ebeln.

* Check company code in user table
* -> needs to be added, if required

* Get Status description
    SELECT SINGLE sdescr FROM /nrk/apaysdef INTO wa_apayhd-sdescr
      WHERE status EQ wa_apayhd-status
        AND langu  EQ sy-langu.

* Get Document type description
    SELECT SINGLE objecttext FROM toasp INTO wa_apayhd-objecttext
      WHERE ar_object = wa_apayhd-ar_object
        AND language = sy-langu.

* Add record to output list
    APPEND wa_apayhd TO apayhd.

  ENDSELECT.

  IF sy-subrc NE 0.
* No records found
    messages-msg_type = 'E'.
    messages-msg_nbr = '003'.
    messages-msg_text = text-003.
    APPEND messages.
    EXIT.
  ENDIF.

* start select only for approver
  IF approver EQ 'X'.

    LOOP AT apayhd.

* get approval records
      SELECT * FROM /nrk/apayappr INTO TABLE t_approver
        WHERE ext_user EQ searchcriteria-extuser
          AND approved EQ space
          AND apayno   EQ apayhd-apayno.

      IF sy-subrc EQ 0.

        LOOP AT t_approver INTO wa_approver.

          IF wa_approver-item = 1.
            READ TABLE apayhd INTO wa_hd WITH KEY apayno = wa_approver-apayno.
            APPEND wa_hd TO t_hd.
            CLEAR: approved.
          ELSE.
            pitem = wa_approver-item - 1.
            SELECT SINGLE approved FROM /nrk/apayappr INTO approved
              WHERE apayno EQ wa_approver-apayno
                AND item = pitem.

            IF approved EQ 'A'.
              READ TABLE apayhd INTO wa_hd WITH KEY apayno = wa_approver-apayno.
              APPEND wa_hd TO t_hd.
              CLEAR: approved.
            ENDIF.
          ENDIF.

        ENDLOOP.
* begin old code
*      DELETE ADJACENT DUPLICATES FROM t_approver COMPARING apayno.
*
*      LOOP AT t_approver INTO wa_approver WHERE approved NE 'X'.
*        READ TABLE apayhd INTO wa_hd WITH KEY apayno = wa_approver-apayno.
*
*        IF sy-subrc EQ 0.
*          APPEND wa_hd TO t_hd.
*        ENDIF.
*      ENDLOOP.
* end old code

      ELSE.
        CLEAR: t_hd[].
*       CLEAR: apayhd[].
      ENDIF.

    ENDLOOP.

    IF NOT t_hd IS INITIAL.
      CLEAR: apayhd, apayhd[].
      MOVE t_hd[] TO apayhd[].
    ENDIF.
  ENDIF.
* end select only for approver

  LOOP AT apayhd INTO wa_hd.

    MOVE wa_hd-apayno TO object_id.
    CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
      EXPORTING
        objecttype               = '/NRK/APAY'
        object_id                = object_id
        client                   = sy-mandt
*       ARCHIV_ID                =
*       ARC_DOC_ID               =
*       DOCUMENTTYPE             =
*       FROM_AR_DATE             =
*       UNTIL_AR_DATE            = SY-DATUM
*       DOCUMENTCLASS            =
*       DEL_DATE                 =
*       LIMITED                  =
*       LIMIT                    =
*     IMPORTING
*       COUNT                    =
*       REDUCEDBYLIMIT           =
*       REDUCEDBYAUTHORITY       =
      TABLES
        connections              = t_connections
*       PARAMETER                =
      EXCEPTIONS
        nothing_found            = 1
        OTHERS                   = 2.

    IF sy-subrc EQ 0.
      LOOP AT t_connections INTO wa_connections.
        doc_links-apayno = wa_hd-apayno.
        doc_links-arcid = wa_connections-archiv_id.
        doc_links-ardid = wa_connections-arc_doc_id.

        SELECT SINGLE objecttext FROM toasp
          INTO doc_links-adesc
          WHERE ar_object EQ wa_connections-ar_object.

*       doc_links-ADESC
        MOVE wa_connections-reserve TO doc_links-amime.
        doc_links-adate = wa_connections-ar_date.
        APPEND doc_links.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

  DESCRIBE TABLE apayhd LINES hits.

  IF hits > 0. " Records found
    hits_c = hits.
    CONCATENATE hits_c text-002 INTO msg_text SEPARATED BY space.
    messages-msg_type = 'I'.
    messages-msg_nbr = '002'.
    messages-msg_text = msg_text.
    APPEND messages.
*    EXIT.
  ELSE. " No records found
    messages-msg_type = 'E'.
    messages-msg_nbr = '003'.
    messages-msg_text = text-003.
    APPEND messages.
    EXIT.
  ENDIF.

ENDFUNCTION.
