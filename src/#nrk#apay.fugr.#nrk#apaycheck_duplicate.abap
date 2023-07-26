FUNCTION /nrk/apaycheck_duplicate.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) LIKE  /NRK/APAYHD-APAYNO
*"  EXPORTING
*"     REFERENCE(DUPLICATE) TYPE  BOOLE
*"  EXCEPTIONS
*"      NO_RECORD_FOUND
*"      MISSING_VALIDATION_DATA
*"----------------------------------------------------------------------

  DATA: lifnr       LIKE /nrk/apayhd-lifnr,
        xblnr       LIKE /nrk/apayhd-xblnr,
        bldat       LIKE /nrk/apayhd-bldat,
        wrbtr       LIKE /nrk/apayhd-wrbtr,
        t_hd        LIKE /nrk/apayhd OCCURS 0,
        wa_hd       LIKE /nrk/apayhd,
        object_id   LIKE toav0-object_id,
        connections LIKE toav0 OCCURS 0.

  CLEAR: t_hd[],
         wa_hd.

  SELECT SINGLE lifnr xblnr bldat wrbtr FROM /nrk/apayhd
    INTO (lifnr, xblnr, bldat, wrbtr)
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE no_record_found.
  ENDIF.

  IF lifnr IS INITIAL
    OR xblnr IS INITIAL
    OR bldat IS INITIAL
    OR wrbtr IS INITIAL.

*    EXIT.
    RAISE missing_validation_data.

  ENDIF.

  SELECT * FROM /nrk/apayhd INTO TABLE t_hd
    WHERE apayno NE apayno
      AND lifnr  EQ lifnr
      AND xblnr  EQ xblnr
      AND bldat  EQ bldat
      AND wrbtr  EQ wrbtr.


  IF sy-subrc NE 0. " No Duplicate
    EXIT.
  ENDIF.

  duplicate = 'X'.

*  LOOP AT t_hd INTO wa_hd.
*
*    MOVE wa_hd-apayno TO object_id.
*
*    CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
*      EXPORTING
*        objecttype               = '/NRK/APAY'
*        object_id                = object_id
**   CLIENT                   =
**   ARCHIV_ID                =
**   ARC_DOC_ID               =
**   DOCUMENTTYPE             =
**   FROM_AR_DATE             =
**   UNTIL_AR_DATE            = SY-DATUM
**   DOCUMENTCLASS            =
**   DEL_DATE                 =
**   LIMITED                  =
**   LIMIT                    =
** IMPORTING
**   COUNT                    =
**   REDUCEDBYLIMIT           =
**   REDUCEDBYAUTHORITY       =
*      TABLES
*        connections              = connections
**   PARAMETER                =
*      EXCEPTIONS
*        nothing_found            = 1
*        OTHERS                   = 2.
*
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*    APPEND LINES OF connections TO duplicate_images.
*
*  ENDLOOP.

ENDFUNCTION.
