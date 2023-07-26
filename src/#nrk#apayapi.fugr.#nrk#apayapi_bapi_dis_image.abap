FUNCTION /nrk/apayapi_bapi_dis_image.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYNO) TYPE  /NRK/APAYNO OPTIONAL
*"  EXPORTING
*"     VALUE(BINLENGTH) TYPE  NUM12
*"  TABLES
*"      IMAGE_DTAB STRUCTURE  TBL1024 OPTIONAL
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"----------------------------------------------------------------------

  DATA:
*       binlength     TYPE num12,
        wa_hd         TYPE /nrk/apayhd,
        object_id     LIKE toav0-object_id,
        connections   LIKE toav0 OCCURS 0,
        wa_connection LIKE toav0.

  CLEAR: wa_hd.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayno.

* Get assigned documents

  MOVE apayno TO object_id.

  CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
    EXPORTING
      objecttype               = '/NRK/APAY'
      object_id                = object_id
*   CLIENT                   =
*   ARCHIV_ID                =
*   ARC_DOC_ID               =
    documenttype             = wa_hd-ar_object
*   FROM_AR_DATE             =
*   UNTIL_AR_DATE            = SY-DATUM
*   DOCUMENTCLASS            =
*   DEL_DATE                 =
*   LIMITED                  =
*   LIMIT                    =
* IMPORTING
*   COUNT                    =
*   REDUCEDBYLIMIT           =
*   REDUCEDBYAUTHORITY       =
  TABLES
    connections              = connections
*   PARAMETER                =
  EXCEPTIONS
    nothing_found            = 1
    OTHERS                   = 2.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Get image bits

  READ TABLE connections INDEX 1 INTO wa_connection.

  CALL FUNCTION 'ARCHIVOBJECT_GET_TABLE'
    EXPORTING
      archiv_id                      = wa_connection-archiv_id
      document_type                  = 'PDF'
      archiv_doc_id                  = wa_connection-arc_doc_id
*   ALL_COMPONENTS                 =
*   SIGNATURE                      = 'X'
*   COMPID                         = 'data'
  IMPORTING
*   LENGTH                         =
    binlength                      = binlength
  TABLES
*   ARCHIVOBJECT                   =
    binarchivobject                = image_dtab
  EXCEPTIONS
    error_archiv                   = 1
    error_communicationtable       = 2
    error_kernel                   = 3
    OTHERS                         = 4.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.




ENDFUNCTION.
