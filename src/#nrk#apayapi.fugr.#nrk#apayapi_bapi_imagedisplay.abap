FUNCTION /nrk/apayapi_bapi_imagedisplay.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYIMAGE) TYPE  /NRK/APAYAPIIMAGE OPTIONAL
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
        doc_class  LIKE toadd-doc_type.

  CLEAR: wa_hd.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayimage-apayno.


* Get image details
  CALL FUNCTION 'ARCHIV_CONNECTDEFINITION_GET'
    EXPORTING
      objecttype          = '/NRK/APAY'
      documenttype        = apayimage-ar_object
      client              = sy-mandt
    IMPORTING
*   CONNECTION          =
*   ARCHIVID            =
*   EXPIRYTIME          =
      documentclass       = doc_class
    EXCEPTIONS
      nothing_found       = 1
      OTHERS              = 2.

  IF sy-subrc <> 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '015'.
    messages-msg_text = text-015.
    APPEND messages.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ARCHIVOBJECT_GET_TABLE'
    EXPORTING
      archiv_id                      = apayimage-archiv_id
      document_type                  = doc_class
      archiv_doc_id                  = apayimage-arc_doc_id
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
