FUNCTION /nrk/apayapi_bapi_add_document.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYNO) TYPE  /NRK/APAYHD-APAYNO OPTIONAL
*"     VALUE(AR_OBJECT) TYPE  /NRK/APAYHD-AR_OBJECT OPTIONAL
*"     VALUE(FLENGTH) TYPE  SAPB-LENGTH OPTIONAL
*"     VALUE(MIMETYPE) TYPE  TOADD-DOC_TYPE OPTIONAL
*"  TABLES
*"      ARCHIVOBJECT STRUCTURE  DOCS OPTIONAL
*"      BINARCHIVOBJECT STRUCTURE  TBL1024 OPTIONAL
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"----------------------------------------------------------------------

  DATA: object_id     LIKE toav0-object_id,
        sap_object    LIKE toaom-sap_object,
        wa_hd         LIKE /nrk/apayhd,
        outdoc        LIKE toadt.

  CLEAR: wa_hd.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '003'.
    messages-msg_text = text-003.
    APPEND messages.
    EXIT.
  ENDIF.

  sap_object = '/NRK/APAY'.
  MOVE apayno TO object_id.

  CALL FUNCTION 'ARCHIV_CREATE_TABLE'
    EXPORTING
     ar_object                      = ar_object
*    DEL_DATE                       =
     object_id                      = object_id
     sap_object                     = sap_object
     flength                        = flength
     doc_type                       = mimetype
*    DOCUMENT                       =
     mandt                          = sy-mandt
  IMPORTING
     outdoc                         = outdoc
  TABLES
     archivobject                   = archivobject
     binarchivobject                = binarchivobject
   EXCEPTIONS
     error_archiv                   = 1
     error_communicationtable       = 2
     error_connectiontable          = 3
     error_kernel                   = 4
     error_parameter                = 5
     error_user_exit                = 6
     error_mandant                  = 7
     OTHERS                         = 8.

  IF sy-subrc <> 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '007'.
    messages-msg_text = text-007.
    APPEND messages.
    EXIT.
  ELSE.
    messages-msg_type = 'I'.
    messages-msg_nbr = '009'.
    messages-msg_text = text-009.
    APPEND messages.
  ENDIF.

  IF NOT wa_hd-belnr IS INITIAL
    AND NOT wa_hd-bukrs IS INITIAL
    AND NOT wa_hd-gjahr IS INITIAL.

    sap_object = 'BKPF'.
    CONCATENATE wa_hd-bukrs
                wa_hd-belnr
                wa_hd-gjahr INTO object_id.

* add link to BKPF
    CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
      EXPORTING
        archiv_id                   = outdoc-contrep_id
        arc_doc_id                  = outdoc-arc_doc_id
        ar_date                     = sy-datum
        ar_object                   = ar_object
*       DEL_DATE                    = ' '
*       MANDANT                     = ' '
        object_id                   = object_id
        sap_object                  = 'BKPF'
*       DOC_TYPE                    = ' '
*       BARCODE                     = ' '
      EXCEPTIONS
        error_connectiontable       = 1
        OTHERS                      = 2.

    IF sy-subrc <> 0.
      messages-msg_type = 'E'.
      messages-msg_nbr = '007'.
      messages-msg_text = text-007.
      APPEND messages.
      EXIT.
    ELSE.
      messages-msg_type = 'I'.
      messages-msg_nbr = '009'.
      messages-msg_text = text-009.
      APPEND messages.
    ENDIF.

  ENDIF.


ENDFUNCTION.
