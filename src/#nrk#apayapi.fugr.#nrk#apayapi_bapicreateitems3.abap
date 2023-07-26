FUNCTION /nrk/apayapi_bapicreateitems3.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(HEADERDATA) TYPE  /NRK/APAYCAPTUREHD OPTIONAL
*"     VALUE(FILESIZE) TYPE  NUM12 OPTIONAL
*"  EXPORTING
*"     VALUE(APAYNO) TYPE  /NRK/APAYNO
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES
*"      LINEITEMS STRUCTURE  /NRK/APAYITEMS
*"      IMAGEBYTES STRUCTURE  TBL1024
*"--------------------------------------------------------------------

  DATA: archiv_id   TYPE saearchivi,
        arc_doc_id  TYPE saeardoid,
        length_arch LIKE sapb-length,
        doc_class   TYPE saedoktyp,
        length      TYPE i,
        l_subrc(1)  TYPE c.

  messages-msg_type = 'I'.
  messages-msg_nbr = '048'.
  messages-msg_text = text-048.
  APPEND messages.

*  DESCRIBE TABLE imagebinaries LINES length.
  MOVE filesize TO length_arch.

* Get image details
  CALL FUNCTION 'ARCHIV_CONNECTDEFINITION_GET'
    EXPORTING
      objecttype    = '/NRK/APAY'
      documenttype  = headerdata-ar_object
      client        = sy-mandt
    IMPORTING
      archivid      = archiv_id
      documentclass = doc_class
    EXCEPTIONS
      nothing_found = 1
      OTHERS        = 2.

* Store image
  CALL FUNCTION 'ARCHIVOBJECT_CREATE_TABLE'
    EXPORTING
      archiv_id                = archiv_id
      document_type            = doc_class
*     length                   = length
      length                   = length_arch
    IMPORTING
      archiv_doc_id            = arc_doc_id
    TABLES
      binarchivobject          = imagebytes
    EXCEPTIONS
      error_archiv             = 1
      error_communicationtable = 2
      error_kernel             = 3
      OTHERS                   = 4.

  IF sy-subrc <> 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '047'.
*    MOVE sy-subrc TO l_subrc.
*    CONCATENATE text-047 '(' l_subrc ')' INTO messages-msg_text.
    messages-msg_text = text-047.
    APPEND messages.
  ELSE.
    messages-msg_type = 'I'.
    messages-msg_nbr = '049'.
    messages-msg_text = text-049.
    APPEND messages.
  ENDIF.

  CALL FUNCTION '/NRK/APAYAPI_BAPI_STORE_ITEMS3'
    EXPORTING
      archiv_id           = archiv_id
      ar_object           = headerdata-ar_object
      archiv_doc_id       = arc_doc_id
      user                = headerdata-user
      status              = headerdata-status
      bukrs               = headerdata-bukrs
      xblnr               = headerdata-xblnr
      bldat               = headerdata-bldat
      invindicator        = headerdata-invindicator
      lifnr               = headerdata-lifnr
      lifname             = headerdata-lifname
      wrbtr               = headerdata-wrbtr
      wrbtr_net           = headerdata-wrbtr_net
      wmwst               = headerdata-wmwst
*     MWSKZ               = headerdata-mwskz
      waers               = headerdata-waers
      ebeln               = headerdata-ebeln
      ext_user            = headerdata-initapprover
      freight             = headerdata-freight
      pallet              = headerdata-pallet
      lfsnr1              = headerdata-lfsnr1
    TABLES
      messages            = messages
      lineitems           = lineitems.

ENDFUNCTION.
