FUNCTION /nrk/apaygetimagebinaries.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYNO
*"  EXPORTING
*"     REFERENCE(DOC_TYPE) TYPE  SAEDOKTYP
*"  TABLES
*"      DATA_TAB STRUCTURE  TBL1024
*"----------------------------------------------------------------------

  DATA: lv_object_id   TYPE saeobjid,
        lv_objecttype  TYPE saeanwdid VALUE '/NRK/APAY',
        lt_connections TYPE toav0 OCCURS 1,
        ls_connections TYPE toav0,
        lt_connections_proc TYPE toav0 OCCURS 1,
        lv_pathname    TYPE string,
        lv_fullpath    TYPE string,
        lv_bin_length  LIKE sapb-length,
        lt_litab_bin   LIKE tbl1024 OCCURS 5000,
        lv_doc_type    TYPE saedoktyp.

* Set format
  lv_object_id = apayno.

* Get images for record
  CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
    EXPORTING
      objecttype    = lv_objecttype
      object_id     = lv_object_id
      client        = sy-mandt
    TABLES
      connections   = lt_connections
    EXCEPTIONS
      nothing_found = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
    MESSAGE s398(00) WITH text-101 space space space.
    EXIT.
  ENDIF.

* Check access authorizations
  LOOP AT lt_connections INTO ls_connections.
* Check authorization
    AUTHORITY-CHECK OBJECT 'S_WFAR_OBJ'
              ID 'OAARCHIV'   FIELD ls_connections-archiv_id
              ID 'OAOBJEKTE'  FIELD ls_connections-sap_object
              ID 'OADOKUMENT' FIELD ls_connections-ar_object
              ID 'ACTVT'      FIELD '03'.
    IF sy-subrc NE 0.
      DELETE lt_connections INDEX sy-tabix.
    ELSE.
* Append to processing table
      APPEND ls_connections TO lt_connections_proc.
    ENDIF.
  ENDLOOP.

* Refresh work area
  CLEAR ls_connections.

* Make sure there are images to display
  READ TABLE lt_connections_proc INTO ls_connections INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE w398(00) WITH text-102 space space space.
    EXIT.
  ELSE.

* Get binaries
    CLEAR: lv_bin_length, lt_litab_bin[], lt_litab_bin.
    lv_doc_type = ls_connections-reserve.

    CALL FUNCTION 'ARCHIVOBJECT_GET_TABLE'
      EXPORTING
        archiv_id                = ls_connections-archiv_id
        document_type            = lv_doc_type
        archiv_doc_id            = ls_connections-arc_doc_id
      IMPORTING
        binlength                = lv_bin_length
      TABLES
        binarchivobject          = lt_litab_bin
      EXCEPTIONS
        error_archiv             = 1
        error_communicationtable = 2
        error_kernel             = 3
        OTHERS                   = 4.

    IF sy-subrc <> 0.
      MESSAGE w398(00) WITH text-965 ls_connections-archiv_id ls_connections-arc_doc_id space.
    ENDIF.

    doc_type = lv_doc_type.

  ENDIF.

ENDFUNCTION.
