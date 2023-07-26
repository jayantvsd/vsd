FUNCTION /NRK/APAYARCHIVEEMAIL .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYNO
*"     REFERENCE(AR_OBJECT) TYPE  TOAOM-AR_OBJECT OPTIONAL
*"     REFERENCE(ARCHIVE_FLAG) TYPE  BOOLE OPTIONAL
*"  EXPORTING
*"     REFERENCE(ARCHIVED) TYPE  BOOLE
*"     REFERENCE(ARCHIV_ID) TYPE  TOAV0-ARC_DOC_ID
*"  TABLES
*"      TEXT STRUCTURE  /NRK/APAYAPI_TEXT
*"  EXCEPTIONS
*"      CREATE_TABLE_FAILED
*"      CONFIGURATION_MISSING
*"      LINK_CREATION_FAILED
*"      TABLE_SIZING_FAILED
*"----------------------------------------------------------------------

  DATA: l_cnt               TYPE i.

  DATA: l_archiv_id         LIKE toav0-archiv_id,
        l_arc_doc_id        LIKE toav0-arc_doc_id,
        l_ar_date           LIKE toav0-ar_date,
        l_object_id         LIKE sapb-sapobjid,
        l_doc_length        LIKE sapb-length.

  DATA: litab_email         LIKE soli OCCURS 50 WITH HEADER LINE,
        litab_doc           LIKE docs OCCURS 100 WITH HEADER LINE,
        litab_doc2          LIKE docs OCCURS 100 WITH HEADER LINE.

  DATA: t_tline             LIKE tline OCCURS 0.

  DATA: wa_toaom            LIKE toaom.

  CONSTANTS: c_apay_object   LIKE toaom-sap_object VALUE '/NRK/APAY'.


  CLEAR: wa_toaom.

* Get ArchiveLink information
  SELECT SINGLE * FROM toaom INTO wa_toaom
    WHERE sap_object EQ c_apay_object
      AND ar_object  EQ ar_object
      AND ar_status EQ 'X'.

  IF sy-subrc NE 0.
    RAISE configuration_missing.
  ENDIF.

* Create email document

  l_archiv_id = wa_toaom-archiv_id.
  MOVE apayno TO l_object_id.

* Convert text to archive format
  MOVE text[] TO litab_email[].

  CALL FUNCTION '/NRK/APAYCHANGETABLESIZE'
    TABLES
      itab   = litab_email
      otab   = litab_doc
    EXCEPTIONS
      error  = 1
      OTHERS = 2.

  IF sy-subrc <> 0.
    RAISE table_sizing_failed.
  ENDIF.

  MOVE litab_email[] TO litab_doc2[].

* Archive email
  DESCRIBE TABLE litab_doc2 LINES l_cnt.
  l_doc_length = l_cnt * 1024.

  CALL FUNCTION 'ARCHIVOBJECT_CREATE_TABLE'
    EXPORTING
      archiv_id                      = l_archiv_id
*   ARC_DOC_ID                     =
      document_type                  = 'TXT'
      length                         = l_doc_length
*     COMPID                         = 'data'
*     SIGNATURE                      = 'X'
  IMPORTING
    archiv_doc_id                  = l_arc_doc_id
  TABLES
    archivobject                   = litab_doc2
*   COMPONENTS                     =
*   BINARCHIVOBJECT                =
 EXCEPTIONS
   error_archiv                   = 1
   error_communicationtable       = 2
   error_kernel                   = 3
   OTHERS                         = 4.

  IF sy-subrc <> 0.
    RAISE create_table_failed.
  ENDIF.

* create link to APAay record
  CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
    EXPORTING
      archiv_id                   = l_archiv_id
      arc_doc_id                  = l_arc_doc_id
*   AR_DATE                     = ' '
      ar_object                   = ar_object
*   DEL_DATE                    = ' '
*   MANDANT                     = ' '
      object_id                   = l_object_id
      sap_object                  = c_apay_object
*   DOC_TYPE                    = ' '
*   BARCODE                     = ' '
   EXCEPTIONS
     error_connectiontable       = 1
     OTHERS                      = 2.

  IF sy-subrc <> 0.
    RAISE link_creation_failed.
  ENDIF.




  archived = 'X'.
  archiv_id = l_arc_doc_id.

ENDFUNCTION.
