
FUNCTION /nrk/apaycreaterecord.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(ARCHIV_ID) TYPE  SAEARCHIVI
*"     REFERENCE(ARC_DOC_ID) TYPE  SAEARDOID
*"     REFERENCE(AR_OBJECT) TYPE  SAEOBJART
*"     REFERENCE(SAP_OBJECT) TYPE  SAEANWDID
*"     REFERENCE(CDATE) TYPE  /NRK/APAYCDATE
*"     REFERENCE(CTIME) TYPE  /NRK/APAYCTIME
*"     REFERENCE(STATUS) TYPE  /NRK/APAYSTATUS
*"     REFERENCE(PRIORITY) TYPE  /NRK/APAYPRIO DEFAULT 3
*"     REFERENCE(HEADER) LIKE  /NRK/APAYHD STRUCTURE  /NRK/APAYHD
*"       OPTIONAL
*"     REFERENCE(USERID) TYPE  UNAME OPTIONAL
*"  EXPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"  EXCEPTIONS
*"      RECORD_CREATION_FAILED
*"      NUMBER_NOT_CREATED
*"      RECORD_ASSIGNMENT_FAILED
*"----------------------------------------------------------------------

  DATA: wa_hd     LIKE /nrk/apayhd,
        object_id LIKE toav0-object_id,
        objectkey LIKE swr_struct-object_key,
        t_his     LIKE /nrk/apayhis OCCURS 0 WITH HEADER LINE,
        wa_his    LIKE /nrk/apayhis,
        tlines    TYPE i,
        item      LIKE /nrk/apayhis-item,
        address   LIKE  bapiaddr3,
        return    LIKE  bapiret2 OCCURS 0.

  CLEAR: wa_hd,
         object_id,
         objectkey,
         t_his, t_his[],
         wa_his,
         tlines,
         item,
         return[],
         address.

* Create APay Record
  wa_hd-ctime = ctime.
  wa_hd-cdate = cdate.
  wa_hd-status = status.
  wa_hd-ar_object = ar_object.
  wa_hd-sap_object = sap_object.
  wa_hd-uname = userid.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr                   = '01'
      object                        = '/NRK/APAY'
*   QUANTITY                      = '1'
*   SUBOBJECT                     = ' '
*   TOYEAR                        = '0000'
*   IGNORE_BUFFER                 = ' '
   IMPORTING
      number                        = wa_hd-apayno
*   QUANTITY                      =
*   RETURNCODE                    =
   EXCEPTIONS
     interval_not_found            = 1
     number_range_not_intern       = 2
     object_not_found              = 3
     quantity_is_0                 = 4
     quantity_is_not_1             = 5
     interval_overflow             = 6
     buffer_overflow               = 7
     OTHERS                        = 8.

  IF sy-subrc <> 0.
    RAISE number_not_created.
  ENDIF.

  apayno = wa_hd-apayno.

*  INSERT /nrk/apayhd FROM wa_hd.
*  IF sy-subrc <> 0.
*    RAISE record_creation_failed.
*  ENDIF.

* Update status history
  SELECT * FROM /nrk/apayhis INTO TABLE t_his
     WHERE apayno = wa_hd-apayno.

  DESCRIBE TABLE t_his LINES tlines.
  item = tlines + 1.

  wa_his-apayno = wa_hd-apayno.
  wa_his-item   = item.
  wa_his-status = wa_hd-status.
  wa_his-sdate  = cdate.
  wa_his-stime  = ctime.

  IF NOT userid IS INITIAL.
    wa_his-suser  = userid.
  ELSE.
    wa_his-suser  = sy-uname.
  ENDIF.

  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = wa_his-suser
    IMPORTING
      address  = address
    TABLES
      return   = return.

  wa_his-sname = address-fullname.

  INSERT /nrk/apayhis FROM wa_his.


* update last change
  wa_hd-lastchange = cdate.
  wa_hd-lastchanget = ctime.

* Add header row
  INSERT /nrk/apayhd FROM wa_hd.
  IF sy-subrc <> 0.
    RAISE record_creation_failed.
  ENDIF.

  COMMIT WORK.

* Assign Document Image
  MOVE wa_hd-apayno TO object_id.

  CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
    EXPORTING
      archiv_id                   = archiv_id
      arc_doc_id                  = arc_doc_id
      ar_date                     = sy-datum
      ar_object                   = ar_object
*   DEL_DATE                    = ' '
*   MANDANT                     = ' '
      object_id                   = object_id
      sap_object                  = '/NRK/APAY'
*   DOC_TYPE                    = ' '
*   BARCODE                     = ' '
  EXCEPTIONS
    error_connectiontable       = 1
    OTHERS                      = 2.

  IF sy-subrc <> 0.
    RAISE record_assignment_failed.
  ENDIF.

* Trigger Create Event
  MOVE wa_hd-apayno TO objectkey.

  CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
    EXPORTING
      object_type             = '/NRK/APAY'
      object_key              = objectkey
      event                   = 'created'
      commit_work             = 'X'
*   EVENT_LANGUAGE          = SY-LANGU
*   LANGUAGE                = SY-LANGU
*   USER                    = SY-UNAME
*   IFS_XML_CONTAINER       =
* IMPORTING
*   RETURN_CODE             =
*   EVENT_ID                =
* TABLES
*   INPUT_CONTAINER         =
*   MESSAGE_LINES           =
*   MESSAGE_STRUCT          =
            .

ENDFUNCTION.
