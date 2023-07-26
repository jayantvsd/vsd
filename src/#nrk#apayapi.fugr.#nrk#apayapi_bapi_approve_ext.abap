FUNCTION /nrk/apayapi_bapi_approve_ext.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(RECORDID) TYPE  /NRK/APAYNO OPTIONAL
*"     VALUE(CODED) LIKE  SONV-FLAG OPTIONAL
*"     VALUE(APPROVED) LIKE  SONV-FLAG OPTIONAL
*"     VALUE(REJECTED) LIKE  SONV-FLAG OPTIONAL
*"     VALUE(COMMENT) TYPE  /NRK/APAYCOMMENT OPTIONAL
*"     VALUE(EXTUSER) TYPE  /NRK/APAYEXUSER OPTIONAL
*"     VALUE(KOSTL) TYPE  KOSTL OPTIONAL
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES
*"----------------------------------------------------------------------

  DATA: parked      LIKE boole-boole,
        acgl_item   LIKE acgl_item OCCURS 0,
        bdcmsgcoll  LIKE bdcmsgcoll OCCURS 0,
        items       TYPE /nrk/apayitems OCCURS 0,
        wa_item     LIKE /nrk/apayitems,
        l_apayhd    LIKE /nrk/apayhd,
        l_objectkey LIKE  swr_struct-object_key.

  messages-msg_type = 'I'.
  messages-msg_nbr = '010'.
  messages-msg_text = text-010.
  APPEND messages.

  messages-msg_type = 'I'.
  messages-msg_nbr = '010'.
  CONCATENATE 'approval=' approved INTO messages-msg_text.
  APPEND messages.

  messages-msg_type = 'I'.
  messages-msg_nbr = '010'.
  CONCATENATE 'rejection=' rejected INTO messages-msg_text.
  APPEND messages.

  messages-msg_type = 'I'.
  messages-msg_nbr = '010'.
  CONCATENATE 'coded=' coded INTO messages-msg_text.
  APPEND messages.


** Add header to structure
  SELECT SINGLE * FROM /nrk/apayhd INTO l_apayhd
    WHERE apayno EQ recordid.


** Add line items to structure
  wa_item-kostl = kostl.
  APPEND wa_item TO items.

  CALL FUNCTION '/NRK/APAYSTORECODING'
    EXPORTING
      apayhd                   = l_apayhd
    IMPORTING
      parked                   = parked
    TABLES
      items                    = items
      bdcmsgcoll               = bdcmsgcoll
    EXCEPTIONS
      parking_failed           = 1
      coding_completed         = 2
      line_item_amount_missing = 3
      company_code_missing     = 4
      OTHERS                   = 5.

  IF sy-subrc NE 0.
    IF sy-subrc EQ 2.
      messages-msg_type = 'I'.
      messages-msg_nbr = '034'.
      messages-msg_text = text-034.
      APPEND messages.
    ELSEIF sy-subrc EQ 3.
      messages-msg_type = 'E'.
      messages-msg_nbr = '038'.
      messages-msg_text = text-038.
      APPEND messages.
      EXIT.
    ELSEIF sy-subrc EQ 4.
      messages-msg_type = 'E'.
      messages-msg_nbr = '042'.
      messages-msg_text = text-042.
      APPEND messages.
      EXIT.
    ELSE.
      messages-msg_type = 'E'.
      messages-msg_nbr = '005'.
      messages-msg_text = text-005.
      APPEND messages.
      EXIT.
    ENDIF.
  ENDIF.

*  IF sy-subrc <> 0.
*    IF sy-subrc EQ 3.
*      messages-msg_type = 'E'.
*      messages-msg_nbr = '038'.
*      messages-msg_text = text-038.
*      APPEND messages.
*      EXIT.
*    ELSE.
*      messages-msg_type = 'E'.
*      messages-msg_nbr = '005'.
*      messages-msg_text = text-005.
*      APPEND messages.
*      EXIT.
*    ENDIF.
*  ENDIF.

* set document status
  IF coded EQ 'X'. "Invoice coded

    CALL FUNCTION '/NRK/APAY_CHANGE_PARKED_DOC'
      EXPORTING
        apayno            = l_apayhd-apayno
        kostl             = kostl
      TABLES
        t_items           = items
      EXCEPTIONS
        no_apay_record_id = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      messages-msg_type = 'E'.
      messages-msg_nbr = '005'.
      messages-msg_text = text-005.
      APPEND messages.
      EXIT.
    ENDIF.

  ENDIF.

  IF approved EQ 'X'. "Invoice approved

    CALL FUNCTION '/NRK/APAY_ADD_COMMENT_API'
      EXPORTING
        iv_apayno           = l_apayhd-apayno
        iv_user             = extuser
        iv_comment          = comment
      EXCEPTIONS
        user_not_registered = 1
        db_insert_failed    = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      messages-msg_type = 'E'.
      messages-msg_nbr = '006'.
      messages-msg_text = text-006.
      APPEND messages.
      EXIT.
    ENDIF.

    MOVE l_apayhd-apayno TO l_objectkey.

    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
      EXPORTING
        object_type             = '/NRK/APAY'
        object_key              = l_objectkey
        event                   = 'approved'
        commit_work             = 'X'
        event_language          = sy-langu
*       LANGUAGE                = SY-LANGU
        user                    = sy-uname
*       IFS_XML_CONTAINER       =
*     IMPORTING
*       RETURN_CODE             =
*       EVENT_ID                =
*     TABLES
*       INPUT_CONTAINER         =
*       MESSAGE_LINES           =
*       MESSAGE_STRUCT          =
              .


  ELSEIF rejected EQ 'X'. "Invoice rejected

    MOVE l_apayhd-apayno TO l_objectkey.

    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
      EXPORTING
        object_type             = '/NRK/APAY'
        object_key              = l_objectkey
        event                   = 'rejected'
        commit_work             = 'X'
        event_language          = sy-langu
*       LANGUAGE                = SY-LANGU
        user                    = sy-uname
*       IFS_XML_CONTAINER       =
*     IMPORTING
*       RETURN_CODE             =
*       EVENT_ID                =
*     TABLES
*       INPUT_CONTAINER         =
*       MESSAGE_LINES           =
*       MESSAGE_STRUCT          =
              .

  ENDIF.



ENDFUNCTION.
