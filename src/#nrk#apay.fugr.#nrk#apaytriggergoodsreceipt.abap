FUNCTION /nrk/apaytriggergoodsreceipt.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(MKPF) TYPE  MKPF
*"  TABLES
*"      MSEG STRUCTURE  MSEG
*"  EXCEPTIONS
*"      APAY_RECORD_NOT_FOUND
*"----------------------------------------------------------------------


  DATA: wa_mseg TYPE mseg,
        t_hd LIKE /nrk/apayhd OCCURS 0,
        wa_hd LIKE /nrk/apayhd,
        input_container TYPE TABLE OF swr_cont,
        objectkey LIKE  swr_struct-object_key.

* Get all apay records for material POs
  LOOP AT mseg INTO wa_mseg.

    SELECT * FROM /nrk/apayhd INTO wa_hd
      WHERE ebeln EQ wa_mseg-ebeln.

      APPEND wa_hd TO t_hd.

    ENDSELECT.

  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM t_hd COMPARING apayno.

* trigger event for each apay record
  LOOP AT t_hd INTO wa_hd.

    MOVE wa_hd-apayno TO objectkey.
    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
      EXPORTING
        object_type             = '/NRK/APAY'
        object_key              = objectkey
        event                   = 'GoodsReceived'
        commit_work             = 'X'
        event_language          = sy-langu
        language                = sy-langu
        user                    = sy-uname
*       IFS_XML_CONTAINER       =
*     IMPORTING
*       RETURN_CODE             =
*       EVENT_ID                =
      TABLES
        input_container         = input_container.
*       MESSAGE_LINES           =
*       MESSAGE_STRUCT          =

  ENDLOOP.

ENDFUNCTION.
