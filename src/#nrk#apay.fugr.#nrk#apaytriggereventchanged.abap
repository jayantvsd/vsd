FUNCTION /nrk/apaytriggereventchanged.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"----------------------------------------------------------------------

  DATA: l_objectkey LIKE  swr_struct-object_key.

  MOVE apayno TO l_objectkey.

    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
      EXPORTING
        object_type             = '/NRK/APAY'
        object_key              = l_objectkey
        event                   = 'changed'
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

ENDFUNCTION.
