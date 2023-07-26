FUNCTION /nrk/apayapi_start_workflow.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APAYNO) TYPE  /NRK/APAYHD-APAYNO OPTIONAL
*"     VALUE(WORKFLOWTYPE) TYPE  CHAR10 OPTIONAL
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"----------------------------------------------------------------------

  DATA: wa_hd          LIKE /nrk/apayhd,
        wa_url         TYPE string,
        url            TYPE char255,
        http_client    TYPE REF TO if_http_client,
        http_response  TYPE REF TO if_http_response,
        wa_config      LIKE /nrk/apayconfig,
        key            LIKE /nrk/apayconfig-key2,
        object_id      LIKE  swr_struct-object_key,
        json_data      TYPE string,
        secret         TYPE char50,
        amount(13)     TYPE c,
        ar_object_desc LIKE /nrk/apayhd_dis-objecttext,
        sdescr         TYPE /nrk/apaysdescr,
        response_fields TYPE tihttpnvp,
        response_rc    TYPE i,
        response_body  TYPE string,
        xjson_data     TYPE xstring,
        message        TYPE string,
        email          LIKE /nrk/apayuser-smtp_addr,
        action(1)      TYPE c,
        wa_vbsegs      LIKE vbsegs.

* Validate apay record
  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '003'.
    messages-msg_text = text-003.
    APPEND messages.
    EXIT.
  ELSE. " check for coding and set flag

    SELECT SINGLE * FROM vbsegs INTO wa_vbsegs
      WHERE bukrs EQ wa_hd-bukrs
        AND belnr EQ wa_hd-belnr
        AND gjahr EQ wa_hd-gjahr.

    IF sy-subrc EQ 0. " line items exist
      action = 'A'. " for approver
    ELSE.
      action = 'C'. " for coder
    ENDIF.

  ENDIF.

  MOVE apayno TO object_id.
  MOVE workflowtype TO key.
*  WRITE wa_hd-wrbtr TO amount NO-GROUPING CURRENCY wa_hd-waers.
  MOVE wa_hd-wrbtr TO amount.

* Get document type description
  SELECT SINGLE objecttext FROM toasp INTO ar_object_desc
    WHERE ar_object = wa_hd-ar_object
      AND language = sy-langu.

  IF sy-subrc EQ 0.
    SELECT SINGLE objecttext FROM toasp INTO ar_object_desc
      WHERE ar_object = wa_hd-ar_object.
  ENDIF.

* Get status description
  SELECT SINGLE sdescr FROM /nrk/apaysdef INTO sdescr
    WHERE status EQ wa_hd-status
      AND langu EQ sy-langu.

* Get email from user
  SELECT SINGLE smtp_addr FROM /nrk/apayuser INTO email
    WHERE extuser EQ wa_hd-ext_approver.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '075'.
    messages-msg_text = text-075.
    APPEND messages.
    EXIT.
  ELSE.
    TRANSLATE email TO UPPER CASE.
  ENDIF.

* Get URL from configuration table
  SELECT SINGLE * FROM /nrk/apayconfig INTO wa_config
    WHERE key1 = 'APAYSP' AND key2 = key.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '068'.
    messages-msg_text = text-068.
    APPEND messages.
    EXIT.
  ELSE.
    MOVE wa_config-val1 TO url.
    MOVE wa_config-val1 TO wa_url.
  ENDIF.

* Get secret
  SELECT SINGLE * FROM /nrk/apayconfig INTO wa_config
    WHERE key1 = 'APAYSP' AND key2 = 'WFTOKEN'.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '073'.
    messages-msg_text = text-073.
    APPEND messages.
    EXIT.
  ELSE.
    MOVE wa_config-val1 TO secret.
  ENDIF.

  CONCATENATE
    '{'
      '"SECRET":"'     secret         '",'
      '"WORKFLOWID":"' workflowtype   '",'
      '"VALUES":{'
        '"APAYNO":" '  wa_hd-apayno   '",'
        '"BUKRS":"'    wa_hd-bukrs    '",'
        '"LIFNR":"'    wa_hd-lifnr    '",'
        '"LIFNAME":"'  wa_hd-lifname  '",'
        '"XBLNR":"'    wa_hd-xblnr    '",'
        '"WRBTR":"'    amount         '",'
        '"WAERS":"'    wa_hd-waers    '",'
        '"EBELN":"'    wa_hd-ebeln    '",'
        '"SHKZG":"'    wa_hd-shkzg    '",'
        '"STATUS":"'   wa_hd-status   '",'
        '"ARDESCR":"'  ar_object_desc '",'
        '"SDESCR":"'   sdescr         '",'
*       '"EXTUSER":"'  wa_hd-ext_approver '",'
        '"EMAIL":"'    email '",'
        '"BLDAT":"'    wa_hd-bldat    '",'
        '"ACTION":"'   action         '"'
      '}'
    '}'
  INTO json_data.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = json_data
    IMPORTING
      buffer = xjson_data.

* Make the HTTP POST request to the target system.
  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = wa_url
    IMPORTING
      client             = http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '074'.
    messages-msg_text = text-074.
    APPEND messages.
    EXIT.
  ENDIF.

  http_client->request->set_cdata( json_data ).
  http_client->request->set_content_type( 'application/json; charset=utf-8' ).
* http_client->request->set_content_type( 'application/json' ).
* http_client->request->set_content_type( 'text/plain' ).
  http_client->request->set_method( 'POST' ).

  CALL METHOD http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '069'.
*   messages-msg_text = text-069.
    http_client->get_last_error( IMPORTING message = message ).
    CONCATENATE text-069 message INTO messages-msg_text SEPARATED BY space.
    APPEND messages.
    EXIT.
  ENDIF.

  http_client->receive( EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '070'.
*   messages-msg_text = text-070.
    http_client->get_last_error( IMPORTING message = message ).
    CONCATENATE text-070 message INTO messages-msg_text SEPARATED BY space.
    APPEND messages.
    EXIT.
  ENDIF.

  response_body = http_client->response->get_cdata( ).
  http_client->response->get_status( IMPORTING code = response_rc ).

**********************************************************
*  CALL METHOD http_client->request->set_method
*    EXPORTING
*      method = 'POST'.
*
* http_client->request->set_content_type( 'application/json' ).
* http_client->request->set_data( xjson_data ).
* http_client->request->set_string_data( xjson_data ).
* http_client->request->set_cdata( json_data ).
*
*  CALL METHOD http_client->request->set_header_field
*    EXPORTING
*      name  = 'Content-Type'
*      value = 'application/json'.
*
*  CALL METHOD http_client->send
*    EXCEPTIONS
*      http_communication_failure = 1
*      http_invalid_state         = 2
*      http_processing_failed     = 3
*      http_invalid_timeout       = 4.
*
*  IF sy-subrc NE 0.
*    messages-msg_type = 'E'.
*    messages-msg_nbr = '069'.
*    messages-msg_text = text-069.
*    APPEND messages.
*    EXIT.
*  ENDIF.
*
*  CALL METHOD http_client->receive
*    EXCEPTIONS
*      http_communication_failure = 1
*      http_invalid_state         = 2
*      http_processing_failed     = 3.
*
*  IF sy-subrc NE 0.
*
** Get header fields
*    CALL METHOD http_client->response->get_header_fields
*      CHANGING
*        fields = response_fields.
*
** Get last error
*    CALL METHOD http_client->response->get_last_error
*      RECEIVING
*        rc = response_rc.
*
*    messages-msg_type = 'E'.
*    messages-msg_nbr = '070'.
*    messages-msg_text = text-070.
*    APPEND messages.
*    EXIT.
*  ENDIF.

  CALL METHOD http_client->close
    EXCEPTIONS
      http_invalid_state = 1.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '071'.
    messages-msg_text = text-071.
    APPEND messages.
    EXIT.
  ENDIF.

  messages-msg_type = 'I'.
  messages-msg_nbr = '072'.
  messages-msg_text = text-072.
  APPEND messages.
  EXIT.

ENDFUNCTION.
