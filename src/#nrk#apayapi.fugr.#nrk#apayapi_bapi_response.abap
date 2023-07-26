FUNCTION /nrk/apayapi_bapi_response.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OBJECT) TYPE  /NRK/APAYAPIDATA OPTIONAL
*"     VALUE(RESPONSE) TYPE  /NRK/APAYAPIDATA OPTIONAL
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES OPTIONAL
*"      DATA STRUCTURE  /NRK/APAYAPIDATA OPTIONAL
*"----------------------------------------------------------------------

  DATA: apayno          LIKE /nrk/apayhd-apayno,
        event           LIKE swr_struct-event,
        status          LIKE /nrk/apayhd-status,
        extuser	        TYPE /nrk/apayexuser,
        email           LIKE /nrk/apayuser-smtp_addr,
        objectkey       LIKE swr_struct-object_key,
        input_container TYPE TABLE OF swr_cont,
        wa_container    LIKE LINE OF input_container,
        wa_hd           LIKE /nrk/apayhd,
        wa_data         LIKE /nrk/apayapidata,
        wa_config       LIKE /nrk/apayconfig,
        note(255)       TYPE c,
        wa_comment      LIKE /nrk/apaycmnt.

  CLEAR: apayno,
         wa_hd,
         wa_data,
         wa_container,
         event,
         objectkey,
         wa_config.

  IF object-field EQ 'APAYNO'
    AND NOT object-value IS INITIAL.

    MOVE object-value TO apayno.

    SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
      WHERE apayno EQ apayno.

    IF sy-subrc NE 0.
* Error APay record not found.
      messages-msg_type = 'E'.
      messages-msg_nbr = '003'.
      messages-msg_text = text-003.
      APPEND messages.
      EXIT.
    ENDIF.

    IF response-field EQ 'APPROVAL'.

      IF response-value EQ 'A'.
        event = 'approved'.

* Get status from config table
        SELECT SINGLE * FROM /nrk/apayconfig INTO wa_config
          WHERE key1 = 'APAYSP' AND key2 = 'APPROVALST'.

        IF sy-subrc EQ 0.
          MOVE wa_config-val1 TO status.
        ELSE.
          messages-msg_type = 'E'.
          messages-msg_nbr = '066'.
          messages-msg_text = text-066.
          APPEND messages.
        ENDIF.
      ELSEIF response-value EQ 'R'.
        event = 'rejected'.

* Get status from config table
        SELECT SINGLE * FROM /nrk/apayconfig INTO wa_config
          WHERE key1 = 'APAYSP' AND key2 = 'REJECTST'.

        IF sy-subrc EQ 0.
          MOVE wa_config-val1 TO status.
        ELSE.
          messages-msg_type = 'E'.
          messages-msg_nbr = '066'.
          messages-msg_text = text-066.
          APPEND messages.
        ENDIF.

      ELSE.
* Error not valid event
        messages-msg_type = 'E'.
        messages-msg_nbr = '063'.
        messages-msg_text = text-063.
        APPEND messages.
        EXIT.
      ENDIF.

* Read values from data table
* Get external user from data import table
      READ TABLE data INTO wa_data WITH KEY field = 'EXTUSER'.

      IF sy-subrc NE 0.
        messages-msg_type = 'E'.
        messages-msg_nbr = '065'.
        messages-msg_text = text-065.
        APPEND messages.
        EXIT.
      ELSE.
        MOVE wa_data-value TO extuser.
      ENDIF.

* Get note from data import table
      READ TABLE data INTO wa_data WITH KEY field = 'NOTE'.

      IF sy-subrc NE 0.
        messages-msg_type = 'I'.
        messages-msg_nbr = '078'.
        messages-msg_text = text-078.
        APPEND messages.
        EXIT.
      ELSE.
        MOVE wa_data-value TO note.

        wa_comment-apayno            = wa_hd-apayno.
        wa_comment-crea_date        = sy-datum.
        wa_comment-crea_time        = sy-uzeit.
        wa_comment-user_id          = extuser.
        wa_comment-process_comment  = note.

* Get user name
        SELECT SINGLE fullname FROM /nrk/apayuser INTO wa_comment-user_name
          WHERE extuser EQ extuser.
        IF sy-subrc NE 0.
*     RAISE user_not_registered.
        ENDIF.

* Insert entry
        INSERT /nrk/apaycmnt FROM wa_comment.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.

      ENDIF.

      MOVE apayno TO objectkey.

      CLEAR: wa_container.
*     wa_container-element = 'Email'.
      wa_container-element = 'EXTUSER'.
      wa_container-value = extuser.
      APPEND wa_container TO input_container.

      CLEAR: wa_container.
      wa_container-element = 'STATUS'.
      wa_container-value = status.
      APPEND wa_container TO input_container.

      CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
        EXPORTING
          object_type     = '/NRK/APAY'
          object_key      = objectkey
          event           = event
          commit_work     = 'X'
          event_language  = sy-langu
          user            = sy-uname
        TABLES
          input_container = input_container.

      messages-msg_type = 'I'.
      messages-msg_nbr = '067'.
      messages-msg_text = text-067.
      APPEND messages.
      EXIT.

    ENDIF.

  ELSE.
* Error object not maintained.
    messages-msg_type = 'E'.
    messages-msg_nbr = '064'.
    messages-msg_text = text-064.
    APPEND messages.
    EXIT.
  ENDIF.

ENDFUNCTION.
