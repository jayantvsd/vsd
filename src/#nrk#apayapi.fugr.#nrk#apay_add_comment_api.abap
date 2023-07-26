FUNCTION /nrk/apay_add_comment_api.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_APAYNO) TYPE  /NRK/APAYNO
*"     VALUE(IV_USER) TYPE  /NRK/APAYEXUSER
*"     VALUE(IV_COMMENT) TYPE  /NRK/APAYCOMMENT
*"  EXCEPTIONS
*"      USER_NOT_REGISTERED
*"      DB_INSERT_FAILED
*"----------------------------------------------------------------------

  DATA: ls_tcmnt TYPE /nrk/apaycmnt.

  IF NOT iv_comment IS INITIAL.

* Translate import parameter to upper case.
    TRANSLATE iv_user TO UPPER CASE.

* Prepare structure
    ls_tcmnt-apayno            = iv_apayno.
    ls_tcmnt-crea_date        = sy-datum.
    ls_tcmnt-crea_time        = sy-uzeit.
    ls_tcmnt-user_id          = iv_user.
    ls_tcmnt-process_comment  = iv_comment.
*    ls_tcmnt-process_comment  = 'OK'.

* Get user name
    SELECT SINGLE fullname FROM /nrk/apayuser INTO ls_tcmnt-user_name WHERE extuser EQ iv_user.
    IF sy-subrc NE 0.
      RAISE user_not_registered.
    ENDIF.

* Insert entry
    INSERT /nrk/apaycmnt FROM ls_tcmnt.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      RAISE db_insert_failed.
    ENDIF.

  ENDIF.

ENDFUNCTION.
