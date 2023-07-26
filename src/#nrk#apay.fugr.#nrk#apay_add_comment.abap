function /nrk/apay_add_comment.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_APAYNO) TYPE  /NRK/APAYNO
*"  EXCEPTIONS
*"      ENTRY_NOT_FOUND
*"      DB_INSERT_FAILED
*"----------------------------------------------------------------------
* Local data declaration
  data:  ls_thd    type /nrk/apayhd,
         ls_tcmnt  type /nrk/apaycmnt,

* Function module 'SUSR_USER_ADDRESS_READ'
  ls_user_address  type addr3_val,

* For function module 'POPUP_GET_VALUES'
  lt_fields        type standard table of sval,
  ls_fields        type sval,
  lv_returncode    type c    length 1.

* Get APay record
  select single * from /nrk/apayhd into ls_thd where apayno eq iv_apayno.

  if sy-subrc ne 0.
    raise entry_not_found.
    exit.
  endif.

* Set fields for comment entry
  ls_fields-tabname    = '/NRK/APAYCMNT'.
  ls_fields-fieldname  = 'PROCESS_COMMENT'.
  ls_fields-value      = space.
  ls_fields-field_attr = space.
  ls_fields-field_obl  = space.
  ls_fields-fieldtext  = text-100.
  ls_fields-novaluehlp = space.
  append ls_fields to lt_fields.

* Call popup for comment entry
  call function 'POPUP_GET_VALUES'
    EXPORTING
      no_value_check  = space
      popup_title     = text-101
      start_column    = '5'
      start_row       = '5'
    IMPORTING
      returncode      = lv_returncode
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      others          = 2.

* Process comment
  if lv_returncode = space.

    read table lt_fields into ls_fields with key fieldname = 'PROCESS_COMMENT'.
    if sy-subrc = 0 and ls_fields-value <> space.

      ls_tcmnt-apayno                      = ls_thd-apayno.
      ls_tcmnt-crea_date                   = sy-datum.
      ls_tcmnt-crea_time                   = sy-uzeit.
      ls_tcmnt-user_id                     = sy-uname.

* Get user full name for SAP user
      select single fullname from /nrk/apayuser into ls_tcmnt-user_name where otype eq 'US' and objid eq sy-uname.
      if sy-subrc ne 0.
* User us not registered
        message s398(00) with text-102 sy-uname text-103 space.
      else.
* Fill comment
        ls_tcmnt-process_comment = ls_fields-value.
      endif.

* Insert comment into table
      insert /nrk/apaycmnt from ls_tcmnt.
      if sy-subrc = 0.
        commit work.
      else.
        raise db_insert_failed.
      endif.

    endif.

  endif.

endfunction.
