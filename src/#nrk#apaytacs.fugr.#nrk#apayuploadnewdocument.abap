FUNCTION /nrk/apayuploadnewdocument.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"     REFERENCE(CANCEL) TYPE  BOOLE-BOOLE
*"  EXCEPTIONS
*"      DOCUMENT_UPLOAD_FAILED
*"----------------------------------------------------------------------

  CLEAR: wa_hd,
         wa_hd_orig,
         wa_ekko,
         wa_lfa1,
         wa_lfb1,
         wa_dtype,
         ebeln,
         lifnr,
         lifname,
         wrbtr,
         wmwst,
         wrbtr_net,
         waers,
         bukrs,
         butxt,
         bldat,
         xblnr,
         rdate,
         fullname,
         street,
         city,
         zip,
         region,
         ekgrp,
         ekorg.

  CLEAR: arc_doc_id,
         filetab,
         outdoctab[].

  REFRESH: outdoctab.

* Get document types
  CLEAR: doctype_list,
         doctype_value.
  REFRESH: doctype_list.

  GET PARAMETER ID 'BUK' FIELD bukrs.

* Set debit/credit indicator
  rad_debit = 'X'.
  rad_credit = ''.

* Set receiving date
  rdate = sy-datum.

* Get workflow approval type
  SELECT SINGLE wf_type FROM /nrk/apaydtype INTO wf_type
     WHERE ar_object EQ wa_hd-ar_object
       AND bukrs EQ wa_hd-bukrs.

  CALL SCREEN '0205'.

  IF NOT wa_hd-apayno IS INITIAL.
    apayno = wa_hd-apayno.
    cancel = ' '.
  ELSE.
    cancel = 'X'.
  ENDIF.

ENDFUNCTION.
