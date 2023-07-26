FUNCTION /nrk/apay_get_buyer.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"  EXPORTING
*"     REFERENCE(USNAM) TYPE  USNAM
*"     REFERENCE(SMTP_ADDR) TYPE  AD_SMTPADR
*"----------------------------------------------------------------------

  DATA: ebeln  LIKE ekko-ebeln,
        errors TYPE rpbenerr OCCURS 0,
        wa_hd  LIKE /nrk/apayhd.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayno.

  IF sy-subrc EQ 0
    AND NOT wa_hd-ebeln IS INITIAL.

    SELECT SINGLE ernam FROM ekko INTO usnam
      WHERE ebeln EQ wa_hd-ebeln.

    IF sy-subrc EQ 0.

      wa_hd-ernam = usnam.

      CALL FUNCTION 'HR_FBN_GET_USER_EMAIL_ADDRESS'
        EXPORTING
          user_id       = usnam
          reaction      = sy-msgty
        IMPORTING
          email_address = smtp_addr
        TABLES
          error_table   = errors.
    ENDIF.

    IF NOT smtp_addr IS INITIAL.
      MOVE smtp_addr TO wa_hd-ext_approver.
      UPDATE /nrk/apayhd FROM wa_hd.
    ENDIF.

  ELSE.
*** message no valid PO found
  ENDIF.

ENDFUNCTION.
