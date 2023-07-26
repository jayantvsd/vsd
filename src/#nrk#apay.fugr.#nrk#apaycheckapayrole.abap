FUNCTION /nrk/apaycheckapayrole.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"     REFERENCE(USER) TYPE  SYST-UNAME
*"     REFERENCE(ROLE) TYPE  CHAR3
*"  EXCEPTIONS
*"      NO_ROLE_DEFINED
*"      NOT_AUTHORIZED
*"----------------------------------------------------------------------

  DATA: wa_user LIKE /nrk/apayuser.

* Role options (SAP user)
* APP = AP processor
* APR = AP approver
* BUY = Buyer

  CLEAR: wa_user.

  IF role = 'APP'. " AP processor

    SELECT SINGLE * FROM /nrk/apayuser INTO wa_user
      WHERE objid EQ user
        AND processor EQ 'X'.

    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
        EXPORTING
          titel     = text-200
          textline1 = text-201.
      RAISE not_authorized.
    ENDIF.

  ELSEIF role = 'APR'. " AP approver

    SELECT SINGLE * FROM /nrk/apayuser INTO wa_user
      WHERE objid EQ user
        AND approver EQ 'X'.

    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
        EXPORTING
          titel     = text-200
          textline1 = text-202.
      RAISE not_authorized.
    ENDIF.

  ELSEIF role = 'BUY'. " Buyer

    SELECT SINGLE * FROM /nrk/apayuser INTO wa_user
      WHERE objid EQ user
        AND approver EQ 'X'.

    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
        EXPORTING
          titel     = text-200
          textline1 = text-203.
      RAISE not_authorized.
    ENDIF.

  ELSE.
    RAISE no_role_defined.
  ENDIF.

ENDFUNCTION.
