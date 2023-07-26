FUNCTION /nrk/apayapi_bapi_get_approver.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(USERDATA) TYPE  /NRK/APAYAPIUSER OPTIONAL
*"  TABLES
*"      APPROVER STRUCTURE  /NRK/APAYAPIAPPROVER OPTIONAL
*"----------------------------------------------------------------------

  DATA: wa_user LIKE /nrk/apayuser,
        wa_approver LIKE /nrk/apayappr.

  CLEAR: wa_user,
         wa_approver.

  TRANSLATE userdata-extuser TO UPPER CASE.

  SELECT SINGLE * FROM /nrk/apayappr INTO wa_approver
    WHERE apayno EQ userdata-apayno
      AND ext_user EQ userdata-extuser
      AND approved NE 'X'.

*  SELECT * FROM /nrk/apayuser INTO wa_user
*    WHERE otype EQ 'EX'
*      AND approver EQ 'X'.

  SELECT * FROM /nrk/apayuser INTO wa_user
    WHERE approver EQ 'X'.

    TRANSLATE wa_user-extuser TO UPPER CASE.
    READ TABLE approver WITH KEY extuser = wa_user-extuser.

    IF sy-subrc NE 0.
      approver-extuser = wa_user-extuser.
      approver-fullname = wa_user-fullname.
      APPEND approver.
    ENDIF.
  ENDSELECT.

*  DELETE ADJACENT DUPLICATES FROM approver COMPARING extuser.

  SORT approver ASCENDING BY fullname.

ENDFUNCTION.
