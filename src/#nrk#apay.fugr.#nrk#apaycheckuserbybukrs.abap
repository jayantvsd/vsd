FUNCTION /nrk/apaycheckuserbybukrs.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(USER) TYPE  SYST-UNAME
*"  EXPORTING
*"     REFERENCE(ALL_DOCUMENTS) TYPE  BOOLE-BOOLE
*"     REFERENCE(LIMITED_DOCUMENTS) TYPE  BOOLE-BOOLE
*"  TABLES
*"      TAB_USER STRUCTURE  /NRK/APAYUSER
*"      RANGE STRUCTURE  RSIS_S_RANGE
*"  EXCEPTIONS
*"      NO_PERMISSION
*"----------------------------------------------------------------------

  DATA: wa_config LIKE /nrk/apayconfig.

**Begin of changes for Copmany Code autho check.
****?/Athma 10/02/2021
  TYPES: BEGIN OF ty_str_auth_bukrs.
          INCLUDE STRUCTURE usvalues.
  TYPES: END OF ty_str_auth_bukrs,

    ty_tab_auth_bukrs TYPE STANDARD TABLE OF ty_str_auth_bukrs.

  DATA: tab_auth_bukrs TYPE ty_tab_auth_bukrs.
  FIELD-SYMBOLS <fs_auth_bukrs> TYPE ty_str_auth_bukrs.

*  RANGES r_bukrs FOR /nrk/apayuser-bukrs.
  CONSTANTS: c_auth_obj TYPE ust12-objct VALUE 'F_BKPF_BUK',
             c_apay TYPE /nrk/apaykey VALUE 'APAY',
             c_ex_cc_chk TYPE /nrk/apaykey VALUE 'EX_CC_CHK'.

  DATA l_auth_chk.

  SELECT SINGLE val1 FROM /nrk/apayconfig INTO l_auth_chk
    WHERE key1 = c_apay
      AND key2 = c_ex_cc_chk.

  IF l_auth_chk = 'X'.
    CALL FUNCTION 'EFG_USER_AUTH_FOR_OBJ_GET'
      EXPORTING
        x_client       = sy-mandt
        x_uname        = user
        x_object       = c_auth_obj
      TABLES
        yt_usvalues    = tab_auth_bukrs
      EXCEPTIONS
        user_not_found = 1
        not_authorized = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    DELETE tab_auth_bukrs WHERE field NE 'BUKRS'.
    IF tab_auth_bukrs[] IS INITIAL.
      MESSAGE e000(znk_invoice)
      WITH 'You are not Authorised' 'For the selected' 'Company' 'Codes'.
    ENDIF.
    SORT tab_auth_bukrs.
    DELETE ADJACENT DUPLICATES FROM tab_auth_bukrs COMPARING ALL FIELDS.
    READ TABLE tab_auth_bukrs ASSIGNING <fs_auth_bukrs> WITH KEY von = '*'.
    IF sy-subrc NE 0.
      LOOP AT tab_auth_bukrs ASSIGNING <fs_auth_bukrs>.
        range-low = <fs_auth_bukrs>-von.
        range-high = <fs_auth_bukrs>-bis.
        range-sign = 'I'.
        IF <fs_auth_bukrs>-bis eq space.
          range-option = 'EQ'.
        ELSE.
          range-option = 'BT'.
        ENDIF.
        APPEND range.
        CLEAR range.
      ENDLOOP.
    ELSE.
      range-low = '*'.
*        range-high = <fs_auth_bukrs>-bis.
*        range-sign = 'I'.
*        IF NOT <fs_auth_bukrs>-bis IS INITIAL.
*          range-option = 'EQ'.
      APPEND range.
      CLEAR range.

    ENDIF.
  ENDIF.
*End of changes for Copmany Code autho check.
****?/Athma 10/02/2021


* Check permission configuration active
  SELECT SINGLE * FROM /nrk/apayconfig INTO wa_config
    WHERE key1 EQ 'APAYPERM'
      AND key2 EQ 'BUKRS'.

  IF sy-subrc NE 0
    OR wa_config-val1 NE 'X'.
    all_documents = 'X'.
    limited_documents = ' '.
    EXIT.
  ENDIF.

*Begin of changes for Copmany Code autho check.
****?/Athma 10/02/2021
* Check user table
*  SELECT * FROM /nrk/apayuser INTO TABLE tab_user
*    WHERE otype EQ 'US'
*      AND objid EQ user.
*
*  IF sy-subrc NE 0.
*    RAISE no_permission.
*  ENDIF.
*End of changes for Copmany Code autho check.
****?/Athma 10/02/2021



  LOOP AT tab_user.
    IF tab_user-bukrs IS INITIAL.
      all_documents = 'X'.
      limited_documents = ' '.
*     EXIT.
    ELSE.
      all_documents = ' '.
      limited_documents = 'X'.
    ENDIF.
  ENDLOOP.

  IF all_documents EQ 'X'.
    REFRESH: tab_user.
  ENDIF.

ENDFUNCTION.
