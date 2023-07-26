FUNCTION /nrk/apayapi_bapi_store_appr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(APPROVER) TYPE  /NRK/APAYAPISTOREAPPR OPTIONAL
*"  TABLES
*"      MESSAGES STRUCTURE  /NRK/APAYAPI_MESSAGES
*"----------------------------------------------------------------------

  DATA: wa_hd       LIKE /nrk/apayhd,
        wa_approver LIKE /nrk/apayappr,
        t_approver  LIKE /nrk/apayappr OCCURS 0.

* Clean up
  CLEAR: wa_hd, wa_approver, t_approver.

* Get main APay record
  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd WHERE apayno = approver-apayno.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '003'.
    messages-msg_text = text-003.
    APPEND messages.
    EXIT.
  ENDIF.

* Get all levels from inbound approver
  TRANSLATE approver-extuser_in TO UPPER CASE.
  SELECT * FROM /nrk/apayappr INTO TABLE t_approver WHERE apayno EQ approver-apayno
                                                    AND ext_user EQ approver-extuser_in.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '036'.
    messages-msg_text = text-036.
    APPEND messages.
    EXIT.
  ENDIF.

* Update records with new approver
  LOOP AT t_approver INTO wa_approver.
* Get full info for new approver
    SELECT SINGLE objid smtp_addr INTO (wa_approver-sap_user, wa_approver-smtp_addr) FROM /nrk/apayuser WHERE extuser EQ approver-extuser_out.
    IF sy-subrc EQ 0.
      wa_approver-ext_user = approver-extuser_out.
      TRANSLATE wa_approver-ext_user TO UPPER CASE.
      MODIFY /nrk/apayappr FROM wa_approver.
    ENDIF.
  ENDLOOP.

* Update APay table
  wa_hd-ext_approver = approver-extuser_out.
  TRANSLATE wa_hd-ext_approver TO UPPER CASE.
  UPDATE /nrk/apayhd FROM wa_hd.

* Update manager
  CALL FUNCTION '/NRK/APAYDETERMINEAPPROVER'
    EXPORTING
      apayno                          = wa_hd-apayno
*   SAP_USER                        =
*   EXT_USER                        =
* IMPORTING
*   APPROVAL                        =
    EXCEPTIONS
      hierarchy_creation_failed       = 1
      record_not_found                = 2
      user_not_found                  = 3
      OTHERS                          = 4.

  IF sy-subrc NE 0.
    messages-msg_type = 'E'.
    messages-msg_nbr = '033'.
    messages-msg_text = text-033.
    APPEND messages.
    EXIT.
  ENDIF.

* Commit
  COMMIT WORK.

  messages-msg_type = 'I'.
  messages-msg_nbr = '039'.
  messages-msg_text = text-039.
  APPEND messages.


ENDFUNCTION.
