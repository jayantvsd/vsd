*****           Implementation of object type /NRK/APAY            *****
INCLUDE <object>.
begin_data object. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF key,
      apayrecordid LIKE /nrk/apayhd-apayno,
  END OF key,
      parkeddocument TYPE swc_object,
      image TYPE swc_object,
      statusdescription(255),
      approveremail TYPE /nrk/apayuser-smtp_addr,
      documenttypedescription TYPE toasp-objecttext,
      vendoremailaddress TYPE adr6-smtp_addr,
      _/nrk/apayhd LIKE /nrk/apayhd.
end_data object. " Do not change.. DATA is generated

begin_method imageassignearly changing container.

DATA: docclass         TYPE toadd-doc_type,
      documenttype     TYPE toav0-ar_object,
      description      TYPE toasp-objecttext,
      orgobject        TYPE wfsyst-agent,
      apaycenterobject TYPE swc_object,
      contentrepid     TYPE toa01-archiv_id,
      documentid       TYPE toa01-arc_doc_id,
      apayrecordstatus TYPE /nrk/apayhd-status,
      apayno           LIKE /nrk/apayhd-apayno,
      user             LIKE wfsyst-initiator,
      userid           TYPE  uname.

CLEAR: apayno, userid.

swc_get_element container 'DOCCLASS' docclass.
swc_get_element container 'DOCUMENTTYPE' documenttype.
swc_get_element container 'DESCRIPTION' description.
swc_get_element container 'ORGOBJECT' orgobject.
swc_get_element container 'ContentRepID' contentrepid.
swc_get_element container 'DocumentID' documentid.
swc_get_element container 'APayRecordStatus' apayrecordstatus.
swc_get_element container 'User' user.

MOVE user+2(12) TO userid.

CALL FUNCTION '/NRK/APAYCREATERECORD'
  EXPORTING
    archiv_id                      = contentrepid
    arc_doc_id                     = documentid
    ar_object                      = documenttype
    sap_object                     = '/NRK/APAY'
    cdate                          = sy-datum
    ctime                          = sy-uzeit
    status                         = apayrecordstatus
*   PRIORITY                       = 3
*   HEADER                         =
    userid                         = userid
  IMPORTING
    apayno                         = apayno
  EXCEPTIONS
    record_creation_failed         = 1
    number_not_created             = 2
    record_assignment_failed       = 3
    OTHERS                         = 4.

IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

swc_create_object apaycenterobject '/NRK/APAY' apayno.
swc_set_element container 'APayCenterObject' apaycenterobject.

end_method.

begin_method updatewithbkpf changing container.

DATA: companycode TYPE bkpf-bukrs,
      documentnumber TYPE bkpf-belnr,
      fiscalyear TYPE bkpf-gjahr,
      apayrecordstatus TYPE /nrk/apayhd-status,
      username  TYPE wfsyst-initiator,
      username2 TYPE /nrk/apayhd-uname,
      documenttype TYPE /nrk/apayhd-ar_object.

swc_get_element container 'CompanyCode' companycode.
swc_get_element container 'DocumentNumber' documentnumber.
swc_get_element container 'FiscalYear' fiscalyear.
swc_get_element container 'APayRecordStatus' apayrecordstatus.
swc_get_element container 'UserName' username.
swc_get_element container 'DocumentType' documenttype.

MOVE username+2 TO username2.

CALL FUNCTION '/NRK/UPDATEAPAYWITHBKPF'
  EXPORTING
    apayno               = object-key-apayrecordid
    bukrs                = companycode
    belnr                = documentnumber
    gjahr                = fiscalyear
    status               = apayrecordstatus
    status_post          = '1900'
    status_park          = apayrecordstatus
    ar_object            = documenttype
    uname                = username2
  EXCEPTIONS
    no_apay_record_found = 1
    bkpf_error           = 2
    OTHERS               = 3.

IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

end_method.

begin_method updatewithbus2081 changing container.

DATA: invoicedocumentno TYPE rbkp-belnr,
      fiscalyear TYPE rbkp-gjahr,
      apayrecordstatus TYPE /nrk/apayhd-status,
      username TYPE /nrk/apayhd-uname,
      username2 TYPE wfsyst-initiator,
      documenttype TYPE /nrk/apayhd-ar_object.

swc_get_element container 'InvoiceDocumentNo' invoicedocumentno.
swc_get_element container 'FiscalYear' fiscalyear.
swc_get_element container 'APayRecordStatus' apayrecordstatus.
swc_get_element container 'UserName' username2.
swc_get_element container 'DocumentType' documenttype.

MOVE username2+2 TO username.

CALL FUNCTION '/NRK/UPDATEAPAYWITHBUS2081'
  EXPORTING
    apayno               = object-key-apayrecordid
    belnr                = invoicedocumentno
    gjahr                = fiscalyear
    status               = apayrecordstatus
    ar_object            = documenttype
    uname                = username
  EXCEPTIONS
    no_apay_record_found = 1
    bkpf_error           = 2
    OTHERS               = 3.

IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

end_method.

begin_method updatestatus changing container.
DATA:
      status         TYPE /nrk/apayhis-status,
      statusdate     TYPE /nrk/apayhis-sdate,
      statustime     TYPE /nrk/apayhis-stime,
      sapuser        TYPE wfsyst-initiator,
      sapuser2       TYPE /nrk/apayhis-suser,
      externaluserid TYPE /nrk/apayhis-sextuser,
      useremail      LIKE /nrk/apayuser-smtp_addr,
      wf_id          TYPE sww_wiid.

DATA: wa_hd LIKE /nrk/apayhd,
      wa_his LIKE /nrk/apayhis,
      t_his  LIKE /nrk/apayhis OCCURS 0,
      lines TYPE i.

CLEAR: wa_his,
        t_his[].

swc_get_element container 'Status' status.
swc_get_element container 'StatusDate' statusdate.
swc_get_element container 'StatusTime' statustime.
swc_get_element container 'SAPUser' sapuser.
swc_get_element container 'ExternalUserID' externaluserid.
swc_get_element container 'UserEmail' useremail.
swc_get_element container 'WorkflowID' wf_id.

MOVE sapuser+2(12) TO sapuser2.

* Update Header
SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
  WHERE apayno EQ object-key-apayrecordid.

IF sy-subrc EQ 0.
  wa_hd-status = status.
  wa_hd-lastchange = sy-datum.

  IF NOT wf_id IS INITIAL.
    wa_hd-workflowid = wf_id.
  ENDIF.

  TRANSLATE externaluserid TO UPPER CASE.
  TRANSLATE useremail TO LOWER CASE.

* Update Status History
  SELECT * FROM /nrk/apayhis INTO TABLE t_his
    WHERE apayno EQ object-key-apayrecordid.

  DESCRIBE TABLE t_his LINES lines.

  lines = lines + 1.

  wa_his-apayno = object-key-apayrecordid.
  wa_his-item = lines.
  wa_his-status = status.
  wa_his-sdate = statusdate.
  wa_his-stime = statustime.
  wa_his-suser = sapuser2.
  wa_his-sextuser = externaluserid.

  SELECT SINGLE fullname FROM /nrk/apayuser INTO wa_his-sname
    WHERE otype EQ 'US'
      AND objid EQ wa_his-suser.

  IF sy-subrc NE 0
    OR wa_his-suser IS INITIAL.
    SELECT SINGLE fullname FROM /nrk/apayuser INTO wa_his-sname
      WHERE extuser EQ wa_his-sextuser.
*       AND extuser EQ wa_his-sextuser.

    IF sy-subrc NE 0.
      SELECT SINGLE fullname FROM /nrk/apayuser INTO wa_his-sname
        WHERE smtp_addr EQ useremail.
    ENDIF.
  ENDIF.

  IF wa_his-sname IS INITIAL.
    wa_his-sname = text-903.
  ENDIF.

  INSERT /nrk/apayhis FROM wa_his.

* Update last change
  wa_hd-lastchange = statusdate.
  wa_hd-lastchanget = statustime.

* Update header row
  MODIFY /nrk/apayhd FROM wa_hd.

* Update workflow log
  IF NOT wf_id IS INITIAL.
    CALL FUNCTION '/NRK/APAYUPDATEWORKFLOWLOG'
      EXPORTING
        apayno                     = wa_hd-apayno
        workflow_id                = wf_id
      EXCEPTIONS
        workflow_log_update_failed = 1
        OTHERS                     = 2.

    IF sy-subrc <> 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

ENDIF.

end_method.

get_property parkeddocument changing container.

DATA: belnr LIKE bkpf-belnr,
      bukrs LIKE bkpf-bukrs,
      gjahr LIKE bkpf-gjahr,
      bstat LIKE bkpf-bstat,
      key(18) TYPE c.

SELECT SINGLE belnr bukrs gjahr bstat FROM /nrk/apayhd
  INTO (belnr, bukrs,gjahr, bstat)
  WHERE apayno EQ object-key-apayrecordid.

IF bstat = 'V'.
  CONCATENATE bukrs belnr gjahr INTO key.
  swc_create_object object-parkeddocument 'FIPP' key.
  swc_set_element container 'ParkedDocument' object-parkeddocument.
ENDIF.

end_property.

begin_method checkforduplicate changing container.
DATA:
      duplicatefound TYPE boole.

CALL FUNCTION '/NRK/APAYCHECK_DUPLICATE'
  EXPORTING
    apayno                  = object-key-apayrecordid
  IMPORTING
    duplicate               = duplicatefound
  EXCEPTIONS
    no_record_found         = 1
    missing_validation_data = 2
    OTHERS                  = 3.

IF sy-subrc <> 0.
  IF sy-subrc EQ 2. "missing data
    exit_return 9001 'Missing data' space space space.
  ELSE. " other exception
    exit_return 9002 'Other exception' space space space.
  ENDIF.
ELSE.

  swc_set_element container 'DuplicateFound' duplicatefound.
ENDIF.
end_method.

TABLES /nrk/apayhd.
*
get_table_property /nrk/apayhd.
DATA subrc LIKE sy-subrc.
* Fill TABLES /NRK/APAYHD to enable Object Manager Access to Table
* Properties
PERFORM select_table_/nrk/apayhd USING subrc.
IF subrc NE 0.
  exit_object_not_found.
ENDIF.
end_property.
*
* Use Form also for other(virtual) Properties to fill TABLES /NRK/APAYHD
FORM select_table_/nrk/apayhd USING subrc LIKE sy-subrc.
* Select single * from /NRK/APAYHD, if OBJECT-_/NRK/APAYHD is initial
  IF object-_/nrk/apayhd-mandt IS INITIAL
  AND object-_/nrk/apayhd-apayno IS INITIAL.
    SELECT SINGLE * FROM /nrk/apayhd CLIENT SPECIFIED
        WHERE mandt = sy-mandt
        AND apayno = object-key-apayrecordid.
    subrc = sy-subrc.
    IF subrc NE 0. EXIT. ENDIF.
    object-_/nrk/apayhd = /nrk/apayhd.
  ELSE.
    subrc = 0.
    /nrk/apayhd = object-_/nrk/apayhd.
  ENDIF.
ENDFORM.                    "SELECT_TABLE_/NRK/APAYHD

begin_method indexsharepoint changing container.

DATA:
   http_client TYPE REF TO if_http_client,
   url TYPE char255,
   wa_url TYPE string.

DATA: object_id LIKE  swr_struct-object_key.

swc_get_element container 'WebService' url.
MOVE object-key-apayrecordid TO object_id.

* Build the URL from the object type and ID
CONCATENATE
  url
  '?type='
  '%2FNRK%2FAPAY'
  '&id=' object_id
INTO wa_url.

* Make the HTTP POST request to the target system.
CALL METHOD cl_http_client=>create_by_url
  EXPORTING
    url    = wa_url
  IMPORTING
    client = http_client.

CALL METHOD http_client->request->set_method
  EXPORTING
    method = 'POST'.
CALL METHOD http_client->request->set_header_field
  EXPORTING
    name  = 'Content-Type'
    value = 'text/plain; charset=utf-8'.

* http_client->send( ).
* http_client->receive( ).
* http_client->close( ).

CALL METHOD http_client->send
  EXCEPTIONS
    http_communication_failure = 1
    http_invalid_state         = 2
    http_processing_failed     = 3
    http_invalid_timeout       = 4.

IF sy-subrc NE 0.
  exit_return 9001 'Send error' space space space.
ENDIF.

CALL METHOD http_client->receive
  EXCEPTIONS
    http_communication_failure = 1
    http_invalid_state         = 2
    http_processing_failed     = 3.

IF sy-subrc NE 0.
  exit_return 9001 'Receive error' space space space.
ENDIF.

CALL METHOD http_client->close
  EXCEPTIONS
    http_invalid_state = 1.

IF sy-subrc NE 0.
  exit_return 9001 'Close error' space space space.
ENDIF.

end_method.

begin_method createexception changing container.

DATA: status LIKE /nrk/apayhd-status,
      role   LIKE /nrk/apayvar-role.

swc_get_element container 'Role' role.

CALL FUNCTION '/NRK/APAYCHECKAPAYROLE'
  EXPORTING
    apayno          = object-key-apayrecordid
    user            = sy-uname
    role            = role
  EXCEPTIONS
    no_role_defined = 1
    not_authorized  = 2
    OTHERS          = 3.

IF sy-subrc EQ 2.
  exit_cancelled.
ENDIF.

CALL FUNCTION '/NRK/APAYEXCEPTION'
  EXPORTING
    iapayno = object-key-apayrecordid
  IMPORTING
    eexcept = status.

IF NOT status IS INITIAL.
  swc_set_element container 'Exception' status.
ELSE.
  exit_cancelled.
ENDIF.

end_method.

begin_method determineapprover changing container.

DATA: sap_user LIKE /nrk/apayuser-objid,
      ext_user LIKE /nrk/apayuser-extuser,
      approval LIKE boole-boole.

swc_get_element container 'SAPUser' sap_user.
swc_get_element container 'ExternalUser' ext_user.

CALL FUNCTION '/NRK/APAYDETERMINEAPPROVER'
  EXPORTING
    apayno                    = object-key-apayrecordid
    sap_user                  = sap_user
    ext_user                  = ext_user
  IMPORTING
    approval                  = approval
  EXCEPTIONS
    hierarchy_creation_failed = 1
    record_not_found          = 2
    user_not_found            = 3
    OTHERS                    = 4.

CASE sy-subrc.
  WHEN 0.

  WHEN 1.
    exit_return 9001 'No Manager found.' space space space.
  WHEN 2.
    exit_return 9001 'Record #' object-key-apayrecordid
      'not found.' space.
  WHEN 3.
    exit_return 9001 'User' ext_user 'not found.' space.
  WHEN 4.
    exit_return 9001 'General error.' space space space.
  WHEN OTHERS.
    exit_return 9001 'Other error.' space space space.
ENDCASE.

swc_set_element container 'ApprovalRequired' approval.

end_method.

begin_method checkapproval changing container.
DATA:
      wfuser           TYPE wfsyst-initiator,
      sapuser          TYPE /nrk/apayuser-objid,
      externaluser     TYPE /nrk/apayuser-extuser,
      approvalcomplete LIKE boole-boole.

swc_get_element container 'SAPUser' wfuser.
swc_get_element container 'ExternalUser' externaluser.

sapuser = wfuser+2(12).

CALL FUNCTION '/NRK/APAYCHECKAPPROVAL'
  EXPORTING
    apayno            = object-key-apayrecordid
    sap_user          = sapuser
    ext_user          = externaluser
  IMPORTING
    approval_complete = approvalcomplete.

swc_set_element container 'ApprovalComplete' approvalcomplete.

end_method.

begin_method clearapproval changing container.
DATA:
      cleared TYPE boole-boole.

CALL FUNCTION '/NRK/APAYCLEARAPPROVAL'
  EXPORTING
    apayno  = object-key-apayrecordid
  IMPORTING
    cleared = cleared.

swc_set_element container 'Cleared' cleared.

end_method.

begin_method createfipp changing container.

DATA: parked     TYPE boole-boole,
      fipp       TYPE swc_object,
      object_key(18) TYPE c,
      wa_hd      LIKE /nrk/apayhd,
      items      LIKE /nrk/apayitems OCCURS 0,
      bdcmsgcoll LIKE bdcmsgcoll OCCURS 0,
      belnr      LIKE /nrk/apayhd-belnr,
      bukrs      LIKE /nrk/apayhd-bukrs,
      gjahr      LIKE /nrk/apayhd-gjahr.

SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
  WHERE apayno EQ object-key-apayrecordid.

CALL FUNCTION '/NRK/APAYCREATEFIPP'
  EXPORTING
    apayhd         = wa_hd
    mode           = 'N'
  IMPORTING
    parked         = parked
    belnr          = belnr
    bukrs          = bukrs
    gjahr          = gjahr
  TABLES
    items          = items
    bdcmsgcoll     = bdcmsgcoll
  EXCEPTIONS
    parking_failed = 1
    OTHERS         = 2.

swc_set_element container 'Parked' parked.
swc_set_table container 'BDCMSGCOLL' bdcmsgcoll.

IF NOT belnr IS INITIAL
  AND NOT bukrs IS INITIAL
  AND NOT gjahr IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = belnr
    IMPORTING
      output = belnr.

  CONCATENATE bukrs belnr gjahr INTO object_key.
  swc_create_object fipp 'FIPP' object_key.
  swc_set_element container 'FIPP' fipp.
ENDIF.

end_method.

begin_method createfippbyuser changing container.

DATA: parked TYPE boole-boole,
      fipp TYPE swc_object,
      object_key(18) TYPE c,
      wa_hd      LIKE /nrk/apayhd,
      items      LIKE /nrk/apayitems OCCURS 0,
      bdcmsgcoll LIKE bdcmsgcoll OCCURS 0,
      belnr      LIKE /nrk/apayhd-belnr,
      bukrs      LIKE /nrk/apayhd-bukrs,
      gjahr      LIKE /nrk/apayhd-gjahr.

SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
  WHERE apayno EQ object-key-apayrecordid.

CALL FUNCTION '/NRK/APAYCREATEFIPP'
  EXPORTING
    apayhd         = wa_hd
    mode           = 'A'
  IMPORTING
    parked         = parked
    belnr          = belnr
    bukrs          = bukrs
    gjahr          = gjahr
  TABLES
    items          = items
    bdcmsgcoll     = bdcmsgcoll
  EXCEPTIONS
    parking_failed = 1
    OTHERS         = 2.

IF parked EQ 'X'.
  swc_set_element container 'Parked' parked.
ELSE.
  exit_cancelled.
ENDIF.

IF NOT belnr IS INITIAL
  AND NOT bukrs IS INITIAL
  AND NOT gjahr IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = belnr
    IMPORTING
      output = belnr.

  CONCATENATE bukrs belnr gjahr INTO object_key.
  swc_create_object fipp 'FIPP' object_key.
  swc_set_element container 'FIPP' fipp.
ENDIF.

end_method.

begin_method deletefipp changing container.
DATA:
      deleted TYPE boole-boole,
      wa_hd LIKE /nrk/apayhd.

CLEAR: wa_hd.

SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
  WHERE apayno EQ object-key-apayrecordid.

IF sy-subrc EQ 0.

  CALL FUNCTION 'PRELIMINARY_POSTING_DOC_DELETE'
    EXPORTING
*   BELNN                    = ' '
      belnr                    = wa_hd-belnr
*   BSTAT                    = ' '
      bukrs                    = wa_hd-bukrs
*   GJAHN                    = 0
      gjahr                    = wa_hd-gjahr
*   NO_BSIP                  =
*   SPLIT_DATA               =
    EXCEPTIONS
      document_not_found       = 1
      update_error             = 2
      OTHERS                   = 3.

  IF sy-subrc EQ 0.
    deleted = 'X'.
    swc_set_element container 'DELETED' deleted.
  ELSE.
    deleted = ' '.
  ENDIF.

ENDIF.

end_method.

get_property statusdescription changing container.

DATA: apayno LIKE /nrk/apayhd-apayno,
      status LIKE /nrk/apayhd-status,
      sdescr LIKE /nrk/apaysdef-sdescr.

SELECT SINGLE status FROM /nrk/apayhd INTO status
  WHERE apayno EQ object-key-apayrecordid.


SELECT SINGLE sdescr FROM /nrk/apaysdef INTO sdescr
  WHERE status EQ status
    AND langu EQ sy-langu.

IF sy-subrc EQ 0.

  MOVE sdescr TO object-statusdescription.

  swc_set_element container 'StatusDescription'
       object-statusdescription.

ENDIF.

end_property.

begin_method createbkpf changing container.
DATA:
      posted     TYPE boole-boole,
      bkpf       TYPE swc_object,
      bdcmsgcoll LIKE bdcmsgcoll OCCURS 0,
      items      LIKE /nrk/apayitems OCCURS 0,
      wa_hd      LIKE /nrk/apayhd,
      bukrs      LIKE /nrk/apayhd-bukrs,
      belnr      LIKE /nrk/apayhd-belnr,
      gjahr      LIKE /nrk/apayhd-gjahr,
      object_key(18) TYPE c.

SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
  WHERE apayno EQ object-key-apayrecordid.

CALL FUNCTION '/NRK/APAYCREATEBKPF'
  EXPORTING
    apayhd         = wa_hd
    mode           = 'N'
  IMPORTING
    posted         = posted
    belnr          = belnr
    bukrs          = bukrs
    gjahr          = gjahr
  TABLES
    items          = items
    bdcmsgcoll     = bdcmsgcoll
  EXCEPTIONS
    posting_failed = 1
    OTHERS         = 2.


swc_set_element container 'Posted' posted.
swc_set_element container 'BKPF' bkpf.
swc_set_table container 'BDCMSGCOLL' bdcmsgcoll.

IF NOT belnr IS INITIAL
  AND NOT bukrs IS INITIAL
  AND NOT gjahr IS INITIAL.

  CONCATENATE bukrs belnr gjahr INTO object_key.
  swc_create_object bkpf 'BKPF' object_key.
  swc_set_element container 'BKPF' bkpf.
ENDIF.

end_method.

begin_method createparkedbus2081 changing container.
DATA:
      bus2081        TYPE swc_object,
      messages       LIKE bapiret2 OCCURS 0,
      parked         TYPE boole-boole,
      liv_belnr      TYPE re_belnr,
      liv_gjahr      TYPE gjahr,
      wa_hd          LIKE /nrk/apayhd,
      object_key(14) TYPE c.

wa_hd-apayno = object-key-apayrecordid.

CALL FUNCTION '/NRK/APAYPARKBUS2081'
  EXPORTING
    apayhd                 = wa_hd
  IMPORTING
    parked                 = parked
    liv_belnr              = liv_belnr
    liv_gjahr              = liv_gjahr
  TABLES
*   items                  =
    tab_return             = messages
  EXCEPTIONS
    parking_failed         = 1
    record_not_found       = 2
    OTHERS                 = 3.

IF sy-subrc <> 0.
  parked = ' '.
ENDIF.

swc_set_element container 'Parked' parked.
swc_set_table container 'Messages' messages.

IF parked EQ 'X'.
  CONCATENATE liv_gjahr liv_belnr INTO object_key.
  swc_create_object bus2081 'BUS2081' object_key.
  swc_set_element container 'BUS2081' bus2081.
ENDIF.

end_method.

begin_method povalidation changing container.

DATA: result_gr(1) TYPE c,
      result_inv(1) TYPE c,
      result(1) TYPE c.

CALL FUNCTION '/NRK/APAYCHECKGRFORPO'
  EXPORTING
    apayno     = object-key-apayrecordid
  IMPORTING
    result_gr  = result_gr
    result_inv = result_inv
    RESULT     = RESULT.


swc_set_element container 'RESULT_GR' result_gr.
swc_set_element container 'RESULT_INV' result_inv.
swc_set_element container 'RESULT' result.

end_method.

begin_method createbus2081byuser changing container.

DATA: posted TYPE boole-boole,
      parked TYPE boole-boole,
      bus2081 TYPE swc_object,
      tcode   TYPE sytcode,
      liv_belnr LIKE /nrk/apayhd-liv_belnr,
      liv_gjahr LIKE /nrk/apayhd-liv_gjahr,
      object_key(14) TYPE c.

swc_get_element container 'TCODE' tcode.

CALL FUNCTION '/NRK/APAYCREATEBUS2081'
  EXPORTING
    apayno                 = object-key-apayrecordid
    mode                   = 'A'
    tcode                  = tcode
  IMPORTING
    parked                 = parked
    posted                 = posted
    liv_belnr              = liv_belnr
    liv_gjahr              = liv_gjahr
* TABLES
*   ITEMS                  =
*   BDCMSGCOLL             =
  EXCEPTIONS
    parking_failed         = 1
    record_not_found       = 2
    OTHERS                 = 3.

IF sy-subrc <> 0.
  EXIT.
ENDIF.

IF NOT posted IS INITIAL
OR NOT parked IS INITIAL.
  swc_set_element container 'Posted' posted.
  swc_set_element container 'Parked' parked.

  CONCATENATE liv_belnr liv_gjahr INTO object_key.
  swc_create_object bus2081 'BUS2081' object_key.
  swc_set_element container 'BUS2081' bus2081.
ELSE.
  exit_cancelled.
ENDIF.

end_method.

get_property approveremail changing container.

SELECT SINGLE smtp_addr FROM /nrk/apayappr
  INTO object-approveremail
  WHERE apayno EQ object-key-apayrecordid
    AND approved EQ space.

IF sy-subrc EQ 0.

  swc_set_element container 'ApproverEmail' object-approveremail.

ENDIF.

end_property.

get_property documenttypedescription changing container.

DATA: l_ar_object LIKE toasp-ar_object.

SELECT SINGLE ar_object FROM /nrk/apayhd
  INTO l_ar_object
  WHERE apayno = object-key-apayrecordid.

IF sy-subrc EQ 0.

  SELECT SINGLE objecttext FROM toasp
    INTO object-documenttypedescription
    WHERE ar_object = l_ar_object
      AND language = sy-langu.

  IF sy-subrc EQ 0.

    swc_set_element container 'DocumentTypeDescription'
         object-documenttypedescription.

  ENDIF.
ENDIF.

end_property.

begin_method checkpurchaseorder changing container.
* Check if purchase order is valid
* 0 - Purchase order is valid
* 1 - Invalid purchase order
* 2 - Vendor does not match
DATA: ls_hd     TYPE /nrk/apayhd,
      ls_ekko   TYPE ekko,
      lv_valid  TYPE c.

* Get APay header data
SELECT SINGLE * FROM /nrk/apayhd INTO ls_hd
               WHERE apayno EQ object-key-apayrecordid.

IF sy-subrc NE 0.
  exit_return 9001 text-901 object-key-apayrecordid space space.
ENDIF.

* Get purchase order data
IF NOT ls_hd-ebeln IS INITIAL.
* Check for validity
  SELECT SINGLE * FROM ekko INTO ls_ekko WHERE ebeln EQ ls_hd-ebeln.
  IF sy-subrc NE 0.
* Purchase order is invalid
    lv_valid = '1'.
  ELSE.
* Check if vendor matches
    IF NOT ls_hd-lifnr IS INITIAL AND ls_hd-lifnr NE ls_ekko-lifnr.
* Vendor is indexed, but not the same as in purchase order
      lv_valid = '2'.
    ELSE.
* Purchase order is valid
      lv_valid = '0'.
    ENDIF.
  ENDIF.
ELSE.
* Purchase order is invalid
  lv_valid = '1'.
ENDIF.

swc_set_element container 'PurchaseOrderValidity' lv_valid.

end_method.

begin_method checkgoodsreceipt changing container.
* Check if goods receipt exists for purchase order
* 0 - Open GR exists
* 1 - No open GR exists
* 2 - No GR needed
* 9 - Invalid purchase order number

DATA: lv_grexist    TYPE syst-input,
      ls_hd         TYPE /nrk/apayhd,
      ls_ekbe       TYPE ekbe,
      ls_ekko       TYPE ekko,
      ls_ekpo       TYPE ekpo,
      lv_menge      TYPE menge_d,
      lv_menge_we   TYPE menge_d,
      lv_menge_re   TYPE menge_d,
      lv_shkzg      TYPE shkzg.

* Get APay header data
SELECT SINGLE * FROM /nrk/apayhd INTO ls_hd
               WHERE apayno EQ object-key-apayrecordid.

IF sy-subrc NE 0.
  exit_return 9001 text-901 object-key-apayrecordid space space.
ENDIF.

* Check if purchase order number is valid
SELECT SINGLE * FROM ekko INTO ls_ekko WHERE ebeln EQ ls_hd-ebeln.
IF sy-subrc NE 0.
  lv_grexist = '9'.
ENDIF.

* Check document type
CASE ls_ekko-bstyp.

  WHEN 'L'.
* Scheduling agreement / Lieferplan, check if GR quantity is less than RE quantity
    CLEAR: lv_menge, lv_menge_we, lv_menge_re.
* Get GR quantity
    SELECT shkzg menge INTO (lv_shkzg, lv_menge)
      FROM ekbe WHERE ebeln EQ ls_hd-ebeln AND vgabe EQ '2'.
      IF lv_shkzg EQ 'H'.
* Credit with negative amount
        lv_menge = lv_menge * ( -1 ).
      ENDIF.
      lv_menge_we = lv_menge_we + lv_menge.
    ENDSELECT.
    CLEAR: lv_menge, lv_shkzg.
* Get RE quantity
    SELECT shkzg menge INTO (lv_shkzg, lv_menge)
      FROM ekbe WHERE ebeln EQ ls_hd-ebeln AND vgabe EQ '2'.
      IF lv_shkzg EQ 'H'.
* Credit with negative amount
        lv_menge = lv_menge * ( -1 ).
      ENDIF.
      lv_menge_re = lv_menge_re + lv_menge.
    ENDSELECT.
    CLEAR: lv_menge, lv_shkzg.
* Compare
    IF lv_menge_we > lv_menge_re.
* Quantity delivered is larger than quantity invoiced, open GR exists
      lv_grexist = '0'.
    ELSE.
* Quantity delivered is less or equal than quantity invoiced, no open GR exists
      lv_grexist = '1'.
    ENDIF.

* Purchase order / Normalbestellung, check only if GR exists
  WHEN OTHERS.
* Check if we need a goods receipt
    SELECT SINGLE * FROM ekpo INTO ls_ekpo WHERE ebeln EQ ls_hd-ebeln
                                             AND wepos EQ 'X'.
    IF sy-subrc NE 0.
* Nothing found, no goods receipt needed
      lv_grexist = '2'.
    ELSE.
* Get purchase order history
      SELECT SINGLE * FROM ekbe INTO ls_ekbe WHERE ebeln EQ ls_hd-ebeln
                                               AND vgabe EQ '1'.
      IF sy-subrc EQ 0.
* Open goods receipt exists
        lv_grexist = '0'.
      ELSE.
* No goods receipt exists
        lv_grexist = '1'.
      ENDIF.
    ENDIF.

ENDCASE.

swc_set_element container 'GoodsReceiptExistance'  lv_grexist.

end_method.

begin_method enterpurchaseorder changing container.

CALL FUNCTION '/NRK/APAYENTERPO'
  EXPORTING
    apayno = object-key-apayrecordid
  EXCEPTIONS
    cancel = 1
    create = 2
    OTHERS = 3.

IF sy-subrc <> 0.
  IF sy-subrc EQ 2.
    exit_return 9001 space space space space.
  ENDIF.
  exit_cancelled.
ENDIF.

end_method.

begin_method callmirobyuser changing container.

DATA:
      status TYPE t040s-char1,
      bus2081 TYPE swc_object,
      liv_belnr LIKE /nrk/apayhd-liv_belnr,
      liv_gjahr LIKE /nrk/apayhd-liv_gjahr,
      object_key(18) TYPE c.

CALL FUNCTION '/NRK/APAY_CALL_MIRO'
  EXPORTING
    apayno         = object-key-apayrecordid
  IMPORTING
    belnr          = liv_belnr
    gjahr          = liv_gjahr
    status         = status
  EXCEPTIONS
    invalid_record = 1
    OTHERS         = 2.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  exit_cancelled.
ENDIF.

swc_set_element container 'Status' status.
IF status EQ 'C'.
  exit_cancelled.
ENDIF.

IF NOT liv_belnr IS INITIAL
  AND NOT liv_gjahr IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = liv_belnr
    IMPORTING
      output = liv_belnr.

  CONCATENATE liv_belnr liv_gjahr INTO object_key.
  swc_create_object bus2081 'BUS2081' object_key.
  swc_set_element container 'BUS2081' bus2081.
ENDIF.



end_method.

begin_method callmigobyuser changing container.

DATA: ebeln LIKE ekko-ebeln.

SELECT SINGLE ebeln FROM /nrk/apayhd INTO ebeln
  WHERE apayno EQ object-key-apayrecordid.

IF ebeln IS INITIAL.
  exit_cancelled.
ENDIF.

CALL FUNCTION 'MIGO_DIALOG'
  EXPORTING
    i_action            = 'A01'
    i_refdoc            = 'R01'
    i_notree            = 'X'
    i_no_auth_check     = 'X'
    i_skip_first_screen = 'X'
    i_deadend           = ' '
    i_okcode            = 'OK_GO'
    i_leave_after_post  = 'X'
    i_new_rollarea      = 'X'
    i_ebeln             = ebeln
  EXCEPTIONS
    illegal_combination = 1
    OTHERS              = 2.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  exit_cancelled.
ENDIF.


end_method.

begin_method getapayrecordfrombus2081 changing container.
DATA:
      apayobject TYPE swc_object,
      invoicedocumentno TYPE rbkp-belnr,
      fiscalyear TYPE rbkp-gjahr,
      apayno LIKE /nrk/apayhd-apayno.

swc_get_element container 'InvoiceDocumentNo' invoicedocumentno.
swc_get_element container 'FiscalYear' fiscalyear.

WAIT UP TO 5 SECONDS.

SELECT SINGLE apayno FROM /nrk/apayhd INTO apayno
  WHERE liv_belnr EQ invoicedocumentno
    AND liv_gjahr EQ fiscalyear.

IF sy-subrc = 0.

  swc_create_object apayobject '/NRK/APAY' apayno.
  swc_set_element container 'APayObject' apayobject.

ENDIF.

end_method.

get_property image changing container.

DATA: archiv_id LIKE toa01-archiv_id,
      arc_doc_id LIKE toa01-arc_doc_id,
      wa_links LIKE toa01,
      key(42) TYPE c,
      object_id TYPE saeobjid,
      connections TYPE STANDARD TABLE OF toav0,
      wa_connect TYPE toav0.

MOVE object-key-apayrecordid TO object_id.

CALL FUNCTION 'ARCHIV_GET_CONNECTIONS'
  EXPORTING
    objecttype               = '/NRK/APAY'
    object_id                = object_id
    client                   = sy-mandt
  TABLES
    connections              = connections
*   PARAMETER                =
  EXCEPTIONS
    nothing_found            = 1
    OTHERS                   = 2.

IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

READ TABLE connections INTO wa_connect INDEX 1.

IF NOT wa_connect IS INITIAL.
  CONCATENATE wa_connect-archiv_id wa_connect-arc_doc_id INTO key.
  swc_create_object object-image 'IMAGE' key.
  swc_set_element container 'Image' object-image.
ENDIF.

end_property.

begin_method entervendornumber changing container.

CALL FUNCTION '/NRK/APAYENTERVENDOR'
  EXPORTING
    apayno = object-key-apayrecordid
  EXCEPTIONS
    cancel = 1
    OTHERS = 2.

IF sy-subrc <> 0.
  exit_cancelled.
ENDIF.

end_method.

begin_method createbkpfbyuser changing container.

DATA: posted     TYPE boole-boole,
      bkpf       TYPE swc_object,
      bukrs      LIKE /nrk/apayhd-bukrs,
      belnr      LIKE /nrk/apayhd-belnr,
      gjahr      LIKE /nrk/apayhd-gjahr,
      wa_hd      LIKE /nrk/apayhd,
      bdcmsgcoll LIKE bdcmsgcoll OCCURS 0,
      items      LIKE /nrk/apayitems OCCURS 0,
      object_key(18) TYPE c.

SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
  WHERE apayno EQ object-key-apayrecordid.

CALL FUNCTION '/NRK/APAYCREATEBKPF'
  EXPORTING
    apayhd         = wa_hd
    mode           = 'A'
  IMPORTING
    posted         = posted
    belnr          = belnr
    bukrs          = bukrs
    gjahr          = gjahr
  TABLES
    items          = items
    bdcmsgcoll     = bdcmsgcoll
  EXCEPTIONS
    posting_failed = 1
    OTHERS         = 2.

IF sy-subrc <> 0.
  posted = ' '.
  exit_cancelled.
ELSE.
  IF NOT belnr IS INITIAL
    AND NOT bukrs IS INITIAL
    AND NOT gjahr IS INITIAL.

    CONCATENATE bukrs belnr gjahr INTO object_key.
    swc_create_object bkpf 'BKPF' object_key.
    swc_set_element container 'BKPF' bkpf.
  ELSE.
    posted = ' '.
    exit_cancelled.
  ENDIF.
ENDIF.

swc_set_element container 'Posted' posted.

end_method.

begin_method determinesapapprover changing container.

DATA: sapuser TYPE /nrk/apayuser-objid,
      approvalrequired TYPE boole-boole.
swc_get_element container 'SAPUser' sapuser.

CALL FUNCTION '/NRK/APAYDETERMINESAPAPPROVER'
  EXPORTING
    apayno                    = object-key-apayrecordid
  IMPORTING
    approval                  = approvalrequired
  EXCEPTIONS
    hierarchy_creation_failed = 1
    record_not_found          = 2
    user_not_found            = 3
    OTHERS                    = 4.

CASE sy-subrc.
  WHEN 0.

  WHEN 1.
    exit_return 9001 'No Manager found.' space space space.
  WHEN 2.
    exit_return 9001 'Record #' object-key-apayrecordid
      'not found.' space.
  WHEN 3.
    exit_return 9001 'User' sapuser 'not found.' space.
  WHEN 4.
    exit_return 9001 'General error.' space space space.
  WHEN OTHERS.
    exit_return 9001 'Other error.' space space space.
ENDCASE.

swc_set_element container 'ApprovalRequired' approvalrequired.

end_method.

begin_method createpobyuser changing container.

CALL FUNCTION '/NRK/APAY_CREATE_PO_BY_USER'
  EXPORTING
    apayno         = object-key-apayrecordid
  EXCEPTIONS
    no_apay_record = 1
    no_po_received = 2
    OTHERS         = 3.

IF sy-subrc <> 0.
  exit_cancelled.
ENDIF.

end_method.

begin_method startexternalworkflow changing container.

DATA: workflowtype TYPE t000-adrnr,
      messages     TYPE /nrk/apayapi_messages OCCURS 0,
      wa_message   TYPE /nrk/apayapi_messages.

swc_get_element container 'WorkflowType' workflowtype.

CALL FUNCTION '/NRK/APAYAPI_START_WORKFLOW'
  EXPORTING
    apayno       = object-key-apayrecordid
    workflowtype = workflowtype
  TABLES
    messages     = messages.

READ TABLE messages INTO wa_message
  WITH KEY msg_type = 'E'.

IF sy-subrc EQ 0.
  exit_return 9001 wa_message-msg_type
                   space
                   wa_message-msg_text
                   space.
ENDIF.

end_method.

begin_method getbuyerofpo changing container.
DATA:
      user TYPE usr01-bname,
      smtp_addr TYPE /nrk/apayappr-smtp_addr.

CALL FUNCTION '/NRK/APAY_GET_BUYER'
  EXPORTING
    apayno    = object-key-apayrecordid
  IMPORTING
    usnam     = user
    smtp_addr = smtp_addr.

swc_set_element container 'User' user.
swc_set_element container 'smtp_addr' smtp_addr.
end_method.

begin_method setcoder changing container.

DATA: externaluserid TYPE /nrk/apayuser-extuser,
      wa_user        TYPE /nrk/apayuser,
      wa_approver    TYPE /nrk/apayappr.

swc_get_element container 'ExternalUserID' externaluserid.

SELECT SINGLE * FROM /nrk/apayuser INTO wa_user
  WHERE extuser EQ externaluserid.

IF sy-subrc NE 0.
  exit_return 1000 text-903 space space space.
ENDIF.

DELETE FROM /nrk/apayappr
  WHERE ext_user EQ externaluserid.

CLEAR: wa_approver.
wa_approver-apayno = object-key-apayrecordid.
wa_approver-item = 1.
wa_approver-sap_user = wa_user-objid.
wa_approver-ext_user = externaluserid.
wa_approver-approved = space.
wa_approver-smtp_addr = wa_user-smtp_addr.
INSERT INTO /nrk/apayappr VALUES wa_approver.

end_method.

begin_method validateserviceentrysheet changing container.

DATA: ebeln TYPE ebeln,
      result TYPE num1.

swc_get_element container 'PurchasingDocument' ebeln.

CALL FUNCTION '/NRK/APAY_VALIDATE_SES'
  EXPORTING
    ebeln  = ebeln
  IMPORTING
    RESULT = RESULT
  exceptions
    OTHERS = 01.
CASE sy-subrc.
  WHEN 0.            " OK
  WHEN OTHERS.       " to be implemented
ENDCASE.

swc_get_element container 'Result' result.

end_method.

begin_method callmirowithlineitemsbyuser changing container.

DATA: status TYPE t040s-char1,
      bus2081 TYPE swc_object,
      liv_belnr LIKE /nrk/apayhd-liv_belnr,
      liv_gjahr LIKE /nrk/apayhd-liv_gjahr,
      object_key(18) TYPE c.

CALL FUNCTION '/NRK/APAY_CALL_MIRO_ITEMS'
  EXPORTING
    apayno         = object-key-apayrecordid
  IMPORTING
    belnr          = liv_belnr
    gjahr          = liv_gjahr
    status         = status
  EXCEPTIONS
    invalid_record = 1
    error          = 2
    OTHERS         = 3.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  exit_cancelled.
ENDIF.

swc_set_element container 'Status' status.

IF status EQ 'C'.
  exit_cancelled.
ENDIF.

IF NOT liv_belnr IS INITIAL
  AND NOT liv_gjahr IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = liv_belnr
    IMPORTING
      output = liv_belnr.

  CONCATENATE liv_belnr liv_gjahr INTO object_key.
  swc_create_object bus2081 'BUS2081' object_key.
  swc_set_element container 'BUS2081' bus2081.

ENDIF.

end_method.

get_property vendoremailaddress changing container.

DATA: l_lifnr LIKE lfa1-lifnr,
      l_adrnr LIKE lfa1-adrnr,
      l_vendoremailaddress LIKE adr6-smtp_addr.

SELECT SINGLE lifnr FROM /nrk/apayhd
  INTO l_lifnr
  WHERE apayno = object-key-apayrecordid.

SELECT SINGLE adrnr FROM lfa1 INTO l_adrnr
  WHERE lifnr = l_lifnr.

SELECT SINGLE smtp_addr FROM adr6 INTO l_vendoremailaddress
  WHERE addrnumber = l_adrnr.

IF sy-subrc EQ 0.
  swc_set_element container 'VendorEMailAddress'
       l_vendoremailaddress.
ENDIF.

end_property.

begin_method validategoodsreceipts changing container.

DATA:
      apayrecordid TYPE /nrk/apayhd-apayno,
      validationresult TYPE syst-input.

swc_get_element container 'APayRecordID' apayrecordid.

CALL FUNCTION '/NRK/APAYVALIDATEGOODSRECEIPT'
  EXPORTING
    apayno    = apayrecordid
  IMPORTING
    gr_result = validationresult
  EXCEPTIONS
    error     = 1
    OTHERS    = 2.

IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ELSE.
  swc_set_element container 'ValidationResult' validationresult.
ENDIF.

end_method.
