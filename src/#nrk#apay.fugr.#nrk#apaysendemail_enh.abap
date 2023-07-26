FUNCTION /nrk/apaysendemail_enh.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"     REFERENCE(ARCHIV_ID) TYPE  TOA01-ARCHIV_ID OPTIONAL
*"     REFERENCE(ARC_DOC_ID) TYPE  TOA01-ARC_DOC_ID OPTIONAL
*"     REFERENCE(SENDEREMAIL) LIKE  SOOS1-RECEXTNAM DEFAULT
*"       'sapnk0@norikkon.com'
*"     REFERENCE(SENDER) TYPE  SOUD-USRNAM DEFAULT 'WF-BATCH'
*"     REFERENCE(RECEIVER) LIKE  SOUD-USRNAM OPTIONAL
*"     REFERENCE(EXT_RECEIVER) LIKE  /NRK/APAYUSER-EXTUSER OPTIONAL
*"     REFERENCE(RECEIVER_EMAIL) LIKE  SOOS1-RECEXTNAM OPTIONAL
*"     REFERENCE(RECEIVER_CC_EMAIL) LIKE  SOOS1-RECEXTNAM OPTIONAL
*"     REFERENCE(MAIL_SUBJECT) TYPE  SO_OBJ_DES
*"     REFERENCE(SEND_NOW) TYPE  BOOLE-BOOLE DEFAULT 'X'
*"  TABLES
*"      MSG STRUCTURE  LINE
*"      DOCS STRUCTURE  TOAV0 OPTIONAL
*"  EXCEPTIONS
*"      INVALID_RECORD
*"      INVALID_USERID
*"      INVALID_EMAIL
*"      MSG_ERROR
*"      URL_ERROR
*"      SEND_ERROR
*"      ERROR
*"      PDF_GEN_ERROR
*"      NO_REC
*"----------------------------------------------------------------------

  DATA: l_cnt               TYPE i,
        l_index_cnt         TYPE i.

  DATA: l_send_now          TYPE c.

* config options

  DATA: l_body_only         TYPE c VALUE 'X'.      " archive the text body only

* document

  DATA: l_crep_id(30)       TYPE c,
        l_doc_id(40)        TYPE c.

  DATA: litab_message       LIKE soli
        OCCURS 200
        WITH HEADER LINE.

  DATA: l_message_line      LIKE soli.

  DATA: litab_receiver      LIKE soos1
        OCCURS 5
        WITH HEADER LINE.

  DATA: l_receiver          LIKE soud-usrnam,
        l_ext_receiver      LIKE /nrk/apayuser-extuser.

  DATA: wa_object_hd_change LIKE sood1.

  DATA: wa_wwwdata          LIKE wwwdatatab.

  DATA: litab_html          LIKE w3html
        OCCURS 200
        WITH HEADER LINE.

  DATA: l_url(255)          TYPE c,
        l_uri               LIKE sapb-uri.

  DATA: l_sender_email      LIKE soos1-recextnam,
        l_receiver_email    LIKE soos1-recextnam,
        l_receiver_cc_email LIKE soos1-recextnam.

* Definitions for CL_BCS

  DATA: l_o_send_request TYPE REF TO cl_bcs,             " email request object
        l_o_document     TYPE REF TO cl_document_bcs,    " documents object
        l_o_sender       TYPE REF TO cl_cam_address_bcs, " sender object
        l_o_recipient    TYPE REF TO cl_cam_address_bcs, " recipient object
        bcs_exception    TYPE REF TO cx_bcs,             " exceptions
        l_v_ret          TYPE os_boolean,                " boolean return value
        l_it_contents    TYPE bcsy_text,                 " document contents
        l_wa_contents    TYPE LINE OF bcsy_text,
        l_it_attachment  TYPE solix_tab.                 " documents attachment

  DATA: l_sender_bcs      TYPE adr6-smtp_addr,
        l_recipient_bcs   TYPE adr6-smtp_addr,
        l_attachment_type TYPE soodk-objtp,
        l_attachment_subject TYPE sood-objdes.

  DATA: wa_hd               LIKE /nrk/apayhd,
        l_document_data     TYPE                   sodocchgi1,
        t_receivers         TYPE STANDARD TABLE OF somlreci1,
        wa_receivers        TYPE                   somlreci1.

  CONSTANTS:
          c_true      TYPE c VALUE 'X',
          c_false     TYPE c VALUE ' '.

  DATA: BEGIN OF wa_header,
        inv_no TYPE /nrk/apayhd-apayno,
        sender_email  LIKE soos1-recextnam,
        reciever_email LIKE soos1-recextnam,
        sender_userid  LIKE soud-usrnam,
        send_date TYPE sy-datum,
        send_time TYPE sy-uzeit,
        email_subject TYPE so_obj_des,
        END OF wa_header.
  DATA: itab_header LIKE TABLE OF wa_header.
  DATA l_sender TYPE soextreci1-receiver.
*
* check for receiver
*
  IF receiver IS INITIAL
    AND ext_receiver IS INITIAL
    AND receiver_email IS INITIAL.
    RAISE invalid_userid.
    EXIT.
  ENDIF.

* set send now option

  l_send_now = send_now.

* get APay record

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
     WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE invalid_record.
    EXIT.
  ENDIF.

** get reply e-mail address

  l_sender_email = senderemail.
  wa_header-sender_email = l_sender_email.
* get e-mail address for user

  IF ext_receiver IS INITIAL.
    litab_receiver-recextnam = receiver_email.
    TRANSLATE litab_receiver-recextnam TO LOWER CASE.
  ELSE.
    l_ext_receiver = ext_receiver.
    SELECT smtp_addr FROM /nrk/apayuser INTO litab_receiver-recextnam WHERE extuser = ext_receiver.
    ENDSELECT.
    IF sy-subrc NE 0.
      RAISE no_rec.
    ENDIF.
  ENDIF.
  wa_header-reciever_email = litab_receiver-recextnam.

  IF litab_receiver-recextnam LE space.
    RAISE invalid_email.
    EXIT.
  ENDIF.

* Receivers

* litab_receiver-recextnam  = 'sapnk@norikkon.com'.
  litab_receiver-recesc     = 'U'.
  litab_receiver-recnam     = 'U-'.
  litab_receiver-sndart     = 'INT'.
  litab_receiver-sndex      = 'X'.
  litab_receiver-sndpri     = '1'.
  litab_receiver-deliver    = 'X'.
  litab_receiver-not_deli   = 'X'.
  litab_receiver-read       = 'X'.
  litab_receiver-mailstatus = 'E'.
  litab_receiver-adr_name   = litab_receiver-recextnam.
  APPEND litab_receiver.

  l_receiver_email = litab_receiver-adr_name.

* Carbon Copy

  IF receiver_cc_email IS NOT INITIAL.

    litab_receiver-recextnam = receiver_cc_email.
    TRANSLATE litab_receiver-recextnam TO LOWER CASE.

    litab_receiver-sndcp      = 'X'.
    litab_receiver-recesc     = 'U'.
    litab_receiver-recnam     = 'U-'.
    litab_receiver-sndart     = 'INT'.
    litab_receiver-sndex      = 'X'.
    litab_receiver-sndpri     = '1'.
    litab_receiver-deliver    = 'X'.
    litab_receiver-not_deli   = 'X'.
    litab_receiver-read       = 'X'.
    litab_receiver-mailstatus = 'E'.
    litab_receiver-adr_name   = litab_receiver-recextnam.
    APPEND litab_receiver.

    l_receiver_cc_email = litab_receiver-adr_name.

  ENDIF.

* General data

  wa_object_hd_change-objla = sy-langu.
  wa_object_hd_change-objnam = '/NRK/APAY'.
  wa_object_hd_change-objsns = 'P'.
  wa_object_hd_change-file_ext = 'HTM'.

* Mail subject

  CONCATENATE '(' apayno ')'
         INTO wa_object_hd_change-objdes.

  CONCATENATE wa_object_hd_change-objdes
              mail_subject
         INTO wa_object_hd_change-objdes
         SEPARATED BY space.

  REFRESH t_receivers. CLEAR wa_receivers.
  wa_receivers-receiver = l_receiver_email.
  wa_receivers-rec_type = 'U'.
  wa_receivers-com_type = 'INT'.
  APPEND wa_receivers TO t_receivers.

  l_document_data-obj_name = 'IMPORTANT'.
  l_document_data-obj_descr = wa_object_hd_change-objdes.
  l_document_data-sensitivty = 'P'.
  l_document_data-proc_type = 'T'.
*lw_doc_data-proc_name = ‘IW32′.
  l_document_data-skip_scren = 'X'.

  wa_header-send_date = sy-datum.
  wa_header-send_time = sy-uzeit.
  wa_header-email_subject = wa_object_hd_change-objdes.
  wa_header-inv_no = apayno.
  INSERT wa_header INTO itab_header INDEX 1.
  CLEAR wa_header.
  DATA: it_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE.
  CLEAR it_packing_list.
  REFRESH it_packing_list.
  it_packing_list-transf_bin = space.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 0.
  it_packing_list-body_start = 1.
  DESCRIBE TABLE msg LINES it_packing_list-body_num.
  it_packing_list-doc_type = 'RAW'.
  APPEND it_packing_list.

  l_sender = l_sender_email.
  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data                    = l_document_data
*     PUT_IN_OUTBOX                    = ' '
      sender_address                   = l_sender
     sender_address_type              = 'INT'
     commit_work                      = 'X'
*  IMPORTING
*     SENT_TO_ALL                      =
*     NEW_OBJECT_ID                    =
*     SENDER_ID                        =
    TABLES
      packing_list                     = it_packing_list
*     OBJECT_HEADER                    =
*     CONTENTS_BIN                     =
     contents_txt                     = msg
*     CONTENTS_HEX                     =
*     OBJECT_PARA                      =
*     OBJECT_PARB                      =
      receivers                        = t_receivers
   EXCEPTIONS
     too_many_receivers               = 1
     document_not_sent                = 2
     document_type_not_exist          = 3
     operation_no_authorization       = 4
     parameter_error                  = 5
     x_error                          = 6
     enqueue_error                    = 7
     OTHERS                           = 8
            .
  IF sy-subrc <> 0.
    RAISE msg_error.
  ENDIF.


  IF sy-subrc = 0.

    DATA:itab LIKE TABLE OF  line,
            wa TYPE line.

    itab[] = msg[].
    EXPORT itab TO MEMORY ID 'ABC'.
    EXPORT itab_header TO MEMORY ID'DEF'.
    SUBMIT /nrk/pdfconvert EXPORTING LIST TO MEMORY AND RETURN.

  ENDIF.


ENDFUNCTION.
