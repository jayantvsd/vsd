FUNCTION /nrk/apaysendemail.
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

  DATA: l_receiver          LIKE soud-usrnam.

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

*
* check for receiver
*
  IF receiver IS INITIAL
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

* get e-mail address for user

  IF receiver IS INITIAL.

    litab_receiver-recextnam = receiver_email.
    TRANSLATE litab_receiver-recextnam TO LOWER CASE.

  ELSE.

    l_receiver = receiver.

    CALL FUNCTION 'EFG_GEN_GET_USER_EMAIL'
      EXPORTING
        i_uname           = receiver
      IMPORTING
        e_email_address   = litab_receiver-recextnam
      EXCEPTIONS
        not_qualified     = 1
        user_not_found    = 2
        address_not_found = 3
        OTHERS            = 4.

  ENDIF.


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

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data                    = l_document_data
      document_type                    = 'RAW'
      commit_work                      = 'X'
    TABLES
*   OBJECT_HEADER                    =
      object_content                   = msg
*   CONTENTS_HEX                     =
*   OBJECT_PARA                      =
*   OBJECT_PARB                      =
      receivers                        = t_receivers
   EXCEPTIONS
     too_many_receivers               = 1
     document_not_sent                = 2
     document_type_not_exist          = 3
     operation_no_authorization       = 4
     parameter_error                  = 5
     x_error                          = 6
     enqueue_error                    = 7
     OTHERS                           = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.




* Mail body

*  CLEAR: litab_message[], litab_message.
*
*  APPEND '<html>'  TO litab_message.
*  APPEND '<head>'  TO litab_message.
*  CONCATENATE '<meta name=apayno content='
*              apayno '>'
*         INTO litab_message.
*  APPEND litab_message.
*  APPEND '</head>' TO litab_message.
*  APPEND '<body>'  TO litab_message.
*
*  LOOP AT msg.
*    CONCATENATE msg '<br>'
*           INTO litab_message.
*    APPEND litab_message.
*  ENDLOOP.
*  CONCATENATE '<!--apayno='
*              apayno '>'
*         INTO litab_message.
*  APPEND litab_message.
*  APPEND '</body>'  TO litab_message.
*  APPEND '</html>'  TO litab_message.
*  APPEND '' TO litab_message.
*
** send text email - SAPConnect must be configured
*
*  TRY.
*
** create email objects
*      l_o_send_request = cl_bcs=>create_persistent( ).
*
** sender
*      l_sender_bcs = l_sender_email.
*      l_o_sender = cl_cam_address_bcs=>create_internet_address( l_sender_bcs ).
*      l_o_send_request->set_sender( i_sender = l_o_sender ).
*
** recipient TO
*      READ TABLE litab_receiver INDEX 1.
*      l_recipient_bcs = litab_receiver-recextnam.
*      l_o_recipient = cl_cam_address_bcs=>create_internet_address( l_recipient_bcs ).
*      l_o_send_request->add_recipient(
*      i_recipient = l_o_recipient
*      i_copy = '' " CC indicator
*      ).
*
** recipient CC
*      IF NOT receiver_cc_email IS INITIAL.
*
*        l_recipient_bcs = receiver_cc_email.
*        l_o_recipient = cl_cam_address_bcs=>create_internet_address( l_recipient_bcs ).
*        l_o_send_request->add_recipient(
*        i_recipient = l_o_recipient
*        i_copy = 'X' " CC indicator
*        ).
*
*      ENDIF.
*
** create email message (html)
*      l_it_contents[] = litab_message[].
*      l_o_document = cl_document_bcs=>create_document(
*      i_type = 'HTM'
*      i_text = l_it_contents
*      i_subject = wa_object_hd_change-objdes
*      ).
*
** add to document to send request.
*
*      l_o_send_request->set_document( l_o_document ).
*
** send email
*      l_v_ret = l_o_send_request->send( ).
*
*    CATCH cx_bcs INTO bcs_exception.
*
*      RAISE send_error.
*      EXIT.
*
*  ENDTRY.
*
*  IF sy-subrc <> 0.
*    RAISE send_error.
*    EXIT.
*  ENDIF.
*
*  COMMIT WORK.
*  CALL FUNCTION 'SO_DEQUEUE_UPDATE_LOCKS'
*    EXCEPTIONS
*      communication_failure = 1
*      system_failure        = 2
*      OTHERS                = 3.
*
*  IF sy-subrc <> 0.
*    RAISE error.
*    EXIT.
*  ENDIF.
*
**
** send all email now if requested
**
*  IF l_send_now EQ c_true.
*
*    WAIT UP TO 2 SECONDS.
*
*    SUBMIT rsconn01 WITH mode = 'INT'
*                  WITH output = ' '
*                  AND RETURN.
*
*  ENDIF.

ENDFUNCTION.
