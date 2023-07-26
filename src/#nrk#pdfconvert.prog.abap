report  /nrk/pdfconvert line-size 1023.
data:spoolid type tsp01-rqident.
data:itab like table of line,
      wa type line.

field-symbols <fs> like line of itab.

data: print_parameters type pri_params,
      valid_flag       type c length 1.

data: complete_email type string.

data : inv_no_objdid type sapb-sapobjid.
data: begin of wa_header,
      inv_no type /nrk/apayhd-apayno,
      sender_email  like soos1-recextnam,
      reciever_email like soos1-recextnam,
      sender_userid  like soud-usrnam,
      send_date type sy-datum,
      send_time type sy-uzeit,
      email_subject type so_obj_des,
      end of wa_header.

data: itab_header like table of wa_header.

data: doc_id_insert type toav0-arc_doc_id,
      arbject type toaom-ar_object,
      objid like sapb-sapobjid,
      sapobj type toaom-sap_object.

data:pdf like table of tline.

data:mi_bytecount     type i,
         jobname like tbtcjob-jobname,
         pdfspoolid like tsp01-rqident,
         jobcount like tbtcjob-jobcount.

data: path like tpfyvalue-value.
data: filename type string.
data: gv_timestamp(1024) type c.
data: gv_time_nopdf(1024) type c.
data:pdf1 like pdf with header line.
data: arobject type saeobjart,
      doc_id(1024) type c.

data:wa_/nrk/apayhd type /nrk/apayhd.

data: begin of out_lines occurs 0,
      line(80) type c,
      end of out_lines.

data: begin of out_lines_wa,
line(80) type c,
end of out_lines_wa.

data:wa_toaom type toaom.

* logging Structure.

data:begin of log_structure,
   msgid      type bal_s_msg-msgid,
   msgno      type bal_s_msg-msgno,
   msgv1      type bal_s_msg-msgv1,
   msgv2      type bal_s_msg-msgv2,
   msgv3      type bal_s_msg-msgv3,
   msgv4      type bal_s_msg-msgv4,
 end of log_structure.

data:wa_/nrk/apayconfig type /nrk/apayconfig.

start-of-selection.

  call function 'GET_PRINT_PARAMETERS'
    exporting
      no_dialog            = 'X'
    importing
      out_parameters       = print_parameters
      valid                = valid_flag
    exceptions
      invalid_print_params = 2
      others               = 4.


  import itab from memory id 'ABC'.
  import itab_header from memory id 'DEF'.
  new-page print on parameters print_parameters
                    no dialog.

  loop at itab_header into wa_header.
    concatenate 'From (Sender) : ' wa_header-sender_email into wa_header-sender_email.
    concatenate 'To (Reciever) : ' wa_header-reciever_email into wa_header-reciever_email.

    write: wa_header-sender_email,
            wa_header-reciever_email,
            'Date and Time : ' , wa_header-send_date,wa_header-send_time.
    write :/'Subject : ' , wa_header-email_subject.
    write:/'_____________________________________________________________________________________'.
    write:/.
  endloop.

*log_structure-msgv1 = 'List Generated Successfully'.
* perform write_log.

  loop at itab into wa.
*  concatenate complete_email '.' wa '.' into complete_email.
*    write: wa-line.
    call function 'RKD_WORD_WRAP'
      exporting
        textline                  = wa-line
*       DELIMITER                 = ' '
       outputlen                 = 80
*     IMPORTING
*       OUT_LINE1                 =
*       OUT_LINE2                 =
*       OUT_LINE3                 =
     tables
      out_lines                 = out_lines
*     EXCEPTIONS
*       OUTPUTLEN_TOO_LARGE       = 1
*       OTHERS                    = 2
              .
    if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

    loop at out_lines into out_lines_wa.
      write out_lines_wa-line.
    endloop.
    refresh out_lines.
    clear out_lines_wa.
*    new-page print off.
  endloop.
*  write complete_email.
  new-page print off.

* select single RQIDENT from tsp01 into spoolid.
  spoolid = sy-spono.
  call function 'CONVERT_ABAPSPOOLJOB_2_PDF'
    exporting
      src_spoolid              = spoolid
       no_dialog                      = 'X'
*   dst_device               = 'LOCL'
*     get_size_from_format     = 'X'
    importing
      pdf_bytecount            = mi_bytecount

          pdf_spoolid                    = pdfspoolid
*       LIST_PAGECOUNT                 =
          btc_jobname                    = jobname
          btc_jobcount                   = jobcount
    tables
      pdf                      = pdf
    exceptions
      err_no_abap_spooljob     = 1
      err_no_spooljob          = 2
      err_no_permission        = 3
      err_conv_not_possible    = 4
      err_bad_destdevice       = 5
      user_cancelled           = 6
      err_spoolerror           = 7
      err_temseerror           = 8
      err_btcjob_open_failed   = 9
      err_btcjob_submit_failed = 10
      err_btcjob_close_failed  = 11
      others                   = 12.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  call function 'SXPG_PROFILE_PARAMETER_GET'
    exporting
      parameter_name  = 'DIR_GLOBAL'
    importing
      parameter_value = path.

  export path to memory id 'GHI'.

  gv_time_nopdf = sy-uzeit.
  concatenate sy-uzeit '.pdf' into gv_timestamp.
  concatenate path '\' gv_timestamp into filename.

*  call function 'GUI_DOWNLOAD'
*    exporting
*      bin_filesize            = mi_bytecount
*      filename                = filename
*      filetype                = 'BIN'
*    tables
*      data_tab                = pdf
*    exceptions
*      file_write_error        = 1
*      no_batch                = 2
*      gui_refuse_filetransfer = 3
*      invalid_type            = 4
*      no_authority            = 5
*      unknown_error           = 6
*      header_not_allowed      = 7
*      separator_not_allowed   = 8
*      filesize_not_allowed    = 9
*      header_too_long         = 10
*      dp_error_create         = 11
*      dp_error_send           = 12
*      dp_error_write          = 13
*      unknown_dp_error        = 14
*      access_denied           = 15
*      dp_out_of_memory        = 16
*      disk_full               = 17
*      dp_timeout              = 18
*      file_not_found          = 19
*      dataprovider_exception  = 20
*      control_flush_error     = 21
*      others                  = 22.

  pdf1[] = pdf[].
  open dataset filename for output in binary mode.
  if sy-subrc = 0.
    loop at pdf1.
      transfer pdf1-tdformat to filename.
      transfer pdf1-tdline to filename.
    endloop.
    close dataset filename .
  endif.

  select single * from /nrk/apayhd into wa_/nrk/apayhd where apayno = wa_header-inv_no.

* arc_id -- to be derived from business object and doc type
* doc_type to be read from config  hardcoded'ZEBPPPDF'-- inputs to be provided

  select single * from /nrk/apayconfig into wa_/nrk/apayconfig where key1 = 'APAYSP' and key2 = 'MAILDTYPE'.
*  if sy-subrc = 0.
  arbject = wa_/nrk/apayconfig-val1.
  select single * from toaom into wa_toaom where sap_object = 'FIPP' and ar_object = arbject.

  call function 'SCMS_AO_FILE_CREATE'
    exporting
      mandt        = sy-mandt
      arc_id       = wa_toaom-archiv_id
      file         = gv_timestamp
      doc_type     = 'PDF'
    importing
      doc_id       = doc_id
    exceptions
      error_http   = 1
      error_archiv = 2
      error_kernel = 3
      error_config = 4
      error_file   = 5
      others       = 6.
  if sy-subrc <> 0.

  endif.

* doc type - inputs required

  doc_id_insert = doc_id.
* replace with invoice ,to be passed from calling module

* objid =   gv_time_nopdf.
  concatenate wa_/nrk/apayhd-bukrs wa_/nrk/apayhd-belnr wa_/nrk/apayhd-gjahr into objid.
  sapobj = 'FIPP'.
  call function 'ARCHIV_CONNECTION_INSERT'
  exporting
*       ARCHIV_ID             =
  arc_doc_id            = doc_id_insert
  ar_date               = sy-datum
  ar_object             = arbject
*       DEL_DATE              = ' '
*       MANDANT               = ' '
  object_id             = objid
  sap_object            = sapobj
*       DOC_TYPE              = ' '
*       BARCODE               = ' '
exceptions
  error_connectiontable = 1
  others                = 2.
  if sy-subrc <> 0.

  endif.

* routine for writing log.
form write_log.
  data:
       ls_log        type bal_s_log,
       lt_handle     type bal_t_logh,
       lf_handle     type balloghndl,
       ls_msg        type bal_s_msg.

* Invoice Log object
  ls_log-object     = 'ZINVOICE'.
  ls_log-subobject  = 'ZAPINVOICE'.

  ls_log-aluser     = sy-uname.
  ls_log-alprog     = sy-repid.
  ls_log-altcode    = 'tcode'.
  ls_log-aldate_del = sy-datum + 30.  "keep for one month
  ls_log-del_before = 'X'.

*   create a log
  call function 'BAL_LOG_CREATE'
    exporting
      i_s_log      = ls_log
    importing
      e_log_handle = lf_handle
    exceptions
      others       = 1.

*   define data of message for Application Log
*   Use generic message template with & & & &
  ls_msg-msgty     = 'S'.
  ls_msg-msgid     = '01'.
  ls_msg-msgno     = '319'.
  ls_msg-msgv1     = log_structure-msgv1.
  ls_msg-msgv2     = log_structure-msgv2.
  ls_msg-msgv3     = log_structure-msgv3.
  ls_msg-msgv4     = log_structure-msgv4.
  ls_msg-probclass = 2.

*clear  log_structure-msgv1.
*   add this message to log
*   this function can be called several times to have one log entry
*   store several different messages
  call function 'BAL_LOG_MSG_ADD'
    exporting
      i_log_handle = lf_handle
      i_s_msg      = ls_msg
    exceptions
      others       = 1.

*   save the log
  append lf_handle to lt_handle.
  call function 'BAL_DB_SAVE'
    exporting
      i_save_all     = 'X'
      i_t_log_handle = lt_handle
    exceptions
      others         = 1.
endform.                    "write_log
