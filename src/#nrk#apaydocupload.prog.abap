*&---------------------------------------------------------------------*
*& Report  /NRK/APAYDOCUPLOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  /nrk/apaydocupload.

CALL FUNCTION '/NRK/APAYUPLOADNEWDOCUMENT'
  EXCEPTIONS
    document_upload_failed = 1
    OTHERS                 = 2.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
