*&---------------------------------------------------------------------*
*& APay Center 2.0 - APay Center record update program
*& (c) by Norikkon, LLC 2014
*&---------------------------------------------------------------------*
*& 1900 - Document posted
*& 9000 - Document paid
*&---------------------------------------------------------------------*
REPORT  /nrk/apaypayupdate_old MESSAGE-ID 00.

TABLES: /nrk/apayhd,
        /nrk/apayhis,
        bseg.

DATA: t_hd  LIKE /nrk/apayhd OCCURS 1 WITH HEADER LINE,
      t_his LIKE /nrk/apayhis OCCURS 1 WITH HEADER LINE.

CONSTANTS: c_bsc21    LIKE bseg-bschl     VALUE '21',
           c_bsc31    LIKE bseg-bschl     VALUE '31',
           c_statpost LIKE /nrk/apayhd-status VALUE '1900',
           c_statpaid LIKE /nrk/apayhd-status VALUE '9000'.

SELECT * INTO TABLE t_hd FROM /nrk/apayhd WHERE status EQ c_statpost.

LOOP AT t_hd.

* Check document clearing/payment
  SELECT SINGLE * FROM bseg
    WHERE bukrs EQ t_hd-bukrs
      AND belnr EQ t_hd-belnr
      AND gjahr EQ t_hd-gjahr
      AND bschl IN (c_bsc21, c_bsc31)
      AND augbl NE space.

  IF sy-subrc EQ 0. " Invoice cleared/paid

    CALL FUNCTION '/NRK/APAY_UPDATE_STATUS'
      EXPORTING
        apayno              = t_hd-apayno
        status              = c_statpaid
        date                = sy-datum
        time                = sy-uzeit
        user                = sy-uname
*   USER_EXT            =
      EXCEPTIONS
        update_failed       = 1
        OTHERS              = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDLOOP.
