FUNCTION /nrk/apaychangerecord.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(APAYNO) TYPE  /NRK/APAYHD-APAYNO
*"  EXCEPTIONS
*"      NO_APAY_RECORD_FOUND
*"----------------------------------------------------------------------

*  DATA: wa_hd   LIKE /nrk/apayhd,
*        wa_ekko LIKE ekko,
*        wa_lfa1 LIKE lfa1,
*        wa_lfb1 LIKE lfb1.

  CLEAR: wa_hd,
         wa_hd_orig,
         wa_ekko,
         wa_lfa1,
         wa_lfb1,
         ebeln,
         lifnr,
         lifname,
         wrbtr,
         wmwst,
         wrbtr_net,
         waers,
         bukrs,
         bldat,
         xblnr,
         lfsnr1.

  SELECT SINGLE * FROM /nrk/apayhd INTO wa_hd
    WHERE apayno EQ apayno.

  IF sy-subrc NE 0.
    RAISE no_apay_record_found.
  ELSE.

    MOVE wa_hd TO wa_hd_orig.

    apayno2 = wa_hd-apayno.
    ebeln = wa_hd-ebeln.
    lifnr = wa_hd-lifnr.
    lifname = wa_hd-lifname.
    wrbtr = wa_hd-wrbtr.
    wmwst = wa_hd-wmwst.
    waers = wa_hd-waers.
    bukrs = wa_hd-bukrs.
    bldat = wa_hd-bldat.
    xblnr = wa_hd-xblnr.

    lfsnr1 = wa_hd-lfsnr1.

    IF wa_hd-wrbtr_net IS INITIAL.
      wa_hd-wrbtr_net = wa_hd-wrbtr - wa_hd-wmwst.
    ELSE.
      wrbtr_net = wa_hd-wrbtr_net.
    ENDIF.

    IF wa_hd-rdate IS INITIAL.
      rdate = wa_hd-cdate.
    ENDIF.

* Get workflow approval type
    SELECT SINGLE wf_type FROM /nrk/apaydtype INTO wf_type
       WHERE ar_object EQ wa_hd-ar_object
         AND bukrs EQ wa_hd-bukrs.

* Get workflow approver
    IF NOT wa_hd-ext_approver IS INITIAL.
      SELECT SINGLE fullname FROM /nrk/apayuser INTO fullname
        WHERE extuser EQ wa_hd-ext_approver.
    ENDIF.

*   CALL SCREEN '0100'.
*   CALL SCREEN '0105'.
    CALL SCREEN '0106'.

  ENDIF.

ENDFUNCTION.
