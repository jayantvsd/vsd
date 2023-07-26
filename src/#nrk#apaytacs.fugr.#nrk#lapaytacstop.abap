FUNCTION-POOL /nrk/apaytacs.                "MESSAGE-ID 00

TYPE-POOLS vrm.

TABLES: /nrk/apaydtype.

DATA: ebeln   LIKE /nrk/apayhd-ebeln,
      lifnr   LIKE /nrk/apayhd-lifnr,
      lifname LIKE /nrk/apayhd-lifname,
      wrbtr   LIKE /nrk/apayhd-wrbtr,
      wmwst   LIKE /nrk/apayhd-wmwst,
      wrbtr_net LIKE /nrk/apayhd-wrbtr_net,
      waers   LIKE /nrk/apayhd-waers,
      bukrs   LIKE /nrk/apayhd-bukrs,
      bldat   LIKE /nrk/apayhd-bldat,
      xblnr   LIKE /nrk/apayhd-xblnr,
      ekgrp   LIKE ekko-ekgrp,
      ekorg   LIKE ekko-ekorg,
      apayno2 LIKE /nrk/apayhd-apayno,
      street  LIKE lfa1-stras,
      city    LIKE lfa1-ort01,
      zip     LIKE lfa1-pstlz,
      region  LIKE lfa1-regio,
      rdate   LIKE /nrk/apayhd-rdate,
      sdescr  LIKE /nrk/apaysdef-sdescr.

DATA: doctype_list  TYPE vrm_values,
      doctype_value TYPE LINE OF vrm_values,
      ok_code       LIKE sy-ucomm,
      doctype_desc  LIKE toasp-objecttext,
      rad_debit,
      rad_credit,
      result(1)     TYPE c,
      wapi_return   TYPE sysubrc,
      answer(1)     TYPE c.

DATA: wa_hd      LIKE /nrk/apayhd,
      wa_hd_orig LIKE /nrk/apayhd,
      wa_ekko    LIKE ekko,
      wa_lfa1    LIKE lfa1,
      wa_lfb1    LIKE lfb1,
      wa_dtype   LIKE /nrk/apaydtype,
      wa_toasp   LIKE toasp,
      wa_swwwihead LIKE swwwihead,
      t_swwwihead  LIKE swwwihead OCCURS 0,
      wa_config LIKE /nrk/apayconfig.

DATA: datachanged  TYPE boole,
      ar_object    LIKE toa01-ar_object,
      wi_id        LIKE swwwihead-wi_id,
      cancelled    TYPE boole,
      cancelstatus LIKE /nrk/apayhis-status,
      value1       LIKE /nrk/apayconfig-val1.

DATA: wa_toaom    TYPE toaom,
      arc_doc_id  LIKE toa01-arc_doc_id,
      outdoctab   TYPE toadt OCCURS 0,
      wa_outdoctab   TYPE toadt,
      filetab     TYPE file_table OCCURS 0,
      object_id    TYPE saeobjid,
      initstatus LIKE /nrk/apayhis-status,
      po_flag    LIKE boole-boole,
      wa_t001    LIKE t001,
      butxt      LIKE t001-butxt,
      extuser_list  TYPE vrm_values,
      extuser_value TYPE LINE OF vrm_values,
      t_approver LIKE /nrk/apayuser OCCURS 0,
      wa_approver LIKE /nrk/apayuser,
      fullname    LIKE /nrk/apayuser-fullname,
      extuser     LIKE /nrk/apayhd-ext_approver,
      wrbtr_total LIKE /nrk/apayhd-wrbtr,
      preapr.

DATA: lfsnr1 LIKE /nrk/apayhd-lfsnr1,
      wf_type LIKE /nrk/apaydtype-wf_type.
